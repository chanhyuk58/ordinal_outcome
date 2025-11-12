# mc_em_ordered_flow.py
# Full PyTorch implementation of MC-EM alternating algorithm for semiparametric ordered model
# using nflows rational-quadratic spline flows (1D).
#
# Usage: python mc_em_ordered_flow.py
# Make sure to install dependencies:
#   pip install torch nflows numpy scipy matplotlib statsmodels tqdm

import numpy as np
import torch
import torch.nn as nn
import torch.optim as optim
from torch.distributions import Normal, Uniform
import matplotlib.pyplot as plt
from tqdm import trange
from scipy.stats import logistic
import time
from scipy.special import erf as scipy_erf
np.erf = scipy_erf

# nflows imports
from nflows.flows import Flow
from nflows.distributions.normal import StandardNormal
from nflows.nn import nets
from nflows.transforms import (
    CompositeTransform,
    ReversePermutation,
    MaskedAffineAutoregressiveTransform,
    PiecewiseRationalQuadraticCouplingTransform,
    MaskedPiecewiseRationalQuadraticAutoregressiveTransform
)

# Optional: ordered probit for warm start
USE_STATSMODELS = True
try:
    from statsmodels.miscmodels.ordinal_model import OrderedModel
    import pandas as pd
except Exception:
    USE_STATSMODELS = False

# -----------------------------
# Utility functions
# -----------------------------
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
normal = Normal(torch.tensor(0.0, device=device), torch.tensor(1.0, device=device))

def log_normal_cdf(x):
    # numerically stable log CDF for standard normal via torch.special.erf
    return 0.5 * (1.0 + torch.erf(x / torch.sqrt(torch.tensor(2.0, device=device))))

def safe_log_cdf(x):
    # return log(Phi(x)) safely
    return torch.log(torch.clamp(0.5 * (1.0 + torch.erf(x / torch.sqrt(torch.tensor(2.0, device=device)))), min=1e-300))

def clamp_probs(p):
    return torch.clamp(p, min=1e-12)

# -----------------------------
# Data simulation (or load your data)
# -----------------------------
def simulate_data(n=1000, seed=2025):
    np.random.seed(seed)
    # True parameters
    beta_true = np.array([0.1, 0.2])
    # X can be binary or continuous: choose binary if requested
    # For the example we'll simulate binary X
    # X = np.random.binomial(1, 0.5, size=(n, 2)).astype(float)
    X = np.random.normal(size=(n,2))
    eps = logistic.rvs(size=n)  # logistic(0,1) true error
    y_star = X.dot(beta_true) + eps
    # thresholds
    alphas = np.array([-np.inf, -0.2, 0.3, np.inf])
    Y = np.digitize(y_star, alphas)  # gives 1..3
    return X, Y, alphas, beta_true

# -----------------------------
# Flow builder (1D)
# -----------------------------
def build_1d_flow(num_bins=16, num_layers=6, hidden_features=64, tail_bound=10.0):
    transforms = []
    for _ in range(num_layers):
        transforms.append(
            MaskedPiecewiseRationalQuadraticAutoregressiveTransform(
                features=1,
                hidden_features=hidden_features,
                num_bins=num_bins,
                tails='linear',
                tail_bound=tail_bound
            )
        )
        transforms.append(ReversePermutation(features=1))
    transform = CompositeTransform(transforms)
    base_dist = StandardNormal(shape=[1])
    flow = Flow(transform, base_dist)
    return flow


# Create the flow
flow = build_1d_flow(num_bins=16, num_layers=6)

# Test function
def test_flow_inverse(flow, n=8):
    print("=== Flow inverse/forward test ===")
    z = torch.randn(n, 1).to(device)
    print("Input z:", z[:3].detach().cpu().numpy())

    x, logdet_fwd = flow._transform.forward(z)
    print("Forward OK. Example x:", x[:3].detach().cpu().numpy())

    inv_out = flow._transform.inverse(x)
    z_back = inv_out[0] if isinstance(inv_out, tuple) else inv_out
    print("Inverse OK. Example z_back:", z_back[:3].detach().cpu().numpy())

    diff = (z - z_back).abs().max().item()
    print(f"Max reconstruction error: {diff:.2e}")

    if isinstance(inv_out, tuple) and len(inv_out) == 2:
        _, logdet_inv = inv_out
        sum_check = (logdet_fwd + logdet_inv).abs().mean().item()
        print(f"Mean |logdet_fwd + logdet_inv| ≈ {sum_check:.2e}")

    print("=== Test complete ===")

# Run test
test_flow_inverse(flow)

# -----------------------------
# Inversion helpers (flow inverse/forward)
# -----------------------------
# For nflows Flow, ._transform.inverse / ._transform.forward details are wrapped,
# but Flow has methods: log_prob(x), sample(n)
# We need an inverse mapping of value u (epsilon) to base z such that epsilon = T(z).
# For typical nflows flows, we can compute z by calling transform.inverse(epsilon) via flow._transform.inverse
# We'll use flow._transform.inverse; it's part of nflows API (internal) but available.

def flow_inverse(flow, eps_tensor):
    """
    eps_tensor: torch tensor shape (k,1) - values in epsilon space
    returns z tensor of same shape in base space
    """
    with torch.no_grad():
        # Flow has ._transform with .inverse method returning z and log_det_jacobian
        z, _ = flow._transform.inverse(eps_tensor)  # returns (z, logabsdet)
    return z

def flow_forward(flow, z_tensor):
    with torch.no_grad():
        eps, _ = flow._transform.forward(z_tensor)
    return eps

# -----------------------------
# Sampling from truncated standard normal in z-space
# -----------------------------
def sample_truncated_standard_normal(z_low, z_high, m=8):
    """
    z_low, z_high: torch tensors (scalars) on device
    returns tensor shape (m,1) of draws from truncated standard normal in [z_low, z_high]
    """
    # Use inverse CDF sampling: u ~ Unif(Phi(z_low), Phi(z_high)); z = Phi^{-1}(u)
    a = 0.5 * (1.0 + torch.erf(z_low / torch.sqrt(torch.tensor(2.0, device=device))))
    b = 0.5 * (1.0 + torch.erf(z_high / torch.sqrt(torch.tensor(2.0, device=device))))
    # clamp a,b
    a = torch.clamp(a, 1e-12, 1.0 - 1e-12)
    b = torch.clamp(b, 1e-12, 1.0 - 1e-12)
    if (b - a).item() <= 1e-12:
        # degenerate: return midpoint
        z_mid = 0.5 * (z_low + z_high)
        return z_mid.repeat(m, 1)
    us = torch.rand(m, device=device) * (b - a) + a
    # inverse CDF for standard normal
    z_draws = torch.from_numpy(np.array([_norm_ppf(u.item()) for u in us.cpu().numpy()], dtype=np.float32)).to(device)
    return z_draws.view(m, 1)

# We need a reliable inverse cdf (ppf) for normal on CPU; use scipy for ppf via numpy
from scipy.stats import norm as sp_norm
def _norm_ppf(u):
    return sp_norm.ppf(u)

# -----------------------------
# Observed-data log-likelihood evaluation
# -----------------------------
def torch_normal_cdf(z):
    """Standard normal CDF in PyTorch"""
    return 0.5 * (1 + torch.erf(z / torch.sqrt(torch.tensor(2.0, device=z.device))))

def torch_normal_ppf(u, eps=1e-6):
    """Approximate standard normal quantile (inverse CDF) in PyTorch"""
    u = torch.clamp(u, eps, 1-eps)
    return torch.sqrt(torch.tensor(2.0, device=u.device)) * torch.erfinv(2*u - 1)

def observed_loglik(flow, beta_vec, mu_param, X_tensor, Y_tensor):
    """Compute observed-data log-likelihood for ordinal outcomes"""
    n = X_tensor.shape[0]
    J = mu_param.shape[0] + 1
    mu_full = torch.cat([torch.tensor([-1e6], device=device), mu_param, torch.tensor([1e6], device=device)])
    ll = torch.tensor(0.0, device=device)
    for i in range(n):
        j = int(Y_tensor[i])
        V_i = (X_tensor[i] @ beta_vec).unsqueeze(0)
        u_low = mu_full[j-1] - V_i
        u_high = mu_full[j] - V_i
        z_low, _ = flow._transform.inverse(u_low.view(1,1))
        z_high, _ = flow._transform.inverse(u_high.view(1,1))
        cdf_low = torch_normal_cdf(z_low)
        cdf_high = torch_normal_cdf(z_high)
        p = torch.clamp(cdf_high - cdf_low, min=1e-12)
        ll = ll + torch.log(p)
    return ll.item()

# def observed_loglik(flow, beta_vec, alphas, X_tensor, Y_tensor):
#     """
#     Compute observed-data log-likelihood sum_i log( F(alphas_j - V_i) - F(alphas_{j-1} - V_i) )
#     where F(u) = Phi( T^{-1}(u) ).
#     """
#     with torch.no_grad():
#         V = X_tensor.matmul(beta_vec)  # (n,)
#         loglik_sum = 0.0
#         for i in range(X_tensor.shape[0]):
#             j = int(Y_tensor[i].item())
#             u_low = torch.tensor(alphas[j-1], dtype=torch.float32, device=device) - V[i]
#             u_high = torch.tensor(alphas[j], dtype=torch.float32, device=device) - V[i]
#             # map to z-space via inverse transform: z = T^{-1}(u)
#             z_low, _ = flow._transform.inverse(u_low.view(1,1))
#             z_high, _ = flow._transform.inverse(u_high.view(1,1))
#             # compute Phi(z_high) - Phi(z_low)
#             cdf_high = 0.5 * (1.0 + torch.erf(z_high / torch.sqrt(torch.tensor(2.0, device=device))))
#             cdf_low  = 0.5 * (1.0 + torch.erf(z_low / torch.sqrt(torch.tensor(2.0, device=device))))
#             p = torch.clamp(cdf_high - cdf_low, min=1e-12)
#             loglik_sum += torch.log(p)
#     return loglik_sum.item()

# -----------------------------
# MC-EM alternating algorithm
# -----------------------------
def run_mc_em(
    X, Y, flow,
    beta_init=None,
    mu_init=None,
    m_draws=8,
    flow_steps=100,
    flow_lr=1e-3,
    index_steps=200,
    index_lr=1e-3,
    n_iterations=6,
    normalize_index=True,
    verbose=True
):
    """
    MC-EM for 1D ordinal outcomes using normalizing flow residuals.
    X: (n,p) feature matrix
    Y: (n,) ordinal categories 1..J
    flow: nflows.Flow instance (1D)
    beta_init: initial beta vector (p,)
    mu_init: initial thresholds (length J+1)
    Returns: estimated beta, mu, flow, history
    """
    n, p = X.shape
    X_tensor = torch.tensor(X, dtype=torch.float32, device=device)
    Y_tensor = torch.tensor(Y, dtype=torch.int64, device=device)

    # Initialize beta
    if beta_init is None:
        beta_vec = nn.Parameter(torch.zeros(p, device=device))
    else:
        beta_vec = nn.Parameter(torch.tensor(beta_init.astype(np.float32), device=device))

    # Initialize thresholds
    J = int(Y.max())
    if mu_init is None:
        mu_init_arr = np.linspace(-1.0, 1.0, J+1)
    else:
        mu_init_arr = mu_init
    mu_param = nn.Parameter(torch.tensor(mu_init_arr[1:-1], dtype=torch.float32, device=device))

    # Flow optimizer
    flow_optimizer = optim.Adam(flow.parameters(), lr=flow_lr)

    history = {'loglik': [], 'beta': [], 'mu': []}

    for it in range(n_iterations):
        if verbose: print(f"\n--- MC-EM iteration {it+1}/{n_iterations} ---")

        # ---------- E-step: sample surrogate residuals ----------
        sampled_residuals = []
        mu_full = torch.cat([torch.tensor([-1e6], device=device), mu_param, torch.tensor([1e6], device=device)])

        for i in range(n):
            t0 = time.time()
            j = int(Y[i])
            V_i = (X_tensor[i] @ beta_vec).unsqueeze(0)
            u_low = mu_full[j-1] - V_i
            u_high = mu_full[j] - V_i
            # map to latent z via inverse flow
            with torch.no_grad():
                z_low, _ = flow._transform.inverse(u_low.view(1,1))
                z_high, _ = flow._transform.inverse(u_high.view(1,1))
            # truncated uniform sampling in CDF space
            cdf_low = torch_normal_cdf(z_low)
            cdf_high = torch_normal_cdf(z_high)
            if (cdf_high - cdf_low).item() < 1e-12:
                z_samples = 0.5 * (z_low + z_high) * torch.ones(m_draws,1, device=device)
            else:
                u_samples = torch.rand(m_draws,1, device=device) * (cdf_high - cdf_low) + cdf_low
                z_samples = torch_normal_ppf(u_samples)
            # map through flow forward
            with torch.no_grad():
                r_samples, _ = flow._transform.forward(z_samples)
            sampled_residuals.extend(r_samples.cpu().numpy().reshape(-1,1).tolist())

        sampled_residuals = torch.tensor(np.array(sampled_residuals, dtype=np.float32), dtype=torch.float32, device=device)

        # ---------- M-step: update flow ----------
        if verbose: print("Flow M-step: fitting flow to surrogate residuals (mc samples:", sampled_residuals.shape[0], ")")
        flow.train()
        for epoch in range(flow_steps):
            perm = torch.randperm(sampled_residuals.shape[0])
            for bstart in range(0, sampled_residuals.shape[0], 1024):
                idx = perm[bstart:bstart+1024]
                batch = sampled_residuals[idx]
                loss = -flow.log_prob(batch).mean()
                flow_optimizer.zero_grad()
                loss.backward()
                flow_optimizer.step()
            if verbose and epoch % max(1, flow_steps//4) == 0:
                with torch.no_grad():
                    ll = flow.log_prob(sampled_residuals[:min(1024,sampled_residuals.shape[0])]).mean().item()
                    print(f"  Flow epoch {epoch}/{flow_steps}, avg log-prob: {ll:.4f}")

        # ---------- M-step: update beta and thresholds ----------
        if verbose: print("Index M-step: updating beta (and thresholds) given fitted flow")
        beta_param = nn.Parameter(beta_vec.detach().clone())
        mu_param_local = nn.Parameter(mu_param.detach().clone())
        opt_idx = optim.Adam([beta_param, mu_param_local], lr=index_lr)

        for step in range(index_steps):
            if verbose: print(f"Index {step} in {index_steps}")
            opt_idx.zero_grad()
            V = X_tensor @ beta_param
            total_neglog = torch.tensor(0.0, device=device)
            mu_full = torch.cat([torch.tensor([-1e6], device=device), mu_param_local, torch.tensor([1e6], device=device)])
            for i in range(n):
                j = int(Y[i])
                u_low = mu_full[j-1] - V[i]
                u_high = mu_full[j] - V[i]
                z_low, _ = flow._transform.inverse(u_low.view(1,1))
                z_high, _ = flow._transform.inverse(u_high.view(1,1))
                cdf_low = torch_normal_cdf(z_low)
                cdf_high = torch_normal_cdf(z_high)
                p = torch.clamp(cdf_high - cdf_low, min=1e-12)
                total_neglog = total_neglog - torch.log(p)
            total_neglog.backward()
            opt_idx.step()
            # enforce monotone thresholds
            with torch.no_grad():
                mu_sorted, _ = torch.sort(mu_param_local)
                mu_param_local.copy_(mu_sorted)

        # update beta and thresholds
        with torch.no_grad():
            beta_vec.copy_(beta_param)
            mu_param.copy_(mu_param_local)

        # normalize index if needed
        if normalize_index:
            with torch.no_grad():
                V = X_tensor @ beta_vec
                V_mean = V.mean()
                V_std = V.std(unbiased=False)
                if V_std.item() > 1e-8:
                    beta_vec /= V_std
                    mu_param.copy_((mu_param - V_mean) / V_std)

        # compute observed log-likelihood
        flow.eval()
        obs_ll = observed_loglik(flow, beta_vec, mu_param, X_tensor, Y_tensor)
        history['loglik'].append(obs_ll)
        history['beta'].append(beta_vec.detach().cpu().numpy().copy())
        history['mu'].append(mu_param.detach().cpu().numpy().copy())

        if verbose:
            print(f"Iteration {it+1} done in {time.time()-t0:.2f}s, observed loglik = {obs_ll:.4f}, beta = {beta_vec.detach().cpu().numpy()}")

    return beta_vec.detach().cpu().numpy(), mu_param.detach().cpu().numpy(), flow, history


# -----------------------------
# Main: assemble everything and run
# -----------------------------
if __name__ == "__main__":
    # simulate or load data
    X, Y, alphas, beta_true = simulate_data(n=300, seed=2025)

    # build flow
    flow = build_1d_flow(num_bins=16, num_layers=6, hidden_features=64)

    # ordered probit warm start if available
    if USE_STATSMODELS:
        try:
            exog = pd.DataFrame(X, columns=[f"x{k}" for k in range(X.shape[1])])
            mod = OrderedModel(Y, exog, distr='probit')
            res = mod.fit(method='bfgs', disp=False)
            params = res.params
            # statsmodels packs thresholds then coefficients; extract last p as betas
            beta_init = np.array(params[:-X.shape[1]], dtype=float)
            mu_init = np.array(params[-X.shape[1]:], dtype=float)
            mu_init = np.append(mu_init, [-np.inf, np.inf])
            mu_init = np.sort(mu_init)
            print("Ordered-probit warm start beta_init:", beta_init)
            print("Ordered-probit warm start mu_init:", mu_init)
        except Exception as e:
            print("OrderedModel warm start failed:", e)
            beta_init = np.zeros(X.shape[1], dtype=float)
    else:
        # fallback: midpoint OLS
        midpoints = np.zeros(X.shape[0])
        for i in range(X.shape[0]):
            j = Y[i]
            lo, hi = alphas[j-1], alphas[j]
            if np.isfinite(lo) and np.isfinite(hi):
                midpoints[i] = 0.5*(lo+hi)
            elif np.isfinite(lo):
                midpoints[i] = lo + 1.0
            elif np.isfinite(hi):
                midpoints[i] = hi - 1.0
        beta_init = np.linalg.lstsq(X, midpoints, rcond=None)[0]
        print("Midpoint OLS warm start beta_init:", beta_init)
        mu_init = None

    # run MC-EM alternating algorithm
    beta_hat, mu_hat, flow_final, history = run_mc_em(
        X, Y, flow,
        beta_init=beta_init,
        mu_init=mu_init,
        m_draws=10,
        flow_steps=20,   # small for demo; increase 200+ in practice
        flow_lr=1e-3,
        index_steps=30,
        index_lr=5e-3,
        n_iterations=6,
        normalize_index=True,
        verbose=True
    )

    print("\nFinal beta_hat:", beta_hat)
    print("Final mu (internal thresholds):", mu_hat)
    print("Loglik history:", history['loglik'])

    # diagnostics: plot final flow density vs logistic
    xs = torch.linspace(-4, 4, 501, device=device).unsqueeze(1)
    with torch.no_grad():
        # compute log_prob on data space (epsilon)
        densities = torch.exp(flow_final.log_prob(xs)).cpu().numpy().squeeze()
    xs_np = xs.cpu().numpy().squeeze()
    plt.figure(figsize=(7,4))
    plt.plot(xs_np, densities, label="Fitted flow density")
    plt.plot(xs_np, logistic.pdf(xs_np), '--', label="True logistic")
    plt.title("Estimated error density (flow) vs true logistic")
    plt.legend()
    plt.savefig("flow.pdf")
