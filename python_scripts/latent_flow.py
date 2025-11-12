import math
import numpy as np
import pandas as pd
import torch
import torch.nn as nn
import torch.optim as optim
from torch.distributions import Normal, Uniform

import matplotlib.pyplot as plt

# Use double precision for numerical stability
torch.set_default_dtype(torch.float64)
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

# nflows spline
from nflows.transforms.splines.rational_quadratic import rational_quadratic_spline

# statsmodels for ordered probit initialization
try:
    from statsmodels.miscmodels.ordinal_model import OrderedModel
except Exception as e:
    raise ImportError("statsmodels >= 0.12 is required for ordered probit initialization") from e

# ========= Numerics utilities =========
SQRT2 = math.sqrt(2.0)
LOG_HALF = math.log(0.5)

def softplus_inv(y):
    eps = 1e-12
    y = torch.clamp(y, min=eps)
    return torch.log(torch.expm1(y))

def log_ndtr(z):
    return LOG_HALF + torch.log(torch.special.erfc(-z / SQRT2))

def log_surv_ndtr(z):
    return LOG_HALF + torch.log(torch.special.erfc(z / SQRT2))

def log_diff_normal_cdfs(zl, zu):
    assert zl.shape == zu.shape
    left_mask = (zu <= 0) & (zl <= 0)
    right_mask = (zu >= 0) & (zl >= 0)
    mid_mask = ~(left_mask | right_mask)
    out = torch.empty_like(zu)

    if left_mask.any():
        a = log_ndtr(zu[left_mask])
        b = log_ndtr(zl[left_mask])
        out[left_mask] = a + torch.log1p(-torch.exp(b - a))

    if right_mask.any():
        a = log_surv_ndtr(zl[right_mask])
        b = log_surv_ndtr(zu[right_mask])
        out[right_mask] = a + torch.log1p(-torch.exp(b - a))

    if mid_mask.any():
        phi_u = Normal(0.0, 1.0).cdf(zu[mid_mask])
        phi_l = Normal(0.0, 1.0).cdf(zl[mid_mask])
        diff = torch.clamp(phi_u - phi_l, min=1e-300)
        out[mid_mask] = torch.log(diff)

    return out

# ========= Global 1D RQS transform (nflows) =========
class GlobalRQS1D(nn.Module):
    def __init__(self, K=16, bounds=8.0, min_bin_width=1e-3, min_bin_height=1e-3, min_derivative=1e-3):
        super().__init__()
        self.K = K
        self.left = -bounds
        self.right = bounds
        self.bottom = -bounds
        self.top = bounds
        self.min_bin_width = min_bin_width
        self.min_bin_height = min_bin_height
        self.min_derivative = min_derivative

        self.unnorm_widths = nn.Parameter(torch.zeros(K))
        self.unnorm_heights = nn.Parameter(torch.zeros(K))
        init_deriv = softplus_inv(torch.tensor(1.0))
        self.unnorm_derivs = nn.Parameter(torch.full((K + 1,), init_deriv))

    def _expand_params(self, batch_size):
        w = self.unnorm_widths.view(1, 1, -1).expand(batch_size, 1, self.K)
        h = self.unnorm_heights.view(1, 1, -1).expand(batch_size, 1, self.K)
        d = self.unnorm_derivs.view(1, 1, -1).expand(batch_size, 1, self.K + 1)
        return w, h, d

    def forward_with_logabsdet(self, z):
        if z.dim() == 1:
            z_in = z.unsqueeze(-1)
        else:
            z_in = z
        B = z_in.shape[0]
        w, h, d = self._expand_params(B)
        y, ladj = rational_quadratic_spline(
            inputs=z_in,
            unnormalized_widths=w,
            unnormalized_heights=h,
            unnormalized_derivatives=d,
            inverse=False,
            left=self.left,
            right=self.right,
            bottom=self.bottom,
            top=self.top,
            min_bin_width=self.min_bin_width,
            min_bin_height=self.min_bin_height,
            min_derivative=self.min_derivative,
        )
        return y.squeeze(-1), ladj.squeeze(-1)

    def forward(self, z):
        y, _ = self.forward_with_logabsdet(z)
        return y

    def inverse_with_logabsdet(self, e):
        if e.dim() == 1:
            e_in = e.unsqueeze(-1)
        else:
            e_in = e
        B = e_in.shape[0]
        w, h, d = self._expand_params(B)
        z, ladj = rational_quadratic_spline(
            inputs=e_in,
            unnormalized_widths=w,
            unnormalized_heights=h,
            unnormalized_derivatives=d,
            inverse=True,
            left=self.left,
            right=self.right,
            bottom=self.bottom,
            top=self.top,
            min_bin_width=self.min_bin_width,
            min_bin_height=self.min_bin_height,
            min_derivative=self.min_derivative,
        )
        return z.squeeze(-1), ladj.squeeze(-1)

    def inverse(self, e):
        z, _ = self.inverse_with_logabsdet(e)
        return z

    def sample_base(self, n):
        return torch.randn(n, device=self.unnorm_widths.device)

# ========= Ordered Flow Model =========
class OrderedFlowModel(nn.Module):
    def __init__(self, p, J, flow_bins=16, bounds=8.0, min_gap=1e-4):
        super().__init__()
        self.p = p
        self.J = J
        self.min_gap = min_gap

        self.beta = nn.Parameter(torch.zeros(p))
        self.alpha1 = nn.Parameter(torch.tensor(0.0))
        if J > 2:
            self.alpha_gaps_raw = nn.Parameter(torch.full((J - 2,), softplus_inv(torch.tensor(0.5))))
        else:
            self.alpha_gaps_raw = None

        self.flow = GlobalRQS1D(K=flow_bins, bounds=bounds)
        self.a_raw = nn.Parameter(softplus_inv(torch.tensor(1.0)))
        self.b = nn.Parameter(torch.tensor(0.0))
        self.base = Normal(0.0, 1.0)

    def a(self):
        return torch.nn.functional.softplus(self.a_raw)

    def alphas(self):
        if self.J == 2:
            return torch.stack([self.alpha1])
        gaps = torch.nn.functional.softplus(self.alpha_gaps_raw) + self.min_gap
        alphas = [self.alpha1]
        for g in gaps:
            alphas.append(alphas[-1] + g)
        return torch.stack(alphas)

    def z_bounds(self, eta, alpha_lower, alpha_upper):
        a = self.a()
        e_lower = alpha_lower - eta
        e_upper = alpha_upper - eta
        zl = torch.empty_like(e_lower)
        zu = torch.empty_like(e_upper)

        lower_is_finite = torch.isfinite(e_lower)
        upper_is_finite = torch.isfinite(e_upper)

        zl[~lower_is_finite] = -torch.inf
        zu[~upper_is_finite] = torch.inf

        if lower_is_finite.any():
            zl[lower_is_finite] = self.flow.inverse((e_lower[lower_is_finite] - self.b) / a)
        if upper_is_finite.any():
            zu[upper_is_finite] = self.flow.inverse((e_upper[upper_is_finite] - self.b) / a)

        return zl, zu

    def neg_loglik(self, X, y):
        eta = X.matmul(self.beta)
        alphas = self.alphas()
        alpha_full = torch.cat([
            torch.tensor([-torch.inf], device=X.device, dtype=X.dtype),
            alphas,
            torch.tensor([torch.inf], device=X.device, dtype=X.dtype)
        ], dim=0)
        y_idx = y.long()
        alpha_lower = alpha_full[y_idx - 1]
        alpha_upper = alpha_full[y_idx]
        zl, zu = self.z_bounds(eta, alpha_lower, alpha_upper)
        logp = log_diff_normal_cdfs(zl, zu)
        nll = -torch.sum(logp)
        return nll

    def predict_proba(self, X):
        N = X.shape[0]
        eta = X.matmul(self.beta)
        alphas = self.alphas()
        alpha_full = torch.cat([
            torch.tensor([-torch.inf], device=X.device, dtype=X.dtype),
            alphas,
            torch.tensor([torch.inf], device=X.device, dtype=X.dtype)
        ], dim=0)
        probs = []
        a = self.a()
        for j in range(1, self.J + 1):
            upper = alpha_full[j] - eta
            lower = alpha_full[j - 1] - eta
            zu = torch.empty_like(upper)
            zl = torch.empty_like(lower)
            upper_is_finite = torch.isfinite(upper)
            lower_is_finite = torch.isfinite(lower)
            zu[~upper_is_finite] = torch.inf
            zl[~lower_is_finite] = -torch.inf
            if upper_is_finite.any():
                zu[upper_is_finite] = self.flow.inverse((upper[upper_is_finite] - self.b) / a)
            if lower_is_finite.any():
                zl[lower_is_finite] = self.flow.inverse((lower[lower_is_finite] - self.b) / a)
            pu = Normal(0.0, 1.0).cdf(zu)
            pl = Normal(0.0, 1.0).cdf(zl)
            p = torch.clamp(pu - pl, min=1e-12, max=1.0)
            probs.append(p)
        return torch.stack(probs, dim=1)

    @torch.no_grad()
    def project_identification(self, mc_samples=32768):
        z = self.flow.sample_base(mc_samples).to(self.a_raw.device)
        eps_base, _ = self.flow.forward_with_logabsdet(z)
        eps = self.a() * eps_base + self.b
        m = eps.mean()
        s = eps.std(unbiased=False)
        s = torch.clamp(s, min=1e-8)
        a_new = self.a() / s
        b_new = (self.b - m) / s
        self.a_raw.data = softplus_inv(a_new).detach()
        self.b.data = b_new.detach()
        self.beta.data = (self.beta / s).detach()
        alphas = self.alphas().detach()
        alphas_new = (alphas - m) / s
        self.alpha1.data = alphas_new[0]
        if self.J > 2:
            gaps = alphas_new[1:] - alphas_new[:-1]
            gaps = torch.clamp(gaps, min=self.min_gap * 10)
            self.alpha_gaps_raw.data = softplus_inv(gaps - self.min_gap).detach()

    @torch.no_grad()
    def init_from_ordered_probit(self, X, y):
        y0 = y.cpu().numpy().astype(int) - 1
        X_np = X.cpu().numpy()
        cols = [f"x{k}" for k in range(X_np.shape[1])]
        dfX = pd.DataFrame(X_np, columns=cols)
        mod = OrderedModel(y0, dfX, distr='probit')
        res = mod.fit(method='bfgs', disp=False)
        beta_hat = res.params[cols].values
        thr_vals = res.params.values[-(self.J - 1):]
        alphas_hat = torch.tensor(np.array(thr_vals), dtype=torch.get_default_dtype())
        self.beta.data = torch.tensor(beta_hat, dtype=torch.get_default_dtype(), device=self.beta.device)
        self.alpha1.data = alphas_hat[0].to(self.alpha1.device)
        if self.J > 2:
            gaps = alphas_hat[1:] - alphas_hat[:-1]
            gaps = torch.clamp(gaps, min=self.min_gap * 10)
            self.alpha_gaps_raw.data = softplus_inv(gaps - self.min_gap).to(self.alpha_gaps_raw.device)
        self.a_raw.data = softplus_inv(torch.tensor(1.0))
        self.b.data = torch.tensor(0.0)

    @torch.no_grad()
    def learned_cdf_on_grid(self, egrid):
        a = self.a()
        z = self.flow.inverse((egrid - self.b) / a)
        return Normal(0.0, 1.0).cdf(z)

    @torch.no_grad()
    def learned_pdf_on_grid(self, egrid):
        a = self.a()
        z, ladj_inv = self.flow.inverse_with_logabsdet((egrid - self.b) / a)
        log_phi = -0.5 * z.pow(2) - 0.5 * math.log(2 * math.pi)
        log_dz_de = ladj_inv - torch.log(a)
        return torch.exp(log_phi + log_dz_de)

# ========= True error distributions: sampling + standardized CDF/PDF =========
def get_true_error(spec: str):
    """
    Returns:
      sample_eps(n): function sampling epsilon of size n on current device
      true_cdf_std(e): CDF of standardized error (mean 0, sd 1)
      true_pdf_std(e): PDF of standardized error (mean 0, sd 1)
      label: string label for plots
    """
    spec = spec.lower()

    # Helper: Normal CDF/PDF
    def Phi(x): return Normal(0.0, 1.0).cdf(x)
    def phi(x): return torch.exp(-0.5 * x**2) / math.sqrt(2 * math.pi)

    if spec == "lognormal":
        mu, sigma = 0.0, 1.0  # base parameters
        m = math.exp(mu + 0.5 * sigma**2)
        v = (math.exp(sigma**2) - 1.0) * math.exp(2 * mu + sigma**2)
        s = math.sqrt(v)

        def sample_eps(n):
            z = torch.randn(n, device=device)
            return torch.exp(mu + sigma * z)

        def F_base(x):
            # x>0: Phi((ln x - mu)/sigma); else 0
            out = torch.zeros_like(x)
            mask = x > 0
            if mask.any():
                out[mask] = Phi((torch.log(x[mask]) - mu) / sigma)
            return out

        def f_base(x):
            out = torch.zeros_like(x)
            mask = x > 0
            if mask.any():
                xm = x[mask]
                out[mask] = torch.exp(-0.5 * ((torch.log(xm) - mu) / sigma) ** 2) / (xm * sigma * math.sqrt(2 * math.pi))
            return out

        def true_cdf_std(e):
            x = m + s * e
            return F_base(x)

        def true_pdf_std(e):
            x = m + s * e
            return s * f_base(x)

        label = "Log-normal(μ=0,σ=1) standardized"

    elif spec == "t":
        nu = 5.0  # degrees of freedom (>2)
        m = 0.0
        s = math.sqrt(nu / (nu - 2.0))

        def sample_eps(n):
            # Sample from Student t via standard method (PyTorch has StudentT)
            return torch.distributions.StudentT(df=nu).sample((n,)).to(device)

        # PDF: gamma((ν+1)/2) / (sqrt(νπ) gamma(ν/2)) * (1 + x^2/ν)^(-(ν+1)/2)
        c = math.exp(torch.lgamma(torch.tensor((nu + 1)/2)).item() - torch.lgamma(torch.tensor(nu/2)).item()) / math.sqrt(nu * math.pi)
        def f_base(x):
            return c * (1.0 + (x**2) / nu) ** (-(nu + 1)/2)

        # CDF via regularized incomplete beta I_x(a,b); for x != 0
        def F_base(x):
            # Using formula: For x>0: 1 - 0.5*I_{nu/(x^2+nu)}(nu/2, 1/2); for x<0: 0.5*I_{nu/(x^2+nu)}(nu/2, 1/2)
            z = nu / (x**2 + nu)
            a = torch.tensor(nu/2, dtype=x.dtype, device=x.device)
            b = torch.tensor(0.5, dtype=x.dtype, device=x.device)
            I = torch.special.betainc(a, b, z)  # regularized
            F = torch.empty_like(x)
            pos = x > 0
            neg = x < 0
            zero = x == 0
            if pos.any():
                F[pos] = 1.0 - 0.5 * I[pos]
            if neg.any():
                F[neg] = 0.5 * I[neg]
            if zero.any():
                F[zero] = 0.5
            return F

        def true_cdf_std(e):
            x = m + s * e
            return F_base(x)

        def true_pdf_std(e):
            x = m + s * e
            return s * f_base(x)

        label = f"Student-t(ν={nu}) standardized"

    elif spec == "mixture_gaussian":
        # Two-component mixture
        weights = torch.tensor([0.6, 0.4], device=device)
        mus = torch.tensor([-1.0, 1.5], device=device)
        sigmas = torch.tensor([1.0, 0.5], device=device)
        m = (weights * mus).sum().item()
        v = (weights * (sigmas**2 + mus**2)).sum().item() - m**2
        s = math.sqrt(v)

        def sample_eps(n):
            cat = torch.distributions.Categorical(weights)
            comps = torch.distributions.Normal(mus, sigmas)
            idx = cat.sample((n,))
            eps = comps.sample((n,))
            return eps[torch.arange(n, device=device), idx]

        def F_base(x):
            # Weighted sum of normal CDFs
            X = x.unsqueeze(-1)
            cdfs = Normal(mus, sigmas).cdf(X)
            return (weights * cdfs).sum(dim=-1)

        def f_base(x):
            X = x.unsqueeze(-1)
            pdfs = torch.exp(Normal(mus, sigmas).log_prob(X))
            return (weights * pdfs).sum(dim=-1)

        def true_cdf_std(e):
            x = m + s * e
            return F_base(x)

        def true_pdf_std(e):
            x = m + s * e
            return s * f_base(x)

        label = "Mixture Gaussian [0.6 N(-1,1^2) + 0.4 N(1.5,0.5^2)] standardized"

    elif spec == "chisq":
        k = 5.0  # degrees of freedom
        m = k
        s = math.sqrt(2.0 * k)

        def sample_eps(n):
            return torch.distributions.Chi2(df=k).sample((n,)).to(device)

        def F_base(x):
            out = torch.zeros_like(x)
            mask = x >= 0
            if mask.any():
                # Regularized lower incomplete gamma P(k/2, x/2)
                a = torch.tensor(k/2, dtype=x.dtype, device=x.device)
                z = x[mask] / 2.0
                out[mask] = torch.special.gammainc(a, z)  # regularized lower gamma
            return out

        def f_base(x):
            out = torch.zeros_like(x)
            mask = x >= 0
            if mask.any():
                xm = x[mask]
                coeff = 1.0 / ( (2.0**(k/2)) * math.gamma(k/2) )
                out[mask] = coeff * xm**(k/2 - 1.0) * torch.exp(-xm / 2.0)
            return out

        def true_cdf_std(e):
            x = m + s * e
            return F_base(x)

        def true_pdf_std(e):
            x = m + s * e
            return s * f_base(x)

        label = f"Chi-square(k={int(k)}) standardized"

    elif spec == "exponential":
        lam = 1.0  # rate
        m = 1.0 / lam
        s = 1.0 / lam

        def sample_eps(n):
            return torch.distributions.Exponential(rate=lam).sample((n,)).to(device)

        def F_base(x):
            out = torch.zeros_like(x)
            mask = x >= 0
            if mask.any():
                out[mask] = 1.0 - torch.exp(-lam * x[mask])
            return out

        def f_base(x):
            out = torch.zeros_like(x)
            mask = x >= 0
            if mask.any():
                out[mask] = lam * torch.exp(-lam * x[mask])
            return out

        def true_cdf_std(e):
            x = m + s * e
            return F_base(x)

        def true_pdf_std(e):
            x = m + s * e
            return s * f_base(x)

        label = f"Exponential(λ={lam}) standardized"

    elif spec == "uniform":
        a_u, b_u = 0.0, 1.0
        m = 0.5 * (a_u + b_u)
        s = (b_u - a_u) / math.sqrt(12.0)

        def sample_eps(n):
            return torch.distributions.Uniform(low=a_u, high=b_u).sample((n,)).to(device)

        def F_base(x):
            out = torch.zeros_like(x)
            out = torch.where(x <= a_u, torch.zeros_like(x), out)
            out = torch.where(x >= b_u, torch.ones_like(x), out)
            mask = (x > a_u) & (x < b_u)
            out = torch.where(mask, (x - a_u) / (b_u - a_u), out)
            return out

        def f_base(x):
            out = torch.zeros_like(x)
            mask = (x >= a_u) & (x <= b_u)
            out = torch.where(mask, torch.full_like(x, 1.0 / (b_u - a_u)), out)
            return out

        def true_cdf_std(e):
            x = m + s * e
            return F_base(x)

        def true_pdf_std(e):
            x = m + s * e
            return s * f_base(x)

        label = f"Uniform[{a_u},{b_u}] standardized"

    else:
        raise ValueError("spec must be one of: 'lognormal', 't', 'mixture_gaussian', 'chisq', 'exponential', 'uniform'")

    return sample_eps, true_cdf_std, true_pdf_std, label

# ========= Simulation using chosen true error =========
def simulate_ordinal_with_error(n, beta_true, alphas_true, eps_sampler, seed=123):
    torch.manual_seed(seed)
    p = beta_true.shape[0]
    X = torch.randn(n, p)
    eta = X.matmul(beta_true)
    eps = eps_sampler(n)
    y_star = eta + eps
    J = len(alphas_true) + 1
    y = torch.empty(n, dtype=torch.long)
    # Assign categories based on thresholds α
    if J == 2:
        y[:] = (y_star > alphas_true[0]).long() + 1
    else:
        for i in range(n):
            if y_star[i] <= alphas_true[0]:
                y[i] = 1
            elif y_star[i] <= alphas_true[1]:
                y[i] = 2
            else:
                y[i] = 3
    return X, y

# ========= Training demo with selectable true error and plots =========
def train_demo(true_error="lognormal", epochs=2000, flow_bins=8, bounds=10.0, n=1000, beta_true=None, alphas_true=None, seed=76898):
    J = alphas_true.shape[0] + 1

    # Get true error sampler and standardized CDF/PDF for plotting
    eps_sampler, true_cdf_std, true_pdf_std, label = get_true_error(true_error)

    # Simulate data
    X, y = simulate_ordinal_with_error(n, beta_true, alphas_true, eps_sampler, seed=seed)

    # Build model
    model = OrderedFlowModel(p=X.shape[1], J=J, flow_bins=flow_bins, bounds=bounds).to(device)

    # Initialization from ordered probit
    model.init_from_ordered_probit(X, y)

    # Move data to device
    X = X.to(device)
    y = y.to(device)

    # Optimizer
    opt = optim.Adam(model.parameters(), lr=1e-2)

    # Learning history
    nll_hist = []
    ks_hist = []

    # Grid for plotting (standardized error scale)
    egrid = torch.linspace(-6, 6, 601, device=device)

    # Training loop with projection after each step
    for ep in range(1, epochs + 1):
        opt.zero_grad()
        loss = model.neg_loglik(X, y)
        loss.backward()
        opt.step()
        model.project_identification(mc_samples=32768)

        nll_hist.append(loss.item())
        with torch.no_grad():
            F_learned = model.learned_cdf_on_grid(egrid)
            F_true = true_cdf_std(egrid)
            ks = torch.max(torch.abs(F_learned - F_true)).item()
            ks_hist.append(ks)

        if ep % 100 == 0 or ep == 1:
            with torch.no_grad():
                alphas_est = model.alphas().cpu().numpy()
                beta_est = model.beta.detach().cpu().numpy()
                print(f"[{true_error}] Epoch {ep:4d} | NLL {loss.item():.3f} | KS {ks:.4f} | beta {np.round(beta_est,4)} | alphas {np.round(alphas_est,4)}")

    # Final results and plots
    with torch.no_grad():
        print(f"Final estimates (true error: {label}):")
        print("Estimated beta:", model.beta.cpu().numpy())
        print("Estimated alphas:", model.alphas().cpu().numpy())

        # Compute learned and true standardized CDF/PDF on grid
        F_learned = model.learned_cdf_on_grid(egrid).cpu().numpy()
        f_learned = model.learned_pdf_on_grid(egrid).cpu().numpy()
        F_true = true_cdf_std(egrid).cpu().numpy()
        f_true = true_pdf_std(egrid).cpu().numpy()
        egrid_np = egrid.cpu().numpy()

        # Plot CDFs
        plt.figure(figsize=(6,4))
        plt.plot(egrid_np, F_true, label=f"True CDF ({label})", lw=2)
        plt.plot(egrid_np, F_learned, label="Learned CDF", lw=2, linestyle="--")
        plt.title("Error distribution CDF (standardized)")
        plt.xlabel("e (standardized)")
        plt.ylabel("F(e)")
        plt.legend()
        plt.grid(alpha=0.3)
        plt.tight_layout()
        plt.savefig("../figures/latent_flow_cdf.pdf")
        # plt.show()

        # Plot PDFs
        plt.figure(figsize=(6,4))
        plt.plot(egrid_np, f_true, label=f"True PDF ({label})", lw=2)
        plt.plot(egrid_np, f_learned, label="Learned flow PDF", lw=2, linestyle="--")
        plt.title("Error distribution PDF (standardized)")
        plt.xlabel("e (standardized)")
        plt.ylabel("f(e)")
        plt.legend()
        plt.grid(alpha=0.3)
        plt.tight_layout()
        plt.savefig("../figures/latent_flow_pdf.pdf")
        # plt.show()

        # Plot learning history
        fig, ax = plt.subplots(1,2, figsize=(10,4))
        ax[0].plot(np.arange(1, epochs+1), nll_hist, lw=1.5)
        ax[0].set_title("Negative log-likelihood")
        ax[0].set_xlabel("Epoch")
        ax[0].set_ylabel("NLL")
        ax[0].grid(alpha=0.3)

        ax[1].plot(np.arange(1, epochs+1), ks_hist, lw=1.5, color="tab:orange")
        ax[1].set_title("Max |F_learned - F_true| (KS-like)")
        ax[1].set_xlabel("Epoch")
        ax[1].set_ylabel("Max abs CDF diff")
        ax[1].grid(alpha=0.3)

        plt.tight_layout()
        plt.savefig("../figures/latent_flow_learning.pdf")
        # plt.show()

if __name__ == "__main__":
    # Choose one of: 'lognormal', 't', 'mixture_gaussian', 'chisq', 'exponential', 'uniform'
    # True DGP
    beta_true = torch.tensor([0.1, 1, -2], dtype=float)
    alphas_true = torch.tensor([-0.2, 0.3], dtype=float)
    # J = 4
    train_demo(true_error="chisq", epochs=3000, flow_bins=8, bounds=10.0, n=1000, beta_true=beta_true, alphas_true=alphas_true, seed=633333330)
    # You can call train_demo with other true_error values to test different DGPs.
