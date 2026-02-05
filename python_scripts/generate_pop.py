# ========================= 01_generate_population.py =========================
"""
Script 1: Generate population, MC indices, and latent-space plot.

Tasks:
1. Generate ordinal outcome data with large N as "population".
2. Generate Monte Carlo sampling indices for given n and R.
3. Plot the density of latent y_star and thresholds.
"""

import os
import math
import numpy as np
import pandas as pd
import torch
import matplotlib.pyplot as plt

from ordinal_flow_core import (
    get_true_error,
)
# ======== Simulation with generalized thresholds ========

def simulate_X_with_two_binary(n, p, bin_idx=(0, 1), bin_prob=(0.5, 0.5), seed=None): # {{{
    """
    Create X with two Bernoulli columns (indices in bin_idx) and remaining columns ~ N(0,1).
    The Bernoulli columns are iid and independent from continuous columns.
    """
    import numpy as np
    import torch
    if seed is not None:
        torch.manual_seed(int(seed))
        np.random.seed(int(seed))
    X = torch.randn(n, p)
    for j, pr in zip(bin_idx, bin_prob):
        X[:, j] = torch.bernoulli(torch.full((n,), float(pr), dtype=torch.get_default_dtype()))
    # Mask for non-binary columns (used to build Z)
    mask_nonbin = torch.ones(p, dtype=torch.bool)
    mask_nonbin[list(bin_idx)] = False
    return X, mask_nonbin
# }}}

def make_thr_params_for_Z(J, q, min_gap=0.2, seed=None, strength=0.5): # {{{
    """
    Construct default thr_params dict for simulate_ordinal_general where
    thresholds depend only on Z (dimension q). Positive gaps enforced.
    """
    import numpy as np
    if seed is not None:
        rng = np.random.default_rng(int(seed))
    else:
        rng = np.random.default_rng()
    alpha1_intercept = rng.normal(loc=0.0, scale=1.0)
    alpha1_gamma = rng.normal(loc=0.0, scale=strength, size=q) if q > 0 else None
    if J > 2:
        base_gaps = np.abs(rng.normal(loc=min_gap + 0.5, scale=0.2, size=J - 2)) + min_gap
        gap_gammas = rng.normal(loc=0.0, scale=strength, size=(J - 2, q)) if q > 0 else None
    else:
        base_gaps = np.array([])
        gap_gammas = None
    thr_params = {
        'alpha1_intercept': float(alpha1_intercept),
        'alpha1_gamma': alpha1_gamma,
        'gap_intercepts': base_gaps,
        'gap_gammas': gap_gammas
    }
    return thr_params
# }}}

def simulate_ordinal_general(n, beta_true, J, eps_sampler, Z=None, thr_params=None, seed=123, X=None): # {{{
    """
    Simulate ordinal outcomes with generalized thresholds.
    If X is provided, use it; otherwise simulate X ~ N(0,1).
    If Z is provided, thresholds depend on Z; else thresholds are intercept-only.

    Returns
    -------
    X, Z, y, y_star, eps, alphas_mat
    """
    import torch
    torch.manual_seed(int(seed))
    if X is None:
        p = beta_true.shape[0]
        X = torch.randn(n, p)
    eta = X.matmul(beta_true)
    eps = eps_sampler(n)
    # Build thresholds per observation
    if Z is None:
        q = 0
    else:
        q = Z.shape[1]
    if thr_params is None:
        # default: constant thresholds evenly spaced
        if J == 2:
            alphas_mat = torch.zeros(n, 1, dtype=torch.get_default_dtype())
        else:
            base = torch.linspace(-1.0, 1.0, J - 1, dtype=torch.get_default_dtype())
            alphas_mat = base.unsqueeze(0).expand(n, J - 1).clone()
    else:
        a1_int = float(thr_params['alpha1_intercept'])
        if q > 0 and thr_params.get('alpha1_gamma', None) is not None:
            alpha1 = torch.tensor(a1_int, dtype=torch.get_default_dtype()) + torch.as_tensor(Z).matmul(
                torch.as_tensor(thr_params['alpha1_gamma'], dtype=torch.get_default_dtype())
            )
        else:
            alpha1 = torch.full((n,), a1_int, dtype=torch.get_default_dtype())
        alphas_mat = torch.empty(n, J - 1, dtype=torch.get_default_dtype())
        alphas_mat[:, 0] = alpha1
        if J > 2:
            gaps = torch.as_tensor(thr_params['gap_intercepts'], dtype=torch.get_default_dtype())  # (J-2,)
            if q > 0 and thr_params.get('gap_gammas', None) is not None:
                gaps_i = gaps.unsqueeze(0) + torch.as_tensor(Z).matmul(
                    torch.as_tensor(thr_params['gap_gammas'], dtype=torch.get_default_dtype()).T
                )
                gaps_i = torch.clamp(gaps_i, min=1e-6)
            else:
                gaps_i = gaps.unsqueeze(0).expand(n, J - 2)
            for k in range(1, J - 1):
                alphas_mat[:, k] = alphas_mat[:, k - 1] + gaps_i[:, k - 1]
    y_star = eta + eps
    # Categorize
    y = torch.empty(n, dtype=torch.long)
    for i in range(n):
        yi = 1
        for j in range(J - 1):
            if y_star[i] > alphas_mat[i, j]:
                yi = j + 2
        y[i] = yi
    return X, Z, y, y_star, eps, alphas_mat
# }}}


torch.set_default_dtype(torch.float64)

def main():
    # -------------------- Configuration --------------------
    out_dir = "../sim_data"
    os.makedirs(out_dir, exist_ok=True)

    # Population settings
    N = int(1e6)          # population size
    p = 5                 # number of covariates
    J = 5                 # number of ordinal categories
    true_error_spec = "lognormal"  # or 'mixture_gaussian', 'lognormal', 'normal'
    seed = 23048

    # True beta (latent index coefficients)
    beta_true = torch.tensor([1.0, 0.8, -0.8, 0.5, -0.5], dtype=torch.get_default_dtype())

    # Thresholds: here constant across observations: [-2.0, -0.5, 0.5, 2.0]
    alpha1 = -2.0
    gaps = np.array([1.5, 1.0, 1.5], dtype=float)  # J-2 = 3 gaps for J=5
    # gaps = np.array([1.5, 2.0, 1.5], dtype=float)  # J-2 = 3 gaps for J=5
    thr_true = {
        "alpha1_intercept": alpha1,
        "alpha1_gamma": None,
        "gap_intercepts": gaps,
        "gap_gammas": None,
    }

    # MC sampling design
    # sample_sizes = [200, 500]  # n's
    sample_sizes = [200, 500, 1000, 2000]  # n's
    # R = 10                                # replications
    R = 1000

    # -------------------- Step 1: Generate population --------------------
    np.random.seed(seed)
    torch.manual_seed(seed)

    # True error distribution
    eps_sampler, true_cdf_std, true_pdf_std, label, m_eps, s_eps = get_true_error(true_error_spec)
    print(f"True error distribution: {label}, mean={m_eps}, sd={s_eps}")

    # Generate X with two binary columns
    bin_idx = (0, 1)
    bin_prob = (0.5, 0.5)
    X_pop, mask_nonbin = simulate_X_with_two_binary(N, p, bin_idx=bin_idx, bin_prob=bin_prob, seed=seed)
    # For this generator, thresholds do not depend on Z (we use const thresholds)
    Z_pop = None

    # Simulate ordinal outcome
    X_pop, Z_pop, y_pop, y_star_pop, eps_pop, alphas_pop = simulate_ordinal_general(
        N, beta_true, J, eps_sampler, Z=Z_pop, thr_params=thr_true, seed=seed, X=X_pop
    )

    # Collect thresholds (they are same across obs when thr_params has no gammas)
    thr_example = alphas_pop[0, :].cpu().numpy()

    # Build population DataFrame for CSV
    df_pop = pd.DataFrame()

    df_pop["id"] = np.arange(N)
    df_pop["y"] = y_pop.cpu().numpy().astype(int)
    df_pop["y_star"] = y_star_pop.cpu().numpy()
    df_pop["eps"] = eps_pop.cpu().numpy()

    # Add X columns
    for j in range(p):
        df_pop[f"x{j}"] = X_pop[:, j].cpu().numpy()

    # Add threshold columns (for reference)
    for k in range(J - 1):
        df_pop[f"thr{k+1}"] = alphas_pop[:, k].cpu().numpy()

    pop_csv = os.path.join(out_dir, f"population_{true_error_spec}_N{N}.csv")
    df_pop.to_csv(pop_csv, index=False)
    print(f"Saved population to: {pop_csv}")

    # -------------------- Step 2: Generate MC indices --------------------
    rng = np.random.default_rng(seed + 1)
    records = []

    for n in sample_sizes:
        for r in range(R):
            idx = rng.integers(low=0, high=N, size=n)
            # Record each index as a row: (n, rep, idx)
            for i in idx:
                records.append((int(n), int(r), int(i)))

    df_idx = pd.DataFrame(records, columns=["n", "rep", "idx"])
    idx_csv = os.path.join(out_dir, f"indices_{true_error_spec}_N{N}.csv")
    df_idx.to_csv(idx_csv, index=False)
    print(f"Saved MC indices to: {idx_csv}")

    # -------------------- Step 3: Plot latent y_star and thresholds --------------------

    # For plotting, we may subsample if N is very large
    max_plot = 200000
    if N > max_plot:
        samp_idx = np.random.choice(N, size=max_plot, replace=False)
        y_star_plot = y_star_pop[samp_idx].cpu().numpy()
    else:
        y_star_plot = y_star_pop.cpu().numpy()

    plt.figure(figsize=(8, 5))
    # Density via histogram or KDE
    plt.hist(y_star_plot, bins=200, density=True, alpha=0.4, color="steelblue", label="Latent y* density")

    # Add vertical lines for thresholds (one example vector)
    for t in thr_example:
        plt.axvline(t, color="red", linestyle="--", linewidth=1)

    x_min, x_max = plt.gca().get_xlim()
    plt.axhline(0, color='black', linewidth=1)
    plt.axvline(x_min, color='black', linewidth=1)
    plt.xlabel("Y*")
    plt.ylabel("Density")

    plot_path = os.path.join(f"../figures/latent_space_{true_error_spec}_N{N}.pdf")
    plt.tight_layout()
    plt.box(False)
    plt.savefig(plot_path)
    plt.close()
    print(f"Saved latent space plot to: {plot_path}")

    # -------------------- Step 4: Generate Zoomed Figure --------------------
    plt.figure(figsize=(8, 5))
    
    # Use the same histogram data but restrict the range
    # Range is set to slightly outside your min/max thresholds
    zoom_range = (min(thr_example) - 2, max(thr_example) + 3)
    
    plt.hist(y_star_plot, bins=200, range=zoom_range, density=True, 
             alpha=0.4, color="steelblue", label="Latent y*")

    # Thresholds and Category Labels
    # We add -infinity and +infinity to the thresholds to find midpoints for all categories
    extended_thrs = np.concatenate([[-5], thr_example, [5]]) 
    
    for i in range(len(extended_thrs) - 1):
        # Calculate midpoint for the label
        mid = (extended_thrs[i] + extended_thrs[i+1]) / 2
        plt.text(mid, 0.02, f"Cat {i+1}", ha='center', fontweight='bold', color='darkred')
        
    # Draw the actual threshold lines
    for t in thr_example:
        plt.axvline(t, color="red", linestyle="--", linewidth=1.5)

    plt.axhline(0, color='black', linewidth=1)
    plt.axvline(zoom_range[0], color='black', linewidth=1)
    plt.xlim(zoom_range)
    plt.xlabel("Y*")
    plt.ylabel("Density")

    zoom_plot_path = os.path.join(f"../figures/latent_zoom_{true_error_spec}_N{N}.pdf")
    plt.tight_layout()
    plt.box(False)
    plt.savefig(zoom_plot_path)
    plt.close()
    print(f"Saved zoomed plot to: {zoom_plot_path}")

if __name__ == "__main__":
    main()
