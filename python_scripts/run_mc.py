# ========================= 02_run_mc.py =========================
"""
Script 2: Run Monte Carlo for ordered probit/logit and flow-based model.

Tasks:
1. Load population and index CSVs for a given error spec and N.
2. For each sample size n (and R replications), run:
   - ordered probit,
   - ordered logit,
   - flow-based ordinal model.
3. Save MC summary with columns:
   - n (sample size),
   - model ('flow','probit','logit'),
   - bias (scalar summary over coefficients),
   - rmse (scalar summary over coefficients),
   - true_beta_normalized (stringified vector for reference).
"""

import os
import math
import numpy as np
import pandas as pd
import torch

from ordinal_flow_core import (
    get_true_error,
    train_ordered_flow,
    fit_ordered_sm,
)

torch.set_default_dtype(torch.float64)

def mc_for_one_n(
    df_pop,
    df_idx,
    n,
    beta_true,
    true_error_spec,
    R,
    flow_epochs=200,
    flow_lr=1e-2,           # base lr for beta
    flow_bins=16,
    flow_bounds=10.0,
    burn_in_checks=10,
    neg_patience=10,
    check_every=50,
    delta_eps=1e-2,
    logit_norm_criterion='variance',
):
    """
    For a fixed sample size n, run MC over R replications given indices.

    Returns a list of dicts, one per model, with keys:
    - n, model, bias, rmse, true_beta_normalized.
    """

    # True error sd for normalization
    _, _, _, label, m_eps, s_eps = get_true_error(true_error_spec)
    beta_true_np = beta_true.detach().cpu().numpy()
    if (s_eps is not None) and np.isfinite(s_eps) and (s_eps > 0):
        beta_true_norm = beta_true_np / float(s_eps)
    else:
        beta_true_norm = beta_true_np

    p = beta_true_np.shape[0]

    # Prepare arrays to store estimates
    beta_hat_flow = np.zeros((R, p))
    beta_hat_probit = np.zeros((R, p))
    beta_hat_logit = np.zeros((R, p))

    # Subset index DataFrame for this n
    df_n = df_idx[df_idx["n"] == n].copy()
    # Ensure reps go from 0..R-1
    assert df_n["rep"].max() >= R - 1, "Not enough replications in index CSV for this n"

    for r in range(R):
        df_r = df_n[df_n["rep"] == r]
        idx = df_r["idx"].values.astype(int)
        # Extract sample from population
        sample = df_pop.iloc[idx]
        y = torch.from_numpy(sample["y"].values.astype(int))
        # Build X from x0..x{p-1}
        X_cols = [f"x{j}" for j in range(p)]
        X = torch.from_numpy(sample[X_cols].values)

        # --- Flow-based model ---
        model_flow = train_ordered_flow(
            X, y, Z=None,
            flow_bins=flow_bins,
            bounds=flow_bounds,
            epochs=flow_epochs,
            lr=flow_lr,           # base lr for beta
            init_probit=True,
            verbose=False,
            monitor_nll=True,
            check_every=check_every,
            burn_in_checks=burn_in_checks,
            neg_patience=neg_patience,
            delta_eps=delta_eps
        )
        print("delta values: ", model_flow.delta_history)

        beta_hat_flow[r, :] = model_flow.beta.detach().cpu().numpy()

        # --- Ordered Probit ---
        try:
            b_probit, thr_p, res_p = fit_ordered_sm(y, X, link='probit', normalize=True)
            beta_hat_probit[r, :] = b_probit
        except Exception:
            beta_hat_probit[r, :] = np.nan

        # --- Ordered Logit (scale to unit variance) ---
        try:
            b_logit, thr_l, res_l = fit_ordered_sm(y, X, link='logit', normalize=True,
                                                   norm_criterion=logit_norm_criterion)
            beta_hat_logit[r, :] = b_logit
        except Exception:
            beta_hat_logit[r, :] = np.nan

        if (r+1) % 10 == 0:
            print(f"  n={n}: finished replication {r+1}/{R}")

        # Function to compute bias and RMSE per coefficient
        def summarize(est):
            """
            est: array of shape (R, p), where R is number of replications and p is number of coefficients.
            Returns:
                bias_vec: shape (p,), mean error per coefficient
                rmse_vec: shape (p,), RMSE per coefficient
            """
            Err = est - beta_true_norm[None, :]  # (R, p)
            bias_vec = Err.mean(axis=0)          # (p,)
            rmse_vec = np.sqrt((Err**2).mean(axis=0))  # (p,)
            return bias_vec, rmse_vec
    
        # Compute per-coefficient bias and RMSE for each model
        bias_flow,   rmse_flow   = summarize(beta_hat_flow)
        bias_probit, rmse_probit = summarize(beta_hat_probit)
        bias_logit,  rmse_logit  = summarize(beta_hat_logit)
    
        p = beta_true_norm.shape[0]
    
        results = []
        for j in range(p):
            # Flow
            results.append({
                "n": int(n),
                "model": "flow",
                "coef": j,  # or use a name like f"beta{j}"
                "bias": float(bias_flow[j]),
                "rmse": float(rmse_flow[j]),
                "true_beta_normalized": float(beta_true_norm[j]),
            })
            # Probit
            results.append({
                "n": int(n),
                "model": "probit",
                "coef": j,
                "bias": float(bias_probit[j]),
                "rmse": float(rmse_probit[j]),
                "true_beta_normalized": float(beta_true_norm[j]),
            })
            # Logit
            results.append({
                "n": int(n),
                "model": "logit",
                "coef": j,
                "bias": float(bias_logit[j]),
                "rmse": float(rmse_logit[j]),
                "true_beta_normalized": float(beta_true_norm[j]),
            })
    return results


def main():
    # -------------------- Configuration --------------------
    out_dir = "../mc_results"
    os.makedirs(out_dir, exist_ok=True)

    N = int(1e6)
    # true_error_spec = "normal"
    # true_error_spec = "mixture_gaussian"
    true_error_spec = "lognormal"
    sample_sizes = [200, 500, 1000, 2000]
    # sample_sizes = [200, 500]
    # R = 10
    R = 100
    # R = 200
    p = 5
    J = 5

    beta_true = torch.tensor([1.0, 0.8, -0.8, 0.5, -0.5], dtype=torch.get_default_dtype())

    # -------------------- Load population and indices --------------------
    pop_csv = f"../sim_data/population_{true_error_spec}_N{N}.csv"
    idx_csv = f"../sim_data/indices_{true_error_spec}_N{N}.csv"
    df_pop = pd.read_csv(pop_csv)
    df_idx = pd.read_csv(idx_csv)

    print(f"Loaded population from {pop_csv}, shape={df_pop.shape}")
    print(f"Loaded indices from {idx_csv}, shape={df_idx.shape}")

    # -------------------- Run MC for each sample size --------------------
    all_results = []

    for n in sample_sizes:
        print(f"Running MC for sample size n={n}")
        res_n = mc_for_one_n(
            df_pop=df_pop,
            df_idx=df_idx,
            n=n,
            beta_true=beta_true,
            true_error_spec=true_error_spec,
            R=R,
            flow_epochs=10000,
            flow_bins=32,
            flow_bounds=12.0,
            flow_lr=1e-1,
            burn_in_checks=1000,
            neg_patience=10,
            check_every=100,
            delta_eps=1e-5,
            logit_norm_criterion='variance',
        )
        all_results.extend(res_n)

    df_res = pd.DataFrame(all_results)
    out_csv = os.path.join(out_dir, f"mc_{R}_{true_error_spec}_test.csv")
    df_res.to_csv(out_csv, index=False)
    # print(f"Saved MC summary to: {out_csv}")
    print(df_res.loc[df_res.coef == 0, :])

if __name__ == "__main__":
    main()
