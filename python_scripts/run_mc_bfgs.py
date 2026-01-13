# ========================= 02_run_mc.py (Revised for BFGS) =========================
import os
import math
import numpy as np
import pandas as pd
import torch
import torch.optim as optim

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
    # Updated Hyperparameters for BFGS
    adam_warmup_epochs=200,   # Was flow_epochs; reduced because BFGS finishes the job
    flow_lr=1e-2,             
    use_lbfgs=True,           # New toggle
    lbfgs_steps=50,           # New: max L-BFGS iterations
    flow_bins=16,
    flow_bounds=10.0,
    logit_norm_criterion='variance',
):
    """
    For a fixed sample size n, run MC over R replications using Hybrid BFGS Flow.
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

    df_n = df_idx[df_idx["n"] == n].copy()
    
    for r in range(R):
        df_r = df_n[df_n["rep"] == r]
        idx = df_r["idx"].values.astype(int)
        sample = df_pop.iloc[idx]
        y = torch.from_numpy(sample["y"].values.astype(int))
        X_cols = [f"x{j}" for j in range(p)]
        X = torch.from_numpy(sample[X_cols].values)

        # --- Flow-based model (using BFGS) ---
        model_flow = train_ordered_flow(
            X, y, Z=None,
            flow_bins=flow_bins,
            bounds=flow_bounds,
            epochs=adam_warmup_epochs, # Adam phase
            lr=flow_lr,                # Adam phase lr
            use_lbfgs=use_lbfgs,       # Enable BFGS
            lbfgs_steps=lbfgs_steps,
            init_probit=True,
            verbose=False,
            monitor_nll=False          # L-BFGS has its own convergence logic
        )

        beta_hat_flow[r, :] = model_flow.beta.detach().cpu().numpy()

        # --- Ordered Probit ---
        try:
            b_probit, thr_p, res_p = fit_ordered_sm(y, X, link='probit', normalize=True)
            beta_hat_probit[r, :] = b_probit
        except Exception:
            beta_hat_probit[r, :] = np.nan

        # --- Ordered Logit ---
        try:
            b_logit, thr_l, res_l = fit_ordered_sm(y, X, link='logit', normalize=True,
                                                   norm_criterion=logit_norm_criterion)
            beta_hat_logit[r, :] = b_logit
        except Exception:
            beta_hat_logit[r, :] = np.nan

        if (r+1) % 10 == 0:
            print(f"  n={n}: finished replication {r+1}/{R}")

    # Function to compute bias and RMSE (Moved outside rep loop for efficiency)
    def summarize(est):
        Err = est - beta_true_norm[None, :]
        bias_vec = np.nanmean(Err, axis=0) # Use nanmean in case a rep failed
        rmse_vec = np.sqrt(np.nanmean(Err**2, axis=0))
        return bias_vec, rmse_vec

    bias_flow,   rmse_flow   = summarize(beta_hat_flow)
    bias_probit, rmse_probit = summarize(beta_hat_probit)
    bias_logit,  rmse_logit  = summarize(beta_hat_logit)

    results = []
    for j in range(p):
        for model_name, b, r in zip(["flow", "probit", "logit"], 
                                     [bias_flow, bias_probit, bias_logit], 
                                     [rmse_flow, rmse_probit, rmse_logit]):
            results.append({
                "n": int(n),
                "model": model_name,
                "coef": j,
                "bias": float(b[j]),
                "rmse": float(r[j]),
                "true_beta_normalized": float(beta_true_norm[j]),
            })
            
    return results


def main():
    # ... (Population loading code same as before) ...
    N = int(1e6)
    true_error_spec = "lognormal"
    sample_sizes = [200, 500, 1000, 2000]
    R = 100
    
    beta_true = torch.tensor([1.0, 0.8, -0.8, 0.5, -0.5])

    # -------------------- Load population and indices --------------------
    pop_csv = f"../sim_data/population_{true_error_spec}_N{N}.csv"
    idx_csv = f"../sim_data/indices_{true_error_spec}_N{N}.csv"
    out_dir = f"../mc_results/"
    df_pop = pd.read_csv(pop_csv)
    df_idx = pd.read_csv(idx_csv)

    print(f"Loaded population from {pop_csv}, shape={df_pop.shape}")
    print(f"Loaded indices from {idx_csv}, shape={df_idx.shape}")

    # -------------------- Run MC for each sample size --------------------

    all_results = []
    for n in sample_sizes:
        print(f"Running MC for sample size n={n} using L-BFGS Flow")
        res_n = mc_for_one_n(
            df_pop=df_pop,
            df_idx=df_idx,
            n=n,
            beta_true=beta_true,
            true_error_spec=true_error_spec,
            R=R,
            # Optimized for L-BFGS:
            adam_warmup_epochs=1000,  # Just enough to stabilize the spline
            lbfgs_steps=30,          # BFGS usually converges in 15-30 steps
            flow_bins=12,
            flow_bounds=10.0,
            flow_lr=1e-3,            # Lower LR is safer for warm-up
            logit_norm_criterion='variance',
        )
        all_results.extend(res_n)

    df_res = pd.DataFrame(all_results)
    out_csv = os.path.join(out_dir, f"mc_{R}_{true_error_spec}_test.csv")
    df_res.to_csv(out_csv, index=False)
    print(df_res.loc[df_res.coef == 0, :])

if __name__ == "__main__":
    main()
