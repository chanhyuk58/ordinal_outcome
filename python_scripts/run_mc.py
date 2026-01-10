# ========================= 02_run_mc.py =========================
"""
MC for ordered regression models (oprobit, ologit, flow-based ordinal regression).

Normalization and output:
- Normalize coefficients by the second coefficient (beta[1]) for bias/RMSE.
- Output columns:
  "sample_size", "model", "coef_index", "true_beta_norm",
  "coef_estimate", "mean_beta_norm", "bias", "rmse", "error_sd"

where:
  - coef_estimate   = mean of raw (unnormalized) coefficient estimates
  - mean_beta_norm  = mean of normalized coefficient estimates
  - bias, rmse      = bias/RMSE in normalized space vs true_beta_norm
  - error_sd:
      * flow   : 1.0 (flow output already normalized)
      * probit : 1.0 (standard normal latent error)
      * logit  : pi / sqrt(3) (standard logistic latent error)
"""

import os
import numpy as np
import pandas as pd
import torch

from ordinal_flow_core import (
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
    flow_epochs=2000,
    flow_lr=1e-2,
    flow_bins=16,
    flow_bounds=10.0,
    burn_in_checks=500,
    neg_patience=10,
    check_every=10,
    delta_eps=1e-2,
    logit_norm_criterion="variance",
):
    """
    For a fixed sample size n, run MC over R replications given indices.

    Returns a list of dicts, one per model and coefficient, with keys:
        "sample_size", "model", "coef_index", "true_beta_norm",
        "coef_estimate", "mean_beta_norm", "bias", "rmse", "error_sd".
    """

    # True beta (numpy) and normalized by second coefficient
    beta_true_np = beta_true.detach().cpu().numpy()
    p = beta_true_np.shape[0]
    if beta_true_np[1] == 0:
        raise ValueError("beta_true[1] (second coefficient) is zero; cannot normalize by it.")
    beta_true_norm = beta_true_np / beta_true_np[1]

    # Prepare arrays to store raw estimates
    beta_hat_flow = np.full((R, p), np.nan)
    beta_hat_probit = np.full((R, p), np.nan)
    beta_hat_logit = np.full((R, p), np.nan)

    # Subset index DataFrame for this n
    df_n = df_idx[df_idx["n"] == n].copy()
    # Ensure there are enough replications
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
            X,
            y,
            Z=None,
            flow_bins=flow_bins,
            bounds=flow_bounds,
            epochs=flow_epochs,
            lr=flow_lr,
            init_probit=True,
            verbose=True,
            monitor_nll=True,
            check_every=check_every,
            burn_in_checks=burn_in_checks,
            neg_patience=neg_patience,
            delta_eps=delta_eps,
        )
        beta_hat_flow[r, :] = model_flow.beta.detach().cpu().numpy()

        # --- Ordered Probit ---
        try:
            b_probit, thr_p, res_p = fit_ordered_sm(
                y, X, link="probit", normalize=False
            )
            beta_hat_probit[r, :] = b_probit
        except Exception:
            beta_hat_probit[r, :] = np.nan

        # --- Ordered Logit ---
        try:
            b_logit, thr_l, res_l = fit_ordered_sm(
                y,
                X,
                link="logit",
                normalize=False,
                norm_criterion=logit_norm_criterion,
            )
            beta_hat_logit[r, :] = b_logit
        except Exception:
            beta_hat_logit[r, :] = np.nan

        if (r + 1) % 10 == 0:
            print(f"  n={n}: finished replication {r+1}/{R}")

    # ------------- After all R replications: normalize and summarize -------------

    def normalize_by_second(est):
        """
        est: array (R, p) of raw coefficient estimates.
        Returns:
            est_norm: array (R, p) normalized by the second coefficient in each row.
                      Rows with invalid/zero second coefficient are set to NaN.
        """
        est_norm = est.copy()
        denom = est[:, 1]
        valid = np.isfinite(denom) & (np.abs(denom) > 1e-8)
        est_norm[~valid, :] = np.nan
        est_norm[valid, :] = est[valid, :] / denom[valid, None]
        return est_norm

    def summarize(est_raw, est_norm):
        """
        est_raw:  array (R, p) of raw estimates.
        est_norm: array (R, p) of normalized estimates.
        Returns:
            mean_raw:  length-p vector of mean raw estimates
            mean_norm: length-p vector of mean normalized estimates
            bias_vec:  length-p vector of bias in normalized space
            rmse_vec:  length-p vector of RMSE in normalized space
        """
        mean_raw = np.nanmean(est_raw, axis=0)

        Err = est_norm - beta_true_norm[None, :]
        mean_norm = np.nanmean(est_norm, axis=0)
        bias_vec = np.nanmean(Err, axis=0)
        rmse_vec = np.sqrt(np.nanmean(Err**2, axis=0))

        return mean_raw, mean_norm, bias_vec, rmse_vec

    # Normalize by second coefficient
    flow_norm   = normalize_by_second(beta_hat_flow)
    probit_norm = normalize_by_second(beta_hat_probit)
    logit_norm  = normalize_by_second(beta_hat_logit)

    # Summaries
    flow_raw_mean,   flow_mean_norm,   flow_bias,   flow_rmse   = summarize(beta_hat_flow,   flow_norm)
    probit_raw_mean, probit_mean_norm, probit_bias, probit_rmse = summarize(beta_hat_probit, probit_norm)
    logit_raw_mean,  logit_mean_norm,  logit_bias,  logit_rmse  = summarize(beta_hat_logit,  logit_norm)

    # Model-specific error SDs
    error_sd_flow = 1.0
    error_sd_probit = 1.0
    error_sd_logit = float(np.pi / np.sqrt(3.0))

    # Assemble results per coefficient
    results = []
    for j in range(p):
        # Flow
        results.append(
            {
                "sample_size": int(n),
                "model": "flow",
                "coef_index": int(j),
                "true_beta_norm": float(beta_true_norm[j]),
                "coef_estimate": float(flow_raw_mean[j]),
                "mean_beta_norm": float(flow_mean_norm[j]),
                "bias": float(flow_bias[j]),
                "rmse": float(flow_rmse[j]),
                "error_sd": float(error_sd_flow),
            }
        )
        # Probit
        results.append(
            {
                "sample_size": int(n),
                "model": "probit",
                "coef_index": int(j),
                "true_beta_norm": float(beta_true_norm[j]),
                "coef_estimate": float(probit_raw_mean[j]),
                "mean_beta_norm": float(probit_mean_norm[j]),
                "bias": float(probit_bias[j]),
                "rmse": float(probit_rmse[j]),
                "error_sd": float(error_sd_probit),
            }
        )
        # Logit
        results.append(
            {
                "sample_size": int(n),
                "model": "logit",
                "coef_index": int(j),
                "true_beta_norm": float(beta_true_norm[j]),
                "coef_estimate": float(logit_raw_mean[j]),
                "mean_beta_norm": float(logit_mean_norm[j]),
                "bias": float(logit_bias[j]),
                "rmse": float(logit_rmse[j]),
                "error_sd": float(error_sd_logit),
            }
        )

    return results


def main():
    # -------------------- Configuration --------------------
    out_dir = "../mc_results"
    os.makedirs(out_dir, exist_ok=True)

    N = int(1e6)
    true_error_spec = "lognormal"
    # true_error_spec = "mixture_gaussian"
    # sample_sizes = [200, 500, 1000, 2000]
    sample_sizes = [500]
    R = 10
    p = 5

    beta_true = torch.tensor([2.0, 0.8, -1.2, 1.0, -0.7], dtype=torch.get_default_dtype())
    # beta_true = torch.tensor([1.6, 0.7, -1.2, 1.0, -0.8], dtype=torch.get_default_dtype())

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
            burn_in_checks=3000,
            neg_patience=10,
            check_every=10,
            delta_eps=1e-5,
            logit_norm_criterion="variance",
        )
        all_results.extend(res_n)

    df_res = pd.DataFrame(all_results)

    # Ensure requested column order
    cols = [
        "sample_size",
        "model",
        "coef_index",
        "true_beta_norm",
        "coef_estimate",
        "mean_beta_norm",
        "bias",
        "rmse",
        "error_sd",
    ]
    df_res = df_res[cols]

    out_csv = os.path.join(out_dir, f"mc_{R}_{true_error_spec}.csv")
    df_res.to_csv(out_csv, index=False)
    print(f"Saved MC summary to: {out_csv}")
    print(df_res.loc[df_res.coef_index == 0, :])


if __name__ == "__main__":
    main()
