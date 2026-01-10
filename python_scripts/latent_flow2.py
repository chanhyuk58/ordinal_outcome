import orjson

import math
import numpy as np
import pandas as pd
import torch
import torch.nn as nn
import torch.optim as optim
from torch.distributions import Normal
from scipy import stats
from nflows.transforms.splines.rational_quadratic import (
    unconstrained_rational_quadratic_spline as uRQS,

# Numerics

# ======== Monte Carlo harness ========
# def monte_carlo_compare(
#     R, n, beta_true, J, true_error_spec='mixture_gaussian', Z=None, 
#     thr_true=None, epochs=800, lr=1e-2, flow_bins=16, bounds=10.0, 
#     seed=1234, normalize_beta=True, logit_norm_criterion='variance'
# ): # {{{
#     """
#     Compare models. If normalize_beta=True, compare against beta_true / s_eps (true error sd).
#     Also normalize logit output to unit-variance scale for fair comparison.
#     """
#     rng = np.random.default_rng(seed)
#     p = beta_true.shape[0]
#     q = 0 if Z is None else Z.shape[1]
#     eps_sampler, true_cdf_std, true_pdf_std, label, m_eps, s_eps = get_true_error(true_error_spec)
# 
#     # Comparator truth on the normalized unit-variance scale
#     beta_true_np = beta_true.detach().cpu().numpy()
#     if normalize_beta and (s_eps is not None) and np.isfinite(s_eps) and s_eps > 0:
#         beta_true_cmp = beta_true_np / float(s_eps)
#     else:
#         beta_true_cmp = beta_true_np
# 
#     beta_flow = []
#     beta_probit = []
#     beta_logit = []
#     nll_flow = []
#     nll_probit = []
#     nll_logit = []
# 
#     for r in range(R):
#         seed_r = rng.integers(1, 10**9)
#         Zr = None if Z is None else torch.randn(n, Z.shape[1])
#         Xr, Zr, y, y_star, eps, alphas_mat = simulate_ordinal_general(
#             n, beta_true, J, eps_sampler, Z=Zr, thr_params=thr_true, seed=int(seed_r)
#         )
# 
#         # Ordered Flow
#         model = train_ordered_flow(Xr, y, Zr, flow_bins=flow_bins, bounds=bounds, epochs=epochs, lr=lr, init_probit=True, verbose=False)
#         beta_flow.append(model.beta.detach().cpu().numpy())
#         with torch.no_grad():
#             probs_flow = model.predict_proba(Xr.to(device), Zr.to(device) if Zr is not None else None).cpu().numpy()
#             y_idx = y.numpy().astype(int) - 1
#             p_y = np.clip(probs_flow[np.arange(n), y_idx], 1e-12, 1.0)
#             nll_flow.append(-np.sum(np.log(p_y)))
# 
#         # Ordered probit (already unit-variance)
#         try:
#             b_probit, thr_p, res_p = fit_ordered_sm(y, Xr, link='probit', normalize=True)
#             beta_probit.append(b_probit)
#             nll_probit.append(-res_p.llf)
#         except Exception:
#             beta_probit.append(np.full(p, np.nan))
#             nll_probit.append(np.nan)
# 
#         # Ordered logit (normalize to unit-variance)
#         try:
#             b_logit, thr_l, res_l = fit_ordered_sm(y, Xr, link='logit', normalize=True, norm_criterion=logit_norm_criterion)
#             beta_logit.append(b_logit)
#             nll_logit.append(-res_l.llf)
#         except Exception:
#             beta_logit.append(np.full(p, np.nan))
#             nll_logit.append(np.nan)
# 
#     beta_flow = np.vstack(beta_flow)
#     beta_probit = np.vstack(beta_probit)
#     beta_logit = np.vstack(beta_logit)
# 
#     out = {
#         'beta_true_norm': beta_true_cmp,
#         'flow_mean': beta_flow.mean(axis=0),
#         'flow_bias': beta_flow.mean(axis=0) - beta_true_cmp,
#         'flow_rmse': np.sqrt(((beta_flow - beta_true_cmp)**2).mean(axis=0)),
#         'probit_mean': beta_probit.mean(axis=0),
#         'probit_bias': beta_probit.mean(axis=0) - beta_true_cmp,
#         'probit_rmse': np.sqrt(((beta_probit - beta_true_cmp)**2).mean(axis=0)),
#         'logit_mean': beta_logit.mean(axis=0),
#         'logit_bias': beta_logit.mean(axis=0) - beta_true_cmp,
#         'logit_rmse': np.sqrt(((beta_logit - beta_true_cmp)**2).mean(axis=0)),
#         'nll_flow_mean': np.nanmean(nll_flow),
#         'nll_probit_mean': np.nanmean(nll_probit),
#         'nll_logit_mean': np.nanmean(nll_logit),
#         'true_error_label': label,
#         's_error': s_eps,
#         'm_error': m_eps,
#         'logit_norm_criterion': logit_norm_criterion
#     }
#     return out
# # }}}


def monte_carlo_compare_two_binary(
    R, n, beta_true, J, true_error_spec='mixture_gaussian',
    p=None, bin_idx=(0, 1), bin_prob=(0.5, 0.5),
    thr_true=None, epochs=800, lr=1e-2,
    flow_bins=16, bounds=10.0, seed=1234,
    normalize_beta=True, logit_norm_criterion='variance'): # {{{
    """
    Monte Carlo where two regressors in X are randomized binary and excluded from thresholds.
    All other regressors (continuous) determine thresholds via Z = X[:, non-binary].
    Assumes train_ordered_flow and model.predict_proba are available.
    """
    import numpy as np
    import torch

    for r in range(R):
        sample = population[idx]
        beta_flow, beta_probit, beta_logit = [], [], []
        nll_flow, nll_probit, nll_logit = [], [], []
        
        # Fit Ordered Flow (with Z driving thresholds)
        model = train_ordered_flow(Xr, y, Zr, flow_bins=flow_bins, bounds=bounds,
                                   epochs=epochs, lr=lr, init_probit=True, verbose=False)
        beta_flow.append(model.beta.detach().cpu().numpy())
        with torch.no_grad():
            dev = next(model.parameters()).device
            probs_flow = model.predict_proba(Xr.to(dev), Zr.to(dev) if Zr is not None else None).cpu().numpy()
            y_idx = y.numpy().astype(int) - 1
            p_y = np.clip(probs_flow[np.arange(n), y_idx], 1e-12, 1.0)
            nll_flow.append(-np.sum(np.log(p_y)))

        # Ordered Probit (parallel thresholds)
        try:
            b_probit, thr_p, res_p = fit_ordered_sm(y, Xr, link='probit', normalize=True)
            beta_probit.append(b_probit)
            nll_probit.append(-res_p.llf)
        except Exception:
            beta_probit.append(np.full(p, np.nan))
            nll_probit.append(np.nan)

        # Ordered Logit (normalize output to unit variance)
        try:
            b_logit, thr_l, res_l = fit_ordered_sm(y, Xr, link='logit', normalize=True, norm_criterion=logit_norm_criterion)
            beta_logit.append(b_logit)
            nll_logit.append(-res_l.llf)
        except Exception:
            beta_logit.append(np.full(p, np.nan))
            nll_logit.append(np.nan)

    beta_flow = np.vstack(beta_flow)
    beta_probit = np.vstack(beta_probit)
    beta_logit = np.vstack(beta_logit)

    out = {
        'beta_true_norm': beta_true_cmp,
        'flow_bias': beta_flow.mean(axis=0) - beta_true_cmp,
        'flow_rmse': np.sqrt(((beta_flow - beta_true_cmp)**2).mean(axis=0)),
        'probit_bias': beta_probit.mean(axis=0) - beta_true_cmp,
        'probit_rmse': np.sqrt(((beta_probit - beta_true_cmp)**2).mean(axis=0)),
        'logit_bias': beta_logit.mean(axis=0) - beta_true_cmp,
        'logit_rmse': np.sqrt(((beta_logit - beta_true_cmp)**2).mean(axis=0)),
    }
    return out
# }}}

# ======== Example usage (adapting your main) ========
if __name__ == "__main__":
    import numpy as np
    import torch

    torch.manual_seed(8994343)
    np.random.seed(8994343)

    # Data generating process
    n = 1000
    p = 5
    J = 5

    # True coefficients
    beta_true = torch.tensor([1.0, 0.8, -0.8, 0.5, -0.5], dtype=torch.get_default_dtype())

    # Z consists of non-binary columns only (thresholds depend only on Z)
    # if mask_nonbin.sum() > 0:
    #     Z = X[:, mask_nonbin].clone()
    #     q = Z.shape[1]  # should be p - 2 = 2
    # else:
    #     Z = None
    #     q = 0

    # thresholds:
    alpha1 = -2.0
    gaps = np.array([1.5, 1.0, 1.5], dtype=float)  # length J-2 = 3 for J=5

    thr_true = {
        "alpha1_intercept": alpha1,
        "alpha1_gamma": None,        # no covariate-dependent thresholds
        "gap_intercepts": gaps,
        "gap_gammas": None           # no covariate-dependent gaps
    }


    # Generalized thresholds: depending on Z
    # thr_true = {
    #     'alpha1_intercept': -2.0,
    #     'alpha1_gamma': np.array([0.5, -0.5]),         # alpha1 varies with Z (length q)
    #     'gap_intercepts': np.array([0.8, 1.0, 2.5]),   # positive gaps (length J-2)
    #     'gap_gammas': np.array([
    #         [0.1, 0.3],
    #         [0.3, 0.1],
    #         [0.5, -0.1],
    #     ])
    # }

    # True error specification
    true_error = "lognormal"
    # eps_sampler, true_cdf_std, true_pdf_std, label, m_true, s_true = get_true_error(true_error)

    # Hessian-based SEs for coefficients (optionally include flow params)
    # se_df, Cov = se_from_hessian(model, X.to(device), y.to(device), Z.to(device), include_flow=True, ridge=1e-5)
    # print(se_df.head(20))

    # Bootstrap SEs (may take time)
    # boot_df, boot_estimates, base_vec, names = bootstrap_se(lambda p,J,q,fb,bd: OrderedFlowModel(p,J,q,fb,bd), X, y, J, Z=Z, B=100)


    # Generate population

    eps_sampler, true_cdf_std, true_pdf_std, label, m_eps, s_eps = get_true_error(true_error_spec)
    beta_true_np = beta_true.detach().cpu().numpy()
    s_norm = float(s_eps) if (s_eps is not None and np.isfinite(s_eps) and s_eps > 0) else 1.0
    beta_true_cmp = beta_true_np / s_norm if normalize_beta else beta_true_np

    rng = np.random.default_rng(seed)
    if p is None:
        p = beta_true.shape[0]
    assert p == beta_true.shape[0], "beta_true length must match p"

    N = 1e+6

    seed_r = int(rng.integers(1, 10**9))
    # Generate X with two binary columns
    Xr, mask_nonbin = simulate_X_with_two_binary(N, p, bin_idx=bin_idx, bin_prob=bin_prob, seed=seed_r)
    # Z is all non-binary columns: thresholds depend only on Z
    if mask_nonbin.sum() > 0:
        Zr = Xr[:, mask_nonbin].clone()
        q = Zr.shape[1]
    else:
        Zr = None
        q = 0
    # Build default thr params if none provided (for q dims)
    thr_r = thr_true
    if thr_r is None:
        thr_r = make_thr_params_for_Z(J, q, min_gap=0.2, seed=seed_r, strength=0.5)
    else:
        # sanity-check shapes if provided
        if q == 0:
            assert (thr_r.get('alpha1_gamma', None) is None) and (thr_r.get('gap_gammas', None) is None), \
                "Provided thr_true includes gammas but q=0 (no Z columns)."
        else:
            if thr_r.get('alpha1_gamma', None) is not None:
                assert len(thr_r['alpha1_gamma']) == q, "alpha1_gamma length must match q"
            if thr_r.get('gap_gammas', None) is not None:
                assert thr_r['gap_gammas'].shape == (J - 2, q), "gap_gammas must be (J-2, q)"
    # Simulate outcomes using X and Z
    Xr, Zr, y, y_star, eps, alphas_mat = simulate_ordinal_general(
        N, beta_true, J, eps_sampler, Z=Zr, thr_params=thr_r, seed=seed_r, X=Xr
    )
    pop = 


    # Monte Carlo comparison with two binary regressors excluded from thresholds
    for n in [200, 500, 1000, 2000]:
        print(f"Working on n = {n}")
        rep = 200

        mc_out = monte_carlo_compare_two_binary(
            R=rep,
            n=n,
            beta_true=beta_true,
            J=J,
            p=p,
            bin_idx=(0, 1),
            bin_prob=(0.5, 0.5),
            true_error_spec=true_error,
            thr_true=thr_true,      # or None to randomize threshold parameters each replication
            epochs=500,             # adjust for runtime
            lr=1e-2,
            flow_bins=16,
            # bounds=12.0,
            seed=78230403,
            normalize_beta=True,
            logit_norm_criterion='variance'
        )

        file_path = f"./mc_{rep}_{true_error}_{n}.json"

        with open(file_path, "w", encoding="utf-8") as json_file:
            json_file.write(
                orjson.dumps(
                    mc_out,
                    option=orjson.OPT_SERIALIZE_NUMPY | orjson.OPT_INDENT_2
                ).decode()
            )

        print("MC summary:")
        for k, v in mc_out.items():
            print(k, v)
