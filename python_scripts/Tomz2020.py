import numpy as np
import pandas as pd
import torch

from ordinal_flow_core import (
    device,
    get_true_error,
    simulate_ordinal_general,
    train_ordered_flow,
    bootstrap_beta_se_flow
)

torch.set_default_dtype(torch.float64)

# Load and prepare data
#{{{
# Load data
tomz = pd.read_stata("../replications/Tomz2020a/TomzWeeks-HumanRights-JOP-Files/2012-10-01-Main-YouGov/output/2012-10-01-Main-prepped.dta")
tomz.columns

# recode variables
tomz["f_strike5"] = pd.factorize(tomz["strike5"], sort=True)[0] + 1
tomz["hrtsdemoc"] = tomz["hrts"] * tomz["democ"]

# subset
var_list = ["f_strike5", "hrts", "democ", "hrtsdemoc", "h1", "i1", "p1", "e1", "r1", "male", "white", "age", "ed4"]
tomz_cleand = tomz.loc[:, var_list]

# Select columns for X and y from the DataFrame
y_pd = tomz["f_strike5"]
X_pd = tomz[["hrts", "democ", "h1", "i1", "p1", "e1", "r1", "male", "white", "age", "ed4"]]

# Case A: y is already integers like 1,...,J
# (Check and, if needed, convert type)
if pd.api.types.is_integer_dtype(y_pd):
    y_np = y_pd.to_numpy()
else:
    # Case B: y is categorical / string / arbitrary labels
    # Turn into 0,...,J-1 then add 1 to get 1,...,J
    y_cat = pd.Categorical(y_pd)
    y_codes = y_cat.codes          # 0,...,J-1
    y_np = y_codes + 1             # 1,...,J

# Convert X and y to torch tensors with correct dtypes
X = torch.tensor(
    X_pd.to_numpy(),
    dtype=torch.get_default_dtype()
)
y = torch.tensor(
    y_np,
    dtype=torch.long
)

X = X.to(device)
y = y.to(device)
# }}}

# Run the model
model = train_ordered_flow(
    X, y, Z=None,
    flow_bins=32,
    bounds=12,
    epochs=50000,
    lr=1e-1,
    init_probit=True,
    verbose=True,
    burn_in_checks=1000,
    neg_patience=5,
)

max(model.nll_history, key = lambda x: x[0])
min(model.nll_history, key = lambda x: x[1])
max(model.delta_history, key = lambda x: x[1])
model.nll_probit

# ----------------------------------------------------------------------
# 4. Point estimates (coefficients)
# ----------------------------------------------------------------------
# Exclude flow parameters from reported coefficients: only beta.
coef_vec = model.beta.detach().cpu().numpy()
coef_vec_rel = coef_vec / coef_vec[1]

p = coef_vec.shape[0]
coef_names = [f"beta_{j}" for j in range(p)]  # or replace with your own names


# ----------------------------------------------------------------------
# 6. Bootstrap standard errors for beta
# ----------------------------------------------------------------------
# B is the number of bootstrap replications; adjust for precision vs. time.
B = 100

# Use the dedicated bootstrap function that only bootstraps beta coefficients.
# Note: this will internally re-fit the model on the full sample, independent
# of the 'model' you already trained above. If you want to reuse the existing
# model, you can skip the full-sample fit inside bootstrap_beta_se_flow and
# modify that function accordingly.
full_model_boot, beta_hat_boot, se_boot = bootstrap_beta_se_flow(
    X=X.cpu(),
    y=y.cpu(),
    Z=None,
    B=B,
    flow_bins=16,
    bounds=10.0,
    epochs=5000,
    lr=1e-3,
    init_probit=True,
    verbose=True,
    train_kwargs={
        "monitor_nll": True,  # usually turn off monitoring inside bootstrap
        "neg_patience": 5      # optional: slightly more aggressive stopping
    },
    return_bootstrap_betas=False
)

# Sanity check: bootstrap SE length must match the number of beta coefficients.
if len(se_boot) != len(coef_vec):
    raise RuntimeError(
        f"Mismatch between bootstrap SE length ({len(se_boot)}) "
        f"and coefficient vector length ({len(coef_vec)})."
    )


# ----------------------------------------------------------------------
# 7. Combine into a summary table
# ----------------------------------------------------------------------
summary = pd.DataFrame({
    "param": coef_names,
    "coef": coef_vec,
    "coef_rel": coef_vec_rel,
    "se_boot": se_boot
})

print(summary.to_string(index=False))

summary.to_csv("../data/tomz_nf(2).csv", index=False, encoding="utf-8-sig")
