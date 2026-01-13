import numpy as np
import pandas as pd
import torch

from ordinal_flow_core import (
    device,
    train_ordered_flow,
    bootstrap_beta_se_flow
)

torch.set_default_dtype(torch.float64)

# Load and prepare data
#{{{
# Load data
df = pd.read_csv("../data/Reshon2023a_Israel_II_cleaned.csv")
df.columns

# recode variables
df["f_followThrough"] = pd.factorize(df["followThrough"], sort=True)[0] + 1

# subset
var_list = ["f_followThrough", "A", "Male", "age", "educ1", "know1", "combat", "milAssert1", "ideo1", "hawk", "isrlBrn"]
df_cleaned = df.loc[:, var_list]
df_cleaned = df_cleaned.dropna()

# Select columns for X and y from the DataFrame
y_pd = df_cleaned["f_followThrough"]
X_pd = df_cleaned[[x for x in var_list if x != "f_followThrough"]]
Z_pd = df_cleaned[[x for x in var_list if (x != "f_followThrough" and x != "A")]]

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
y = torch.tensor(
    y_np,
    dtype=torch.long
)
X = torch.tensor(
    X_pd.to_numpy(),
    dtype=torch.get_default_dtype()
)
Z = torch.tensor(
    Z_pd.to_numpy(),
    dtype=torch.get_default_dtype()
)

y = y.to(device)
X = X.to(device)
Z = Z.to(device)
# }}}

# Run the model
model = train_ordered_flow(
    X, y, Z=Z,
    flow_bins=32,
    bounds=12,
    epochs=1000,
    lr=1e-3,
    use_lbfgs=True,
    init_probit=True,
    verbose=True,
    monitor_nll=False,
)

# ----------------------------------------------------------------------
# 4. Point estimates (coefficients)
# ----------------------------------------------------------------------
# Exclude flow parameters from reported coefficients: only beta.
coef_vec = model.beta.detach().cpu().numpy()

p = coef_vec.shape[0]
coef_names = [f"beta_{j}" for j in range(p)]  # or replace with your own names


# ----------------------------------------------------------------------
# 6. Bootstrap standard errors for beta
# ----------------------------------------------------------------------
# B is the number of bootstrap replications; adjust for precision vs. time.
B = 0

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
    "se_boot": se_boot
})

print(summary.to_string(index=False))

summary.to_csv("../data/df_nf_bfgs.csv", index=False, encoding="utf-8-sig")
