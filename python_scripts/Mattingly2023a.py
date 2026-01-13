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
df = pd.read_csv("../data/Mattingly2023a.csv")

# recode variables
df["f_political_model"] = pd.factorize(df["political_model"], sort=True)[0] + 1
df["f_econ_model"] = pd.factorize(df["econ_model"], sort=True)[0] + 1
df["f_world_leader"] = pd.factorize(df["world_leader"], sort=True)[0] + 1

df = pd.concat([df, pd.get_dummies(df.treatment, prefix="treatment")], axis=1)
df = pd.concat([df, pd.get_dummies(df.country, prefix="country")], axis=1)
df = pd.concat([df, pd.get_dummies(df.gender, prefix="gender")], axis=1)
df.columns

# subset
var_list = (["f_political_model", "f_econ_model", "f_world_leader", "age", "education", "national_pride", "leftright"] +
            [col for col in df if col.startswith("treatment")] + 
            [col for col in df if col.startswith("country")] + 
            [col for col in df if col.startswith("gender")]
            )
cov_list = (["age", "education", "national_pride", "leftright"] +
            [col for col in df if col.startswith("treatment")] + 
            [col for col in df if col.startswith("country")] + 
            [col for col in df if col.startswith("gender")]
            )
cov_list2 = (["age", "education", "national_pride", "leftright"] +
            [col for col in df if col.startswith("country")] + 
            [col for col in df if col.startswith("gender")]
            )
df_cleaned = df.loc[:, var_list]
df_cleaned = df_cleaned.dropna()


# Select columns for X and y from the DataFrame
y_pd = df_cleaned["f_political_model"]
X_pd = df_cleaned[cov_list].drop(["treatment", "treatment_Control", "country", "country_3", "gender", "gender_0"], axis=1)
Z_pd = df_cleaned[cov_list2].drop(["country", "country_3", "gender", "gender_0"], axis=1)
Z_pd.columns

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
    X_pd.to_numpy(dtype="float32"),
    dtype=torch.get_default_dtype()
)
Z = torch.tensor(
    Z_pd.to_numpy(dtype="float32"),
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

# # DF of coef and their names
# result = pd.DataFrame({"names": X_pd.columns, "coef": coef_vec})
# result.to_csv("../data/Mattingly2023a_world_leader_results.csv")

p = coef_vec.shape[0]
# coef_names = [f"beta_{j}" for j in range(p)]  # or replace with your own names
coef_names = X_pd.columns


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
    flow_bins=32,
    bounds=12.0,
    epochs=1000,
    lr=1e-3,
    init_probit=True,
    verbose=True,
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

summary.to_csv("../data/Mattingly2023a_econ_results_full.csv", index=False, encoding="utf-8-sig")
