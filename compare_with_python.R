library(ggplot2)
library(permshap)

# Turn ordinal factors into unordered
ord <- c("clarity", "color", "cut")
diamonds[, ord] <- lapply(diamonds[ord], factor, ordered = FALSE)

# Fit model
fit <- lm(log(price) ~ log(carat) * (clarity + color + cut), data = diamonds)

# Subset of 120 diamonds used as background data
bg_X <- diamonds[seq(1, nrow(diamonds), 450), ]

# Subset of 1018 diamonds to explain
X_small <- diamonds[seq(1, nrow(diamonds), 53), c("carat", ord)]

# Exact crunching (5s)
system.time(
  ps <- permshap(fit, X_small, bg_X = bg_X)
)
ps

# SHAP values of first 2 observations:
#          carat     clarity     color        cut
# [1,] -2.050074 -0.28048747 0.1281222 0.01587382
# [2,] -2.085838  0.04050415 0.1283010 0.03731644

# Using parallel backend
library("doFuture")

registerDoFuture()
plan(multisession, workers = 2)  # Windows
# plan(multicore, workers = 2)   # Linux, macOS, Solaris

# 4 seconds
system.time(
  ps3 <- permshap(fit, X_small, bg_X = bg_X, parallel = TRUE)
)
ps3

library(shapviz)

sv <- shapviz(ps)
sv_dependence(sv, "carat")


# More features (but non-sensical model)
# Fit model
fit <- lm(
  log(price) ~ log(carat) * (clarity + color + cut) + x + y + z + table + depth,
  data = diamonds
)

# Subset of 1018 diamonds to explain
X_small <- diamonds[seq(1, nrow(diamonds), 53), setdiff(names(diamonds), "price")]

# Exact permshap on X_small, using X_small as background data
# (58 seconds unplugged)
system.time(
  ps <- permshap(fit, X_small, bg_X = bg_X)
)
ps

#          carat        cut     color     clarity         depth         table          x
# [1,] -1.842799 0.01424231 0.1266108 -0.27033874 -0.0007084443  0.0017787647 -0.1720782
# [2,] -1.876709 0.03856957 0.1266546  0.03932912 -0.0004202636 -0.0004871776 -0.1739880
#                y            z
# [1,] 0.001330275 -0.006445693
# [2,] 0.001397792 -0.006560624

#========================
# Not the same in Python
#========================

import numpy as np
import pandas as pd
from plotnine.data import diamonds
from statsmodels.formula.api import ols
import shap

ord = ["clarity", "color", "cut"]
# x = ["carat"] + ord + ["table", "depth", "x", "y", "z"]
x = ["carat"] + ord
X = diamonds[x].to_numpy()

# Fit model with interactions and dummy variables
fit = ols(
  "np.log(price) ~ np.log(carat) * (C(clarity) + C(cut) + C(color))", # + x + y + z + table + depth",
  data=diamonds
).fit()

# Background data (120 rows)
bg_X = X[0:len(X):450]

# Define subset of 1018 diamonds to explain
X_small = X[0:len(X):53]

# Calculate SHAP values
explainer = shap.explainers.Exact(lambda X: fit.predict(pd.DataFrame(X, columns=x)), bg_X)
shap_values = explainer(X_small) # 24 s

sv[0:2].values

# array([[-1.92614207, -0.27566705,  0.14111178,  0.01701598],
#        [-1.96487857,  0.04583417,  0.14127493,  0.0409374 ]])

