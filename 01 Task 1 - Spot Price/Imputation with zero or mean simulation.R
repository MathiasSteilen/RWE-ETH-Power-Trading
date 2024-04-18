

n = 10000
X_no_missing = rnorm(n)
Z_no_missing = rnorm(n)

y = 2*X_no_missing + -10 * Z_no_missing + rnorm(n)

missing_number = round(0.25 * n)
missing_indices_X = sample(1:length(X_no_missing), size = missing_number,
                         replace = FALSE)
missing_indices_Z = sample(1:length(Z_no_missing), size = missing_number,
                           replace = FALSE)

X_missing = X_no_missing
X_missing[missing_indices_X] = NA
Z_missing = Z_no_missing
Z_missing[missing_indices_Z] = NA

X_mean = X_missing
X_mean[missing_indices_X] = mean(X_missing, na.rm = T)
Z_mean = Z_missing
Z_mean[missing_indices_Z] = mean(Z_missing, na.rm = T)

X_zero = X_missing
X_zero[missing_indices_X] = 0
Z_zero = Z_missing
Z_zero[missing_indices_Z] = 0

dummy_X = ifelse(is.na(X_missing), 1, 0)
dummy_Z = ifelse(is.na(Z_missing), 1, 0)

# True regression
lm(y ~ X_no_missing + Z_no_missing)

# Mean imputing
lm(y ~ X_mean + dummy_X + Z_mean + dummy_Z)

# Zero imputing
lm(y ~ X_zero + dummy_X + Z_zero + dummy_Z)
