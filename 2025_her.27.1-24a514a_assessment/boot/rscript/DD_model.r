
# 1. Maturity mean function with fixed asymptotes (logistic scaled between L and U)
maturity_mean <- function(age, logN, a0, a1, b0, L0, U0) {
  inv_logit <- function(x) 1 / (1 + exp(-x))
  L <- inv_logit(L0)
  U <- inv_logit(U0)
  if (U <= L) U <- L + 0.01
  L + (U - L) / (1 + exp(-b0 * (age - (a0 + a1 * logN))))
}

# 2. Negative log-likelihood function factory (creates a function for mle2)
beta_nll_factory <- function(data, age_col, density_col, response_col) {
  function(a0, a1, b0, L0, U0, log_phi) {
    phi <- exp(log_phi)
    logN <- log(data[[density_col]])
    mu <- maturity_mean(data[[age_col]], logN, a0, a1, b0, L0, U0)
    mu <- pmin(pmax(mu, 1e-6), 1 - 1e-6)  # keep inside (0,1)
    shape1 <- mu * phi
    shape2 <- (1 - mu) * phi
    -sum(dbeta(data[[response_col]], shape1, shape2, log = TRUE))
  }
}

# 3. Fit model to data, specifying column names
fit_density_dependent_model <- function(
    data,
    age_col = "age",
    density_col = "Estimate",
    response_col = "propMat",
    start_vals = list(a0 = 3, a1 = 0.1, b0 = 1, L0 = -5, U0 = 5, log_phi = 2),
    method = "BFGS"
) {
  nll <- beta_nll_factory(data, age_col, density_col, response_col)
  mle2(nll, start = start_vals, method = method)
}

# 4. Predict maturity proportions using fitted parameters on any dataset
predict_maturity <- function(
    data, params,
    age_col = "age",
    density_col = "Estimate"
) {
  logN <- log(data[[density_col]])
  maturity_mean(
    age = data[[age_col]],
    logN = logN,
    a0 = params["a0"],
    a1 = params["a1"],
    b0 = params["b0"],
    L0 = params["L0"],
    U0 = params["U0"]
  )
}


make_predicted_maturity_table <- function(data,what='mo_pred',digits=1){
  pred <- reshape(
    data[, c("year", "age", what)],  # Only relevant columns
    timevar = "age",
    idvar = "year",
    direction = "wide"
  )
  colnames(pred) <- sub(paste0(what,"\\."), "", colnames(pred))
  ages <- as.numeric(colnames(pred)[-1])
  ordered_indices <- order(ages)
  pred <- round(pred[, c(1, ordered_indices + 1)],digits=digits)
  rownames(pred)<-pred$year
  pred$year<-NULL
  return(pred)
}
