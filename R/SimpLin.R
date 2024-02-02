library(RcppArmadillo)

simpLinR <- function(x, y) {
  if (!is.numeric(x) | !is.numeric(y) | length(x) != length(y)) {
    stop("x and y must be numeric vectors of the same length")
  }
  
  a = SimpLinCpp(x, y)
}

# x <- rnorm(100)
# y <- 1 - 1 * x + rnorm(100)
# simpLinR(x, y)
# 
# mod <- lm(y ~ x)
# attributes(lm(y ~ x))
# mod$coefficients
# mod$residuals
# mod$fitted.values
# 
# 
# X <- cbind(rep(1, 100), x)
# beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
# mse <- (1/98) * sum((y - (X %*% beta_hat))^2)
# 
# mse * solve(t(X) %*% X)
