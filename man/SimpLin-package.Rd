\name{SimpLin-package}
\alias{SimpLin-package}
\alias{SimpLin}
\docType{package}
\title{
SimpLin
}
\description{
A RcppArmadillo package to do Simple Linear Regression
}
\details{

Performs Simple linear regression. Two functions available: SimpLinCpp and SimpLinR. Returns the coefficients, standard errors, 95 percent CI for both coefficients, residuals and predicted values in a list. SimpLinR(x, y) must be passed two numeric vectors x and y. 
}
\author{
Jony Karki
}

\examples{
x <- rnorm(100)
y <- 1 - x + rnorm(100)

mod <- SimpLinR(x, y)

# get the coefficients
mod$coefficients

# to see all the other available attributes
attributes(mod)
}
