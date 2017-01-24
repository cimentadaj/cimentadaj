
#' Estimate robust standard errors for different types of coefficients
#'
#' @param x A linear or glm models
#' @param coef A character vector indicating the type of coefficient. The function accepts 'logit' for
#' log coefficients, 'odd.ratio' for odd ratios and 'probs' for probability scaled coefficients.
#'
#' @return A matrix containing the estimated coefficient, robust standard errors, z values and p values
#' @export
#'
#' @examples
#' library(haven)
#' dat <- read_dta("http://www.stata-press.com/data/r9/quad1.dta")
#' mod1 <- glm(z ~ x1 + x2 + x3, dat, family = binomial)
#'
#' robustse(mod1, coef = "logit")
#' robustse(mod1, coef = "odd.ratio")
#' robustse(mod1, coef = "probs")
#'
#' # See http://www.jorgecimentada.com/?p=214 for a detailed example.
robustse <- function(x, coef = c("logit", "odd.ratio", "probs")) {
    sandwich1 <- function(object) sandwich::sandwich(object) * stats::nobs(object) / (stats::nobs(object) - 1) # Function calculates SE's
    mod1 <- lmtest::coeftest(x, vcov = sandwich1) # apply the function over the variance-covariance matrix
    if (coef == "logit") {
    return(mod1) # return logit with robust SE's
    } else if (coef == "odd.ratio") {
    mod1[, 1] <- exp(mod1[, 1]) # return odd ratios with robust SE's
    mod1[, 2] <- mod1[, 1] * mod1[, 2]
    return(mod1)
    } else {
    mod1[, 1] <- (mod1[, 1]/4) # return probabilites with robust SE's
    mod1[, 2] <- mod1[, 2]/4
    return(mod1)
    }
}

