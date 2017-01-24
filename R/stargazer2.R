#' Logistic regression table with odd ratios
#'
#' @param model A non-empty list containing one or more glm models
#' @param odd.ratio A logical value indicating whether you want odd ratios (TRUE)
#'  instead of the usual log odds (FALSE).
#' @param ... further aguments passed to the stargazer function. See ?stargazer::stargazer
#'
#' @return A stargazer table with correct odd ratios, standard errors and p values
#' @export
#'
#' @examples
#' stargazer2(glm(am ~ mpg, data = mtcars), odd.ratio = TRUE, type = "text") # Odd ratios
#' stargazer2(glm(am ~ mpg, data = mtcars), odd.ratio = FALSE, type = "text") # Log odds
#'
#' # Let's compare it with Stata output:
#'
#' # R table
#' library(haven)
#' auto <- haven::read_dta("http://www.stata-press.com/data/r13/auto.dta")
#' auto$mpg2 <- auto$mpg > 21.29
#' model <- glm(mpg2 ~ price + rep78 + turn + displacement, data = auto, family = "binomial")
#' stargazer2(model, odd.ratio = TRUE, type = "text", single.row = TRUE)
#'
#' # Stata table (run in Stata)
#' # use http://www.stata-press.com/data/r13/auto.dta, clear
#' # gen mpg2 = mpg > 21.29
#' # logit mpg2 price rep78 turn displacement, or
#'
stargazer2 <- function(model, odd.ratio = FALSE, ...) {
  if(!("list" %in% class(model))) model <- list(model)

  if (odd.ratio) {
    coefOR2 <- lapply(model, function(x) exp(stats::coefficients(x)))
    seOR2 <- lapply(model, function(x) exp(stats::coefficients(x)) * summary(x)$coef[, 2])
    p2 <- lapply(model, function(x) summary(x)$coefficients[, 4])
    stargazer::stargazer(model, coef = coefOR2, se = seOR2, p = p2, ...)

  } else {
    stargazer::stargazer(model, ...)
  }
}
