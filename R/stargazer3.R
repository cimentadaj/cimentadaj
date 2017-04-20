#' Title
#'
#' @param model
#' @param odd.ratio
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
stargazer3 <- function(model, odd.ratio = FALSE, ...) {

  if (!("list" %in% class(model))) model <- list(model)

  if (odd.ratio) {
    # Get coefficients
    coef_table <- purrr::map(model, ~ as.data.frame(summary(.x)$coefficients))

    # Estimate odds for all models
    odds <- purrr::map(coef_table, ~ exp(.x[, 1]))

    # Loop through odds and SE and multiply them
    oddsSE <- purrr::map2(odds, coef_table, ~ .x * .y[, 2])

    # Get p vals from models
    p_vals <- purrr::map(coef_table, ~ .x[, 4])

    stargazer::stargazer(model,
                         coef = odds,
                         se = oddsSE,
                         p = p_vals, ...)

  } else {
    stargazer::stargazer(model, ...)
  }

}
