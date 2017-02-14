#' Build sequence of replicate-survey-adjusted models
#'
#' @param dv A character element specifying the name of the dependent variable
#' @param covariates A character vector with the names of the covariates
#' @param data The replicated survey design which contains all of the variables outlined above
#'
#' @return Returns a list with length(covariates) where the first slot are the results of the dependent variable
#' regressed on the first covariate. The second slot is the dependent variable regressed on the first two
#' covariates and so on, until the last slot, which contains the dependent variable regressed on all covariates
#' @export
#'
#' @examples
#' # Don't have time
#'
models <- function(dv, covariates, data) {
  dv <- paste(dv, "~ 1")

  combinations <- lapply(1:length(covariates), function(i) seq(1:i))

  # List of sequential formulas
  formulas <- lapply(combinations, function(p) x <- stats::as.formula(paste(c(dv, covariates[p]), collapse=" + ")))

  # Run the model on each formula and returns a list of models
  results <- lapply(formulas, function(o) with(data, svyglm(o))[[1]])
  return(results)
}
