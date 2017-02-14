#' PISA search table for all country names
#'
#' This vector contains all abbreviated country names for all PISA
#' waves (2000 until 2015) with their correctly written equivalent.
#' Some of these abbreviations are in ISO 3-letter while others
#' were written incorrectly, e.g. Korea, Republic of.
#'
#' @docType data
#' @keywords pisa_countrynames
#' @name pisa_countrynames
#' @usage pisa_countrynames
#'
#' @examples
#' # install.packages(PISA2006lite)
#' library(PISA2006lite)
#'
#' unique(PISA2006lite::student2006$CNT)
#' unique(pisa_countrynames[as.character(PISA2006lite::student2006$CNT)]) # New country names
#' # This can be saved as a new column or replace the previous column.
NULL
