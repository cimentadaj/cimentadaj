#' Get a description of object x
#'
#' @param x Any object. Currently supported are all R classes, plus matrices, data frames and lists.
#'
#' @return A tibble with usual properties such as length, size, and attributes such as names, dimensions
#' if apply
#' @export
#'
#' @examples
#'
#' x <- NULL
#' describe(x)
#'
#' x <- numeric()
#' describe(x)
#'
#' x <- "hey"
#' describe(x)
#'
#' x <- seq(0.01, 0.05, 0.01)
#' describe(x)
#'
#' x <- 1:10
#' describe(x)
#'
#' x <- matrix()
#' describe(x)
#'
#' x <- mtcars
#' describe(x)
#'
#' x <- data.frame()
#' describe(x)
#'
#' x <- list(list(1:10), 1:2)
#' describe(x)
#'
#' x <- list()
#' describe(x)
#'
#' x <- list(1:10, 1:2)
#' describe(x)
describe <- function(x) {
  if (any(class(x) %in% c("numeric", "integer", "double", "character"))) {
    describe.generic(x)
  } else {
    UseMethod("describe")
  }
}

#' Describe method for atomic vectors
#'
#' @param x A numeric, integer, double or character vector
#'
#' @return A tibble with usual properties such as length, size, and attributes such as names, dimensions
#' if apply.
#' @export
#'
describe.generic <- function(x) {
  tibble::tribble(
    ~ Detail, ~ Info,
    "Class", class(x),
    "Length", length(x),
    "N. attributes", length(attributes(x)),
    "Size", format(utils::object.size(x), units = "Kb")
  )
}


#' Describe method for unknown object classes
#'
#' @param x An object
#'
#' @return Text specifying that no method exists for that class
#' @export
#'
describe.default <- function(x) {
  cat("Unknown class:", class(x))
}

#' Describe method for matrices
#'
#' @param x A matrix
#'
#' @return A tibble with usual properties such as length, size, and attributes such as names, dimensions
#' if apply.
#' @export
#'
describe.matrix <- function(x) {

  other_attr <- setdiff(names(attributes(x)), "dim")

  tibble::tribble(
    ~ Detail, ~ Info,
    "Class", class(x),
    "Length", length(x),
    "Dimesions", paste(dim(x), collapse = " x "),
    "Other attr", ifelse(length(other_attr) > 0, "Yes", "No"),
    "Size", format(utils::object.size(x), units = "Kb")
  )
}


#' Describe method for data frames
#'
#' @param x A data frame
#'
#' @return A tibble with usual properties such as length, size, and attributes such as names, dimensions
#' if apply.
#' @export
#'
describe.data.frame <- function(x) {

  other_attr <- setdiff(names(attributes(x)), "dim")

  if (length(names(x)) > 0) {
    columns <- names(x)
    colnames <-
      switch(as.character(length(columns)),
             "1" = columns[1],
             "2" = paste(columns[1:2], collapse = ", "),
             paste0(paste(columns[1:2], collapse = ", "), "..."))
  } else {
    colnames <- "No names"
  }

  tibble::tribble(
    ~ Detail, ~ Info,
    "Class", paste(class(x), collapse = ", "),
    "Length", length(x),
    "Column names", colnames,
    "Dimesions", paste(dim(x), collapse = " x "),
    "Other attr", ifelse(length(other_attr) > 0, "Yes", "No"),
    "Size", format(utils::object.size(x), units = "Mb")
  )
}


#' Describe method for lists
#'
#' @param x A list
#'
#' @return A tibble with usual properties such as length, size, and attributes such as names, dimensions
#' if apply.
#' @export
#'
describe.list <- function(x) {

  if (!length(x)) return(cat("empty list"))

  if (!is.null(names(x))) {
    columns <- names(x)
    colnames <-
      switch(as.character(length(columns)),
             "1" = columns[1],
             "2" = paste(columns[1:2], collapse = ", "),
             paste0(paste(columns[1:2], collapse = ", "), "..."))
  } else {
    colnames <- "No names"
  }

  list_compare <- function(object, function_compare) {

    unique_fun <- lapply(object, function_compare)
    random_object <- unique_fun[[sample(length(unique_fun), 1)]]
    all_equal <- vapply(unique_fun, function(class) identical(class, random_object),
                        FUN.VALUE = logical(1))
    all_equal
  }

  all_classes_equal <- list_compare(x, mode)
  all_lengths_equal <- list_compare(x, length)

  df <- tibble::tribble(
    ~ Detail, ~ Info,
    "Class", paste(class(x), collapse = ", "),
    "Length", length(x),
    "Object names", colnames,
    "Class the same?", ifelse(all(all_classes_equal), "Yes", "No"),
    "Length the same?", ifelse(all(all_lengths_equal), "Yes", "No"),
    "Size", format(utils::object.size(x), units = "Mb")
  )

  if (all(all_classes_equal)) {
    row_index <- grep("Class the same?", df$Detail)
    df <- tibble::add_row(df, Detail = "Class of objects", Info = class(x[[1]]), .after = row_index)
  }

  if (all(all_lengths_equal)) {
    row_index <- grep("Length the same?", df$Detail)
    df <- tibble::add_row(df, Detail = "Length of objects", Info = length(x[[1]]), .after = row_index)
  }

  df
}
