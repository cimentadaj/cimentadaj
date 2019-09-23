##' Adds the citation skeleton in \code{inst/CITATION} filled out
##' automatically with the information from the \code{DESCRIPTION} file.
##' Additionally, it creates an \code{.onAttach} function on 'R/utils.R'
##' (created if necessary, appended if exists) which prints out the citation
##' details on startup.
##'
##' @title Add all citation information of your package
##' @return Nothing
##' @author Jorge
##' @export
add_citation <- function() {
  desc <- desc::description$new()
  desc$set("Date", Sys.Date())
  year <- sub('.*(2[[:digit:]]{3})-.*', '\\1', {desc$get('Date')}, perl = TRUE)
  vers <- paste('R package version', {desc$get('Version')})
  author <- gsub('<.+]', '', desc$get_authors())
  split_lastname <- strsplit(author, " ")[[1]]

  full_citation <-
    glue::glue(
      "{split_lastname[2]}, {split_lastname[1]} ({year}). {desc$get('Title')} {vers}."
    )

  citation_str <- glue::glue(
    "citHeader('To cite { desc$get('Package') } in publications use:')

  citEntry(
    entry    = 'Manual',
    title    = '{desc$get('Title')}',
    author   = '{author}',
    year     = '{year}',
    note     = '{vers}',
    url      = '{desc$get('URL')}',
    textVersion = '{full_citation}'
  )"
  )

  dir.create(here::here("inst"), showWarnings = FALSE)
  file.create(here::here("inst", "CITATION"), showWarnings = FALSE)
  writeLines(citation_str, here::here("inst", "CITATION"))

  onload_fun <- glue::glue(
    '
    .onAttach <- function(libname, pkgname) {{
       packageStartupMessage(\"\\nPlease cite as: \\n\")
       packageStartupMessage("{full_citation}")
    }}'
  )

  write(onload_fun, here::here("R/utils.R"), append = TRUE)
}
