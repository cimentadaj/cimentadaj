#' Generate an R script with a preamble
#'
#' @param dir The directory where the new script will be stored as a character string
#' @param filename The R script file name as a character string (without extensions)
#' @param author The author of the script as a character
#' @param project Project name or script name that will appear in the preamble as character string.
#' @param open_file Logical to whether you want to open the file with Rstudio after the function call
#'
#' @return Returns nothing. If \code{open_file} is \code{TRUE}, it will open the script.
#' @export
#'
#' @examples
#' # Only been tested with OSx El Capitan 10.11.6
#'
#' gen_rscript(dir = ".", "new_script", "Jorge Cimentada", "new_project", FALSE)
#' # This should create a new R script in your working directory
#'
gen_rscript <- function(dir, filename, author, project, open_file = F) {
  if(missing(dir)) stop("The directory was not provided")

  correct_dir <- normalizePath(dir)

  if(!dir.exists(correct_dir)) stop("Directory doesn't exist")

  file_path <- paste0(correct_dir, "/", filename, ".R")

  indentation <- paste(rep("#", 15), collapse = "")
  form_fields <- c("Author: ", "Time: ","Project: ")
  contents <- c(author, date(), project)

  preamble <-
    c(indentation,
      purrr::map2_chr(form_fields, contents, function(x, y) paste0("# ", x, y)),
      indentation)

  writeLines(preamble, file_path)

  if(open_file) {
    file_path <- gsub(" ", "\\\\ ", file_path) # Make sure spaces are replaced by backsladh and space
    system2("open", paste0(file_path, " -a Rstudio"))
  }
}
