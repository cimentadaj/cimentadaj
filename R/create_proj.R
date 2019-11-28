##' Create an opinionated package structure using `drake` and `renv`
##'
##' Clones `repo_name` from your Github account and saves it in the
##' `path` specified in the argument. Once cloned, the function
##' creates files `R/plan.R`, `R/functions.R`, `R/plan.R` and `_drake.R`
##' as per the `drake` project structure. It adds a few files to ignore
##'  in .gitignore, creates and `Rmd` for the README and adds a template
##' of code for `_drake.R` and `R/plan.R`.
##'
##' Finally, it ends by installing `renv` for package dependency, installs
##' `drake` automatically and restarts the session in the project directory
##' to being working.
##' 
##' @title Create  opinionated package structure using `drake` and `renv`
##' @param repo_name Name of the repository in github. For example
##' "cimentadaj/fragile_families".
##' @param path the path where the repository will be saved locally
##' @return invisible TRUE
##' @author Jorge
##'
##' @examples
##'
##' \dontrun{
##' create_proj("cimentadaj/fragile_families", ".")
##' }
##' 
create_proj <- function(repo_name, path = ".") {
  stopifnot(is.character(path),
            is.character(repo_name))

  ############################# Clone repository ###############################
  ##############################################################################
  print_styler('Cloning repository')
  path <- normalizePath(path, winslash = .Platform$file.sep, mustWork = FALSE)
  usethis::create_from_github(repo_name, destdir = path)

  ############################# Create project folders #########################
  ##############################################################################

  # Since usethis changed the working directory to the cloned
  # repo, we get the latest path
  path <- getwd()
  dirs_create <- file.path(path, c("R", "data"))
  for (folder in dirs_create) dir.create(folder, recursive = TRUE)
  print_styler('Created folder ', dirs_create)

  ############################# Git ignore #####################################
  ##############################################################################

  file.create(file.path(path, ".gitignore"))
  usethis::use_git_ignore(c(".Rhistory", ".RData", ".Rproj.user"))

  ############################# Create R files #################################
  ##############################################################################
  
  r_dir <- grep("/R", dirs_create, value = TRUE)
  files_to_create <- c(
    file.path(r_dir, c("packages.R", "functions.R", "plan.R")),
    file.path(dirname(r_dir), "_drake.R")
  )

  for (r_file in files_to_create) file.create(r_file)
  print_styler('Created file ', files_to_create)

  ############################# Populate R files ###############################
  ##############################################################################

  drake_file <- grep("_drake.R", files_to_create, value = TRUE)
  paste_in_drake <- '
  source("R/packages.R")
  source("R/functions.R")
  source("R/plan.R")

  # options(clustermq.scheduler = "multicore") # optional parallel computing
  drake_config(plan, verbose = 2)'
  writeLines(paste_in_drake, drake_file)


  plan_file <- grep("plan.R", files_to_create, value = TRUE)
  paste_in_plan <- 'plan <- drake_plan()'
  writeLines(paste_in_plan, plan_file)

  print_styler('Populated ', c(drake_file, plan_file))
  # I think this adds .Rbuildignore -- exclude
  # Add a custom readme for ds projects
  usethis::use_readme_rmd(open = FALSE)

  ############################# Project dependencies ###########################
  ##############################################################################

  # Add a set of preinstalled packages for every project
  print_styler("Installing renv for package dependency")
  cat("\n")

  unloadNamespace('renv')
  pak::pkg_install("renv")

  print_styler("Activating renv project")
  cat("\n")
  renv::init(path)

  print_styler("Installing drake as project dependency")
  pak::pkg_install("drake")
  
  # Restart for renv to take effect
  startup::restart()

  invisible(TRUE)
}

initial_styler <- function(x) cat(crayon::red(crayon::green(clisymbols::symbol$tick)), x)

print_styler <- function(x, files) {
  if (!missing(files)) x <- paste0(x, crayon::blue(paste0(paste0(files, .Platform$file.sep))), "\n")
  for (i in x) initial_styler(i)
}
