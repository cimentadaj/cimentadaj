#' Insert a new post using blogdown
#'
#' @param title Your blogpost's title
#' @param ... All extra arguments passed to blogdown::new_post
#'
#' @return It creates a .Rmd and .html file under ./blog/your-title-name/
#' @export
#'
#' @examples
#'
#' # No time
#'
my_new_post <- function(title, ...) {

  title_change <- trimws(gsub("[[:punct:]]", "", title))
  slug <- stringr::str_to_lower(gsub("\\s","-", title_change))

  dir <- paste0(lubridate::today(), "-", slug)

  blogdown::new_post(title = title,
                     author = "Jorge Cimentada",
                     subdir = file.path("./blog", dir),
                     ext = ".Rmd",
                     ...)
}
