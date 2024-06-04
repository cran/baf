#' Download a file
#'
#' @param url a URL
#' @param path a file path
#' @param overwrite should the file at path be overwritten if it already exists? Default is FALSE.
#' @param quiet should output and progress bar be suppressed?
#'
#' @return a path to the downloaded file, invisibly
#' @export
#'
#' @examples
#' tf <- tempfile(fileext = '.zip')
#' ex <- 'https://www2.census.gov/geo/docs/maps-data/data/baf2020/BlockAssign_ST01_AL.zip'
#' baf_download(url = ex, path = tf)
baf_download <- function(url, path, overwrite = FALSE, quiet = FALSE) {
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  if (!file.exists(path) || overwrite) {
    curl::curl_download(url, path, quiet = quiet)
  } else {
    cli::cli_inform(c('File already downloaded at {.path {path}}',
      '>' = 'Set {.arg overwrite = TRUE} to overwrite.'
    ))
  }

  invisible(path)
}

#' Get download location for `baf` files
#'
#' @return a path
#' @export
#'
#' @examples
#' baf_download_path()
baf_download_path <- function() {
  user_cache <- getOption('baf.cache_dir')
  if (!is.null(user_cache)) {
    p <- user_cache
  } else if (getOption('baf.use_cache', FALSE)) {
    p <- tools::R_user_dir('baf', 'data')
  } else {
    p <- tempdir()
  }

  if (!dir.exists(p)) {
    dir.create(p, recursive = TRUE)
  }

  p
}
