#' Download Block Assignment Files (BAF) from the Census Bureau
#'
#' @param state state to download data for.
#' @param year year to download data for. Default is `2020`.
#' @param geographies geography types to download. Default is `NULL`, which gets all available geographies.
#' @param cache_to directory name to save the downloaded data. Default is `baf_download_path()`.
#' @param overwrite Should the data be re-downloaded? Default is `FALSE`.
#'
#' @return a list of [tibble]s for each geography type
#' @export
#'
#' @examples
#' baf('DE', 2020)
baf <- function(state, year = 2020, geographies = NULL,
                cache_to = baf_download_path(), overwrite = FALSE) {
  if (length(state) != 1) {
    cli::cli_abort('{.arg state} must be a single state abbreviation.')
  }
  if (length(year) != 1) {
    cli::cli_abort('{.arg year} must be a single year.')
  }

  fips_v <- lookup_state_fips(state)
  abb_v <- lookup_state_abb(state)

  tb_links <- lookup_links(year = year, geographies = geographies)

  # if (nrow(tb_links) > 1) {
  #   cli::cli_abort(c(
  #     x = 'Multiple links found for the state, year, and geographies.',
  #     i = 'Please open an issue at {.url https://github.com/christopherkenny/baf/issues}.'
  #   ))
  # }

  out <- lapply(
    seq_len(nrow(tb_links)),
    function(i) {
      if (tb_links$level[i] == 'state') {
        globs <- vapply(seq_along(state), function(st_i) {
          # construct download path ----
          fips <- fips_v[st_i]
          abb <- abb_v[st_i]

          link <- tb_links$url[i]
          link <- glue::glue(link)

          cache_subdir <- fs::path(cache_to, tb_links$decade[i])
          target <- fs::path(cache_subdir, fs::path_file(link))

          if (!fs::file_exists(target) || overwrite) {
            baf_download(
              url = link,
              path = target,
              overwrite = overwrite
            )
            utils::unzip(target, exdir = cache_subdir)
          }

          fs::path(cache_subdir, fs::path_ext_remove(fs::path_file(link)))
        }, character(1))

        stub <- fs::path_file(globs)
        files <- fs::dir_ls(fs::path_dir(globs), regexp = paste0(stub, '*'))
        # exclude zip file
        files <- files[!grepl(x = files, '.zip')]

        short_name <- vapply(seq_along(files), function(i) {
          x <- files[i] |>
            fs::path_file() |>
            fs::path_ext_remove()
          substr(x, 21, nchar(x))
        }, character(1))

        files <- lapply(files, function(f) {
          out <- readr::read_delim(f,
            delim = ifelse(tb_links$decade[1] == 2010, ',', '|'),
            col_types = readr::cols(.default = 'c'),
            progress = interactive()
          )

          if (all(is.na(out[[ncol(out)]]))) {
            out <- NULL
          }

          out
        }) |>
          stats::setNames(short_name) |>
          Filter(Negate(is.null), x = _)
      } else {
        # download national files

        link <- tb_links$url[i]
        cache_subdir <- fs::path(cache_to, tb_links$decade[i])
        target <- fs::path(cache_subdir, fs::path_file(link))

        if (!fs::file_exists(target) || overwrite) {
          baf_download(
            url = link,
            path = target,
            overwrite = overwrite
          )
          utils::unzip(target, exdir = cache_subdir)
        }

        files <- utils::unzip(target, list = TRUE)$Name

        if (any(grepl(x = files, 'National'))) {
          files <- files[grepl(x = files, 'National')]
        } else {
          files <- files[grepl(x = files, paste0(fips_v, '_', abb_v))]
        }

        short_name <- tb_links$short_name[i]

        files <- lapply(files, function(f) {
          out <- readr::read_delim(
            fs::path(cache_subdir, f),
            delim = ',',
            col_types = readr::cols(.default = 'c'),
            progress = interactive()
          )

          out <- out |>
            dplyr::mutate(
              state = substr(.data[[names(out)[1]]], 1, 2)
            ) |>
            dplyr::filter(state %in% fips_v) |>
            dplyr::select(-state)

          if (all(is.na(out[[ncol(out)]]))) {
            out <- NULL
          }

          out
        }) |>
          stats::setNames(short_name) |>
          Filter(Negate(is.null), x = _)
      }

      files
    }
  ) |>
    unlist(recursive = FALSE)

  if (!is.null(geographies)) {
    if ('cd' %in% tolower(geographies)) {
      geographies <- c(geographies, paste0('CD', 109:118))
    }
    if ('shd' %in% tolower(geographies)) {
      geographies <- c(geographies, paste0('SHD', 2012:2022))
    }
    if ('ssd' %in% tolower(geographies)) {
      geographies <- c(geographies, paste0('SSD', 2012:2022))
    }
    out <- out[names(out) %in% geographies]
  }

  out
}
