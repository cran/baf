test_that('clean_bafs works', {
  out <- NULL

  try({
    out <- baf('WA') |>
      clean_bafs('WA')
  })

  skip_if(is.null(out), message = 'data did not download.')

  expect_true('VTD' %in% names(out))
})
