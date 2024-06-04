test_that('baf_download works', {
  tf <- tempfile(fileext = '.zip')
  ex <- 'https://www2.census.gov/geo/docs/maps-data/data/baf2020/BlockAssign_ST01_AL.zip'

  out <- NULL

  try({
    out <- baf_download(url = ex, path = tf)
  })

  skip_if(is.null(out), message = 'data did not download.')

  expect_true(file.exists(tf))
})


test_that('baf_download_path works', {
  testthat::expect_type(baf_download_path(), 'character')
})
