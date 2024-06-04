test_that('baf works', {
  out <- NULL

  try({
    out <- baf('DE', 2020)
  })

  skip_if(is.null(out), message = 'data did not download.')

  expect_type(out, 'list')
  expect_length(out, 7)
})
