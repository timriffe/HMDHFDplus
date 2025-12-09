test_that("readHMDweb errors if username is missing in non-interactive mode", {
  testthat::local_mocked_bindings(
    interactive = function() FALSE,
    .package    = "base"
  )
  
  expect_error(
    readHMDweb(
      CNTRY    = "SWE",
      item     = "fltper_1x1",
      # username missing on purpose
      password = "whatever",
      validate_items = FALSE
    ),
    regexp = "username/password must be given in non-interactive mode",
    fixed  = TRUE
  )
})

test_that("readHMDweb errors if password is missing in non-interactive mode", {
  testthat::local_mocked_bindings(
    interactive = function() FALSE,
    .package    = "base"
  )
  
  expect_error(
    readHMDweb(
      CNTRY    = "SWE",
      item     = "fltper_1x1",
      username = "someone@example.org"
      # password missing on purpose
    ),
    regexp = "username/password must be given in non-interactive mode",
    fixed  = TRUE
  )
})


