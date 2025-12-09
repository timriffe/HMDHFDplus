test_that("getHMDcountries returns a sensible country table", {
  skip_on_cran()
  skip_if_offline()
  
  df <- getHMDcountries()
  
  # basic structure
  expect_s3_class(df, "data.frame")
  
  # we rely on CNTRY and link existing in readHMDweb()
  expect_true("CNTRY" %in% names(df))
  expect_true("link"  %in% names(df))
  
  # CNTRY codes look like non-empty strings and are unique
  expect_true(is.character(df$CNTRY))
  expect_true(all(nchar(df$CNTRY) > 0))
  expect_length(unique(df$CNTRY), nrow(df))
  
  # at least one country we know should be there
  expect_true("SWE" %in% df$CNTRY)
})

test_that("getHMDitemavail returns a sensible item table for SWE", {
  skip_on_cran()
  skip_if_offline()
  
  tab <- getHMDitemavail("SWE")
  
  expect_s3_class(tab, "data.frame")
  
  # we rely on item and link existing in readHMDweb()
  expect_true("item" %in% names(tab))
  expect_true("link" %in% names(tab))
  
  # items should be non-empty character
  expect_true(is.character(tab$item))
  expect_true(all(nchar(tab$item) > 0))
  
  # links should be non-empty character
  expect_true(is.character(tab$link))
  expect_true(all(nchar(tab$link) > 0))
  
  # sanity check: at least one series name looks like a period-based series
  expect_true(any(grepl("per", tab$item, fixed = TRUE)))
})

test_that("getHMDitemavail handles invalid CNTRY codes in a defined way", {
  skip_on_cran()
  skip_if_offline()
  
  # Document current behaviour:
  # either error OR return an empty data.frame are acceptable here.
  res <- try(getHMDitemavail("XXX"), silent = TRUE)
  
  if (inherits(res, "try-error")) {
    succeed()  # documented as "it errors for invalid CNTRY"
  } else {
    expect_s3_class(res, "data.frame")
    expect_identical(nrow(res), 0L)
  }
})
