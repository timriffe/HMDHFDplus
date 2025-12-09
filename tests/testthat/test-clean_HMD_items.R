test_that("clean_HMD_items drops PDFs and inputDB-like series", {
  tbl <- data.frame(
    CNTRY = rep("DEUTNP", 6),
    item  = c(
      "DEUTNPfltper_1x1",   # keep
      "DEUTNPbirth",        # drop
      "DEUTNPbirthbymonth", # drop
      "DEUTNPdeath",        # drop
      "DEUTNPpop",          # drop
      "DEUTNPnote.pdf"      # drop
    ),
    stringsAsFactors = FALSE
  )
  
  out <- clean_HMD_items(tbl)
  
  # kept
  expect_true("DEUTNPfltper_1x1" %in% out)
  
  # dropped inputDB-like
  expect_false(any(grepl("birth", out, ignore.case = TRUE)))
  expect_false(any(grepl("death", out, ignore.case = TRUE)))
  expect_false(any(grepl("pop$",  out, ignore.case = TRUE)))
  
  # dropped PDFs
  expect_false(any(grepl("\\.pdf$", out, ignore.case = TRUE)))
  
  # nothing else slipped through
  expect_length(out, 1L)
})

test_that("clean_HMD_items respects CNTRY and errors on mixed CNTRY", {
  tbl_ok <- data.frame(
    CNTRY = rep("SWE", 2),
    item  = c("SWEfltper_1x1", "SWEmltper_1x1"),
    stringsAsFactors = FALSE
  )
  tbl_bad <- data.frame(
    CNTRY = c("SWE", "FIN"),
    item  = c("SWEfltper_1x1", "FINfltper_1x1"),
    stringsAsFactors = FALSE
  )
  
  # normal case: just returns the items
  out_ok <- clean_HMD_items(tbl_ok)
  expect_setequal(out_ok, c("SWEfltper_1x1", "SWEmltper_1x1"))
  
  # mixed-country table should error
  expect_error(clean_HMD_items(tbl_bad))
})
