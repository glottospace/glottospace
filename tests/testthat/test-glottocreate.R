test_that("empty glottodata structure is of expected form", {

  empty_df <- glottocreate(glottocodes = c("yucu1253", "tani1257"), variables = 3, meta = FALSE)

  # Is it a data frame?
  expect_true(is.data.frame(empty_df))

  # Is it the expected size?
  expect_equal(ncol(empty_df), 4)
  expect_equal(nrow(empty_df), 2)

  # Other than first column (-1), expect values of all other columns to be NA
  expect_true(all(is.na(unlist(empty_df[, -1], use.names = FALSE))))

})
