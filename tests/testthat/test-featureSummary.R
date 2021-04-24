test_that("featureSummary works", {
  feature_summary <- featureSummary(bd)
  
  expect_s3_class(feature_summary,'tbl_df')
})
