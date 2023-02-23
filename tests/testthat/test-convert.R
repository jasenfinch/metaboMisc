test_that("multiplication works", {
  sample_information <- system.file('repository/Thermo-Exactive/Experiment_1/QC01.raw',package = 'grover') %>% 
      rawrr::readFileHeader() %>% 
      tibble::as_tibble() %>% 
      convertSampleInfo()
  
  expect_s3_class(sample_information,'tbl_df')
})
