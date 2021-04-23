file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes') %>% 
    .[61:63]
    
sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes') %>% 
    dplyr::filter(name == 'QC01' | name == 'QC02' | name == 'QC03')
    
bp <- binneR::detectParameters(file_paths)

bd <- binneR::binneRlyse(file_paths,sample_information,bp)

test_that('miss injection detectioni works',{
  miss_injections <- detectMissInjections(bd)
  
  expect_type(miss_injections,'list')
  expect_length(miss_injections,2)
})

test_that('batch correction detection works',{
  batch_diff <- detectBatchDiff(bd)
  
  expect_null(batch_diff)
})

test_that('pre-treatment parameter detection works',{
  pp <- detectPretreatmentParameters(bd)  
  
  expect_s4_class(pp,'AnalysisParameters')
})
