
test_that('miss injection detectioni works for Binalysis class',{
  miss_injections <- detectMissInjections(bd)
  
  expect_type(miss_injections,'list')
  expect_length(miss_injections,2)
})

test_that('miss injection detectioni works for MetaboProfile class',{
  
  
  miss_injections <- detectMissInjections(lcd)
  
  expect_type(miss_injections,'list')
  expect_length(miss_injections,2)
})



test_that('batch correction detection works for Binalysis class',{
  batch_diff <- detectBatchDiff(bd)
  
  expect_null(batch_diff)
})

test_that('batch correction detection works for MetaboProfile class',{
  batch_diff <- detectBatchDiff(lcd)
  
  expect_null(batch_diff)
})

test_that('pre-treatment parameter detection works for Binalysis class',{
  pp <- detectPretreatmentParameters(bd)  
  
  expect_s4_class(pp,'AnalysisParameters')
})

test_that('pre-treatment parameter detection works for MetaboProfile class',{
  
  
  pp <- detectPretreatmentParameters(lcd)  
  
  expect_s4_class(pp,'AnalysisParameters')
})

test_that('modelling parameter detection works for Binalysis class',{
  mp <- detectModellingParameters(bd)
  
  expect_s4_class(mp,'AnalysisParameters')
})

test_that('modelling parameter detection works for MetaboProfile class',{
  
  
  mp <- detectModellingParameters(lcd)
  
  expect_s4_class(mp,'AnalysisParameters')
})

test_that('modelling parameter detection works for Analysis class',{
  mp <- detectModellingParameters(a,cls = 'day')
  
  expect_s4_class(mp,'AnalysisParameters')
})
