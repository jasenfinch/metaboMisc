library(metaboData)

## Example Binalysis class
file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes') %>% 
    .[61:63]
    
sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes') %>% 
    dplyr::filter(name == 'QC01' | name == 'QC02' | name == 'QC03')
    
bp <- binneR::detectParameters(file_paths)

bd <- binneR::binneRlyse(file_paths,sample_information,bp)

## Example MetaboProfile class
file_paths <- list.files(
  system.file("cdf", 
              package = "faahKO"),
  full.names = TRUE,
  recursive = TRUE)[1:2]
file_names <- basename(file_paths)
sample_names <- tools::file_path_sans_ext(file_names)

sample_info <- tibble(fileOrder = seq_along(file_paths),
                      injOrder = seq_along(file_paths),
                      fileName = file_names,
                      batch = 1,
                      block = 1,
                      name = sample_names,
                      class = substr(sample_names,1,2))

parameters <- profilePro::profileParameters('LCMS-RP',nCores = 2)
profilePro::processingParameters(parameters)$peakDetection <- xcms::CentWaveParam(snthresh = 20, 
                                                                noise = 1000)
profilePro::processingParameters(parameters)$retentionTimeCorrection <- xcms::ObiwarpParam()
profilePro::processingParameters(parameters)$grouping <- xcms::PeakDensityParam(sampleGroups = sample_info$class,
                                                              maxFeatures = 300,
                                                              minFraction = 2/3)
lcd <- profilePro::profileProcess(file_paths,sample_info,parameters)

## Example Analysis class
a <- new('Analysis')
preTreated(a) <- analysisData(abr1$neg,abr1$fact)

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