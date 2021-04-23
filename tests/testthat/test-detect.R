## prepare spectral data
file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes') %>% 
    .[61:63]
    
sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes') %>% 
    dplyr::filter(name == 'QC01' | name == 'QC02' | name == 'QC03')
    
bp <- binneR::detectParameters(file_paths)

bd <- binneR::binneRlyse(file_paths,sample_information,bp)

test_that('pre-treatment parameter detection',{
  pp <- detectPretreatmentParameters(bd)  
  expect_s4_class(pp,'AnalysisParameters')
})
