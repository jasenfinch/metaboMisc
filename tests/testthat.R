library(testthat)
library(metaboMisc)

## Example Binalysis class
file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes') %>% 
    .[stringr::str_detect(.,'QC')]

sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes') %>% 
    dplyr::filter(stringr::str_detect(name,'QC'))

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

sample_info <- tibble::tibble(fileOrder = seq_along(file_paths),
                              injOrder = seq_along(file_paths),
                              fileName = file_names,
                              batch = 1,
                              block = 1,
                              name = sample_names,
                              class = substr(sample_names,1,2))

parameters <- profilePro::profileParameters('LCMS-RP')
profilePro::processingParameters(parameters)$peakDetection <- xcms::CentWaveParam(snthresh = 20,
                                                                      noise = 1000)
profilePro::processingParameters(parameters)$retentionTimeCorrection <- xcms::ObiwarpParam()
profilePro::processingParameters(parameters)$grouping <- xcms::PeakDensityParam(sampleGroups = sample_info$class,
                                                                    maxFeatures = 300,
                                                                    minFraction = 2/3)
lcd <- profilePro::profileProcess(file_paths,sample_info,parameters)

## Example Analysis class
a <- new('Analysis')
metabolyseR::preTreated(a) <- metabolyseR::analysisData(metaboData::abr1$neg[,200:300],
                                           metaboData::abr1$fact)
mp <- metabolyseR::analysisParameters(c('modelling','correlations'))
a <- metabolyseR::reAnalyse(a,mp)

## Example Assingment class
p <- assignments::assignmentParameters('FIE-HRMS')
 
assignment <- assignments::assignMFs(assignments::feature_data,p)

## Example Construction class

structural_classifications <- construction::construction(assignment)

test_check("metaboMisc")
