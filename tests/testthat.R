library(testthat)
library(metaboMisc)
library(metaboData)
library(profilePro)
library(metabolyseR)
library(MFassign)

## Example Binalysis class
file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes') %>% 
    .[61:63]

sample_information <- runinfo('FIE-HRMS','BdistachyonEcotypes') %>% 
    dplyr::filter(name == 'QC01' | name == 'QC02' | name == 'QC03')

bp <- binneR::detectParameters(file_paths)

bd <- binneR::binneRlyse(file_paths,sample_information,bp)

## Example MetaboProfile class
# file_paths <- list.files(
#     system.file("cdf", 
#                 package = "faahKO"),
#     full.names = TRUE,
#     recursive = TRUE)[1:2]
# file_names <- basename(file_paths)
# sample_names <- tools::file_path_sans_ext(file_names)
# 
# sample_info <- tibble::tibble(fileOrder = seq_along(file_paths),
#                               injOrder = seq_along(file_paths),
#                               fileName = file_names,
#                               batch = 1,
#                               block = 1,
#                               name = sample_names,
#                               class = substr(sample_names,1,2))
# 
# parameters <- profileParameters('LCMS-RP',nCores = 2)
# processingParameters(parameters)$peakDetection <- xcms::CentWaveParam(snthresh = 20, 
#                                                                       noise = 1000)
# processingParameters(parameters)$retentionTimeCorrection <- xcms::ObiwarpParam()
# processingParameters(parameters)$grouping <- xcms::PeakDensityParam(sampleGroups = sample_info$class,
#                                                                     maxFeatures = 300,
#                                                                     minFraction = 2/3)
# lcd <- profileProcess(file_paths,sample_info,parameters)

## Example Analysis class
a <- new('Analysis')
preTreated(a) <- analysisData(abr1$neg[,200:300],abr1$fact)
mp <- analysisParameters(c('modelling','correlations'))
a <- reAnalyse(a,mp)

## Example Assingment class
p <- assignmentParameters('FIE')

assignment <- assignMFs(peakData,p)

test_check("metaboMisc")
