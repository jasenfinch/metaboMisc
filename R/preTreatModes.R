#' @importFrom utils packageVersion
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom methods new
#' @importFrom metabolyseR analysisParameters dat info analysisData

preTreat <- function(dat,info,parameters,verbose = T){
    
    p <- analysisParameters('preTreat')
    p@preTreat <- parameters@preTreat
    
    preTreated <- dat %>%
        map(metabolyse,info = info,parameters = p,verbose = verbose)
    preTreatedDat <- preTreated %>%
        map(~{
            .@preTreated %>%
                dat()
        }) %>%
        bind_cols()
    
    preTreatedInf <- preTreated[[1]]@preTreated %>% 
        metabolyseR::info()
    
    all <- new('Analysis')
    all@log <- list(packageVersion = packageVersion('metabolyseR'),analysis = date(),verbose = F)
    all@parameters <- parameters
    all@rawData <- analysisData(bind_cols(dat), info)
    all@preTreated <- analysisData(preTreatedDat,preTreatedInf)
    
    return(all)
}



#' preTreatModes
#' @rdname preTreatModes
#' @description Pre-treat both positive and negative ionisation modes
#' @param processedData of class Binalysis or MetaboProfile
#' @param parameters object of class AnalysisParameters containing pre-treatment parameters
#' @param verbose console output
#' @importFrom binneR binnedData info
#' @importFrom metabolyseR metabolyse preTreatedData preTreatedInfo
#' @importFrom dplyr bind_cols
#' @importClassesFrom binneR Binalysis
#' @importClassesFrom profilePro MetaboProfile

setMethod('preTreatModes',signature = 'Binalysis',
          function(processedData,parameters,verbose = T){
              dat <- binnedData(processedData)
              sampleInfo <- binneR::info(processedData)

              preTreated <- preTreat(dat,sampleInfo,parameters,verbose = verbose)
              
              return(preTreated)
})

#' @rdname preTreatModes

setMethod('preTreatModes',signature = 'MetaboProfile',
          function(processedData,parameters,verbose = T){
              dat <- processedData@Data
              sampleInfo <- processedData %>%
                  metabolyseR::info()
              
              preTreated <- preTreat(dat,sampleInfo,parameters,verbose = verbose)
              
              return(preTreated)
          })