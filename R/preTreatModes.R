#' @importFrom utils packageVersion
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom methods new
#' @importFrom metabolyseR dat sinfo
#' @importFrom metabolyseR analysisData analysisParameters

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
        sinfo()
    
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
#' @importMethodsFrom  binneR binnedData info
#' @importFrom metabolyseR metabolyse preTreatedData preTreatedInfo
#' @importFrom dplyr bind_cols
#' @importClassesFrom binneR Binalysis
#' @importClassesFrom profilePro MetaboProfile
#' @export

setMethod('preTreatModes',signature = 'Binalysis',
          function(processedData,parameters,verbose = T){
              dat <- binnedData(processedData)
              sampleInfo <- info(processedData)

              preTreated <- preTreat(dat,sampleInfo,parameters,verbose = verbose)
              
              return(preTreated)
})

#' @rdname preTreatModes
#' @export

setMethod('preTreatModes',signature = 'MetaboProfile',
          function(processedData,parameters,verbose = T){
              dat <- processedData@Data
              sInfo <- processedData@Info
              
              preTreated <- preTreat(dat,sInfo,parameters,verbose = verbose)
              
              return(preTreated)
          })