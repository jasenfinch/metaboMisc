
preTreat <- function(dat,info,parameters){
    
    preTreated <- dat %>%
        map(metabolyse,info = info,parameters = parameters)
    preTreatedDat <- preTreated %>%
        map(~{
            .@preTreated$Data
        }) %>%
        bind_cols()
    
    preTreatedInf <- preTreated[[1]]@preTreated$Info
    
    all <- new('Analysis')
    all@log <- list(packageVersion = packageVersion('metabolyseR'),analysis = date(),verbose = F)
    all@parameters <- parameters
    all@rawData <- list(Data = bind_cols(dat), Info = sampleInfo)
    all@preTreated <- list(Data = preTreatedDat,Info = preTreatedInf)
    
    return(all)
}



#' preTreatModes
#' @rdname preTreatModes
#' @importFrom binneR binnedData info
#' @importFrom metabolyseR metabolyse preTreatedData preTreatedInfo
#' @importFrom dplyr bind_cols
#' @importClassesFrom binneR Binalysis
#' @importClassesFrom profilePro MetaboProfile

setMethod('preTreatModes',signature = 'Binalysis',
          function(processedData,parameters){
              dat <- binnedData(processedData)
              sampleInfo <- info(processedData)

              preTreated <- preTreat(dat,sampleInfo,parameters)
              
              return(preTreated)
})

#' @rdname preTreatModes

setMethod('preTreatModes',signature = 'MetaboProfile',
          function(processedData,parameters){
              dat <- processedData@Data
              sampleInfo <- processedData@Info
              
              preTreated <- preTreat(dat,sampleInfo,parameters)
              
              return(preTreated)
          })