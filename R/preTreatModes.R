
preTreat <- function(dat,info,parameters,verbose = T){
    
    preTreated <- dat %>%
        map(metabolyse,info = info,parameters = parameters,verbose = verbose)
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
          function(processedData,parameters,verbose = T){
              dat <- binnedData(processedData)
              sampleInfo <- info(processedData)

              preTreated <- preTreat(dat,sampleInfo,parameters,verbose = verbose)
              
              return(preTreated)
})

#' @rdname preTreatModes

setMethod('preTreatModes',signature = 'MetaboProfile',
          function(processedData,parameters,verbose = T){
              dat <- processedData@Data
              sampleInfo <- processedData@Info
              
              preTreated <- preTreat(dat,sampleInfo,parameters,verbose = verbose)
              
              return(preTreated)
          })