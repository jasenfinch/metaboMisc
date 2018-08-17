#' preTreatModes
#' @rdname preTreatModes
#' @importFrom binneR binnedData info
#' @importFrom metabolyseR metabolyse preTreatedData preTreatedInfo
#' @importFrom dplyr bind_cols
#' @importClassesFrom binneR Binalysis

setMethod('preTreatModes',signature = 'Binalysis',
          function(binalysis,parameters){
              dat <- binnedData(binalysis)
              sampleInfo <- info(binalysis)

              neg <- metabolyse(dat$n,sampleInfo,parameters)
              pos <- metabolyse(dat$p,sampleInfo,parameters)

              all <- new('Analysis')
              all@log <- list(packageVersion = packageVersion('metabolyseR'),analysis = date(),verbose = F)
              all@parameters <- parameters
              all@rawData <- list(Data = bind_cols(dat$n,dat$p), Info = sampleInfo)
              all@preTreated <- list(Data = bind_cols(preTreatedData(neg),preTreatedData(pos)),Info = preTreatedInfo(neg))

              return(all)
})