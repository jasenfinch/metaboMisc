#' @importFrom utils packageVersion
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom methods new
#' @importFrom metabolyseR dat sinfo
#' @importFrom metabolyseR analysisData analysisParameters
#' @importFrom crayon blue red green
#' @importFrom cli console_width
#' @importFrom lubridate seconds_to_period

preTreat <- function(dat,info,parameters,verbose = TRUE){
    
    p <- analysisParameters('preTreat')
    p@preTreat <- parameters@preTreat
    
    if (verbose == TRUE) {
        analysisStart <- date()
        startTime <- proc.time()
        version <- packageVersion('metabolyseR') %>%
            as.character()
        cat('\n',blue('metabolyseR'),' ',red(str_c('v',version)),' ',analysisStart,'\n',sep = '')
        cat(rep('_',console_width()),'\n',sep = '')
        print(p)
        cat(rep('_',console_width()),'\n\n',sep = '')
    }
    
    names(dat)[names(dat) == 'n'] <- 'Negative mode'
    names(dat)[names(dat) == 'p'] <- 'Positive mode'
    
    preTreated <- names(dat) %>%
        map(~{ 
            m <- .
            d <- dat[[m]]
            
            if (verbose == TRUE) {
                startTime <- proc.time()
                cat(blue(m),cli::symbol$continue,'\r',sep = '') 
            }
            
            res <- metabolyse(d,info = info,parameters = p,verbose = FALSE)
            
            if (verbose == TRUE) {
                endTime <- proc.time()
                elapsed <- {endTime - startTime} %>%
                    .[3] %>%
                    round(1) %>%
                    seconds_to_period() %>%
                    str_c('[',.,']')
                cat(blue(m),' \t\t',green(cli::symbol$tick),' ',elapsed,'\n',sep = '')
            }
            
            return(res)
        })
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
    
    if (verbose == TRUE) {
        endTime <- proc.time()
        elapsed <- {endTime - startTime} %>%
            .[3] %>%
            round(1) %>%
            seconds_to_period() %>%
            str_c('[',.,']')
        
        cat(rep('_',console_width()),'\n',sep = '')
        cat('\n',green('Complete! '),elapsed,'\n\n',sep = '')
    }
    
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