#' @importFrom utils packageVersion
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom methods new
#' @importFrom metabolyseR dat sinfo
#' @importFrom metabolyseR analysisData analysisParameters parameters parameters<-
#' @importFrom crayon blue red green
#' @importFrom cli console_width
#' @importFrom lubridate seconds_to_period

preTreat <- function(raw_data,sample_info,parameters,verbose = TRUE){
    
    p <- analysisParameters('pre-treatment')
    parameters(p,'pre-treatment') <- parameters(parameters,'pre-treatment')
    
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
    
    names(raw_data)[names(raw_data) == 'n'] <- 'Negative mode'
    names(raw_data)[names(raw_data) == 'p'] <- 'Positive mode'
    
    pre_treated <- names(raw_data) %>%
        map(~{ 
            m <- .
            d <- raw_data[[m]]
            
            if (verbose == TRUE) {
                startTime <- proc.time()
                cat(blue(m),cli::symbol$continue,'\r',sep = '') 
            }
            
            res <- metabolyse(d,info = sample_info,parameters = p,verbose = FALSE)
            
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
    
    pre_treated_data <- pre_treated %>%
        map(dat,type = 'pre-treated') %>%
        bind_cols()
    
    pre_treated_info <- sinfo(pre_treated[[1]],type = 'pre-treated')
    
    all <- new('Analysis')
    all@log <- list(packageVersion = packageVersion('metabolyseR'),analysis = date(),verbose = F)
    all@parameters <- parameters
    raw(all) <- analysisData(bind_cols(raw_data), sample_info)
    preTreated(all) <- analysisData(pre_treated_data,pre_treated_info)
    
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
#' @importMethodsFrom  binneR binnedData
#' @importFrom metabolyseR metabolyse
#' @importFrom dplyr bind_cols
#' @importClassesFrom binneR Binalysis
#' @importClassesFrom profilePro MetaboProfile
#' @export

setMethod('preTreatModes',signature = 'Binalysis',
          function(processedData,parameters,verbose = T){
              binned_data <- binnedData(processedData)
              sample_info <- binneR::sampleInfo(processedData)
              
              preTreated <- preTreat(binned_data,sample_info,parameters,verbose = verbose)
              
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