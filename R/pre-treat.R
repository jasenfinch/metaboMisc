#' Pre-treatment of multiple ionisation modes
#' @rdname preTreatModes
#' @description Pre-treat both positive and negative ionisation modes for `Binalysis` and `MetaboProfile` classes.
#' @param processed_data S4 object of class `Binalysis` or `MetaboProfile`
#' @param parameters object of class `AnalysisParameters` containing pre-treatment parameters
#' @param verbose console output
#' @return S4 object of class `Analysis`
#' @examples
#' library(metabolyseR)
#' 
#' ## Retrieve file paths and sample information for example data
#' files <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')[1:2]
#' 
#' info <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')[1:2,]
#' 
#' ## Detect spectral binning parameters
#' bp <- binneR::detectParameters(files)
#' 
#' ## Perform spectral binning
#' analysis <- binneR::binneRlyse(files, 
#'                                info, 
#'                                parameters = bp)
#' 
#' ## Declare pre-treatment parameters
#' pre_treatment_parameters <- analysisParameters('pre-treatment')
#' parameters(pre_treatment_parameters,
#'                         'pre-treatment') <- preTreatmentParameters(
#'  list(
#'    occupancyFilter = 'maximum',
#'    impute = 'all',
#'    transform = 'TICnorm'
#'  )
#' )
#' changeParameter(pre_treatment_parameters,'parallel') <- 'no'
#' 
#' ## Perform pre-treatment
#' pre_treated_data <- preTreatModes(analysis,
#'                                   pre_treatment_parameters)
#' @export

setGeneric("preTreatModes", function(processed_data,parameters,verbose = TRUE) {
    standardGeneric("preTreatModes")
})

#' @rdname preTreatModes
#' @importFrom  binneR binnedData

setMethod('preTreatModes',signature = 'Binalysis',
          function(processed_data,parameters,verbose = TRUE){
              binned_data <- binnedData(processed_data)
              sample_info <- binneR::sampleInfo(processed_data)
              
              preTreated <- preTreat(binned_data,sample_info,parameters,verbose = verbose)
              
              return(preTreated)
          })

#' @rdname preTreatModes

setMethod('preTreatModes',signature = 'MetaboProfile',
          function(processed_data,parameters,verbose = TRUE){
              sample_info <- profilePro::sampleInfo(processed_data)
              processed_data <- processedData(processed_data) 
              
              preTreated <- preTreat(processed_data,
                                     sample_info,
                                     parameters,
                                     verbose = verbose)
              
              return(preTreated)
          })

#' @importFrom utils packageVersion
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom methods new
#' @importFrom metabolyseR dat sinfo metabolyse
#' @importFrom metabolyseR analysisData analysisParameters parameters parameters<-
#' @importFrom crayon blue red green
#' @importFrom cli console_width
#' @importFrom lubridate seconds_to_period
#' @importFrom dplyr bind_cols
#' @importFrom stringr str_c

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
    all@log <- list(packageVersion = packageVersion('metabolyseR'),analysis = date(),verbose = FALSE)
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