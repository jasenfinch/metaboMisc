#' Summarise `m/z` features
#' @rdname featureSummary
#' @description Summarise spectrally processed *m/z* features.
#' @param x an object of S4 class `Binalysis` or `MetaboProfile`
#' @return A tibble containing feature summaries.
#' @examples 
#' ## Retrieve file paths and sample information for example data
#' files <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')[1:2]
#' 
#' info <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')[1:2,]
#' 
#' ## Perform spectral binning
#' analysis <- binneR::binneRlyse(files, 
#'                                info, 
#'                                parameters = binneR::detectParameters(files))
#' 
#' featureSummary(analysis)
#' @export

setGeneric('featureSummary',function(x)
    standardGeneric('featureSummary'))

#' @rdname featureSummary

setMethod('featureSummary',signature = 'Binalysis',
          function(x){
              x %>%
                  binnedData() %>% 
                  featSummary()
          })

#' @rdname featureSummary

setMethod('featureSummary',signature = 'MetaboProfile',
          function(x){
              x %>% 
                  processedData() %>% 
                  featSummary()
          })

#' @importFrom dplyr n_distinct

featSummary <- function(x){
    
    if (!is.list(x)){
        x <- list(x)
    }
    
    x %>%
        map(~{{
            .x %>%
                rowid_to_column(var = 'Sample') %>%
                gather('Feature','Intensity',-Sample)
        }}) %>%
        bind_rows() %>%
        mutate(Mode = str_sub(Feature,1,1)) %>%
        mutate(Mode = replace(Mode, 
                              Mode != 'n' & Mode != 'p',
                              NA)) %>% 
        group_by(Mode) %>%
        summarise(
            `Number of bins` = n_distinct(Feature),
            `Missing Data (%)` = round(length(which(Intensity == 0)) / 
                                           length(Intensity) * 100,2),
            .groups = 'drop') %>%
        mutate(Mode = replace(Mode,Mode == 'n','Negative') %>% 
                   replace(Mode == 'p','Positive'))
}