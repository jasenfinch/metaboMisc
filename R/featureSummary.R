#' Summarise features
#' @rdname featureSummary
#' @description Summarise spectrally binned features.
#' @param x S4 object of class `Binalysis`
#' @return A tibble containing feature summaries
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
#' @importFrom dplyr n_distinct

setMethod('featureSummary',signature = 'Binalysis',
          function(x){
              x %>%
                  binnedData() %>%
                  map(~{{
                      .x %>%
                          rowid_to_column(var = 'Sample') %>%
                          gather('Feature','Intensity',-Sample)
                  }}) %>%
                  bind_rows() %>%
                  mutate(Mode = str_sub(Feature,1,1)) %>%
                  group_by(Mode) %>%
                  summarise(
                      `Number of bins` = n_distinct(Feature),
                      `Missing Data (%)` = round(length(which(Intensity == 0)) / 
                                                     length(Intensity) * 100,2),
                      .groups = 'drop') %>%
                  {{
                      .$Mode[.$Mode == 'n'] <- 'Negative'
                      .$Mode[.$Mode == 'p'] <- 'Positive'
                      .
                  }}
          })
