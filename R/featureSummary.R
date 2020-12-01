#' Summarise features
#' @rdname featureSummary
#' @description Summarise spectral binning features.
#' @param x S4 object of class Binalysis
#' @importFrom dplyr n_distinct
#' @export

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
                      .$Mode[.$Mode == 'n'] = 'Negative'
                      .$Mode[.$Mode == 'p'] = 'Positive'
                      .
                  }}
          })
