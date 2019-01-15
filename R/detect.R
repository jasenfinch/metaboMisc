#' detectMissInjections
#' @rdname detectMissInjections
#' @description detect miss injected samples
#' @importFrom stats IQR quantile

setMethod('detectMissInjections',signature = 'Binalysis',
          function(x,idx = 'fileOrder'){
              
              i <- x %>%
                  info() %>%
                  select(idx)
              
              x %>%
                  binnedData %>%
                  map(rowSums) %>%
                  bind_cols() %>%
                  rowSums() %>%
                  as_tibble() %>%
                  bind_cols(i) %>%
                  missInject(idx = idx)
          })

#' @rdname detectMissInjections
setMethod('detectMissInjections',signature = 'MetaboProfile',
          function(x,idx = 'fileOrder'){
              
              i <- x %>%
                  .@Info %>%
                  select(idx)
              
              x %>%
                  .@Data %>%
                  map(rowSums) %>%
                  bind_cols() %>%
                  rowSums() %>%
                  as_tibble() %>%
                  bind_cols(i) %>%
                  missInject(idx = idx)
          })

batchDiff <- function(TICdat,pthresh){
    ANOVAres <- TICdat %>%
        split(.$Mode) %>%
        map(~{
            res <- oneway.test(TIC~batch,.)
            res <- tibble(F = res$statistic,`num df` = res$parameter[1],`denom df` = res$parameter[2],`p-value` = res$p.value)
            if (res$`p-value` < pthresh) {
                res <- res %>%
                    mutate(`Correction needed` = TRUE)
            } else {
                res <- res %>%
                    mutate(`Correction needed` = FALSE)
            }
            return(res)
        }) %>%
        bind_rows(.id = 'Mode')
    return(ANOVAres)
}

#' detectBatchDiff
#' @rdname detectBatchDiff
#' @importClassesFrom binneR Binalysis
#' @importFrom binneR binnedData info
#' @importFrom tidyr gather

setMethod('detectBatchDiff',signature = 'Binalysis',
          function(x, by = 'block', pthresh = 0.05){
              rawInfo <- x %>%
                  info()
              
              TICdat <- x %>%
                  binnedData %>%
                  map(rowSums) %>%
                  bind_cols() %>%
                  rowid_to_column(var = 'Sample') %>%
                  mutate(batch = rawInfo[,by] %>% unlist() %>% factor()) %>%
                  gather('Mode','TIC',-batch,-Sample)
              
              diff <- batchDiff(TICdat,pthresh)
              return(diff)
          }
)

#' @rdname detectBatchDiff

setMethod('detectBatchDiff',signature =  "MetaboProfile",
          function(x, by = 'block', pthresh = 0.05){
              rawInfo <- x %>%
                  .@Info
              
              TICdat <- x %>%
                  .@Data %>%
                  map(rowSums) %>%
                  bind_cols() %>%
                  rowid_to_column(var = 'Sample') %>%
                  mutate(batch = rawInfo[,by] %>% unlist() %>% factor()) %>%
                  gather('Mode','TIC',-batch,-Sample)
              
              diff <- batchDiff(TICdat,pthresh)
              return(diff) 
          })

#' detectPairwises
#' @importClassesFrom metabolyseR Analysis
#' @importFrom metabolyseR preTreatedInfo
#' @rdname detectPairwises

setMethod('detectPairwises',signature = 'Analysis',
          function(x,cls,type){
              if (type == 'classification') {
                  minimum <- 6
              }
              if (type == 'featureSelection') {
                  minimum <- 3
              }
              info <- x %>%
                  preTreatedInfo() %>%
                  select(!!cls) %>%
                  unlist() %>%
                  table() %>%
                  as_tibble() %>%
                  filter(n >= minimum) %>%
                  select(1) %>%
                  unlist() %>%
                  sort()
              if (length(info) > 0) {
                  com <- combn(info,2) %>%
                      t() %>%
                      split(1:nrow(.)) %>%
                      map_chr(str_c,collapse = '~')
              } else {
                  com <- character()
              }
              return(com)
          }
)