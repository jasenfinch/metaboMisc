missInject <- function(TICdat,idx){
    thresh <- quantile(TICdat$value)[2] - IQR(TICdat$value) * 1.5
    
    missinjections <- TICdat %>%
        filter(value < thresh) %>%
        select(idx) %>%
        unlist() %>%
        unname() %>%
        list(idx = idx,missInjections = .)
    return(missinjections)
}

#' detectMissInjections
#' @rdname detectMissInjections
#' @description detect miss injected samples
#' @param x object of class Binalysis or MetaboProfile
#' @param idx info column to use for sample indexes
#' @importFrom stats IQR quantile
#' @examples 
#' \dontrun{
#' files <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
#' 
#' info <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
#' 
#' analysis <- binneR::binneRlyse(files, 
#'                                info, 
#'                                parameters = binneR::detectParameters(files))
#' 
#' miss_injections <- detectMissInjections(analysis)
#' 
#' miss_injections$missInjections
#' }
#' @export

setMethod('detectMissInjections',signature = 'Binalysis',
          function(x,idx = 'fileOrder'){
              
              i <- x %>%
                  binneR::sampleInfo() %>%
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
#' @export

setMethod('detectMissInjections',signature = 'MetaboProfile',
          function(x,idx = 'fileOrder'){
              
              i <- x %>%
                  .@Info %>%
                  select(all_of(idx))
              
              if (str_detect(x@processingParameters@technique,'GCMS')) {
                  mi <- x %>%
                      .@Data %>%
                      rowSums() %>%
                      tibble(value = .) %>%
                      bind_cols(i)
              } else {
                  mi <- x %>%
                      .@Data %>%
                      map(rowSums) %>%
                      bind_cols() %>%
                      rowSums() %>%
                      as_tibble() %>%
                      bind_cols(i)    
              }
              mi %>%
                  missInject(idx = idx) 
          })

#' detectBatchDiff
#' @rdname detectBatchDiff
#' @description Detect batch differences
#' @param x object of class Binalysis or MetaboProfile
#' @param by info class column to use for batch information
#' @param pthresh p-value threshold for significance
#' @importClassesFrom binneR Binalysis
#' @importFrom binneR binnedData
#' @importFrom tidyr gather
#' @importFrom tibble rowid_to_column
#' @importFrom dplyr group_by_all
#' @importFrom tidyselect all_of
#' @export

setGeneric('detectBatchDiff',function(x, by = 'block', pthresh = 0.05)
    standardGeneric('detectBatchDiff')
)

#' @rdname detectBatchDiff

setMethod('detectBatchDiff',signature = 'Binalysis',
          function(x, by = 'block', pthresh = 0.05){
              rawInfo <- x %>%
                  binneR::sampleInfo()
              
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
#' @importFrom stringr str_detect
#' @importFrom tibble deframe

setMethod('detectBatchDiff',signature =  "MetaboProfile",
          function(x, by = 'block', pthresh = 0.05){
              ri <- x %>%
                  .@Info
              
              if (str_detect(x@processingParameters@technique,'GCMS')) {
                  TICdat <- x %>%
                      .@Data %>%
                      rowSums() %>%
                      {tibble(Sample = 1:length(.),
                              TIC = .,
                              batch = ri[,by] %>% deframe() %>% factor())}
              } else {
                  TICdat <- x %>%
                      .@Data %>%
                      map(rowSums) %>%
                      bind_cols() %>%
                      rowid_to_column(var = 'Sample') %>%
                      mutate(batch = ri[,by] %>% unlist() %>% factor()) %>%
                      gather('Mode','TIC',-batch,-Sample)   
              }
              
              diff <- batchDiff(TICdat,pthresh)
              return(diff) 
          })

#' @importFrom dplyr bind_rows mutate n
#' @importFrom tibble tibble

batchDiff <- function(TICdat,pthresh = 0.05){
    
    block_frequencies <- TICdat %>%
        group_by(batch) %>%
        summarise(n = n())
    
    if (nrow(block_frequencies) < 2) {
        message('Only 1 batch detected, skipping detection')
        return()
    }
    
    if (TRUE %in% (block_frequencies$n < 3)) {
        removal_blocks <- block_frequencies %>%
            filter(n < 3)
        
        TICdat <- TICdat %>%
            filter(!(batch %in% removal_blocks$batch))
        
        message(str_c('Batches with < 3 replicates removed: ',
                      str_c(str_c('"',removal_blocks$batch,'"'),
                            collapse = ', ')))
        
    }
    
    new_block_frequencies <- TICdat %>%
        group_by(batch) %>%
        summarise(n = n())
    
    if (nrow(new_block_frequencies) < 2) {
        message('Only 1 batch detected, skipping detection')
        return()
    }
    
    if ('Mode' %in% colnames(TICdat)) {
        ANOVAres <- TICdat %>%
            split(.$Mode)    
    } else {
        ANOVAres <- list(TICdat)
    }
    
    ANOVAres <- ANOVAres %>%
        map(~{
            res <- oneway.test(TIC~batch,.)
            res <- tibble(`F` = res$statistic,`num df` = res$parameter[1],`denom df` = res$parameter[2],`p-value` = res$p.value)
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

#' detectPairwises
#' @description Detect availble pairwise comparisons
#' @param x object of class Analysis
#' @param cls info column to use for class information
#' @param type type of analysis (classification or featureSelection)
#' @importClassesFrom metabolyseR Analysis
#' @importFrom utils combn
#' @importFrom dplyr filter select
#' @importFrom tibble as_tibble
#' @importFrom purrr map_chr
#' @importFrom stringr str_c
#' @rdname detectPairwises
#' @export

setMethod('detectPairwises',signature = 'Analysis',
          function(x,cls,type){
              if (type == 'classification') {
                  minimum <- 6
              }
              if (type == 'featureSelection') {
                  minimum <- 3
              }
              info <- x %>%
                  sinfo(type = 'pre-treated') %>% 
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

#' Detect pre-treatment parameters
#' @rdname detectPretreatmentParameters
#' @description Detect pre-treatment parameters for `Binalysis` or `MetaboProfile` class objects. 
#' @param x S4 object of class `Binalysis` or `MetaboProfile`
#' @return S4 object of class 
#' @examples
#' ## Retreive example file paths and sample information 
#' file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes') %>% 
#'    .[61:63]
#' 
#' sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes') %>% 
#'     dplyr::filter(name == 'QC01' | name == 'QC02' | name == 'QC03')
#' 
#' ## Detect spectral binning parameters
#' bp <- binneR::detectParameters(file_paths)
#' 
#' ## Perform spectral binning
#' bd <- binneR::binneRlyse(file_paths,sample_information,bp)
#' 
#' ## Detect pre-treatment parameters
#' pp <- detectPretreatmentParameters(bd) 
#' @export

setGeneric('detectPretreatmentParameters',function(x){
    standardGeneric('detectPretreatmentParameters')
})

#' @rdname detectPretreatmentParameters

setMethod('detectPretreatmentParameters',signature = 'Binalysis',
          function(x){
              detectPretreatment(x)
          })

#' @rdname detectPretreatmentParameters

setMethod('detectPretreatmentParameters',signature = 'MetaboProfile',
          function(x){
              detectPretreatment(x)
          })

#' @importFrom metabolyseR parameters<- parameters

detectPretreatment <- function(x){
    miss_injections <- detectMissInjections(x)
    batch_correction <- detectBatchDiff(x)
    
    
    pre_treat_params <- analysisParameters('pre-treatment')
    
    if (TRUE %in% batch_correction$`Correction needed`) {
        parameters(pre_treat_params,'pre-treatment') <- c(
            list(
                correction = list(
                    center = list(block = 'block',
                                  type = 'median',
                    )),
                parameters(pre_treat_params,'pre-treatment')
            ))
    }
    
    if (length(miss_injections$missInjections) > 0){
        parameters(pre_treat_params,'pre-treatment') <- c(
            list(
                remove = list(
                    samples = list(idx = miss_injections$idx,samples = miss_injections$missInjections)
                )),
            parameters(pre_treat_params)
        )
    }
    
    return(pre_treat_params)
} 