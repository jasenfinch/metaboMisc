#' Detect miss injections
#' @rdname detectMissInjections
#' @description Detect miss injected samples.
#' @param x object of class `Binalysis` or `MetaboProfile`
#' @param idx sample information column to use for sample indexes
#' @return A list containing the name of sample information column used to index the miss injections and a vector of miss injection indexes.
#' @details 
#' Samples with a total ion count (TIC) below 1.5 times the inter-quartile range are detected as miss injections.
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
#' ## Detect miss injections
#' miss_injections <- detectMissInjections(analysis)
#' 
#' ## Display detected miss injections
#' miss_injections$missInjections
#' @export

setGeneric('detectMissInjections',function(x,idx = 'fileOrder')
    standardGeneric('detectMissInjections'))

#' @rdname detectMissInjections

setMethod('detectMissInjections',signature = 'Binalysis',
          function(x,idx = 'fileOrder'){
              
              i <- x %>%
                  binneR::sampleInfo() %>%
                  select(all_of(idx))
              
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

#' @importFrom stats IQR quantile

missInject <- function(TICdat,idx){
    thresh <- quantile(TICdat$value)[2] - IQR(TICdat$value) * 1.5
    
    missinjections <- TICdat %>%
        filter(value < thresh) %>%
        select(all_of(idx)) %>%
        unlist() %>%
        unname() %>%
        list(idx = idx,missInjections = .)
    return(missinjections)
}


#' Detect batch/block differences
#' @rdname detectBatchDiff
#' @description Detect batch/block differences within analytical runs for each ionisation mode.
#' @param x object of class `Binalysis` or `MetaboProfile`
#' @param by info class column to use for batch information
#' @param pthresh p-value threshold for significance
#' @return If no differences between batches are found then `NULL` is returned. If significant differences are found then a tibble is returned containing the ANOVA results for each ionisation mode and showing whether batch correction is needed. 
#' @details Analysis of Variance (ANOVA) is used to detect differences in total ion count (TIC) averages between batches/blocks. 
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
#' ## Detect batch differences
#' batch_diff <- detectBatchDiff(analysis)
#' 
#' ## Display batch diffferences
#' batch_diff
#' @export

setGeneric('detectBatchDiff',function(x, by = 'block', pthresh = 0.05)
    standardGeneric('detectBatchDiff')
)

#' @rdname detectBatchDiff
#' @importFrom binneR binnedData
#' @importFrom tidyr gather
#' @importFrom tibble rowid_to_column
#' @importFrom dplyr group_by_all
#' @importFrom tidyselect all_of

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
#' 
#' pp
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