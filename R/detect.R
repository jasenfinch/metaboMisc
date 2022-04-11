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

setGeneric('detectMissInjections',function(x,idx = 'injOrder')
    standardGeneric('detectMissInjections'))

#' @rdname detectMissInjections
#' @importFrom tibble as_tibble

setMethod('detectMissInjections',signature = 'Binalysis',
          function(x,idx = 'injOrder'){
              
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
#' @importFrom profilePro technique

setMethod('detectMissInjections',signature = 'MetaboProfile',
          function(x,idx = 'injOrder'){
              
              i <- x %>%
                  profilePro::sampleInfo() %>% 
                  select(all_of(idx))
              
              mi <- x %>% 
                  processedData()
              
              if (!is.list(mi)){
                  mi <- list(mi)   
              }
              
              mi <- mi %>% 
                  map(rowSums)
              
              if (mi %>% 
                  names() %>% 
                  is.na() %>% 
                  any()) {
                  names(mi) <- replace(names(mi),
                                       is.na(names(mi)),
                                       'NA')
              }
              
              mi %>% 
                  bind_cols() %>%
                  rowSums() %>%
                  as_tibble() %>%
                  bind_cols(i) %>%
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
        sort() %>% 
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
#' files <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes',ask = FALSE)[1:2]
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
                  profilePro::sampleInfo()
              
              TICdat <- x %>%
                  processedData()
              
              if (!is.list(TICdat)){
                  TICdat <- list(TICdat)
              }
              
              TICdat <- TICdat %>% 
                  map(rowSums) 
              
              if (TICdat %>% 
                  names() %>% 
                  is.na() %>% 
                  any()) {
                  names(TICdat) <- replace(names(TICdat),
                                           is.na(names(TICdat)),
                                           'NA')
              }
              
              TICdat <- TICdat %>%
                  bind_cols() %>%
                  rowid_to_column(var = 'Sample') %>%
                  mutate(batch = ri[,by] %>% 
                             unlist() %>% 
                             factor()) %>%
                  gather('Mode','TIC',-batch,-Sample)   
              
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
#' @param x S4 object of class `Binalysis`, `MetaboProfile` or `AnalysisData`
#' @param cls the name of the  sample information table column containing the sample class information
#' @param QCidx QC sample class label
#' @return S4 object of class `AnalysisParameters`
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

setGeneric('detectPretreatmentParameters',function(x,
                                                   cls = 'class',
                                                   QCidx = 'QC'){
    standardGeneric('detectPretreatmentParameters')
})

#' @rdname detectPretreatmentParameters
#' @importFrom metabolyseR changeParameter<-

setMethod('detectPretreatmentParameters',signature = 'Binalysis',
          function(x,cls = 'class',QCidx = 'QC'){
              pp <-  detectPretreatment(x)
              
              sample_info <- binneR::sampleInfo(x)
              
              if (!detectQC(sample_info,cls,QCidx)){
                  parameters(pp,'pre-treatment')$QC <- NULL
              } else {
                  changeParameter(pp,'QCidx','pre-treatment') <- QCidx
              }
              
              changeParameter(pp,'cls','pre-treatment') <- cls
              
              return(pp)
          })

#' @rdname detectPretreatmentParameters

setMethod('detectPretreatmentParameters',signature = 'MetaboProfile',
          function(x,cls = 'class',QCidx = 'QC'){
              pp <- detectPretreatment(x)
              
              sample_info <- profilePro::sampleInfo(x)
              
              if (!detectQC(sample_info,cls,QCidx)){
                  parameters(pp,'pre-treatment')$QC <- NULL
              } else {
                  changeParameter(pp,'QCidx','pre-treatment') <- QCidx
              }
              
              changeParameter(pp,'cls','pre-treatment') <- cls
              
              return(pp)
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
                                  type = 'median'
                    ))
            ),
            parameters(pre_treat_params,'pre-treatment'))
    }
    
    if (length(miss_injections$missInjections) > 0){
        parameters(pre_treat_params,'pre-treatment') <- c(
            list(
                remove = list(
                    samples = list(idx = miss_injections$idx,samples = miss_injections$missInjections)
                )),
            parameters(pre_treat_params,'pre-treatment')
        )
    }
    
    return(pre_treat_params)
} 

detectQC <- function(sample_info,cls,QCidx = 'QC'){
    any(QCidx %in% sample_info[[cls]])
}

#' Detect modelling parameters
#' @rdname detectModellingParameters
#' @description Detect modelling parameters for `Binalysis`, `MetaboProfile` or `Analysis` S4 classes. 
#' @param x S4 object of class `Binalysis`,`MetaboProfile` or `Analysis`
#' @param type detect parameters for `raw` or `pre-treated` data for `Analysis` class
#' @param cls sample information column to use for modelling
#' @param ... arguments to pass to the appropriate method
#' @return S4 object of class `AnalysisParameters`
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
#' ## Detect modelling parameters
#' modelling_parameters <- detectModellingParameters(analysis)
#' 
#' modelling_parameters
#' @export

setGeneric('detectModellingParameters',function(x,...){
    standardGeneric('detectModellingParameters')
})

#' @rdname detectModellingParameters
#' @importFrom binneR cls 

setMethod('detectModellingParameters',signature = 'Binalysis',
          function(x){
              idx <- cls(x)
              
              if (length(idx) == 0){
                  idx <- 'class'
              }
              
              sample_information <- binneR::sampleInfo(x) %>% 
                  select(all_of(idx)) %>% 
                  deframe()
              
              detectModelling(sample_information,idx)
          })

#' @rdname detectModellingParameters
#' @importFrom profilePro processingParameters

setMethod('detectModellingParameters',signature = 'MetaboProfile',
          function(x){
              idx <- x %>% 
                  processingParameters() %>% 
                  .$info %>% 
                  .$cls
              
              sample_information <- x %>% 
                  profilePro::sampleInfo() %>% 
                  select(all_of(idx)) %>% 
                  deframe()
              
              detectModelling(sample_information,idx)
          })

#' @rdname detectModellingParameters

setMethod('detectModellingParameters',signature = 'Analysis',
          function(x,type = 'pre-treated',cls = 'class'){
              
              sample_information <- x %>% 
                  sinfo(type = type) %>% 
                  select(all_of(cls)) %>% 
                  deframe()
              
              detectModelling(sample_information,cls)
          })

#' @importFrom metabolyseR modellingParameters
#' @importFrom dplyr filter

detectModelling <- function(sample_info,idx){
    
    ## Detect if regression is needed
    if (is.numeric(sample_info)){
        message('Numeric class column detected, using regression')
        if (length(sample_info) > 6){
            detected_parameters <- modellingParameters('randomForest')
        } else {
            detected_parameters <- modellingParameters('linearRegression')
        }
        
    } else {
        ## Detect classification parameters
        class_frequencies <- sample_info %>%
            tibble(class = .) %>% 
            group_by(class) %>% 
            summarise(freq = n())
        
        if (nrow(class_frequencies) < 2){
            detected_parameters <- list()
        } else {
            if (nrow(class_frequencies %>%
                     filter(freq > 5)) < (floor(length(unique(sample_info)) / 2))) {
                message('Less than 50% of classes have > 5 replicates. Using ANOVA.')
                
                detected_parameters <- modellingParameters('anova')
            } else {
                detected_parameters <- modellingParameters('randomForest')
            }   
        }
        
    }
    
    if (length(detected_parameters) > 0){
        detected_parameters[[1]]$cls <- idx 
        
        if (names(detected_parameters)[1] == 'randomForest'){
            detected_parameters[[1]]$reps <- 10
        }
    }
    
    modelling_parameters <- analysisParameters('modelling')
    parameters(modelling_parameters,'modelling') <- detected_parameters
    
    return(modelling_parameters)
}