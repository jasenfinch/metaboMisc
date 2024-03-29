#' Detect miss injections
#' @rdname detectMissInjections
#' @description Detect miss injected samples or samples with a total ion count below a percentage threshold of the median.
#' @param x object of S4 class `Binalysis` or `MetaboProfile`
#' @param idx the sample information column to use for sample indexes
#' @param threshold the percentage of the median TIC below which samples will be considered miss injections.
#' @return A list containing the name of the sample information column used to index the miss injections and a vector of miss injection indexes.
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
#' miss_injections
#' @export

setGeneric('detectMissInjections',function(x,idx = 'injOrder',threshold = 25)
    standardGeneric('detectMissInjections'))

#' @rdname detectMissInjections
#' @importFrom tibble as_tibble

setMethod('detectMissInjections',signature = 'Binalysis',
          function(x,idx = 'injOrder',threshold = 25){
              
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
                  missInject(idx = idx,threshold)
          })

#' @rdname detectMissInjections
#' @importFrom profilePro technique

setMethod('detectMissInjections',signature = 'MetaboProfile',
          function(x,idx = 'injOrder',threshold = 25){
              
              i <- x %>%
                  profilePro::sampleInfo() %>% 
                  select(all_of(idx))
              
              mi <- x %>% 
                  processedData()
              
              if (is.data.frame(mi)){
                  mi <- list(mi)   
              }
              
              mi <- mi %>% 
                  map(rowSums)
              
              if (mi %>% 
                  names() %>% 
                  {is.null(.) | is.na(.)} %>% 
                  any()) {
                  names(mi) <- replace(names(mi),
                                       is.null(names(mi)) | is.na(names(mi)),
                                       'NA')
              }
              
              mi %>% 
                  bind_cols() %>%
                  rowSums() %>%
                  as_tibble() %>%
                  bind_cols(i) %>%
                  missInject(idx = idx,threshold) 
          })

#' @importFrom stats median

missInject <- function(TICdat,idx,threshold){
    TIC_threshold <- median(TICdat$value) * threshold/100
    
    missinjections <- TICdat %>%
        filter(value < TIC_threshold) %>%
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
#' @param x object of S4 class `Binalysis` or `MetaboProfile`
#' @param by info class column to use for batch/block information
#' @param pthresh p-value threshold for significance
#' @return If no differences between batches are found then `NULL` is returned. If significant differences are found then a tibble is returned containing the ANOVA results for each ionisation mode and showing whether batch correction is needed. 
#' @details Analysis of Variance (ANOVA) is used to detect differences in total ion count (TIC) averages between batches/blocks. 
#' @examples 
#' ## Retrieve file paths and sample information for example data
#' files <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes',ask = FALSE)
#' 
#' info <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
#' 
#' ## Perform spectral binning
#' analysis <- binneR::binneRlyse(files, 
#'                                info, 
#'                                parameters = binneR::detectParameters(files))
#' 
#' ## Detect batch differences
#' batch_diff <- detectBatchDiff(analysis)
#' 
#' ## Display batch differences
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
              
              if (is.data.frame(TICdat)){
                  TICdat <- list(TICdat)
              }
              
              TICdat <- TICdat %>% 
                  map(rowSums) 
              
              if (TICdat %>% 
                  names() %>% 
                  {is.null(.) | is.na(.)} %>% 
                  any()) {
                  names(TICdat) <- replace(names(TICdat),
                                           is.null(names(TICdat)) | is.na(names(TICdat)),
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
#' @description Detect suitable pre-treatment parameters for `Binalysis` or `MetaboProfile` class objects. 
#' @param x S4 object of class `Binalysis`, `MetaboProfile` or `AnalysisData`
#' @param cls the name of the  sample information table column containing the sample class information
#' @param QCidx QC sample class label
#' @param miss_injections TRUE/FALSE. Detect the presence of possible miss injections and include parameters to remove these if necessary.
#' @param batch_correction TRUE/FALSE. Detect if a batch correction is necessary and include parameters to perform this if necessary.
#' @param threshold the percentage of the median TIC below which samples will be considered miss injections. This will be ignored if `miss_injections = FALSE`.
#' @return An object of S4 class `AnalysisParameters`
#' @examples
#' ## Retreive example file paths and sample information 
#' file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes') 
#' 
#' sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes') 
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
                                                   QCidx = 'QC',
                                                   miss_injections = TRUE,
                                                   batch_correction = TRUE,
                                                   threshold = 25){
    standardGeneric('detectPretreatmentParameters')
})

#' @rdname detectPretreatmentParameters
#' @importFrom metabolyseR changeParameter<-

setMethod('detectPretreatmentParameters',signature = 'Binalysis',
          function(x,
                   cls = 'class',
                   QCidx = 'QC',
                   miss_injections = TRUE,
                   batch_correction = TRUE,
                   threshold = 25){
              pp <-  detectPretreatment(x,
                                        miss_injections,
                                        batch_correction,
                                        threshold)
              
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
          function(x,
                   cls = 'class',
                   QCidx = 'QC',
                   miss_injections = TRUE,
                   batch_correction = TRUE,
                   threshold = 25){
              pp <- detectPretreatment(x,
                                       miss_injections,
                                       batch_correction,
                                       threshold)
              
              chromatographic_technique <- technique(x) %>% 
                  str_split_fixed('-',2) %>% 
                  .[,1]
              
              rsd <- switch(chromatographic_technique,
                            LCMS = 20,
                            GCMS = 30)
              
              changeParameter(pp,'RSDthresh') <- rsd
              
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

detectPretreatment <- function(x,miss_injections,batch_correction,threshold){
    if(isTRUE(miss_injections)) {
        miss_injections <- detectMissInjections(x,threshold = threshold)
    } else {
        miss_injections <- NULL
    } 
    
    
    if (isTRUE(batch_correction)) {
        batch_correction <- detectBatchDiff(x)
    } else {
        batch_correction <- NULL
    } 
    
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
#' @description Detect suitable modelling parameters for `Binalysis`, `MetaboProfile` or `Analysis` S4 classes. 
#' @param x an object of S4 class `Binalysis`,`MetaboProfile` or `Analysis`
#' @param type detect modelling parameters for `raw` or `pre-treated` data for the `Analysis` S4 class
#' @param cls sample information column to use as the response for modelling
#' @param ... arguments to pass to the appropriate method
#' @return  and object of S4 class `AnalysisParameters`
#' @examples 
#' ## Retrieve file paths and sample information for example data
#' files <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
#' 
#' info <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
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
    
    ## Where regression parameters are required
    if (is.numeric(sample_info)){
        message('Numeric class column detected, using regression')
        
        detected_parameters <- modellingParameters('randomForest')
        
        if (length(sample_info) < 6){
            detected_parameters <- modellingParameters('linearRegression')
        }
        
    }
    
    ## Where classification parameters are required
    if (!is.numeric(sample_info)){
        
        detected_parameters <- modellingParameters('randomForest')
        
        class_frequencies <- sample_info %>%
            tibble(class = .) %>% 
            group_by(class) %>% 
            summarise(freq = n())
        
        if (nrow(class_frequencies) < 2){
            detected_parameters[[1]]$cls <- NULL
            detected_parameters[[1]] <- c(
                list(
                    cls = NULL),
                detected_parameters[[1]]
            )
        }
        
        if (nrow(class_frequencies %>%
                 filter(freq > 5)) < (floor(length(unique(sample_info)) / 2))) {
            message('Less than 50% of classes have > 5 replicates. Using ANOVA.')
            
            detected_parameters <- modellingParameters('anova')
        }
        
    }
    
    if (!is.null(detected_parameters$cls)){
        detected_parameters[[1]]$cls <- idx 
    }
    
    if (names(detected_parameters)[1] == 'randomForest'){
        detected_parameters[[1]]$reps <- 10
    }
    
    modelling_parameters <- analysisParameters('modelling')
    parameters(modelling_parameters,'modelling') <- detected_parameters
    
    return(modelling_parameters)
}