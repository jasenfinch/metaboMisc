#' plotRSD
#' @rdname plotRSD
#' @description Plot RSD distributions of processed data in quality control samples.
#' @param analysis S4 object of class Binalysis or MetaboProfile
#' @param cls info column to use for class labels
#' @param QCidx QC sample label
#' @param QCparameters alternative parameters for QC sample pre-treatment. See details
#' @param histBins number of bins to use for histogram plotting
#' @details If QCparameters is set as NULL, the default QC pre-treatment parameters are used as given by \code{analysisParameters('preTreat')}. Alternative pre-treatment routines can be used by specifying an AnalysisParameters object for QCparameters.
#' @importFrom metabolyseR plotRSD
#' @importFrom magrittr set_names
#' @export

setMethod('plotRSD',signature = 'Binalysis',function(analysis, cls = 'class', QCidx = 'QC', QCparameters = NULL, histBins = 30){
    si <- info(analysis)
    d <- binnedData(analysis)
    
    d %>%
        names() %>%
        map(~{
            da <- d[[.]]
            
            if (. == 'n') {
                title <- 'Negative Mode'
            }
            
            if (. == 'p') {
                title <- 'Positive Mode'
            }
            
            analysisData(da,info = si) %>%
                plotRSD(cls = cls,QCidx = QCidx,QCparameters = QCparameters,histBins = histBins,title = title)
        }) %>%
        set_names(names(d))
})

#' @rdname plotRSD
#' @importFrom profilePro sampleInfo processedData
#' @export

setMethod('plotRSD',signature = 'MetaboProfile',function(analysis, cls = 'class', QCidx = 'QC', QCparameters = NULL, histBins = 30){
    si <- sampleInfo(analysis)
    d <- processedData(analysis)
    
    d %>%
        names() %>%
        map(~{
            da <- d[[.]]
            
            if (. == 'n') {
                title <- 'Negative Mode'
            }
            
            if (. == 'p') {
                title <- 'Positive Mode'
            }
            
            analysisData(da,info = si) %>%
                plotRSD(cls = cls,QCidx = QCidx,QCparameters = QCparameters,histBins = histBins,title = title)
        }) %>%
        set_names(names(d))
})