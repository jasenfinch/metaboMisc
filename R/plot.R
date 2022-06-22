#' Plot relative standard deviation (RSD) distributions
#' @rdname plotRSD
#' @description Plot RSD distributions of processed data.
#' @param analysis S4 object of class `Binalysis` or `MetaboProfile`
#' @param cls info column to use for class labels.
#' @return A list of plots of RSD distributions
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
#' ## Plot RSD distributions
#' plotRSD(analysis)
#' @export

setMethod('plotRSD',signature = 'Binalysis',function(analysis, cls = 'class'){
    si <- binneR::sampleInfo(analysis)
    d <- binnedData(analysis)
    
    rsdPlot(d,
            si,
            cls = cls)
})

#' @rdname plotRSD
#' @importFrom profilePro processedData
#' @export

setMethod('plotRSD',signature = 'MetaboProfile',function(analysis, cls = 'class'){
    si <- profilePro::sampleInfo(analysis)
    d <- processedData(analysis)
    
    rsdPlot(d,
            si,
            cls = cls) 
})

#' @importFrom metabolyseR plotRSD raw<-
#' @importFrom patchwork plot_annotation
#' @importFrom ggplot2 theme element_text

rsdPlot <- function(d,si,cls = 'class'){
    d %>%
        names() %>%
        map(~{
            
            if (is.na(.x)){
                title <- ''
                da <- d[[1]]
            } else {
                da <- d[[.x]]
                
                if (.x == 'n') {
                    title <- 'Negative Mode'
                }
                
                if (.x == 'p') {
                    title <- 'Positive Mode'
                }   
            }
            
            ad <- analysisData(da,si)
            
            ad %>%
                plotRSD(cls = cls) +
                plot_annotation(title = title,
                                theme = theme(plot.title = element_text(face = 'bold',
                                                                        size = 14)))
        }) %>%
        setNames(names(d))
}