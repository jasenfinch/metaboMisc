#' @rdname preTreatModes

setGeneric("preTreatModes", function(processedData,parameters,verbose = T) {
    standardGeneric("preTreatModes")
})

#' @rdname detectBatchDiff
#' @export

setGeneric('detectBatchDiff',function(x, by = 'block', pthresh = 0.05){
    standardGeneric('detectBatchDiff')
})

#' @rdname detectMissInjections
#' @export

setGeneric('detectMissInjections',function(x,idx = 'fileOrder'){
    standardGeneric('detectMissInjections')
})

#' @rdname detectPairwises
#' @export

setGeneric('detectPairwises',function(x,cls,type){
    standardGeneric('detectPairwises')
})

#' @rdname addAssignments
#' @export 

setGeneric('addAssignments',function(analysis,assignment){
    standardGeneric('addAssignments')
})

#' @rdname reduce
#' @export 

setGeneric('reduce',function(x, isotopes = T, adducts = T, unknowns = F){
    standardGeneric('reduce')
})

#' @rdname export
#' @export

setGeneric('export',function(analysis,outPath = '.'){
    standardGeneric('export')
})
