#' @rdname preTreatModes

setGeneric("preTreatModes", function(processedData,parameters,verbose = TRUE) {
    standardGeneric("preTreatModes")
})

#' @rdname detectMissInjections

setGeneric('detectMissInjections',function(x,idx = 'fileOrder')
    standardGeneric('detectMissInjections'))

#' @rdname detectPairwises

setGeneric('detectPairwises',function(x,cls,type)
    standardGeneric('detectPairwises'))

#' @rdname addAssignments

setGeneric('addAssignments',function(analysis,assignment)
    standardGeneric('addAssignments'))

#' @rdname reduce

setGeneric('reduce',
           function(x, 
                    isotopes = TRUE, 
                    adducts = TRUE, 
                    unknowns = FALSE)
               standardGeneric('reduce'))

#' @rdname export

setGeneric('export',function(analysis,outPath = '.')
    standardGeneric('export'))

#' @rdname featureSummary

setGeneric('featureSummary',function(x)
    standardGeneric('featureSummary'))
