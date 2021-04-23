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
