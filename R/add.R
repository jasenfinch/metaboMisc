#' Add molecular formula assignments
#' @rdname addAssignments
#' @description Add molecular formula assignments to an `Analysis` S4 class object.
#' @param analysis an object of S4 class `Analysis`
#' @param assignment an object of S4 class `Assignment` that includes assignemnts for the *m/z* features specified for argument `analysis`
#' @return An object of S4 class `Analysis` that includes the molecular formulas in the feature names for the pre-treated data.
#' @export

setGeneric('addAssignments',
           function(analysis,assignment)
           standardGeneric('addAssignments'))

#' @rdname addAssignments
#' @importFrom assignments assignedData

setMethod('addAssignments',signature = c('Analysis','Assignment'),
          function(analysis,assignment){
              dat(analysis,'pre-treated') <- assignedData(assignment)   
              
              return(analysis)
          })
