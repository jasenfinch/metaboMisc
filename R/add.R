#' Add molecular formula assignments
#' @rdname addAssignments
#' @description Add molecular formaul assignments to an `Analysis` class object.
#' @param analysis S4 object of class `Analysis`
#' @param assignment S4 object of class `Assignment`
#' @return An S4 object of class `Analysis`
#' @details Molecular formula assignments are added to the `Analysis` pre-treated data 
#' @export

setGeneric('addAssignments',
           function(analysis,assignment)
           standardGeneric('addAssignments'))

#' @rdname addAssignments
#' @importFrom MFassign assignedData

setMethod('addAssignments',signature = c('Analysis','Assignment'),
          function(analysis,assignment){
              dat(analysis,'pre-treated') <- assignedData(assignment)   
              
              return(analysis)
          })
