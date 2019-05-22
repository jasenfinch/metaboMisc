#' addAssignments
#' @rdname addAssignments
#' @description add molecular formula assignments to an Analysis object.
#' @param analysis S4 object of class Analysis
#' @param assignment S4 object of class Assignment
#' @importFrom dplyr left_join
#' @importFrom MFassign assignments
#' @importClassesFrom MFassign Assignment

setMethod('addAssignments',signature = signature(analysis = 'Analysis',assignment = 'Assignment'),
          function(analysis,assignment){
              
              assignedFeats <- assignment %>%
                  assignments() %>%
                  select(Feature,Name)
              
              assignedFeats <- left_join(
                  tibble(Feature = analysis %>% preTreatedData() %>% colnames()),
                  assignedFeats, 
                  by = "Feature")
              
              assignedFeats$Name[is.na(assignedFeats$Name)] <- assignedFeats$Feature[is.na(assignedFeats$Name)] 
              
              colnames(analysis@preTreated$Data) <- assignedFeats$Name 
              
              return(analysis)
          })