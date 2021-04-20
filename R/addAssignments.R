#' addAssignments
#' @rdname addAssignments
#' @description add molecular formula assignments to an Analysis object.
#' @param analysis S4 object of class Analysis or AnalysisData
#' @param assignment S4 object of class Assignment
#' @importFrom dplyr left_join
#' @importFrom MFassign assignments
#' @importClassesFrom MFassign Assignment
#' @export

setMethod('addAssignments',signature = signature(analysis = 'Analysis',assignment = 'Assignment'),
          function(analysis,assignment){
              
              assignedFeats <- assignment %>%
                  assignments() %>%
                  select(Feature,Name)
              
              assignedFeats <- left_join(
                  tibble(Feature = analysis %>% dat(type = 'pre-treated') %>% colnames()),
                  assignedFeats, 
                  by = "Feature")
              
              assignedFeats$Name[is.na(assignedFeats$Name)] <- assignedFeats$Feature[is.na(assignedFeats$Name)] 
              
              assignedFeats <- assignedFeats %>%
                  filter(!duplicated(Feature))
              
              colnames(analysis@preTreated@data) <- assignedFeats$Name 
              
              return(analysis)
          })

#' @rdname addAssignments
#' @export

setMethod('addAssignments',signature = signature(analysis = 'AnalysisData',assignment = 'Assignment'),
          function(analysis,assignment){
              
              assignedFeats <- assignment %>%
                  assignments() %>%
                  select(Feature,Name)
              
              assignedFeats <- left_join(
                  tibble(Feature = analysis %>% dat() %>% colnames()),
                  assignedFeats, 
                  by = "Feature")
              
              assignedFeats$Name[is.na(assignedFeats$Name)] <- assignedFeats$Feature[is.na(assignedFeats$Name)] 
              
              assignedFeats <- assignedFeats %>%
                  filter(!duplicated(Feature))
              
              colnames(analysis@data) <- assignedFeats$Name 
              
              return(analysis)
          })