#' Export results
#' @rdname export
#' @description Export data tables from `Binalysis`,`MetaboProfile`, `Analysis` and `Assignment` classes.
#' @param x S4 object of class `Binalysis`, `MetaboProfile`, `Analysis` or `Assignment`,
#' @param outPath directory path to export to.
#' @param type data type to extract. `raw` or `pre-treated`
#' @param idx sample information column name to use as sample IDs
#' @param ... arguments to pass to relevant method
#' @return A character vector of exported file paths.
#' @export

setGeneric('exportData',function(x,outPath = '.',...)
    standardGeneric('exportData'))

#' @rdname export
#' @importFrom binneR binnedData
#' @importFrom stringr str_remove_all
#' @importFrom purrr map_chr
#' @importFrom jfmisc exportCSV

setMethod('exportData',signature = 'Binalysis',
          function(x,outPath = '.'){
              bd <- x %>%
                  binnedData() 
              
              i <- x %>% 
                  binneR::sampleInfo()
              
              if (TRUE %in% duplicated(i$name)) {
                  i <- fixNames(i)
              }
              
              names(bd)[names(bd) == 'p'] <- 'positive'
              names(bd)[names(bd) == 'n'] <- 'negative'
              
              file_paths <- bd %>%
                  names() %>%
                  map_chr(~{
                      bind_cols(bd[[.x]],i %>% 
                                    select(name)) %>%
                          gather('m/z','Intensity',-name) %>%
                          spread(name,Intensity) %>%
                          mutate(`m/z` = str_remove_all(`m/z`, '[:alpha:]') %>% as.numeric()) %>%
                          exportCSV(str_c(outPath,'/',.x,'_mode_processed_data.csv'))
                  })
              
              return(file_paths)
          })

#' @rdname export

setMethod('exportData',signature = 'MetaboProfile',
          function(x,outPath = '.'){
              
              i <- x %>%
                  profilePro::sampleInfo()
              
              if (TRUE %in% duplicated(i$name)) {
                  i <- fixNames(i)
              }
              
              pd <- x %>%
                  processedData() 
              
              if (is.data.frame(pd)){
                  file_paths <- pd %>% 
                      bind_cols(i %>% select(name)) %>% 
                      gather('Feature','Intensity',-name) %>%
                      spread(name,Intensity) %>%
                      exportCSV(str_c(outPath,'/','processed_data.csv'))
              } else {
                  names(pd)[is.na(names(pd))] <- '1'
                  
                  file_paths <- pd %>%
                      names() %>%
                      map_chr(~{
                          prefix <- .x
                          
                          if (prefix != 'n' | prefix != 'p'){
                              prefix <- ''
                          } else {
                              prefix <- switch(prefix,
                                               n = 'negative_mode_',
                                               p = 'positive_mode_')   
                          }
                          
                          bind_cols(pd[[.x]],i %>% select(name)) %>%
                              gather('Feature','Intensity',-name) %>%
                              spread(name,Intensity) %>%
                              exportCSV(str_c(outPath,'/',prefix,'processed_data.csv'))
                      })
              }
              
              return(file_paths)
          })

#' @rdname export

setMethod('exportData',signature = 'Analysis',
          function(x,outPath = '.',type = 'raw',idx = 'name'){
              i <- x %>%
                  sinfo(type = type)
              
              if (TRUE %in% duplicated(i[[idx]])) {
                  i <- fixNames(i)
              }
              
              x %>%
                  dat(type = type) %>%
                  bind_cols(i %>% select(all_of(idx))) %>%
                  gather('m/z','Intensity',-idx) %>%
                  spread(idx,Intensity) %>%
                  mutate(Mode = str_sub(`m/z`,1,1)) %>%
                  mutate(`m/z` = str_split_fixed(`m/z`,' ',2)[,1] %>%
                             str_remove_all('[:alpha:]') %>%
                             as.numeric()) %>%
                  select(Mode,everything()) %>%
                  exportCSV(str_c(outPath,'/',type,'_data.csv'))
          })

#' @rdname export

setMethod('exportData',signature = 'Assignment',
          function(x,outPath = '.'){
              x %>% 
                  assignedData() %>% 
                  exportCSV(str_c(outPath,'/assigned_data.csv'))
          })

#' @rdname export
#' @export

setGeneric('exportSampleInfo',function(x,outPath = '.',...)
    standardGeneric('exportSampleInfo'))

#' @rdname export

setMethod('exportSampleInfo',signature = 'Binalysis',
          function(x,outPath = '.'){
              
              i <- x %>%
                  binneR::sampleInfo()
              
              if (TRUE %in% duplicated(i$name)) {
                  i <- fixNames(i)
              }
              
              exportCSV(i,str_c(outPath,'/sample_information.csv'))
          })

#' @rdname export

setMethod('exportSampleInfo',signature = 'MetaboProfile',
          function(x,outPath = '.'){
              i <- x %>%
                  profilePro::sampleInfo()
              
              if (TRUE %in% duplicated(i$name)) {
                  i <- fixNames(i)
              }
              
              exportCSV(i,str_c(outPath,'/sample_information.csv'))
          })

#' @rdname export

setMethod('exportSampleInfo',signature = 'Analysis',
          function(x,outPath = '.',type = 'raw'){
              si <- x %>% 
                  sinfo(type = type)
              
              exportCSV(si,str_c(outPath,'/',type,'_sample_information.csv'))
          })

#' @rdname export
#' @export

setGeneric('exportAccurateData',function(x,outPath = '.')
    standardGeneric('exportAccurateData'))

#' @rdname export
#' @importFrom binneR accurateData

setMethod('exportAccurateData',signature = 'Binalysis',
          function(x,outPath = '.'){
              bi <- x %>% 
                  accurateData()
              
              exportCSV(bi,str_c(outPath,'/accurate_data.csv'))
          })

#' @rdname export
#' @export

setGeneric('exportPeakInfo',function(x,outPath = '.')
    standardGeneric('exportPeakInfo'))

#' @rdname export

setMethod('exportPeakInfo',signature = 'MetaboProfile',
          function(x,outPath = '.'){
              exportCSV(peakInfo(x) %>% 
                            select(-peakidx),str_c(outPath,'/peak_info.csv'))
          })

#' @rdname export
#' @export

setGeneric('exportModellingMetrics',function(x,outPath = '.')
    standardGeneric('exportModellingMetrics'))

#' @rdname export
#' @importFrom metabolyseR metrics

setMethod('exportModellingMetrics',signature = 'Analysis',
          function(x,outPath = '.'){
              performance <- x %>% 
                  metrics()
              
              if (nrow(performance) > 0){
                  exportCSV(performance,str_c(outPath,'/modelling_performance_metrics.csv'))
              }
          })

#' @rdname export
#' @export

setGeneric('exportModellingImportance',function(x,outPath = '.')
    standardGeneric('exportModellingImportance'))

#' @rdname export
#' @importFrom metabolyseR importance

setMethod('exportModellingImportance',signature = 'Analysis',
          function(x,outPath = '.'){
              importances <- x %>% 
                  importance()
              
              if (nrow(importances) > 0){
                  exportCSV(importances,str_c(outPath,'/modelling_importance_metrics.csv'))
              }
          })

#' @rdname export
#' @export

setGeneric('exportModelling',function(x,outPath = '.')
    standardGeneric('exportModelling'))

#' @rdname export

setMethod('exportModelling',signature = 'Analysis',
          function(x,outPath = '.'){
              m_fp <- exportModellingMetrics(x,outPath)
              i_fp <- exportModellingImportance(x,outPath)
              
              return(c(m_fp,i_fp))
          })

#' @rdname export
#' @export

setGeneric('exportCorrelations',function(x,outPath = '.')
    standardGeneric('exportCorrelations'))

#' @rdname export
#' @importFrom metabolyseR analysisResults

setMethod('exportCorrelations',signature = 'Analysis',
          function(x,outPath = '.'){
              correl <- x %>% 
                  analysisResults('correlations')
              
              if (length(correl) > 0){
                  exportCSV(correl,str_c(outPath,'/correlations.csv'))
              }
          })

#' @rdname export
#' @export

setGeneric('exportAssignments',function(x,outPath = '.')
    standardGeneric('exportAssignments'))

#' @rdname export
#' @importFrom assignments assignments

setMethod('exportAssignments',signature = 'Assignment',
          function(x,outPath = '.'){
              x %>% 
                  assignments() %>% 
                  select(-Iteration,-`MF Plausibility (%)`,-Name) %>%
                  exportCSV(str_c(outPath,'/assignments.csv'))
          })

#' @rdname export
#' @export

setGeneric('exportSummarisedAssignments',function(x,outPath = '.')
    standardGeneric('exportSummarisedAssignments'))

#' @rdname export
#' @importFrom assignments summariseAssignments

setMethod('exportSummarisedAssignments',signature = 'Assignment',
          function(x,outPath = '.'){
              x %>% 
                  summariseAssignments() %>% 
                  exportCSV(str_c(outPath,'/summarised_assignments.csv'))
          })

#' @rdname export
#' @importFrom construction classifications
#' @export

setGeneric('exportConstruction',function(x,outPath = '.')
    standardGeneric('exportConstruction'))

#' @rdname export

setMethod('exportConstruction',signature = 'Construction',
          function(x,outPath = '.'){
              x %>% 
                  classifications() %>% 
                  exportCSV(str_c(outPath,'/consensus_structural_classifications.csv'))
          })

#' @rdname export
#' @export

setGeneric('exportSummarisedConstruction',function(x,outPath = '.')
    standardGeneric('exportSummarisedConstruction'))

#' @rdname export
#' @importFrom construction summariseClassifications

setMethod('exportSummarisedConstruction',signature = 'Construction',
          function(x,outPath = '.'){
              x %>% 
                  summariseClassifications() %>% 
                  exportCSV(str_c(outPath,'/summarised_consensus_structural_classifications.csv'))
          })

#' @rdname export
#' @export

setGeneric('export',function(x,outPath = '.',...)
    standardGeneric('export')
)

#' @rdname export

setMethod('export',signature = 'Binalysis',
          function(x,outPath = '.'){
              
              si_fp <- exportSampleInfo(x,outPath)
              ad_fp <- exportAccurateData(x,outPath)
              bd_fp <- exportData(x,outPath)
              
              return(c(si_fp,ad_fp,bd_fp))
          })

#' @rdname export
#' @importFrom profilePro peakInfo

setMethod('export',signature = 'MetaboProfile',
          function(x,outPath = '.'){
              
              si_fp <- exportSampleInfo(x,outPath)
              pi_fp <- exportPeakInfo(x,outPath)
              pd_fp <- exportData(x,outPath)
              
              return(c(si_fp,pi_fp,pd_fp))
          })

#' @rdname export
#' @importFrom dplyr bind_cols everything mutate select
#' @importFrom tidyr gather spread
#' @importFrom stringr str_sub str_split_fixed

setMethod('export',signature = 'Analysis',
          function(x,outPath = '.',type = 'raw',idx = 'name'){
              si_fp <- exportSampleInfo(x,outPath,type)
              ad_fp <- exportData(x,outPath,type,idx)
              m_fp <- exportModelling(x,outPath)
              co_fp <- exportCorrelations(x,outPath)
              
              c(si_fp,
                ad_fp,
                m_fp,
                co_fp) %>% 
                  return()
          })

#' @rdname export

setMethod('export',signature = 'Assignment',function(x,outPath = '.'){
    as_fp <- exportAssignments(x,outPath)
    ad_fp <- exportData(x,outPath)
    sa_fp <- exportSummarisedAssignments(x,outPath)
    
    return(c(as_fp,ad_fp,sa_fp))
})

#' @rdname export

setMethod('export',signature = 'Construction',function(x,outPath = '.'){
    cl_fp <- exportConstruction(x,outPath)
    scl_fp <- exportSummarisedConstruction(x,outPath)
    
    return(c(cl_fp,scl_fp))
})

#' @importFrom dplyr bind_rows
#' @importFrom purrr map

fixNames <- function(i){
    i %>%
        split(.$name) %>%
        map(~{
            if (nrow(.) > 1) {
                .$name <- str_c(.$name,'_',1:length(.$name))    
            }
            return(.)
        }) %>%
        bind_rows()
}
