#' Export results
#' @rdname export
#' @description Export data tables from analysis results.
#' @param analysis S4 object of class Binalysis, Analysis, Assignment,
#' @param outPath directory path to export to.
#' @importFrom binneR info binnedData
#' @importFrom readr write_csv
#' @importFrom purrr walk
#' @importFrom stringr str_remove_all
#' @export

setMethod('export',signature = 'Binalysis',function(analysis,outPath = '.'){
    exportPath <- str_c(outPath,'/exports')
    
    message('Exporting binned data')
    
    if (!dir.exists(exportPath)) {
        dir.create(exportPath)
    }
    
    i <- analysis %>%
        info()
    
    if (T %in% duplicated(i$name)) {
        i <- fixNames(i)
    }
    
    write_csv(i,str_c(exportPath,'/','raw_sample_info.csv'))
    
    bd <- analysis %>%
        binnedData() 
    
    names(bd)[names(bd) == 'p'] <- 'positive'
    names(bd)[names(bd) == 'n'] <- 'negative'
    
    bd %>%
        names() %>%
        walk(~{
            m <- .
            bind_cols(bd[[m]],i %>% select(name)) %>%
                gather('m/z','Intensity',-name) %>%
                spread(name,Intensity) %>%
                mutate(`m/z` = str_remove_all(`m/z`, '[:alpha:]') %>% as.numeric()) %>%
                write_csv(str_c(exportPath,'/',m,'_mode_raw_data.csv'))
        })
})

#' @rdname export
#' @importFrom dplyr bind_cols everything mutate select
#' @importFrom tidyr gather spread
#' @importFrom stringr str_sub str_split_fixed
#' @export

setMethod('export',signature = 'Analysis',function(analysis,outPath = '.'){
    exportPath <- str_c(outPath,'/exports')
    
    message('Exporting pre-treated data')
    
    if (!dir.exists(exportPath)) {
        dir.create(exportPath)
    }
    
    i <- analysis %>%
        preTreatedInfo()
    
    if (T %in% duplicated(i$name)) {
        i <- fixNames(i)
    }
    
    write_csv(i,str_c(exportPath,'/pre-treated_sample_info.csv'))
    
    analysis %>%
        preTreatedData() %>%
        bind_cols(i %>% select(name)) %>%
        gather('m/z','Intensity',-name) %>%
        spread(name,Intensity) %>%
        mutate(Mode = str_sub(`m/z`,1,1)) %>%
        mutate(`m/z` = str_split_fixed(`m/z`,' ',2)[,1] %>%
                   str_remove_all('[:alpha:]') %>%
                   as.numeric()) %>%
        select(Mode,everything()) %>%
        write_csv(str_c(exportPath,'/pre-treated_data.csv'))
    
})

#' @rdname export
#' @importFrom MFassign assignments summariseAssignment
#' @export

setMethod('export',signature = 'Assignment',function(analysis,outPath = '.'){
    
    message('Exporting molecular formula assignments')
    
    exportPath <- str_c(outPath,'/exports')
    
    if (!dir.exists(exportPath)) {
        dir.create(exportPath)
    }
    
    analysis %>%
        assignments() %>%
        select(-Iteration,-Score,-Name) %>%
        write_csv(str_c(exportPath,'/','molecular_formula_assignments.csv'))
    
    analysis %>%
        summariseAssignment() %>%
        write_csv(str_c(exportPath,'/','summarised_molecular_formula_assignments.csv'))
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
