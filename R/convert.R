#' Convert .raw file header information
#' @description Convert .raw file header information from either `grover::sampleInfo()`, 
#' `grover::runInfo()` or `rawrrr::readFileHeader()` into a sample information format compatible 
#' with [`binneR`](https://github.com/aberHRML/binneR) or [`profilePro`](https://github.com/jasenfinch/profilePro).
#' @param sample_info a tibble containing sample information as returned from from either `grover::sampleInfo()`, 
#' `grover::runInfo()` or `rawrrr::readFileHeader()`
#' @param gzip_ext add a `.gz` extension to the returned mzML file names
#' @param user_text the column names for the five `User text` columns
#' @return A tibble containing converted sample information.
#' @examples 
#' ## Read the file header information from an example .raw file in the 
#' ## grover package available from <https://jasenfinch.github.io/grover/>
#' sample_information <- system.file(
#'     'repository/Thermo-Exactive/Experiment_1/QC01.raw',
#'     package = 'grover') %>% 
#'     rawrr::readFileHeader() %>% 
#'     tibble::as_tibble()
#' 
#' ## Convert the header information
#' converted_sample_info <- convertSampleInfo(
#'     sample_information,
#'     user_text = c('batch','block','name','class','rawFileOrder'))
#' 
#' converted_sample_info
#' @importFrom stringr str_replace_all
#' @importFrom dplyr rename arrange rename_with contains pull
#' @importFrom tools file_path_sans_ext
#' @export

convertSampleInfo <- function(sample_info,
                              gzip_ext = TRUE,
                              user_text = c('batch','block','class','rawFileOrder','sampleOrder')){
    
    new_columns <- tibble(
        column_name = paste('User text',c(0,seq_len(4))),
        new_name = user_text
    )
    
    sample_info <- sample_info %>%
        select(`RAW file`,
               `Creation date`,
               `User text 0`:`User text 4`) %>%
        rowid_to_column(var = 'fileOrder') %>% 
        rename(fileName = `RAW file`) %>% 
        rename_with(
            .fn = ~filter(new_columns,
                               column_name == .x) %>% 
                pull(new_name),
            .cols = contains('User')) %>% 
        mutate(fileName = str_replace_all(fileName,'.raw','.mzML'),
               name = tools::file_path_sans_ext(fileName)) %>%
        arrange(`Creation date`) %>%
        mutate(injOrder = seq_len(nrow(.))) %>%
        arrange(fileOrder) %>%
        select(fileOrder,
               injOrder,
               fileName,
               all_of(user_text))
    
    if (isTRUE(gzip_ext)) {
        sample_info <- sample_info %>% 
           mutate(fileName = str_c(fileName,'.gz')) 
    }
    
    return(sample_info)
}