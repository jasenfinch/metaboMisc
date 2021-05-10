#' Convert sample information to a workflow compatible format
#' @description Convert sample information from `grover::runInfo` to a format compatible with `binneR` and `profilePro`.
#' @param sample_info tibble containing sample information as returned from `grover::runInfo`
#' @param gzip_ext add `.gz` extension to file names
#' @return A tibble containing converted sample information.
#' @importFrom stringr str_replace_all
#' @importFrom dplyr rename arrange
#' @importFrom tools file_path_sans_ext
#' @export

convertSampleInfo <- function(sample_info,gzip_ext = TRUE){
    sample_info <- sample_info %>%
        select(`RAW file`,
               `Creation date`,
               `User text 0`:`User text 4`) %>%
        rename(fileName = `RAW file`,
               batch = `User text 0`,
               block = `User text 1`,
               class = `User text 2`,
               rawFileOrder = `User text 3`,
               sampleOrder = `User text 4`) %>%
        arrange(fileName) %>%
        mutate(fileOrder = seq_len(nrow(.)),
               fileName = str_replace_all(fileName,'.raw','.mzML'),
               name = tools::file_path_sans_ext(fileName)) %>%
        arrange(`Creation date`) %>%
        mutate(injOrder = seq_len(nrow(.))) %>%
        arrange(fileOrder) %>%
        select(fileOrder,
               injOrder,
               rawFileOrder,
               sampleOrder,
               fileName,
               batch,
               block,
               name,
               class)
    
    if (isTRUE(gzip_ext)) {
        sample_info <- sample_info %>% 
           mutate(fileName = str_c(fileName,'.gz')) 
    }
    
    return(sample_info)
}