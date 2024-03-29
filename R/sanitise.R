#' Sanitise a data table
#' @description  Sanitise a data table by restricting the number of rows or characters and rounding numeric columns.
#' @param x a tibble or data.frame containing the data to be sanitised
#' @param maxRows the maximum number of rows with which to restrict the table size
#' @param sigFig the significant figures with which to round numeric columns
#' @param maxCharacters the maximum number of characters allowed in a string before it is truncated
#' @return A sanitised tibble or data.frame.
#' @examples
#' sanitiseTable(iris,maxRows = 10,sigFig = 1)
#' @importFrom dplyr mutate_if
#' @importFrom purrr map_df
#' @export


sanitiseTable <- function(x,maxRows = 5000,sigFig = 3,maxCharacters = 100){
    x <- mutate_if(x,is.numeric,signif,digits = sigFig)
    
    if (nrow(x) > maxRows){
        message(str_c('Number of rows in table restricted to ',maxRows,'.'))
        x <- x[seq_len(maxRows),]
    }
    
    x <- map_df(x,~{
        if (typeof(.x) == 'character'){
            limit_characters <- .x %>% 
                nchar() %>% 
                {. > maxCharacters}
            
            .x[limit_characters] <- str_sub(.x[limit_characters],1,maxCharacters) %>% 
                str_c(.,'...')
        }
        
        return(.x)
    })
    
    return(x)
}
