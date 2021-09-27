#' Sanitise a data table
#' @description  Sanitise a data table by restricting the number of rows and rounding numeric columns.
#' @param x A tibble or data.frame containing the data to be sanitised
#' @param maxRows Maximum number of rows with which to restrict the table
#' @param sigFig Significant figures with which to round numeric columns
#' @examples
#' sanitiseTable(iris,maxRows = 10,sigFig = 1)
#' @importFrom dplyr mutate_if
#' @export


sanitiseTable <- function(x,maxRows = 5000,sigFig = 3){
    x <- mutate_if(x,is.numeric,signif,digits = sigFig)
    
    if (nrow(x) > maxRows){
        message(str_c('Number of rows in table restricted to ',maxRows,'.'))
        x <- x[seq_len(maxRows),]
    }
    
    return(x)
}
