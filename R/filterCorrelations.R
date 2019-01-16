#' filterCorrelations
#' @description filter correlation tables to give best comprimise between coefficient and number
#' @param correlations correlation table as returned by the correlations method in metabolyseR
#' @param rthresh correlation coefficient threshold
#' @param n correlation number threshold
#' @param rIncrement correlation coefficient increase increment
#' @param nIncrement correlation number increase increment if all correlations above rthresh
#' @export

filterCorrelations <- function(correlations, rthresh = 0.7, n = 100000, rIncrement = 0.01, nIncrement = 20000){
    filCors <- function(cors,rthresh,n){
        while (nrow(cors) > n) {
            cat('rthresh = ',rthresh,'\n')
            cors <- correlations %>%
                filter(r > rthresh | r < -rthresh)
            rthresh <- rthresh + rIncrement
        }
        return(cors)
    }
    
    while (TRUE) {
        cat('n = ',n,'\n')
        cors <- filCors(correlations,rthresh,n)
        if (nrow(cors) > 0) {
            break()
        } else {
            n <- n + nIncrement
        }
    }
    return(cors)
}
