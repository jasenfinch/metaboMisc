#' filterCorrelations
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
