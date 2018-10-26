#' preTreatModes
#' @export

filterCorrelations <- function(correlations, rthresh = 0.7, n = 100000){
    while (nrow(correlations) > n) {
        correlations <- correlations %>%
            filter(r > rthresh | r < -rthresh)
        rthresh <- rthresh + 0.01
    }
    return(correlations)
}
