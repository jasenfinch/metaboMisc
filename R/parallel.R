#' Generate a suitable parallel processing plan
#' @description Generate a suitable parallel processing future plan for your session. See ?future::plan for more details on future plans?
#' @examples 
#' \dontrun{
#' suitableParallelPlan()
#' }
#' @importFrom future availableCores multisession multicore plan supportsMulticore
#' @export

suitableParallelPlan <- function(){
    workers <- ceiling(availableCores() * 0.75)
    
    if (supportsMulticore()) {
        strategy <- 'multicore'
    } else {
        strategy <- 'multisession'
    }
    
    message(paste('Setting up',strategy,'plan with',workers,'workers'))
    
    plan(get(strategy),workers = workers)
}
