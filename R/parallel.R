#' Generate a suitable parallel processing plan
#' @description Generate a suitable parallel processing future plan for your session. See `?future::plan` for more details on future plans.
#' @param strategy Parallel strategy to use. `?future::plan` for details. If `NULL` (the default), an appropriate strategy will be detected.
#' @param workers Number of workers to use. If `NULL`, the selected number will be two thirds of the available system workers.
#' @examples 
#' \dontrun{
#' suitableParallelPlan()
#' }
#' @importFrom future availableCores multisession multicore plan supportsMulticore
#' @export

suitableParallelPlan <- function(strategy = NULL,workers = NULL){
    
    if (is.null(strategy)) strategy <- ifelse(
        supportsMulticore(),
        'multicore',
        'multisession')
    
    if (is.null(workers)) workers <- ceiling(availableCores() * 0.75)
    
    message(paste('Setting up',strategy,'plan with',workers,'workers'))
    
    plan(get(strategy),workers = workers)
}
