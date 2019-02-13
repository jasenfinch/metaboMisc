#' theme_neat
#' @description Neat theme for ggplot2
#' @param base_size base font size
#' @param base_family base font family
#' @importFrom ggplot2 theme_bw theme element_text
#' @export

theme_neat <- function(base_size = 12, base_family = 'Ubunbu'){
    theme_bw(base_size = base_size,base_family = base_family) +
        theme(
            legend.position = 'bottom',
            plot.title = element_text(face = 'bold'),
            axis.title = element_text(face = 'bold'),
            legend.title = element_text(face = 'bold')
        )
}