#'
#' Builds the main function for styling ggplot2 plots in the hotteR style
#' Import the correct colours as hex codes to be used
#'
#' @author Trent Henderson
#'

hotteR_palette <- c(
  "#fa448c",
  "#fec859",
  "#43b5a0",
  "#491d88",
  "#331a38"
)

#' hotteR colour and fill scales
#'
#' @seealso [ggplot2::scale_colour_discrete] [ggplot2:scale_fill_discrete]
#' @inheritDotParams ggplot2::discrete_scale
#' @name hotteR_pal
#'
#' @return the hotteR palette
#' @export
#'

hotteR_pal <- function(){
  scales::manual_pal(hotteR_palette)
}

#' @rdname hotteR_pal
#' @export
scale_colour_hotteR <- function(...) {
  ggplot2::discrete_scale("colour", "hotteR", hotteR_pal(), ...)
}

#' @rdname hotteR_pal
#' @export
scale_color_hotteR <- scale_colour_hotteR


#' @rdname hotteR_pal
#' @export
scale_fill_hotteR <- function(...) {
  ggplot2::discrete_scale('fill', 'hotteR', hotteR_pal(), ...)
}

#' Style general plot features according to the hotteR theme
#'
#' @rdname theme_hotteR
#' @export

theme_hotteR <- function(grids = FALSE){
  the_theme <- ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom",
                   axis.text = ggplot2::element_text(colour = "#331a38"),
                   axis.title = ggplot2::element_text(colour = "#331a38", face = "bold"),
                   panel.border = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "#331a38"),
                   panel.background = ggplot2::element_rect(fill = "#E1E6E6", colour = "#E1E6E6"),
                   plot.background = ggplot2::element_rect(fill = "#E1E6E6", colour = "#E1E6E6"),
                   legend.background = ggplot2::element_rect(fill = "#E1E6E6", colour = "#E1E6E6"),
                   legend.box.background = ggplot2::element_rect(fill = "#E1E6E6", colour = "#E1E6E6"),
                   legend.key = ggplot2::element_rect(fill = "#E1E6E6", colour = "#E1E6E6"),
                   legend.text = ggplot2::element_text(colour = "#331a38"),
                   legend.title = ggplot2::element_text(colour = "#331a38"),
                   plot.title = ggplot2::element_text(colour = "#331a38", face = "bold"),
                   plot.subtitle = ggplot2::element_text(colour = "#331a38"),
                   plot.caption = ggplot2::element_text(colour = "#331a38"),
                   strip.background = ggplot2::element_rect(fill = "white", colour = "white"),
                   strip.text = ggplot2::element_text(colour = "#331a38"))
  if(grids){
    the_theme <- the_theme +
      ggplot2::theme(panel.grid.minor.x = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     panel.grid.major = ggplot2::element_line(colour = "white"))
  }
  return(the_theme)
}
