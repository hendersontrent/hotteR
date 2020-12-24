#'
#' Sets default geom colours to be compliant with The Runoff branding
#' @importFrom ggplot2 update_geom_defaults theme_set
#'

.onAttach <- function(libname, pkgname) {

  ggplot2::update_geom_defaults("bar", list(fill = "#fa448c"))
  ggplot2::update_geom_defaults("col", list(fill = "#fa448c"))
  ggplot2::update_geom_defaults("point", list(colour = "#fa448c"))
  ggplot2::update_geom_defaults("line", list(colour = "#fa448c"))
  ggplot2::update_geom_defaults("path", list(colour = "#fa448c"))
  ggplot2::update_geom_defaults("smooth", list(colour = "#fa448c"))
  ggplot2::update_geom_defaults("boxplot", list(colour = "#fa448c"))
  ggplot2::update_geom_defaults("segment", list(colour = "#fa448c"))
  ggplot2::update_geom_defaults("abline", list(colour = "#fa448c"))
  ggplot2::update_geom_defaults("hline", list(colour = "#fa448c"))
  ggplot2::update_geom_defaults("vline", list(colour = "#fa448c"))
  ggplot2::update_geom_defaults("rug", list(colour = "#fa448c"))
  ggplot2::update_geom_defaults("density", list(colour = "#fa448c", fill = "#fa448c", alpha = 0.5))
  ggplot2::update_geom_defaults("pointrange", list(colour = "#491d88"))
  ggplot2::update_geom_defaults("linerange", list(colour = "#491d88"))
  ggplot2::update_geom_defaults("errorbar", list(colour = "#491d88"))
  ggplot2::update_geom_defaults("crossbar", list(colour = "#491d88"))
  ggplot2::update_geom_defaults("text", list(colour = "#331a38"))
  ggplot2::update_geom_defaults("label", list(colour = "#331a38"))

  ggplot2::theme_set(theme_hotteR())

  invisible()

}
