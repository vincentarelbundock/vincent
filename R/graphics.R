#' Default theme for base R graphics
#' 
#' @export
set_base_theme <- function() {
    parameters <- list(
           fg              = "black",
           adj             = .5,
           ann             = TRUE,
           bg              = "white",
           bty             = "l",
           cex             = 1,
           cex.axis        = 0.8,
           cex.lab         = 1,
           cex.main        = 1.2,
           cex.sub         = 1,
           col             = "black",
           col.axis        = "black",
           col.lab         = "black",
           col.main        = "black",
           col.sub         = "black",
           family          = "serif",
           font            = 1,
           font.axis       = 1,
           font.lab        = 1,
           font.main       = 1,
           font.sub        = 1,
           lab             = c(4,4,7),
           las             = 1,
           lend            = 0,
           ljoin           = 0,
           lmitre          = 10,
           lty             = 1,
           lwd             = 1,
           mgp             = c(2.5,0.2,0),
           pch             = 20,
           tck             = 0,
           xaxs            = "r",
           xaxt            = "s",
           xpd             = FALSE,
           yaxs            = "r",
           yaxt            = "s"
    )

    set_par <- function(parameters) {
        function() {
            do.call(graphics::par, parameters)
        }
    }

    hook <- getHook("before.plot.new")
    hook$par <- set_par(parameters)
    setHook('before.plot.new', hook, 'replace')

}

#' Conditional formatting of axes for facets
#'
#' @inheritParams ggplot2::ggplot_add
#' @export
#' @keywords internal
ggplot_add.conditional_theme_vab <- function(object, plot, object_name) {
  if (!inherits(plot$facet, "FacetNull")) {
    plot + theme(panel.border = element_rect(colour = "grey50", fill = NA),
                 axis.line = element_blank(),
                 axis.ticks = element_line(colour = 'grey50'))
  } else {
    plot
  }
}

#' Default theme for ggplot2 graphics
#'
#' @export
theme_vab <- function() {
    out <- theme_bw(base_family = 'serif') +
           theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(), 
                 panel.border = element_blank(),
                 axis.line = element_line(),
                 legend.key = element_blank(),
                 legend.title = element_blank(),
                 strip.background = element_blank())
    class(out) <- c("conditional_theme_vab", class(out))
    return(out)
}

#' Set themes for base R graphics and ggplot2
#'
#' @param palette scico color scheme name (character)
#' @param begin numeric [0, 1]
#' @param end numeric [0, 1]
#' @param direction -1 or 1
#' @export
theming <- function(palette = 'batlow',
                    begin = 0,
                    end = 1,
                    direction = -1) {
    # base R
    set_base_theme()
    # Paul Tol's color schemes
    # https://personal.sron.nl/~pault/data/colourschemes.pdf
    # "muted" (w/ numbered hue ranks)
    pal <- c("#CC6677", # rose   7
             "#332288", # indigo 1
             "#DDCC77", # sand   6
             "#117733", # green  4
             "#88CCEE", # cyan   2
             "#882255", # wine   8
             "#44AA99", # teal   3
             "#999933", # olive  5
             "#AA4499", # purple 9
             "#DDDDDD") # grey   10) 
    palette(pal)

    # ggplot2 theme
    theme_set(theme_vab())

    # ggplot2 discrete colors
    options(ggplot2.discrete.fill = list(pal))
    options(ggplot2.discrete.color = list(pal))

    # ggplot2 continuous colors w/ `scico`
    scc <- function() scale_colour_scico(palette = palette,
                                         begin = begin,
                                         end = end,
                                         direction = direction)
    options(ggplot2.continuous.colour = scc)
    options(ggplot2.continuous.fill = scc)

}
