#' @title Geom for for plotting a time line of earthquakes
#' with a point for each earthquake
#'
#' @description This function creates a new geom to plot a timeline
#' for a data range and points for each earthquake
#'
#' @param mapping Set of aesthetics mappings created by aes
#'
#' @param data The data to be displayed in this layer
#'
#' @param position Position adjustment
#'
#' @param stat not working (no stat functionality)
#'
#' @param ... Other arguments to layer
#'
#' @param na.rm If FALSE, the default, missing values are removed with a warning.
#'  If TRUE, missing values are silently removed.
#'
#' @param show.legend logical. Should this layer be included in the legends? NA,
#'  the default, includes if any aesthetics are mapped. FALSE never includes,
#'  and TRUE always includes. It can also be a named logical vector to finely
#'  select the aesthetics to display.
#'
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than
#'  combining with them.
#'
#' @examples
#' \dontrun{
#' dataset %>% filter((Year> 2010) &  (Country =="INDIA")) %>%
#'    ggplot(ggplot2::aes(x = Date,
#'             y = Country,
#'             color = Deaths,
#'             size = Mag)) + geom_timeline(alpha = 0.2)
#' }
#'
#' @export


geom_timeline <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          show.legend = NA,
                          na.rm = FALSE,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeLine,  # use the geom class built in the package
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' @title Class GeomTimeline
#'
#' @return no return value
#'
#' @description Construct a new class 'GeomTimeline' which is a 'ggproto' object
#' for plotting a time line of earthquakes.
#'
#' @import ggplot2
#'
#' @export

GeomTimeLine <- ggplot2::ggproto("GeomTimeLine",
                                 ggplot2::Geom,
                                 required_aes = c("x"),
                                 non_missing_aes = c("size", "shape", "colour", "y"),
                                 default_aes = ggplot2::aes(y = 0.5,
                                                            shape = 19,
                                                            colour = "grey",
                                                            size = 5,
                                                            fill = "grey20",
                                                            alpha = 0.2,
                                                            stroke = 0.5),
                                 draw_key = ggplot2::draw_key_point,

                                 draw_panel = function(data,
                                                       panel_params,
                                                       coord) {

                                      coords <- coord$transform(data,
                                                                panel_params)

                                      points <- grid::pointsGrob(coords$x,
                                                                 coords$y,
                                                    pch = coords$shape,
                                                    gp = grid::gpar(
                                                          col = alpha(coords$colour, coords$alpha),
                                                          fill = alpha(coords$fill, coords$alpha),
                                                          lwd = coords$stroke * .pt,
                                                          fontsize = coords$size * .pt)
                                                  )

                                   lines <- purrr::map(unique(coords$y), function(x)
                                     grid::linesGrob(y = c(x, x)))

                                   params <- c(list(points), lines)

                                   grid::gTree(children = do.call(grid::gList, params))
                                 }
)

#' @title Label the earthquakes on the timeline
#'
#' @description geom_timeline_label  labels the n biggest earthquakes
#'     (based on the Richter scale)
#'
#' @param mapping Set of aesthetics mappings created by ggplot2 aes
#'
#' @param data The data to be displayed in this layer
#'
#' @param position Position adjustment, either as a string, or the result of a
#'  call to position adjustment function
#'
#'@param stat not working (no stat functionality)
#'
#' @param ... Other arguments to layer
#'
#' @param na.rm If FALSE, the default, missing values are removed with a warning.
#'  If TRUE, missing values are silently removed
#'
#' @param show.legend logical. Should this layer be included in the legends? NA,
#'  the default, includes if any aesthetics are mapped. FALSE never includes,
#'  and TRUE always includes. It can also be a named logical vector to finely
#'  select the aesthetics to display
#'
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than
#'  combining with them
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' dataset %>% filter(Country %in% c("MEXICO", "IRAN") &
#'      Date %within% lubridate::interval(ymd(20000103), ymd(20180104))) %>%
#'      mutate(Country = factor(Country, levels = unique(Country))) %>%
#'      ggplot() +
#'      geom_timeline(ggplot2::aes(x = Date, y = Country, size = Mag, colour = Deaths)) +
#'      geom_timeline_label(aes(x = Date, y = Country, label = Location,
#'                          magnitude = Mag, colour = Deaths, n_max = 7), alpha = 0.5) +
#'      scale_colour_continuous(name = "Nb. of deaths") +
#'      theme(legend.position = "bottom") +
#'      ylab("")
#' }
#'
#' @export

geom_timeline_label <- function(mapping = NULL,
                                data = NULL,
                                stat = "identity",
                                position = "identity",
                                show.legend = NA,
                                na.rm = FALSE,
                                inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimeLineLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @title GeomTimelineLabel
#'
#' @description This is the setup required for creating a new geom class.
#'     This GeomTimeLineLabel inherits from a top level class called Geom.
#'     It creates a geom that draws the timeline for a specified date interval
#'     and put points on it for each earthquake. It then creates labels in
#'     order to annotate the biggest earthquakes on the timeline.
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
GeomTimeLineLabel <- ggplot2::ggproto("GeomTimeLineLabel",
                                      ggplot2::Geom,
                                      required_aes = c("x", "label", "magnitude"),
                                      non_missing_aes = c("size", "shape", "colour", "y", "n_max"),
                                      default_aes = ggplot2::aes(y = 0.5, shape = 19,
                                                                 colour = "grey",
                                                                 size = 3, fill = "grey20", alpha = 0.5,
                                                                 stroke = 0, n_max = 5),
                                      draw_key = ggplot2::draw_key_point,

                                      draw_panel = function(data, panel_scales, coord) {

                                        n_max <- data$n_max[1]

                                        data <- data %>%
                                          dplyr::group_by(group) %>%
                                          dplyr::slice_max(magnitude, n = n_max)

                                        # transform the data
                                        coords <- coord$transform(data, panel_scales)

                                        # Draw the points
                                        points <- grid::pointsGrob(
                                          coords$x, coords$y,
                                          pch = coords$shape,
                                          gp = grid::gpar(
                                            col = alpha(coords$colour, coords$alpha),
                                            fill = alpha(coords$fill, coords$alpha),
                                            lwd = coords$size * .pt,
                                            fontsize = coords$size * .pt
                                          )
                                        )

                                        # adding the text to the vertical lines over the timeline
                                        labels <- grid::textGrob(
                                          label = coords$label,
                                          x = unit(coords$x, "npc"),
                                          y = unit(coords$y + 0.16/length(unique(coords$y)), "npc"),
                                          just = "left",
                                          rot = 60,
                                          gp = grid::gpar(lwd = coords$size * .pt, fontsize=10 )
                                        )

                                        # creating the vertical lines over the timeline
                                        seg_label <- grid::segmentsGrob(
                                          x0 = grid::unit(coords$x, "npc"),
                                          y0 = grid::unit(coords$y, "npc"),
                                          x1 = grid::unit(coords$x, "npc"),
                                          y1 = grid::unit(coords$y + 0.12/length(unique(coords$y)), "npc"),
                                          default.units = "npc",
                                          arrow = NULL,
                                          name = NULL,
                                          gp = grid::gpar(),
                                          vp = NULL
                                        )

                                        lines <- purrr::map(unique(coords$y), function(x)
                                          grid::linesGrob(y = c(x, x)))

                                        params <- c(list(points), lines, list(labels),
                                                    list(seg_label))

                                        # use do.call to apply a function
                                        # to a variable number of parameters
                                        grid::gTree(children = do.call(grid::gList, params))
                                      }
)
