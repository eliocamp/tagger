#' Adds tags to facets
#'
#' @param tag Character indicating the element to tag. Possible values are
#' * "panel": each single panels is individually tag.
#' * "rc": facets are tagged by row and then column.
#' * "cr": factes are tagged by columns and then row.
#'
#' @param position Character indicating the position of the tag. Options are "tl"
#'  (top-left), "tr" (top-right), "br" (bottom-right) and "bl" (bottom-left).
#'  Alternatively, for finer control, it can be a list of elements "x", "u",
#'  "hjust" and "vjust" that define the position of the tag within each panel.
#'
#' @inheritParams patchwork::plot_annotation
#'
#' @param tag_pool An optional character vector of user-defined "pool" of tags.
#' If not `NULL`, it will be used instead of `tag_levels`. Should be at least
#' as long as the total number of panels.
#'
#'
#' @details
#'
#' Tags inherit aesthetic properties (size, font, colour, etc...) from strip.text and
#' strip.background defined by [ggplot2::theme()]. For fine-grained detail, these can be
#' overriden by setting tagger.panel.tag.text and tagger.panel.tag.background.
#'
#' @examples
#'
#' library(ggplot2)
#' # Base plot
#' g <- ggplot(mtcars, aes(hp, mpg)) +
#'   geom_point() +
#'   facet_grid(cyl ~ vs)
#'
#' g + tag_facets()
#' g + tag_facets("rc")
#' g + tag_facets(position = "br")
#' g + tag_facets("rc", tag_levels  = c("A", "I"))
#'
#' # You can get finer control over position.
#' g + tag_facets(position = list(x = 0.5, y = 0.5))
#'
#' g + tag_facets(tag_pool = c("one", "two", "three", "four", "five", "six"), tag_suffix = "")
#' # Thanks to theme inheritance, tags should look aceptable
#' # out of the box in any theme.
#' g + tag_facets() +
#'    theme_dark()
#'
#' # But you can control their appearance and create your own atrocities
#' g + tag_facets() +
#'    theme_dark() +
#'    theme(tagger.panel.tag.text = element_text(color = "red", size = 16),
#'          tagger.panel.tag.background = element_rect(fill = "purple"))
#'
#' @export
#' @import checkmate
tag_facets <- function(tag = c("panel", "rc", "cr"), position = "tl",
                       tag_levels = c("a", "1"), tag_pool = NULL, tag_prefix = "",
                       tag_suffix = ")", tag_sep = ".") {

   if (!is.list(position)) {
      assert_choice(position, c("tl", "tr", "bl", "br"))

      position <- switch(position,
                         tl = list(x = 0, y = 1, hjust = 0, vjust = 1),
                         tr = list(x = 1, y = 1, hjust = 1, vjust = 1),
                         bl = list(x = 0, y = 0, hjust = 0, vjust = 0),
                         br = list(x = 1, y = 0, hjust = 1, vjust = 0),
                         position)
   }

   position$hjust <- if (!is.null(position$hjust)) position$hjust else 0.5
   position$vjust <- if (!is.null(position$vjust)) position$vjust else 0.5

   checks <- makeAssertCollection()


   assert_list(position, types = "numeric", len = 4, add = checks)
   assert_names(names(position), must.include = c("x", "y", "hjust", "vjust"),
                .var.name = "position", add = checks)

   assert_choice(tag[1], c("panel", "rc", "cr"), add = checks)
   assert_character(tag_prefix, any.missing = FALSE, len = 1, add = checks)
   assert_character(tag_suffix, any.missing = FALSE, len = 1, add = checks)
   assert_character(tag_sep, any.missing = FALSE, len = 1, add = checks)

   tag <- switch(tag[1],
                 rc = c("ROW", "COL"),
                 cr = c("COL", "ROW"),
                 panel = "PANEL")

   assert_character(tag_levels, any.missing = FALSE, min.len = length(tag), add = checks)
   reportAssertions(checks)


   structure(list(tag = tag, tag_pool = tag_pool, tag_levels = tag_levels, open = tag_prefix,
                  close = tag_suffix, tag_sep = tag_sep, position = position),
             class = "tagger")
}

#' @export
#' @importFrom ggplot2 ggplot_add
ggplot_add.tagger <- function(object, plot, object_name) {
   class(plot) <- c("ggtagged", class(plot))
   plot$tag_options <- object
   plot
}

`%||%` <- function(a, b) {
   if (length(a) == 0) return(a)
   nulls <- lengths(a) == 0
   if (all(nulls[-length(nulls)])) return(b)
   a[nulls] <- b[nulls]
   a
}

as.element_text <- function(x) {
   if (length(x) == 0) return(ggplot2::element_text())
   return(x)
}


as.element_rect <- function(x) {
   if (length(x) == 0) return(ggplot2::element_rect())
   return(x)
}


#' @export
#' @importFrom ggplot2 ggplot_build
ggplot_build.ggtagged <- function(plot) {
   gb <- NextMethod("ggplot_build")
   gb <- add_class(gb, "ggplot_build_ggtagged")
   gb
}

add_class <- function(object, class) {
   class(object) <- c(class, setdiff(class(object), class))
   return(object)
}


#' @export
#' @importFrom ggplot2 ggplot_gtable
ggplot_gtable.ggplot_build_ggtagged <- function(data) {
   gt <- NextMethod("ggplot_gtable")

   lay <- asign_tags(data)
   facet_tags <- lay$PANEL

   tag_options <- data$plot$tag_options

   theme <- ggplot2:::plot_theme(data$plot)

   tag_style <- ggplot2::calc_element("tagger.panel.tag.text", theme, verbose = FALSE, skip_blank = FALSE)
   tag_gpar <- list(col = tag_style$colour,
                    family = tag_style$family,
                    fontface = tag_style$face,
                    fontsize = tag_style$size,
                    lineheight = tag_style$lineheight)
   tag_gpar <- tag_gpar[lengths(tag_gpar) != 0]
   tag_gpar <- do.call(grid::gpar, tag_gpar)

   box <- ggplot2::calc_element("tagger.panel.tag.background", theme, verbose = FALSE, skip_blank = FALSE)
   box_gp <-  grid::gpar(col = box$colour, fill = box$fill, lty = box$linetype, lwd = box$size)

   x <-  tag_options$position$x
   y <-  tag_options$position$y

   hjust <- tag_options$position$hjust
   vjust <- tag_options$position$vjust
   panels <- which(grepl("panel", gt$layout$name))

   grob_args <- list(x = x, y = y, hjust = hjust, vjust = vjust,
                     rot = tag_style$angle,
                     gp = tag_gpar,
                     box_gp = box_gp,
                     # r = grid::unit(c(0.1, .1, .1, .1), "lines"),
                     padding = grid::unit(c(0.2, .2, .2, .2), "lines"),
                     use_markdown = FALSE)
   grob_args <- grob_args[lengths(grob_args) != 0]
   grob <- gridtext::richtext_grob

   for (p in seq_along(facet_tags)) {
      grob_args$text <- facet_tags[p]
      tagGrob <- do.call(grob, grob_args)
      this_panel <- gt$layout[panels[p], ]
      gt <- gtable::gtable_add_grob(gt, tagGrob, t = this_panel$t, l = this_panel$l, clip = "on", z = 2)
   }

   return(gt)
}


asign_tags <- function(plot) {

   tag_options <- plot$plot$tag_options

   lay <- plot$layout$layout
   lay <- lay[order(lay$COL, lay$ROW), ]
   facet_vars <- lay[toupper(tag_options$tag)]
   facet_vars <- lapply(facet_vars, function(x) as.numeric(as.factor(x)))
   tag_options$tag_levels <- rep_len(tag_options$tag_levels, length.out = length(facet_vars))
   facet_tags <- lapply(seq_along(facet_vars), function(i) {
      i_panels <- facet_vars[[i]]
      if (is.null(tag_options$tag_pool[[i]])) {
         tag_pool <- switch(tag_options$tag_levels[i],
                            a = letters[i_panels],
                            A = LETTERS[i_panels],
                            "1" = i_panels,
                            "i" = tolower(utils::as.roman(i_panels)),
                            "I" = utils::as.roman(i_panels),
                            stop("tag_levels is not valid")
         )

      } else {
         tag_pool <- tag_options$tag_pool[i_panels]
      }
      return(tag_pool)
   })

   facet_tags <- Reduce(function(a, b) paste(a, b, sep = tag_options$tag_sep), facet_tags)
   facet_tags <- paste0(tag_options$open, facet_tags, tag_options$close)

   lay[["PANEL"]] <- facet_tags
   return(lay)
}

#' Get labels asigned to each panel of a tagged plot
#'
#' @param filter an expression that, evaluated within the data used
#' to generate the facets of the plot, evaluates to a logical vector
#' or a sequence of rows.
#' @param plot a plot object
#'
#' @return
#' A data.frame with columns `PANEL`, containing the tag of each panel,
#' `ROW`, `COL` and the variables used to create the facets.
#'
#' @examples
#' library(ggplot2)
#' g <- ggplot(mtcars, aes(hp, mpg)) +
#'   geom_point() +
#'   facet_grid(cyl ~ vs) +
#'   tag_facets()
#'
#' get_tags()
#'
#' get_tags(cyl == 4 & vs == 0)
#'
#' # Use it with inline markdown to refer always to the correct panel:
#' # "As you can see in panel `r get_tags(cyl == 4 & vs == 0)$PANEL` ...
#'
#' @export
get_tags <- function(filter = TRUE, plot = ggplot2::last_plot()) {
   if (!inherits(plot, "ggtagged")) {
      stop("plot has no tags")
   }

   plot <- ggplot2::ggplot_build(plot)
   lay <- asign_tags(plot)
   lay <- lay[, setdiff(colnames(lay), c("SCALE_X", "SCALE_Y"))]

   lay[eval(substitute(filter), envir  = lay), ]
}
