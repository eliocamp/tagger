#' Adds tags to facets
#'
#' @param tag Character indicating the element to tag. Possible values are
#' * "panel": each single panels is individually tag.
#' * "rc": facets are tagged by row and then column.
#' * "cr": facets are tagged by columns and then row.
#'
#' @param position Character indicating the position of the tag. Options are "tl"
#'  (top-left), "tr" (top-right), "br" (bottom-right) and "bl" (bottom-left).
#'  Alternatively, for finer control, it can be a list of elements "x", "y",
#'  "hjust",  "vjust" and "clip", that define the position of the tag within
#'  each panel and whether clipping is "on" or "off".
#'
#' @inheritParams patchwork::plot_annotation
#'
#' @param tag_pool An optional character vector of user-defined "pool" of tags.
#' If not `NULL`, it will be used instead of `tag_levels`. Should be at least
#' as long as the total number of panels.
#'
#' @details
#'
#' Tags inherit aesthetic properties (size, font, colour, etc...) from strip.text and
#' strip.background defined by [ggplot2::theme()]. For fine-grained detail, these can be
#' overridden by setting tagger.panel.tag.text and tagger.panel.tag.background.
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
#' g + tag_facets("cr")
#' g + tag_facets(position = "br")
#' g + tag_facets("rc", tag_levels  = c("A", "I"))
#'
#' # You can get finer control over position.
#' g + tag_facets(position = list(x = 0.5, y = 0.5))
#'
#' # Draw tags inside the panel strip with clip = "off" and custom position.
#' g +
#'   facet_wrap(cyl ~ vs) +
#'   tag_facets(position = list(x = 0, y = 1, vjust = 0, hjust = 0, clip = "off"))
#'
#' g + tag_facets(tag_pool = c("one", "two", "three", "four", "five", "six"), tag_suffix = "")
#' # Thanks to theme inheritance, tags should look acceptable
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
tag_facets <- function(
  tag = c("panel", "rc", "cr"),
  position = "tl",
  tag_levels = c("a", "1"),
  tag_pool = NULL,
  tag_prefix = "",
  tag_suffix = ")",
  tag_sep = "."
) {
  if (!is.list(position)) {
    assert_choice(position, c("tl", "tr", "bl", "br"))

    position <- switch(
      position,
      tl = list(x = 0, y = 1, hjust = 0, vjust = 1, clip = "on"),
      tr = list(x = 1, y = 1, hjust = 1, vjust = 1, clip = "on"),
      bl = list(x = 0, y = 0, hjust = 0, vjust = 0, clip = "on"),
      br = list(x = 1, y = 0, hjust = 1, vjust = 0, clip = "on"),
      position
    )
  }

  position$hjust <- if (!is.null(position$hjust)) position$hjust else 0.5
  position$vjust <- if (!is.null(position$vjust)) position$vjust else 0.5
  position$clip <- if (!is.null(position$clip)) position$clip else "on"

  checks <- makeAssertCollection()

  assert_list(
    position,
    types = c("numeric", "character"),
    len = 5,
    add = checks
  )
  assert_choice(position$clip, c("on", "off"), add = checks)
  assert_names(
    names(position),
    must.include = c("x", "y", "hjust", "vjust"),
    .var.name = "position",
    add = checks
  )

  assert_choice(tag[1], c("panel", "rc", "cr"), add = checks)
  assert_character(tag_prefix, any.missing = FALSE, len = 1, add = checks)
  assert_character(tag_suffix, any.missing = FALSE, len = 1, add = checks)
  assert_character(tag_sep, any.missing = FALSE, len = 1, add = checks)

  tag <- switch(
    tag[1],
    rc = c("ROW", "COL"),
    cr = c("COL", "ROW"),
    panel = "PANEL"
  )

  assert_character(
    tag_levels,
    any.missing = FALSE,
    min.len = length(tag),
    add = checks
  )
  reportAssertions(checks)

  structure(
    list(
      tag = tag,
      tag_pool = tag_pool,
      tag_levels = tag_levels,
      open = tag_prefix,
      close = tag_suffix,
      tag_sep = tag_sep,
      position = position
    ),
    class = "tagger"
  )
}

#' @export
#' @importFrom ggplot2 ggplot_add
ggplot_add.tagger <- function(object, plot, ...) {
  class(plot) <- c("ggtagged", class(plot))
  plot$tag_options <- object
  plot
}

#' @export
#' @importFrom ggplot2 ggplot_build
ggplot_build.ggtagged <- function(plot, ...) {
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
  lay <- assign_tags(data)
  facet_tags <- lay$TAG_PANEL

  tag_options <- data$plot$tag_options

  facet_tags <- paste0(tag_options$open, facet_tags, tag_options$close)

  theme <- ggplot2:::plot_theme(data$plot)

  tag_style <- ggplot2::calc_element(
    "tagger.panel.tag.text",
    theme,
    verbose = FALSE,
    skip_blank = FALSE
  )
  tag_gpar <- list(
    col = tag_style$colour,
    family = tag_style$family,
    fontface = tag_style$face,
    fontsize = tag_style$size,
    lineheight = tag_style$lineheight
  )
  tag_gpar <- tag_gpar[lengths(tag_gpar) != 0]
  tag_gpar <- do.call(grid::gpar, tag_gpar)

  box <- ggplot2::calc_element(
    "tagger.panel.tag.background",
    theme,
    verbose = FALSE,
    skip_blank = FALSE
  )
  box_gp <- grid::gpar(
    col = box$colour,
    fill = box$fill,
    lty = box$linetype,
    lwd = box$size
  )

  x <- tag_options$position$x
  y <- tag_options$position$y

  hjust <- tag_options$position$hjust
  vjust <- tag_options$position$vjust

  panels <- which(startsWith(gt$layout$name, "panel"))

  # Panels are sorted top to bottom, left to right
  if (!is.null(lay$ROW) && !is.null(lay$COL)) {
    panels <- c(matrix(
      panels,
      ncol = max(lay$ROW),
      nrow = max(lay$COL),
      byrow = TRUE
    ))
  }

  # Remove empty panels
  empty <- vapply(
    panels,
    function(p) inherits(gt$grobs[p][[1]], "zeroGrob"),
    FUN.VALUE = logical(1)
  )
  panels <- panels[!empty]

  grob_args <- list(
    x = x,
    y = y,
    hjust = hjust,
    vjust = vjust,
    rot = tag_style$angle,
    gp = tag_gpar,
    box_gp = box_gp,
    # r = grid::unit(c(0.1, .1, .1, .1), "lines"),
    padding = grid::unit(c(0.2, .2, .2, .2), "lines"),
    use_markdown = FALSE
  )
  grob_args <- grob_args[lengths(grob_args) != 0]
  grob <- gridtext::richtext_grob

  for (p in seq_along(facet_tags)) {
    grob_args$text <- facet_tags[p]
    tagGrob <- do.call(grob, grob_args)
    this_panel <- gt$layout[panels[p], ]
    gt <- gtable::gtable_add_grob(
      gt,
      tagGrob,
      t = this_panel$t,
      l = this_panel$l,
      clip = tag_options$position$clip,
      z = 2
    )
  }

  return(gt)
}


assign_tags <- function(plot) {
  tag_options <- plot$plot$tag_options

  lay <- plot$layout$layout

  # ggh4x::facet_manual doesn't have ROW and COL.
  row_column <- any(c("COL", "ROW") %in% toupper(tag_options$tag))
  row_column_facet <- any(c("COL", "ROW") %in% sort(names(lay)))
  if (row_column && !row_column_facet) {
    stop("Row-column taggs not supported for unstructured facets.")
  }

  if (row_column) {
    o <- order(lay$ROW, lay$COL)
    lay <- lay[o, ]
  }

  facet_vars <- lay[toupper(tag_options$tag)]
  facet_vars <- lapply(facet_vars, function(x) as.numeric(as.factor(x)))
  tag_options$tag_levels <- rep_len(
    tag_options$tag_levels,
    length.out = length(facet_vars)
  )
  facet_tags <- lapply(seq_along(facet_vars), function(i) {
    i_panels <- facet_vars[[i]]
    if (is.null(tag_options$tag_pool[[i]])) {
      tag_pool <- switch(
        tag_options$tag_levels[i],
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

  if (!identical(tag_options$tag, "PANEL")) {
    lay[paste0("TAG_", tag_options$tag)] <- facet_tags
  }

  facet_tags <- Reduce(
    function(a, b) paste(a, b, sep = tag_options$tag_sep),
    facet_tags
  )

  lay[["TAG_PANEL"]] <- facet_tags

  return(lay)
}
