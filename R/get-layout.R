#' Get labels assigned to each panel of a tagged plot
#'
#' @param filter an expression that, evaluated within the data used
#' to generate the facets of the plot, evaluates to a logical vector
#' or a sequence of rows.
#' @param plot a plot object
#' @param n number of expected panels.
#'
#' @return
#' `get_tags()`, `get_row()` and `get_col()` return a character vector
#' of length `n` with the tags, unique rows or unique columns that meet
#' the `filter` condition.
#'
#' `get_layout()` returns the full data.frame describing the panel layout.
#'
#'
#' @examples
#' library(ggplot2)
#' g <- ggplot(mtcars, aes(hp, mpg)) +
#'   geom_point() +
#'   facet_grid(cyl ~ vs) +
#'   tag_facets("rc")
#'
#' # By default these functions retrieve tags from
#' # the result of ggplo2::last_plot().
#'
#' # Get all tags
#' get_layout()
#'
#' # Get one tag
#' get_tag(cyl == 4 & vs == 0)
#'
#' # Get more than one tag
#' get_tag(cyl == 4, n = 2)
#'
#'
#' get_row(cyl == 4)
#' get_col(vs == 0)
#'
#' # Use it with inline markdown to refer always to the correct panel:
#' # "As you can see in panel `r get_tag(cyl == 4 & vs == 0)` ..."
#'
#' @export
get_layout <- function(plot = ggplot2::last_plot()) {
  if (!inherits(plot, "ggtagged")) {
    stop("The plot has no tags.")
  }
  plot <- ggplot_build_memoised(plot)
  lay <- assign_tags(plot)
  lay <- lay[, setdiff(colnames(lay), c("SCALE_X", "SCALE_Y"))]

  return(lay)
}

#' @export
#' @rdname get_layout
get_tag <- function(filter = TRUE, plot = ggplot2::last_plot(), n = 1) {
  lay <- get_layout(plot = plot)
  filter <- eval(substitute(filter), envir = lay)
  lay <- lay[filter, ]

  if (nrow(lay) != n) {
    stop(
      "Returned ",
      nrow(lay),
      ifelse(n == 1, " panel", " panels"),
      " (expected ",
      n,
      ")."
    )
  }
  lay$TAG_PANEL
}

if (requireNamespace("memoise", quietly = TRUE)) {
  ggplot_build_memoised <- memoise::memoise(ggplot2::ggplot_build)
} else {
  ggplot_build_memoised <- ggplot2::ggplot_build
}

#' @export
#' @rdname get_layout
get_row <- function(filter = TRUE, plot = ggplot2::last_plot(), n = 1) {
  lay <- get_layout(plot = plot)
  filter <- eval(substitute(filter), envir = lay)
  lay <- lay[filter, ]

  row <- unique(lay$TAG_ROW)
  if (length(row) != n) {
    stop(
      "Returned ",
      length(row),
      ifelse(n == 1, " panel", " panels"),
      " (expected ",
      n,
      ")."
    )
  }
  row
}

#' @export
#' @rdname get_layout
get_col <- function(filter = TRUE, plot = ggplot2::last_plot(), n = 1) {
  lay <- get_layout(plot = plot)
  filter <- eval(substitute(filter), envir = lay)
  lay <- lay[filter, ]

  col <- unique(lay$TAG_COL)
  if (length(col) != n) {
    stop(
      "Returned ",
      length(col),
      ifelse(n == 1, " panel", " panels"),
      " (expected ",
      n,
      ")."
    )
  }
  col
}
