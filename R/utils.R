# # from ggplot2
# plot_theme <- function(x, default = ggplot2::theme_get()) {
#    theme <- x$theme
#
#    # apply theme defaults appropriately if needed
#    if (ggplot2:::is_theme_complete(theme)) {
#       # for complete themes, we fill in missing elements but don't do any element merging
#       # can't use `defaults()` because it strips attributes
#       missing <- setdiff(names(default), names(theme))
#       theme[missing] <- default[missing]
#    } else {
#       # otherwise, we can just add the theme to the default theme
#       theme <- default + theme
#    }
#
#    # if we're still missing elements relative to fallback default, fill in those
#    missing <- setdiff(names(ggplot2:::ggplot_global$theme_default), names(theme))
#    theme[missing] <- ggplot2:::ggplot_global$theme_default[missing]
#
#    # Check that all elements have the correct class (element_text, unit, etc)
#    if (ggplot2:::is_theme_validate(theme)) {
#       mapply(
#          ggplot2:::validate_element, theme, names(theme),
#          MoreArgs = list(element_tree = ggplot2::get_element_tree())
#       )
#    }
#
#    theme
# }
#
