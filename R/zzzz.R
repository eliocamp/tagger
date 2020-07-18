.onLoad <- function(libname, pkgname) {
   ggplot2::register_theme_elements(tagger.panel.tag.text = ggplot2::element_text(),
                                    tagger.panel.tag.background = ggplot2::element_rect(),
                                    element_tree = list(tagger.panel.tag.text = ggplot2::el_def("element_text", c("strip.text", "text")),
                                                        tagger.panel.tag.background = ggplot2::el_def("element_rect", c("strip.background", "rect"))))
}

