library(ggplot2)
library(vdiffr)
# Base plot
g <- ggplot(mtcars, aes(hp, mpg)) +
  geom_point()

test_that("", {
  grid <- g +
    facet_grid(cyl ~ vs)
  wrap <- g +
    facet_wrap(cyl ~ vs)

  design <- "
 A##
 AB#
 #BC
 ##C
"
  manual <- g + ggh4x::facet_manual(~cyl, design)

  expect_doppelganger("panel tag grid", grid + tag_facets())
  expect_doppelganger("panel tag wrap", wrap + tag_facets())
  expect_doppelganger("panel tag wrap", manual + tag_facets())

  expect_doppelganger("rc tag grid", grid + tag_facets(tag = "rc"))
  expect_doppelganger("rc tag wrap", wrap + tag_facets(tag = "rc"))
  expect_error(manual + tag_facets(tag = "rc"), "unstructured")

  expect_doppelganger("cr tag wrap", wrap + tag_facets(tag = "cr"))
  expect_doppelganger("cr tag grid", wrap + tag_facets(tag = "cr"))
  expect_error(manual + tag_facets(tag = "cr"), "unstructured")
})
