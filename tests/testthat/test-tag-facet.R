library(ggplot2)
library(vdiffr)
# Base plot
g <- ggplot(mtcars, aes(hp, mpg)) +
  geom_point()
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

pdf(NULL) # Prevent Rplot.pdf
test_that("tagging works", {
  expect_doppelganger("panel tag grid", grid + tag_facets())
  expect_doppelganger("panel tag wrap", wrap + tag_facets())
  expect_doppelganger("panel tag manual", manual + tag_facets())

  expect_doppelganger("rc tag grid", grid + tag_facets(tag = "rc"))
  expect_doppelganger("rc tag wrap", wrap + tag_facets(tag = "rc"))
  expect_error(print(manual + tag_facets(tag = "rc")), "unstructured")

  expect_doppelganger("cr tag wrap", wrap + tag_facets(tag = "cr"))
  expect_doppelganger("cr tag grid", wrap + tag_facets(tag = "cr"))
  expect_error(print(manual + tag_facets(tag = "cr")), "unstructured")
})


test_that("levels", {
  expect_doppelganger(
    "grid levels",
    grid + tag_facets("rc", tag_levels = c("A", "I"))
  )
  expect_doppelganger(
    "wrap levels",
    wrap + tag_facets("rc", tag_levels = c("A", "I"))
  )

  expect_doppelganger(
    "grid panel levels",
    grid + tag_facets("panel", tag_levels = c("A", "I"))
  )
})


test_that("options work", {
  positions <- c("tr", "tl", "bl", "br")
  for (p in positions) {
    expect_doppelganger(
      paste("position", p),
      grid +
        tag_facets(
          position = p
        )
    )
  }

  expect_doppelganger(
    "position custom",
    grid +
      tag_facets(position = list(x = 0.5, y = 0.5))
  )

  expect_doppelganger(
    "tag_pool",
    grid +
      tag_facets(
        tag_pool = c("one", "two", "three", "four", "five", "six")
      )
  )

  expect_doppelganger(
    "tag_pool manual",
    manual +
      tag_facets(
        tag_pool = c("one", "two", "three", "four", "five", "six")
      )
  )
  expect_doppelganger(
    "tag_suffix",
    grid +
      tag_facets(
        tag_suffix = ""
      )
  )
  expect_doppelganger(
    "tag_sep",
    grid +
      tag_facets(tag = "rc", tag_sep = "p")
  )
})


test_that("theming works", {
  expect_doppelganger("theme_dark", wrap + tag_facets() + theme_dark())
  expect_doppelganger(
    "custom theme",
    wrap +
      tag_facets() +
      theme(
        tagger.panel.tag.text = element_text(color = "red", size = 16),
        tagger.panel.tag.background = element_rect(fill = "purple")
      )
  )
})

test_that("clip off works", {
  expect_doppelganger(
    "clip off",
    wrap +
      tag_facets(
        position = list(x = 0, y = 1, vjust = 0, hjust = 0, clip = "off")
      )
  )
})
