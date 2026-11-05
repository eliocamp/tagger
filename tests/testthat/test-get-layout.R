library(ggplot2)

base <- ggplot(mtcars, aes(hp, mpg)) +
  geom_point()

g1 <- base +
  facet_grid(gear ~ am) +
  tag_facets("rc")

g <- base +
  facet_grid(cyl ~ vs) +
  tag_facets("rc")
test_that("getting tags works", {
  expect_identical(get_tag(cyl == 4 & vs == 0), "a.1")
  expect_identical(get_tag(cyl == 4, n = 2), c("a.1", "a.2"))
  expect_error(get_tag(cyl == 4), "expected 1")

  expect_identical(get_row(cyl == 4), "a")
  expect_identical(get_col(vs == 0), 1)
  expect_error(get_col(vs == 0, plot = g1), "not found")

  expect_error(get_col(gear == 3, plot = g1), "expected 1")
  expect_error(get_row(am == 0, plot = g1), "expected 1")
  expect_identical(get_row(gear == 3, plot = g1), "a")

  expect_error(get_col(gear == 3, plot = base), "no tags")
})
