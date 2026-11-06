# Get labels assigned to each panel of a tagged plot

Get labels assigned to each panel of a tagged plot

## Usage

``` r
get_layout(plot = ggplot2::last_plot())

get_tag(filter = TRUE, plot = ggplot2::last_plot(), n = 1)

get_row(filter = TRUE, plot = ggplot2::last_plot(), n = 1)

get_col(filter = TRUE, plot = ggplot2::last_plot(), n = 1)
```

## Arguments

- plot:

  a plot object

- filter:

  an expression that, evaluated within the data used to generate the
  facets of the plot, evaluates to a logical vector or a sequence of
  rows.

- n:

  number of expected panels.

## Value

`get_tags()`, `get_row()` and `get_col()` return a character vector of
length `n` with the tags, unique rows or unique columns that meet the
`filter` condition.

`get_layout()` returns the full data.frame describing the panel layout.

## Examples

``` r
library(ggplot2)
g <- ggplot(mtcars, aes(hp, mpg)) +
  geom_point() +
  facet_grid(cyl ~ vs) +
  tag_facets("rc")

# By default these functions retrieve tags from
# the result of ggplo2::last_plot().

# Get all tags
get_layout()
#>   PANEL ROW COL cyl vs COORD TAG_ROW TAG_COL TAG_PANEL
#> 1     1   1   1   4  0     1       a       1       a.1
#> 2     2   1   2   4  1     1       a       2       a.2
#> 3     3   2   1   6  0     1       b       1       b.1
#> 4     4   2   2   6  1     1       b       2       b.2
#> 5     5   3   1   8  0     1       c       1       c.1
#> 6     6   3   2   8  1     1       c       2       c.2

# Get one tag
get_tag(cyl == 4 & vs == 0)
#> [1] "a.1"

# Get more than one tag
get_tag(cyl == 4, n = 2)
#> [1] "a.1" "a.2"


get_row(cyl == 4)
#> [1] "a"
get_col(vs == 0)
#> [1] 1

# Use it with inline markdown to refer always to the correct panel:
# "As you can see in panel `r get_tag(cyl == 4 & vs == 0)` ..."
```
