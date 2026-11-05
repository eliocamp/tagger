main <- function() {
  sha <- Sys.getenv("GITHUB_SHA", unset = "")
  if (sha == "") {
    stop(
      "This script needs to be run in GitHub Actions. For local testing, set GITHUB_SHA first."
    )
  }

  # The workflow checks the coverage branch out into ./coverage (a git worktree)
  if (!dir.exists("coverage")) {
    stop(
      "./coverage not found. The coverage branch must be checked out there first."
    )
  }
  out_dir <- normalizePath("coverage")
  history_file <- file.path(out_dir, "history.json")

  history <- if (file.exists(history_file)) {
    jsonlite::read_json(history_file)
  } else {
    list()
  }

  if (length(history) > 0 && identical(history[[length(history)]]$sha, sha)) {
    message("Commit ", sha, " already recorded; not appending.")
    return(invisible(0))
  }

  cov <- covr::package_coverage(quiet = FALSE)
  percent <- round(covr::percent_coverage(cov), 2)

  # Build the report in a temp dir so lib/ never touches the repo
  work <- tempfile("covr-")
  dir.create(work)
  old_wd <- setwd(work)
  on.exit(
    {
      setwd(old_wd)
      unlink(work, recursive = TRUE)
    },
    add = TRUE
  )

  covr::report(cov, file = file.path(work, "report.html"), browse = FALSE)

  # Remove the doctype, because pandoc escapes it otherwise
  lines <- readLines("report.html", warn = FALSE, encoding = "UTF-8")
  lines[1] <- sub("^\\s*<!DOCTYPE[^>]*>", "", lines[1], ignore.case = TRUE)
  writeLines(lines, "report.html", useBytes = TRUE)

  # Pandoc resolves lib/ relative to the working directory, hence setwd() above
  rmarkdown::pandoc_self_contained_html("report.html", "report_sc.html")

  lines <- readLines("report_sc.html", warn = FALSE, encoding = "UTF-8")
  writeLines(
    c("<!DOCTYPE html>", lines),
    file.path(out_dir, "report.html"),
    useBytes = TRUE
  )

  # Badge, same thresholds as ghacoveragebadge
  colour <- if (percent >= 90) {
    "#4c1"
  } else if (percent >= 80) {
    "#97ca00"
  } else if (percent >= 70) {
    "#a4a61d"
  } else if (percent >= 60) {
    "#dfb317"
  } else if (percent >= 50) {
    "#fe7d37"
  } else {
    "#e05d44"
  }

  value <- sprintf("%.2f%%", percent)
  lw <- 62
  vw <- nchar(value) * 7 + 10
  w <- lw + vw

  svg <- sprintf(
    paste0(
      '<svg xmlns="http://www.w3.org/2000/svg" width="%s" height="20" role="img" aria-label="coverage: %s">',
      '<title>coverage: %s</title>',
      '<clipPath id="r"><rect width="%s" height="20" rx="3" fill="#fff"/></clipPath>',
      '<g clip-path="url(#r)"><rect width="%s" height="20" fill="#555"/>',
      '<rect x="%s" width="%s" height="20" fill="%s"/></g>',
      '<g fill="#fff" text-anchor="middle" font-family="Verdana,Geneva,DejaVu Sans,sans-serif" font-size="11">',
      '<text x="%s" y="14">coverage</text><text x="%s" y="14">%s</text></g></svg>'
    ),
    w,
    value,
    value,
    w,
    lw,
    lw,
    vw,
    colour,
    lw / 2,
    lw + vw / 2,
    value
  )
  writeLines(svg, file.path(out_dir, "coverage.svg"))

  history[[length(history) + 1]] <- list(
    sha = sha,
    date = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    percent = percent
  )
  jsonlite::write_json(history, history_file, auto_unbox = TRUE, pretty = TRUE)

  message("Coverage: ", percent, "%")
  invisible(0)
}

main()
