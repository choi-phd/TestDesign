# Precompile vignettes
# Edit source files for any content change and run this script to render

library(knitr)
knit("vignettes/split.Rmd.source", "vignettes/split.Rmd")

fs <- list.files(pattern = ".svg")
for (f in fs) {
  file.rename(
    file.path(f),
    file.path("vignettes", f)
  )
}
