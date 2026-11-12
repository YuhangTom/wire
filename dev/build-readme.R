# Run from the package root: source("dev/build-readme.R")
# bulletxtrctr requires smoother, which was archived on CRAN on 2025-12-19.
# Keep current CRAN available and use the preceding snapshot as a fallback.
# with_options() restores the original repository settings after rendering.
withr::with_options(
  list(repos = c(
    CRAN = "https://cloud.r-project.org",
    CRANarchive = "https://packagemanager.posit.co/cran/2025-12-18"
  )),
  devtools::build_readme()
)
