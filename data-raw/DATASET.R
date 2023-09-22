## code to prepare `x3p_subsamples` dataset

x3p_names <- c(
  "T2AW-LM-R1-B30",
  "T2AW-LM-R2-B32",
  "T2CW-LI-R1-B14",
  "T2CW-LI-R2-B15",
  "T3CW-LI-R2-B17"
)

dir <- x3p_names %>%
  paste0("./data-raw/", .) %>%
  paste0(".x3p")

x3p_subsamples <- map(dir, x3p_read) %>%
  map(x3p_average, b = 10) %>%
  set_names(x3p_names)

usethis::use_data(x3p_subsamples, overwrite = TRUE)
