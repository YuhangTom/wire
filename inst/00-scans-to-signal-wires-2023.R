library(tidyverse)
library(x3ptools)
library(wire)

data_path <- "~/Documents/Data/Wire Cutting Project Summer 2023/Wirecuts/scans/"
source = dir(data_path, full.names = TRUE, recursive = TRUE, pattern = "x3p")



meta <- data.frame(source=source)


i <- 1

bsample <- 5

x3p <- x3p_read(meta$source[i])
res <- replicate(10, {
  p1 <- proc.time()
  insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = bsample)

  p2 <- proc.time()

  x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)

  p3 <- proc.time()

  x3p_inner_impute <- x3p_impute(x3p_inner_nomiss_res,
                                 ifsave = FALSE, dir_name = NULL, ifplot = FALSE)

  p4 <- proc.time()

  x3p_bin_rotate <- x3p_vertical(x3p_inner_impute, min_score_cut = 0.1, ifplot = FALSE)

  p5 <- proc.time()

  x3p_shift <- x3p_shift_sig_vec(x3p_bin_rotate, ifplot = FALSE)

  p6 <- proc.time()
  # write output

  rbind(start=p1, x3p_insidepoly_df=p2-p1, detrend=p3-p2, impute=p4-p3, rotate=p5-p4, shift=p6-p5, overall=p6-p1)
})

res_list <- res %>% apply(MARGIN=3, FUN = function(d) {
  functions <- dimnames(d)[[1]]
  d <- as.tibble(d)
  d$functions <- functions
  d
})

dframe <- data.frame(rep = 1:dim(res)[3], sim_data=I(res_list)) %>% unnest(col=sim_data)

dframe$test_name <- "baseline-Oct-05"
dframe$x3p_source <-basename(meta$source[i])
dframe$b <- bsample

write_csv(dframe, "speed-evaluation.csv",
          append = file.exists("speed-evaluation.csv"))

dframe %>% filter(functions != "start") %>% ggplot(aes(x = functions, y = elapsed)) + geom_point()

