library(wire)

# x3p <- x3p_subsamples[[1]]
# insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1)
# x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)
#
# tempout <- x3p_impute(x3p_inner_nomiss_res,
#   ifout = TRUE, ifsave = FALSE, dir_name = NULL, ifplot = FALSE
# )
# tempin <- x3p_impute(x3p_inner_nomiss_res,
#   ifout = FALSE, ifsave = FALSE, dir_name = NULL, ifplot = FALSE
# )
#
# identical(tempout, tempin)

library(rbenchmark)

x3p <- x3ptools::x3p_read("data-raw/T2CW-LI-R2-B15.x3p")

t0 <- Sys.time()

benchmark(
  "x3p_insidepoly_df" = {
    insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1)
  },
  "df_rmtrend_x3p" = {
    x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)
  },
  "x3p_impute-in" = {
    x3p_inner_impute <- x3p_impute(x3p_inner_nomiss_res,
      ifout = FALSE, ifsave = FALSE, dir_name = NULL, ifplot = FALSE
    )
  },
  "x3p_vertical" = {
    x3p_bin_rotate <- x3p_vertical(x3p_inner_impute, min_score_cut = 0.1)
  },
  "x3p_shift_sig_vec" = {
    shift_sig <- x3p_shift_sig_vec(x3p_bin_rotate)
  },
  replications = 1,
  columns = c("test", "user.self", "sys.self", "elapsed", "user.child", "sys.child"),
  order = NULL
)

Sys.time() - t0

#                test user.self sys.self elapsed user.child sys.child
# 1 x3p_insidepoly_df     1.911    0.186   2.048          0         0
# 2    df_rmtrend_x3p     1.930    0.247   2.183          0         0
# 3     x3p_impute-in     4.698    0.656   5.352          0         0
# 4      x3p_vertical     8.342    3.722  12.691          0         0
# 5 x3p_shift_sig_vec     1.954    0.319   2.272          0         0
#
# Time difference of 50.55963 secs
