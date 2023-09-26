x3p <- x3p_subsamples[[1]]
insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1, ifplot = FALSE)
x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)
x3p_inner_impute <- x3p_impute(x3p_inner_nomiss_res, ifsave = FALSE, dir_name = NULL, ifplot = FALSE)
x3p_bin_rotate <- x3p_vertical(x3p_inner_impute, min_score_cut = 0.1, ifplot = FALSE)
raw_sig <- x3p_raw_sig_vec(x3p_bin_rotate, method = "median", ifplot = FALSE)
shift_sig <- x3p_shift_sig_vec(x3p_bin_rotate, method = "median", ifplot = FALSE, delta = -5:5)
alignedsigs <- vec_align_sigs_list(raw_sig, shift_sig,
  min.overlap = NULL, ifplot = FALSE,
  name1 = "Cut1", name2 = "Cut2", legendname = "Signal",
  titlename = NULL
)
insidepoly <- inside_polygon(x = 1:3, y = 2:4, concavity = 1.5, center = NULL)
