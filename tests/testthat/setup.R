x3p <- x3p_subsamples[[1]]
insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 2, ifplot = TRUE)
x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)
x3p_inner_impute <- x3p_impute(x3p_inner_nomiss_res, ifout = FALSE, ifsave = FALSE, dir_name = NULL, ifplot = TRUE)
MLE_angle <- x3p_MLE_angle_vec(x3p_inner_impute, ntheta = 720, min_score_cut = 0.1, ifplot = FALSE, loess_span = 0.2)
x3p_bin_rotate <- x3p_vertical(x3p_inner_impute, min_score_cut = 0.1, ifplot = FALSE)
raw_sig <- x3p_raw_sig_vec(x3p_bin_rotate, ifplot = TRUE)
shift_sig <- x3p_shift_sig_vec(x3p_bin_rotate, ifplot = FALSE, delta = -5:5)
alignedsigs <- vec_align_sigs_list(raw_sig$sig, shift_sig$sig,
  min.overlap = NULL, ifplot = TRUE, legendname = "Signal",
  titlename = NULL
)
boundary <- x3p_boundary_points(x3p, sample = 3)
insidepoly <- inside_polygon(x = 1:3, y = 2:4, concavity = 1.5, center = NULL)
poly <- x3p_surface_polygon(x3p, colour = "red", sample = 10, center = NULL, concavity = 1.5)
