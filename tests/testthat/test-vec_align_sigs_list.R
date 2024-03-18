test_that("input check works", {
  expect_error(
    vec_align_sigs_list("abc", shift_sig_df$sig,
      min.overlap = NULL, ifplot = FALSE, legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(TRUE, shift_sig_df$sig,
      min.overlap = NULL, ifplot = FALSE, legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(data.frame(1:3, 2:4), shift_sig_df$sig,
      min.overlap = NULL, ifplot = FALSE, legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig_df$sig, "abc",
      min.overlap = NULL, ifplot = FALSE, legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig_df$sig, TRUE,
      min.overlap = NULL, ifplot = FALSE, legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig_df$sig, data.frame(1:3, 2:4),
      min.overlap = NULL, ifplot = FALSE, legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig_df$sig, shift_sig_df$sig,
      min.overlap = -1, ifplot = FALSE, legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig_df$sig, shift_sig_df$sig,
      min.overlap = 0, ifplot = FALSE, legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig_df$sig, shift_sig_df$sig,
      min.overlap = 1.1, ifplot = FALSE, legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig_df$sig, shift_sig_df$sig,
      min.overlap = "abc", ifplot = FALSE, legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig_df$sig, shift_sig_df$sig,
      min.overlap = TRUE, ifplot = FALSE, legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig_df$sig, shift_sig_df$sig,
      min.overlap = NULL, ifplot = 1, legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig_df$sig, shift_sig_df$sig,
      min.overlap = NULL, ifplot = "abc", legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig_df$sig, shift_sig_df$sig,
      min.overlap = NULL, ifplot = data.frame(1:3, 2:4), legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig_df$sig, shift_sig_df$sig,
      min.overlap = NULL, ifplot = FALSE, legendname = 1,
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig_df$sig, shift_sig_df$sig,
      min.overlap = NULL, ifplot = FALSE, legendname = TRUE,
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig_df$sig, shift_sig_df$sig,
      min.overlap = NULL, ifplot = FALSE, legendname = data.frame(1:3, 2:4),
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig_df$sig, shift_sig_df$sig,
      min.overlap = NULL, ifplot = FALSE, legendname = "Signal",
      titlename = 1
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig_df$sig, shift_sig_df$sig,
      min.overlap = NULL, ifplot = FALSE, legendname = "Signal",
      titlename = TRUE
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig_df$sig, shift_sig_df$sig,
      min.overlap = NULL, ifplot = FALSE, legendname = "Signal",
      titlename = data.frame(1:3, 2:4)
    )
  )
})


test_that("output plot works", {
  expect_visible(
    attr(alignedsigs, "sig_align_plot")
  )
})


test_that("output return works", {
  expect_type(
    alignedsigs, "list"
  )
  expect_length(
    alignedsigs, 4
  )
  expect_named(
    alignedsigs,
    c("ccf", "lag", "lands", "cors")
  )
})
