test_that("input check works", {
  expect_error(
    vec_align_sigs_list("abc", shift_sig$sig,
      min.overlap = NULL, ifplot = FALSE,
      name1 = "Cut1", name2 = "Cut2", legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(TRUE, shift_sig$sig,
      min.overlap = NULL, ifplot = FALSE,
      name1 = "Cut1", name2 = "Cut2", legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(data.frame(1:3, 2:4), shift_sig$sig,
      min.overlap = NULL, ifplot = FALSE,
      name1 = "Cut1", name2 = "Cut2", legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig$sig, "abc",
      min.overlap = NULL, ifplot = FALSE,
      name1 = "Cut1", name2 = "Cut2", legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig$sig, TRUE,
      min.overlap = NULL, ifplot = FALSE,
      name1 = "Cut1", name2 = "Cut2", legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig$sig, data.frame(1:3, 2:4),
      min.overlap = NULL, ifplot = FALSE,
      name1 = "Cut1", name2 = "Cut2", legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig$sig, shift_sig$sig,
      min.overlap = -1, ifplot = FALSE,
      name1 = "Cut1", name2 = "Cut2", legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig$sig, shift_sig$sig,
      min.overlap = 0, ifplot = FALSE,
      name1 = "Cut1", name2 = "Cut2", legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig$sig, shift_sig$sig,
      min.overlap = 1.1, ifplot = FALSE,
      name1 = "Cut1", name2 = "Cut2", legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig$sig, shift_sig$sig,
      min.overlap = "abc", ifplot = FALSE,
      name1 = "Cut1", name2 = "Cut2", legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig$sig, shift_sig$sig,
      min.overlap = TRUE, ifplot = FALSE,
      name1 = "Cut1", name2 = "Cut2", legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig$sig, shift_sig$sig,
      min.overlap = NULL, ifplot = 1,
      name1 = "Cut1", name2 = "Cut2", legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig$sig, shift_sig$sig,
      min.overlap = NULL, ifplot = "abc",
      name1 = "Cut1", name2 = "Cut2", legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig$sig, shift_sig$sig,
      min.overlap = NULL, ifplot = data.frame(1:3, 2:4),
      name1 = "Cut1", name2 = "Cut2", legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig$sig, shift_sig$sig,
      min.overlap = NULL, ifplot = FALSE,
      name1 = 1, name2 = "Cut2", legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig$sig, shift_sig$sig,
      min.overlap = NULL, ifplot = FALSE,
      name1 = TRUE, name2 = "Cut2", legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig$sig, shift_sig$sig,
      min.overlap = NULL, ifplot = FALSE,
      name1 = data.frame(1:3, 2:4), name2 = "Cut2", legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig$sig, shift_sig$sig,
      min.overlap = NULL, ifplot = FALSE,
      name1 = "Cut1", name2 = 1, legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig$sig, shift_sig$sig,
      min.overlap = NULL, ifplot = FALSE,
      name1 = "Cut1", name2 = TRUE, legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig$sig, shift_sig$sig,
      min.overlap = NULL, ifplot = FALSE,
      name1 = "Cut1", name2 = data.frame(1:3, 2:4), legendname = "Signal",
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig$sig, shift_sig$sig,
      min.overlap = NULL, ifplot = FALSE,
      name1 = "Cut1", name2 = "Cut2", legendname = 1,
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig$sig, shift_sig$sig,
      min.overlap = NULL, ifplot = FALSE,
      name1 = "Cut1", name2 = "Cut2", legendname = TRUE,
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig$sig, shift_sig$sig,
      min.overlap = NULL, ifplot = FALSE,
      name1 = "Cut1", name2 = "Cut2", legendname = data.frame(1:3, 2:4),
      titlename = NULL
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig$sig, shift_sig$sig,
      min.overlap = NULL, ifplot = FALSE,
      name1 = "Cut1", name2 = "Cut2", legendname = "Signal",
      titlename = 1
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig$sig, shift_sig$sig,
      min.overlap = NULL, ifplot = FALSE,
      name1 = "Cut1", name2 = "Cut2", legendname = "Signal",
      titlename = TRUE
    )
  )
  expect_error(
    vec_align_sigs_list(raw_sig$sig, shift_sig$sig,
      min.overlap = NULL, ifplot = FALSE,
      name1 = "Cut1", name2 = "Cut2", legendname = "Signal",
      titlename = data.frame(1:3, 2:4)
    )
  )
})


test_that("output plot works", {
  expect_visible(
    vec_align_sigs_list(raw_sig$sig, shift_sig$sig,
      min.overlap = NULL, ifplot = TRUE,
      name1 = "Cut1", name2 = "Cut2", legendname = "Signal",
      titlename = NULL
    )
  )
})


test_that("output return works", {
  expect_type(
    alignedsigs, "list"
  )
  expect_length(
    alignedsigs, 3
  )
  expect_named(
    alignedsigs,
    c("ccf", "lag", "lands")
  )
})
