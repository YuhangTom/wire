test_that("input check works", {
  expect_error(
    df_ccsig(1:3, span1 = 400, span2 = 40,
             breaks_q = c(0, 0.025, 0.05, 0.95, 0.975, 1),
             ifplot = FALSE)
  )
  expect_error(
    df_ccsig("abc", span1 = 400, span2 = 40,
             breaks_q = c(0, 0.025, 0.05, 0.95, 0.975, 1),
             ifplot = FALSE)
  )
  expect_error(
    df_ccsig(TRUE, span1 = 400, span2 = 40,
                   breaks_q = c(0, 0.025, 0.05, 0.95, 0.975, 1),
                   ifplot = FALSE)
  )
  expect_error(
    df_ccsig(raw_sig_df, span1 = 1:3, span2 = 40,
             breaks_q = c(0, 0.025, 0.05, 0.95, 0.975, 1),
             ifplot = FALSE)
  )
  expect_error(
    df_ccsig(raw_sig_df, span1 = "abc", span2 = 40,
                   breaks_q = c(0, 0.025, 0.05, 0.95, 0.975, 1),
                   ifplot = FALSE)
  )
  expect_error(
    df_ccsig(raw_sig_df, span1 = TRUE, span2 = 40,
                   breaks_q = c(0, 0.025, 0.05, 0.95, 0.975, 1),
                   ifplot = FALSE)
  )
  expect_error(
    df_ccsig(raw_sig_df, span1 = data.frame(1:3, 2:4), span2 = 40,
             breaks_q = c(0, 0.025, 0.05, 0.95, 0.975, 1),
             ifplot = FALSE)
  )
  expect_error(
    df_ccsig(raw_sig_df, span1 = 400, span2 = 1:3,
             breaks_q = c(0, 0.025, 0.05, 0.95, 0.975, 1),
             ifplot = FALSE)
  )
  expect_error(
    df_ccsig(raw_sig_df, span1 = 400, span2 = "abc",
             breaks_q = c(0, 0.025, 0.05, 0.95, 0.975, 1),
             ifplot = FALSE)
  )
  expect_error(
    df_ccsig(raw_sig_df, span1 = 400, span2 = TRUE,
                   breaks_q = c(0, 0.025, 0.05, 0.95, 0.975, 1),
                   ifplot = FALSE)
  )
  expect_error(
    df_ccsig(raw_sig_df, span1 = 400, span2 = data.frame(1:3, 2:4),
                   breaks_q = c(0, 0.025, 0.05, 0.95, 0.975, 1),
                   ifplot = FALSE)
  )
  expect_error(
    df_ccsig(raw_sig_df, span1 = 400, span2 = 40,
             breaks_q = 1:3,
             ifplot = FALSE)
  )
  expect_error(
    df_ccsig(raw_sig_df, span1 = 400, span2 = 40,
             breaks_q = "abc",
             ifplot = FALSE)
  )
  expect_error(
    df_ccsig(raw_sig_df, span1 = 400, span2 = 40,
             breaks_q = TRUE,
             ifplot = FALSE)
  )
  expect_error(
    df_ccsig(raw_sig_df, span1 = 400, span2 = 40,
             breaks_q = data.frame(1:3, 2:4),
             ifplot = FALSE)
  )
  expect_error(
    df_ccsig(raw_sig_df, span1 = 400, span2 = 40,
             breaks_q = c(0, 0.025, 0.05, 0.95, 0.975, 1),
             ifplot = 1:3)
  )
  expect_error(
    df_ccsig(raw_sig_df, span1 = 400, span2 = 40,
             breaks_q = c(0, 0.025, 0.05, 0.95, 0.975, 1),
             ifplot = "abc")
  )
  expect_error(
    df_ccsig(raw_sig_df, span1 = 400, span2 = 40,
             breaks_q = c(0, 0.025, 0.05, 0.95, 0.975, 1),
             ifplot = data.frame(1:3, 2:4))
  )
})


test_that("output plot works", {
  expect_visible(
    attr(raw_ccsig_df, "sig_df_plot")
  )
})


test_that("output return works", {
  expect_type(
    raw_sig_df, "list"
  )
})
