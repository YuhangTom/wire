x3p_bin <- x3p_read(here::here("../Wirecuts/scans/T4BW-LI-R2.x3p")) %>%
  x3p_insidepoly_df(b = 1) %>%
  df_rmtrend_x3p() %>%
  x3p_impute() %>%
  x3p_bin_stripes(
    direction = "vertical",
    colors = c("#b12819", "#ffffff", "#134D6B"), freqs = c(0, 0.3, 0.7, 1)
  )
x3p_plot <- x3p_bin %>%
  x3p_delete_mask() %>%
  x3p_trim_na()

list.files(here::here("inst"), pattern = "^x3p_bin_.*.png$", full.names = TRUE) %>%
  file.remove()

x3p_image(x3p_plot,
          size = c(500, 200), zoom = 0.6,
          file = here::here("inst/x3p_bin_before_rotate_no_mask.png")
)
x3p_plot <- x3p_bin %>%
  x3p_trim_na()
x3p_image(x3p_plot,
          size = c(500, 200), zoom = 0.6,
          file = here::here("inst/x3p_bin_before_rotate.png")
)
x3p_bin_red <- x3p_extract(x3p_bin, mask_vals = "#b12819")

x3p_plot <- x3p_bin_red %>%
  x3p_trim_na()
x3p_image(x3p_plot,
          size = c(500, 200), zoom = 0.6,
          file = here::here("inst/x3p_bin_red_before_rotate.png")
)

angle <- -6.158554
n_frame <- 20
angle_gif <- angle / n_frame
purrr::walk(1:n_frame, function(i_frame) {
  x3p_plot <- x3p_rotate(x3p_bin_red, angle_gif * i_frame)
  x3p_image(x3p_plot,
            size = c(500, 200), zoom = 0.6,
            file = here::here(sprintf("inst/x3p_bin_red_before_rotate_%s.png", i_frame))
  )
})

x3p_plot <- x3p_bin %>%
  x3p_rotate(angle)
x3p_image(x3p_plot,
          size = c(500, 200), zoom = 0.6,
          file = here::here("inst/x3p_bin_after_rotate.png")
)
x3p_plot <- x3p_bin %>%
  x3p_rotate(angle) %>%
  x3p_delete_mask()
x3p_image(x3p_plot,
          size = c(500, 200), zoom = 0.6,
          file = here::here("inst/x3p_bin_after_rotate_no_mask.png")
)

library(magick)
x3p_bin_red_before_rotate_gif <- c(
  rep(here::here("inst/x3p_bin_before_rotate_no_mask.png"), 10),
  rep(here::here("inst/x3p_bin_before_rotate.png"), 10),
  rep(here::here("inst/x3p_bin_red_before_rotate.png"), 10),
  here::here(sprintf("inst/x3p_bin_red_before_rotate_%s.png", 1:n_frame)),
  rep(here::here(sprintf("inst/x3p_bin_red_before_rotate_%s.png", n_frame)), 10),
  rep(here::here("inst/x3p_bin_after_rotate.png"), 10),
  rep(here::here("inst/x3p_bin_after_rotate_no_mask.png"), 10)
) %>%
  map(image_read) %>%
  image_join() %>%
  image_animate()

image_write(
  image = x3p_bin_red_before_rotate_gif,
  path = here::here("inst/x3p_bin_red_before_rotate.gif")
)

list.files(here::here("inst"), pattern = "^x3p_bin_.*.png$", full.names = TRUE) %>%
  file.remove()
