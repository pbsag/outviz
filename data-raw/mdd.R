mdd <- dplyr::data_frame(
  volume = c(1000, 2500, 5000, 10000, 25000, 50000, 70000),
  mdd = c(0.9, 0.7, 0.5, 0.35, 0.25, 0.2, 0.17) * 100
)

devtools::use_data(mdd, overwrite = TRUE)
