library(dplyr)
links <-
  readr::read_csv("data-raw/Hwy_eval_links.csv") %>%
  select(
    a = A, b = B,
    facility_type = `Fac Type`,
    facility_group = `Fac Type Group`,
    area_type = `Area Type`,
    volume = Volume,
    screenline = ScreenLine,
    count = Count
  ) %>%
  mutate(
    facility_group = factor(
      facility_group,
      labels = c("Expressway", "Highway", "Arterial", "Collector")
    ),
    area_name = factor(
      area_type,
      labels = c("Urban", "Exurban", "Suburban", "Rural")
    )
  )

devtools::use_data(links, overwrite = TRUE)
