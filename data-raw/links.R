library(dplyr)
links <-
  readr::read_csv("data-raw/Hwy_eval_links.csv") %>%
  select(
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
      levels = 1:4,
      labels = c("Expressway", "Highway", "Arterial", "Collector")
    ),
    area_name = factor(
      area_type,
      levels = 1:5,
      labels = c("CBD", "Urban", "Exurban", "Suburban", "Rural")
    )
  )

devtools::use_data(links, overwrite = TRUE)
