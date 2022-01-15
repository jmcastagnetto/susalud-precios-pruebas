library(tidyverse)
library(readxl)

ipress <- read_csv("extra/RENIPRESS_2022_v2.csv")
saveRDS(ipress, "extra/ipress.rds")

pcr <- read_excel("20220115/ReportePrecios-pcr.xlsx") %>%
  janitor::clean_names() %>%
  mutate(
    precio = str_extract(precio_al_publico, "\\d+") %>%
      as.numeric()
  )
saveRDS(
  pcr,
  "20220115/pcr.rds"
)

antig <- read_excel("20220115/ReportePrecios-antigenos.xlsx") %>%
  janitor::clean_names() %>%
  mutate(
    precio = str_extract(precio_al_publico, "\\d+") %>%
      as.numeric()
  )
saveRDS(
  antig,
  "20220115/antig.rds"
)
