library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(writexl)
library(imputeTS)
rm(list = ls())

out <- "./databases"
list.files(out)

data <- read_excel(paste(out, "homicidios.xlsx", sep = "/")) %>% 
  select(-nombre_edo) %>% 
  mutate(
    cve_mun = str_pad(cve_mun, 3, pad = "0")
  ) %>% 
  pivot_longer(
    cols = 3:32,
    names_to = "year",
    values_to = "homicidios"
  ) %>% 
  mutate(
    muni = paste0(edo, cve_mun),
    muniYear = paste(muni, year, sep = "_")
  ) %>% 
  filter(year >= 1994 & year<=2015) %>% 
  select(muniYear, homicidios, )

write.csv(data, paste(out, "homicidios.csv", sep = "/"), row.names = F)