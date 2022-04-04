library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(writexl)
library(imputeTS)
rm(list = ls())

inp <- "./datos/bienes"
list.files(inp)
out <- "./databases"

claves <- read_excel(paste(inp, "catalogo claves municipales.xlsx", sep = "/"), skip = 3) %>% 
  select(1,4,5) %>% 
  rename(nombre_edo = nom_mun,
         edo = cve_ent)


data <- read_excel(paste(inp, "INEGI_Exporta_20210414233356_homicidios_municipio.xlsx", sep = "/"), sheet = 2) %>% 
  mutate(
    dummy = ifelse(is.na(edo), 0, 1)
  ) %>% 
  fill(edo) %>% 
  filter(!str_detect(nombre_edo, "especificado") & dummy!=1) %>% 
  select(-dummy) %>% 
  mutate(
    edo = str_pad(edo, 2, pad = "0")
  )

final <- left_join(data, claves)

write_xlsx(final, path = paste(out, "homicidios.xlsx", sep = "/"))