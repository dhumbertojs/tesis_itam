library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(stringr)
library(writexl)
library(lubridate)
rm(list = ls())

inp <- "./databases"
out <- getwd()

cal <- read_excel(paste(inp, "Calendario Electoral Mexico 1980-2025.xlsx", sep = "/"), sheet = 2)
  
fed <- cal %>% 
  filter(edon == 0) %>% 
  mutate(
    fecha = paste(elDay, elMonth, elYear, sep = "-"),
    fecha = dmy(fecha),
  ) %>% 
  select(fecha, office, elYear) %>% 
  group_by(elYear) %>% 
  pivot_wider(
    names_from = office,
    values_from = fecha
  )

local <- cal %>% 
  filter(edon != 0 & round == 1) %>% 
  mutate(
    fecha = paste(elDay, elMonth, elYear, sep = "-"),
    fecha = dmy(fecha),
  ) %>% 
  select(
    elYear, fecha, office, edon, extraordinaria
  ) %>% 
  pivot_wider(
    names_from = office,
    values_from = fecha
  )

calendario <- local %>% 
  left_join(fed) %>% 
  mutate(
    edon = str_pad(edon, width = 2, side = "left", pad = "0"),
    d_gob = ifelse(ayun == gob, 1, 0),
    d_gob = ifelse(is.na(d_gob), 0, d_gob),
    
    d_dipl = ifelse(ayun == dloc, 1, 0),
    d_dipl = ifelse(is.na(d_dipl), 0, d_dipl),
    
    d_dipf = ifelse(ayun == dip, 1, 0),
    d_dipf = ifelse(is.na(d_dipf), 0, d_dipf),
    
    d_sen = ifelse(ayun == sen, 1, 0),
    d_sen = ifelse(is.na(d_sen), 0, d_sen),
    
    d_pres = ifelse(ayun == pres, 1, 0),
    d_pres = ifelse(is.na(d_pres), 0, d_pres)
  ) %>% 
  rename(year = elYear, state = edon) %>% 
  select(1,2,10:14)


crimen <- read.csv(paste(inp, "homicidios.csv", sep = "/")) %>% 
  rename(hom = homicidios) %>% 
  mutate(muni = sapply(strsplit(muniYear, "_"), "[", 1))
  
cri <- crimen %>% 
  group_by(muni) %>% 
  ungroup() %>% 
  select(-3)

crimen <- crimen %>% 
  group_by(muni) %>% 
  mutate(
    hom = lead(hom, 1)
  ) %>% 
  #fill(hom) %>% 
  ungroup() %>% 
  select(-3)

ele <- read.csv(paste(inp, "Electoral.csv", sep = "/"))

mar <- read.csv(paste(inp, "margination.csv", sep = "/")) %>% 
  rename(year = AÑO, muni = CVE_MUN) %>% 
  select(-POB_TOT) %>% 
  mutate(muni = str_pad(muni, width = 5, side = "left", pad = "0"))
pob <- read.csv(paste(inp, "poblation.csv", sep = "/"))

servicios <- read.csv(paste(inp, "Services_original.csv", sep = "/")) %>% 
  mutate(muni = sapply(strsplit(muniYear, "_"), "[", 1)) 

ser <- servicios %>% 
  group_by(muni) %>% 
  ungroup() %>% 
  select(-5)

servicios <- servicios %>% 
  group_by(muni) %>% 
  mutate(
    agua = lead(agua, n = 1),
    dren = lead(dren ,n = 1),
    elec = lead(elec, n = 1)  
  ) %>% 
  ungroup() %>% 
  select(-5)

muni <- levels(as.factor(mar$muni))
year <- 1990:2015
years <- data.frame(year)
years <- filter(years, !year %in% c(1990, 1995, 2000, 2005, 2010, 2015))
im <- map(years$year, 
          ~ bind_cols(.x, muni))
im <- bind_rows(im) %>% 
  rename(year = ...1, muni = ...2) %>% 
  mutate(muniYear = paste(muni, year, sep = "_"))
mar <- bind_rows(mar, im) %>% 
  arrange(muniYear) %>% 
  fill(IM) %>% 
  select(-c(muni, year))

# #64,710 observaciones
nrow(ele)#27,292
ele <- filter(ele, year >= 1994)
nrow(ele)#17,023


try <- ser %>% 
  #
  left_join(cri, by = "muniYear") %>% 
  #13,373
  left_join(mar, by = "muniYear") %>% 
  #11,142 observaciones
  left_join(pob, by = "muniYear") %>% 
  left_join(ele, by = "muniYear")

try <- try %>% 
  mutate(
    year = as.numeric(sapply(strsplit(muniYear, "_"), "[", 2)),
    trienio_inicio = ifelse(!is.na(muni), year, NA)
  ) %>% 
  fill(trienio_inicio, .direction = "up") %>% 
  group_by(trienio_inicio) %>% 
  mutate(
    n_agua = ifelse(is.na(agua), 1, 0),
    n_dren = ifelse(is.na(dren), 1, 0),
    n_elec = ifelse(is.na(elec), 1, 0),
    n_hom = ifelse(is.na(hom), 1, 0),
  ) %>% 
  ungroup() %>% 
  mutate(
    inc.ch = inc.ch*100,
    t.agua = (agua/POB_TOT) * 100000,
    t.dren = (dren/POB_TOT) * 100000,
    t.elec = (elec/POB_TOT) * 100000,
    t.hom = (hom/POB_TOT) * 100000
  ) %>% 
  group_by(muni) %>% 
  mutate(
    lt.agua = lag(t.agua, n = 1, order_by = year),
    lt.dren = lag(t.dren, n = 1, order_by = year),
    lt.elec = lag(t.elec, n = 1, order_by = year), 
    lt.hom = lag(t.hom, n = 1, order_by = year),
    
    #Valor presente - valor pasado / valor pasado
    
    ch.agua = ifelse(!is.na(lt.agua) & !is.na(t.agua), ((t.agua - lt.agua) * 100)/lt.agua, NA),
    ch.dren = ifelse(!is.na(lt.dren) & !is.na(t.dren), ((t.dren - lt.dren) * 100)/lt.dren, NA),
    ch.elec = ifelse(!is.na(lt.elec) & !is.na(t.elec), ((t.elec - lt.elec) * 100)/lt.elec, NA),
    ch.hom = t.hom - lt.hom,
    
    ch.agua = ifelse(is.infinite(ch.agua), NA, ch.agua),
    ch.dren = ifelse(is.infinite(ch.dren), NA, ch.dren),
    ch.elec = ifelse(is.infinite(ch.elec), NA, ch.elec)
  )  %>% 
  ungroup() %>% 
  mutate(
    state = str_pad(state, width = 2, side = "left", pad = "0"),
    edo.year = paste(state, year, sep = "_")
  ) %>% 
  left_join(calendario)
  
nrow(try)
##Con esta transformación me quedan 10,587 observaciones
#despues del filter son 10,230

write.csv(try, paste(out, "final_original.csv", sep = "/"), row.names = F)


# lead --------------------------------------------------------------------

dada <- ele %>% 
  left_join(servicios, by = "muniYear") %>% 
  left_join(crimen, by = "muniYear") %>% 
  #13,373
  left_join(mar, by = "muniYear") %>% 
  #11,142 observaciones
  left_join(pob, by = "muniYear")

summary(dada)

dada <- dada %>% 
  mutate(
    year = as.numeric(sapply(strsplit(muniYear, "_"), "[", 2)),
    trienio_inicio = ifelse(!is.na(muni), year, NA)
  ) %>% 
  fill(trienio_inicio, .direction = "up") %>% 
  group_by(trienio_inicio) %>% 
  mutate(
    n_agua = ifelse(is.na(agua), 1, 0),
    n_dren = ifelse(is.na(dren), 1, 0),
    n_elec = ifelse(is.na(elec), 1, 0),
    n_hom = ifelse(is.na(hom), 1, 0),
  ) %>% 
  ungroup() %>% 
  mutate(
    inc.ch = inc.ch*100,
    t.agua = (agua/POB_TOT) * 100000,
    t.dren = (dren/POB_TOT) * 100000,
    t.elec = (elec/POB_TOT) * 100000,
    t.hom = (hom/POB_TOT) * 100000
  ) %>% 
  group_by(muni) %>% 
  mutate(
    lt.agua = lag(t.agua, n = 1, order_by = year),
    lt.dren = lag(t.dren, n = 1, order_by = year),
    lt.elec = lag(t.elec, n = 1, order_by = year), 
    lt.hom = lag(t.hom, n = 1, order_by = year),
    
    #Valor presente - valor pasado / valor pasado
    
    ch.agua = ifelse(!is.na(lt.agua) & !is.na(t.agua), ((t.agua - lt.agua) * 100)/lt.agua, NA),
    ch.dren = ifelse(!is.na(lt.dren) & !is.na(t.dren), ((t.dren - lt.dren) * 100)/lt.dren, NA),
    ch.elec = ifelse(!is.na(lt.elec) & !is.na(t.elec), ((t.elec - lt.elec) * 100)/lt.elec, NA),
    ch.hom = t.hom - lt.hom,
    
    ch.agua = ifelse(is.infinite(ch.agua), NA, ch.agua),
    ch.dren = ifelse(is.infinite(ch.dren), NA, ch.dren),
    ch.elec = ifelse(is.infinite(ch.elec), NA, ch.elec)
  )  %>% 
  ungroup() %>% 
  mutate(
    state = str_pad(state, width = 2, side = "left", pad = "0"),
    edo.year = paste(state, year, sep = "_")
  ) %>% 
  left_join(calendario)

nrow(dada)
##Con esta transformación me quedan 10,587 observaciones
#despues del filter son 10,230

write.csv(dada, paste(out, "final_original_lead.csv", sep = "/"), row.names = F)
