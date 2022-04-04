library(stargazer)
library(openxlsx)
library(writexl)
library(janitor)
library(ggplot2)
library(Hmisc)
library(dplyr)
library(tidyr)
rm(list = ls())

inp <- getwd()
out <- "./tables"
plot <- "./plots"

data <- read.csv(paste(inp, "final_original_lead.csv", sep = "/"))

ele_ser <- data %>% 
  mutate(
    n.agua = ifelse(!is.na(ch.agua), 1, 0),
    n.dren = ifelse(!is.na(ch.dren), 1, 0),
    n.elec = ifelse(!is.na(ch.elec), 1, 0),
    n.hom = ifelse(!is.na(ch.hom), 1, 0),
    elecciones = ifelse(!is.na(inc_top), 1, 0)
  ) %>% 
  group_by(state) %>% 
  summarise(
    elecciones = sum(elecciones),
    agua = sum(n.agua),
    dren = sum(n.dren),
    elec = sum(n.elec),
    hom = sum(n.hom),
  ) %>% 
  ungroup()

write.xlsx(ele_ser, file = paste(out, "Eleciones y servicios por estado.xlsx", sep = "/"))

mun_edo_party <- data %>% 
  filter(!is.na(inc_top)) %>% 
  group_by(state) %>% 
  count(inc_top) %>% 
  pivot_wider(
    names_from = inc_top,
    values_from = n
  )

write.xlsx(mun_edo_party, paste(out, "partidos por estado.xlsx", sep = "/"))

party <- c("PAN" = "#153588", "PRI" = "#E13A27", 
           "PRD" = "#F6D626", "otros" = "#000000")

party_series <- data %>% 
  filter(!is.na(inc_top)) %>% 
  group_by(year) %>% 
  count(inc_top) %>% 
  mutate(
    total = sum(n),
    porcentaje = n/total
  )

ggplot(party_series, 
       aes(x = year, y = porcentaje, group = inc_top)) +
  geom_line(aes(color = inc_top), size = 2) +
  geom_point(aes(color = inc_top), shape = 2, size = 4) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = (1994:2015)) +
  scale_colour_manual(values = party, name = "Partido") +
  labs(title = "Municipios gobernados por partido",
       subtitle = "Como porcentaje por año",
       x = "", y = "") +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 23)
    #axis.text.x = element_text(size = 14)
  )
ggsave("municipios gobernados.png", 
       path = plot,
       dpi = 300,
       height = 6,
       width = 14
)

party_series_state <- data %>% 
  filter(!is.na(inc_top)) %>% 
  group_by(state, year) %>% 
  count(inc_top)


ele_ser %>% 
  mutate(
    agua = agua*100/elecciones,
    dren = dren*100/elecciones,
    elec = elec*100/elecciones,
    hom = hom*100/elecciones
  ) %>% 
  mutate(
    prom = rowMeans(.[3:6])
  ) %>% 
  filter(prom >= 80)

data2 <- data %>% 
  select(
    muniYear, state, muni, year, wintop_state, win_top, inc_top, conco,        
    inc.ch, IM, POB_TOT, ch.agua, ch.dren, ch.elec,  ch.hom, alt, edo.year,
    d_gob, d_dipl, d_dipf, d_sen, d_pres
  )

tabla <- data2 %>% 
  select(
    IM, POB_TOT,
    inc.ch, ch.agua, ch.dren, ch.elec, ch.hom     
  ) %>% 
  rename(POB = POB_TOT) %>% 
  stargazer(
    type = "html",
    title = "Estadística descriptiva",
    column.labels = c("holi", "Observaciones", "Promedio",
                      "Desviación estándar", "Mínimo", "Máximo"),
    covariate.labels = c("Índice de marginación",
                         "Población", 
                         "Cambio de porcentaje de votos al Incumbent",
                         "Cambio porcentual de tomas de agua",
                         "Cambio porcentual de tomas de drenaje",
                         "Cambio porcentual de tomas de electricidad",
                         "Cambio porcentual de carpetas de homicidios"),
    omit.summary.stat = c("p25", "p75"),
    digits = 2,
    out = paste(out, "Descriptivos_total.html", sep = "/")
  )

corr <- data2 %>% 
  select(
    alt, inc.ch, ch.agua, ch.dren, ch.elec, ch.hom, IM, POB_TOT, 
    conco, d_gob, d_dipl, d_dipf, d_sen, d_pres
  ) %>% 
  rename(pob = POB_TOT, im = IM)

cor <- rcorr(as.matrix(corr), type = "pearson")

wb <- createWorkbook()
sh <- addWorksheet(wb, "Correlacion")
writeData(wb, sh, cor[[1]])

sh <- addWorksheet(wb, "Observaciones")
writeData(wb, sh, cor[[2]])

sh <- addWorksheet(wb, "valores p")
writeData(wb, sh, cor[[3]])

saveWorkbook(wb, paste(out, "Correlaciones_lead.xlsx", sep = "/"), overwrite = T) 


cor1 <- as.data.frame(cor$n) %>% 
  stargazer(summary = F, 
            rownames = F, 
            type = "html",
            title = "Matriz de correlaciones",
            out = paste(out, "observaciones_lead.html", sep = "/"))

cor2 <- round(as.data.frame(cor$r)) %>% 
  stargazer(summary = F, 
            rownames = F, 
            type = "html",
            title = "Matriz de correlaciones",
            out = paste(out, "matriz_correlaciones_lead.html", sep = "/"))

# 5% de cada extremo ------------------------------------------------------

data3 <- data %>% 
  mutate(
    ch.agua = ifelse(ch.agua <= quantile(ch.agua, 0.975, na.rm = T) & 
                       ch.agua >= quantile(ch.agua, 0.025, na.rm = T), ch.agua, NA),
    
    ch.dren = ifelse(ch.dren <= quantile(ch.dren, 0.975, na.rm = T) &
                       ch.dren >= quantile(ch.dren, 0.025, na.rm = T) , ch.dren, NA),
    
    ch.elec = ifelse(ch.elec <= quantile(ch.elec, 0.975, na.rm = T) &
                       ch.elec >= quantile(ch.elec, 0.025, na.rm = T), ch.elec, NA),
    
    ch.hom = ifelse(ch.hom <= quantile(ch.hom, 0.975, na.rm = T) & 
                      ch.hom >= quantile(ch.hom, 0.025, na.rm = T), ch.hom, NA)
  )

tabla2 <- data3 %>% 
  select(
    IM, POB_TOT,
    inc.ch, ch.agua, ch.dren, ch.elec,ch.hom, 
    d_gob, d_dipl, d_dipf, d_sen, d_pres
  ) %>% 
  rename(POB = POB_TOT) %>% 
  stargazer(
    type = "html",
    title = "Estadística descriptiva",
    column.labels = c("holi", "Observaciones", "Promedio",
                      "Desviación estándar", "Mínimo", "Máximo"),
    covariate.labels = c("Índice de marginación",
                         "Población", 
                         "Cambio de porcentaje de votos al Incumbent",
                         "Cambio porcentual de tomas de agua",
                         "Cambio porcentual de tomas de drenaje",
                         "Cambio porcentual de tomas de electricidad",
                         "Cambio porcentual de carpetas de delitos",
                         "Cambio porcentual de carpetas de homicidios"),
    omit.summary.stat = c("p25", "p75"),
    digits = 2,
    out = paste(out, "Descriptivos_95%.html", sep = "/")
  )

corr2 <- data3 %>% 
  select(
    alt, inc.ch, ch.agua, ch.dren, ch.elec,  ch.hom, IM, POB_TOT,
    d_gob, d_dipl, d_dipf, d_sen, d_pres
  ) %>% 
  rename(pob = POB_TOT, im = IM)

cor <- rcorr(as.matrix(corr2), type = "pearson")

wb <- createWorkbook()
sh <- addWorksheet(wb, "Correlacion")
writeData(wb, sh, cor[[1]])

sh <- addWorksheet(wb, "Observaciones")
writeData(wb, sh, cor[[2]])

sh <- addWorksheet(wb, "valores p")
writeData(wb, sh, cor[[3]])

saveWorkbook(wb, paste(out, "Correlaciones_lead_95.xlsx", sep = "/"), overwrite = T) 


cor1 <- as.data.frame(cor$n) %>% 
  stargazer(summary = F, 
            rownames = F, 
            type = "html",
            title = "Matriz de correlaciones",
            out = paste(out, "observaciones_lead_95.html", sep = "/"))

cor2 <- round(as.data.frame(cor$r)) %>% 
  stargazer(summary = F, 
            rownames = F, 
            type = "html",
            title = "Matriz de correlaciones",
            out = paste(out, "matriz_correlaciones_lead_95.html", sep = "/"))
