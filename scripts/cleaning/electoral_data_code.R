library(dplyr)
library(stringr)
rm(list = ls())

inp <- "./datos"
list.files(inp)
out <- "./databases"

data <- read.csv(paste(inp, "data.csv", sep = "/")) %>% 
  select(-c(PRI_ofi, PAN_ofi, PRD_ofi, win, win_top, conco))
  
data <- data %>% 
  mutate(
    win = as.character(Winner2),
    win = sapply(strsplit(win, "_"), "[", 1),
    win_top = case_when(
      win == "PRI" ~ "PRI",
      win == "PAN" ~ "PAN",
      win == "PRD" ~ "PRD", 
      T ~ "otros")
  ) %>% 
  group_by(muni) %>% 
  mutate(
    inc = lag(win, n = 1, order_by = year),
    inc_top = lag(win_top, n = 1, order_by = year)
  ) %>% 
  ungroup() %>% 
  mutate(
    alt = ifelse(win != inc, 1, 0),
    PAN.s = PAN/total,
    PRI.s = PRI/total,
    PRD.s = PRD/total
  )

data <- data %>% 
  group_by(muni) %>% 
  mutate(
    lag_PAN.s = lag(PAN.s, n = 1, order_by = year),
    lag_PRI.s = lag(PRI.s, n = 1, order_by = year),
    lag_PRD.s = lag(PRD.s, n = 1, order_by = year)
  ) %>% 
  ungroup() %>% 
  mutate(
    PAN.ch = PAN.s - lag_PAN.s,
    PRI.ch = PRI.s - lag_PRI.s,
    PRD.ch = PRD.s - lag_PRD.s,
    
    inc.ch = ifelse(inc_top == "PAN", PAN.ch,
                    ifelse(inc_top == "PRI", PRI.ch,
                           ifelse(inc_top == "PRD", PRD.ch, NA))),
    
    conco = ifelse(win_top == wintop_state, 1, 0)
  )

write.csv(data, paste(out, "Electoral.csv", sep = "/"), row.names = F)
