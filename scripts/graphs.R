library(dplyr)
library(scales)
library(ggplot2)
rm(list = ls())

inp <- getwd()
list.files(inp)
out <- "./plots"

options(scipen=999)

data <- read.csv(paste(inp, "final_original_lead.csv", sep = "/"))
nrow(data) #17,023
data <- data %>% 
  filter(!is.na(inc_top) & 
           inc_top != "otros"
  )
nrow(data)
#16951
summary(data)

party <- c("PAN" = "#153588", "PRI" = "#E13A27", 
           "PRD" = "#F6D626", "otros" = "#000000")

# 1. Todas las observaciones -------------------------------------------------

# 1.1 Diagramas de densidad ---------------------------------------------------

ggplot(data, aes(x = inc.ch)) +
  geom_density() +
  labs(title = "Diagrama de densidad",
       subtitle = "Cambio en % de votos al Incumbent",
       x = "", y = "") +
  theme_classic()
ggsave("densidad_incumbent.png", 
       path = paste(out, "density", sep = "/"),
       dpi = 300)

ggplot(data, aes(x = ch.agua)) +
  geom_density() +
  labs(title = "Diagrama de densidad",
       subtitle = "Cambio en tomas de agua por 100k",
       x = "", y = "") +
  theme_classic()
ggsave("densidad_agua.png", 
       path = paste(out, "density", sep = "/"),
       dpi = 300)

ggplot(data, aes(x = ch.dren)) +
  geom_density() +
  labs(title = "Diagrama de densidad",
       subtitle = "Cambio en drenaje por cada 100k",
       x = "", y = "") +
  theme_classic()
ggsave("densidad_drenaje.png", 
       path = paste(out, "density", sep = "/"),
       dpi = 300)

ggplot(data, aes(x = ch.elec)) +
  geom_density() +
  labs(title = "Diagrama de densidad",
       subtitle = "Cambio en tomas de electricidad por cada 100k",
       x = "", y = "") +
  theme_classic()
ggsave("densidad_electricidad.png", 
       path = paste(out, "density", sep = "/"),
       dpi = 300)

# ggplot(data, aes(x = ch.del)) +
#   geom_density() +
#   labs(title = "Diagrama de densidad",
#        subtitle = "Cambio en total de delitos por cada 100k",
#        x = "", y = "") +
#   theme_classic()
# ggsave("densidad_delitos.png", 
#        path = paste(out, "density", sep = "/"),
#        dpi = 300)

ggplot(data, aes(x = ch.hom)) +
  geom_density() +
  labs(title = "Diagrama de densidad",
       subtitle = "Cambio en homicidios por cada 100k",
       x = "", y = "") +
  theme_classic()
ggsave("densidad_homicidios.png", 
       path = paste(out, "density", sep = "/"),
       dpi = 300)


# 1.2 Boxplots ----------------------------------------------------------------

ggplot(data, aes(x = inc_top, y = ch.agua)) +
  geom_boxplot() +
  geom_hline(yintercept = mean(data$ch.agua, na.rm = T), color = "red") +
  labs(title = "Boxplot por partido",
       subtitle = "Diferencia en tasa de agua",
       x = "", y = "") +
  theme_classic()
ggsave("boxplot_agua.png", 
       path = paste(out, "boxplot", sep = "/"),
       dpi = 300)

ggplot(data, aes(x = inc_top, y = ch.dren)) +
  geom_boxplot() +
  geom_hline(yintercept = mean(data$ch.dren, na.rm = T), color = "red") +
  labs(title = "Boxplot por partido",
       subtitle = "Diferencia en tasa de drenaje",
       x = "", y = "") +
  theme_classic()
ggsave("boxplot_drenaje.png", 
       path = paste(out, "boxplot", sep = "/"),
       dpi = 300)

ggplot(data, aes(x = inc_top, y = ch.elec)) +
  geom_boxplot() +
  geom_hline(yintercept = mean(data$ch.elec, na.rm = T), color = "red") +
  labs(title = "Boxplot por partido",
       subtitle = "Diferencia en tasa de electricidad",
       x = "", y = "") +
  theme_classic()
ggsave("boxplot_eelctricidad.png", 
       path = paste(out, "boxplot", sep = "/"),
       dpi = 300)

# ggplot(data, aes(x = inc_top, y = ch.del)) +
#   geom_boxplot() +
#   geom_hline(yintercept = mean(data$ch.del, na.rm = T), color = "red") +
#   labs(title = "Boxplot por partido",
#        subtitle = "Diferencia en tasa de delitos",
#        x = "", y = "") +
#   theme_classic()
# ggsave("boxplot_delitos.png", 
#        path = paste(out, "boxplot", sep = "/"),
#        dpi = 300)

ggplot(data, aes(x = inc_top, y = ch.hom)) +
  geom_boxplot() +
  geom_hline(yintercept = mean(data$ch.hom, na.rm = T), color = "red") +
  labs(title = "Boxplot por partido",
       subtitle = "Diferencia en tasa de homicidios",
       x = "", y = "") +
  theme_classic()
ggsave("boxplot_homicidios.png", 
       path = paste(out, "boxplot", sep = "/"),
       dpi = 300)


ggplot(data, aes(x = as.factor(alt), y = ch.agua)) +
  geom_boxplot() +
  labs(title = "Boxplot por alternancia",
       subtitle = "Diferencia en tasa de agua",
       x = "", y = "")

ggplot(data, aes(x = as.factor(alt), y = ch.dren)) +
  geom_boxplot() +
  labs(title = "Boxplot por alternancia",
       subtitle = "Diferencia en tasa de drenaje",
       x = "", y = "")

ggplot(data, aes(x = as.factor(alt), y = ch.elec)) +
  geom_boxplot() +
  labs(title = "Boxplot por alternancia",
       subtitle = "Diferencia en tasa de electricidad",
       x = "", y = "")

# ggplot(data, aes(x = as.factor(alt), y = ch.del)) +
#   geom_boxplot() +
#   labs(title = "Boxplot por alternancia",
#        subtitle = "Diferencia en tasa de delitos",
#        x = "", y = "")

ggplot(data, aes(x = as.factor(alt), y = ch.hom)) +
  geom_boxplot() +
  labs(title = "Boxplot por alternancia",
       subtitle = "Diferencia en tasa de homicidios",
       x = "", y = "")

##Hay un cchingo de outliers

agua <- quantile(data$ch.agua, prob = seq(0, 1, length = 11), na.rm = T)
agua <- as.data.frame(agua)

dren <- quantile(data$ch.dren, prob = seq(0, 1, length = 11), na.rm = T)
dren <- as.data.frame(dren)

elec <- quantile(data$ch.elec, prob = seq(0, 1, length = 11), na.rm = T)
elec <- as.data.frame(elec)

# del <- quantile(data$ch.del, prob = seq(0, 1, length = 11), na.rm = T)
# del <- as.data.frame(del)

hom <- quantile(data$ch.hom, prob = seq(0, 1, length = 11), na.rm = T)
hom <- as.data.frame(hom)

cuant <- bind_cols(agua, dren, elec, #del, 
                   hom) 

cuant <- cuant %>% 
  mutate(Porcentaje = paste0(seq(0, 1, length = 11)*100, "%")) %>% 
  select(Porcentaje, agua, dren, elec, #del, 
         hom)

# 1.3 Scatter -------------------------------------------------------------

ggplot(data, aes(x = ch.agua, y = inc.ch, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F, color = "black") +   scale_y_continuous(limits = c(-100, 100)) +
  scale_y_continuous(limits = c(-100, 100)) +
  # scale_x_continuous(labels = percent_format(1L)) +
  labs(title = "Diagrama de dispersión de agua", subtitle = "Todas las observaciones", 
       x = "Cambio % de agua", y = "Cambio % de votos al incumbent") +
  theme_classic() +
  facet_wrap(. ~ inc_top) +
  theme(legend.position = "null")
ggsave("point_ch.agua_tot.png", path = paste(out, "scatter", sep = "/"), dpi = 300)

ggplot(data, aes(x = ch.dren, y = inc.ch, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F, color = "black") +   scale_y_continuous(limits = c(-100, 100)) +
  # scale_x_continuous(labels = percent_format(1L)) +
  # scale_y_continuous(labels = percent_format(1L)) +
  labs(title = "Diagrama de dispersión de drenaje", subtitle = "Todas las observaciones", 
       x = "Cambio % de drenaje", y = "Cambio % de votos al incumbent") +
  theme_classic() +
  facet_wrap(. ~ inc_top) +
  theme(legend.position = "null")
ggsave("point_ch.dren_tot.png", path = paste(out, "scatter", sep = "/"), dpi = 300)

ggplot(data, aes(x = ch.elec, y = inc.ch, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F, color = "black") +   scale_y_continuous(limits = c(-100, 100)) +
  scale_x_continuous(labels = percent_format(1L)) +
  scale_y_continuous(labels = percent_format(1L)) +
  labs(title = "Diagrama de dispersión de electricidad", subtitle = "Todas las observaciones", 
       x = "Cambio % de electricidad", y = "Cambio % de votos al incumbent") +
  theme_classic() +
  facet_wrap(. ~ inc_top) +
  theme(legend.position = "null")
ggsave("point_ch.elec_tot.png", path = paste(out, "scatter", sep = "/"), dpi = 300)

# ggplot(data, aes(x = ch.del, y = inc.ch, col = inc_top)) +
#   geom_jitter(alpha = 0.3) +
#   scale_colour_manual(values = party, name = "Partido") +
#   geom_smooth(method = "lm", se = F, color = "black") +   scale_y_continuous(limits = c(-100, 100)) +
#   # scale_x_continuous(labels = percent_format(1L)) +
#   # scale_y_continuous(labels = percent_format(1L)) +
#   labs(title = "Diagrama de dispersión de delitos", subtitle = "Todas las observaciones", 
#        x = "Cambio % de delitos", y = "Cambio % de votos al incumbent") +
#   theme_classic() +
#   facet_wrap(. ~ inc_top) +
#   theme(legend.position = "null")
# ggsave("point_ch.del_tot.png", path = paste(out, "scatter", sep = "/"), dpi = 300)

ggplot(data, aes(x = ch.hom, y = inc.ch, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F, color = "black") +   scale_y_continuous(limits = c(-100, 100)) +
  # scale_x_continuous(labels = percent_format(1L)) +
  # scale_y_continuous(labels = percent_format(1L)) +
  labs(title = "Diagrama de dispersión de homicidios", subtitle = "Todas las observaciones", 
       x = "Cambio % de homicidios", y = "Cambio % de votos al incumbent") +
  theme_classic() +
  facet_wrap(. ~ inc_top) +
  theme(legend.position = "null")
ggsave("point_ch.hom_tot.png", path = paste(out, "scatter", sep = "/"), dpi = 300)

# 2. Capped -----------------------------------------------------------

##So... qué vamos a quitar? 
#Empecemos con el 10% superior e inferior

data1 <- data %>% 
  mutate(
    ch.agua = ifelse(ch.agua > 100, 100, ch.agua),
    ch.dren = ifelse(ch.agua > 100, 100, ch.dren),
    ch.elec = ifelse(ch.agua > 100, 100, ch.elec),
    
    # ch.del = ifelse(ch.del < -10000, -10000,
    #                 ifelse(ch.del > 10000, 10000, ch.del)),
    # 
    # ch.del = ifelse(ch.del < -10000, -10000,
    #                 ifelse(ch.del > 10000, 10000, ch.del))
  )

summary(data1)

# 2.1 Diagramas de densidad ---------------------------------------------------

ggplot(data1, aes(x = inc.ch)) +
  geom_density() +
  labs(title = "Diagrama de densidad",
       subtitle = "Cambio en % de votos al Incumbent",
       x = "", y = "") +
  theme_classic()
ggsave("primer_densidad_incumbent.png", 
       path = paste(out, "density", sep = "/"),
       dpi = 300)

ggplot(data1, aes(x = ch.agua)) +
  geom_density() +
  labs(title = "Diagrama de densidad",
       subtitle = "Cambio en tomas de agua por 100k",
       x = "", y = "") +
  theme_classic()
ggsave("primer_densidad_agua.png", 
       path = paste(out, "density", sep = "/"),
       dpi = 300)

ggplot(data1, aes(x = ch.dren)) +
  geom_density() +
  labs(title = "Diagrama de densidad",
       subtitle = "Cambio en drenaje por cada 100k",
       x = "", y = "") +
  theme_classic()
ggsave("primer_densidad_drenaje.png", 
       path = paste(out, "density", sep = "/"),
       dpi = 300)

ggplot(data1, aes(x = ch.elec)) +
  geom_density() +
  labs(title = "Diagrama de densidad",
       subtitle = "Cambio en tomas de electricidad por cada 100k",
       x = "", y = "") +
  theme_classic()
ggsave("primer_densidad_electricidad.png", 
       path = paste(out, "density", sep = "/"),
       dpi = 300)

# ggplot(data1, aes(x = ch.del)) +
#   geom_density() +
#   labs(title = "Diagrama de densidad",
#        subtitle = "Cambio en total de delitos por cada 100k",
#        x = "", y = "") +
#   theme_classic()
# ggsave("primer_densidad_delitos.png", 
#        path = paste(out, "density", sep = "/"),
#        dpi = 300)

ggplot(data1, aes(x = ch.hom)) +
  geom_density() +
  labs(title = "Diagrama de densidad",
       subtitle = "Cambio en homicidios por cada 100k",
       x = "", y = "") +
  theme_classic()
ggsave("primer_densidad_homicidios.png", 
       path = paste(out, "density", sep = "/"),
       dpi = 300)


#2.2 Boxplots ----------------------------------------------------------------

ggplot(data1, aes(x = inc_top, y = ch.agua)) +
  geom_boxplot() +
  geom_hline(yintercept = mean(data1$ch.agua, na.rm = T), color = "red") +
  labs(title = "Boxplot por partido",
       subtitle = "Diferencia en tasa de agua",
       x = "", y = "") +
  theme_classic()
ggsave("primer_boxplot_agua.png", 
       path = paste(out, "boxplot", sep = "/"),
       dpi = 300)

ggplot(data1, aes(x = inc_top, y = ch.dren)) +
  geom_boxplot() +
  geom_hline(yintercept = mean(data1$ch.dren, na.rm = T), color = "red") +
  labs(title = "Boxplot por partido",
       subtitle = "Diferencia en tasa de drenaje",
       x = "", y = "") +
  theme_classic()
ggsave("primer_boxplot_drenaje.png", 
       path = paste(out, "boxplot", sep = "/"),
       dpi = 300)

ggplot(data1, aes(x = inc_top, y = ch.elec)) +
  geom_boxplot() +
  geom_hline(yintercept = mean(data1$ch.elec, na.rm = T), color = "red") +
  labs(title = "Boxplot por partido",
       subtitle = "Diferencia en tasa de electricidad",
       x = "", y = "") +
  theme_classic()
ggsave("primer_boxplot_electricidad.png", 
       path = paste(out, "boxplot", sep = "/"),
       dpi = 300)

# ggplot(data1, aes(x = inc_top, y = ch.del)) +
#   geom_boxplot() +
#   geom_hline(yintercept = mean(data1$ch.del, na.rm = T), color = "red") +
#   labs(title = "Boxplot por partido",
#        subtitle = "Diferencia en tasa de delitos",
#        x = "", y = "") +
#   theme_classic()
# ggsave("primer_boxplot_delitos.png", 
#        path = paste(out, "boxplot", sep = "/"),
#        dpi = 300)

ggplot(data1, aes(x = inc_top, y = ch.hom)) +
  geom_boxplot() +
  geom_hline(yintercept = mean(data1$ch.hom, na.rm = T), color = "red") +
  labs(title = "Boxplot por partido",
       subtitle = "Diferencia en tasa de homicidios",
       x = "", y = "") +
  theme_classic()
ggsave("primer_boxplot_homicidios.png", 
       path = paste(out, "boxplot", sep = "/"),
       dpi = 300)


ggplot(data1, aes(x = as.factor(alt), y = ch.agua)) +
  geom_boxplot() +
  labs(title = "Boxplot por alternancia",
       subtitle = "Diferencia en tasa de agua",
       x = "", y = "") +
  theme_classic()

ggplot(data1, aes(x = as.factor(alt), y = ch.dren)) +
  geom_boxplot() +
  labs(title = "Boxplot por alternancia",
       subtitle = "Diferencia en tasa de drenaje",
       x = "", y = "") +
  theme_classic()

ggplot(data1, aes(x = as.factor(alt), y = ch.elec)) +
  geom_boxplot() +
  labs(title = "Boxplot por alternancia",
       subtitle = "Diferencia en tasa de electricidad",
       x = "", y = "") +
  theme_classic()

# ggplot(data1, aes(x = as.factor(alt), y = ch.del)) +
#   geom_boxplot() +
#   labs(title = "Boxplot por alternancia",
#        subtitle = "Diferencia en tasa de delitos",
#        x = "", y = "") +
#   theme_classic()

ggplot(data1, aes(x = as.factor(alt), y = ch.hom)) +
  geom_boxplot() +
  labs(title = "Boxplot por alternancia",
       subtitle = "Diferencia en tasa de homicidios",
       x = "", y = "") +
  theme_classic()


# 2.3 Scatter -------------------------------------------------------------

ggplot(data1, aes(x = ch.agua, y = inc.ch, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F, color = "black") +   scale_y_continuous(limits = c(-100, 100)) +
  # scale_x_continuous(labels = percent_format(1L)) +
  # scale_y_continuous(labels = percent_format(1L)) +
  labs(title = "Diagrama de dispersión de agua", subtitle = "Todas las observaciones", 
       x = "Cambio % de agua", y = "Cambio % de votos al incumbent") +
  facet_wrap(. ~ inc_top) +
  theme_classic() +
  theme(legend.position = "null")
ggsave("primer_point_ch.agua_tot.png", path = paste(out, "scatter", sep = "/"), dpi = 300)

ggplot(data1, aes(x = ch.dren, y = inc.ch, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F, color = "black") +   scale_y_continuous(limits = c(-100, 100)) +
  # scale_x_continuous(labels = percent_format(1L)) +
  # scale_y_continuous(labels = percent_format(1L)) +
  labs(title = "Diagrama de dispersión de drenaje", subtitle = "Todas las observaciones", 
       x = "Cambio en la tasa de drenaje", y = "Cambio % de votos al incumbent") +
  facet_wrap(. ~ inc_top) +
  theme_classic() +
  theme(legend.position = "null")
ggsave("primer_point_ch.dren_tot.png", path = paste(out, "scatter", sep = "/"), dpi = 300)

ggplot(data1, aes(x = ch.elec, y = inc.ch, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F, color = "black") +   scale_y_continuous(limits = c(-100, 100)) +
  # scale_x_continuous(labels = percent_format(1L)) +
  # scale_y_continuous(labels = percent_format(1L)) +
  labs(title = "Diagrama de dispersión de electricidad", subtitle = "Todas las observaciones", 
       x = "Cambio % de electricidad", y = "Cambio % de votos al incumbent") +
  facet_wrap(. ~ inc_top) +
  theme_classic() +
  theme(legend.position = "null")
ggsave("primer_point_ch.elec_tot.png", path = paste(out, "scatter", sep = "/"), dpi = 300)

# ggplot(data1, aes(x = ch.del, y = inc.ch, col = inc_top)) +
#   geom_jitter(alpha = 0.3) +
#   scale_colour_manual(values = party, name = "Partido") +
#   geom_smooth(method = "lm", se = F, color = "black") +   scale_y_continuous(limits = c(-100, 100)) +
#   # scale_x_continuous(labels = percent_format(1L)) +
#   # scale_y_continuous(labels = percent_format(1L)) +
#   labs(title = "Diagrama de dispersión de delitos", subtitle = "Todas las observaciones", 
#        x = "Cambio en la tasa de delitos", y = "Cambio % de votos al incumbent") +
#   facet_wrap(. ~ inc_top) +
#   theme_classic() +
#   theme(legend.position = "null")
# ggsave("primer_point_ch.del_tot.png", path = paste(out, "scatter", sep = "/"), dpi = 300, 
#        width = 6, height = 5.21)

ggplot(data1, aes(x = ch.hom, y = inc.ch, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F, color = "black") +   scale_y_continuous(limits = c(-100, 100)) +
  # scale_x_continuous(labels = percent_format(1L)) +
  # scale_y_continuous(labels = percent_format(1L)) +
  labs(title = "Diagrama de dispersión de homicidios", subtitle = "Todas las observaciones", 
       x = "Cambio en la tasa de homicidios", y = "Cambio % de votos al incumbent") +
  facet_wrap(. ~ inc_top) +
  theme_classic() +
  theme(legend.position = "null")
ggsave("primer_point_ch.hom_tot.png", path = paste(out, "scatter", sep = "/"), dpi = 300)

ggplot(data1, aes(x = IM, y = inc.ch, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F, color = "black") +   scale_y_continuous(limits = c(-100, 100)) +
  # scale_x_continuous(labels = percent_format(1L)) +
  # scale_y_continuous(labels = percent_format(1L)) +
  labs(title = "Diagrama de dispersión del Índice de Marginación", #subtitle = "Todas las observaciones", 
       x = "", y = "Cambio % de votos al incumbent") +
  facet_wrap(. ~ inc_top) +
  theme_classic() +
  theme(legend.position = "null", panel.spacing.x = unit(6, "mm"))
ggsave("primer_point_IM_tot_cap.png", path = paste(out, "scatter", sep = "/"), dpi = 300, height = 5.21, width = 6)



# 3. Cropped -----------------------------------------------------------

data2 <- data %>% 
  mutate(
    ch.agua = ifelse(ch.agua <= quantile(ch.agua, 0.975, na.rm = T) & 
                       ch.agua >= quantile(ch.agua, 0.025, na.rm = T), ch.agua, NA),
    
    ch.dren = ifelse(ch.dren <= quantile(ch.dren, 0.975, na.rm = T) &
                       ch.dren >= quantile(ch.dren, 0.025, na.rm = T) , ch.dren, NA),
    
    ch.elec = ifelse(ch.elec <= quantile(ch.elec, 0.975, na.rm = T) &
                       ch.elec >= quantile(ch.elec, 0.025, na.rm = T), ch.elec, NA),
    
    # ch.del = ifelse(ch.del <= quantile(ch.del, 0.975, na.rm = T) &
    #                   ch.del >= quantile(ch.del, 0.025, na.rm = T), ch.del, NA),
    
    ch.hom = ifelse(ch.hom <= quantile(ch.hom, 0.975, na.rm = T) & 
                      ch.hom >= quantile(ch.hom, 0.025, na.rm = T), ch.hom, NA)
  )


nrow(data2)
summary(data2)

# 3.1 Diagramas de densidad ---------------------------------------------------

ggplot(data2, aes(x = inc.ch)) +
  geom_density() +
  labs(title = "Diagrama de densidad",
       subtitle = "Cambio en % de votos al Incumbent",
       x = "", y = "") + 
  theme_classic()
ggsave("segundo_densidad_incumbent_crop.png", 
       path = paste(out, "density", sep = "/"),
       dpi = 300)

ggplot(data2, aes(x = ch.agua)) +
  geom_density() +
  labs(title = "Diagrama de densidad",
       subtitle = "Cambio en tomas de agua por 100k",
       x = "", y = "") + 
  theme_classic()
ggsave("segundo_densidad_agua_crop.png", 
       path = paste(out, "density", sep = "/"),
       dpi = 300)

ggplot(data2, aes(x = ch.dren)) +
  geom_density() +
  labs(title = "Diagrama de densidad",
       subtitle = "Cambio en drenaje por cada 100k",
       x = "", y = "") + 
  theme_classic()
ggsave("segundo_densidad_drenaje_crop.png", 
       path = paste(out, "density", sep = "/"),
       dpi = 300)

ggplot(data2, aes(x = ch.elec)) +
  geom_density() +
  labs(title = "Diagrama de densidad",
       subtitle = "Cambio en tomas de electricidad por cada 100k",
       x = "", y = "") + 
  theme_classic()
ggsave("segundo_densidad_electricidad_crop.png", 
       path = paste(out, "density", sep = "/"),
       dpi = 300)

# ggplot(data2, aes(x = ch.del)) +
#   geom_density() +
#   labs(title = "Diagrama de densidad",
#        subtitle = "Cambio en total de delitos por cada 100k",
#        x = "", y = "") + 
#   theme_classic()
# ggsave("segundo_densidad_delitos_crop.png", 
#        path = paste(out, "density", sep = "/"),
#        dpi = 300)

ggplot(data2, aes(x = ch.hom)) +
  geom_density() +
  labs(title = "Diagrama de densidad",
       subtitle = "Cambio en homicidios por cada 100k",
       x = "", y = "") + 
  theme_classic()
ggsave("segundo_densidad_homicidios_crop.png", 
       path = paste(out, "density", sep = "/"),
       dpi = 300)


# 3.2 Boxplots ----------------------------------------------------------------

ggplot(data2, aes(x = inc_top, y = ch.agua)) +
  geom_boxplot() +
  geom_hline(yintercept = mean(data2$ch.agua, na.rm = T), color = "red") +
  labs(title = "Boxplot por partido",
       subtitle = "Diferencia en tasa de agua",
       x = "", y = "") + 
  theme_classic()
ggsave("segundo_boxplot_agua_crop.png", 
       path = paste(out, "boxplot", sep = "/"),
       dpi = 300)

ggplot(data2, aes(x = inc_top, y = ch.dren)) +
  geom_boxplot() +
  geom_hline(yintercept = mean(data2$ch.dren, na.rm = T), color = "red") +
  labs(title = "Boxplot por partido",
       subtitle = "Diferencia en tasa de drenaje",
       x = "", y = "") + 
  theme_classic()
ggsave("segundo_boxplot_drenaje_crop.png", 
       path = paste(out, "boxplot", sep = "/"),
       dpi = 300)

ggplot(data2, aes(x = inc_top, y = ch.elec)) +
  geom_boxplot() +
  geom_hline(yintercept = mean(data2$ch.elec, na.rm = T), color = "red") +
  labs(title = "Boxplot por partido",
       subtitle = "Diferencia en tasa de electricidad",
       x = "", y = "") + 
  theme_classic()
ggsave("segundo_boxplot_electricidad_crop.png", 
       path = paste(out, "boxplot", sep = "/"),
       dpi = 300)

# ggplot(data2, aes(x = inc_top, y = ch.del)) +
#   geom_boxplot() +
#   geom_hline(yintercept = mean(data2$ch.del, na.rm = T), color = "red") +
#   labs(title = "Boxplot por partido",
#        subtitle = "Diferencia en tasa de delitos",
#        x = "", y = "") + 
#   theme_classic()
# ggsave("segundo_boxplot_delitos_crop.png", 
#        path = paste(out, "boxplot", sep = "/"),
#        dpi = 300)

ggplot(data2, aes(x = inc_top, y = ch.hom)) +
  geom_boxplot() +
  geom_hline(yintercept = mean(data2$ch.hom, na.rm = T), color = "red") +
  labs(title = "Boxplot por partido",
       subtitle = "Diferencia en tasa de homicidios",
       x = "", y = "") + 
  theme_classic()
ggsave("segundo_boxplot_homicidios_crop.png", 
       path = paste(out, "boxplot", sep = "/"),
       dpi = 300)


ggplot(data2, aes(x = as.factor(alt), y = ch.agua)) +
  geom_boxplot() +
  labs(title = "Boxplot por alternancia",
       subtitle = "Diferencia en tasa de agua",
       x = "", y = "")

ggplot(data2, aes(x = as.factor(alt), y = ch.dren)) +
  geom_boxplot() +
  labs(title = "Boxplot por alternancia",
       subtitle = "Diferencia en tasa de drenaje",
       x = "", y = "")

ggplot(data2, aes(x = as.factor(alt), y = ch.elec)) +
  geom_boxplot() +
  labs(title = "Boxplot por alternancia",
       subtitle = "Diferencia en tasa de electricidad",
       x = "", y = "")

# ggplot(data2, aes(x = as.factor(alt), y = ch.del)) +
#   geom_boxplot() +
#   labs(title = "Boxplot por alternancia",
#        subtitle = "Diferencia en tasa de delitos",
#        x = "", y = "")

ggplot(data2, aes(x = as.factor(alt), y = ch.hom)) +
  geom_boxplot() +
  labs(title = "Boxplot por alternancia",
       subtitle = "Diferencia en tasa de homicidios",
       x = "", y = "")


# 3.3 Scatter -------------------------------------------------------------

ggplot(data2, aes(x = ch.agua, y = inc.ch, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F, color = "black") +   scale_y_continuous(limits = c(-100, 100)) +
  # scale_x_continuous(labels = percent_format(1L)) +
  # scale_y_continuous(labels = percent_format(1L)) +
  #ylim(-100, 100) +
  labs(title = "Diagrama de dispersión de agua", #subtitle = "Todas las observaciones", 
       x = "Cambio % de agua", y = "Cambio % de votos al incumbent") +
  facet_wrap(. ~ inc_top) +
  theme_classic() +
  theme(legend.position = "null", panel.spacing.x = unit(6, "mm"))
ggsave("segundo_point_ch.agua_tot_crop.png", path = paste(out, "scatter", sep = "/"), dpi = 300)

ggplot(data2, aes(x = ch.dren, y = inc.ch, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F, color = "black") +   scale_y_continuous(limits = c(-100, 100)) +
  # scale_x_continuous(labels = percent_format(1L)) +
  # scale_y_continuous(labels = percent_format(1L)) +
  labs(title = "Diagrama de dispersión de drenaje", #subtitle = "Todas las observaciones", 
       x = "Cambio % de drenaje", y = "Cambio % de votos al incumbent") +
  facet_wrap(. ~ inc_top) +
  theme_classic() +
  theme(legend.position = "null", panel.spacing.x = unit(6, "mm"))
ggsave("segundo_point_ch.dren_tot_crop.png", path = paste(out, "scatter", sep = "/"), dpi = 300)

ggplot(data2, aes(x = ch.elec, y = inc.ch, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F, color = "black") +   scale_y_continuous(limits = c(-100, 100)) +
  # scale_x_continuous(labels = percent_format(1L)) +
  # scale_y_continuous(labels = percent_format(1L)) +
  labs(title = "Diagrama de dispersión de electricidad", #subtitle = "Todas las observaciones", 
       x = "Cambio % de electricidad", y = "Cambio % de votos al incumbent") +
  facet_wrap(. ~ inc_top) +
  theme_classic() +
  theme(legend.position = "null", panel.spacing.x = unit(6, "mm"))
ggsave("segundo_point_ch.elec_tot_crop.png", path = paste(out, "scatter", sep = "/"), dpi = 300)

# ggplot(data2, aes(x = ch.del, y = inc.ch, col = inc_top)) +
#   geom_jitter(alpha = 0.3) +
#   scale_colour_manual(values = party, name = "Partido") +
#   geom_smooth(method = "lm", se = F, color = "black") +   scale_y_continuous(limits = c(-100, 100)) +
#   # scale_x_continuous(labels = percent_format(1L)) +
#   # scale_y_continuous(labels = percent_format(1L)) +
#   labs(title = "Diagrama de dispersión de delitos", #subtitle = "Todas las observaciones", 
#        x = "Cambio en la tasa de delitos", y = "Cambio % de votos al incumbent") +
#   facet_wrap(. ~ inc_top) +
#   theme_classic() +
#   theme(legend.position = "null", panel.spacing.x = unit(6, "mm"))
# ggsave("segundo_point_ch.del_tot_crop.png", path = paste(out, "scatter", sep = "/"), dpi = 300, height = 5.21, width = 6)

ggplot(data2, aes(x = ch.hom, y = inc.ch, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F, color = "black") +   scale_y_continuous(limits = c(-100, 100)) +
  # scale_x_continuous(labels = percent_format(1L)) +
  # scale_y_continuous(labels = percent_format(1L)) +
  labs(title = "Diagrama de dispersión de homicidios", #subtitle = "Todas las observaciones", 
       x = "Cambio en la tasa de homicidios", y = "Cambio % de votos al incumbent") +
  facet_wrap(. ~ inc_top) +
  theme_classic() +
  theme(legend.position = "null", panel.spacing.x = unit(6, "mm"))
ggsave("segundo_point_ch.hom_tot_crop.png", path = paste(out, "scatter", sep = "/"), dpi = 300)

ggplot(data2, aes(x = IM, y = inc.ch, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F, color = "black") +   scale_y_continuous(limits = c(-100, 100)) +
  # scale_x_continuous(labels = percent_format(1L)) +
  # scale_y_continuous(labels = percent_format(1L)) +
  labs(title = "Diagrama de dispersión del Índice de Marginación", #subtitle = "Todas las observaciones", 
       x = "", y = "Cambio % de votos al incumbent") +
  facet_wrap(. ~ inc_top) +
  theme_classic() +
  theme(legend.position = "null", panel.spacing.x = unit(6, "mm"))
ggsave("segundo_point_IM_tot_crop.png", path = paste(out, "scatter", sep = "/"), dpi = 300, height = 5.21, width = 6)

# 4. Capped  -----------------------------------------------------------

##So... qué vamos a quitar? 
#Empecemos con el 10% superior e inferior

data3 <- data %>% 
  mutate(
    ch.agua = ifelse(ch.agua <= 100, ch.agua, NA),
    
    ch.dren = ifelse(ch.dren < -100 |
                       ch.dren > 100 , NA, ch.dren),
    
    ch.elec = ifelse(ch.elec <= 100, ch.elec, NA),
    
    # ch.del = ifelse(ch.del < -100 |
    #                   ch.del > 100, NA, ch.del),
    
    ch.hom = ifelse(ch.hom < -100 | 
                      ch.hom > 100, NA, ch.hom)
  )

nrow(data2)
summary(data2)

# 4.1 Diagramas de densidad ---------------------------------------------------

ggplot(data3, aes(x = inc.ch)) +
  geom_density() +
  labs(title = "Diagrama de densidad",
       subtitle = "Cambio en % de votos al Incumbent",
       x = "", y = "")
ggsave("segundo_densidad_incumbent_cap.png", 
       path = paste(out, "density", sep = "/"),
       dpi = 300)

ggplot(data3, aes(x = ch.agua)) +
  geom_density() +
  labs(title = "Diagrama de densidad",
       subtitle = "Cambio en tomas de agua por 100k",
       x = "", y = "")
ggsave("segundo_densidad_agua_cap.png", 
       path = paste(out, "density", sep = "/"),
       dpi = 300)

ggplot(data3, aes(x = ch.dren)) +
  geom_density() +
  labs(title = "Diagrama de densidad",
       subtitle = "Cambio en drenaje por cada 100k",
       x = "", y = "")
ggsave("segundo_densidad_drenaje_cap.png", 
       path = paste(out, "density", sep = "/"),
       dpi = 300)

ggplot(data3, aes(x = ch.elec)) +
  geom_density() +
  labs(title = "Diagrama de densidad",
       subtitle = "Cambio en tomas de electricidad por cada 100k",
       x = "", y = "")
ggsave("segundo_densidad_electricidad_cap.png", 
       path = paste(out, "density", sep = "/"),
       dpi = 300)

# ggplot(data3, aes(x = ch.del)) +
#   geom_density() +
#   labs(title = "Diagrama de densidad",
#        subtitle = "Cambio en total de delitos por cada 100k",
#        x = "", y = "")
# ggsave("segundo_densidad_delitos_cap.png", 
#        path = paste(out, "density", sep = "/"),
#        dpi = 300)

ggplot(data3, aes(x = ch.hom)) +
  geom_density() +
  labs(title = "Diagrama de densidad",
       subtitle = "Cambio en homicidios por cada 100k",
       x = "", y = "")
ggsave("segundo_densidad_homicidios_cap.png", 
       path = paste(out, "density", sep = "/"),
       dpi = 300)


# 4.2 Boxplots ----------------------------------------------------------------

ggplot(data3, aes(x = inc_top, y = ch.agua)) +
  geom_boxplot() +
  geom_hline(yintercept = mean(data3$ch.agua, na.rm = T), color = "red") +
  labs(title = "Boxplot por partido",
       subtitle = "Diferencia en tasa de agua",
       x = "", y = "")
ggsave("segundo_boxplot_agua_cap.png", 
       path = paste(out, "boxplot", sep = "/"),
       dpi = 300)

ggplot(data3, aes(x = inc_top, y = ch.dren)) +
  geom_boxplot() +
  geom_hline(yintercept = mean(data3$ch.dren, na.rm = T), color = "red") +
  labs(title = "Boxplot por partido",
       subtitle = "Diferencia en tasa de drenaje",
       x = "", y = "")
ggsave("segundo_boxplot_drenaje_cap.png", 
       path = paste(out, "boxplot", sep = "/"),
       dpi = 300)

ggplot(data3, aes(x = inc_top, y = ch.elec)) +
  geom_boxplot() +
  geom_hline(yintercept = mean(data3$ch.elec, na.rm = T), color = "red") +
  labs(title = "Boxplot por partido",
       subtitle = "Diferencia en tasa de electricidad",
       x = "", y = "")
ggsave("segundo_boxplot_electricidad_cap.png", 
       path = paste(out, "boxplot", sep = "/"),
       dpi = 300)

# ggplot(data3, aes(x = inc_top, y = ch.del)) +
#   geom_boxplot() +
#   geom_hline(yintercept = mean(data3$ch.del, na.rm = T), color = "red") +
#   labs(title = "Boxplot por partido",
#        subtitle = "Diferencia en tasa de delitos",
#        x = "", y = "")
# ggsave("segundo_boxplot_delitos_cap.png", 
#        path = paste(out, "boxplot", sep = "/"),
#        dpi = 300)

ggplot(data3, aes(x = inc_top, y = ch.hom)) +
  geom_boxplot() +
  geom_hline(yintercept = mean(data3$ch.hom, na.rm = T), color = "red") +
  labs(title = "Boxplot por partido",
       subtitle = "Diferencia en tasa de homicidios",
       x = "", y = "")
ggsave("segundo_boxplot_homicidios_cap.png", 
       path = paste(out, "boxplot", sep = "/"),
       dpi = 300)


ggplot(data3, aes(x = as.factor(alt), y = ch.agua)) +
  geom_boxplot() +
  labs(title = "Boxplot por alternancia",
       subtitle = "Diferencia en tasa de agua",
       x = "", y = "")

ggplot(data3, aes(x = as.factor(alt), y = ch.dren)) +
  geom_boxplot() +
  labs(title = "Boxplot por alternancia",
       subtitle = "Diferencia en tasa de drenaje",
       x = "", y = "")

ggplot(data3, aes(x = as.factor(alt), y = ch.elec)) +
  geom_boxplot() +
  labs(title = "Boxplot por alternancia",
       subtitle = "Diferencia en tasa de electricidad",
       x = "", y = "")

# ggplot(data3, aes(x = as.factor(alt), y = ch.del)) +
#   geom_boxplot() +
#   labs(title = "Boxplot por alternancia",
#        subtitle = "Diferencia en tasa de delitos",
#        x = "", y = "")

ggplot(data3, aes(x = as.factor(alt), y = ch.hom)) +
  geom_boxplot() +
  labs(title = "Boxplot por alternancia",
       subtitle = "Diferencia en tasa de homicidios",
       x = "", y = "")


# 4.3 Scatter -------------------------------------------------------------

ggplot(data3, aes(x = ch.agua, y = inc.ch, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F, color = "black") +   scale_y_continuous(limits = c(-100, 100)) +
  # scale_x_continuous(labels = percent_format(1L)) +
  # scale_y_continuous(labels = percent_format(1L)) +
  labs(title = "Diagrama de dispersión de agua",# subtitle = "Todas las observaciones", 
       x = "Cambio % de agua", y = "Cambio % de votos al incumbent") +
  facet_wrap(. ~ inc_top) +
  theme_classic() +
  theme(legend.position = "null")
ggsave("segundo_point_ch.agua_tot_cap.png", path = paste(out, "scatter", sep = "/"), dpi = 300)

ggplot(data3, aes(x = ch.dren, y = inc.ch, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F, color = "black") +   scale_y_continuous(limits = c(-100, 100)) +
  # scale_x_continuous(labels = percent_format(1L)) +
  # scale_y_continuous(labels = percent_format(1L)) +
  labs(title = "Diagrama de dispersión de drenaje", #subtitle = "Todas las observaciones", 
       x = "Cambio en la tasa de drenaje", y = "Cambio % de votos al incumbent") +
  facet_wrap(. ~ inc_top) +
  theme_classic() +
  theme(legend.position = "null")
ggsave("segundo_point_ch.dren_tot_cap.png", path = paste(out, "scatter", sep = "/"), dpi = 300)

ggplot(data3, aes(x = ch.elec, y = inc.ch, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F, color = "black") +   scale_y_continuous(limits = c(-100, 100)) +
  # scale_x_continuous(labels = percent_format(1L)) +
  # scale_y_continuous(labels = percent_format(1L)) +
  labs(title = "Diagrama de dispersión de electricidad", #subtitle = "Todas las observaciones", 
       x = "Cambio % de electricidad", y = "Cambio % de votos al incumbent") +
  facet_wrap(. ~ inc_top) +
  theme_classic() +
  theme(legend.position = "null")
ggsave("segundo_point_ch.elec_tot_cap.png", path = paste(out, "scatter", sep = "/"), dpi = 300)

# ggplot(data3, aes(x = ch.del, y = inc.ch, col = inc_top)) +
#   geom_jitter(alpha = 0.3) +
#   scale_colour_manual(values = party, name = "Partido") +
#   geom_smooth(method = "lm", se = F, color = "black") +   scale_y_continuous(limits = c(-100, 100)) +
#   # scale_x_continuous(labels = percent_format(1L)) +
#   # scale_y_continuous(labels = percent_format(1L)) +
#   labs(title = "Diagrama de dispersión de delitos", #subtitle = "Todas las observaciones", 
#        x = "Cambio la tasa de delitos", y = "Cambio % de votos al incumbent") +
#   facet_wrap(. ~ inc_top) +
#   theme_classic() +
#   theme(legend.position = "null")
# ggsave("segundo_point_ch.del_tot_cap.png", path = paste(out, "scatter", sep = "/"), dpi = 300)

ggplot(data3, aes(x = ch.hom, y = inc.ch, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F, color = "black") +   scale_y_continuous(limits = c(-100, 100)) +
  # scale_x_continuous(labels = percent_format(1L)) +
  # scale_y_continuous(labels = percent_format(1L)) +
  labs(title = "Diagrama de dispersión de homicidios", #subtitle = "Todas las observaciones", 
       x = "Cambio en la tasa de homicidios", y = "Cambio % de votos al incumbent") +
  facet_wrap(. ~ inc_top) +
  theme_classic() +
  theme(legend.position = "null")
ggsave("segundo_point_ch.hom_tot_cap.png", path = paste(out, "scatter", sep = "/"), dpi = 300)

ggplot(data3, aes(x = IM, y = inc.ch, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F, color = "black") +   scale_y_continuous(limits = c(-100, 100)) +
  # scale_x_continuous(labels = percent_format(1L)) +
  # scale_y_continuous(labels = percent_format(1L)) +
  labs(title = "Diagrama de dispersión del Índice de Marginación", #subtitle = "Todas las observaciones", 
       x = "", y = "Cambio % de votos al incumbent") +
  facet_wrap(. ~ inc_top) +
  theme_classic() +
  theme(legend.position = "null", panel.spacing.x = unit(6, "mm"))
ggsave("segundo_point_IM_tot_cap.png", path = paste(out, "scatter", sep = "/"), dpi = 300)
