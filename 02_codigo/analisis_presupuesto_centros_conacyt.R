### Paquetes ----
library(pacman)
p_load(ggrepel, janitor, scales, readxl, tidyverse, treemapify)

### Setup ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen=999) # Prevenir notación científica

### Definir tema de gráficas ----
tema <-  theme_minimal() +
  theme(text = element_text(family="Didact Gothic Regular", color = "grey35"),
        plot.title = element_text(size = 24, face = "bold", margin = margin(10,0,20,0), family="Trebuchet MS Bold", color = "grey25"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666", margin = margin(0, 0, 20, 0), family="Didact Gothic Regular"),
        plot.caption = element_text(hjust = 0, size = 15),
        panel.grid = element_line(linetype = 2), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 16, face = "bold", family="Trebuchet MS Bold"),
        legend.text = element_text(size = 14, family="Didact Gothic Regular"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 18, hjust = 1, face = "bold", margin = margin(0,0,0,0), family="Didact Gothic Regular"),
        axis.text = element_text(size = 16, face = "bold", family="Didact Gothic Regular"))

### Datos ----

# Los datos los obtuve de las secciones ¿PARA QUÉ SE GASTA? y ¿QUIÉN GASTA? en esta liga: https://www.transparenciapresupuestaria.gob.mx/es/PTP/infografia_ppef2019#page3

# PEFs 2011-2018. Fuente: https://www.transparenciapresupuestaria.gob.mx
pef_11 <- read_csv("01_datos/presupuesto_mexico__2011.csv", locale = locale("es", asciify = TRUE))
pef_12 <- read_csv("01_datos/presupuesto_mexico__2012.csv", locale = locale("es", asciify = TRUE))
pef_13 <- read_csv("01_datos/presupuesto_mexico__2013.csv", locale = locale("es", asciify = TRUE))
pef_14 <- read_csv("01_datos/presupuesto_mexico__2014.csv", locale = locale("es", asciify = TRUE))
pef_15 <- read_csv("01_datos/presupuesto_mexico__2015.csv", locale = locale("es", asciify = TRUE))
pef_16 <- read_csv("01_datos/presupuesto_mexico__2016.csv", locale = locale("es", asciify = TRUE))
pef_17 <- read_csv("01_datos/presupuesto_mexico__2017.csv", locale = locale("es", asciify = TRUE))
pef_18 <- read_csv("01_datos/presupuesto_mexico__2018.csv", locale = locale("es", asciify = TRUE))

# PPEF 2019 completo
ppef_19 <- read_csv("01_datos/PPEF_2019.csv", locale = locale(encoding = "latin1"))


### "Limpiar" nombres de variables ----
pef_11 <- pef_11 %>% clean_names()
pef_12 <- pef_12 %>% clean_names()
pef_13 <- pef_13 %>% clean_names()
pef_14 <- pef_14 %>% clean_names()
pef_15 <- pef_15 %>% clean_names()
pef_16 <- pef_16 %>% clean_names()
pef_17 <- pef_17 %>% clean_names()
pef_18 <- pef_18 %>% clean_names()
ppef_19 <- ppef_19 %>% clean_names() 


### Unir PEFs 2011-2018 ----
pef_11_18 <- rbind(pef_11, pef_12, pef_13, pef_14, pef_15, pef_16, pef_17, pef_18)


### Agregar columnas que faltan para unir las dos bases de datos ----
pef_11_18_corto <- pef_11_18 %>%  
  select(ciclo, desc_ramo, desc_ur, desc_pp, contains("monto")) %>% 
  mutate(monto = monto_aprobado, 
         monto_proyecto = NA)

ppef_19_corto <- 
  ppef_19 %>%
  select(ciclo, desc_ramo, desc_ur, desc_pp, contains("monto")) %>% 
  mutate(monto = monto_proyecto, 
         monto_adefas = NA, 
         monto_aprobado = NA,
         monto_devengado = NA, 
         monto_ejercicio = NA, 
         monto_ejercido = NA, 
         monto_modificado = NA,
         monto_pagado = NA)


### Unir datos de PEFs 2011-2018 y PPEF 2019 ----
bd <- rbind(pef_11_18_corto, ppef_19_corto) 