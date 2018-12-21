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

# Dado el tamaño de los datos, no están disponibles en el repositorio de GitHub pero se pueden descargar en https://www.transparenciapresupuestaria.gob.mx

# PEFs 2011-2018. 
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

### Generar data frame que solo contenga los datos de los Centros Conacyt ----
cc <- bd %>% 
  filter(str_detect(desc_ramo, "Ciencia")) %>% 
  mutate(acronimo = case_when(desc_ur == "Centro de Ingeniería y Desarrollo Industrial" ~ "CIDESI",
                              desc_ur == "Centro de Investigación Científica de Yucatán, A.C." ~ "CICY",
                              desc_ur == "Centro de Investigación Científica y de Educación Superior de Ensenada, B.C." | desc_ur ==  "Centro de Investigación Científica y de Educación Superior de Ensenada, Baja California" ~ "CICESE",
                              desc_ur == "Centro de Investigación en Alimentación y Desarrollo, A.C." ~ "CIAD",
                              desc_ur == "Centro de Investigación en Ciencias de Información Geoespacial, A.C." | str_detect(desc_ur, "Centro de Investigación en Geografía y Geomática") ~ "CentroGEO",
                              desc_ur == "Centro de Investigación en Matemáticas, A.C." ~ "CIMAT",
                              desc_ur == "Centro de Investigación en Materiales Avanzados, S.C." ~ "CIMAV",
                              desc_ur == "Centro de Investigación en Química Aplicada" ~ "CIQA",
                              desc_ur == "Centro de Investigación y Asistencia en Tecnología y Diseño del Estado de Jalisco, A.C." ~ "CIATEJ",
                              desc_ur == "Centro de Investigación y Desarrollo Tecnológico en Electroquímica, S.C." ~ "CIDETEQ",
                              desc_ur == "Centro de Investigación y Docencia Económicas, A.C." ~ "CIDE",
                              desc_ur == "Centro de Investigaciones Biológicas del Noroeste, S.C." ~ "CIB",
                              desc_ur == "Centro de Investigaciones en Optica, A.C." | desc_ur ==  "Centro de Investigaciones en Óptica, A.C." ~ "CIO",
                              desc_ur == "Centro de Investigaciones y Estudios Superiores en Antropología Social" ~ "CIESAS",
                              desc_ur == "CIATEC, A.C. \"Centro de Innovación Aplicada en Tecnologías Competitivas\"" ~ "CIATEC",
                              desc_ur == "CIATEQ, A.C. Centro de Tecnología Avanzada" ~ "CIATEQ",
                              desc_ur == "Consejo Nacional de Ciencia y Tecnología" ~ "Conacyt",
                              desc_ur == "El Colegio de la Frontera Norte, A.C." ~ "COLEF",
                              desc_ur == "El Colegio de la Frontera Sur" ~ "ECOSUR",
                              desc_ur == "El Colegio de Michoacán, A.C." ~ "COLMICH",
                              desc_ur == "El Colegio de San Luis, A.C." ~ "COLSAN",
                              desc_ur == "Fondo para el Desarrollo de Recursos Humanos" ~ "FIDERH",
                              desc_ur == "Instituto de Ecología, A.C." ~ "INECOL",
                              desc_ur == "Instituto de Investigaciones \"Dr. José María Luis Mora\"" ~ "Instituto Mora",
                              desc_ur == "Instituto Nacional de Astrofísica, Optica y Electrónica" | desc_ur == "Instituto Nacional de Astrofísica, Óptica y Electrónica" ~ "INAOE",
                              desc_ur == "Instituto Potosino de Investigación Científica y Tecnológica, A.C." ~ "IPICYT",
                              TRUE ~ desc_ur)) %>% 
  filter(!acronimo %in% c("Conacyt", "FIDERH")) %>% 
  group_by(ciclo, acronimo) %>% 
  summarise(monto_anual = sum(monto, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(deflactor = case_when(ciclo == 2011 ~ 71.8, # Deflactor tomado de datos calculados por Alberto Serdán. El archivo está en la carpeta 01_datos
                               ciclo == 2012 ~ 74.8,
                               ciclo == 2013 ~ 75.9,
                               ciclo == 2014 ~ 79.3,
                               ciclo == 2015 ~ 81.5,
                               ciclo == 2016 ~ 85.8,
                               ciclo == 2017 ~ 91.6,
                               ciclo == 2018 ~  96.3,
                               ciclo == 2019 ~ 100),
         monto_anual_deflactado = (monto_anual/deflactor)*100) # Deflactar presupuestos


### Gráfica: cambio % del presupuesto de Centros Conacyt, 2018 vs. 2019 ----
cc %>% 
  arrange(acronimo, ciclo) %>% 
  group_by(acronimo) %>% 
  mutate(lag_uno = lag(monto_anual_deflactado)) %>% 
  ungroup() %>% 
  filter(ciclo == 2019) %>% 
  mutate(cambio_18_19 = ((monto_anual_deflactado - lag_uno)/lag_uno)*100,
         color_barras = ifelse(cambio_18_19 > 0, "positivo", "negativo")) %>% 
  ggplot(aes(fct_rev(fct_reorder(acronimo, cambio_18_19)), cambio_18_19)) +
  geom_col(aes(fill = color_barras)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-20, 15, 5), limits = c(-20, 10)) +
  scale_fill_manual(values = c("salmon", "steelblue")) +
  labs(title = str_wrap(str_to_upper("cambio porcentual del presupuesto de 24 centros conacyt entre 2018 y 2019"), width = 75), 
       x = "",
       y = "\nCambio porcentual",
       color = NULL,
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: SHCP, url: https://bit.ly/2BzeG1Q. Los datos de 2018 corresponde al presupuesto aprobado; los de\n2019 al presupuesto proyectado. La gráfica incluye datos de todos los Centros Conacyt excepto el COMIMS e Infotec.") +
  tema +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "none")


ggsave(filename = "cambio_porcetual_prespuesto_centros_conacyt_2018_2019.png", path = "03_graficas/", width = 15, height = 10, dpi = 200)



### Gráfica: evolución presupuesto Centros Conacyt, 2011-2019 ----
cc %>% 
  mutate(color_cide = ifelse(acronimo == "CIDE", "CIDE", "Otros centros")) %>% 
  ggplot(aes(ciclo, monto_anual_deflactado/1000000, group = acronimo)) +
  geom_line(size = 1, alpha = 0.6, color = "steelblue") +
  scale_x_continuous(breaks = 2011:2019) +
  scale_y_continuous(labels = comma, breaks = seq(0, 700, 100)) +
  labs(title = str_wrap(str_to_upper("presupuesto anual de 24 de los 26 centros conacyt"), width = 65), 
       subtitle = "Millones de pesos constantes", 
       x = "",
       y = "Millones de pesos constantes\n",
       color = NULL,
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: SHCP, url: https://bit.ly/2BzeG1Q. Los datos de 2011 a 2018 corresponde a presupuestos aprobados; los de\n2019 a presupuestos proyectados. La gráfica incluye datos de todos los Centros Conacyt excepto el COMIMS e Infotec.") +
  tema +
  theme(legend.direction = "vertical",
        legend.position = c(0.9, 0.9))

ggsave(filename = "prespuesto_centros_conacyt_2011_2019_lineas.png", path = "03_graficas/", width = 15, height = 10, dpi = 200)


### Gráfica: boxlplot de evolución presupuesto Centros Conacyt, 2011-2019 ----
cc %>% 
  mutate(color_cide = ifelse(acronimo == "CIDE", "CIDE", "Otros centros")) %>% 
  ggplot(aes(ciclo, monto_anual_deflactado/1000000, group = ciclo)) +
  geom_boxplot(color = "steelblue", outlier.color = "salmon") +
  scale_x_continuous(breaks = 2011:2019) +
  scale_y_continuous(labels = comma, breaks = seq(0, 700, 100)) +
  labs(title = str_wrap(str_to_upper("presupuesto anual de 24 de los 26 centros conacyt"), width = 65), 
       subtitle = "Millones de pesos constantes", 
       x = "",
       y = "Millones de pesos constantes\n",
       color = NULL,
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: SHCP, url: https://bit.ly/2BzeG1Q. Los datos de 2011 a 2018 corresponde a presupuestos aprobados; los de\n2019 a presupuestos proyectados. La gráfica incluye datos de todos los Centros Conacyt excepto el COMIMS e Infotec.") +
  tema +
  theme(legend.direction = "vertical",
        legend.position = c(0.9, 0.9))

ggsave(filename = "prespuesto_centros_conacyt_2011_2019_boxplot.png", path = "03_graficas/", width = 15, height = 10, dpi = 200)

### Gráfica: líneas de evolución presupuesto Centros Conacyt, 2011-2019, CIDE resaltado ----
cc %>% 
  mutate(color_cide = ifelse(acronimo == "CIDE", "CIDE", "Otros centros")) %>% 
  ggplot(aes(ciclo, monto_anual_deflactado/1000000, group = acronimo)) +
  geom_line(aes(color = color_cide), size = 1, alpha = 0.6) +
  scale_x_continuous(breaks = 2011:2019) +
  scale_y_continuous(labels = comma, breaks = seq(0, 700, 100)) +
  scale_color_manual(values = c("salmon", "steelblue")) +
  labs(title = str_wrap(str_to_upper("presupuesto anual de 24 de los 26 centros conacyt"), width = 65), 
       subtitle = "Millones de pesos constantes", 
       x = "",
       y = "Millones de pesos constantes\n",
       color = NULL,
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: SHCP, url: https://bit.ly/2BzeG1Q. Los datos de 2011 a 2018 corresponde a presupuestos aprobados; los de\n2019 a presupuestos proyectados. La gráfica incluye datos de todos los Centros Conacyt excepto el COMIMS e Infotec.") +
  tema +
  theme(legend.direction = "vertical",
        legend.position = c(0.9, 0.9))

ggsave(filename = "prespuesto_centros_conacyt_2011_2019_lineas_cide_resaltado.png", path = "03_graficas/", width = 15, height = 10, dpi = 200)


### Gráfica: columnas de cambio % del presupuesto de Centros Conacyt, 2015 vs. 2019 ----
cc %>% 
  arrange(acronimo, ciclo) %>% 
  group_by(acronimo) %>% 
  mutate(lag_cuatro = lag(monto_anual_deflactado, n = 4)) %>% 
  ungroup() %>% 
  filter(ciclo == 2019) %>% 
  mutate(cambio_15_19 = ((monto_anual_deflactado - lag_cuatro)/lag_cuatro)*100) %>% 
  ggplot(aes(fct_rev(fct_reorder(acronimo, cambio_15_19)), cambio_15_19)) +
  geom_col(fill = "salmon") +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-45, 5, 5), limits = c(-45, 5)) +
  labs(title = str_wrap(str_to_upper("cambio porcentual del presupuesto de 24 centros conacyt entre 2015 y 2019"), width = 75), 
       x = "",
       y = "\nCambio porcentual",
       color = NULL,
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: SHCP, url: https://bit.ly/2BzeG1Q. Los datos de 2015 corresponde al presupuesto aprobado; los de\n2019 al presupuesto proyectado. La gráfica incluye datos de todos los Centros Conacyt excepto el COMIMS e Infotec.") +
  tema +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggsave(filename = "cambio_porcetual_prespuesto_centros_conacyt_2015_2019.png", path = "03_graficas/", width = 15, height = 10, dpi = 200)



### Gráfica: presupuesto de Centros Conacyt en el primer año de los gobiernos de Erique Peña Nieto (2013) y Andrés Manuel López Obrador (2019) ----
cc %>% 
  arrange(acronimo, ciclo) %>% 
  group_by(acronimo) %>% 
  mutate(lag_cuatro = lag(monto_anual_deflactado, n = 4)) %>% 
  ungroup() %>% 
  filter(ciclo == 2019 | ciclo == 2013) %>% 
  ggplot(aes(fct_rev(fct_reorder(acronimo, monto_anual_deflactado/1000000)), monto_anual_deflactado/1000000)) +
  geom_point(aes(color = factor(ciclo)), size = 4) +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 700, 50), limits = c(0, 709)) +
  scale_color_manual(values = c("salmon", "steelblue")) +
  labs(title = str_wrap(str_to_upper("presupuesto de 24 centros conacyt en el primer año de los gobiernos de enrique Peña Nieto (2013) y andrés manuel López Obrador (2019)"), width = 70), 
       subtitle = "Millones de pesos constantes", 
       x = "",
       y = "\nMillones de pesos constantes",
       color = NULL,
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: SHCP, url: https://bit.ly/2BzeG1Q. Los datos de 2013 corresponde al presupuesto aprobado; los de\n2019 al presupuesto proyectado. La gráfica incluye datos de todos los Centros Conacyt excepto el COMIMS e Infotec.") +
  tema +
  theme(legend.position = c(0.89, 0.9),
        legend.direction = "vertical",
        legend.text = element_text(size = 13))


ggsave(filename = "prespuesto_centros_conacyt_2013_vs_2019.png", path = "03_graficas/", width = 15, height = 10, dpi = 200)
