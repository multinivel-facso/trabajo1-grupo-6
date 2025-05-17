# Librerías
library(pacman)
pacman::p_load(lme4,
               reghelper,
               haven,
               stargazer,
               ggplot2, #gráficos
               texreg, #tabla de regresión
               dplyr) #manipulación de datos

p_load(tidyverse,   # manipulacion datos
       sjPlot,      # tablas
       confintr,    # IC
       gginference, # visualizacion 
       rempsyc,     # reporte
       broom,       # varios
       sjmisc,      # para descriptivos
       knitr) 

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls())       # para limpar el entonrno de trabajo


# Base de datos -----------------------------------------------------------

load('input/data/ELSOC_Long_2016_2023.RData')

datos = elsoc_long_2016_2023

datos <- elsoc_long_2016_2023 %>% 
  filter(ola==7) %>%
  select(sexo=m0_sexo,
         edad=m0_edad,
         nedu=m01, 
         comuna, 
         comuna_cod,
         region_cod, 
         d01_01) 

datos <- datos %>% filter(region_cod==13)





## Remover NA's ----------------------------------------------------------------
datos <- datos %>% 
  set_na(., na = c(-888, -999)) %>% 
  na.omit()

agg_data=datos %>% group_by(comuna_cod) %>% summarise_all(funs(mean)) %>% as.data.frame()


results_0 = lmer(d01_01 ~ 1 + (1 | comuna), data = datos)
summary(results_0)

x<- reghelper::ICC(results_0)
x*100



# Extras ------------------------------------------------------------------

simce <- merge(simcerbd_proc, pobreza_proc, by="cod_com")
