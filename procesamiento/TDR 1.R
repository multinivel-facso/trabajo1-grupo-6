# Librerías
library(pacman)

pacman::p_load(tidyverse,   # manipulacion datos
               sjPlot,      # tablas
               confintr,    # IC
               gginference, # visualizacion 
               rempsyc,     # reporte
               broom,       # varios
               sjmisc,      # para descriptivos
               lme4,
               reghelper,
               haven,
               stargazer,
               ggplot2, #gráficos
               texreg, #tabla de regresión
               dplyr, #manipulación de datos
               knitr,
               summarytools,
               Publish,
               corrplot,
               readxl) 

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls())       # para limpar el entonrno de trabajo


# Base de datos -----------------------------------------------------------

load('input/data/ELSOC_Long_2016_2023.RData')

elsoc = elsoc_long_2016_2023
pobrezamultidimensional <- read_excel("input/data/Estimaciones_Indice_Pobreza_Multidimensional_Comunas_2022.xlsx")

pobreza_proc <- dplyr::select(pobrezamultidimensional,
                              comuna_cod=cod_com,
                              com= "Nombre comuna",
                              reg="Región",
                              pob_multi="Porcentaje de personas en situación de pobreza multidimensional 2022")

datos <- merge(elsoc, pobreza_proc, by="comuna_cod")

datos_proc <- datos %>% 
  filter(ola==6) %>%
  select(comuna, #cluster
         comuna_cod,
         region_cod,
         sexo=m0_sexo, #variables nivel 1
         edad=m0_edad,
         nedu=m01,
         ess=d01_01,
         ess_f=d01_02,
         ingreso=m13,
         t06_01, #variables nivel 2: Seguridad
         seg_ins=t10,
         rinas=t09_01,
         asalto=t09_02,
         trafico=t09_03,
         amigos=t03_01, #cohesión: sociabilidad
         gen_soc=t03_02,
         gen_cord=t03_03,
         gen_colab=t03_04,
         conf_vecinos= t01, #cohesión: confianza
         barr_ideal=t02_01, #cohesión: apego
         barr_integr=t02_02,
         barr_iden=t02_03,
         barr_parte=t02_04,
         pob_multi) 



datos_proc <- datos_proc %>% filter(region_cod==13)



## Remover NA's ----------------------------------------------------------------
datos_proc <- datos_proc %>% 
  set_na(., na = c(-888, -999)) %>% 
  na.omit()

# Explorar varaibles ------------------------------------------------------

view(dfSummary(datos_proc, headings=FALSE, graph.col = FALSE))

# Seguridad ---------------------------------------------------------------

datos_proc = datos_proc %>% 
  rowwise() %>%
  mutate(frec_seguridad = mean(c(rinas,asalto,trafico)),
         seguridad = mean(c(t06_01, seg_ins))) %>% 
  ungroup()


datos_proc %>% select(seguridad) %>% head(10) # Primeros 10 casos

# Promedio
datos_proc = datos_proc %>%  
  group_by(comuna_cod) %>% 
  mutate(meanseg = mean(frec_seguridad, na.rm = TRUE))

# Desviación estándar
datos_proc = datos_proc %>%  
  group_by(comuna_cod) %>% 
  mutate(sdseg = sd(seguridad, na.rm = TRUE))

# Tamaño (cantidad de casos por país)
datos_proc = datos_proc %>%  
  group_by(comuna_cod) %>% 
  mutate(count = length(comuna_cod))

datos_proc %>% 
  group_by(Comuna=to_label(comuna_cod)) %>% 
  summarise("Mean Seguridad"=mean(meanseg), 
            "SD Seguridad"=mean(sdseg), 
            N=mean(count)) %>% 
  print(n = nrow(.))


# Cohesión ----------------------------------------------------------------

datos_proc = datos_proc %>% 
  rowwise() %>%
  mutate(sociabilidad = mean(c(amigos,gen_soc,gen_cord,gen_colab)),
         apego = mean(c(barr_ideal, barr_integr,barr_iden,barr_parte)),
         cohesion = mean (c(sociabilidad,apego,conf_vecinos))) %>% 
  ungroup()

datos_proc %>% select(cohesion) %>% head(10) # Primeros 10 casos

# Promedio
datos_proc = datos_proc %>%  
  group_by(comuna_cod) %>% 
  mutate(meancoe = mean(cohesion, na.rm = TRUE))

# Desviación estándar
datos_proc = datos_proc %>%  
  group_by(comuna_cod) %>% 
  mutate(sdcoe = sd(cohesion, na.rm = TRUE))

# Tamaño (cantidad de casos por país)
datos_proc = datos_proc %>%  
  group_by(comuna_cod) %>% 
  mutate(count = length(comuna_cod))

datos_proc %>% 
  group_by(Comuna=to_label(comuna_cod)) %>% 
  summarise("Mean Cohesion"=mean(meancoe), 
            "SD Cohesion"=mean(sdcoe), 
            N=mean(count)) %>% 
  print(n = nrow(.))


# Matriz Corr -------------------------------------------------------------

cormat=datos_proc %>% select(ess,seguridad, frec_seguridad, t06_01, seg_ins) %>% cor()
round(cormat, digits=2)

corrplot.mixed(cormat)
# Corr Intraclase ---------------------------------------------------------

agg_data=datos_proc %>% group_by(comuna_cod) %>% summarise_all(funs(mean)) %>% as.data.frame()

results_0 = lmer(ess ~ 1 + (1 | comuna), data = datos_proc)
summary(results_0)

x<- reghelper::ICC(results_0)
x*100


# Modelos -----------------------------------------------------------------

#Modelo 1: Predictores de nivel individual -------------------------------

results_1 = lmer(ess ~ 1 + ingreso + ess_f + nedu  + t06_01 + seg_ins + frec_seguridad + sociabilidad + apego + conf_vecinos + (1 | comuna_cod), data = datos_proc)
screenreg(results_1, naive=TRUE)

results_11 = lmer(ess ~ 1 + ess_f + nedu + (1 | comuna_cod), data = datos_proc)
screenreg(results_11, naive=TRUE)


# Modelo 1 IND Seguridad --------------------------------------------------

results_12 = lmer(ess ~ 1 + t06_01 + seg_ins + frec_seguridad + (1 | comuna_cod), data = datos_proc)
screenreg(results_12, naive=TRUE)

results_13 = lmer(ess ~ 1 + frec_seguridad + (1 | comuna_cod), data = datos_proc)
screenreg(results_13, naive=TRUE)

results_14 = lmer(ess ~ 1 + t06_01 + seg_ins + (1 | comuna_cod), data = datos_proc)
screenreg(results_14, naive=TRUE)

results_15 = lmer(ess ~ 1 + frec_seguridad + seg_ins + (1 | comuna_cod), data = datos_proc)
screenreg(results_15, naive=TRUE)


# Modelo 1 IND Cohe -------------------------------------------------------

results_16 = lmer(ess ~ 1 + sociabilidad + apego + conf_vecinos + (1 | comuna_cod), data = datos_proc)
screenreg(results_16, naive=TRUE)

results_17 = lmer(ess ~ 1 + conf_vecinos + (1 | comuna_cod), data = datos_proc)
screenreg(results_17, naive=TRUE)

results_18 = lmer(ess ~ 1 + sociabilidad + apego + (1 | comuna_cod), data = datos_proc)
screenreg(results_18, naive=TRUE)

# Modelo 2: Predictores nivel 2 -------------------------------------------

results_2 = lmer(ess ~ 1 + meancoe + meanseg  + pob_multi +(1 | comuna_cod), data = datos_proc)
screenreg(results_2)



# Modelo 3: Predictores individuales y grupales ---------------------------

results_3 = lmer(ess ~ 1 + nedu + meancoe + meanseg  + pob_multi + (1 | comuna_cod), data = datos_proc)
screenreg(results_3)


# Comparación individual, agregado y multinivel ---------------------------

reg_ind=lm(ess ~ nedu + meancoe + meanseg + pob_multi, data=datos_proc)
agg_data=datos_proc %>% group_by(comuna_cod) %>% summarise_all(funs(mean))
reg_agg=lm(ess ~ nedu + meancoe + meanseg + pob_multi, data=datos_proc)

# Observar: ¿Qué sucede con los coeficientes y errores estándar cuando se comparan los coeficientes y los errores estándar?
screenreg(list(reg_ind, reg_agg, results_3))

# Generación de tabla para publicar en HTML
htmlreg(list(reg_ind, reg_agg, results_3), 
        custom.model.names = c("Individual","Agregado","Multinivel"),    
        custom.coef.names = c("Intercepto", "$ess_f_{ij}$","$nedu_{ij}$", "$meancoe_{j}$", "$meanseg_{j}$", "$pob_multi_{j}$"), 
        custom.gof.names=c(NA,NA,NA,NA,NA,NA,NA, 
                           "Var:id ($\\tau_{00}$)","Var: Residual ($\\sigma^2$)"),
        custom.note = "%stars. Errores estándar en paréntesis",
        caption="Comparación de modelos Individual, Agregado y Multinivel",
        caption.above=TRUE,
        doctype = FALSE)



