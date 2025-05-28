# Análisis de datos multinivel
# Entrega 1: Análisis multinivel del estatus social subjetivo: el caso de la Región Metropolitana
# Integrantes: Victoria Arias, Cristóbal Mejías, Nicolas Outerbridge
# Fecha: 28/05/2025


#Librerías
library(pacman)

pacman::p_load(tidyverse,sjPlot,confintr,gginference,rempsyc,broom,sjmisc,lme4,
               reghelper,haven,stargazer,ggplot2,texreg,dplyr,knitr,summarytools,Publish,
               corrplot,readxl,dplyr,sjPlot,ggfortify,sjlabelled,lmtest,sandwich) 

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls())       # para limpar el entonrno de trabajo


# Base de datos -----------------------------------------------------------

load('input/data/ELSOC_Long_2016_2023.RData')

elsoc = elsoc_long_2016_2023

pobrezamultidimensional <- read_excel("input/data/Estimaciones_Indice_Pobreza_Multidimensional_Comunas_2022.xlsx")


# Filtrar BBDD --------------------------------------------------

pobreza_proc <- dplyr::select(pobrezamultidimensional,
                              comuna_cod=cod_com,
                              com= "Nombre comuna",
                              reg="Región",
                              pob_multi="Porcentaje de personas en situación de pobreza multidimensional 2022")

datos <- merge(elsoc, pobreza_proc, by="comuna_cod")

datos_proc <- datos %>%
  filter(ola==6) %>%
  select(sexo=m0_sexo, #demográficos
         edad=m0_edad,
         comuna, #cluster
         comuna_cod,
         region,
         region_cod,
         ess=d01_01, #variable dependiente
         ess_f=d01_02, #variables nivel 1
         nedu=m01,
         seg_bar=t06_01, #variables nivel 2
         seg_ins=t10,
         pob_multi) 


#Filtrar por Región Metropolitana 
datos_proc <- datos_proc %>% filter(region=='Metropolitana',
                                    comuna_cod != 2203) #se elimina dato aislado agregado por error

## Remover NA's ----------------------------------------------------------------
datos_proc <- datos_proc %>% 
  set_na(., na = c(-888, -999)) %>% 
  na.omit()

# Creación de variable Seguridad ---------------------------------------------------------------

datos_proc = datos_proc %>% 
  rowwise() %>%
  mutate(seguridad = mean(c(seg_bar, seg_ins))) %>% 
  ungroup()

datos_proc %>% select(seguridad) %>% head(10) # Primeros 10 casos

# Promedio por comuna
datos_proc = datos_proc %>%  
  group_by(comuna_cod) %>% 
  mutate(meanseg = mean(seguridad, na.rm = TRUE))

#Visualización de nueva variable
# Desviación estándar
datos_proc = datos_proc %>%  
  group_by(comuna_cod) %>% 
  mutate(sdseg = sd(seguridad, na.rm = TRUE))

# Tamaño (cantidad de casos por país)
datos_proc = datos_proc %>%  
  group_by(comuna_cod) %>% 
  mutate(count = length(comuna_cod))

datos_proc %>% 
  group_by(comuna_cod=to_label(comuna_cod)) %>% 
  summarise("Mean Seguridad"=mean(meanseg), 
            "SD Seguridad"=mean(sdseg), 
            N=mean(count)) %>% 
  print(n = nrow(.))

datos_proc %>% select(seguridad) %>% head(10) # Primeros 10 casos



# Análisis bivariado ------------------------------------------------------

view(dfSummary(datos_proc, headings=FALSE, graph.col = FALSE))

dat_scat=datos_proc %>% group_by(comuna) %>% select(ess,nedu) %>% na.omit() %>% summarise_all(mean)
names(dat_scat)

sjPlot::plot_scatter(dat_scat, ess,nedu,
                     dot.labels = to_label(dat_scat$comuna),
                     fit.line = "lm",
                     show.ci = TRUE)



# Matriz Corr -------------------------------------------------------------

cormat=datos_proc %>% select(ess,ess_f,nedu, meanseg, pob_multi) %>% cor()
round(cormat, digits=2)

corrplot.mixed(cormat)



# Análisis multinivel -----------------------------------------------------



# Corr Intraclase ---------------------------------------------------------

agg_data=datos_proc %>% group_by(comuna_cod) %>% summarise_all(funs(mean)) %>% as.data.frame()

results_0 = lmer(ess ~ 1 + (1 | comuna_cod), data = datos_proc)
summary(results_0)

x<- reghelper::ICC(results_0)
x*100

# Modelos -----------------------------------------------------------------

#Modelo 1: Predictores de nivel individual -------------------------------

results_1 = lmer(ess ~ 1 + nedu  + ess_f + (1 | comuna_cod), data = datos_proc)
screenreg(results_1, naive=TRUE)

# Modelo 2: Predictores nivel 2 -------------------------------------------

results_2 = lmer(ess ~ 1 + pob_multi + meanseg  + (1 | comuna_cod), data = datos_proc)
screenreg(results_2)

# Modelo 3: Predictores individuales y grupales ---------------------------

results_3 = lmer(ess ~ 1 + nedu + ess_f + pob_multi + meanseg + (1 | comuna_cod), data = datos_proc)
screenreg(results_3)


# Comparación individual, agregado y multinivel ---------------------------

reg_ind=lm(ess ~ nedu + ess_f + pob_multi + meanseg, data=datos_proc)
reg_agg=lm(ess ~ nedu + ess_f + pob_multi + meanseg, data=agg_data)

# Observar: ¿Qué sucede con los coeficientes y errores estándar cuando se comparan los coeficientes y los errores estándar?
screenreg(list(reg_ind, reg_agg, results_3))

screenreg(list(results_1, results_2, results_3))


# Guardar BBDD ------------------------------------------------------------

saveRDS(datos_proc, file = "output/base_proc.Rdata")

