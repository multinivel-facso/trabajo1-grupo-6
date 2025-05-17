# Librerías
library(pacman)
pacman::p_load(lme4,
               reghelper,
               haven,
               stargazer,
               ggplot2, #gráficos
               texreg, #tabla de regresión
               dplyr) #manipulación de datos

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls())       # para limpar el entonrno de trabajo


# Base de datos -----------------------------------------------------------

load('input/data/ELSOC_Long_2016_2023.RData')

data = elsoc_long_2016_2023

datos2 <- data %>% filter(ola == 7) %>% 
  select(s30_01, s30_02, s30_03, s30_04, s30_05, s30_06, s30_07, s30_08, comuna, m0_sexo, m0_edad, m01, m02, comuna_cod, d01_01, d01_02, d01_03, d02_01, d02_02, d02_03) %>% 
  dplyr::filter(comuna_cod>=10)


datos2 <- mutate(datos2,
                 d01_01 = na_if(d01_01, -999),
                 d01_02 = na_if(d01_02, -999),
                 d01_03 = na_if(d01_03, -999))

datos2 <- mutate(datos2,
                 d01_01 = na_if(d01_01, -888),
                 d01_02 = na_if(d01_02, -888),
                 d01_03 = na_if(d01_03, -888))

datos2 <- mutate(datos,
                 d02_01 = na_if(d02_01, -999),
                 d02_02 = na_if(d02_02, -999),
                 d02_03 = na_if(d02_03, -999))

datos2 <- mutate(datos2,
                 d02_01 = na_if(d02_01, -888),
                 d02_02 = na_if(d02_02, -888),
                 d02_03 = na_if(d02_03, -888))

datos2 <- mutate(datos2,
                 s30_01 = na_if(s30_01, -888),
                 s30_02 = na_if(s30_02, -888),
                 s30_03 = na_if(s30_03, -888),
                 s30_04 = na_if(s30_04, -888),
                 s30_05 = na_if(s30_05, -888),
                 s30_06 = na_if(s30_06, -888),
                 s30_07 = na_if(s30_07, -888),
                 s30_08 = na_if(s30_08, -888))
                 

datos2 <- mutate(datos2,
                 s30_01 = na_if(s30_01, -999),
                 s30_02 = na_if(s30_02, -999),
                 s30_03 = na_if(s30_03, -999),
                 s30_04 = na_if(s30_04, -999),
                 s30_05 = na_if(s30_05, -999),
                 s30_06 = na_if(s30_06, -999),
                 s30_07 = na_if(s30_07, -999),
                 s30_08 = na_if(s30_08, -999))

datos2 <- mutate(datos2,
                metas = s30_01+s30_02+s30_03+s30_04+s30_05+s30_06+s30_07+s30_08,
                escala = d01_01+d01_02+d01_03,
                justo= d02_01+d02_02+d02_03)
              

agg_datos2=datos2 %>% group_by(comuna_cod) %>% summarise_all(funs(mean)) %>% as.data.frame()

stargazer(agg_datos2, type = "text")

results_0 = lmer(escala ~ 1 + (1 | comuna), data = datos2)
summary(results_0)

x <- reghelper::ICC(results_0)
x*100


tabla_comunas <- table(datos2$comuna_cod)
print(tabla_comunas)

conteo_comunas <- datos2 %>%
  count(comuna_cod, name = "n_casos") %>%
  arrange(desc(n_casos))

print(conteo_comunas)


         