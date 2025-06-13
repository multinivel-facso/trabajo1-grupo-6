# Consulta de cómo ingresar la variable de ingresos en el trabajo del grupo 4: 



# Paquetes y BBDD ---------------------------------------------------------



pacman::p_load(tidyverse,sjPlot,confintr,gginference,rempsyc,broom,sjmisc,lme4,
               reghelper,haven,stargazer,ggplot2,texreg,dplyr,knitr,summarytools,Publish,
               corrplot,readxl,dplyr,sjPlot,ggfortify,sjlabelled,lmtest,sandwich)

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls())       # para limpar el entonrno de trabajo

load('input/data/ELSOC_Long_2016_2023.RData')

elsoc = elsoc_long_2016_2023


# Selección y filtro por RM -----------------------------------------------

data <- elsoc %>%
  filter(ola==6) %>%
  select(sexo=m0_sexo, 
         edad=m0_edad,
         comuna, 
         comuna_cod,
         region,
         region_cod,
         ess=d01_01, 
         ess_f=d01_02,
         nedu=m01,
         inghogar=m29, # Ingreso por hogar
         inghogar_t=m30, # Ingreso total del hogar (en 20 tramos)7
         m46,
         m46_nhogar)

data <- data %>% filter(region=='Metropolitana',
                                    comuna_cod != 2203) #se elimina dato aislado agregado por error


# Ingresos ----------------------------------------------------------------

#Visulaización de datos

view(dfSummary(data, headings = FALSE, method = "render"))
view_df(data,max.len = 100)

#Descriptivos de ingresos

descr(data$inghogar,style = "rmarkdown",stats = "common", transpose = T,headings = F)
descr(data$inghogar_t,style = "rmarkdown",stats = "common", transpose = T,headings = F)


#Aquí se observa que ingresos por hogar no tiene NA, pero al revisar el libro de códigos y las tablas del inicio, se decide hacer lo siguiente:

data <- mutate(data,
               inghogar = na_if(inghogar, -999),
               inghogar = na_if(inghogar, -888),
               inghogar = na_if(inghogar, -777),
               inghogar = na_if(inghogar, -666))
 
colSums(is.na(data))
descr(data$inghogar,style = "rmarkdown",stats = "common", transpose = T,headings = F)

#Ahora si se identificó los 80 casos que se pierden por la variable de ingreso por hogar, ahora visualizamos los ingresos en tramos

sjmisc::frq(data$inghogar_t,
            out = "txt",
            show.na = T) %>% knitr::kable()


# Pasos seguidos de los tips del profesor:
data$inghogar_t[data$inghogar_t==1] <-(       220000 )    # [1]  "Menos de $220.000 mensuales liquidos"
data$inghogar_t[data$inghogar_t==2] <-(220001 +280000 )/2 # [2]  "De $220.001 a $280.000 mensuales liquidos"
data$inghogar_t[data$inghogar_t==3] <-(280001 +330000 )/2 # [3]  "De $280.001 a $330.000 mensuales liquidos"
data$inghogar_t[data$inghogar_t==4] <-(330001 +380000 )/2 # [4]  "De $330.001 a $380.000 mensuales liquidos"
data$inghogar_t[data$inghogar_t==5] <-(380001 +420000 )/2 # [5]  "De $380.001 a $420.000 mensuales liquidos"
data$inghogar_t[data$inghogar_t==6] <-(420001 +470000 )/2 # [6]  "De $420.001 a $470.000 mensuales liquidos"
data$inghogar_t[data$inghogar_t==7] <-(470001 +510000 )/2 # [7]  "De $470.001 a $510.000 mensuales liquidos"
data$inghogar_t[data$inghogar_t==8] <-(510001 +560000 )/2 # [8]  "De $510.001 a $560.000 mensuales liquidos"
data$inghogar_t[data$inghogar_t==9] <-(560001 +610000 )/2 # [9]  "De $560.001 a $610.000 mensuales liquidos"
data$inghogar_t[data$inghogar_t==10]<-(610001 +670000 )/2 # [10] "De $610.001 a $670.000 mensuales liquidos"
data$inghogar_t[data$inghogar_t==11]<-(670001 +730000 )/2 # [11] "De $670.001 a $730.000 mensuales liquidos"
data$inghogar_t[data$inghogar_t==12]<-(730001 +800000 )/2 # [12] "De $730.001 a $800.000 mensuales liquidos"
data$inghogar_t[data$inghogar_t==13]<-(800001 +890000 )/2 # [13] "De $800.001 a $890.000 mensuales liquidos"
data$inghogar_t[data$inghogar_t==14]<-(890001 +980000 )/2 # [14] "De $890.001 a $980.000 mensuales liquidos"
data$inghogar_t[data$inghogar_t==15]<-(980001 +1100000)/2 # [15] "De $980.001 a $1.100.000 mensuales liquidos"
data$inghogar_t[data$inghogar_t==16]<-(1100001+1260000)/2 # [16] "De $1.100.001 a $1.260.000 mensuales liquidos"
data$inghogar_t[data$inghogar_t==17]<-(1260001+1490000)/2 # [17] "De $1.260.001 a $1.490.000 mensuales liquidos"
data$inghogar_t[data$inghogar_t==18]<-(1490001+1850000)/2 # [18] "De $1.490.001 a $1.850.000 mensuales liquidos"
data$inghogar_t[data$inghogar_t==19]<-(1850001+2700000)/2 # [19] "De $1.850.001 a $2.700.000 mensuales liquidos"
data$inghogar_t[data$inghogar_t==20]<-(2700000)           # [20] "Mas de $2.700.000 a mensuales liquidos"


data$inghogar_i <- ifelse(test = (is.na(data$inghogar)), #¿existen NA en ingresos?
                           yes = data$inghogar_t,         #VERDADERO, remplazar con la media del tramo
                           no = data$inghogar)            #FALSE, mantener la variable original.

data$inghogar_i <- set_label(data$inghogar_i,"Ingreso total del hogar (imputada)")

sjmisc::descr(data[,c("inghogar","inghogar_i")],
              show =c("label", "n", "NA.prc", "mean", "md","sd")) %>% knitr::kable(digits = 2)

#Hasta este paso estamos OK, pero en los pasos se continúa con el cálculo del ingreso per capita por hogar
#En la presente ola aparece como NA en todas las preguntas de cantidad de personas por hogar. Por lo tanto intentamos lo siguiente

data_2 = elsoc %>%
  select(sexo=m0_sexo, 
         edad=m0_edad,
         comuna, 
         comuna_cod,
         region,
         region_cod,
         ess=d01_01, 
         ess_f=d01_02,
         nedu=m01,
         inghogar=m29, # Ingreso por hogar
         inghogar_t=m30, # Ingreso total del hogar (en 20 tramos)
         m46,
         m46_nhogar)

#Si visualizamos aparece bien:
descr(data_2$m46_nhogar,style = "rmarkdown",stats = "common", transpose = T,headings = F)


#Pero si filtramos por la región metropolitana y visualizamos nuevamente, los NA representan un 88%
data_2 <- data_2 %>% filter(region_cod==13,
                                    comuna_cod != 2203) #se elimina dato aislado agregado por error


sjmisc::frq(data_2$m46_nhogar,
            out = "txt",
            show.na = T) %>% knitr::kable()

# A partir de esto nos preguntamos ¿De qué manera podemos incluir esta variable sin aumentar los NA?
# Además, la variable de ingreso total del hogar imputado tan solo tiene un 0,63% de NA, ¿Nos recomiendan quedarnos con esto o es mejor utilizar los inresos percapita?   




