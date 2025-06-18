# Análisis de datos multinivel
# Entrega 1: Análisis multinivel del estatus social subjetivo: el caso de la Región Metropolitana
# Integrantes: Victoria Arias, Cristóbal Mejías, Nicolas Outerbridge
# Fecha: 28/05/2025


#Librerías
library(pacman)

pacman::p_load(tidyverse,sjPlot,confintr,gginference,rempsyc,broom,sjmisc,lme4,
               reghelper,haven,stargazer,ggplot2,texreg,dplyr,knitr,summarytools,Publish,
               corrplot,readxl,ggfortify,sjlabelled,lmtest,sandwich,
               foreign, lattice, ggeffects, car) 

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls())       # para limpar el entonrno de trabajo


# Base de datos -----------------------------------------------------------

# BBDD ELSOC
load('input/data/ELSOC_Long_2016_2023.RData')
elsoc <- elsoc_long_2016_2023

# BBDD Pobreza multidimensional SAE
pobrezamultidimensional <- read_excel("input/data/Estimaciones_Indice_Pobreza_Multidimensional_Comunas_2022.xlsx")

pobreza_proc <- pobrezamultidimensional %>% 
  dplyr::select(cod_com,
                com= "Nombre comuna",
                reg="Región",
                pob_multi="Porcentaje de personas en situación de pobreza multidimensional 2022")

# BBDD Matriz de Bienestar Humano Territorial
mbht <- read_excel("output/mbht.xlsx")



# Juntas BBDD -------------------------------------------------------------

elsoc <- elsoc %>%
  rename(cod_com = comuna_cod) %>%
  mutate(cod_com = as.character(cod_com))

pobreza_proc <- pobreza_proc %>%
  mutate(cod_com = as.character(cod_com))

mbht <- mbht %>%
  mutate(cod_com = as.character(cod_com))


datos <- elsoc %>%
  left_join(pobreza_proc, by = "cod_com") %>%
  left_join(mbht, by = "cod_com")


# Explorar BBDD -----------------------------------------------------------

datos %>%
  group_by(ola) %>%
  summarise(N_valid = sum(!is.na(d01_01)))

datos %>%
  group_by(ola) %>%
  summarise(N_valid = sum(!is.na(d01_02)))

datos %>%
  group_by(ola) %>%
  summarise(N_valid = sum(!is.na(m29)))

datos %>%
  group_by(ola) %>%
  summarise(N_valid = sum(!is.na(m30)))

datos %>%
  filter(region_cod == 13) %>%
  group_by(ola) %>%
  summarise(N_personas_RM = n())


datos %>%
  filter(region_cod == 13) %>%
  group_by(ola) %>%
  summarise(n_comunas_RM = n_distinct(cod_com)) %>%
  arrange(desc(n_comunas_RM))

#Tras la revisión de estos datos, se opta por la ola


# Filtrar BBDD --------------------------------------------------

datos_proc <- datos %>%
  filter(ola==6) %>%
  select(idencuesta,
         region,
         region_cod,
         comuna,
         cod_com,
         sexo=m0_sexo, #demográficos
         edad=m0_edad,
         ess=d01_01, #variable dependiente
         ess_f=d01_02, #variables nivel 1
         edu=m01,
         inghogar=m29,
         inghogar_t=m30,
         pob_multi, #Variable nivel 2
         dim_seg,
         dim_amb,
         t08)


# -------------------------------------------------------------------------


# Filtrar ola 6
elsoc_ola6 <- elsoc %>% 
  filter(ola == 6)

# Seleccionar variables numéricas
numericas <- elsoc_ola6 %>%
  select(where(is.numeric))

# Eliminar variables con desviación estándar cero o solo NA
numericas <- numericas %>% 
  select(where(~ sd(., na.rm = TRUE) != 0 & !all(is.na(.))))

# Calcular correlaciones con d01_01
correlaciones <- sapply(numericas, function(x) cor(x, numericas$d01_01, use = "complete.obs"))

# Filtrar y ordenar correlaciones mayores a |0.18|
cor_filtradas <- correlaciones[abs(correlaciones) > 0.18 & names(correlaciones) != "d01_01"]
cor_filtradas <- sort(cor_filtradas, decreasing = TRUE)

# Mostrar resultados
print(cor_filtradas)






# Respecto a comunas ------------------------------------------------------

#Filtrar por Región Metropolitana 
datos_proc <- datos_proc %>% filter(region_cod== 13,
                                    cod_com != 2203) #se elimina dato aislado agregado por error


#Visualizar comunas
datos_proc %>%
  filter(region_cod == 13) %>%
  count(comuna, cod_com) %>%
  arrange(n)

# ¿Qué comunas quedaron fuera?

codigos_rm <- tribble(
  ~cod_com, ~comuna,
  "13101", "Santiago",
  "13102", "Cerrillos",
  "13103", "Cerro Navia",
  "13104", "Conchalí",
  "13105", "El Bosque",
  "13106", "Estación Central",
  "13107", "Huechuraba",
  "13108", "Independencia",
  "13109", "La Cisterna",
  "13110", "La Florida",
  "13111", "La Granja",
  "13112", "La Pintana",
  "13113", "La Reina",
  "13114", "Las Condes",
  "13115", "Lo Barnechea",
  "13116", "Lo Espejo",
  "13117", "Lo Prado",
  "13118", "Macul",
  "13119", "Maipú",
  "13120", "Ñuñoa",
  "13121", "Pedro Aguirre Cerda",
  "13122", "Peñalolén",
  "13123", "Providencia",
  "13124", "Pudahuel",
  "13125", "Quilicura",
  "13126", "Quinta Normal",
  "13127", "Recoleta",
  "13128", "Renca",
  "13129", "San Joaquín",
  "13130", "San Miguel",
  "13131", "San Ramón",
  "13132", "Vitacura",
  "13201", "Puente Alto",
  "13202", "Pirque",
  "13203", "San José de Maipo",
  "13301", "Colina",
  "13302", "Lampa",
  "13303", "Tiltil",
  "13401", "San Bernardo",
  "13402", "Buin",
  "13403", "Calera de Tango",
  "13404", "Paine",
  "13501", "Melipilla",
  "13502", "Alhué",
  "13503", "Curacaví",
  "13504", "María Pinto",
  "13505", "San Pedro",
  "13601", "Talagante",
  "13602", "El Monte",
  "13603", "Isla de Maipo",
  "13604", "Padre Hurtado",
  "13605", "Peñaflor")


# Comunas que faltan

comunas_faltantes <- codigos_rm %>%
  anti_join(datos_proc, by = "cod_com")

print(comunas_faltantes)



# Visualización BBDD ------------------------------------------------------

view(dfSummary(datos_proc, headings = FALSE, method = "render"))
view_df(datos_proc,max.len = 100)



# Agregar Labels ----------------------------------------------------------

datos_proc$cod_com <- set_label(datos_proc$cod_com,"Comuna (Codigo)")
datos_proc$ess <- set_label(datos_proc$ess,"Estatus Social Subjetivo Individual")
datos_proc$ess_f <- set_label(datos_proc$ess_f,"Estatus Social Subjetivo Familiar")


# Tratamiento variable sexo -----------------------------------------------

datos_proc <- datos_proc %>%
  mutate(
    sexo = car::recode(sexo, recodes = "'1' = 'Hombre'; '2' = 'Mujer'"),
    sexo = factor(sexo, levels = c("Hombre", "Mujer")))

datos_proc$sexo <- set_label(datos_proc$sexo,"Sexo del entrevistado")

# Tratamiendo Variable ESS  ---------------------------------------

datos_proc <- mutate(datos_proc,
                     ess = na_if(ess, -999),
                     ess = na_if(ess, -888),
                     ess = na_if(ess, -777),
                     ess = na_if(ess, -666))

colSums(is.na(datos_proc))

# Tratamiendo Variable ESS Familiar ---------------------------------------

datos_proc <- mutate(datos_proc,
                     ess_f = na_if(ess_f, -999),
                     ess_f = na_if(ess_f, -888),
                     ess_f = na_if(ess_f, -777),
                     ess_f = na_if(ess_f, -666))

colSums(is.na(datos_proc))


#Centrado ESS Familiar

datos_proc <- datos_proc %>%
  group_by(cod_com) %>%
  mutate(ess_f_cmc = ess_f - mean(ess_f, na.rm = TRUE)) %>%
  ungroup()


# Tratamiento variable educación ------------------------------------------

datos_proc <- datos_proc %>%
  mutate(
    edu = case_when(
      edu %in% c(-999, -888, -777, -666) ~ NA_character_,
      edu == 1 ~ "Sin estudios",
      edu == 2 ~ "Educacion Basica o Preparatoria incompleta",
      edu == 3 ~ "Educacion Basica o Preparatoria completa",
      edu == 4 ~ "Educacion Media o Humanidades incompleta",
      edu == 5 ~ "Educacion Media o Humanidades completa",
      edu == 6 ~ "Tecnica Superior incompleta",
      edu == 7 ~ "Tecnica Superior completa",
      edu == 8 ~ "Universitaria incompleta",
      edu == 9 ~ "Universitaria completa",
      edu == 10 ~ "Estudios de posgrado (magister o doctorado)",
      TRUE ~ NA_character_
    ),
    edu = factor(edu, levels = c(
      "Sin estudios",
      "Educacion Basica o Preparatoria incompleta",
      "Educacion Basica o Preparatoria completa",
      "Educacion Media o Humanidades incompleta",
      "Educacion Media o Humanidades completa",
      "Tecnica Superior incompleta",
      "Tecnica Superior completa",
      "Universitaria incompleta",
      "Universitaria completa",
      "Estudios de posgrado (magister o doctorado)")))

datos_proc$edu <- set_label(datos_proc$edu,"Nivel Educativo")

colSums(is.na(datos_proc))


# Tratamiento variable ingresos -------------------------------------------

datos_proc <- mutate(datos_proc,
                     inghogar = na_if(inghogar, -999),
                     inghogar = na_if(inghogar, -888),
                     inghogar = na_if(inghogar, -777),
                     inghogar = na_if(inghogar, -666))

colSums(is.na(datos_proc))



#Creación de nueva variable de ingreso imputada

datos_proc$inghogar_i <- ifelse(test = (is.na(datos_proc$inghogar)), #¿existen NA en ingresos?
                                yes = datos_proc$inghogar_t,         #VERDADERO, remplazar con la media del tramo
                                no = datos_proc$inghogar)            #FALSE, mantener la variable original.

datos_proc$inghogar_i <- set_label(datos_proc$inghogar_i,"Ingreso total del hogar (imputada)")


sjmisc::descr(datos_proc[,c("inghogar","inghogar_i")],
              show =c("label", "n", "NA.prc", "mean", "md","sd")) %>% knitr::kable(digits = 2)


datos_proc <- mutate(datos_proc,
                     inghogar_i = na_if(inghogar_i, -999),
                     inghogar_i = na_if(inghogar_i, -888),
                     inghogar_i = na_if(inghogar_i, -777),
                     inghogar_i = na_if(inghogar_i, -666))

colSums(is.na(datos_proc))


# Tratamiendo variable ingreso imputada


#Opción 1
datos_proc$quintil_inghogar <- ntile(datos_proc$inghogar_i, 5)

#Opción 2
datos_proc$quintil_inghogar2<- dplyr::ntile(x = datos_proc$inghogar_i,
                                            n = 5) # n de categorias, para quintiles usamos 5
datos_proc$quintil_inghogar2 <- factor(datos_proc$quintil_inghogar2,c(1,2,3,4,5), c("Quintil 1","Quintil 2","Quintil 3","Quintil 4","Quintil 5"))
datos_proc %>%
  group_by(quintil_inghogar2) %>%
  summarise(n=n(),
            Media=mean(inghogar_i,na.rm = T),
            Mediana=median(inghogar_i,na.rm = T)) %>%
  knitr::kable()

#Opción 3
datos_proc$log_ing_i <- log(datos_proc$inghogar_i + 1)


# T08 ---------------------------------------------------------------------

datos_proc <- mutate(datos_proc,
                     t08 = na_if(t08, -999),
                     t08 = na_if(t08, -888),
                     t08 = na_if(t08, -777),
                     t08 = na_if(t08, -666))

colSums(is.na(datos_proc))

datos_proc = datos_proc %>%  
  group_by(cod_com) %>% 
  mutate(meant08 = mean(t08, na.rm = TRUE))

## Remover NA's ----------------------------------------------------------------

datos_proc <- datos_proc %>%
  filter(if_all(-c(inghogar_t, inghogar), ~ !is.na(.)))



# Variable Pobreza Multidimensional ---------------------------------------

descr(datos_proc$pob_multi,style = "rmarkdown",stats = "common", transpose = T,headings = F)

datos_proc$pob_multi <- set_label(datos_proc$pob_multi,"Pobreza multidimensional")

#Centrado:
# datos_proc <- datos_proc %>% 
# mutate(pob.gmc = pob_multi-mean(pob_multi))

#        #Centrar un predictor en la media general
#        pob.c.alt = scale(pob_multi, center=T, scale=F), #Otra forma de hacer el centrado en la media general (GMC)
#        pob_multi.gm = mean(pob_multi)) %>%  #Obtener la media general y agregarla a cada observación
# group_by(cod_com) %>% 
# mutate(pob.g.home = mean(pob_multi), #Obtener la media del grupo para el predictor
#        meanpob.gmc = pob.g.home - pob_multi.gm) %>% #Centrar el predictor de nivel 2 (L2)
# ungroup()

datos_proc <- datos_proc %>%
  mutate(pob_multi.gmc = pob_multi - mean(pob_multi))

# Variables MBHT ----------------------------------------------------------

sjmisc::descr(datos_proc[,c("dim_amb", "dim_seg" )],
              show =c("label", "n", "NA.prc", "mean", "md","sd")) %>% knitr::kable(digits = 2)



datos_proc$dim_amb <- set_label(datos_proc$dim_amb,"BHT-Dimension Ambiental")

datos_proc$dim_seg <- set_label(datos_proc$dim_seg,"BHT-Dimension Seguridad")


# Visualización nuevamente ------------------------------------------------

view(dfSummary(datos_proc, headings = FALSE, method = "render"))
view_df(datos_proc,max.len = 100)



# Descriptivos ------------------------------------------------------------

descr(datos_proc$pob_multi,style = "rmarkdown",stats = "common", transpose = T,headings = F)





# Análisis bivariado ------------------------------------------------------

# view(dfSummary(datos_proc, headings=FALSE, graph.col = FALSE))
# 
dat_scat=datos_proc %>% group_by(comuna) %>% select(ess,pob_multidim.gmc) %>% na.omit() %>% summarise_all(mean)
names(dat_scat)

sjPlot::plot_scatter(dat_scat, ess,pob_multidim.gmc,
                     dot.labels = to_label(dat_scat$comuna),
                     fit.line = "lm",
                     show.ci = TRUE)

scatterplot(datos_proc$ess_f ~ datos_proc$pob_multi, data=datos_proc, xlab="ESS_f", ylab="Pobreza Multidimensional", main="Math on SES", smooth=FALSE)



# Matriz Corr -------------------------------------------------------------

datos_proc <- datos_proc %>%
  mutate(edu_num = as.numeric(edu))


cormat=datos_proc %>% select(ess,ess_f_cmc, quintil_inghogar, pob_multi, dim_seg, dim_amb, t08) %>% cor()
round(cormat, digits=2)

corrplot.mixed(cormat)


datos_proc %>%
  select(ess, ess_f_cmc, edu_num, quintil_inghogar2, pob_multi, dim_seg, dim_amb) %>%
  summarise(across(everything(), class))



cor(select(datos_proc, ess_f_cmc, quintil_inghogar, pob_multi, dim_seg, dim_amb), use = "complete.obs")

datos_proc <- datos_proc %>%
  mutate(across(c(ess_f, pob_multi, dim_seg, dim_amb), scale))




# Análisis multinivel -----------------------------------------------------


# Modelo Nulo -------------------------------------------------------------
agg_data=datos_proc %>% group_by(cod_com) %>% summarise_all(funs(mean)) %>% as.data.frame()

model0 = lmer(ess ~ 1 + (1 | cod_com), data = datos_proc)
screenreg(model0)

ICC<-reghelper::ICC(model0)
ICC*100

# Modelo 1: Predictores de nivel individual -------------------------------

model1 = lmer(ess ~ 1 + ess_f_cmc  + edu_num + quintil_inghogar2 + t08 + (1 | cod_com), data = datos_proc)
screenreg(model1, naive=TRUE)

# Modelo 2: Predictores nivel 2 -------------------------------------------

model2 = lmer(ess ~ 1 + pob_multi.gmc +  dim_seg  + dim_amb +  (1 | cod_com), data = datos_proc)
screenreg(model2)

model2b = lmer(ess ~ 1 + pob_multidim.gmc +  dim_seg  + dim_amb +  (1 | cod_com), data = datos_proc)
screenreg(model2b)

# Modelo 3: Predictores individuales y grupales ---------------------------

model3 = lmer(ess ~ 1 + ess_f_cmc  + edu_num + quintil_inghogar2 + t08 +  pob_multi.gmc + dim_seg  + dim_amb + (1 | cod_com), data = datos_proc)
screenreg(model3)

# Modelo 4: Pendiente aleatoria -------------------------------------------

# Modelo con pendiente aleatoria
model4= lmer(ess ~ 1 + ess_f_cmc  + edu_num + quintil_inghogar2 + t08 + pob_multi.gmc + dim_seg  + dim_amb + (1 + ess_f_cmc| cod_com), data = datos_proc)
screenreg(model4)


#Comparación pendiete fija y aleatoria con Anova
anova(model3,model4)

# Modelo 5: Interacción entre niveles -------------------------------------

model5 = lmer(ess ~ 1 + ess_f_cmc  + edu_num + quintil_inghogar2 + t08 + ess_f_cmc*pob_multi.gmc  + dim_seg  + dim_amb + (1 + ess_f_cmc| cod_com), data = datos_proc)
screenreg(model5)

# Comparación entre modelos -----------------------------------------------

sjPlot::tab_model(model0, model1, model2, model3, model4, model5, dv.labels = c("Nulo ","Individual","Grupal", "Individual y Grupal", "Pendiente Aleatoria", "Interacción"), show.ci = FALSE)



# Gráficos ----------------------------------------------------------------


#Modelo con predictores fijos

reg_fij=lmer(ess ~ 1 + ess_f_cmc  + edu_num + quintil_inghogar2 + pob_multi.gmc + dim_seg  + dim_amb + (1 | comuna), data = datos_proc)

datos_proc$ess_fijo <- predict(reg_fij)
datos_proc %>%  
  ggplot(aes(ess_f_cmc, ess_fijo, color = comuna, group = comuna)) + 
  geom_smooth(se = F, method = lm)

graf_fij <- ggpredict(reg_fij, terms = c("ess_f_cmc","comuna [sample=5]"), type="random")
plot(graf_fij)

# Modelo con predictores aleatorios

reg_aleat=lmer(ess ~ 1 + ess_f_cmc  + edu_num + quintil_inghogar2 + pob_multi.gmc + dim_seg  + dim_amb + (1 + ess_f_cmc| comuna), data = datos_proc)
graf_aleat=ggpredict(reg_aleat, terms = c("ess_f_cmc","comuna [sample=3]"), type="random")
plot(graf_aleat)


# Sección interacción -----------------------------------------------------

plot_model(model5, type = "int")



# Guardar BBDD ------------------------------------------------------------

saveRDS(datos_proc, file = "output/base_proc.Rdata")

