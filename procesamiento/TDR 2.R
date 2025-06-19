# Análisis de datos multinivel
# Entrega 2: Análisis multinivel del estatus social subjetivo: el caso de la Región Metropolitana
# Integrantes: Victoria Arias y Cristóbal Mejías
# Fecha: 01/07/2025


# Cargar Librerías --------------------------------------------------------

library(pacman)

pacman::p_load(tidyverse,sjPlot,confintr,gginference,rempsyc,broom,sjmisc,lme4,
               reghelper,haven,stargazer,ggplot2,texreg,dplyr,knitr,summarytools,Publish,
               corrplot,readxl,ggfortify,sjlabelled,lmtest,sandwich,
               foreign, lattice, ggeffects, car, lme4, lmerTest, influence.ME, tidyr ) 


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
mbht <- read_excel("input/data/mbht.xlsx")

#Juntas BBDD -------------------------------------------------------------

elsoc <- elsoc %>%
  rename(cod_com = comuna_cod) %>%
  mutate(cod_com = as.character(cod_com))

pobreza_proc <- pobreza_proc %>%
  mutate(cod_com = as.character(cod_com))

mbht <- mbht %>%
  mutate(cod_com = as.character(cod_com))


base <- elsoc %>%
  left_join(pobreza_proc, by = "cod_com") %>%
  left_join(mbht, by = "cod_com")

# Filtrar BBDD --------------------------------------------------

data <- base %>%
  filter(ola==6) %>%
  select(idencuesta,
         region,
         region_cod,
         comuna,
         cod_com,
         sexo=m0_sexo, #contról
         edad=m0_edad,
         ess=d01_01, #variable dependiente
         ess_f=d01_02, #variables nivel 1
         edu=m01,
         inghogar=m29,
         inghogar_t=m30,
         pob_multi, #Variables nivel 2
         dim_seg,
         dim_amb)

# Análisis Cluster ------------------------------------------------------

#Filtrar por Región Metropolitana 
data <- data %>% filter(region_cod== 13,
                        cod_com != 2203) #se elimina dato aislado agregado por error


#Visualizar comunas
data %>%
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


comunas_faltantes <- codigos_rm %>%
  anti_join(data, by = "cod_com")

print(comunas_faltantes)

# Visualización BBDD ------------------------------------------------------

view(dfSummary(data, headings = FALSE, method = "render"))
view_df(data,max.len = 100)

# Tratamiento variable sexo -----------------------------------------------

data <- data %>%
  mutate(
    sexo = car::recode(sexo, recodes = "'1' = 'Hombre'; '2' = 'Mujer'"),
    sexo = factor(sexo, levels = c("Hombre", "Mujer")))

data$sexo <- set_label(data$sexo,"Sexo del entrevistado")

# Tratamiendo Variable ESS  ---------------------------------------

data <- mutate(data,
               ess = na_if(ess, -999),
               ess = na_if(ess, -888),
               ess = na_if(ess, -777),
               ess = na_if(ess, -666))

colSums(is.na(data))

data$ess <- set_label(data$ess,"Estatus Social Subjetivo Individual")

# Tratamiendo Variable ESS Familiar ---------------------------------------

data <- mutate(data,
               ess_f = na_if(ess_f, -999),
               ess_f = na_if(ess_f, -888),
               ess_f = na_if(ess_f, -777),
               ess_f = na_if(ess_f, -666))

colSums(is.na(data))

data$ess_f <- set_label(data$ess_f,"Estatus Social Subjetivo Familiar")

#Centrado ESS Familiar
data <- data %>%
  group_by(cod_com) %>%
  mutate(ess_f_cmc = ess_f - mean(ess_f, na.rm = TRUE)) %>%
  ungroup()

data$ess_f_cmc <- set_label(data$ess_f_cmc,"Estatus Social Subjetivo Familiar Centrada")

#Visualizar diferencias

descr(data$ess_f,style = "rmarkdown",stats = "common", transpose = T,headings = F)
descr(data$ess_f_cmc,style = "rmarkdown",stats = "common", transpose = T,headings = F)

sjmisc::descr(data[,c("ess_f","ess_f_cmc")],
              show =c("label", "n", "NA.prc", "mean", "md","sd")) %>% knitr::kable(digits = 2)


# Tratamiento variable educación ------------------------------------------

data <- data %>%
  mutate(edu = car::recode(edu,
                           recodes = "-999 = NA;
                           -888 = NA;
                           -777 = NA;
                           -666 = NA;
                           1 = 'Sin estudios';
                           2 = 'Educacion Basica o Preparatoria incompleta';
                           3 = 'Educacion Basica o Preparatoria completa';
                           4 = 'Educacion Media o Humanidades incompleta';
                           5 = 'Educacion Media o Humanidades completa';
                           6 = 'Tecnica Superior incompleta';
                           7 = 'Tecnica Superior completa';
                           8 = 'Universitaria incompleta';
                           9 = 'Universitaria completa';
                           10 = 'Estudios de posgrado (magister o doctorado)'",
                           as.factor = TRUE,
                           levels = c(
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


data <- data %>%
  mutate(
    edu_univ = case_when(
      edu %in% c("Universitaria completa", "Estudios de posgrado (magister o doctorado)") ~ "Universitaria o más",
      !is.na(edu) ~ "Menos que universitaria",
      TRUE ~ NA_character_),
    edu_univ = factor(edu_univ, levels = c("Menos que universitaria", "Universitaria o más")))


colSums(is.na(data))

# Tratamiento variable ingresos -------------------------------------------

data <- mutate(data,
               inghogar = na_if(inghogar, -999),
               inghogar = na_if(inghogar, -888),
               inghogar = na_if(inghogar, -777),
               inghogar = na_if(inghogar, -666))

colSums(is.na(data))



#Creación de nueva variable de ingreso imputada

data$inghogar_i <- ifelse(test = (is.na(data$inghogar)), #¿existen NA en ingresos?
                          yes = data$inghogar_t,         #VERDADERO, remplazar con la media del tramo
                          no = data$inghogar)            #FALSE, mantener la variable original.

data$inghogar_i <- set_label(data$inghogar_i,"Ingreso total del hogar (imputada)")

data <- mutate(data,
               inghogar_i = na_if(inghogar_i, -999),
               inghogar_i = na_if(inghogar_i, -888),
               inghogar_i = na_if(inghogar_i, -777),
               inghogar_i = na_if(inghogar_i, -666))

colSums(is.na(data))

# Tratamiendo variable ingreso imputada

# Escalar ingreso imputado dividiendo por 10.000
data$inghogar_i_mil <- data$inghogar_i / 10000

data$inghogar_i_mil <- set_label(data$inghogar_i_mil,"Ingreso total del hogar (en decenas de mil CLP)")


# Tratamiento variable pobreza multinivel ---------------------------------

#Centrado
data <- data %>%
  mutate(pob_multi_gm = mean(pob_multi),                 # Media general (nivel 2)
         pob_multi_gmc = pob_multi - pob_multi_gm)       # Centrado a nivel 2 (GMC)

#Visualizar diferencias

descr(data$pob_multi,style = "rmarkdown",stats = "common", transpose = T,headings = F)
descr(data$pob_multi_gmc,style = "rmarkdown",stats = "common", transpose = T,headings = F)

sjmisc::descr(data[,c("pob_multi","pob_multi_gmc")],
              show =c("label", "n", "NA.prc", "mean", "md","sd")) %>% knitr::kable(digits = 2)

data$pob_multi <- set_label(data$pob_multi,"Pobreza multidimensional")
data$pob_multi_gmc <- set_label(data$pob_multi_gmc,"Pobreza multidimensional Centrada")

# Variables MBHT ----------------------------------------------------------

sjmisc::descr(data[,c("dim_amb", "dim_seg" )],
              show =c("label", "n", "NA.prc", "mean", "md","sd")) %>% knitr::kable(digits = 2)



data$dim_amb <- set_label(data$dim_amb,"BHT-Dimension Ambiental")
data$dim_seg <- set_label(data$dim_seg,"BHT-Dimension Seguridad")


# Remover NA's ----------------------------------------------------------------

data <- data %>%
  filter(if_all(-c(inghogar_t, inghogar), ~ !is.na(.)))

# Visualización  ------------------------------------------------

view(dfSummary(data, headings = FALSE, method = "render"))
view_df(data,max.len = 100)


# Descriptivos ------------------------------------------------------------

data %>%  select (ess, ess_f, ess_f_cmc, edu, edu_univ,inghogar_i, inghogar_i_mil) %>% sjmisc::descr(.,
                                                                                                     show = c("label","range", "mean", "sd", "NA.prc", "n"))%>%
  kable(., digits =2, "markdown", caption = "Variables nivel 1")


data %>%
  select(comuna, pob_multi_gmc, dim_amb, dim_seg) %>%
  distinct() %>%
  sjmisc::descr(., show = c("label","range", "mean", "sd", "NA.prc", "n")) %>%
  kable(digits = 2, format = "markdown", caption = "Variables nivel 2")


# Bivariados --------------------------------------------------------------

cormat1 = data %>% select(ess,ess_f, inghogar_i, pob_multi, dim_seg, dim_amb) %>% cor()
round(cormat1, digits=2)
corrplot.mixed(cormat1)


# Modelo multinivel: Modelo nulo -------------------------------------------------------

agg_data=data %>% group_by(cod_com) %>% summarise_all(funs(mean)) %>% as.data.frame()

model0 = lmer(ess ~ 1 + (1 | cod_com), data = data)
screenreg(model0)

ICC<-reghelper::ICC(model0)
ICC*100

#Modelo 1: Predictores de nivel individual -------------------------------

model1 = lmer(ess ~ 1 + ess_f_cmc  + edu_univ + inghogar_i_mil + (1 | cod_com), data = data)
screenreg(model1, naive=TRUE)

# Modelo 2: Predictores nivel 2 -------------------------------------------

model2 = lmer(ess ~ 1 + pob_multi_gmc +  dim_seg  + dim_amb +  (1 | cod_com), data = data)
screenreg(model2)

# Modelo 3: Predictores individuales y grupales ---------------------------

model3 = lmer(ess ~ 1 + ess_f_cmc  + edu_univ  + inghogar_i_mil + pob_multi_gmc + dim_seg  + dim_amb + (1 | cod_com), data = data)
screenreg(model3)

#Modelo 4: Pendiente aleatoria -------------------------------------------

model4= lmer(ess ~ 1 + ess_f_cmc  + edu_univ + inghogar_i_mil + pob_multi_gmc + dim_seg  + dim_amb + (1 + ess_f_cmc| cod_com), data = data)
screenreg(model4)

#Comparación pendiete fija y aleatoria con Anova
anova(model3,model4)

# Modelo 5: Interacción entre niveles -------------------------------------

model5 = lmer(ess ~ 1 + ess_f_cmc  + edu_univ + inghogar_i_mil + ess_f_cmc*pob_multi_gmc  + dim_seg  + dim_amb + edad + sexo + (1 + ess_f_cmc| cod_com), data = data)
screenreg(model5)

# Comparación entre modelos -----------------------------------------------

sjPlot::tab_model(
  model0, model1, model2, model3, model4, model5,
  dv.labels = c("Nulo", "Individual", "Grupal", "Individual y Grupal", "Pendiente Aleatoria", "Interacción"),
  show.ci = FALSE,
  file = "output/modelos_tabla.html"
)

# Casos influyentes  -----------------------------------------------------

estex.m3 <- influence(model3, "cod_com")
estex.m4 <- influence(model4, "cod_com") 
estex.m5 <- influence(model5, "cod_com") 

cooks.distance(estex.m3, sort = TRUE)
cooks.distance(estex.m4, sort = TRUE)
cooks.distance(estex.m5, sort = TRUE)
4/42 # cut point: 4/ 42 comunas

plot(estex.m3, which="cook",
     cutoff=.09, sort=TRUE,
     xlab="Cooks Distance",
     ylab="cod_com")

plot(estex.m4, which="cook",
     cutoff=.09, sort=TRUE,
     xlab="Cooks Distance",
     ylab="cod_com")

plot(estex.m5, which="cook",
     cutoff=.09, sort=TRUE,
     xlab="Cooks Distance",
     ylab="cod_com")

sigtest(estex.m3, test=-1.96)$pob_multi_gmc[1:10,]
sigtest(estex.m4, test=-1.96)$pob_multi_gmc[1:10,]
sigtest(estex.m5, test=-1.96)$pob_multi_gmc[1:10,]
#No existen casos influyentes

# Gráficos fijos y aleatorias ----------------------------------------------------------------

#Modelo con predictores fijos

reg_fij=lmer(ess ~ 1 + ess_f_cmc  + edu_univ  + inghogar_i_mil + pob_multi_gmc + dim_seg  + dim_amb + (1 | comuna), data = data)

data$ess_fijo <- predict(reg_fij)
data %>%  
  ggplot(aes(ess_f_cmc, ess_fijo, color = comuna, group = comuna)) + 
  geom_smooth(se = F, method = lm)


graf_fij <- ggpredict(reg_fij, terms = c("ess_f_cmc", "comuna [sample=5]"), type = "random")

plot(graf_fij) +
  labs(
    title = "Predicciones del Estatus Social Subjetivo Individual",
    subtitle = "Según el Estatus Familiar Centrado, por Comuna (efectos fijos)",
    x = "Estatus Social Subjetivo Familiar Centrado",
    y = "Estatus Social Subjetivo Individual",
    color = "Comuna"
  ) +
  scale_color_brewer(palette = "Set1") + 
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.title = element_text(size = 13),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank())

ggsave("output/grafico_efecto_fijo.png", width = 8, height = 6, dpi = 300)

# Modelo con predictores aleatorios

reg_aleat=lmer(ess ~ 1 + ess_f_cmc  + edu_univ + inghogar_i_mil + pob_multi_gmc + dim_seg  + dim_amb + (1 + ess_f_cmc| comuna), data = data)

graf_aleat=ggpredict(reg_aleat, terms = c("ess_f_cmc","comuna [sample=3]"), type="random")


plot(graf_aleat) +
  labs(
    title = "Predicciones del Estatus Social Subjetivo Individual",
    subtitle = "Según Estatus Familiar Centrado, por Comuna (efectos aleatorios)",
    x = "Estatus Social Subjetivo Familiar Centrado",
    y = "Estatus Social Subjetivo Individual",
    color = "Comuna"
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.title = element_text(size = 13),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank())

ggsave("output/grafico_efecto_aleatorio.png", width = 8, height = 6, dpi = 300)


# Gráfico de interacción entre ess_f_cmc y pob_multi_gmc
plot_model(model5, type = "int", terms = c("ess_f_cmc", "pob_multi_gmc")) +
  labs(
    title = "Interacción entre ESS Familiar y Pobreza Multidimensional Comunal",
    x = "ESS Familiar Centrado",
    y = "ESS Individual",
    color = "Pobreza Multidimensional"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 12),
    legend.position = "bottom")

ggsave("output/grafico_interacción.png", width = 8, height = 6, dpi = 300)


# Guardar BBDD ------------------------------------------------------------

saveRDS(data, file = "output/data_proc.Rdata")


