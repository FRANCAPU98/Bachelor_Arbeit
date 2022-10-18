library(ggplot2)
library(raster)
library(dplyr)
library(readr)
library(haven)
library(utils)
library(sf)
library(sp)
library(psych)
library(tidyverse)
library(RColorBrewer)
library(lingmatch)

# CENSO 2017 #####

Diccionario <- file.choose(read.dic())

#### INEI CÓDIFICACION ####

#Codigo de Actividad Economica Negocio
ENAHO_TABLA_CIIU_REV4.sav <- read_sav(file.choose()) #2018
ENAHO_TABLA_CIIU_REV3.sav <- read_sav(file.choose()) #2015

# Codigo de oficios 
ENAHO_TABLA_CIUO_88 <- read_sav(file.choose())  #2018  en Empleo e ingresos P505
ENAHO_TABLA_CNO_2015.sav <- read_sav(file.choose()) # 2015

#### ENAHO 2018 ####
## Caracteristicas del Hogar ( ENAHO01-2018-100.SAV ) ##
Enaho01_2018_100.sav <- read_sav(file.choose())
names(Enaho01_2018_100.sav)
nrow(Enaho01_2018_100.sav)

## Caracteristicas de miembros del hogar (ENAHO01-2018-200.SAV) ##
ENAHO01_2018_200.sav <- read_sav(file.choose())
names(ENAHO01_2018_200.sav)
nrow(ENAHO01_2018_200.sav)

## Empleo e Ingreso ( ENAHO01A-2018-500.SAV ) ## 
Enaho01A_2018_500.sav <- read_sav(file.choose())
View(Enaho01A_2018_500.sav$FAC500A)
names(Enaho01A_2018_500.sav)
nrow(Enaho01A_2018_500.sav)

##  Transportes y Comunicaciones ( ENAHO01-2018-604.SAV ) ##
Enaho01_2018_604.sav <- read_sav(file.choose())
View(Enaho01_2018_604.sav)
names(Enaho01_2018_604.sav)
nrow(Enaho01_2018_604.sav)

#### ENAHO 2019 ####
## Caracteristicas del Hogar ( ENAHO01-2019-100.SAV ) ##
Enaho01_2019_100.sav <- read_sav(file.choose())
names(Enaho01_2019_100.sav)

## Caracteristicas de miembros del hogar (ENAHO01-2019-200.SAV) ##
ENAHO01_2019_200.sav <- read_sav(file.choose())
names(ENAHO01_2019_200.sav)

## Empleo e Ingreso ( ENAHO01A-2019-500.SAV ) ## 
Enaho01A_2019_500.sav <- read_sav(file.choose())
nrow(Enaho01A_2019_500.sav)
##  Transportes y Comunicaciones ( ENAHO01-2019-604.SAV ) ##
Enaho01_2019_604.sav <- read_sav(file.choose())

#### ENAHO 2020 ####
## Caracteristicas del Hogar ( ENAHO01-2020-100.SAV ) ##
Enaho01_2020_100.sav <- read_sav(file.choose())
names(Enaho01_2020_100.sav)

## Caracteristicas de miembros del hogar (ENAHO01-2020-200.SAV) ##
ENAHO01_2020_200.sav <- read_sav(file.choose())

## Empleo e Ingreso ( ENAHO01A-2020-500.SAV ) ## 
Enaho01A_2020_500.sav <- read_sav(file.choose())
names(Enaho01A_2020_500.sav)
##  Transportes y Comunicaciones ( ENAHO01-2020-604.SAV ) ##
Enaho01_2020_604.sav <- read_sav(file.choose())


#### Trabajo con los Ingresos ####
# Seleccionar ingreso ::: P524A1 == Income P524A1 en AMLC
#Para la adapación de Precios se usará el ano 2019 como una muestra grande

#Ingresos 2018
P524A1_18 <- Enaho01A_2018_500.sav[which(Enaho01A_2018_500.sav$DOMINIO == 
                                           8),][,c(1:19, 103, "FAC500A" )] %>% na.omit()
#Ingresos 2019 ANO BASE
P524A1_19 <- Enaho01A_2019_500.sav[which(Enaho01A_2019_500.sav$DOMINIO == 
                                           8),][,c(1:19, 103)] %>% na.omit()
#Ingresos 2020
P524A1_20 <-  Enaho01A_2020_500.sav[which(Enaho01A_2020_500.sav$DOMINIO == 
                                           8),][,c(1:19, 107)] %>% na.omit()


## Antes de unificacion de los sueldos, estos deben ser adaptados
#2018#
#Correcion del ingreso al 2019 con ayuda de las proyecccion
#Supocisión Apatación de ingresos usando cuantiles  #### Datos de la correcion del Evolucion de la POBREZA MONETARIA 2008-19 P.25
quantile(P524A1_18$P524A1, probs = seq(0, 1, .1), na.rm = T, names = F)
#Percentiles
P524A1_18 <- P524A1_18 %>% mutate(income_quq = ifelse( P524A1 < 200, 1, 
                                                           ifelse( P524A1 >= 200 & P524A1 < 400 , 2,
                                                                   ifelse(P524A1 >= 400 & P524A1 < 800,3,
                                                                          ifelse(P524A1 >= 800 & P524A1<950,4, 
                                                                                 ifelse(P524A1 >= 950 & P524A1<1200, 5,
                                                                                        ifelse(P524A1 >= 1200 & P524A1< 1400,6,
                                                                                               ifelse(P524A1 >= 1400 & P524A1 <1700, 7, 
                                                                                                      ifelse(P524A1 >= 1700 & P524A1 <2200, 8,
                                                                                                             ifelse(P524A1 >= 2200 & P524A1 <3500, 9, 10))))))))))

#Correction
P524A1_18_act <- P524A1_18 %>% mutate(income_adp =ifelse( income_quq == 1,P524A1*(1-0.022) , 
                                                          ifelse( income_quq == 2 ,P524A1*(1-0.01),
                                                                  ifelse(income_quq == 3,P524A1*1.017,
                                                                         ifelse(income_quq == 4 ,P524A1*1.028, 
                                                                                ifelse(income_quq == 5, P524A1*1.011,
                                                                                       ifelse(income_quq == 6,P524A1*(1-0.012),
                                                                                              ifelse(income_quq == 7, P524A1*(1-0.021), 
                                                                                                     ifelse(income_quq == 8, P524A1*(1-0.028),
                                                                                                            ifelse(income_quq == 9, P524A1*(1-0.022), P524A1*(1-0.009)))))))))))
#Quantiles y variacion interanual
#1 // select(P524A1 < 200)*(1-0.022)
#2 // select(P524A1 >= 200 & P524A1<400)*(1-0.01)
#3 // select(P524A1 >= 400 & P524A1<800)*1.017
#4 // select(P524A1 >= 400 & P524A1<800)*1.028
#5 // select(P524A1 >= 950 & P524A1<1200)*1.011
#6 // select(P524A1 >= 1200 & P524A1<1400)*(1-0.012)
#7 // select(P524A1 >= 1400 & P524A1<1700)*(1-0.021)
#8 //select(P524A1 >= 1700 & P524A1<2200)*(1-0.028)
#9 // select(P524A1 >= 2200 & P524A1<3500)*(1-0.022)
#10//select(P524A1 >= 3500 & P524A1<22000)*(1-0.009)

#2020#
#Correcion del ingreso al 2019 con ayuda de las proyecccion
#Supocisión Apatación de ingresos usando cuantiles #### Datos de la correcion del Evolucion de la POBREZA MONETARIA 2008-20 P.26
quantile(P524A1_20$P524A1, probs = seq(0, 1, .1), na.rm = T, names = F)
#Percentiles
P524A1_20 <- 
  P524A1_20 %>% 
  mutate(income_quq = 
           ifelse( P524A1 < 180, 1,
                   ifelse( P524A1 >= 180 &
                             P524A1 < 350 , 2,
                           ifelse(P524A1 >= 350 & 
                                    P524A1 < 760,3,
                                  ifelse(P524A1 >= 760 & 
                                           P524A1<950,4,
                                         ifelse(P524A1 >= 950 & 
                                                  P524A1<1200, 5,
                                                ifelse(P524A1 >= 1200 & 
                                                         P524A1< 1800,6,
                                                       ifelse(P524A1 >= 1800 &
                                                                P524A1 <2500, 7,
                                                              ifelse(P524A1 >= 2500 &
                                                                       P524A1 <3680, 8,
                                                                     ifelse(P524A1 >= 3680 &
                                                                              P524A1 < 20300, 9, 10))))))))))


#Correction
P524A1_20_act <- P524A1_20 %>% mutate(income_adp =ifelse( income_quq == 1,P524A1*1.551 , 
                                                          ifelse( income_quq == 2 ,P524A1*1.42,
                                                                  ifelse(income_quq == 3,P524A1*1.382,
                                                                         ifelse(income_quq == 4 ,P524A1*1.361, 
                                                                                ifelse(income_quq == 5, P524A1*1.328,
                                                                                       ifelse(income_quq == 6,P524A1*1.298,
                                                                                              ifelse(income_quq == 7, P524A1*1.27, 
                                                                                                     ifelse(income_quq == 8, P524A1*1.252,
                                                                                                            ifelse(income_quq == 9, P524A1*1.223, P524A1*1.176))))))))))

# Unificación de la bases de datos al ano 2019#
colnames(P524A1_19)[colnames(P524A1_19) == "P524A1"] <- "income_adp" 

P524A1_20_all <- rbind(P524A1_19 %>% select(-NCONGLOME), P524A1_20_act %>% 
                         select(-P501,-P524A1,-income_quq))


P524A1_all_19 <- rbind(P524A1_20_all,  P524A1_18_act %>% 
                         select(-NCONGLOME,-P524A1,-income_quq))

P524A1_all_19$income_adp %>% summary() 


nrow(P524A1_all_19)
summary(P524A1_all_19)
View(P524A1_all_19)
colnames(P524A1_all_19)




#Unir con codigos de distritos en Lima.

P524A1_all_19_geo <- full_join(P524A1_all_19,maps_LM_sf, by = "UBIGEO")  %>% na.omit()
nrow(P524A1_all_19_geo)
View(P524A1_all_19_geo)



boxplot(P524A1_all_19_geo$income_adp)
#### GINI COEFFICIENT ####

#### "ineq" Packages ###
library(ineq)
Gini_Gen <- ineq(P524A1_all_19_geo$income_adp, type = "Gini")
plot(Lc(P524A1_all_19_geo$income_adp), xlab = "Cumulative share of people from lowest to highest income", 
     ylab = "Cumulative share of income earned", main = "Income Distribution for Lima, Peru")

legend("topleft", c("Lorenz Curve", "Perfect Equality"), col = 1:2, lty = 1, box.col = 1)

#Graph using gglorenz
#library(gglorenz)
#ggplot(P524A1_all_19_geo, mapping = aes(income_adp)) + 
#         stat_lorenz_generalized(geom = "path") + 

Gini_DIV <- P524A1_all_19_geo %>% group_by(DISTRITO) %>% 
  summarise(Gini= ineq(income_adp), Median_income = median(income_adp))

View(P524A1_all_19_geo)

Gini_DIV <- full_join(Gini_DIV,maps_LM_sf[,-c(1:4, 7:10)],by = "DISTRITO") %>%
  distinct(UBIGEO, .keep_all = T) %>% na.omit()
View(Gini_DIV)


#### "dineq" Packages to estimate GINI Coefficient and its contributions ###
# Descomposition of Income Inequality using Package "dineq" to analize the contributions to inequelity #
library(dineq)

#Por distritos
Regional_Decomp <- gini_decomp(x=P524A1_all_19_geo$income_adp,
                             z=P524A1_all_19_geo$DISTRITO)
# Befehlen                      # Descripción
Regional_Decomp["gini_decomp"]  # Descomposition del Gini (General)
Regional_Decomp["gini_group"]   # Gini por distritos y aporte de cada distrito al Gini General
Regional_Decomp["share_groups"] # Porcetage de cada Distrito a la prueba final

Gini_Contribution <- Regional_Decomp$gini_group$gini_group_contribution
View(Gini_Contribution)
str(Gini_Contribution)



#### Thiel Index ####

#Analisis con "REAT" package
library(REAT)
# General Theil
theil(P524A1_all_19_geo$income_adp)

# Theil por Distritos
Thei_DIV <- P524A1_all_19_geo %>% group_by(DISTRITO, UBIGEO) %>% 
  summarise(Theil = theil(income_adp))
Thei_DIV <- full_join(Thei_DIV,maps_LM_sf[,-c(1:4, 7:10)],
                      by = c("DISTRITO","UBIGEO"))%>%  
  distinct(UBIGEO, .keep_all = T) %>% na.omit()
View(Thei_DIV)
summary(Thei_DIV$Theil)

Inequality_pro_dis <- full_join(Thei_DIV, Gini_DIV, 
                                by = c("DISTRITO","UBIGEO","geometry"))%>%  
  distinct(UBIGEO, .keep_all = T) %>% na.omit()
Inequality_pro_dis <- Inequality_pro_dis[!is.na(Inequality_pro_dis$Gini),]
Inequality_pro_dis$GINI_CONTRIBUTION <- Regional_Decomp$gini_group$gini_group_contribution

class(Inequality_pro_dis)
colnames(Inequality_pro_dis)
View(Inequality_pro_dis)

#Classification of income homogeinity
#[0.2624588,0.3963924)  [0.3963924,0.419747) mas homogenous A
# Mittel B
#[0.4757041,0.4867538) [0.4867538,0.5496313] más heterogeos C

Inequality_pro_dis$clasi <- ifelse(Inequality_pro_dis$Gini <0.419747, "A",
                                   ifelse(Inequality_pro_dis$Gini>0.4757041, 
                                          "C", "B"))

View(Inequality_pro_dis)
table(Inequality_pro_dis$clasi)



# VISUALIZACION ####
Inequality_pro_dis_sf <- st_as_sf(Inequality_pro_dis, crs = 32721)
View(Inequality_pro_dis_sf)

sum(Inequality_pro_dis$GINI_CONTRIBUTION)

ggplot( data = Inequality_pro_dis_sf) +geom_sf(aes(fill = clasi)) + 
  scale_fill_brewer(palette = "OrRd") + 
  guides(fill = guide_legend(title = "GINI COEFFIENT INTERVALS")) + 
  labs( title = "Metropolitan Area of Lima and Callao\n classified by the Gini Coefficient")



library(classInt)


breaks_qt1_GINI <- 
  classIntervals(c(min(Inequality_pro_dis_sf$Gini) - 
                     .00001, Inequality_pro_dis_sf$Gini),
                 n = 9, style = "quantile")
breaks_qt1_GINI

Inequality_pro_dis_sf <- Inequality_pro_dis_sf %>% 
  mutate(GINI_CUT = cut(Gini,breaks_qt1_GINI$brks))

breaks_qt1_GINI_Contribution <- 
  classIntervals(c(min(Inequality_pro_dis_sf$GINI_CONTRIBUTION) - .00001, Inequality_pro_dis_sf$GINI_CONTRIBUTION),
                                  n = 9, style = "quantile")
breaks_qt1_GINI_Contribution

Inequality_pro_dis_sf <- Inequality_pro_dis_sf %>% 
  mutate(GINI_CONTRIBUTION_CUT = cut(GINI_CONTRIBUTION,breaks_qt1_GINI_Contribution$brks))

breaks_qt1_income_MED <- classIntervals(c(min(Inequality_pro_dis_sf$Median_income) - .00001, Inequality_pro_dis_sf$Median_income),
                                  n = 9, style = "quantile")
breaks_qt1_income_MED

Inequality_pro_dis_sf <- Inequality_pro_dis_sf %>% 
  mutate(INCOME_CUT = cut(Median_income,breaks_qt1_income_MED$brks))

ggplot( data = Inequality_pro_dis_sf) +geom_sf(aes(fill = GINI_CUT)) + 
  scale_fill_brewer(palette = "OrRd") + 
  guides(fill = guide_legend(title = "GINI COEFFIENT INTERVALS")) + 
  labs( title = "Metropolitan Area of Lima and Callao\n classified by the Gini Coefficient")
  
ggplot( data = Inequality_pro_dis_sf) +geom_sf(aes(fill = INCOME_CUT)) + 
  scale_fill_brewer(palette = "OrRd") + 
  guides(fill = guide_legend(title = "MEDIA INCOME INTERVALS")) + 
  labs(titel = "Metropolitan Area of Lima and Callao\n classified by the Median Income")

ggplot( data = Inequality_pro_dis_sf) +geom_sf(aes(fill = GINI_CONTRIBUTION_CUT)) + 
  scale_fill_brewer(palette = "OrRd") + 
  guides(fill = guide_legend(title = "GINI CONTRIBUTION INTERVALS")) + 
  labs(titel = "Metropolitan Area of Lima and Callao\n classified by the GINI CONTRIBUTION")

#, 119, 120, 124, 125, 127,128,130,131,392)] %>% na.omit()
#"P501" ,"P505R4","P520","P524A1","P5291A","P5291B","P5293A","P5293B","P5294A","P5294B","P5295A","P5295B","P558C"] 
# otros datos de interes
# Problema, todos los datos no son homogénios
# EMPLEO, TIPO DE EMPLEO, HORAS TRABAJADAS,INGRESO , FRECUENCIA COMIDA, COSTO COMIDA, 
###Zugelassene Autos & Motorräder /// GRAVITATIONSMODELLE Jornal of Economic Policy 
#10% percentil zur Schätzung der Gini Coeffient
