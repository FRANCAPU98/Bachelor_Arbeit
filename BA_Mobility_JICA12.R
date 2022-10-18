##### Encuesta de Movilidad JICA #####

library(readxl)
library(RODBC)
library(dplyr)
library(tidyr)
library(stringr)
library(stats4)
library(ggplot2)

#### hogares #####
hogares <- read_excel(file.choose())
View(hogares)
colnames(hogares)

hogares_clean <- hogares %>% dplyr::select(-c(2:16,21:24,26:30,32:63,65:134))
colnames(hogares_clean)

  ## DATOS #####
#HOGAR	1	Número de hogar
#DIA_01	2	Dia de la entrevista - Primera
#MES_01	3	Mes de la entrevista - Primera
#HORA_01	4	Hora de la entrevista - Primera
#TURNO_01	5	Turno de la entrevista - Primera
#MINUTO_01	6	Minuto de la entrevista - Primera
#DIA_02	7	Dia de la entrevista - Segunda
#MES_02	8	Mes de la entrevista - Segunda
#HORA_02	9	Hora de la entrevista - Segunda
#TURNO_02	10	Turno de la entrevista - Segunda
#MINUTO_02	11	Minuto de la entrevista - Segunda
#DIA_03	12	Dia de la entrevista - Tercera
#MES_03	13	Mes de la entrevista - Tercera
#HORA_03	14	Hora de la entrevista - Tercera
#TURNO_03	15	Turno de la entrevista - Tercera
#MINUTO_03	16	Minuto de la entrevista - Tercera

#DOMINIO	17	Dominio
#DISTRITO	18	Distrito
#SEGMENTO	19	Segmento
#VIVIENDA	20	Número de vivienda

#ENCUESTADOR	21	Código del encuestador
#SUPERVISOR	22	Código del Supervisor
#CODIFICADOR	23	Código del Codificador
#RESULTADO	24	Resultado de la entrevista


#ZONA_TRANSITO	25	Zona de Transito

#P1	  26	1. Nombre del Jefe del Hogar
#P2_1	27	2.1. Dirección de la Vivienda
#P2_2	28	2.2. Puerta
#P2_3	29	2.3. Interior
#P2_4	30	2.4. Piso
#P2_5	31	2.5. Etapa/Sector/Grupo/Urbanización
#P2_6	32	2.6. Manzana
#P2_7	33	2.7. Lote
#P2_8	34	2.8. Kilometro
#P2_9	35	2.9. Teléfono
#P3	36	3. ¿Cuál es el Nivel de Educación del Jefe del Hogar?
#P4_1_01	37	4.1.01. Hombres Menores de 6 años
#P4_1_02	38	4.1.02. Hombres De 6 años a más
#P4_1_03	39	4.1.03. Hombres Empleados del hogar
#P4_1_04	40	4.1.04. Hombres Total
#P4_2_01	41	4.2.01. Mujeres Menores de 6 años
#P4_2_02	42	4.2.02. Mujeres De 6 años a más
#P4_2_03	43	4.2.03. Mujeres Empleados del hogar
#P4_2_04	44	4.2.04. Mujeres Total
#P4_3_01	45	4.3.01. Total Menores de 6 años
#P4_3_02	46	4.3.02. Total De 6 años a más
#P4_3_03	47	4.3.03. Total Empleados del hogar
#P4_3_04	48	4.3.04. Total
#P5_01	49	5.01. ¿Tiene Ud. en el hogar Equipo de Sonido ? ¿Cuántos?
#P5_02	50	5.02. ¿Tiene Ud. en el hogar Televisor a Color ? ¿Cuántos?
#P5_03	51	5.03. ¿Tiene Ud. en el hogar DVD ? ¿Cuántos?
#P5_04	52	5.04. ¿Tiene Ud. en el hogar Licuadora ? ¿Cuántos?
#P5_05	53	5.05. ¿Tiene Ud. en el hogar Refrigeradora/Congeladora ? ¿Cuántos?
#P5_06	54	5.06. ¿Tiene Ud. en el hogar Cocina a Gas ? ¿Cuántos?
#P5_07	55	5.07. ¿Tiene Ud. en el hogar Teléfono Fijo ? ¿Cuántos?
#P5_08	56	5.08. ¿Tiene Ud. en el hogar Plancha Electrica ? ¿Cuántos?
#P5_09	57	5.09. ¿Tiene Ud. en el hogar Lavadora ? ¿Cuántos?
#P5_10	58	5.1. ¿Tiene Ud. en el hogar Computadora ? ¿Cuántos?
#P5_11	59	5.11. ¿Tiene Ud. en el hogar Horno Microondas ? ¿Cuántos?
#P5_12	60	5.12. ¿Tiene Ud. en el hogar Internet ? ¿Cuántos?
#P5_13	61	5.13. ¿Tiene Ud. en el hogar Cable ? ¿Cuántos?
#P5_14	62	5.14. ¿Tiene Ud. en el hogar Aspiradora / Lustradora ? ¿Cuántos?
#P5_15	63	5.15. ¿Tiene Ud. en el hogar Cocina eléctrica con horno ? ¿Cuántos?
#P6	64	6. ¿A cuánto asciende el ingreso mensual del hogar? (Ver Tarjeta #1)
#P7_1	65	7. Tenencia de vehículos
#P7_2	66	7. Tenencia de vehículos
#P7_3	67	7. Tenencia de vehículos
#                                                             P7_4	68	7. Tenencia de vehículos
#                                                             P7_5	69	7. Tenencia de vehículos
#                                                             P7_6	70	7. Tenencia de vehículos
#                                                             P7_7	71	7. Tenencia de vehículos
#                                                             P7_8	72	7. Tenencia de vehículos
#                                                             P7_9	73	7. Tenencia de vehículos
#                                                             P7A_1	74	7A. ¿Cuántas Bicicleta ?
#                                                               P7A_2	75	7A. ¿Cuántas Motocicleta ?
#                                                               P7A_3	76	7A. ¿Cuántas Auto particular ?
#                                                               P7A_4	77	7A. ¿Cuántas Camioneta rural ?
#                                                               P7A_5	78	7A. ¿Cuántas Microbús/Coaster ?
#                                                               P7A_6	79	7A. ¿Cuántas Ómnibus ?
#                                                               P7A_7	80	7A. ¿Cuántas Camión ?
#                                                               P7A_8	81	7A. ¿Cuántas Trailer ?
#                                                               P7A_9	82	7A. ¿Cuántas Otros ?
#                                                               P7B_1	83	7B. ¿En que año lo compró?
#                                                               P7B_2	84	7B. ¿En que año lo compró?
#                                                               P7B_3	85	7B. ¿En que año lo compró?
#                                                               P7B_4	86	7B. ¿En que año lo compró?
#                                                               P7B_5	87	7B. ¿En que año lo compró?
#                                                               P7B_6	88	7B. ¿En que año lo compró?
#                                                               P7B_7	89	7B. ¿En que año lo compró?
#                                                               P7B_8	90	7B. ¿En que año lo compró?
#                                                               P7B_9	91	7B. ¿En que año lo compró?
#                                                               P7C_1	92	7C. ¿En que año lo compró?
#                                                               P7C_2	93	7C. ¿En que año lo compró?
#                                                               P7C_3 94	7C. ¿En que año lo compró?
#                                                               P7C_4	95	7C. ¿En que año lo compró?
#                                                               P7C_5	96	7C. ¿En que año lo compró?
#                                                               P7C_6	97	7C. ¿En que año lo compró?
#                                                               P7C_7	98	7C. ¿En que año lo compró?
#                                                               P7C_8	99	7C. ¿En que año lo compró?
#                                                               P7C_9	100	7C. ¿En que año lo compró?
#                                                               P7D_1	101	7D. ¿En que año lo compró?
#                                                               P7D_2	102	7D. ¿En que año lo compró?
#                                                               P7D_3	103	7D. ¿En que año lo compró?
#                                                               P7D_4	104	7D. ¿En que año lo compró?
#                                                               P7D_5	105	7D. ¿En que año lo compró?
#                                                               P7D_6	106	7D. ¿En que año lo compró?
#                                                               P7D_7	107	7D. ¿En que año lo compró?
#                                                               P7D_8	108	7D. ¿En que año lo compró?
#                                                               P7D_9	109	7D. ¿En que año lo compró?
#                                                               P7E_1	110	7E. ¿En que año lo compró?
#                                                               P7E_2	111	7E. ¿En que año lo compró?
#                                                               P7E_3	112	7E. ¿En que año lo compró?
#                                                               P7E_4	113	7E. ¿En que año lo compró?
#                                                               P7E_5	114	7E. ¿En que año lo compró?
#                                                               P7E_6	115	7E. ¿En que año lo compró?
#                                                               P7E_7	116	7E. ¿En que año lo compró?
#                                                               P7E_8	117	7E. ¿En que año lo compró?
#                                                               P7E_9	118	7E. ¿En que año lo compró?
#                                                               OBSERVACION	119	Observaciones
#                                                             P8	120	8. Régimen de tenencia
#                                                             P9	121	9. Tipo de vivienda
#                                                             P10	122	10. Sin contar baño; cocina, pasadizos, ni garaje cuantas habitaciones ocupa este hogar?
#                                                               P11	123	11. Años viviendo en este domicilio
#                                                             P12	124	12. ¿En que lugar vivió antes? (Distrito)
#                                                             P13	125	13. El Material predominante de la paredes es:
#                                                               P14	126	14. El material predominante en los techos es:
#                                                               P15	127	15. El material predominante de los pisos es:
#                                                               P16	128	16. ¿Cual es el tipo de alumbrado que tiene su vivienda?
#                                                              P17	129	17. El abastecimiento de agua en su vivienda procede de:
#                                                               P18	130	18. El servicio higiénico (water, letrina, etc.) que tiene su vivienda esta conectado a:
#                                                               P19	131	19. ¿Cual es el combustible que mas se utiliza en el hogar para cocinar?
#                                                               SUPERVISION	132	Supervisión
#                                                             REENTREVISTA	133	Reemplazo
#                                                             NSE	134	NSE#


# Limpiar y ordenar ####
colnames(hogares_clean)
colnames(hogares_clean)[colnames(hogares_clean) == "HOGAR"] <- "hogar_id" 
colnames(hogares_clean)[colnames(hogares_clean) == "DISTRITO"] <- "UBIGEO" 
colnames(hogares_clean)[colnames(hogares_clean) == "P2_5"] <- 
  "Etapa/Sector/Grupo/Urbanización"

hogares_clean$UBIGEO <- as.character(hogares_clean$UBIGEO)
nrow(hogares_clean)
hogares_clean <- hogares_clean[!is.na(hogares_clean$hogar_id),]
hogares_clean <-  hogares_clean %>% filter(P6 != -9)


### Para LATEX ####
(table(hogares_clean$P6))
x <- as.character(c(-9,1,2,3,4,5,6,7,8))
n <- c(1008,2810,5291,4421,3150,2880,1850,1091,202)
intervals <- as.character(c("Noon",                           #1
                            "Less than S/. 730",              #2
                            "From S/. 731 to S/. 1,030",      #3
                            "From S/. 1,031 to S/. 1,370",    #4
                            "From S/. 1,371 to S/. 1,590",    #5
                            "From S/. 1,591 to S/. 2,700",    #6
                            "From S/. 2,701 to S/. 3,400",    #7
                            "From S/. 3,401 to S/. 9,000",    #8
                            "More than S/. 9,000"))           #9

intervals <- data.frame(x,intervals,n)
xtable(intervals)

### Transformation der Daten
vec <- c(2810, 5291, 4421, 3150, 2880, 1850, 1091, 202)
boundaries <- c(0, 730, 1030, 1370, 1590, 2700, 3400, 9000, Inf) / 1000
### ANPASSUNGSPARAMETER VON fitting_scrip.R
#$par
#[1]  4.2012611  1.1601262  1.7441505  0.5678049 -0.8649696
#(WERT * T1)^T2.

boundaries_t <- c((0*4.2012611)^1.1601262, (730*4.2012611)^1.1601262/100,
                  (1030*4.2012611)^1.1601262/100, (1370*4.2012611)^1.1601262/100,
                  (1590*4.2012611)^1.1601262/100, (2700*4.2012611)^1.1601262/100,
                  (3400*4.2012611)^1.1601262/100,(9000*4.2012611)^1.1601262/100,
                  (Inf*4.2012611)^1.1601262/100)


#((WERT)^(1/T2))/T1
#income_info_t<-  ((rgengamma(21695,1.7441505,0.5678049,
#                              0.8649696)^(1/1.1601262))/4.2012611)
#Transformierte Werte (WERT * T1)^T2
ingreso_mensual<-  ((rgengamma(21695,1.7441505,0.5678049,
                             0.8649696)^(4.2012611))/1.1601262)


hist(log(ingreso_mensual))
class(ingreso_mensual)
View(income_info)

income_info <- as.data.frame(ingreso_mensual)

### Corregir datos, para eliminar Ausreißer de la muestra generadas
# Usando Datos del INEI.... INVESTIGAR LITERATURA

#ingreso_mensual <- income_info %>% dplyr::mutate(P6 = if_else(income_info<110.9213, 1,
#                                              if_else(income_info>=110.9213 & income_info<165.3752,2,
#                                                      if_else(income_info>=165.3752 & income_info<230.2453, 3,
#                                                              if_else(income_info>=230.2453 & income_info<273.6678, 4,
#                                                                      if_else(income_info >=273.6678 & income_info < 505.8411, 5,
#                                                                              if_else(income_info >= 505.8411 & income_info < 660.9374, 6,
#                                                                                      if_else(income_info >= 660.9374 & income_info < 2044.6521, 7, 8))))))))

income_info <- 
  income_info[order(income_info$ingreso_mensual), ]

str(income_info)
View(income_info)
summary(income_info)
colnames(income_info)

hogares_clean <- 
  hogares_clean[order(hogares_clean$
                          P6), ]

View(hogares_clean)
hogares_clean<- cbind(hogares_clean, income_info)
colnames(hogares_clean)

View(hogares_clean)




hogares_clean[which(hogares_clean$UBIGEO =="70101"),]$UBIGEO <- "070101"
hogares_clean[which(hogares_clean$UBIGEO =="70102"),]$UBIGEO <- "070102"
hogares_clean[which(hogares_clean$UBIGEO =="70103"),]$UBIGEO <- "070103"
hogares_clean[which(hogares_clean$UBIGEO =="70104"),]$UBIGEO <- "070104"
hogares_clean[which(hogares_clean$UBIGEO =="70105"),]$UBIGEO <- "070105"
hogares_clean[which(hogares_clean$UBIGEO =="70106"),]$UBIGEO <- "070106"
hogares_clean[which(hogares_clean$UBIGEO =="70107"),]$UBIGEO <- "070107"
#POR DISTRIRO
hogares_clean_sf <- full_join(hogares_clean,maps_LM_sf[,-c(1:4, 7:10)],
                              by = "UBIGEO") %>% 
  distinct(hogar_id, .keep_all = T)


nrow(hogares_clean_sf)
View(hogares_clean_pq_sf)
names(hogares_clean_sf)

#ANALISIS DE LOS INGRESOS DEL JICA #####
## GINI Coeficient#
library(ineq)
Gini_Gen_12 <- ineq(hogares_clean$income_info, type = "Gini")

plot(Lc(hogares_clean$income_info), 
     xlab = "Cumulative share of people from lowest to highest income", 
     ylab = "Cumulative share of income earned", 
     main = "Lorenz Curve of the Income Distribution for Lima, Peru", col = "red")

legend("topleft", c("Perfect Equality","Lorenz Curve"), col = 1:2, lty = 1,
       box.col = 1)

#Desagregación por Distritos ####
Gini_DIV_DIST <- income_info %>% group_by(UBIGEO) %>% 
  summarise(Gini = ineq(income_info),
            Median_income = median(income_info))
View(Gini_DIV_DIST)

summary(income_info$ingreso_mensual)
#### Calcular el Gini contribution 
library(dineq)
#Desagregación por distritos##
Regional_Decomp_12 <- gini_decomp(x=hogares_clean$income_info,
                                  z=hogares_clean$UBIGEO)
# Befehlen                      # Descripción
Regional_Decomp_12["gini_decomp"]  # Descomposition del Gini (General)
Regional_Decomp_12["gini_group"]   # Gini por distritos y aporte de cada distrito al Gini Withhin
Regional_Decomp_12["share_groups"] # Porcetage de cada Distrito a la prueba final



Regional_Decomp_12_dist <- hogares_clean %>% group_by(UBIGEO) %>% 
  summarise(gini_decomp(x=hogares_clean$ingreso_mensual,
                        z=hogares_clean$UBIGEO))

View(Regional_Decomp_12_dist)
#Gini Contribution by district
Gini_Contribution_12 <- Regional_Decomp_12$gini_group$gini_group_contribution
Gini_Contribution_12 <-  
  cbind(Gini_Contribution_12,as.character(c("070101","070102","070103",
                                            "070104","070105","070106",
                                            150101:150143)))

Gini_Contribution_12 <- as.data.frame.table(Gini_Contribution_12)
colnames(Gini_Contribution_12)
Gini_Contribution_12 <- Gini_Contribution_12[,-c(2)]

names(Gini_Contribution_12) <- c("UBIGEO", "GINI_CONTRIBUTION")
str(Gini_Contribution_12)
Gini_Contribution_12$GINI_CONTRIBUTION <- 
  as.numeric(Gini_Contribution_12$GINI_CONTRIBUTION)
Gini_Contribution_12 <-Gini_Contribution_12 %>% distinct(UBIGEO, .keep_all = T)

View(Gini_Contribution_12)
#Unificación
income_parameter_dist <- full_join(Gini_DIV_DIST,Gini_Contribution_12, 
                                   by = "UBIGEO",copy = T) %>% 
  distinct(UBIGEO, .keep_all = T)

View(income_parameter_dist)  #### FINAL INCOME PRO DISTRITO
str(income_parameter_dist)
# GEOREFERENCIACIÓN
income_parameter_dist_sf <- 
  full_join(income_parameter_dist,maps_LM_sf[,-c(1:4,7:10)],
            by = "UBIGEO", copy = T)%>% 
  distinct(UBIGEO, .keep_all = T)
nrow(income_parameter_dist_sf)

View(income_parameter_dist_sf)  #### FINAL INCOME PER DISTRITOS SF Format

summary(income_parameter_dist_sf$Gini)
#Classification of income homogeinity
income_parameter_dist_sf$clasi_gini <- 
  ifelse(income_parameter_dist_sf$Gini <0.5845, "1. Quantil",
         ifelse(income_parameter_dist_sf$Gini>=0.5845 &
                  income_parameter_dist_sf$Gini<0.6976, 
                "2.Quantil",ifelse(income_parameter_dist_sf$Gini >= 0.6976 &
                                     income_parameter_dist_sf$Gini < 0.7595,
                                   "3.Quantil", "4.Quantil")))


table(income_parameter_dist_sf$clasi_gini)

# Classificacion of media income
quantile(income_parameter_dist_sf$Median_income, 
         probs = seq(0, 1, .4), na.rm = T)
boxplot(income_parameter_dist_sf$Median_income)
summary(income_parameter_dist_sf$Median_income)

income_parameter_dist_sf$clasi_income <- 
  ifelse(income_parameter_dist_sf$Median_income <443.85, "1. Quantil",
         ifelse(income_parameter_dist_sf$Median_income>=443.85 &
                  income_parameter_dist_sf$Median_income<752.68, 
                "2.Quantil",ifelse(income_parameter_dist_sf$Median_income >= 752.68 &
                                     income_parameter_dist_sf$Median_income < 1969.15,"3.Quantil", "4.Quantil")))
table(income_parameter_dist_sf$clasi_income)
# VISUALIZACION ####
income_parameter_dist_sf <- st_as_sf(income_parameter_dist_sf, crs = 32721)
View(income_parameter_dist_sf)

sum(income_parameter_dist_sf$GINI_CONTRIBUTION)

ggplot(data = income_parameter_dist_sf) +geom_sf(aes(fill = clasi_gini)) + 
  scale_fill_brewer(palette = "OrRd") + 
  guides(fill = guide_legend(title = "GINI COEFFIENT INTERVALS")) + 
  labs( title = "Metropolitan Area of Lima and Callao\nclassified by the Gini Coefficient")

ggplot(data = income_parameter_dist_sf) +geom_sf(aes(fill = clasi_income)) + 
  scale_fill_brewer(palette = "OrRd") + 
  guides(fill = guide_legend(title = "MEDIAN INCOME INTERVALS")) + 
  labs( title = "Districts of AMLC\nclassified by the Median Income")

library(classInt)
#breaks_qt <- 
#  classIntervals(c(min(philly_crimes_sf$homic_rate) - 
#                     .00001, philly_crimes_sf$homic_rate), n = 7, 
#                 style = "quantile")
#philly_crimes_sf <- mutate(philly_crimes_sf,
#                           homic_rate_cat = cut(homic_rate, breaks_qt$brks)) 
##GINI
breaks_qt1_GINI_12 <- 
  classIntervals(c(min(income_parameter_dist_sf$Gini) -
                     .00001,income_parameter_dist_sf$Gini),
                 n = 9, style = "quantile")
breaks_qt1_GINI_12

Inequality_pro_dis_12_geo_graph <- income_parameter_dist_sf %>% 
  mutate(GINI_CUT = cut(Gini,breaks_qt1_GINI_12$brks))
##GINI CONTRIBUTION
breaks_qt1_GINI_Contribution_12 <- 
  classIntervals(c(min(income_parameter_dist_sf$GINI_CONTRIBUTION) -
                     .00001, income_parameter_dist_sf$GINI_CONTRIBUTION),
                 n = 9, style = "quantile")
breaks_qt1_GINI_Contribution_12

Inequality_pro_dis_12_geo_graph <- Inequality_pro_dis_12_geo_graph %>% 
  mutate(GINI_CONTRIBUTION_CUT = cut(GINI_CONTRIBUTION,
                                     breaks_qt1_GINI_Contribution_12$brks))
##MEDIAN INCOME
breaks_qt1_income_MED_12 <- 
  classIntervals(c(min(income_parameter_dist_sf$Median_income) -
                     .00001, income_parameter_dist_sf$Median_income),
                 n = 4, style = "quantile")
breaks_qt1_income_MED_12

Inequality_pro_dis_12_geo_graph <- Inequality_pro_dis_12_geo_graph %>% 
  mutate(INCOME_CUT = cut(Median_income,
                          breaks_qt1_income_MED_12$brks))

View(Inequality_pro_dis_12_geo_graph)

Inequality_pro_dis_12_geo_graph <- 
  st_as_sf(Inequality_pro_dis_12_geo_graph, crs = 32721)

ggplot(data = Inequality_pro_dis_12_geo_graph) + geom_sf(aes(fill = GINI_CUT)) + 
  scale_fill_brewer(palette = "OrRd") + 
  guides(fill = guide_legend(title = "GINI COEFFIENT INTERVALS")) + 
  labs( title = "Districts of AMLC\nclassified by the Gini Coefficient")

ggplot(data = Inequality_pro_dis_12_geo_graph) + geom_sf(aes(fill = INCOME_CUT)) + 
  scale_fill_brewer(palette = "OrRd") + 
  guides(fill = guide_legend(title = "MEDIA INCOME INTERVALS")) + 
  labs(title = "Metropolitan Area of Lima and Callao\n classified by 
       the Median Income")

ggplot(data = Inequality_pro_dis_12_geo_graph) + geom_sf(aes(fill = GINI_CONTRIBUTION_CUT)) + 
  scale_fill_brewer(palette = "OrRd") + 
  guides(fill = guide_legend(title = "GINI CONTRIBUTION INTERVALS")) + 
  labs(title = "Districts of AMLC\nclassified by the Gini Contribution")

# Desagregación por ZT ####
income_parameter_ZT <- hogares_clean %>% group_by(ZONA_TRANSITO) %>% 
  summarise(Gini= ineq(income_info), 
            Median_income = median(income_info))

View(income_parameter_ZT)
income_parameter_ZT$ZONA_TRANSITO <- 
  as.character(income_parameter_ZT$ZONA_TRANSITO)

#### Calcular el Gini contribution 
library(dineq)

#Desagregación por ZONA_TRANSITO##
Regional_Decomp_ZT <- gini_decomp(x=hogares_clean$income_info,
                                  z=hogares_clean$ZONA_TRANSITO)
# Befehlen                      # Descripción
Regional_Decomp_ZT["gini_decomp"]  # Descomposition del Gini (General)
Regional_Decomp_ZT["gini_group"]   # Gini por distritos y aporte de cada distrito al Gini General
Regional_Decomp_ZT["share_groups"] # Porcetage de cada Distrito a la prueba final

#Gini Contribution by district
Gini_Contribution_ZT <- Regional_Decomp_ZT$gini_group$gini_group_contribution

Gini_Contribution_ZT <- 
  data.matrix(Gini_Contribution_ZT, rownames.force = NA)

Gini_Contribution_ZT <- as.data.frame.table(Gini_Contribution_ZT)
colnames(Gini_Contribution_ZT)
Gini_Contribution_ZT <- Gini_Contribution_ZT[,-c(2)]

names(Gini_Contribution_ZT) <- c("ZONA_TRANSITO", "GINI_CONTRIBUTION")
str(Gini_Contribution_ZT)
Gini_Contribution_ZT$GINI_CONTRIBUTION <- 
  as.numeric(Gini_Contribution_ZT$GINI_CONTRIBUTION)
Gini_Contribution_ZT <-Gini_Contribution_ZT %>% distinct(ZONA_TRANSITO, .keep_all = T)

View(Gini_Contribution_ZT)

#Unificación
income_parameter_ZT <- full_join(income_parameter_ZT,Gini_Contribution_ZT,
                                 by = "ZONA_TRANSITO",copy = T) %>% 
  distinct(ZONA_TRANSITO, .keep_all = T)

View(income_parameter_ZT)
# GEOREFERENCIACIÓN
income_parameter_ZT_sf <- full_join(income_parameter_ZT,ZT_LIMA[,-c(8)], 
                              by = "ZONA_TRANSITO", copy = T)
nrow(income_parameter_ZT_sf)


#Classification of income homogeinity
income_parameter_ZT_sf$clasi_gini <- 
  ifelse(income_parameter_ZT_sf$Gini <0.268808, "A",
         ifelse(income_parameter_ZT_sf$Gini>0.338832, 
                                          "C", "B"))


table(income_parameter_ZT_sf$clasi_gini)

boxplot(log(income_parameter_ZT_sf$Median_income))
summary(income_parameter_ZT_sf$Median_income)
# Classificacion of media income
income_parameter_ZT_sf$clasi_income <- 
  ifelse(income_parameter_ZT_sf$Median_income <261.0, "1. Quantil",
         ifelse(income_parameter_ZT_sf$Median_income>=261.0 
                & income_parameter_ZT_sf$Median_income<720.9, 
                "2.Quartil", ifelse(income_parameter_ZT_sf$Median_income>=720.9 
                                    & income_parameter_ZT_sf$Median_income<2420.0,
                                    "3.Quantil","4.Quantil")))

table(income_parameter_ZT_sf$clasi_income)

View(income_parameter_ZT_sf)   #### Desagregación FINAL
colnames(income_parameter_ZT_sf)
#income_parameter_ZT_sf <- st_drop_geometry(income_parameter_ZT_sf)

nas.$validacion <- income_parameter_ZT_sf %>% 
  mutate(na = is.na())

# VISUALIZACION ####
income_parameter_ZT_sf <- st_as_sf(income_parameter_ZT_sf, crs = 32721)
View(income_parameter_ZT_sf)

ggplot( data = income_parameter_ZT_sf) +geom_sf(aes(fill = clasi_gini)) + 
  scale_fill_brewer(palette = "OrRd") + 
  guides(fill = guide_legend(title = "GINI COEFFIENT INTERVALS")) + 
  labs( title = "Metropolitan Area of Lima and Callao\n classified by the Gini 
        Coefficient")

ggplot( data = income_parameter_ZT_sf) +geom_sf(aes(fill = clasi_income)) + 
  scale_fill_brewer(palette = "OrRd") + 
  guides(fill = guide_legend(title = "MEDIAN INCOME INTERVALS")) + 
  labs( title = "Microzone of AMLC\nclassified by the Median Income")

library(classInt)
#breaks_qt <- 
#  classIntervals(c(min(philly_crimes_sf$homic_rate) - 
#                     .00001, philly_crimes_sf$homic_rate), n = 7, 
#                 style = "quantile")
#philly_crimes_sf <- mutate(philly_crimes_sf,
#                           homic_rate_cat = cut(homic_rate, breaks_qt$brks)) 

##GINI
breaks_qt1_GINI_12 <- 
  classIntervals(c(min(income_parameter_ZT_sf$Gini) -
                     .00001,income_parameter_ZT_sf$Gini),
                 n = 9, style = "quantile")
breaks_qt1_GINI_12

income_parameter_ZT_graph_sf <- income_parameter_ZT_sf %>% 
  mutate(GINI_CUT = cut(Gini ,breaks_qt1_GINI_12$brks))

##MEDIAN INCOME
breaks_qt1_income_MED_12 <- 
  classIntervals(c(min(income_parameter_ZT_sf$Median_income) -
                     .00001, income_parameter_ZT_sf$Median_income),
                 n = 4, style = "quantile")
breaks_qt1_income_MED_12

income_parameter_ZT_graph_sf <- income_parameter_ZT_graph_sf %>% 
  mutate(INCOME_CUT = cut(Median_income ,breaks_qt1_income_MED_12$brks))

##GINI CONTRIBUTION
breaks_qt1_GINI_Contribution_12 <- 
  classIntervals(c(min(income_parameter_ZT_sf$GINI_CONTRIBUTION) -
                     .00001, income_parameter_ZT_sf$GINI_CONTRIBUTION),
                 n = 9, style = "quantile")
breaks_qt1_GINI_Contribution_12

income_parameter_ZT_graph_sf <- income_parameter_ZT_graph_sf %>% 
  mutate(GINI_CONTRIBUTION_CUT = cut(GINI_CONTRIBUTION,
                                     breaks_qt1_GINI_Contribution_12$brks))

income_parameter_ZT_graph_sf <- 
  st_as_sf(income_parameter_ZT_graph_sf, crs = 32721)

ggplot(data = income_parameter_ZT_graph_sf) + geom_sf(aes(fill = GINI_CUT)) + 
  scale_fill_brewer(palette = "OrRd") + 
  guides(fill = guide_legend(title = "GINI COEFFIENT INTERVALS")) + 
  labs( title = "Microzones of AMLC\nclassified by the Gini Coefficient")

ggplot(data = income_parameter_ZT_graph_sf) + geom_sf(aes(fill = INCOME_CUT)) + 
  scale_fill_brewer(palette = "OrRd") + 
  guides(fill = guide_legend(title = "MEDIA INCOME INTERVALS")) + 
  labs(titel = "Metropolitan Area of Lima and Callao\nclassified by 
       the Median Income")

ggplot(data = income_parameter_ZT_graph_sf) + geom_sf(aes(fill = GINI_CONTRIBUTION_CUT)) + 
  scale_fill_brewer(palette = "OrRd") + 
  guides(fill = guide_legend(title = "GINI CONTRIBUTION INTERVALS")) + 
  labs(titel = "Microzones of AMLC \nclassified by 
       the Gini Contribution")


#### personas #####
personas <- read_excel(file.choose())
names(personas)
nrow(personas)
#DICCIONARIO DE COLUMNAS PERSONAS####
#HOGAR	1	  Número de Hogar
#C2	    2  	Número de persona
#P20  	3	  Nombres y Apellidos de los miembros del hogar
#P21  	4	  21. Relación de parentesco con el jefe
#P22	  5  	22. Sexo
#P23	  6  	23. Edad ¿Cuántos años tiene?
#P24A	  7	  24a. ¿Tiene Ud. Alguna discapacidad?
#P24B	  8	  24b. ¿Necesita ayuda para viajar?
#P25	  9	  25. ¿Cuál es su ocupación actualmente?
#P26	  10	26. ¿A que actividad se dedica el negocio, empresa, u organismo en el que usted trabaja?
#P27_1	11	27.1. Ocupación en el 2006
#P27_2	12	27.2. Actividad en el 2006
#P28	  13	28. ¿Tiene Ud. licencia de conducir?
#P28A	  14	28a. ¿Se movilizó el día de ayer?
#P29A_1	15	29a  ¿Dónde se ubica su centro de trabajo?
#P29A_2	16	29a  ¿Dónde se ubica su centro de trabajo secundario?
#P29B	17	  29b  ¿Dónde se ubica su centro de estudio?
#P28B	18	        <ninguno>




#Limpiar y ordenar ####


colnames(personas)[colnames(personas) == "HOGAR"] <- "hogar_id"
colnames(personas)[colnames(personas) == "C2"] <- "id_person"
colnames(personas)[colnames(personas) == "P20"] <- "Nombres y Apellidos de los miembros del hogar"
colnames(personas)[colnames(personas) == "P21"] <- "Relación de parentesco con el jefe"
colnames(personas)[colnames(personas) == "P22"] <- "Sexo"
colnames(personas)[colnames(personas) == "P23"] <- "Edad"
colnames(personas)[colnames(personas) == "P24A"] <- "Discapacidad"
colnames(personas)[colnames(personas) == "P24B"] <- "Ayuda para viajar"
colnames(personas)[colnames(personas) == "P25"] <- "Ocupación Actual"
colnames(personas)[colnames(personas) == "P26"] <- "Actividad de la empresa en el que trabaja"
colnames(personas)[colnames(personas) == "P27_1"] <- "Ocupación" ###En el 2006
colnames(personas)[colnames(personas) == "P27_2"] <- "Actividad" ###En el 2006
colnames(personas)[colnames(personas) == "P28"] <- "Brevete"
colnames(personas)[colnames(personas) == "P28A"] <- "Movilización ayer?"
colnames(personas)[colnames(personas) == "P29A_1"] <- "Ubicación del Centro de Trabajo"
colnames(personas)[colnames(personas) == "P29A_2"] <- "Ubicación del Centro de Trabajo Secundario"
colnames(personas)[colnames(personas) == "P29B"] <- "Ubicación del Centro de Estudios"
colnames(personas)[colnames(personas) == "P28B"] <- "<ninguno>"

colnames(personas)

personas_clean <- personas[,-c(3,4,7,8,11,12,18)]
#personas_clean <- personas %>% select(-one_of("Nombres y Apellidos de los miembros del hogar",
#                                              "Relación de parentesco con el jefe",
#                                              "Discapacidad","Ayuda para viajar",
#                                              "Actividad de la empresa en el que trabaja",
#                                              "<ninguno>"))
colnames(personas_clean)

personas_clean$id <- seq.int(nrow(personas_clean))

# Sexo :::: P22#
P22 <- c(-9,1,2)
sex <- c("ninguno", "hombre","mujer")

sex_cod <- data.frame(P22, sex)

person_pro <- full_join(personas_clean, sex_cod, by = "P22",copy = T) %>% 
  distinct(id, .keep_all = T)

# Ocupación Actual :::: P25#
P25 <- c(-9,1:17)
ocupación <- c("ninguno", "Gerente, Jefe, Patrón,Director",
         "Profesional Independiente",
         "Empleado",
         "Técnico Independiente",
         "Obrero, Peón",
         "Vendedor, Comerciante",
         "Trabajador independiente no especializado",
         "Policía / FF.AA.",
         "Chofer",
         "Vigilante",
         "Estudiante Trabajador",
         "Estudiante Primaria/ Secundaria",
         "Estudiante Superior",
         "Ama de casa",
         "Trabajador del hogar",
         "Desocupado/Jubilado", "Otra")

ocupacion_cod <- data.frame(P25, ocupación)

person_pro <- full_join(person_pro, ocupacion_cod,by="P25" ,copy = T)%>% 
  distinct(id, .keep_all = T)

# Brevete :::: P28#
P28 <- c(-9,1,2)
Brevete <- c("ninguno",
             "Si",
             "No")

Brevete_cod <- data.frame(P28, Brevete)

person_pro <- full_join(person_pro, Brevete_cod,by="P28" ,copy = T)%>% 
  distinct(id, .keep_all = T)

# Limpieza y ordenanza ###

person_pro <- person_pro[,-c(3,5,7,8)]
colnames(person_pro)[colnames(person_pro) == "P23"] <- "Edad"
colnames(person_pro)[colnames(person_pro) == "P26"] <- "Actividad_empresa"
View(person_pro)

#### viajes #####
viajes <- read_excel(file.choose())
View(viajes)
names(viajes)
nrow(viajes)
str(viajes)
colnames(viajes)

# LIMPIEZA DE DATA ####  
colnames(viajes)[colnames(viajes) == "ID"] <- "trip_id"
colnames(viajes)[colnames(viajes) == "HOGAR"] <- "hogar_id"
colnames(viajes)[colnames(viajes) == "C2"] <- "id_person"
#colnames(viajes)[colnames(viajes) == "T_VIAJES...5"] <- "Total de viajes"
#colnames(viajes)[colnames(viajes) == "VIAJE_01"] <- "Número de viaje"
#colnames(viajes)[colnames(viajes) == "REFERENCIA"] <- "Fecha de referencia"
#colnames(viajes)[colnames(viajes) == "P30_01"] <- "Origen del viaje"
#colnames(viajes)[colnames(viajes) == "P31_01"] <- "Lugar de origen"
colnames(viajes)[colnames(viajes) == "P32_1_01"] <- "Salida_H"
colnames(viajes)[colnames(viajes) == "P32_2_01"] <- "salida_M"
colnames(viajes)[colnames(viajes) == "P32_3_01"] <- "Salida_T"  #AM (1) // #PM (2)
#colnames(viajes)[colnames(viajes) == "P33_01"] <- "Destino del viaje" 
#colnames(viajes)[colnames(viajes) == "P34_01"] <- "Lugar de destino" 
colnames(viajes)[colnames(viajes) == "P35_1_01"] <- "Llegada_H"
colnames(viajes)[colnames(viajes) == "P35_2_01"] <- "Llegada_M"
colnames(viajes)[colnames(viajes) == "P35_3_01"] <- "Llegada_T" #AM (1) // #PM (2)
#colnames(viajes)[colnames(viajes) == "P36_01"] <- "Proposito"
#colnames(viajes)[colnames(viajes) == "P37_1_1_01"] <- "1. Modalidad"
colnames(viajes)[colnames(viajes) == "P37_1_2_01"] <- "Tiempo_MIN_1"
colnames(viajes)[colnames(viajes) == "P37_1_3_01"] <- "Cost_1"
#colnames(viajes)[colnames(viajes) == "P37_2_1_01"] <- "2. Modalidad"
colnames(viajes)[colnames(viajes) == "P37_2_2_01"] <- "Tiempo_MIN_2"
colnames(viajes)[colnames(viajes) == "P37_2_3_01"] <- "Cost_2"
#colnames(viajes)[colnames(viajes) == "P37_3_1_01"] <- "3. Modalidad"
colnames(viajes)[colnames(viajes) == "P37_3_2_01"] <- "Tiempo_MIN_3"
colnames(viajes)[colnames(viajes) == "P37_3_3_01"] <- "Cost_3"
#colnames(viajes)[colnames(viajes) == "P37_4_1_01"] <- "4. Modalidad"
colnames(viajes)[colnames(viajes) == "P37_4_2_01"] <- "Tiempo_MIN_4"
colnames(viajes)[colnames(viajes) == "P37_4_3_01"] <- "Cost_4"
#colnames(viajes)[colnames(viajes) == "P37_5_1_01"] <- "5. Modalidad"
colnames(viajes)[colnames(viajes) == "P37_5_2_01"] <- "Tiempo_MIN_5"
colnames(viajes)[colnames(viajes) == "P37_5_3_01"] <- "Cost_5"
#colnames(viajes)[colnames(viajes) == "P37_6_1_01"] <- "6. Modalidad"
colnames(viajes)[colnames(viajes) == "P37_6_2_01"] <- "Tiempo_MIN_6"
colnames(viajes)[colnames(viajes) == "P37_6_3_01"] <- "Cost_6"
#colnames(viajes)[colnames(viajes) == "P37_7_1_01"] <- "7. Modalidad"
colnames(viajes)[colnames(viajes) == "P37_7_2_01"] <- "Tiempo_MIN_7"
colnames(viajes)[colnames(viajes) == "P37_7_3_01"] <- "Cost_7"
#colnames(viajes)[colnames(viajes) == "P37_T_01_1"] <- "1 Transbordo"
#colnames(viajes)[colnames(viajes) == "P37_T_01_2"] <- "2 Transbordo"
#colnames(viajes)[colnames(viajes) == "P37_T_01_3"] <- "3 Transbordo"
#colnames(viajes)[colnames(viajes) == "P37_T_01_4"] <- "4 Transbordo"#
#colnames(viajes)[colnames(viajes) == "P37_T_01_5"] <- "5 Transbordo"
#colnames(viajes)[colnames(viajes) == "P37_T_01_6"] <- "6 Transbordo"

# DICCIONARIO DE COLUMNAS VIAJES #####
#HOGAR	1	Número de hogar
#C2	2	Código de la persona
#T_VIAJES	3	Total de viajes
#VIAJE_01	4	Número de viaje
#REFERENCIA	5	Fecha de referencia
#P30_01	6	30  Origen del viaje
#P31_01	7	31  Lugar de origen
#P32_1_01	8	32.1  Hora de salida
#P32_2_01	9	32.2  Minutos de salida
#P32_3_01	10	32.3  Turno de salida
#P33_01	11	33  Destino del viaje
#P34_01	12	34  Lugar de destino
#P35_1_01	13	35.1  Hora de llegada
#P35_2_01	14	35.2  Minutos de llegada
#P35_3_01	15	35.3  Turno de llegada
#P36_01	16	36  Propósito
#TIME_VIAJE_1  ::::  TIEMPO DE VIAJE, CALCULADO DE HORA DE SALIDA Y LLEGADA
#TIME_VIEJES_2 :::: TIEMPO DE VIEAJE, CALCULADO SUMANDO TODOS LOS TIEMPOS DE USO DE LA MODALIDAD
#C_VIAJES      :::: COSTE TOTAL DEL VIAJE
#TIME_TRANS    :::: TIEMPO DE TRANSBORDO O TIEMPO DE ESPERA  dif (TIME_VIAJE_1-TIME_VIEJES_2)
#TRANS         :::: # TOTAL DE TRANSBORDOS
#P37_1_1_01	17	37.1 Medio de transporte
#P37_1_2_01	18	37.2 Tiempo en minutos
#P37_1_3_01	19	37.3 Costo (Soles)
#P37_2_1_01	20	37.1 Medio de transporte
#P37_2_2_01	21	37.2 Tiempo en minutos
#P37_2_3_01	22	37.3 Costo (Soles)
#P37_3_1_01	23	37.1 Medio de transporte
#P37_3_2_01	24	37.2 Tiempo en minutos
#P37_3_3_01	25	37.3 Costo (Soles)
#P37_4_1_01	26	37.1 Medio de transporte
#P37_4_2_01	27	37.2 Tiempo en minutos
#P37_4_3_01	28	37.3 Costo (Soles)
#P37_5_1_01	29	37.1 Medio de transporte
#P37_5_2_01	30	37.2 Tiempo en minutos
#P37_5_3_01	31	37.3 Costo (Soles)
#P37_6_1_01	32	37.1 Medio de transporte
#P37_6_2_01	33	37.2 Tiempo en minutos
#P37_6_3_01	34	37.3 Costo (Soles)
#P37_7_1_01	35	37.1 Medio de transporte
#P37_7_2_01	36	37.2 Tiempo en minutos
#P37_7_3_01	37	37.3 Costo (Soles)
#P37_T_01_1	38	Primer Transbordo
#P37_T_01_2	39	Segundo Transbordo
#P37_T_01_3	40	Tercer Transbordo
#P37_T_01_4	41	Cuarto Transbordo
#P37_T_01_5	42	Quinto Transbordo
#P37_T_01_6	43	Sexto Transbordo
#P37_C_01_1	44	Codigo 1
#P37_C_01_2	45	Codigo 2
#P37_C_01_3	46	Codigo 3
#P37_C_01_4	47	Codigo 4
#P37_C_01_5	48	Codigo 5
#P38_01	49	38  ¿Manejó Ud. el auto en que viajó?
#P39_1_01	50	39  Estacionamiento: Lugar
#P39_2_01	51	39  Estacionamiento: Pago
#P39_3_01	52	39  Estacionamiento: Tarifa
#P39_4_01	53	39  Estacionamiento: Unidad


#Origen del viaje ::::: P30_01 #####   
# Combinacion con P37_C_01_1



#Lugar de origen ::::::  P31_01 ####
P31_01 <- c(-9, 1:9)
origen_lugar <- c("Ninguno", "Residencial", "Oficina/Banco","Industrial",
                  "Educacional","Comercial","Recreacional","Médico",
                  "Restaurante","Otros")

origen_cod <- data.frame(P31_01, origen_lugar)


viajes_pro <- full_join(viajes, origen_cod, by = "P31_01", copy = T)%>% 
  distinct(trip_id, .keep_all = T)

#prueba
names(viajes_pro)

# Destino del viaje ::::: P33_01 ####
#IGUAL ENCIFRADO QUE P30_01 CON P37_C_01_1



#Lugar de destino ::::: P34_01 #####
P34_01 <- c(-9, 1:9)
destino_lugar <- c("Ninguno", "Residencial", "Oficina/Banco","Industrial",
                   "Educacional","Comercial","Recreacional","Médico",
                   "Restaurante","Otros")

destino_cod <- data.frame(P34_01, destino_lugar)
viajes_pro <- full_join(viajes_pro, destino_cod ,by = "P34_01") %>% 
  distinct(trip_id, .keep_all = T)

#prueba
names(viajes_pro)

# Proposito del viaje ::::: P36_01 #####

P36_01 <- c(-9, 1:11)
proposito <- c("Ninguno",
               "A trabajar",
               "A estudiar",
               "Por trabajo",
               "Compras",
               "Comer",
               "Hacer ejercicios",
               "Llevar a un familiar",
               "Pasear",
               "Esparcimiento",
               "Otro particular",
               "Volver a casa")

proposito_cod <- data.frame(P36_01, proposito)
viajes_pro <- full_join(viajes_pro, proposito_cod ,by = "P36_01")%>% 
  distinct(trip_id, .keep_all = T)

#prueba
names(viajes_pro)


#MODALIDADES P37_1_1_01  ::::::  P37_7_1_01 ####
### 1.Modalidad P37_1_1_01 ###
P37_1_1_01 <- c(-9, 1:17)
Modalidad_1 <- c("Ninguno","Caminando","Bicicleta","Motocicleta","Mototaxi","Auto_Particular",
            "Taxi","Colectivo","Combi","Microbús","Ómnibus","Metropolitano","Camión_pequeno",
            "Camión","Tráiler","Tren","Movilidad_Particular","Otros")

modalidad_cod <- data.frame(P37_1_1_01, Modalidad_1)
viajes_pro <- full_join(viajes_pro, modalidad_cod ,by = "P37_1_1_01")%>% 
  distinct(trip_id, .keep_all = T)

#### 2.Modalidad P37_2_1_01###
P37_2_1_01 <- c(-9, 1:17)
Modalidad_2 <- c("Ninguno","Caminando","Bicicleta","Motocicleta","Mototaxi","Auto_Particular",
                 "Taxi","Colectivo","Combi","Microbús","Ómnibus","Metropolitano","Camión_pequeno",
                 "Camión","Tráiler","Tren","Movilidad_Particular","Otros")

modalidad_cod2 <- data.frame(P37_2_1_01, Modalidad_2)
viajes_pro <- full_join(viajes_pro, modalidad_cod2 ,by = "P37_2_1_01")%>% 
  distinct(trip_id, .keep_all = T)

#### 3.Modalidad P37_3_1_01###
P37_3_1_01 <- c(-9, 1:17)
Modalidad_3 <- c("Ninguno","Caminando","Bicicleta","Motocicleta","Mototaxi","Auto_Particular",
                 "Taxi","Colectivo","Combi","Microbús","Ómnibus","Metropolitano","Camión_pequeno",
                 "Camión","Tráiler","Tren","Movilidad_Particular","Otros")

modalidad_cod3 <- data.frame(P37_3_1_01, Modalidad_3)
viajes_pro <- full_join(viajes_pro, modalidad_cod3 ,by = "P37_3_1_01")%>% 
  distinct(trip_id, .keep_all = T)

#### 4.Modalidad P37_4_1_01###
P37_4_1_01 <- c(-9, 1:17)
Modalidad_4 <- c("Ninguno","Caminando","Bicicleta","Motocicleta","Mototaxi","Auto_Particular",
                 "Taxi","Colectivo","Combi","Microbús","Ómnibus","Metropolitano","Camión_pequeno",
                 "Camión","Tráiler","Tren","Movilidad_Particular","Otros")

modalidad_cod4 <- data.frame(P37_4_1_01, Modalidad_4)
viajes_pro <- full_join(viajes_pro, modalidad_cod4 ,by = "P37_4_1_01")%>% 
  distinct(trip_id, .keep_all = T)

#### 5.Modalidad P37_5_1_01###
P37_5_1_01 <- c(-9, 1:17)
Modalidad_5 <- c("Ninguno","Caminando","Bicicleta","Motocicleta","Mototaxi","Auto_Particular",
                 "Taxi","Colectivo","Combi","Microbús","Ómnibus","Metropolitano","Camión_pequeno",
                 "Camión","Tráiler","Tren","Movilidad_Particular","Otros")

modalidad_cod5 <- data.frame(P37_5_1_01, Modalidad_5)
viajes_pro <- full_join(viajes_pro, modalidad_cod5 ,by = "P37_5_1_01")%>% 
  distinct(trip_id, .keep_all = T)

#### 6.Modalidad P37_6_1_01 ###
P37_6_1_01 <- c(-9, 1:17)
Modalidad_6 <- c("Ninguno","Caminando","Bicicleta","Motocicleta","Mototaxi","Auto_Particular",
                 "Taxi","Colectivo","Combi","Microbús","Ómnibus","Metropolitano","Camión_pequeno",
                 "Camión","Tráiler","Tren","Movilidad_Particular","Otros")

modalidad_cod6 <- data.frame(P37_6_1_01, Modalidad_6)
viajes_pro <- full_join(viajes_pro, modalidad_cod6 ,by = "P37_6_1_01")%>% 
  distinct(trip_id, .keep_all = T)

#### 7.Modalidad P37_7_1_01 ###
P37_7_1_01 <- c(-9, 1:17)
Modalidad_7 <- c("Ninguno","Caminando","Bicicleta","Motocicleta","Mototaxi","Auto_Particular",
                 "Taxi","Colectivo","Combi","Microbús","Ómnibus","Metropolitano","Camión_pequeno",
                 "Camión","Tráiler","Tren","Movilidad_Particular","Otros")

modalidad_cod7 <- data.frame(P37_7_1_01, Modalidad_7)
viajes_pro <- full_join(viajes_pro, modalidad_cod7 ,by = "P37_7_1_01")%>% 
  distinct(trip_id, .keep_all = T)

#prueba
names(viajes_pro)

# CODIGOS :::::: P37_C_01_1 ::: P37_C_01_5 ####
# CODIGO 1
P37_C_01_1 <- c(-9, 1:138)
cod_1 <- c("ninguno", #-9
           "AV.NARANJAL (LOS OLIVOS)", #1
           "AV.PRIMAVERA (PUENTE) /AV. CIRCUNVALACIÓN", #2
           "AV. SAN MARTIN (COMAS)", #3
           "AV. TUPAC AMARU (COMAS)", #4
           "AV. JAVIER PRADO ( ESTE Y OESTE)", #5
           "AV. LA MAR (LA VICTORIA)", #6
           "AV. ISABEL LA CATOLICA (LA VICTORIA)", #7
           "AV. LA PASCANA (COMAS)", #8
           "AV. UNIVERSITARIA", #9
           "AV. TOMAS VALLE", #10
           "AV. SAENZ PEÑA (CALLAO)", #11
           "AV. PANAMERICANA NORTE (PARADERO SANTA LUISA/OVALO INFANTAS)", #12
           "AV. ALCAZAR (RIMAC)", #13
           "AV.PANAMERICANA SUR (PUENTE ALIPIO PONCE)", #14
           "AV. ARGENTINA (LIMA)", #15
           "AV. AVIACION", #16
           "AV. GRAU (LIMA)", #17
           "AV. ZARUMILLA (SMP)", #18
           "AV. SAN FELIPE (COMAS)", #19
           "AV. BRASIL", #20
           "AV. BELAUNDE (COMAS) PARADERO BELAUNDE/CEMENTERIO JICAMARCA", #21
           "AV. EL SOL (VILLA EL SALVADOR)", #22
           "AV. NICOLAS ARRIOLA (LA VICTORIA)", #23
           "AV.TACNA (LIMA) - AV. WILSON/ GARCILAZO DE LA VEGA", #24
           "AV. CANTA CALLAO", #25
           "AV. CAQUETA -OVALO CAQUETA", #26
           "AV. NICOLAS AYLLON - CARRETERA CENTRAL - PUENTE SANTA ANITA", #27
           "AV. 28 DE JULIO (LIMA)", #28
           "AV. ABANCAY", #29
           "AV. TOMAS MARZANO", #30
           "AV. ALFONSO UGARTE", #31
           "AV. ANGAMOS", #32
           "AV. COLONIAL (LIMA CALLAO)", #33
           "AV. PERU (SMP)", #34
           "AV.AYACUCHO (SURCO)", #35
           "AV.CHIMPU OCLLO (CARABAYLLO)", #36
           "AV. AREQUIPA (LIMA - LINCE -MIRAFLORES - SAN ISIDRO)", #37
           "AV. VENEZUELA (LIMA) - URUGUAY", #38
           "AV. ARAMBURU (SURQUILLO)", #39
           "AV. DEL EJERCITO - PEREZ ARANIBAR (MAGDALENA - SAN ISIDRO)", #40
           "AV. EDUARDO DE HABICH (SMP)", #41
           "AV.NESTOR GAMBETA CALLAO - AV. JUPITER - AV. VENTANILLA", #42
           "AV. LA MARINA/ GUARDIA CHALACA", #43
           "AV. LOS INCAS (COMAS)", #44
           "AV. CHILLON TRAPICHE", #45
           "AV. NICOLAS DE PIEROLA (LA COLMENA)", #46
           "AV.CARLOS IZAGUIRRE (LOS OLIVOS - INDEPENDENCIA)", #47
           "AV. CESAR CANEVARO - SJM", #48
           "AV. CANADA - CANEVARO", #49
           "AV. ANGELICA GAMARRA DE LEON VELARDE", #50
           "PASEO COLON (AV. 9 DE DICIEMBRE)", #51
           "AV. 9 DE OCTUBRE (PARADERO ACHO)", #52
           "AV. BAYOVAR (SJL)", #53
           "AV. REVOLUCION - COLLIQUE COMAS", #54
           "AV. PROCERES DE LA INDEPENDENCIA/ AV. WIESE - PUENTE NUEVO", #55
           "AV. MEXICO (LIMA - LA VICTORIA)", #56
           "AV. ELMER FAUCET - CALLAO", #57
           "AV. EMANCIPACION - LIMA", #58
           "AV. JORGE CHAVEZ (COMAS)", #59
           "AV. EL CORREGIDOR (LA MOLINA)", #60
           "AV. OSCAR R. BENAVIDES (LAS MALVINAS) LIMA", #61
           "AV. CHINCHAYSUYO (PARADERO LINEA SANTA CLARA TAHUANTINSUYO)", #62
           "AV. PASEO DE LA REPUBLICA/ VIA EXPRESA", #63
           "AV. SALAVERRY - JESUS MARIA", #64
           "AV. LAS PALMERAS / ANTUNEZ DE MAYOLO (LOS OLIVOS)", #65
           "AV. NICOLAS DUEÑAS  - LIMA", #66
           "AV.MORALES DUAREZ - LIMA", #67
           "AV. CAMINOS DEL INCA (SURCO)", #68
           "AV. EVITAMIENTO - EL AGUSTINO - SANTA ANITA - ATE", #69
           "AV. JOSE CARLOS MARIATEGUI - RIVA AGÜERO (PUENTE NUEVO) EL AGUSTINO", #70
           "AV. CESAR VALLEJO - AV. CAMINO REAL - FERROCARRIL (EL AGUSTINO)", #71
           "AV. PACHACUTEC - PISTA NUEVA - AV.LOS HEROES  (VMT - SJM -SURCO - VES)", #72
           "AV. HUANDOY - PROCERES DE HUANDOY - LOS OLIVOS", #73
           "AV. BENAVIDES - MIRAFLORES", #74
           "AV. JOSE GRANDA, AV. LOS DOMINICOS (SMP - CALLAO)", #75
           "AV. ZORRITOS - LIMA", #76
           "AV. SAN JUAN - SJM", #77
           "AV. REPUBLICA DE PANAMA (LIMA - SURQUILLO - MIRAFLORES - BARRANCO)", #78
           "AV. SEPARADORA INDUSTRIAL - ATE", #79
           "AV. MATELLINI - CHORRILLOS", #80
           "AV. LAS LOMAS DE CARABAYLLO", #81
           "AV. LARCO - ARMENDARIZ (MIRAFLORES)", #82
           "AV. LA MOLINA -AV.LA UNIVERSIDAD (LA MOLINA)", #83
           "AV. LOS QUECHUAS - ATE (AV. LAS TORRES - SAN LUIS)", #84
           "AV. GRAN CHIMU - MALECON CHECA - MALECON DE LOS LIBERTADORES (ZARATE)", #85
           "AV. SAN JUAN (PUENTE PIEDRA - CARABAYLLO)", #86
           "AV. TRUJILLO (RIMAC)", #87
           "AV. HUMBOLT (LA VICTORIA)", #88
           "AV. IQUITOS", #89
           "AV. HUAYLAS - CHORRILLOS", #90
           "AV. GUARDIA CIVIL  CHORRILLOS", #91
           "AV. ALAMEDA SUR - ALAMEDA DE LOS HORIZONTES/TRANQUERA -CHORRILLOS", #92
           "AV. PEDRO DE OSMA - AV. SAN MARTIN CHORRILLOS", #93
           "AV. ARICA (BREÑA)", #94
           "AV. PASTOR SEVILLA (VILLA EL SALVADOR)", #95
           "AV. EL SOL - AV. SAN JUAN - AV. TUPAC AMARU (CHORRILLOS)", #96
           "AV. LAS TORRES - AV. 5 DE AGOSTO (LURIGANCHO - CHOSICA)", #97
           "AV. JOSE CARLOS MARIATEGUI (VILLA MARIA DEL TRIUNFO)", #98
           "AV. PUNO - COMAS", #100
           "AV.CANTO GRANDE - JOSE CARLOS MARIATEGUI (LAS FLORES SJL)", #101
           "AV. PROCERES - URB. CAMPOY (SAN JUAN DE LURIGANCHO)",
           "AUTOPISTA RAMIRO PRIALE (EL AGUSTINO / LURIGANCHO)",
           "AV. LIMA (VMT - VES - LURIN)",
           "AV. TINGO MARIA - AV. SUCRE (BREÑA - PUEBLO LIBRE)",
           "AV. 200 MILLAS (VENTANILLA)",
           "AV. WIRACOCHA - VENTANILLA",
           "AV. MARCO JARA - VENTANILLA",
           "AV. 225 - VENTANILLA",
           "AV. LA ENCALADA - SURCO",
           "AV. FERRERO - LA MOLINA",
           "AV.MIGUEL IGLESIAS (SAN JUAN DE MIRAFLORES - VES)",
           "AV. BOLOGNESI - BARRANCO",
           "PASEO DE LA REPUBLICA - CHORRILLOS",
           "AV. MALASQUEZ - PACHACAMAC",
           "AV. SEPARADORA INDUSTRIAL - AV. UNION (VMT - VES)",
           "AV. NUEVA TOLEDO - CIENEGUILL",
           "AV. 2 DE MAYO, AV.MARCO POLO (CALLAO)",
           "AV. MANUEL LA VALLE - LURIN",
           "AV. BOLIVAR - PUEBLO LIBRE",
           "AV. SANTA ROSA / JUAN PABLO II - LA PERLA",
           "AV.CONQUISTADORES - COMANDANTE ESPINAR- OVALO GUTIERRES (SAN ISIDRO)",
           "AV. MANCO CAPAC (LA VICTORIA)",
           "AV. DEL AIRE (SAN LUIS)",
           "AV. GRAU (CALLAO)",
           "AV. PROCERES / VIRU (RIMAC)",
           "AV. LAS AMERICAS (LA VICTORIA)",
           "AV. RICARDO PALMA (MIRAFLORES)",
           "AV. JORGE CHAVEZ (BARRANCO)",
           "AV. PACASMAYO (SMP, CALLAO)",
           "AV LIMA NORTE (INDEPENDENCIA), LAS FLORES (LURIGANCHO)",
           "AV PIRAMIDE DEL SOL ( S J LURIGANCHO)",
           "AV. SANTA ROSA (SJL)",
           "AV. G. UNGER, BOLOGNESI, ROSA DE AMERICA, METROPOLITANA (COMAS)",
           "AV. 27 DE DICIEMBRE",
           "AV. AREQUIPA/ ESPAÑA (COMAS)",
           "AV. CIRCUNVALACION",
           "AV CUBA, REP. DOMINICANA, A. TIRADO",
           "AV. CONFRATERNIDAD, 2 DE OCTUBRE, 25 DE ENERO, HONDURAS (LOS OLIVOS, COMAS)")

gen_cod_1 <- data.frame(P37_C_01_1, cod_1)
viajes_pro <- full_join(viajes_pro, gen_cod_1 ,by = "P37_C_01_1")%>% 
  distinct(trip_id, .keep_all = T)


# CODIGO 2
P37_C_01_2 <- c(-9, 1:138)
cod_2 <- c("ninguno",
           "AV.NARANJAL (LOS OLIVOS)",
           "AV.PRIMAVERA (PUENTE) /AV. CIRCUNVALACIÓN",
           "AV. SAN MARTIN (COMAS)",
           "AV. TUPAC AMARU (COMAS)",
           "AV. JAVIER PRADO ( ESTE Y OESTE)",
           "AV. LA MAR (LA VICTORIA)",
           "AV. ISABEL LA CATOLICA (LA VICTORIA)",
           "AV. LA PASCANA (COMAS)",
           "AV. UNIVERSITARIA",
           "AV. TOMAS VALLE",
           "AV. SAENZ PEÑA (CALLAO)",
           "AV. PANAMERICANA NORTE (PARADERO SANTA LUISA/OVALO INFANTAS)",
           "AV. ALCAZAR (RIMAC)",
           "AV.PANAMERICANA SUR (PUENTE ALIPIO PONCE)",
           "AV. ARGENTINA (LIMA)",
           "AV. AVIACION",
           "AV. GRAU (LIMA)",
           "AV. ZARUMILLA (SMP)",
           "AV. SAN FELIPE (COMAS)",
           "AV. BRASIL",
           "AV. BELAUNDE (COMAS) PARADERO BELAUNDE/CEMENTERIO JICAMARCA",
           "AV. EL SOL (VILLA EL SALVADOR)",
           "AV. NICOLAS ARRIOLA (LA VICTORIA)",
           "AV.TACNA (LIMA) - AV. WILSON/ GARCILAZO DE LA VEGA",
           "AV. CANTA CALLAO",
           "AV. CAQUETA -OVALO CAQUETA",
           "AV. NICOLAS AYLLON - CARRETERA CENTRAL - PUENTE SANTA ANITA",
           "AV. 28 DE JULIO (LIMA)",
           "AV. ABANCAY",
           "AV. TOMAS MARZANO",
           "AV. ALFONSO UGARTE",
           "AV. ANGAMOS",
           "AV. COLONIAL (LIMA CALLAO)",
           "AV. PERU (SMP)",
           "AV.AYACUCHO (SURCO)",
           "AV.CHIMPU OCLLO (CARABAYLLO)",
           "AV. AREQUIPA (LIMA - LINCE -MIRAFLORES - SAN ISIDRO)",
           "AV. VENEZUELA (LIMA) - URUGUAY",
           "AV. ARAMBURU (SURQUILLO)",
           "AV. DEL EJERCITO - PEREZ ARANIBAR (MAGDALENA - SAN ISIDRO)",
           "AV. EDUARDO DE HABICH (SMP)",
           "AV.NESTOR GAMBETA CALLAO - AV. JUPITER - AV. VENTANILLA",
           "AV. LA MARINA/ GUARDIA CHALACA",
           "AV. LOS INCAS (COMAS)",
           "AV. CHILLON TRAPICHE",
           "AV. NICOLAS DE PIEROLA (LA COLMENA)",
           "AV.CARLOS IZAGUIRRE (LOS OLIVOS - INDEPENDENCIA)",
           "AV. CESAR CANEVARO - SJM",
           "AV. CANADA - CANEVARO",
           "AV. ANGELICA GAMARRA DE LEON VELARDE",
           "PASEO COLON (AV. 9 DE DICIEMBRE)",
           "AV. 9 DE OCTUBRE (PARADERO ACHO)",
           "AV. BAYOVAR (SJL)",
           "AV. REVOLUCION - COLLIQUE COMAS",
           "AV. PROCERES DE LA INDEPENDENCIA/ AV. WIESE - PUENTE NUEVO",
           "AV. MEXICO (LIMA - LA VICTORIA)",
           "AV. ELMER FAUCET - CALLAO",
           "AV. EMANCIPACION - LIMA",
           "AV. JORGE CHAVEZ (COMAS)",
           "AV. EL CORREGIDOR (LA MOLINA)",
           "AV. OSCAR R. BENAVIDES (LAS MALVINAS) LIMA",
           "AV. CHINCHAYSUYO (PARADERO LINEA SANTA CLARA TAHUANTINSUYO)",
           "AV. PASEO DE LA REPUBLICA/ VIA EXPRESA",
           "AV. SALAVERRY - JESUS MARIA",
           "AV. LAS PALMERAS / ANTUNEZ DE MAYOLO (LOS OLIVOS)",
           "AV. NICOLAS DUEÑAS  - LIMA",
           "AV.MORALES DUAREZ - LIMA",
           "AV. CAMINOS DEL INCA (SURCO)",
           "AV. EVITAMIENTO - EL AGUSTINO - SANTA ANITA - ATE",
           "AV. JOSE CARLOS MARIATEGUI - RIVA AGÜERO (PUENTE NUEVO) EL AGUSTINO",
           "AV. CESAR VALLEJO - AV. CAMINO REAL - FERROCARRIL (EL AGUSTINO)",
           "AV. PACHACUTEC - PISTA NUEVA - AV.LOS HEROES  (VMT - SJM -SURCO - VES)",
           "AV. HUANDOY - PROCERES DE HUANDOY - LOS OLIVOS",
           "AV. BENAVIDES - MIRAFLORES",
           "AV. JOSE GRANDA, AV. LOS DOMINICOS (SMP - CALLAO)",
           "AV. ZORRITOS - LIMA",
           "AV. SAN JUAN - SJM",
           "AV. REPUBLICA DE PANAMA (LIMA - SURQUILLO - MIRAFLORES - BARRANCO)",
           "AV. SEPARADORA INDUSTRIAL - ATE",
           "AV. MATELLINI - CHORRILLOS",
           "AV. LAS LOMAS DE CARABAYLLO",
           "AV. LARCO - ARMENDARIZ (MIRAFLORES)",
           "AV. LA MOLINA -AV.LA UNIVERSIDAD (LA MOLINA)",
           "AV. LOS QUECHUAS - ATE (AV. LAS TORRES - SAN LUIS)",
           "AV. GRAN CHIMU - MALECON CHECA - MALECON DE LOS LIBERTADORES (ZARATE)",
           "AV. SAN JUAN (PUENTE PIEDRA - CARABAYLLO)",
           "AV. TRUJILLO (RIMAC)",
           "AV. HUMBOLT (LA VICTORIA)",
           "AV. IQUITOS",
           "AV. HUAYLAS - CHORRILLOS",
           "AV. GUARDIA CIVIL  CHORRILLOS",
           "AV. ALAMEDA SUR - ALAMEDA DE LOS HORIZONTES/TRANQUERA -CHORRILLOS",
           "AV. PEDRO DE OSMA - AV. SAN MARTIN CHORRILLOS",
           "AV. ARICA (BREÑA)",
           "AV. PASTOR SEVILLA (VILLA EL SALVADOR)",
           "AV. EL SOL - AV. SAN JUAN - AV. TUPAC AMARU (CHORRILLOS)",
           "AV. LAS TORRES - AV. 5 DE AGOSTO (LURIGANCHO - CHOSICA)",
           "AV. JOSE CARLOS MARIATEGUI (VILLA MARIA DEL TRIUNFO)",
           "AV. PUNO - COMAS",
           "AV.CANTO GRANDE - JOSE CARLOS MARIATEGUI (LAS FLORES SJL)",
           "AV. PROCERES - URB. CAMPOY (SAN JUAN DE LURIGANCHO)",
           "AUTOPISTA RAMIRO PRIALE (EL AGUSTINO / LURIGANCHO)",
           "AV. LIMA (VMT - VES - LURIN)",
           "AV. TINGO MARIA - AV. SUCRE (BREÑA - PUEBLO LIBRE)",
           "AV. 200 MILLAS (VENTANILLA)",
           "AV. WIRACOCHA - VENTANILLA",
           "AV. MARCO JARA - VENTANILLA",
           "AV. 225 - VENTANILLA",
           "AV. LA ENCALADA - SURCO",
           "AV. FERRERO - LA MOLINA",
           "AV.MIGUEL IGLESIAS (SAN JUAN DE MIRAFLORES - VES)",
           "AV. BOLOGNESI - BARRANCO",
           "PASEO DE LA REPUBLICA - CHORRILLOS",
           "AV. MALASQUEZ - PACHACAMAC",
           "AV. SEPARADORA INDUSTRIAL - AV. UNION (VMT - VES)",
           "AV. NUEVA TOLEDO - CIENEGUILL",
           "AV. 2 DE MAYO, AV.MARCO POLO (CALLAO)",
           "AV. MANUEL LA VALLE - LURIN",
           "AV. BOLIVAR - PUEBLO LIBRE",
           "AV. SANTA ROSA / JUAN PABLO II - LA PERLA",
           "AV.CONQUISTADORES - COMANDANTE ESPINAR- OVALO GUTIERRES (SAN ISIDRO)",
           "AV. MANCO CAPAC (LA VICTORIA)",
           "AV. DEL AIRE (SAN LUIS)",
           "AV. GRAU (CALLAO)",
           "AV. PROCERES / VIRU (RIMAC)",
           "AV. LAS AMERICAS (LA VICTORIA)",
           "AV. RICARDO PALMA (MIRAFLORES)",
           "AV. JORGE CHAVEZ (BARRANCO)",
           "AV. PACASMAYO (SMP, CALLAO)",
           "AV LIMA NORTE (INDEPENDENCIA), LAS FLORES (LURIGANCHO)",
           "AV PIRAMIDE DEL SOL ( S J LURIGANCHO)",
           "AV. SANTA ROSA (SJL)",
           "AV. G. UNGER, BOLOGNESI, ROSA DE AMERICA, METROPOLITANA (COMAS)",
           "AV. 27 DE DICIEMBRE",
           "AV. AREQUIPA/ ESPAÑA (COMAS)",
           "AV. CIRCUNVALACION",
           "AV CUBA, REP. DOMINICANA, A. TIRADO",
           "AV. CONFRATERNIDAD, 2 DE OCTUBRE, 25 DE ENERO, HONDURAS (LOS OLIVOS, COMAS)")

gen_cod_2 <- data.frame(P37_C_01_2, cod_2)
viajes_pro <- full_join(viajes_pro, gen_cod_2 ,by = "P37_C_01_2")%>% 
  distinct(trip_id, .keep_all = T)


# CODIGO 3
P37_C_01_3 <- c(-9, 1:138)
cod_3 <- c("ninguno",
           "AV.NARANJAL (LOS OLIVOS)",
           "AV.PRIMAVERA (PUENTE) /AV. CIRCUNVALACIÓN",
           "AV. SAN MARTIN (COMAS)",
           "AV. TUPAC AMARU (COMAS)",
           "AV. JAVIER PRADO ( ESTE Y OESTE)",
           "AV. LA MAR (LA VICTORIA)",
           "AV. ISABEL LA CATOLICA (LA VICTORIA)",
           "AV. LA PASCANA (COMAS)",
           "AV. UNIVERSITARIA",
           "AV. TOMAS VALLE",
           "AV. SAENZ PEÑA (CALLAO)",
           "AV. PANAMERICANA NORTE (PARADERO SANTA LUISA/OVALO INFANTAS)",
           "AV. ALCAZAR (RIMAC)",
           "AV.PANAMERICANA SUR (PUENTE ALIPIO PONCE)",
           "AV. ARGENTINA (LIMA)",
           "AV. AVIACION",
           "AV. GRAU (LIMA)",
           "AV. ZARUMILLA (SMP)",
           "AV. SAN FELIPE (COMAS)",
           "AV. BRASIL",
           "AV. BELAUNDE (COMAS) PARADERO BELAUNDE/CEMENTERIO JICAMARCA",
           "AV. EL SOL (VILLA EL SALVADOR)",
           "AV. NICOLAS ARRIOLA (LA VICTORIA)",
           "AV.TACNA (LIMA) - AV. WILSON/ GARCILAZO DE LA VEGA",
           "AV. CANTA CALLAO",
           "AV. CAQUETA -OVALO CAQUETA",
           "AV. NICOLAS AYLLON - CARRETERA CENTRAL - PUENTE SANTA ANITA",
           "AV. 28 DE JULIO (LIMA)",
           "AV. ABANCAY",
           "AV. TOMAS MARZANO",
           "AV. ALFONSO UGARTE",
           "AV. ANGAMOS",
           "AV. COLONIAL (LIMA CALLAO)",
           "AV. PERU (SMP)",
           "AV.AYACUCHO (SURCO)",
           "AV.CHIMPU OCLLO (CARABAYLLO)",
           "AV. AREQUIPA (LIMA - LINCE -MIRAFLORES - SAN ISIDRO)",
           "AV. VENEZUELA (LIMA) - URUGUAY",
           "AV. ARAMBURU (SURQUILLO)",
           "AV. DEL EJERCITO - PEREZ ARANIBAR (MAGDALENA - SAN ISIDRO)",
           "AV. EDUARDO DE HABICH (SMP)",
           "AV.NESTOR GAMBETA CALLAO - AV. JUPITER - AV. VENTANILLA",
           "AV. LA MARINA/ GUARDIA CHALACA",
           "AV. LOS INCAS (COMAS)",
           "AV. CHILLON TRAPICHE",
           "AV. NICOLAS DE PIEROLA (LA COLMENA)",
           "AV.CARLOS IZAGUIRRE (LOS OLIVOS - INDEPENDENCIA)",
           "AV. CESAR CANEVARO - SJM",
           "AV. CANADA - CANEVARO",
           "AV. ANGELICA GAMARRA DE LEON VELARDE",
           "PASEO COLON (AV. 9 DE DICIEMBRE)",
           "AV. 9 DE OCTUBRE (PARADERO ACHO)",
           "AV. BAYOVAR (SJL)",
           "AV. REVOLUCION - COLLIQUE COMAS",
           "AV. PROCERES DE LA INDEPENDENCIA/ AV. WIESE - PUENTE NUEVO",
           "AV. MEXICO (LIMA - LA VICTORIA)",
           "AV. ELMER FAUCET - CALLAO",
           "AV. EMANCIPACION - LIMA",
           "AV. JORGE CHAVEZ (COMAS)",
           "AV. EL CORREGIDOR (LA MOLINA)",
           "AV. OSCAR R. BENAVIDES (LAS MALVINAS) LIMA",
           "AV. CHINCHAYSUYO (PARADERO LINEA SANTA CLARA TAHUANTINSUYO)",
           "AV. PASEO DE LA REPUBLICA/ VIA EXPRESA",
           "AV. SALAVERRY - JESUS MARIA",
           "AV. LAS PALMERAS / ANTUNEZ DE MAYOLO (LOS OLIVOS)",
           "AV. NICOLAS DUEÑAS  - LIMA",
           "AV.MORALES DUAREZ - LIMA",
           "AV. CAMINOS DEL INCA (SURCO)",
           "AV. EVITAMIENTO - EL AGUSTINO - SANTA ANITA - ATE",
           "AV. JOSE CARLOS MARIATEGUI - RIVA AGÜERO (PUENTE NUEVO) EL AGUSTINO",
           "AV. CESAR VALLEJO - AV. CAMINO REAL - FERROCARRIL (EL AGUSTINO)",
           "AV. PACHACUTEC - PISTA NUEVA - AV.LOS HEROES  (VMT - SJM -SURCO - VES)",
           "AV. HUANDOY - PROCERES DE HUANDOY - LOS OLIVOS",
           "AV. BENAVIDES - MIRAFLORES",
           "AV. JOSE GRANDA, AV. LOS DOMINICOS (SMP - CALLAO)",
           "AV. ZORRITOS - LIMA",
           "AV. SAN JUAN - SJM",
           "AV. REPUBLICA DE PANAMA (LIMA - SURQUILLO - MIRAFLORES - BARRANCO)",
           "AV. SEPARADORA INDUSTRIAL - ATE",
           "AV. MATELLINI - CHORRILLOS",
           "AV. LAS LOMAS DE CARABAYLLO",
           "AV. LARCO - ARMENDARIZ (MIRAFLORES)",
           "AV. LA MOLINA -AV.LA UNIVERSIDAD (LA MOLINA)",
           "AV. LOS QUECHUAS - ATE (AV. LAS TORRES - SAN LUIS)",
           "AV. GRAN CHIMU - MALECON CHECA - MALECON DE LOS LIBERTADORES (ZARATE)",
           "AV. SAN JUAN (PUENTE PIEDRA - CARABAYLLO)",
           "AV. TRUJILLO (RIMAC)",
           "AV. HUMBOLT (LA VICTORIA)",
           "AV. IQUITOS",
           "AV. HUAYLAS - CHORRILLOS",
           "AV. GUARDIA CIVIL  CHORRILLOS",
           "AV. ALAMEDA SUR - ALAMEDA DE LOS HORIZONTES/TRANQUERA -CHORRILLOS",
           "AV. PEDRO DE OSMA - AV. SAN MARTIN CHORRILLOS",
           "AV. ARICA (BREÑA)",
           "AV. PASTOR SEVILLA (VILLA EL SALVADOR)",
           "AV. EL SOL - AV. SAN JUAN - AV. TUPAC AMARU (CHORRILLOS)",
           "AV. LAS TORRES - AV. 5 DE AGOSTO (LURIGANCHO - CHOSICA)",
           "AV. JOSE CARLOS MARIATEGUI (VILLA MARIA DEL TRIUNFO)",
           "AV. PUNO - COMAS",
           "AV.CANTO GRANDE - JOSE CARLOS MARIATEGUI (LAS FLORES SJL)",
           "AV. PROCERES - URB. CAMPOY (SAN JUAN DE LURIGANCHO)",
           "AUTOPISTA RAMIRO PRIALE (EL AGUSTINO / LURIGANCHO)",
           "AV. LIMA (VMT - VES - LURIN)",
           "AV. TINGO MARIA - AV. SUCRE (BREÑA - PUEBLO LIBRE)",
           "AV. 200 MILLAS (VENTANILLA)",
           "AV. WIRACOCHA - VENTANILLA",
           "AV. MARCO JARA - VENTANILLA",
           "AV. 225 - VENTANILLA",
           "AV. LA ENCALADA - SURCO",
           "AV. FERRERO - LA MOLINA",
           "AV.MIGUEL IGLESIAS (SAN JUAN DE MIRAFLORES - VES)",
           "AV. BOLOGNESI - BARRANCO",
           "PASEO DE LA REPUBLICA - CHORRILLOS",
           "AV. MALASQUEZ - PACHACAMAC",
           "AV. SEPARADORA INDUSTRIAL - AV. UNION (VMT - VES)",
           "AV. NUEVA TOLEDO - CIENEGUILL",
           "AV. 2 DE MAYO, AV.MARCO POLO (CALLAO)",
           "AV. MANUEL LA VALLE - LURIN",
           "AV. BOLIVAR - PUEBLO LIBRE",
           "AV. SANTA ROSA / JUAN PABLO II - LA PERLA",
           "AV.CONQUISTADORES - COMANDANTE ESPINAR- OVALO GUTIERRES (SAN ISIDRO)",
           "AV. MANCO CAPAC (LA VICTORIA)",
           "AV. DEL AIRE (SAN LUIS)",
           "AV. GRAU (CALLAO)",
           "AV. PROCERES / VIRU (RIMAC)",
           "AV. LAS AMERICAS (LA VICTORIA)",
           "AV. RICARDO PALMA (MIRAFLORES)",
           "AV. JORGE CHAVEZ (BARRANCO)",
           "AV. PACASMAYO (SMP, CALLAO)",
           "AV LIMA NORTE (INDEPENDENCIA), LAS FLORES (LURIGANCHO)",
           "AV PIRAMIDE DEL SOL ( S J LURIGANCHO)",
           "AV. SANTA ROSA (SJL)",
           "AV. G. UNGER, BOLOGNESI, ROSA DE AMERICA, METROPOLITANA (COMAS)",
           "AV. 27 DE DICIEMBRE",
           "AV. AREQUIPA/ ESPAÑA (COMAS)",
           "AV. CIRCUNVALACION",
           "AV CUBA, REP. DOMINICANA, A. TIRADO",
           "AV. CONFRATERNIDAD, 2 DE OCTUBRE, 25 DE ENERO, HONDURAS (LOS OLIVOS, COMAS)")

gen_cod_3 <- data.frame(P37_C_01_3, cod_3)
viajes_pro <- full_join(viajes_pro, gen_cod_3 ,by = "P37_C_01_3")%>% 
  distinct(trip_id, .keep_all = T)



# CODIGO 4
P37_C_01_4 <- c(-9, 1:138)
cod_4 <- c("ninguno",
           "AV.NARANJAL (LOS OLIVOS)",
           "AV.PRIMAVERA (PUENTE) /AV. CIRCUNVALACIÓN",
           "AV. SAN MARTIN (COMAS)",
           "AV. TUPAC AMARU (COMAS)",
           "AV. JAVIER PRADO ( ESTE Y OESTE)",
           "AV. LA MAR (LA VICTORIA)",
           "AV. ISABEL LA CATOLICA (LA VICTORIA)",
           "AV. LA PASCANA (COMAS)",
           "AV. UNIVERSITARIA",
           "AV. TOMAS VALLE",
           "AV. SAENZ PEÑA (CALLAO)",
           "AV. PANAMERICANA NORTE (PARADERO SANTA LUISA/OVALO INFANTAS)",
           "AV. ALCAZAR (RIMAC)",
           "AV.PANAMERICANA SUR (PUENTE ALIPIO PONCE)",
           "AV. ARGENTINA (LIMA)",
           "AV. AVIACION",
           "AV. GRAU (LIMA)",
           "AV. ZARUMILLA (SMP)",
           "AV. SAN FELIPE (COMAS)",
           "AV. BRASIL",
           "AV. BELAUNDE (COMAS) PARADERO BELAUNDE/CEMENTERIO JICAMARCA",
           "AV. EL SOL (VILLA EL SALVADOR)",
           "AV. NICOLAS ARRIOLA (LA VICTORIA)",
           "AV.TACNA (LIMA) - AV. WILSON/ GARCILAZO DE LA VEGA",
           "AV. CANTA CALLAO",
           "AV. CAQUETA -OVALO CAQUETA",
           "AV. NICOLAS AYLLON - CARRETERA CENTRAL - PUENTE SANTA ANITA",
           "AV. 28 DE JULIO (LIMA)",
           "AV. ABANCAY",
           "AV. TOMAS MARZANO",
           "AV. ALFONSO UGARTE",
           "AV. ANGAMOS",
           "AV. COLONIAL (LIMA CALLAO)",
           "AV. PERU (SMP)",
           "AV.AYACUCHO (SURCO)",
           "AV.CHIMPU OCLLO (CARABAYLLO)",
           "AV. AREQUIPA (LIMA - LINCE -MIRAFLORES - SAN ISIDRO)",
           "AV. VENEZUELA (LIMA) - URUGUAY",
           "AV. ARAMBURU (SURQUILLO)",
           "AV. DEL EJERCITO - PEREZ ARANIBAR (MAGDALENA - SAN ISIDRO)",
           "AV. EDUARDO DE HABICH (SMP)",
           "AV.NESTOR GAMBETA CALLAO - AV. JUPITER - AV. VENTANILLA",
           "AV. LA MARINA/ GUARDIA CHALACA",
           "AV. LOS INCAS (COMAS)",
           "AV. CHILLON TRAPICHE",
           "AV. NICOLAS DE PIEROLA (LA COLMENA)",
           "AV.CARLOS IZAGUIRRE (LOS OLIVOS - INDEPENDENCIA)",
           "AV. CESAR CANEVARO - SJM",
           "AV. CANADA - CANEVARO",
           "AV. ANGELICA GAMARRA DE LEON VELARDE",
           "PASEO COLON (AV. 9 DE DICIEMBRE)",
           "AV. 9 DE OCTUBRE (PARADERO ACHO)",
           "AV. BAYOVAR (SJL)",
           "AV. REVOLUCION - COLLIQUE COMAS",
           "AV. PROCERES DE LA INDEPENDENCIA/ AV. WIESE - PUENTE NUEVO",
           "AV. MEXICO (LIMA - LA VICTORIA)",
           "AV. ELMER FAUCET - CALLAO",
           "AV. EMANCIPACION - LIMA",
           "AV. JORGE CHAVEZ (COMAS)",
           "AV. EL CORREGIDOR (LA MOLINA)",
           "AV. OSCAR R. BENAVIDES (LAS MALVINAS) LIMA",
           "AV. CHINCHAYSUYO (PARADERO LINEA SANTA CLARA TAHUANTINSUYO)",
           "AV. PASEO DE LA REPUBLICA/ VIA EXPRESA",
           "AV. SALAVERRY - JESUS MARIA",
           "AV. LAS PALMERAS / ANTUNEZ DE MAYOLO (LOS OLIVOS)",
           "AV. NICOLAS DUEÑAS  - LIMA",
           "AV.MORALES DUAREZ - LIMA",
           "AV. CAMINOS DEL INCA (SURCO)",
           "AV. EVITAMIENTO - EL AGUSTINO - SANTA ANITA - ATE",
           "AV. JOSE CARLOS MARIATEGUI - RIVA AGÜERO (PUENTE NUEVO) EL AGUSTINO",
           "AV. CESAR VALLEJO - AV. CAMINO REAL - FERROCARRIL (EL AGUSTINO)",
           "AV. PACHACUTEC - PISTA NUEVA - AV.LOS HEROES  (VMT - SJM -SURCO - VES)",
           "AV. HUANDOY - PROCERES DE HUANDOY - LOS OLIVOS",
           "AV. BENAVIDES - MIRAFLORES",
           "AV. JOSE GRANDA, AV. LOS DOMINICOS (SMP - CALLAO)",
           "AV. ZORRITOS - LIMA",
           "AV. SAN JUAN - SJM",
           "AV. REPUBLICA DE PANAMA (LIMA - SURQUILLO - MIRAFLORES - BARRANCO)",
           "AV. SEPARADORA INDUSTRIAL - ATE",
           "AV. MATELLINI - CHORRILLOS",
           "AV. LAS LOMAS DE CARABAYLLO",
           "AV. LARCO - ARMENDARIZ (MIRAFLORES)",
           "AV. LA MOLINA -AV.LA UNIVERSIDAD (LA MOLINA)",
           "AV. LOS QUECHUAS - ATE (AV. LAS TORRES - SAN LUIS)",
           "AV. GRAN CHIMU - MALECON CHECA - MALECON DE LOS LIBERTADORES (ZARATE)",
           "AV. SAN JUAN (PUENTE PIEDRA - CARABAYLLO)",
           "AV. TRUJILLO (RIMAC)",
           "AV. HUMBOLT (LA VICTORIA)",
           "AV. IQUITOS",
           "AV. HUAYLAS - CHORRILLOS",
           "AV. GUARDIA CIVIL  CHORRILLOS",
           "AV. ALAMEDA SUR - ALAMEDA DE LOS HORIZONTES/TRANQUERA -CHORRILLOS",
           "AV. PEDRO DE OSMA - AV. SAN MARTIN CHORRILLOS",
           "AV. ARICA (BREÑA)",
           "AV. PASTOR SEVILLA (VILLA EL SALVADOR)",
           "AV. EL SOL - AV. SAN JUAN - AV. TUPAC AMARU (CHORRILLOS)",
           "AV. LAS TORRES - AV. 5 DE AGOSTO (LURIGANCHO - CHOSICA)",
           "AV. JOSE CARLOS MARIATEGUI (VILLA MARIA DEL TRIUNFO)",
           "AV. PUNO - COMAS",
           "AV.CANTO GRANDE - JOSE CARLOS MARIATEGUI (LAS FLORES SJL)",
           "AV. PROCERES - URB. CAMPOY (SAN JUAN DE LURIGANCHO)",
           "AUTOPISTA RAMIRO PRIALE (EL AGUSTINO / LURIGANCHO)",
           "AV. LIMA (VMT - VES - LURIN)",
           "AV. TINGO MARIA - AV. SUCRE (BREÑA - PUEBLO LIBRE)",
           "AV. 200 MILLAS (VENTANILLA)",
           "AV. WIRACOCHA - VENTANILLA",
           "AV. MARCO JARA - VENTANILLA",
           "AV. 225 - VENTANILLA",
           "AV. LA ENCALADA - SURCO",
           "AV. FERRERO - LA MOLINA",
           "AV.MIGUEL IGLESIAS (SAN JUAN DE MIRAFLORES - VES)",
           "AV. BOLOGNESI - BARRANCO",
           "PASEO DE LA REPUBLICA - CHORRILLOS",
           "AV. MALASQUEZ - PACHACAMAC",
           "AV. SEPARADORA INDUSTRIAL - AV. UNION (VMT - VES)",
           "AV. NUEVA TOLEDO - CIENEGUILL",
           "AV. 2 DE MAYO, AV.MARCO POLO (CALLAO)",
           "AV. MANUEL LA VALLE - LURIN",
           "AV. BOLIVAR - PUEBLO LIBRE",
           "AV. SANTA ROSA / JUAN PABLO II - LA PERLA",
           "AV.CONQUISTADORES - COMANDANTE ESPINAR- OVALO GUTIERRES (SAN ISIDRO)",
           "AV. MANCO CAPAC (LA VICTORIA)",
           "AV. DEL AIRE (SAN LUIS)",
           "AV. GRAU (CALLAO)",
           "AV. PROCERES / VIRU (RIMAC)",
           "AV. LAS AMERICAS (LA VICTORIA)",
           "AV. RICARDO PALMA (MIRAFLORES)",
           "AV. JORGE CHAVEZ (BARRANCO)",
           "AV. PACASMAYO (SMP, CALLAO)",
           "AV LIMA NORTE (INDEPENDENCIA), LAS FLORES (LURIGANCHO)",
           "AV PIRAMIDE DEL SOL ( S J LURIGANCHO)",
           "AV. SANTA ROSA (SJL)",
           "AV. G. UNGER, BOLOGNESI, ROSA DE AMERICA, METROPOLITANA (COMAS)",
           "AV. 27 DE DICIEMBRE",
           "AV. AREQUIPA/ ESPAÑA (COMAS)",
           "AV. CIRCUNVALACION",
           "AV CUBA, REP. DOMINICANA, A. TIRADO",
           "AV. CONFRATERNIDAD, 2 DE OCTUBRE, 25 DE ENERO, HONDURAS (LOS OLIVOS, COMAS)")

gen_cod_4 <- data.frame(P37_C_01_4, cod_4)
viajes_pro <- full_join(viajes_pro, gen_cod_4 ,by = "P37_C_01_4")%>% 
  distinct(trip_id, .keep_all = T)

# CODIGO 5
P37_C_01_5 <- c(-9, 1:138)
cod_5 <- c("ninguno",
           "AV.NARANJAL (LOS OLIVOS)",
           "AV.PRIMAVERA (PUENTE) /AV. CIRCUNVALACIÓN",
           "AV. SAN MARTIN (COMAS)",
           "AV. TUPAC AMARU (COMAS)",
           "AV. JAVIER PRADO ( ESTE Y OESTE)",
           "AV. LA MAR (LA VICTORIA)",
           "AV. ISABEL LA CATOLICA (LA VICTORIA)",
           "AV. LA PASCANA (COMAS)",
           "AV. UNIVERSITARIA",
           "AV. TOMAS VALLE",
           "AV. SAENZ PEÑA (CALLAO)",
           "AV. PANAMERICANA NORTE (PARADERO SANTA LUISA/OVALO INFANTAS)",
           "AV. ALCAZAR (RIMAC)",
           "AV.PANAMERICANA SUR (PUENTE ALIPIO PONCE)",
           "AV. ARGENTINA (LIMA)",
           "AV. AVIACION",
           "AV. GRAU (LIMA)",
           "AV. ZARUMILLA (SMP)",
           "AV. SAN FELIPE (COMAS)",
           "AV. BRASIL",
           "AV. BELAUNDE (COMAS) PARADERO BELAUNDE/CEMENTERIO JICAMARCA",
           "AV. EL SOL (VILLA EL SALVADOR)",
           "AV. NICOLAS ARRIOLA (LA VICTORIA)",
           "AV.TACNA (LIMA) - AV. WILSON/ GARCILAZO DE LA VEGA",
           "AV. CANTA CALLAO",
           "AV. CAQUETA -OVALO CAQUETA",
           "AV. NICOLAS AYLLON - CARRETERA CENTRAL - PUENTE SANTA ANITA",
           "AV. 28 DE JULIO (LIMA)",
           "AV. ABANCAY",
           "AV. TOMAS MARZANO",
           "AV. ALFONSO UGARTE",
           "AV. ANGAMOS",
           "AV. COLONIAL (LIMA CALLAO)",
           "AV. PERU (SMP)",
           "AV.AYACUCHO (SURCO)",
           "AV.CHIMPU OCLLO (CARABAYLLO)",
           "AV. AREQUIPA (LIMA - LINCE -MIRAFLORES - SAN ISIDRO)",
           "AV. VENEZUELA (LIMA) - URUGUAY",
           "AV. ARAMBURU (SURQUILLO)",
           "AV. DEL EJERCITO - PEREZ ARANIBAR (MAGDALENA - SAN ISIDRO)",
           "AV. EDUARDO DE HABICH (SMP)",
           "AV.NESTOR GAMBETA CALLAO - AV. JUPITER - AV. VENTANILLA",
           "AV. LA MARINA/ GUARDIA CHALACA",
           "AV. LOS INCAS (COMAS)",
           "AV. CHILLON TRAPICHE",
           "AV. NICOLAS DE PIEROLA (LA COLMENA)",
           "AV.CARLOS IZAGUIRRE (LOS OLIVOS - INDEPENDENCIA)",
           "AV. CESAR CANEVARO - SJM",
           "AV. CANADA - CANEVARO",
           "AV. ANGELICA GAMARRA DE LEON VELARDE",
           "PASEO COLON (AV. 9 DE DICIEMBRE)",
           "AV. 9 DE OCTUBRE (PARADERO ACHO)",
           "AV. BAYOVAR (SJL)",
           "AV. REVOLUCION - COLLIQUE COMAS",
           "AV. PROCERES DE LA INDEPENDENCIA/ AV. WIESE - PUENTE NUEVO",
           "AV. MEXICO (LIMA - LA VICTORIA)",
           "AV. ELMER FAUCET - CALLAO",
           "AV. EMANCIPACION - LIMA",
           "AV. JORGE CHAVEZ (COMAS)",
           "AV. EL CORREGIDOR (LA MOLINA)",
           "AV. OSCAR R. BENAVIDES (LAS MALVINAS) LIMA",
           "AV. CHINCHAYSUYO (PARADERO LINEA SANTA CLARA TAHUANTINSUYO)",
           "AV. PASEO DE LA REPUBLICA/ VIA EXPRESA",
           "AV. SALAVERRY - JESUS MARIA",
           "AV. LAS PALMERAS / ANTUNEZ DE MAYOLO (LOS OLIVOS)",
           "AV. NICOLAS DUEÑAS  - LIMA",
           "AV.MORALES DUAREZ - LIMA",
           "AV. CAMINOS DEL INCA (SURCO)",
           "AV. EVITAMIENTO - EL AGUSTINO - SANTA ANITA - ATE",
           "AV. JOSE CARLOS MARIATEGUI - RIVA AGÜERO (PUENTE NUEVO) EL AGUSTINO",
           "AV. CESAR VALLEJO - AV. CAMINO REAL - FERROCARRIL (EL AGUSTINO)",
           "AV. PACHACUTEC - PISTA NUEVA - AV.LOS HEROES  (VMT - SJM -SURCO - VES)",
           "AV. HUANDOY - PROCERES DE HUANDOY - LOS OLIVOS",
           "AV. BENAVIDES - MIRAFLORES",
           "AV. JOSE GRANDA, AV. LOS DOMINICOS (SMP - CALLAO)",
           "AV. ZORRITOS - LIMA",
           "AV. SAN JUAN - SJM",
           "AV. REPUBLICA DE PANAMA (LIMA - SURQUILLO - MIRAFLORES - BARRANCO)",
           "AV. SEPARADORA INDUSTRIAL - ATE",
           "AV. MATELLINI - CHORRILLOS",
           "AV. LAS LOMAS DE CARABAYLLO",
           "AV. LARCO - ARMENDARIZ (MIRAFLORES)",
           "AV. LA MOLINA -AV.LA UNIVERSIDAD (LA MOLINA)",
           "AV. LOS QUECHUAS - ATE (AV. LAS TORRES - SAN LUIS)",
           "AV. GRAN CHIMU - MALECON CHECA - MALECON DE LOS LIBERTADORES (ZARATE)",
           "AV. SAN JUAN (PUENTE PIEDRA - CARABAYLLO)",
           "AV. TRUJILLO (RIMAC)",
           "AV. HUMBOLT (LA VICTORIA)",
           "AV. IQUITOS",
           "AV. HUAYLAS - CHORRILLOS",
           "AV. GUARDIA CIVIL  CHORRILLOS",
           "AV. ALAMEDA SUR - ALAMEDA DE LOS HORIZONTES/TRANQUERA -CHORRILLOS",
           "AV. PEDRO DE OSMA - AV. SAN MARTIN CHORRILLOS",
           "AV. ARICA (BREÑA)",
           "AV. PASTOR SEVILLA (VILLA EL SALVADOR)",
           "AV. EL SOL - AV. SAN JUAN - AV. TUPAC AMARU (CHORRILLOS)",
           "AV. LAS TORRES - AV. 5 DE AGOSTO (LURIGANCHO - CHOSICA)",
           "AV. JOSE CARLOS MARIATEGUI (VILLA MARIA DEL TRIUNFO)",
           "AV. PUNO - COMAS",
           "AV.CANTO GRANDE - JOSE CARLOS MARIATEGUI (LAS FLORES SJL)",
           "AV. PROCERES - URB. CAMPOY (SAN JUAN DE LURIGANCHO)",
           "AUTOPISTA RAMIRO PRIALE (EL AGUSTINO / LURIGANCHO)",
           "AV. LIMA (VMT - VES - LURIN)",
           "AV. TINGO MARIA - AV. SUCRE (BREÑA - PUEBLO LIBRE)",
           "AV. 200 MILLAS (VENTANILLA)",
           "AV. WIRACOCHA - VENTANILLA",
           "AV. MARCO JARA - VENTANILLA",
           "AV. 225 - VENTANILLA",
           "AV. LA ENCALADA - SURCO",
           "AV. FERRERO - LA MOLINA",
           "AV.MIGUEL IGLESIAS (SAN JUAN DE MIRAFLORES - VES)",
           "AV. BOLOGNESI - BARRANCO",
           "PASEO DE LA REPUBLICA - CHORRILLOS",
           "AV. MALASQUEZ - PACHACAMAC",
           "AV. SEPARADORA INDUSTRIAL - AV. UNION (VMT - VES)",
           "AV. NUEVA TOLEDO - CIENEGUILL",
           "AV. 2 DE MAYO, AV.MARCO POLO (CALLAO)",
           "AV. MANUEL LA VALLE - LURIN",
           "AV. BOLIVAR - PUEBLO LIBRE",
           "AV. SANTA ROSA / JUAN PABLO II - LA PERLA",
           "AV.CONQUISTADORES - COMANDANTE ESPINAR- OVALO GUTIERRES (SAN ISIDRO)",
           "AV. MANCO CAPAC (LA VICTORIA)",
           "AV. DEL AIRE (SAN LUIS)",
           "AV. GRAU (CALLAO)",
           "AV. PROCERES / VIRU (RIMAC)",
           "AV. LAS AMERICAS (LA VICTORIA)",
           "AV. RICARDO PALMA (MIRAFLORES)",
           "AV. JORGE CHAVEZ (BARRANCO)",
           "AV. PACASMAYO (SMP, CALLAO)",
           "AV LIMA NORTE (INDEPENDENCIA), LAS FLORES (LURIGANCHO)",
           "AV PIRAMIDE DEL SOL ( S J LURIGANCHO)",
           "AV. SANTA ROSA (SJL)",
           "AV. G. UNGER, BOLOGNESI, ROSA DE AMERICA, METROPOLITANA (COMAS)",
           "AV. 27 DE DICIEMBRE",
           "AV. AREQUIPA/ ESPAÑA (COMAS)",
           "AV. CIRCUNVALACION",
           "AV CUBA, REP. DOMINICANA, A. TIRADO",
           "AV. CONFRATERNIDAD, 2 DE OCTUBRE, 25 DE ENERO, HONDURAS (LOS OLIVOS, COMAS)")

gen_cod_5 <- data.frame(P37_C_01_5, cod_5)
viajes_pro <- full_join(viajes_pro, gen_cod_5 ,by = "P37_C_01_5")%>% 
  distinct(trip_id, .keep_all = T)

names(viajes_pro)




#Limpiar y ordenar  #####
#Lipieza
viajes_clean <- viajes_pro[,-c(8,       #P31_01 Lugar de Origen
                 13,        #P34_01 Lugar de Destino
                 17,        #P36_01  Proposito
                 23,   # P37_1_1_01 Medio de Transporte 1
                 26,   # P37_2_1_01 Medio de Transporte 2
                 29,   # P37_3_1_01 Medio de Transporte 3
                 32,   # P37_4_1_01 Medio de Transporte 4
                 35,   # P37_5_1_01 Medio de Transporte 5
                 38,   # P37_6_1_01 Medio de Transporte 6
                 41,   # P37_7_1_01 Medio de Transporte 7
                 50,   # P37_C_01_1 Codigo 1
                 51,   # P37_C_01_2 Codigo 2
                 52,   # P37_C_01_3 Codigo 3
                 53,   # P37_C_01_4 Codigo 4
                 54,   # P37_C_01_5 Codigo 5
                 44,   # P37_T_01_1 Primer Transbordo
                 45,   # P37_T_01_2 Seundo Transborde
                 46,   # P37_T_01_3 Tercer Transbordp
                 47,   # P37_T_01_4 Cuarto Transbordo
                 48,   # P37_T_01_5 Quinto Transbordo
                 49)]  # P37_T_01_6Sexto Transbordo

colnames(viajes_clean)
View(viajes_clean)
#Ordenar 
orden <- c(1:7,
           39, 8:11,
           40,12:19,
           41,20,21,
           42,22,23,
           43,24,25,
           44,26,27,
           45,28,29,
           46,30,31,
           47,32,33,
           48,34,35,
           49:53)

vviajes_clean <- viajes_clean[,orden]
colnames(vviajes_clean)

View(vviajes_clean)


table(vviajes_clean$origen_lugar)/sum(table(vviajes_clean$origen_lugar))*100
table(vviajes_clean$destino_lugar)/sum(table(vviajes_clean$destino_lugar))*100
table(vviajes_clean$proposito)/sum(table(vviajes_clean$proposito))*100
table(vviajes_clean$Modalidad_1)/sum(table(vviajes_clean$Modalidad_1))*100

## Filtración de Datos ####
# Proposito Solo considerar factores que son indispensables en la para la superviviencia
# A Trabajar  B a estudiar C Por Trabajo D Volver a casa





# UNIFICAR BASE DE DATOS #### 

#person_pro con hogares_clean y dsps con viajes

unif_max <- full_join(person_pro, hogares_clean, by="hogar_id", copy = T) %>% 
  distinct(id, .keep_all = T)

unif_max$UBIGEO
View(unif_max)
colnames(unif_max)
nrow(unif_max)
unif_max <- full_join(vviajes_clean, unif_max, 
                      by=c("hogar_id","id_person"), copy = T) %>% 
  distinct(trip_id, .keep_all = T)
nrow(unif_max)
#Filtrar datos que son indispensables
unif_max <- unif_max[!is.na(unif_max$Modalidad_1),]
unif_max <- unif_max[!is.na(unif_max$origen_lugar),]
unif_max <- unif_max[!is.na(unif_max$destino_lugar),]
unif_max <- unif_max[!is.na(unif_max$TIME_VIAJES_2),]
unif_max <- unif_max[!is.na(unif_max$C_VIAJES),]
unif_max <- unif_max[!is.na(unif_max$proposito),]
unif_max <- unif_max[!is.na(unif_max$ZONA_TRANSITO),]
unif_max <- unif_max[!is.na(unif_max$DOMINIO),]
nrow(unif_max)

unif_max$UBIGEO <- as.character(unif_max$UBIGEO)
unif_max$ZONA_TRANSITO <- as.character(unif_max$ZONA_TRANSITO)

View(unif_max)
colnames(unif_max)
unif_max$UBIGEO
table(is.na(unif_max$UBIGEO))
#Llenar espacios vacios y corregir aquellos que son negativos
unif_max <- unif_max %>% mutate_all(~replace(., is.na(.), 0))
unif_max[unif_max < 0] <- 0

aas <- unif_max %>% mutate(1:n())

View(aas[,c("trip_id", "hogar_id", "id_person", "id","1:n()")])


# Modalidades que son relevantes 
#Filtrar Trailer Otros Ninguno Camion pequeno Camion Eliminar además los n.as

unif_max %>% unif_max %>% filter(Modalidad_1 != c("Camión_pequeno", "Camión",
                                                  "Ninguno", "Otros", "Tráiler"))
unif_max %>% unif_max %>% filter(Modalidad_2 != c("Camión_pequeno", "Camión",
                                                  "Ninguno", "Otros", "Tráiler"))
unif_max %>% unif_max %>% filter(Modalidad_3 != c("Camión_pequeno", "Camión",
                                                  "Ninguno", "Otros", "Tráiler"))
unif_max %>% unif_max %>% filter(Modalidad_4 != c("Camión_pequeno", "Camión",
                                                  "Ninguno", "Otros", "Tráiler"))
unif_max %>% unif_max %>% filter(Modalidad_5 != c("Camión_pequeno", "Camión",
                                                  "Ninguno", "Otros", "Tráiler"))
unif_max %>% unif_max %>% filter(Modalidad_6 != c("Camión_pequeno", "Camión",
                                                  "Ninguno", "Otros", "Tráiler"))
unif_max %>% unif_max %>% filter(Modalidad_7 != c("Camión_pequeno", "Camión",
                                                  "Ninguno", "Otros", "Tráiler"))


#Cost Time Summary ZT####
# Assumption se reparte
cost_time_rel <- unif_max %>% group_by(hogar_id, ZONA_TRANSITO) %>% 
  summarise(sum_viaj = sum(TIME_VIAJES_2), sum_viaje_pro = sum_viaj/TOTAL_VIAJES, 
            cost_viaje = 
              sum(C_VIAJES),cost_viaje_pro = cost_viaje/TOTAL_VIAJES) %>% 
  distinct(hogar_id, .keep_all = T) 
summary(cost_time_rel)
View(cost_time_rel)

# Travel Time and Costs per ZT
cost_time_rel_ZT <- cost_time_rel %>% group_by(ZONA_TRANSITO) %>% 
  summarise(sum_viaj = sum(sum_viaj)/n(), 
            cost_viaje = sum(cost_viaje)/n())

summary(cost_time_rel_ZT)

#install.packages("stargazer")
library(stargazer)
library(xtable)
xtable(summary(cost_time_rel_ZT),auto = TRUE)


View(cost_time_rel_ZT)   ### FINAL

cost_time_rel_ZT_sf <- full_join(cost_time_rel_ZT, 
                                 ZT_LIMA[,-c(8)],
                                 by = "ZONA_TRANSITO", copy = T)
cost_time_rel_ZT_sf <- 
  st_as_sf(cost_time_rel_ZT_sf, crs = 32721)

plot(cost_time_rel_ZT_sf)
nrow(cost_time_rel_ZT_sf)
View(cost_time_rel_ZT_sf)
colnames(cost_time_rel_ZT_sf)
table(is.na(cost_time_rel_ZT_sf))/sum(table(is.na(cost_time_rel_ZT_sf)))*100


#VIsualización####
#Costs
library(classInt)
breaks_qt1_Cost_ZT <- 
  classIntervals(c(min(cost_time_rel_ZT_sf$cost_viaje_ZT) - 
                     .00001, cost_time_rel_ZT_sf$cost_viaje_ZT), 
                 n = 9, style = "quantile")
breaks_qt1_Cost_ZT
cost_time_rel_ZT_graph_sf <- cost_time_rel_ZT_sf %>% 
  mutate(COST_CUT = cut(cost_viaje_ZT,breaks_qt1_Cost_ZT$brks))

cost_time_rel_ZT_graph_sf <- 
  st_as_sf(cost_time_rel_ZT_graph_sf, crs = 32721)
View(cost_time_rel_ZT_graph_sf)

ggplot( data = cost_time_rel_ZT_graph_sf) +geom_sf(aes(fill = COST_CUT)) + 
  scale_fill_brewer(palette = "OrRd") + 
  guides(fill = guide_legend(title = "Travel Costs Intervall")) +
  labs( title = "Microzones of AMLC classified by the Travel Cost Intervalls")

#Travel Times
breaks_qt1_trav_time_ZT <- 
  classIntervals(c(min(cost_time_rel_ZT_sf$sum_viaj_ZT) - 
                     .00001, cost_time_rel_ZT_sf$sum_viaj_ZT), 
                 n = 9, style = "quantile")
breaks_qt1_trav_time_ZT
cost_time_rel_ZT_graph_sf <- cost_time_rel_ZT_sf %>% 
  mutate(TRAVEL_CUT = cut(sum_viaj_ZT,breaks_qt1_trav_time_ZT$brks))

cost_time_rel_ZT_graph_sf <- 
  st_as_sf(cost_time_rel_ZT_graph_sf, crs = 32721)
View(cost_time_rel_ZT_graph_sf)

ggplot( data = cost_time_rel_ZT_graph_sf) +geom_sf(aes(fill = TRAVEL_CUT)) + 
  scale_fill_brewer(palette = "OrRd") + 
  guides(fill = guide_legend(title = "Travel Time Intervalls")) +
  labs( title = "Microzones of AMLC classified by the Travel Time Intervalls")

#Cost Time Summary UBIGEO####
# Assumption se reparte

cost_time_rel_by_district <- unif_max %>% group_by(hogar_id, UBIGEO) %>% 
  summarise(sum_viaj = sum(TIME_VIAJES_2), sum_viaje_pro = sum_viaj/TOTAL_VIAJES, 
            cost_viaje = 
              sum(C_VIAJES),cost_viaje_pro = cost_viaje/TOTAL_VIAJES) %>% 
  distinct(hogar_id, .keep_all = T)

summary(cost_time_rel_by_district)
View(cost_time_rel_by_district)
colnames(cost_time_rel_by_district)

# Travel Time and Costs per District
cost_time_rel_by_district <- cost_time_rel_by_district %>% group_by(UBIGEO) %>% 
  summarise(sum_viaj = sum(sum_viaj)/n(), cost_viaje = sum(cost_viaje)/n())

View(cost_time_rel_by_district)  ##FINAL
xtable(summary(cost_time_rel_by_district))

summary(cost_time_rel_by_district)
cost_time_rel_by_district_sf <- full_join(cost_time_rel_by_district, 
                                        maps_LM_sf[,-c(1:4, 7:10)],
                                        by = "UBIGEO", copy = T) %>% 
  distinct(UBIGEO, .keep_all = T)
cost_time_rel_by_district_sf <- 
  st_as_sf(cost_time_rel_by_district_sf, crs = 32721)


plot(cost_time_rel_by_district_sf)
nrow(cost_time_rel_by_district_sf)
View(cost_time_rel_by_district_sf)
colnames(cost_time_rel_by_district_sf)
table(is.na(cost_time_rel_by_district_sf))
#VIsualización####
#Costs
library(classInt)
breaks_qt1_Cost <- 
  classIntervals(c(min(cost_time_rel_by_district_sf$cost_viaje) - 
                     .00001, cost_time_rel_by_district_sf$cost_viaje), 
                 n = 9, style = "quantile")
breaks_qt1_Cost
cost_time_rel_by_district_graph_sf <- cost_time_rel_by_district_sf %>% 
  mutate(COST_CUT = cut(cost_viaje,breaks_qt1_Cost$brks))

cost_time_rel_by_district_graph_sf <- 
  st_as_sf(cost_time_rel_by_district_graph_sf, crs = 32721)
View(cost_time_rel_by_district_graph_sf)

ggplot( data = cost_time_rel_by_district_graph_sf) +geom_sf(aes(fill = COST_CUT)) + 
  scale_fill_brewer(palette = "OrRd") + 
  guides(fill = guide_legend(title = "Travel Costs Intervall")) +
  labs( title = "Districts of AMLC classified by the Travel Costs Intervals")

#Travel Times
breaks_qt1_trav_time <- 
  classIntervals(c(min(cost_time_rel_by_district_sf$sum_viaj) - 
                     .00001, cost_time_rel_by_district_sf$sum_viaj), 
                 n = 9, style = "quantile")
breaks_qt1_trav_time
cost_time_rel_by_district_graph_sf <- cost_time_rel_by_district_sf %>% 
  mutate(TRAVEL_CUT = cut(sum_viaj,breaks_qt1_trav_time$brks))

cost_time_rel_by_district_graph_sf <- 
  st_as_sf(cost_time_rel_by_district_graph_sf, crs = 32721)
View(cost_time_rel_by_district_graph_sf)

ggplot( data = cost_time_rel_by_district_graph_sf) +geom_sf(aes(fill = TRAVEL_CUT)) + 
  scale_fill_brewer(palette = "OrRd") + 
  guides(fill = guide_legend(title = "Travel Time Intervalls")) +
  labs( title = "Districts of AMLC classified by the Travel Times Intervals")



# División del Modal Split por UBIGEO####
modal_split_1 <- 
  unif_max %>% group_by(UBIGEO, Modalidad_1) %>% 
  summarise(n1 = n(), travel_cost1=sum(P37_1_3_01),
            travel_time1 = sum(P37_1_2_01))%>% filter(Modalidad_1 != 0)
colnames(modal_split_1)[colnames(modal_split_1) == "Modalidad_1"] <- "Modalidad"

modal_split_2 <- 
  unif_max %>% group_by(UBIGEO, Modalidad_2) %>% 
  summarise(n2 = n(), travel_cost2=sum(P37_2_3_01),
            travel_time2 = sum(P37_2_2_01)) %>% filter(Modalidad_2 != 0)
colnames(modal_split_2)[colnames(modal_split_2) == "Modalidad_2"] <- "Modalidad"

modal_split_3 <- 
  unif_max %>% group_by(UBIGEO, Modalidad_3) %>% 
  summarise(n3 = n(),travel_cost3=sum(P37_3_3_01),
            travel_time3 = sum(P37_3_2_01)) %>% filter(Modalidad_3 != 0)
colnames(modal_split_3)[colnames(modal_split_3) == "Modalidad_3"] <- "Modalidad"

modal_split_4 <- 
  unif_max %>% group_by(UBIGEO, Modalidad_4) %>% 
  summarise(n4 = n(),travel_cost4=sum(P37_4_3_01),
            travel_time4 = sum(P37_4_2_01)) %>% filter(Modalidad_4 != 0)
colnames(modal_split_4)[colnames(modal_split_4) == "Modalidad_4"] <- "Modalidad"

modal_split_5 <- 
  unif_max %>% group_by(UBIGEO, Modalidad_5) %>% 
  summarise(n5 = n(),travel_cost5=sum(P37_5_3_01),
            travel_time5 = sum(P37_5_2_01))%>% filter(Modalidad_5 != 0)
colnames(modal_split_5)[colnames(modal_split_5) == "Modalidad_5"] <- "Modalidad"

modal_split_6 <- 
  unif_max %>% group_by(UBIGEO, Modalidad_6) %>% 
  summarise(n6 = n(),travel_cost6=sum(P37_6_3_01),
            travel_time6 = sum(P37_6_2_01))%>% filter(Modalidad_6 != 0)
colnames(modal_split_6)[colnames(modal_split_6) == "Modalidad_6"] <- "Modalidad"

modal_split_7 <- 
  unif_max %>% group_by(UBIGEO, Modalidad_7) %>% 
  summarise(n7 = n(),travel_cost7=sum(P37_7_3_01),
            travel_time7 = sum(P37_7_2_01))%>% filter(Modalidad_7 != 0)
colnames(modal_split_7)[colnames(modal_split_7) == "Modalidad_7"] <- "Modalidad"



modal_split <- full_join(modal_split_1,modal_split_2, by = c("UBIGEO", "Modalidad"))
modal_split <- full_join(modal_split,modal_split_3, by = c("UBIGEO", "Modalidad"))
modal_split <- full_join(modal_split,modal_split_4, by = c("UBIGEO", "Modalidad"))
modal_split <- full_join(modal_split,modal_split_5, by = c("UBIGEO", "Modalidad"))
modal_split <- full_join(modal_split,modal_split_6, by = c("UBIGEO", "Modalidad"))
modal_split <- full_join(modal_split,modal_split_7, by = c("UBIGEO", "Modalidad"))
View(modal_split)
str(modal_split)

modal_split$n1 <- as.numeric(modal_split$n1)
modal_split$n2 <- as.numeric(modal_split$n2)
modal_split$n3 <- as.numeric(modal_split$n3)
modal_split$n4 <- as.numeric(modal_split$n4)
modal_split$n5 <- as.numeric(modal_split$n5)
modal_split$n6 <- as.numeric(modal_split$n6)
modal_split$n7 <- as.numeric(modal_split$n7)


modal_split <- mutate_at(modal_split, 
                         c("n1","travel_cost1","travel_time1",
                           "n2","travel_cost2","travel_time2",
                           "n3","travel_cost3","travel_time3",
                           "n4","travel_cost4","travel_time4",
                           "n5","travel_cost5","travel_time5",
                           "n6","travel_cost6","travel_time6",
                           "n7","travel_cost7","travel_time7"),
                         ~replace(., is.na(.), 0))

modal_split$n_t <- c(modal_split$n1 + modal_split$n2 +
                     modal_split$n3 + modal_split$n4 + 
                     modal_split$n5 + modal_split$n6 + 
                     modal_split$n7)
modal_split$total_travel_cost <- c(modal_split$travel_cost1+
                                     modal_split$travel_cost2 +
                                     modal_split$travel_cost3 + 
                                     modal_split$travel_cost4 + 
                                     modal_split$travel_cost5 + 
                                     modal_split$travel_cost6 + 
                                     modal_split$travel_cost7)

modal_split$total_travel_time <- c(modal_split$travel_time1+
                                     modal_split$travel_time2 +
                                     modal_split$travel_time3 + 
                                     modal_split$travel_time4 + 
                                     modal_split$travel_time5 + 
                                     modal_split$travel_time6 + 
                                     modal_split$travel_time7)

modal_split <- modal_split %>% 
  mutate(n_t_M = sum(n_t), number_Modalities = c(1 :n())) %>% 
  mutate(n_per = n_t/n_t_M*100, cost_per_travel = total_travel_cost/n_t, 
         traveltime_per_trave = total_travel_time/n_t)

#PRUEBA
modal_split%>%group_by(UBIGEO) %>% summarise(sum(n_per)) %>% View()

modal_split$UBIGEO <- as.character(modal_split$UBIGEO)

modal_split[which(modal_split =="70101"),]$UBIGEO <- "070101"
modal_split[which(modal_split =="70102"),]$UBIGEO <- "070102"
modal_split[which(modal_split =="70103"),]$UBIGEO <- "070103"
modal_split[which(modal_split =="70104"),]$UBIGEO <- "070104"
modal_split[which(modal_split =="70105"),]$UBIGEO <- "070105"
modal_split[which(modal_split =="70106"),]$UBIGEO <- "070106"
modal_split[which(modal_split =="70107"),]$UBIGEO <- "070107"

modal_split$id <- seq.int(nrow(modal_split))
View(modal_split)
nrow(modal_split)


modal_split_sf <- full_join(modal_split, maps_LM_sf[,-c(1:4, 7:10)],
                            by = c("UBIGEO"), copy =T) %>% 
  distinct(id, .keep_all = T)

View(modal_split_sf)
# Gini del Modal Split
library("ineq")
Gini_Mode <- modal_split_sf %>% group_by(DISTRITO) %>% 
  summarise(Gini_MODAL = ineq(n_t))
View(Gini_Mode)

modal_split_sf <- full_join(modal_split_sf, Gini_Mode, by = "DISTRITO", 
                               copy = T) %>% distinct(id, .keep_all = T)


View(modal_split_sf)
#PRUEBA
modal_split_sf%>%group_by(UBIGEO) %>% summarise(sum(n_per)) 
nrow(modal_split_sf)
colnames(modal_split_sf)
View(modal_split_sf)

modal_split_sf%>%group_by(UBIGEO) %>% summarise(sum(n_per)) %>% View()

modal_split_sf <- modal_split_sf[c("DISTRITO","UBIGEO","Modalidad","n_per","n_t",
                                         "n_t_M","Gini_MODAL","cost_per_travel",
                                         "traveltime_per_trave","id","geometry")]

# VISUALIZACION ####
library(classInt)
#Ejemplo ::::: Orientación
#breaks_qt1_GINI <- classIntervals(c(min(Inequality_pro_dis_sf$Gini) - .00001, Inequality_pro_dis_sf$Gini),
#                                  n = 9, style = "quantile")
#Inequality_pro_dis_sf <- Inequality_pro_dis_sf %>% 
#  mutate(GINI_CUT = cut(Gini,breaks_qt1_GINI$brks))

breaks_qt1_GINI_MODAL <- classIntervals(c(min(modal_split_sf$Gini_MODAL) - .00001, modal_split_sf$Gini_MODAL),
                                        n = 9, style = "quantile")
breaks_qt1_GINI_MODAL
modal_split_graph_ZT_sf <- modal_split_sf %>% 
  mutate(GINI_CUT = cut(Gini_MODAL,breaks_qt1_GINI_MODAL$brks))
modal_split_graph_ZT_sf <- st_as_sf(modal_split_graph_ZT_sf)

ggplot( data = modal_split_graph_ZT_sf) + geom_sf(aes(fill = GINI_CUT)) + 
  scale_fill_brewer(palette = "OrRd") + 
  guides(fill = guide_legend(title = "Modal Split's Heterogeneity")) +
  labs( title = "Districts of AMLC classified by the\nModal Split's Heterogeneity Intervalls")


# UNIFICACION TOTTAL DISTRITOS

General_table <- full_join(modal_split_sf, cost_time_rel_by_district, 
                              by = c("UBIGEO"), copy = T) %>%
  distinct(id, .keep_all = T)

str(General_table)
View(General_table)
colnames(General_table)

General_table%>%group_by(UBIGEO) %>% summarise(sum(n_per)) 


# División del Modal Split por ZT_TRANSITO####
modal_split_1_ZT <- 
  unif_max %>% group_by(ZONA_TRANSITO, Modalidad_1) %>% 
  summarise(n1 = n(), travel_cost1=sum(P37_1_3_01),
            travel_time1 = sum(P37_1_2_01))%>% filter(Modalidad_1 != 0)
colnames(modal_split_1_ZT)[colnames(modal_split_1_ZT) == "Modalidad_1"] <- "Modalidad"

modal_split_2_ZT <- 
  unif_max %>% group_by(ZONA_TRANSITO, Modalidad_2) %>% 
  summarise(n2 = n(), travel_cost2=sum(P37_2_3_01),
            travel_time2 = sum(P37_2_2_01)) %>% filter(Modalidad_2 != 0)
colnames(modal_split_2_ZT)[colnames(modal_split_2_ZT) == "Modalidad_2"] <- "Modalidad"

modal_split_3_ZT <- 
  unif_max %>% group_by(ZONA_TRANSITO, Modalidad_3) %>% 
  summarise(n3 = n(),travel_cost3=sum(P37_3_3_01),
            travel_time3 = sum(P37_3_2_01)) %>% filter(Modalidad_3 != 0)
colnames(modal_split_3_ZT)[colnames(modal_split_3_ZT) == "Modalidad_3"] <- "Modalidad"

modal_split_4_ZT <- 
  unif_max %>% group_by(ZONA_TRANSITO, Modalidad_4) %>% 
  summarise(n4 = n(),travel_cost4=sum(P37_4_3_01),
            travel_time4 = sum(P37_4_2_01)) %>% filter(Modalidad_4 != 0)
colnames(modal_split_4_ZT)[colnames(modal_split_4_ZT) == "Modalidad_4"] <- "Modalidad"

modal_split_5_ZT <- 
  unif_max %>% group_by(ZONA_TRANSITO, Modalidad_5) %>% 
  summarise(n5 = n(),travel_cost5=sum(P37_5_3_01),
            travel_time5 = sum(P37_5_2_01))%>% filter(Modalidad_5 != 0)
colnames(modal_split_5_ZT)[colnames(modal_split_5_ZT) == "Modalidad_5"] <- "Modalidad"

modal_split_6_ZT <- 
  unif_max %>% group_by(ZONA_TRANSITO, Modalidad_6) %>% 
  summarise(n6 = n(),travel_cost6=sum(P37_6_3_01),
            travel_time6 = sum(P37_6_2_01))%>% filter(Modalidad_6 != 0)
colnames(modal_split_6_ZT)[colnames(modal_split_6_ZT) == "Modalidad_6"] <- "Modalidad"

modal_split_7_ZT <- 
  unif_max %>% group_by(ZONA_TRANSITO, Modalidad_7) %>% 
  summarise(n7 = n(),travel_cost7=sum(P37_7_3_01),
            travel_time7 = sum(P37_7_2_01))%>% filter(Modalidad_7 != 0)
colnames(modal_split_7_ZT)[colnames(modal_split_7_ZT) == "Modalidad_7"] <- "Modalidad"


modal_split_ZT <- full_join(modal_split_1_ZT,modal_split_2_ZT, by = c("ZONA_TRANSITO", "Modalidad"))
modal_split_ZT <- full_join(modal_split_ZT,modal_split_3_ZT, by = c("ZONA_TRANSITO", "Modalidad"))
modal_split_ZT <- full_join(modal_split_ZT,modal_split_4_ZT, by = c("ZONA_TRANSITO", "Modalidad"))
modal_split_ZT <- full_join(modal_split_ZT,modal_split_5_ZT, by = c("ZONA_TRANSITO", "Modalidad"))
modal_split_ZT <- full_join(modal_split_ZT,modal_split_6_ZT, by = c("ZONA_TRANSITO", "Modalidad"))
modal_split_ZT <- full_join(modal_split_ZT,modal_split_7_ZT, by = c("ZONA_TRANSITO", "Modalidad"))
View(modal_split_ZT)
str(modal_split_ZT)

modal_split_ZT$n1 <- as.numeric(modal_split_ZT$n1)
modal_split_ZT$n2 <- as.numeric(modal_split_ZT$n2)
modal_split_ZT$n3 <- as.numeric(modal_split_ZT$n3)
modal_split_ZT$n4 <- as.numeric(modal_split_ZT$n4)
modal_split_ZT$n5 <- as.numeric(modal_split_ZT$n5)
modal_split_ZT$n6 <- as.numeric(modal_split_ZT$n6)
modal_split_ZT$n7 <- as.numeric(modal_split_ZT$n7)


modal_split_ZT <- mutate_at(modal_split_ZT, 
                         c("n1","travel_cost1","travel_time1",
                           "n2","travel_cost2","travel_time2",
                           "n3","travel_cost3","travel_time3",
                           "n4","travel_cost4","travel_time4",
                           "n5","travel_cost5","travel_time5",
                           "n6","travel_cost6","travel_time6",
                           "n7","travel_cost7","travel_time7"),
                         ~replace(., is.na(.), 0))

modal_split_ZT$n_t <- c(modal_split_ZT$n1 + modal_split_ZT$n2 +
                          modal_split_ZT$n3 + modal_split_ZT$n4 + 
                          modal_split_ZT$n5 + modal_split_ZT$n6 + 
                          modal_split_ZT$n7)
modal_split_ZT$total_travel_cost <- c(modal_split_ZT$travel_cost1+
                                        modal_split_ZT$travel_cost2 +
                                        modal_split_ZT$travel_cost3 + 
                                        modal_split_ZT$travel_cost4 + 
                                       modal_split_ZT$travel_cost5 + 
                                        modal_split_ZT$travel_cost6 + 
                                        modal_split_ZT$travel_cost7)

modal_split_ZT$total_travel_time <- c(modal_split_ZT$travel_time1+
                                        modal_split_ZT$travel_time2 +
                                        modal_split_ZT$travel_time3 + 
                                        modal_split_ZT$travel_time4 + 
                                        modal_split_ZT$travel_time5 + 
                                        modal_split_ZT$travel_time6 + 
                                        modal_split_ZT$travel_time7)

modal_split_ZT <- modal_split_ZT %>% 
  mutate(n_t_M = sum(n_t), number_Modalities = c(1 :n())) %>% 
  mutate(n_per = n_t/n_t_M*100, cost_per_travel = total_travel_cost/n_t, 
         traveltime_per_trave = total_travel_time/n_t)
nrow(modal_split_ZT)
modal_split_ZT$id <- seq.int(nrow(modal_split_ZT))
colnames(modal_split_ZT)
#PRUEBA
#modal_split_ZT%>%group_by(ZONA_TRANSITO) %>% summarise(sum(n_per)) %>% View()
# Gini del Modal Split
library("ineq")
Gini_Mode_ZT <- modal_split_ZT %>% group_by(ZONA_TRANSITO) %>% 
  summarise(Gini_MODAL = ineq(n_t))

modal_split_ZT <- full_join(modal_split_ZT, Gini_Mode_ZT, by = "ZONA_TRANSITO", 
                               copy = T) %>% distinct(id, .keep_all = T)
colnames(modal_split_ZT)

modal_split_ZT_sf <- full_join(modal_split_ZT, ZT_LIMA[,-c(8)],
                            by = c("ZONA_TRANSITO"), copy =T) %>%
  distinct(id, .keep_all = T) 
colnames(modal_split_ZT_sf)

View(modal_split_ZT_sf)

#PRUEBAS
#nrow(modal_split_ZT_sf)
colnames(modal_split_ZT_sf)
View(modal_split_ZT_sf)
#modal_split_ZT_sf%>%group_by(ZONA_TRANSITO) %>% summarise(sum(n_per)) %>% View()

modal_split_ZT_sf <- modal_split_ZT_sf[c("DISTRITO","ZONA_TRANSITO","Modalidad","n_per","n_t",
                                   "n_t_M","Gini_MODAL","cost_per_travel",
                                   "traveltime_per_trave","id","REGION","geometry")]



# VISUALIZACION ####
library(classInt)
#Ejemplo ::::: Orientación
#breaks_qt1_GINI <- classIntervals(c(min(Inequality_pro_dis_sf$Gini) - .00001, Inequality_pro_dis_sf$Gini),
#                                  n = 9, style = "quantile")

#Inequality_pro_dis_sf <- Inequality_pro_dis_sf %>% 
#  mutate(GINI_CUT = cut(Gini,breaks_qt1_GINI$brks))

breaks_qt1_GINI_MODAL <- classIntervals(c(min(modal_split_ZT_sf$Gini_MODAL) - .00001, modal_split_ZT_sf$Gini_MODAL),
                                        n = 9, style = "quantile")
breaks_qt1_GINI_MODAL
modal_split_graph_ZT_sf <- modal_split_ZT_sf %>% 
  mutate(GINI_CUT = cut(Gini_MODAL,breaks_qt1_GINI_MODAL$brks))
modal_split_graph_ZT_sf <- st_as_sf(modal_split_graph_ZT_sf)
ggplot( data = modal_split_graph_ZT_sf) +geom_sf(aes(fill = GINI_CUT)) + 
  scale_fill_brewer(palette = "OrRd") + 
  guides(fill = guide_legend(title = "Modal Split's Heterogeneity")) +
  labs( title = "Microzones of AMLC classified by the\nModal Split's Heterogeneity Intervalls")



###### UNIFICACION TOTAL ZT
General_table_ZT <- full_join(modal_split_ZT, cost_time_rel_ZT, 
                           by = c("ZONA_TRANSITO"), copy = T) %>%
  distinct(id, .keep_all = T)


View(General_table_ZT)
colnames(General_table_ZT)

General_table_ZT%>%group_by(ZONA_TRANSITO) %>% summarise(sum(n_per)) 
