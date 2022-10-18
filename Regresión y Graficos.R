library(ggplot2)
library(ggrepel)
library(ggcorrplot)
library(lubridate)
library(dplyr)
library(sf)
library(PerformanceAnalytics)
library(ggpubr)
### Regresion con distritos#####

regrx <- full_join(General_table, income_parameter_dist_sf,
                   by = c("DISTRITO", "UBIGEO", "geometry"), copy = T) %>% 
  distinct(id, .keep_all = T)
View(regrx)
nrow(regrx)
colnames(regrx)
regrx <- regrx[,c("DISTRITO","UBIGEO","Modalidad","n_per","cost_per_travel",
                  "traveltime_per_trave","n_t","Gini","Median_income",
                  "sum_viaj","cost_viaje","GINI_CONTRIBUTION","n_t_M",
                  "Gini_MODAL","clasi_gini","clasi_income","geometry")]

# Graph Summery
# Between districts
only_district <- regrx %>% distinct(UBIGEO, .keep_all = T)
colnames(only_district)


#View(only_district)
only_district$Gini_MODAL_qua <- (only_district$Gini_MODAL)^2
only_district$Travel_Budget <- only_district$cost_viaje/only_district$Median_income
only_district$Travel_Budget.qua <- (only_district$Travel_Budget)^2

#INDEX Graficas ordenadas por ####

modal_split_gini_vs_income <- only_district[,c(1,8,9,10,11,12,14,15,16, 19)]
colnames(modal_split_gini_vs_income)
nrow(modal_split_gini_vs_income)
modal_split_gini_vs_income <- 
  modal_split_gini_vs_income[!is.na(modal_split_gini_vs_income$Gini),]
modal_split_gini_vs_income <- 
  modal_split_gini_vs_income[!is.na(modal_split_gini_vs_income$Gini_MODAL),]
modal_split_gini_vs_income <- 
  modal_split_gini_vs_income[!is.na(modal_split_gini_vs_income$sum_viaj),]
modal_split_gini_vs_income <- 
  modal_split_gini_vs_income[!is.na(modal_split_gini_vs_income$cost_viaje),]
modal_split_gini_vs_income <-
  modal_split_gini_vs_income[!is.na(modal_split_gini_vs_income$Median_income),]

#Comprobar la Normalidad de la Distribution de las variables 0,01 Alpha
### GSM ###
shapiro.test(modal_split_gini_vs_income$Gini_MODAL)      
#P-Wert: 0.00716 Nicht-Normalverteilt
ggqqplot(modal_split_gini_vs_income$Gini_MODAL, ylab = "GMS")

shapiro.test(log(modal_split_gini_vs_income$Gini_MODAL)) 
#P-Wert: 0.0002779 Nicht-Normalverteilt
ggqqplot(log(modal_split_gini_vs_income$Gini_MODAL), ylab = "Log GMS")

### Travel Time  (TT) ###
shapiro.test(modal_split_gini_vs_income$sum_viaj)    
#P-Wert: 0.04555 Normalverteilt
ggqqplot(modal_split_gini_vs_income$sum_viaj, ylab = "Travel Time  (TT)")

shapiro.test(log(modal_split_gini_vs_income$sum_viaj))
#P-Wert: 0.0294 Normalverteilt
ggqqplot(log(modal_split_gini_vs_income$sum_viaj), ylab = "Log Travel Time  (TT)")

### Travel Cost (TC) ###
shapiro.test(modal_split_gini_vs_income$cost_viaje) 
#P-Wert: 3.042e-06 Nicht-Normalverteil
ggqqplot(modal_split_gini_vs_income$cost_viaje, ylab = "Travel Cost  (TC)")
shapiro.test(log(modal_split_gini_vs_income$cost_viaje))
#P-Wert: 0.009846 Nicht-Normalverteil
ggqqplot(log(modal_split_gini_vs_income$cost_viaje), ylab = "Log Travel Cost  (TC)")

### Gini Coefficient by District ###
shapiro.test(modal_split_gini_vs_income$Gini)

ggqqplot(modal_split_gini_vs_income$Gini, ylab = "Gini Coefficient")
#P-Wert: 0.05442 Normalverteilt 
shapiro.test(log(modal_split_gini_vs_income$Gini))
#P-Wert: 3.597e-05 Nicht-Normalverteilt 

### Median Income by District ###
shapiro.test(modal_split_gini_vs_income$Median_income)
#P-Wert: 2.342e-10 Nicht-Normalverteilt 
shapiro.test(log(modal_split_gini_vs_income$Median_income))
#P-Wert: 7.855e-07 Nicht-Normalverteilt

### Gini Contribution ###
shapiro.test(modal_split_gini_vs_income$GINI_CONTRIBUTION)
#P-Wert: 5.071e-12 Nicht-Normalverteilt
shapiro.test(log(modal_split_gini_vs_income$GINI_CONTRIBUTION))
#P-Wert: 0.03197 Normalverteilt


#PLOT DE CORRELACIONES PARA MEJOR ORIENTACION
library(corrplot)
corrplot.mixed(cor(ordered_by_income[,c(2,3,4,5,6,7)]), 
               use = "complete.obs",lower = 'shade', upper = 'pie', order = 'hclust')


#Ordered by Travel Budget
ordered_by_travel_budget <- 
  modal_split_gini_vs_income[order(modal_split_gini_vs_income$
                                     Gini_MODAL), ]
ordered_by_GMS$idu <- as.numeric(row.names(modal_split_gini_vs_income))


#Ordered by GMS
ordered_by_GMS <- 
  modal_split_gini_vs_income[order(modal_split_gini_vs_income$
                                     Gini_MODAL), ]
ordered_by_GMS$idu <- as.numeric(row.names(modal_split_gini_vs_income))

# Con Median Income
ggplot(ordered_by_GMS) + 
  geom_line(aes(x = idu,y=log(Median_income), col = clasi_income)) + 
  geom_point(aes(x = idu,y=log(Median_income))) +
  labs(y="Lod Gini Contribution",
       x="Districts ordered by GMS") + theme_classic()

cor.test(ordered_by_GMS$Gini_MODAL,
         ordered_by_GMS$Median_income, method = "pearson")
cor.test(ordered_by_GMS$Gini_MODAL,
         ordered_by_GMS$Median_income, method = "spearman")
cor.test(ordered_by_GMS$Gini_MODAL,
         ordered_by_GMS$Median_income, method = "kendall")

dat <- ordered_by_GMS%>% dplyr::select(Gini_MODAL, Median_income)
chart.Correlation(dat)   ### NO HAY CORRELACIÓN APRECIABLE

#Analysis por grupos sociales
ordered_by_GMS_1q <- ordered_by_GMS[which(ordered_by_GMS$clasi_income ==
                                                  "1. Quantil"),]
ordered_by_GMS_2q <- ordered_by_GMS[which(ordered_by_GMS$clasi_income ==
                                                  "2.Quantil"),]
ordered_by_GMS_3q <- ordered_by_GMS[which(ordered_by_GMS$clasi_income ==
                                                  "3.Quantil"),]
ordered_by_GMS_4q <- ordered_by_GMS[which(ordered_by_GMS$clasi_income ==
                                            "4.Quantil"),]

cor.test(ordered_by_GMS_1q$Gini_MODAL,ordered_by_GMS_1q$Median_income)
datq1 <- ordered_by_GMS_1q%>% dplyr::select(sum_viaj, Median_income)
chart.Correlation(datq1)   ### No hay Correlación

cor.test(ordered_by_GMS_2q$sum_viaj,ordered_by_GMS_2q$Median_income)
datq2 <- ordered_by_GMS_2q%>% dplyr::select(sum_viaj, Median_income)
chart.Correlation(datq2)   ### 0.26074 No significante

cor.test(ordered_by_GMS_3q$sum_viaj,ordered_by_GMS_3q$Median_income)
datq3 <- ordered_by_GMS_3q%>% dplyr::select(sum_viaj, Median_income)
chart.Correlation(datq3)   ### 0.26074 No significante

cor.test(ordered_by_GMS_4q$sum_viaj,ordered_by_GMS_4q$Median_income)
datq4 <- ordered_by_GMS_4q%>% dplyr::select(sum_viaj, Median_income)
chart.Correlation(datq4)   ### 0.26074 No significante


ggplot(ordered_by_GMS) + 
  geom_line(aes(x = idu,y= log(GINI_CONTRIBUTION), col = clasi_gini)) + 
  geom_point(aes(x = idu,y= log(GINI_CONTRIBUTION))) +
  labs(y="GINI CONTRIBUTION OF EACH DISTRICT",
       x="Districts ordered by GMS")+ theme_classic()
cor.test(only_district$Gini_MODAL, only_district$GINI_CONTRIBUTION)

ggplot(ordered_by_GMS) + 
  geom_line(aes(x = idu,y= log(Median_income),col = clasi_income)) + 
  geom_point(aes(x = idu,y= log(Median_income))) +
  labs(y="Median Income By District",
       x="Districts ordered by GMS")+ theme_classic()

ggplot(ordered_by_GMS) + 
  geom_line(aes(x = idu,y= log(Travel_Budget),col = clasi_income)) + 
  geom_point(aes(x = idu,y= log(Travel_Budget))) +
  labs(y="Median Income By District",
       x="Districts ordered by GMS")+ theme_classic()

#Ordered by Travel Budget
ordered_by_travel_budget <- 
  modal_split_gini_vs_income[order(modal_split_gini_vs_income$
                                     Travel_Budget), ]
ordered_by_travel_budget$idu <- as.numeric(row.names(modal_split_gini_vs_income))

# Con GMS
ggplot(ordered_by_travel_budget) + 
  geom_line(aes(x = idu,y=Gini_MODAL, col = clasi_income)) + 
  geom_point(aes(x = idu,y=Gini_MODAL)) +
  labs(y="GMS",
       x="Districts ordered by Travel Budget")+ theme_classic()

#Ordered by income
ordered_by_income <- 
  modal_split_gini_vs_income[order(modal_split_gini_vs_income$
                                     Median_income), ]
ordered_by_income$idu <- as.numeric(row.names(modal_split_gini_vs_income))

# Con GMS
ggplot(ordered_by_income) + 
  geom_line(aes(x = idu,y=Gini_MODAL, col = clasi_income)) + 
  geom_point(aes(x = idu,y=Gini_MODAL)) +
  labs(y="GMS",
       x="Districts ordered by Median Income")+ theme_classic()



cor.test(ordered_by_income$Gini_MODAL,
         ordered_by_income$Median_income, method = "pearson")
cor.test(ordered_by_income$Gini_MODAL,
         ordered_by_income$Median_income, method = "spearman")
cor.test(ordered_by_income$Gini_MODAL,
         ordered_by_income$Median_income, method = "kendall")

dat <- ordered_by_income%>% dplyr::select(Gini_MODAL, Median_income)
chart.Correlation(dat)   ### NO HAY CORRELACIÓN APRECIABLE

#Analysis por grupos sociales
ordered_by_income_1q <- ordered_by_income[which(ordered_by_income$clasi_income ==
                                                  "1. Quantil"),]
ordered_by_income_2q <- ordered_by_income[which(ordered_by_income$clasi_income ==
                                                  "2.Quantil"),]
ordered_by_income_3q <- ordered_by_income[which(ordered_by_income$clasi_income ==
                                                  "3.Quantil"),]
ordered_by_income_4q <- ordered_by_income[which(ordered_by_income$clasi_income ==
                                                  "4.Quantil"),]

cor.test(ordered_by_income_1q$Gini_MODAL,ordered_by_income_1q$Median_income)
datq1 <- ordered_by_income_1q%>% dplyr::select(sum_viaj, Median_income)
chart.Correlation(datq1)   ### No hay Correlación

cor.test(ordered_by_income_2q$sum_viaj,ordered_by_income_2q$Median_income)
datq2 <- ordered_by_income_2q%>% dplyr::select(sum_viaj, Median_income)
chart.Correlation(datq2)   ### 0.26074 No significante

cor.test(ordered_by_income_3q$sum_viaj,ordered_by_income_3q$Median_income)
datq3 <- ordered_by_income_3q%>% dplyr::select(sum_viaj, Median_income)
chart.Correlation(datq3)   ### 0.26074 No significante

cor.test(ordered_by_income_4q$sum_viaj,ordered_by_income_4q$Median_income)
datq4 <- ordered_by_income_4q%>% dplyr::select(sum_viaj, Median_income)
chart.Correlation(datq4)   ### 0.26074 No significante

# Travel Time
ggplot(ordered_by_income) + 
  geom_line(aes(x = idu,y=sum_viaj, col = clasi_income)) + 
  geom_point(aes(x = idu,y=sum_viaj)) +
  labs(y="Travel Time",
       x="Districts ordered by Median Income")+ theme_classic()

cor.test(ordered_by_income$sum_viaj, ordered_by_income$Median_income)
cor.test(ordered_by_income$sum_viaj,
         ordered_by_income$Median_income, method = "spearman")
cor.test(ordered_by_income$sum_viaj,
         ordered_by_income$Median_income, method = "kendall")


dat1 <- ordered_by_income%>% dplyr::select(sum_viaj, Median_income)
chart.Correlation(dat1)   ### Corelacion Negativa -0,33 *

#Analysis por grupos sociales
ordered_by_income_1q <- ordered_by_income[which(ordered_by_income$clasi_income ==
                                                  "1. Quantil"),]
ordered_by_income_2q <- ordered_by_income[which(ordered_by_income$clasi_income ==
                                                  "2.Quantil"),]
ordered_by_income_3q <- ordered_by_income[which(ordered_by_income$clasi_income ==
                                                  "3.Quantil"),]
ordered_by_income_4q <- ordered_by_income[which(ordered_by_income$clasi_income ==
                                                  "4.Quantil"),]

cor.test(ordered_by_income_1q$sum_viaj,ordered_by_income_1q$Median_income)
dat1q1 <- ordered_by_income_1q%>% dplyr::select(sum_viaj, Median_income)
chart.Correlation(dat1q1)   ### No hay Correlación

cor.test(ordered_by_income_2q$sum_viaj,ordered_by_income_2q$Median_income)
dat1q2 <- ordered_by_income_2q%>% dplyr::select(sum_viaj, Median_income)
chart.Correlation(dat1q2)   ### 0.26074 No significante

cor.test(ordered_by_income_3q$sum_viaj,ordered_by_income_3q$Median_income)
dat1q3 <- ordered_by_income_3q%>% dplyr::select(sum_viaj, Median_income)
chart.Correlation(dat1q3)   ### 0.26074 No significante

cor.test(ordered_by_income_4q$sum_viaj,ordered_by_income_4q$Median_income)
dat1q4 <- ordered_by_income_4q%>% dplyr::select(sum_viaj, Median_income)
chart.Correlation(dat1q4) 

#### DATOS DE INGRESO POR DISTRITOS NO MUY PRECISOS::: ANALIZAR CON MICROZONAS

#Travel Cost
ggplot(ordered_by_income) + 
  geom_line(aes(x = idu,y=log(cost_viaje/Median_income), col = clasi_income)) + 
  geom_point(aes(x = idu,y=log(cost_viaje/Median_income))) +
  labs(y="LOG Ratio between Travel Cost and Median Income by District",
       x="Districts ordered by Median Income")+ theme_classic()

cor.test(ordered_by_income$cost_viaje, 
         log(ordered_by_income$Median_income))
cor.test(ordered_by_income$cost_viaje,
         ordered_by_income$Median_income, method = "spearman")
cor.test(ordered_by_income$cost_viaje,
         ordered_by_income$Median_income, method = "kendall")

dat2 <- ordered_by_income%>% select(cost_viaje, Median_income)
chart.Correlation(dat2)  ### Corelacion positiva 0,46** 

#Analysis por grupos sociales
ordered_by_income_1q <- ordered_by_income[which(ordered_by_income$clasi_income ==
                                                  "1. Quartil"),]
ordered_by_income_2q <- ordered_by_income[which(ordered_by_income$clasi_income ==
                                                  "2.Quartil"),]
ordered_by_income_3q <- ordered_by_income[which(ordered_by_income$clasi_income ==
                                                  "3.Quantil"),]

cor.test(log(ordered_by_income_1q$cost_viaje),
         ordered_by_income_1q$Median_income)
dat2q1 <- ordered_by_income_1q%>% select(cost_viaje, Median_income)
chart.Correlation(dat2q1)   ### No hay Correlación

cor.test(ordered_by_income_2q$cost_viaje,
         ordered_by_income_2q$Median_income)
dat2q2 <- ordered_by_income_2q%>% select(cost_viaje, Median_income)
chart.Correlation(dat2q2)  

cor.test(log(ordered_by_income_3q$cost_viaje),
         ordered_by_income_3q$Median_income)
dat3q3 <- ordered_by_income_3q%>% select(cost_viaje, Median_income)
chart.Correlation(dat3q3)   

#Travel Cost/Y
ggplot(ordered_by_income) + 
  geom_line(aes(x = idu,y=cost_viaje/Median_income, col = clasi_income)) + 
  geom_point(aes(x = idu,y=cost_viaje/Median_income)) +
  labs(y="Travel Cost by Travel",
       x="Districts ordered by Median Income")+ theme_classic()

#### DATOS DE INGRESO POR DISTRITOS NO MUY PRECISOS::: ANALIZAR CON MICROZONAS

#Gini Contribution
ggplot(ordered_by_income) + 
  geom_line(aes(x = idu,y=log(GINI_CONTRIBUTION), col = clasi_income)) + 
  geom_point(aes(x = idu,y=log(GINI_CONTRIBUTION))) +
  labs( y="Log Gini Contribution of each district to Gini Total",
        x="Districts ordered by Median Income")+ theme_classic()

cor.test(ordered_by_income$Median_income, 
         log(ordered_by_income$GINI_CONTRIBUTION))
cor.test(ordered_by_income$cost_viaje,
         log(ordered_by_income$GINI_CONTRIBUTION), method = "kendall")

dat3 <- ordered_by_income%>% dplyr::select(GINI_CONTRIBUTION, Median_income)
dat3$GINI_CONTRIBUTION <- log(dat3$GINI_CONTRIBUTION)
chart.Correlation(dat3)

#Gini
ggplot(ordered_by_income) + 
  geom_line(aes(x = idu,y=Gini, col = clasi_income)) + 
  geom_point(aes(x = idu,y=Gini)) +
  labs( y="Gini by district",
        x="Districts ordered by Median Income")+ theme_classic()

cor.test(ordered_by_income$Median_income, 
         ordered_by_income$Gini)
cor.test(ordered_by_income$Gini,
         ordered_by_income$Median_income, method = "kendall")

dat7 <- ordered_by_income%>% dplyr::select(Gini, Median_income)
chart.Correlation(dat7)

#ordered by Travel Time
ordered_by_TT <- 
  modal_split_gini_vs_income[order(modal_split_gini_vs_income$sum_viaj), ]
ordered_by_TT$idu <- as.numeric(row.names(ordered_by_TT))


# Travel Time
ggplot(ordered_by_TT) + 
  geom_line(aes(x = idu,y=log(Median_income), col = clasi_income)) +
  geom_point(aes(x = idu,y=log(Median_income))) +
  labs(y="Log Median Income",
       x="Districts ordered by Travel Time")+ theme_classic()


#ordered by Modalsplit Heterogeneity
ordered_by_Modalsplit_Heterogeneity <- 
  modal_split_gini_vs_income[order(modal_split_gini_vs_income$Gini_MODAL), ]
ordered_by_Modalsplit_Heterogeneity$idu <- as.numeric(row.names(ordered_by_Modalsplit_Heterogeneity))


# Travel Time
ggplot(ordered_by_Modalsplit_Heterogeneity) + 
  geom_line(aes(x = idu,y=(sum_viaj), col = clasi_income)) +
  geom_point(aes(x = idu,y=sum_viaj)) +
  labs(y="Travel Time",
       x="Districts ordered by Modalsplit Heterogeneity")+ theme_classic()
cor.test(ordered_by_Modalsplit_Heterogeneity$sum_viaj, 
         ordered_by_Modalsplit_Heterogeneity$Gini_MODAL)
cor.test(ordered_by_Modalsplit_Heterogeneity$sum_viaj,
         ordered_by_Modalsplit_Heterogeneity$Gini_MODAL, method = "kendall")

dat4 <- ordered_by_Modalsplit_Heterogeneity%>% 
  dplyr::select(sum_viaj, Gini_MODAL)
chart.Correlation(dat4)

#Analysis por grupos sociales
ordered_by_Modalsplit_Heterogeneity_1q <- 
  ordered_by_Modalsplit_Heterogeneity[which(ordered_by_Modalsplit_Heterogeneity$clasi_income ==
                                              "1. Quantil"),]
ordered_by_Modalsplit_Heterogeneity_2q <- 
  ordered_by_Modalsplit_Heterogeneity[which(ordered_by_Modalsplit_Heterogeneity$clasi_income ==
                                              "2.Quantil"),]
ordered_by_Modalsplit_Heterogeneity_3q <- 
  ordered_by_Modalsplit_Heterogeneity[which(ordered_by_Modalsplit_Heterogeneity$clasi_income ==
                                                  "3.Quantil"),]
ordered_by_Modalsplit_Heterogeneity_4q <- 
  ordered_by_Modalsplit_Heterogeneity[which(ordered_by_Modalsplit_Heterogeneity$clasi_income ==
                                              "4.Quantil"),]

cor.test(ordered_by_Modalsplit_Heterogeneity_1q$sum_viaj,
         ordered_by_Modalsplit_Heterogeneity_1q$Gini_MODAL)
dat4q1 <- ordered_by_Modalsplit_Heterogeneity_1q%>%  dplyr::select(sum_viaj, Gini_MODAL)
chart.Correlation(dat4q1) 

cor.test(ordered_by_Modalsplit_Heterogeneity_2q$sum_viaj,
         ordered_by_Modalsplit_Heterogeneity_2q$Gini_MODAL)
dat4q2 <- ordered_by_Modalsplit_Heterogeneity_2q%>%  dplyr::select(sum_viaj, Gini_MODAL)
chart.Correlation(dat4q2)  

cor.test(ordered_by_Modalsplit_Heterogeneity_3q$sum_viaj,
         ordered_by_Modalsplit_Heterogeneity_3q$Gini_MODAL)
dat4q3 <- ordered_by_Modalsplit_Heterogeneity_3q%>%  dplyr::select(sum_viaj, Gini_MODAL)
chart.Correlation(dat4q3)

cor.test(ordered_by_Modalsplit_Heterogeneity_4q$sum_viaj,
         ordered_by_Modalsplit_Heterogeneity_4q$Gini_MODAL)
dat4q4 <- ordered_by_Modalsplit_Heterogeneity_4q%>%  dplyr::select(sum_viaj, Gini_MODAL)
chart.Correlation(dat4q4)

#### DATOS DE INGRESO POR DISTRITOS NO MUY PRECISOS::: ANALIZAR CON MICROZONAS


# Travel Cost/y
ggplot(ordered_by_Modalsplit_Heterogeneity) + 
  geom_line(aes(x = idu,y=log(cost_viaje/Median_income), col = clasi_income)) + 
  geom_point(aes(x = idu,y=log(cost_viaje/Median_income))) +
  labs(y="Log Travel Cost divided by Median Income",
       x="Districts ordered by Modalsplit Heterogeneity")+ theme_classic()

cor.test(ordered_by_Modalsplit_Heterogeneity$cost_viaje, 
         ordered_by_Modalsplit_Heterogeneity$Gini_MODAL)
cor.test(ordered_by_Modalsplit_Heterogeneity$cost_viaje,
         ordered_by_Modalsplit_Heterogeneity$Gini_MODAL, method = "kendall")
cor.test(ordered_by_Modalsplit_Heterogeneity$cost_viaje,
         ordered_by_Modalsplit_Heterogeneity$Gini_MODAL, method = "spearman")

dat5 <- ordered_by_Modalsplit_Heterogeneity%>% 
  select(cost_viaje, Gini_MODAL)
chart.Correlation(dat5)

#Analysis por grupos sociales
ordered_by_Modalsplit_Heterogeneity_1q <- 
  ordered_by_Modalsplit_Heterogeneity[which(ordered_by_Modalsplit_Heterogeneity$clasi_income ==
                                              "1. Quartil"),]
ordered_by_Modalsplit_Heterogeneity_2q <- 
  ordered_by_Modalsplit_Heterogeneity[which(ordered_by_Modalsplit_Heterogeneity$clasi_income ==
                                              "2.Quartil"),]
ordered_by_Modalsplit_Heterogeneity_3q <- 
  ordered_by_Modalsplit_Heterogeneity[which(ordered_by_Modalsplit_Heterogeneity$clasi_income ==
                                              "3.Quantil"),]

cor.test(ordered_by_Modalsplit_Heterogeneity_1q$cost_viaje,
         ordered_by_Modalsplit_Heterogeneity_1q$Gini_MODAL)
dat5q1 <- ordered_by_Modalsplit_Heterogeneity_1q%>% select(cost_viaje, Gini_MODAL)
chart.Correlation(dat5q1) 

cor.test(ordered_by_Modalsplit_Heterogeneity_2q$cost_viaje,
         ordered_by_Modalsplit_Heterogeneity_2q$Gini_MODAL)
dat5q2 <- ordered_by_Modalsplit_Heterogeneity_2q%>% select(cost_viaje, Gini_MODAL)
chart.Correlation(dat5q2) 

cor.test(ordered_by_Modalsplit_Heterogeneity_3q$cost_viaje,
         ordered_by_Modalsplit_Heterogeneity_3q$Gini_MODAL)
dat5q3 <- ordered_by_Modalsplit_Heterogeneity_3q%>% select(cost_viaje, Gini_MODAL)
chart.Correlation(dat5q3)

# Travel Cost
ggplot(ordered_by_Modalsplit_Heterogeneity) + 
  geom_line(aes(x = idu,y=log(cost_viaje), col = clasi_income)) + 
  geom_point(aes(x = idu,y=log(cost_viaje))) +
  labs(y="Log Travel_Cost/Median Income by Travel",
       x="Districts ordered by Modalsplit Heterogeneity")+ theme_classic()

#### DATOS DE INGRESO POR DISTRITOS NO MUY PRECISOS::: ANALIZAR CON MICROZONAS

# Gini Contribution
ggplot(ordered_by_Modalsplit_Heterogeneity) + 
  geom_line(aes(x = idu,y=log(GINI_CONTRIBUTION), col = clasi_income)) + 
  geom_point(aes(x = idu,y=log(GINI_CONTRIBUTION))) +
  labs(y="Log Gini Contribution to Gini within",
       x="Districts ordered by Modalsplit Heterogeneity") +theme_classic()

cor.test(log(ordered_by_Modalsplit_Heterogeneity$GINI_CONTRIBUTION), 
         ordered_by_Modalsplit_Heterogeneity$Gini_MODAL)
cor.test(log(ordered_by_Modalsplit_Heterogeneity$GINI_CONTRIBUTION),
         ordered_by_Modalsplit_Heterogeneity$Gini_MODAL, method = "kendall")

dat6 <- ordered_by_Modalsplit_Heterogeneity%>% 
  dplyr::select(GINI_CONTRIBUTION, Gini_MODAL)
dat6$GINI_CONTRIBUTION <- log(dat6$GINI_CONTRIBUTION)
chart.Correlation(dat6)


#Analysis por grupos sociales
ordered_by_Modalsplit_Heterogeneity_1q <- 
  ordered_by_Modalsplit_Heterogeneity[which(ordered_by_Modalsplit_Heterogeneity$clasi_income ==
                                              "1. Quartil"),]
ordered_by_Modalsplit_Heterogeneity_2q <- 
  ordered_by_Modalsplit_Heterogeneity[which(ordered_by_Modalsplit_Heterogeneity$clasi_income ==
                                              "2.Quartil"),]
ordered_by_Modalsplit_Heterogeneity_3q <- 
  ordered_by_Modalsplit_Heterogeneity[which(ordered_by_Modalsplit_Heterogeneity$clasi_income ==
                                              "3.Quantil"),]

cor.test(log(ordered_by_Modalsplit_Heterogeneity_1q$GINI_CONTRIBUTION),
         ordered_by_Modalsplit_Heterogeneity_1q$Gini_MODAL)
dat5q1 <- ordered_by_Modalsplit_Heterogeneity_1q%>% 
  dplyr::select(GINI_CONTRIBUTION, Gini_MODAL)
dat5q1$GINI_CONTRIBUTION <- log(dat5q1$GINI_CONTRIBUTION)
chart.Correlation(dat5q1) 

cor.test(log(ordered_by_Modalsplit_Heterogeneity_2q$GINI_CONTRIBUTION),
         ordered_by_Modalsplit_Heterogeneity_2q$Gini_MODAL)
dat5q2 <- ordered_by_Modalsplit_Heterogeneity_2q%>% 
  dplyr::select(GINI_CONTRIBUTION, Gini_MODAL)
dat5q2$GINI_CONTRIBUTION <- log(dat5q2$GINI_CONTRIBUTION)
chart.Correlation(dat5q2) 

cor.test(log(ordered_by_Modalsplit_Heterogeneity_3q$GINI_CONTRIBUTION),
         ordered_by_Modalsplit_Heterogeneity_3q$Gini_MODAL)
dat5q3 <- ordered_by_Modalsplit_Heterogeneity_3q%>% 
  dplyr::select(GINI_CONTRIBUTION, Gini_MODAL)
dat5q3$GINI_CONTRIBUTION <- log(dat5q3$GINI_CONTRIBUTION)
chart.Correlation(dat5q3) 

# Gini
ggplot(ordered_by_Modalsplit_Heterogeneity) + 
  geom_line(aes(x = idu,y=Gini, col = clasi_income)) + 
  geom_point(aes(x = idu,y=Gini)) +
  labs(y="Gini Coefficient by District",
       x="Districts ordered by Modalsplit Heterogeneity") +theme_classic()





cor.test(ordered_by_Modalsplit_Heterogeneity$Gini, 
         ordered_by_Modalsplit_Heterogeneity$Gini_MODAL)
dat8 <- ordered_by_Modalsplit_Heterogeneity%>% 
  select(Gini, Gini_MODAL)
chart.Correlation(dat8)

#RELATIONSHIP BETWEEN GMS \nAND Median Income no significante :( ####
only_district %>% ggplot(mapping = aes(x = log(Median_income),
                                    y = Gini_MODAL, color=clasi_income)) +
  geom_smooth(method = "lm" ,se = F) + 
  geom_point() +theme_classic() + geom_label_repel(aes(label = DISTRITO), 
                                                   box.padding = 
                                                    0.35,point.padding = 0.5,
                                                   segment.color = 'grey50') +
  labs(title = "Relationship Between Median Income\nand GMS by districts",
       x = "Log Median Income of each District",
       y = "GMS")

cor.test(only_district$Gini_MODAL, only_district$Gini)


# Statistical Analysis
lm_Gini_vs_Gini_Modal <- lm(Gini ~ Gini_MODAL*Median_incom, 
                            data = only_district)

summary(lm_Gini_vs_Gini_Modal)

lm_Gini_vs_Gini_Modal_by_income <- lm(Gini ~ Gini_MODAL, 
                            data = only_district)
anova(lm_Gini_vs_Gini_Modal_by_income)
summary(lm_Gini_vs_Gini_Modal_by_income)

## PUEDE QUE HAYA UNA CORRELATION NEGATIVA ENTRE EL GMS Y EL 
# GINI COEFFICIENT, SIN EMBARGO NO ES SIGNIFICANTE
# INTERPRETATION::: EN GENERAL UN AUMENTO DE LA VARIEDAD DE LOS MEDIOS DE
#TRANSPORTE ESTARÍA RELACIONADO CON UNA DISMINUCIÓN DE LA DISPARIDAD DE INGRESOS
# Analysis según ingresos

# Considering the division of the Gini Coefficient into Quantiles 
ggplot(only_district, mapping = aes(x = Gini_MODAL,
                                    y = Gini, color=clasi_income)) +
  geom_smooth(method = "lm" ,se = F) + 
  geom_point() +theme_classic() +
  labs(title = "RELATIONSHIP BETWEEN GMS\nAND GINI COEFFICIENT,\nCONSIDERING A PARTITION OF THE MEDIAN INCOME INTO QUANTILES",
       x ="GMS",
       y = "GINI COEFFICIENT")



#Relationship between Gini Contribution \nand Modal Split heterogeneity Index ####
ggplot(only_district, mapping = aes(x = only_district$Gini_MODAL, 
                                   y = log(only_district$GINI_CONTRIBUTION),
                                   color=clasi_income)) +
  geom_point() + theme_classic() +
  geom_smooth(method = "lm", se = 0) + 
  geom_label_repel(aes(label = DISTRITO), box.padding = 
                     0.35,point.padding = 0.5,segment.color = 'grey50') +
  labs(title = "Relationship between Gini Contribution \nand Modal Split 
       heterogeneity Index",
       x = "LOG GINI CONTRIBUTION BY DISTRICT", 
       y = "MODAL SPLIT HETEROGENEITY INDEX")

#Statistal Analysis
summary(lm(log(GINI_CONTRIBUTION) ~ Gini_MODAL, 
           data = only_district))

summary(lm((Gini) ~ poly(Median_income,2), 
           data = only_ZT))

summary(lm(log(GINI_CONTRIBUTION) ~ Gini_MODAL*Median_income, 
           data = only_district))

summary(lm(Gini_MODAL ~ log(GINI_CONTRIBUTION) + Gini + Median_income, 
           data = only_district))

only_district1 <- 
  only_district[!is.na(only_district$Gini),]

summary(lm(Gini_MODAL ~  poly(Gini,2), 
           data = only_district1))


only_ZT1 <- 
  only_ZT[!is.na(only_ZT$Gini),]

summary(lm(Gini_MODAL ~  poly(Gini,2), 
           data = only_ZT1))

plot(only_district$Gini_MODAL, only_district$Gini)

##### Effectos del coste y tiempo de viaje promedio por distrito al median income y Gini#####
lm1 <- lm(log(Median_income) ~ log(cost_viaje) + log(sum_viaj) , data = only_district)
summary(lm1)


lm2 <- lm(Gini_MODAL  ~ log(sum_viaj), data = only_district)
summary(lm2)  # Se puede LOG tmb

lm3 <- lm(Gini_MODAL ~ log(cost_viaje), data = only_district)
summary(lm3)  # No hy relation

lm4 <- lm(Gini_MODAL ~ log(Median_income), data = only_district)
summary(lm4)# No hy relation

## Interactiones
lm5 <- lm((Gini_MODAL)  ~ log(sum_viaj) + 
            (cost_viaje)/(Median_income), data = only_district)
summary(lm5)

lm6 <- lm((Gini_MODAL)  ~ log(cost_viaje)/log(sum_viaj), 
          data = only_district)
summary(lm6)

lm12 <- lm(log(Gini_MODAL)  ~ log(cost_viaje)/log(Median_income), 
          data = only_district)
summary(lm12) # No hy relation

lm7 <- lm(Gini_MODAL  ~ log(cost_viaje)*log(sum_viaj), 
          data = only_district)
summary(lm7)  #  hy relation pero no significante

lm8 <- lm(Gini_MODAL  ~ log(cost_viaje)/log(Median_income), 
          data = only_district)
summary(lm8)# No hy relation

lm9 <- lm(Gini_MODAL  ~ log(cost_viaje)*log(Median_income), 
          data = only_district)
summary(lm9)# No hy relation

lm10 <- lm(Gini_MODAL  ~ log(sum_viaj) + log(cost_viaje)*log(Median_income), 
          data = only_district)
summary(lm10)
anova(lm10)

lm20 <- lm(Gini_MODAL  ~ Travel_Budget.qua + Travel_Budget, 
           data = only_district)
summary(lm20)

##### Effecto del Gini, Gini_Modal y log(Median Income District) al Uso en Porcentaje del una modalidad#####
colnames(regrx)

ggplot(regrx, mapping = aes(x = log(GINI_CONTRIBUTION), y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + facet_wrap(~Modalidad, scales="free_x")+
  labs(title = "RELATIONSHIP BETWEEN GINI CONTRIBUTION \nAND MODAL SHARE",
       x = "GINI CONTRIBUTION", y = "LOG %MODAL SHARE")

ggplot(regrx, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + facet_wrap(~Modalidad, scales="free_x")+
  labs(title = "RELATIONSHIP BETWEEN GINI COEFFICIENT \nAND MODAL SHARE",
       x = "GINI COEFFICIENT BY DISTRICT", y = "LOG %MODAL SHARE")

ggplot(regrx, mapping = aes(x = Gini_MODAL, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + facet_wrap(~Modalidad, scales="free_x")+
  labs(title = "RELATIONSHIP BETWEEN MODAL SPLIT HETEROGENEITY INDEX \nAND MODAL SHARE",
       x = "MODAL SPLIT HETEROGENEITY INDEX", y = "LOG %MODAL SHARE")

ggplot(regrx, mapping = aes(x = log(Median_income), y = log(n_per))) +
  geom_smooth(method = "lm" ,se = T) + facet_wrap(~Modalidad, scales="free_x")+
  geom_point(aes(color = Modalidad), size= 1) + theme_classic() +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND MODAL SHARE",
       x = "LOG MEDIAN INCOME BY DISTRICT", y = "LOG %MODAL SHARE")

## Dividir por Modalidad ###
table(regrx$Modalidad)
# Graph Summary 
# Between Modalities


# Auto_Particular#####
rgr_auto_particular <- regrx[which(regrx$Modalidad == "Auto_Particular"),] 

#GINI
auto_regr <-  lm(log(n_per) ~ Gini, data = rgr_auto_particular)
summary(auto_regr)
plot(rgr_auto_particular$Gini, log(rgr_auto_particular$n_per), 
     xlab = "GINI COEFFICIENT BY DISTRICTS", 
     ylab = "MODAL SPLIT LOG %AUTO")
abline(auto_regr)

ggplot(rgr_auto_particular, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = 0) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 'grey50')

#Median Income
auto_regr_med <-  lm(log(n_per) ~ log(Median_income), data = rgr_auto_particular)
summary(auto_regr_med) #SEHR SIGNIFICANT

plot(log(rgr_auto_particular$Median_income), log(rgr_auto_particular$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %AUTO")
abline(auto_regr_med)

ggplot(rgr_auto_particular, 
       mapping = aes(x = log(rgr_auto_particular$Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_text(aes(label=DISTRITO),
                                                 hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE AUTO",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE AUTO") 

# Bicicleta#####
rgr_Bicicleta <- regrx[which(regrx$Modalidad == "Bicicleta"),] 

#GINI
bici_regr <-  lm(log(n_per) ~ Gini, data = rgr_Bicicleta, na.action("na.omit"))
summary(bici_regr)   #NICHT SIGNIFIKANT


plot(rgr_Bicicleta$Gini, log(rgr_Bicicleta$n_per), 
     xlab = "GINI COEFFICIENT BY DISTRICTS", 
     ylab = "MODAL SPLIT LOG %BICICLE")
abline(bici_regr)

ggplot(rgr_Bicicleta, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 'grey50')

#Median Income
bici_regr_med <-  lm(log(n_per) ~ log(Median_income), data = rgr_Bicicleta)
summary(bici_regr_med)

plot(log(rgr_Bicicleta$Median_income), log(rgr_Bicicleta$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %BICICLE")
abline(bici_regr_med)

ggplot(rgr_Bicicleta, 
       mapping = aes(x = log(rgr_Bicicleta$Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_text(aes(label=DISTRITO),
                                                 hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE BICICLE",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE BICICLE") 

# Caminando####
rgr_Caminando <- regrx[which(regrx$Modalidad == "Caminando"),] 

#GINI
walk_regr <-  lm(log(n_per) ~ Gini, data = rgr_Caminando)
summary(walk_regr)


plot(rgr_Caminando$Gini, log(rgr_Caminando$n_per), 
     xlab = "GINI COEFFICIENT BY DISTRICTS", 
     ylab = "MODAL SPLIT LOG %WALK")
abline(walk_regr)

ggplot(rgr_Caminando, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 'grey50')


#Median Income
rgr_Caminando_med <-  lm(log(n_per) ~ log(Median_income), data = rgr_Caminando,
                         na.action("na.omit"))
summary(rgr_Caminando_med) #SIGNIFICNAT

plot(log(rgr_Caminando$Median_income), log(rgr_Caminando$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %BICICLE")
abline(rgr_Caminando_med)

ggplot(rgr_Caminando, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_text(aes(label=DISTRITO),
                                                 hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE BICICLE",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE WALK") 

# Camión#####
rgr_Camion <- regrx[which(regrx$Modalidad == "Camión"),] 

#GINI
track_regr <-  lm(log(n_per) ~ Gini, data = rgr_Camion,
                  na.action("na.omit"))
summary(track_regr)


plot(rgr_Camion$Gini, log(rgr_Camion$n_per), 
     xlab = "GINI COEFFICIENT BY DISTRICTS", 
     ylab = "MODAL SPLIT LOG %TRACKS")
abline(track_regr)

ggplot(rgr_Camion, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 'grey50')

#Median Income
rgr_track_med <-  lm(log(n_per) ~ log(Median_income), data = rgr_Camion,
                     na.action("na.omit"))
summary(rgr_track_med)

plot(log(rgr_Camion$Median_income), log(rgr_Camion$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %BICICLE")
abline(rgr_track_med)

ggplot(rgr_Camion, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_text(aes(label=DISTRITO),
                                                 hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE BICICLE",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE TRACK") 

# Camión_pequeno#####
rgr_Camion_pe <- regrx[which(regrx$Modalidad == "Camión_pequeno"),]

#GINI
track_l_regr <-  lm(log(n_per) ~ Gini, data = rgr_Camion_pe,
                    na.action("na.omit"))
summary(track_l_regr)

plot(rgr_Camion_pe$Gini, log(rgr_Camion_pe$n_per), 
     xlab = "GINI COEFFICIENT BY DISTRICTS", 
     ylab = "MODAL SPLIT LOG %LITTEL TRACKS")
abline(track_l_regr)

ggplot(rgr_Camion_pe, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 'grey50')

#Median Income
track_l_regr_ed <-  lm(log(n_per) ~ log(Median_income), data = rgr_Camion_pe,
                       na.action("na.omit"))
summary(track_l_regr_ed)

plot(log(rgr_Camion_pe$Median_income), log(rgr_Camion_pe$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %LITTLE TRACK")
abline(track_l_regr_ed)

ggplot(rgr_Camion_pe, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T,na.rm = F) + geom_text(aes(label=DISTRITO),
                                                 hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE LITTEL TRACK",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE LITTLE TRACK") 

# Colectivos#####
rgr_colectivo <- regrx[which(regrx$Modalidad == "Colectivo"),]

#GINI
col_regr <-  lm(log(n_per) ~ Gini, data = rgr_colectivo,
                na.action("na.omit"))
summary(col_regr)

plot(rgr_colectivo$Gini, log(rgr_colectivo$n_per), 
     xlab = "GINI COEFFICIENT BY DISTRICTS", 
     ylab = "MODAL SPLIT LOG %COLECTIVO")
abline(col_regr)

ggplot(rgr_colectivo, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 
                                                          'grey50') +
  labs(title = "RELATIONSHIP BETWEEN GINI COEFFICIENT \nAND \nMODAL SHARE COLECTIVO",
       x = "GINI", y = "LOG %MODAL SHARE COLECTIVO") 

#Median Income
col_regr_med <-  lm(log(n_per) ~ log(Median_income), data = rgr_colectivo,
                       na.action("na.omit"))
summary(col_regr_med)

plot(log(rgr_colectivo$Median_income), log(rgr_colectivo$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %COLECTIVO")
abline(col_regr_med)

ggplot(rgr_colectivo, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T,na.rm = F) + geom_text(aes(label=DISTRITO),
                                                           hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE COLECTIVO",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE COLECTIVO") 


# Combi#####
rgr_combi <- regrx[which(regrx$Modalidad == "Combi"),]

#GINI
combi_regr <-  lm(log(n_per) ~ Gini, data = rgr_combi,
                na.action("na.omit"))
summary(combi_regr)

plot(rgr_combi$Gini, log(rgr_combi$n_per), 
     xlab = "GINI COEFFICIENT BY DISTRICTS", 
     ylab = "MODAL SPLIT LOG %COMBI")
abline(combi_regr)

ggplot(rgr_combi, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 
                                                          'grey50') +
  labs(title = "RELATIONSHIP BETWEEN GINI COEFFICIENT \nAND \nMODAL SHARE COMBI",
       x = "GINI", y = "LOG %MODAL SHARE COMBI") 

# Median Income 
combi_regr_med <-  lm(log(n_per) ~ log(Median_income), data = rgr_combi,
                    na.action("na.omit"))
summary(combi_regr_med)

plot(log(rgr_combi$Median_income), log(rgr_combi$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %COMBI")
abline(combi_regr_med)

ggplot(rgr_combi, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T,na.rm = F) + geom_text(aes(label=DISTRITO),
                                                           hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE COMBI",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE COMBI") 

# Metropolitano#####
rgr_metro <- regrx[which(regrx$Modalidad == "Metropolitano"),]

#GINI
metro_regr <-  lm(log(n_per) ~ Gini, data = rgr_metro,
                  na.action("na.omit"))
summary(metro_regr)


plot(rgr_metro$Gini, log(rgr_metro$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %BRT")
abline(metro_regr)

ggplot(rgr_metro, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 
                                                          'grey50') +
  labs(title = "RELATIONSHIP BETWEEN GINI COEFFICIENT \nAND \nMODAL SHARE BRT",
       x = "GINI", y = "LOG %MODAL SHARE BRT") 

#Median Income
rgr_metro_med <-  lm(log(n_per) ~ log(Median_income), data = rgr_metro,
                      na.action("na.omit"))
summary(rgr_metro_med)

plot(log(rgr_metro$Median_income), log(rgr_metro$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %BRT")
abline(rgr_metro_med)

ggplot(rgr_metro, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T,na.rm = F) + geom_text(aes(label=DISTRITO),
                                                           hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE BRT",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE BRT")


# Microbus #####
rgr_micro <- regrx[which(regrx$Modalidad == "Microbús"),]

#GINI
micro_regr <-  lm(log(n_per) ~ Gini, data = rgr_micro,
                  na.action("na.omit"))
summary(micro_regr)


plot(rgr_micro$Gini, log(rgr_micro$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %MICROBUS")
abline(micro_regr)

ggplot(rgr_micro, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 
                                                          'grey50') +
  labs(title = "RELATIONSHIP BETWEEN GINI COEFFICIENT \nAND \nMODAL SHARE MICROBUS",
       x = "GINI", y = "LOG %MODAL SHARE MICROBUS") 
#Median Income
micro_regr_med <-  lm(log(n_per) ~ log(Median_income), data = rgr_micro,
                     na.action("na.omit"))
summary(micro_regr_med)

plot(log(rgr_micro$Median_income), log(rgr_micro$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %MICROBUS")
abline(micro_regr_med)

ggplot(rgr_micro, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T,na.rm = F) + geom_text(aes(label=DISTRITO),
                                                           hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE MICROBUS",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE MICROBUS")

# Motocicleta####
rgr_motocicleta <- regrx[which(regrx$Modalidad == "Motocicleta"),]

#GINI
motocicleta_regr <-  lm(log(n_per) ~ Gini, data = rgr_motocicleta,
                  na.action("na.omit"))
summary(motocicleta_regr)


plot(rgr_motocicleta$Gini, log(rgr_motocicleta$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %MOTOCICLETA")
abline(motocicleta_regr)

ggplot(rgr_motocicleta, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 
                                                          'grey50') +
  labs(title = "RELATIONSHIP BETWEEN GINI COEFFICIENT \nAND \nMODAL SHARE MOTOCICLETA",
       x = "GINI", y = "LOG %MODAL SHARE MOTOCICLETA") 
#Median Income
motocicleta_regr_med <-  lm(log(n_per) ~ log(Median_income), data = rgr_motocicleta,
                      na.action("na.omit"))
summary(motocicleta_regr_med)

plot(log(rgr_motocicleta$Median_income), log(rgr_motocicleta$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %MOTOCICLETA")
abline(motocicleta_regr_med)

ggplot(rgr_motocicleta, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T,na.rm = F) + geom_text(aes(label=DISTRITO),
                                                           hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE MOTOCICLETA",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE MOTOCICLETA")

#Mototaxi####
rgr_mototaxi <- regrx[which(regrx$Modalidad == "Mototaxi"),]

#GINI
mototaxi_regr <-  lm(log(n_per) ~ Gini, data = rgr_mototaxi,
                        na.action("na.omit"))
summary(mototaxi_regr)

plot(rgr_mototaxi$Gini, log(rgr_mototaxi$n_per), 
     xlab = "GINI COEFFICIENT BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %MOTOTAXI")
abline(mototaxi_regr)

ggplot(rgr_mototaxi, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 
                                                          'grey50') +
  labs(title = "RELATIONSHIP BETWEEN GINI COEFFICIENT \nAND \nMODAL SHARE MOTOTAXI",
       x = "GINI", y = "LOG %MODAL SHARE MOTOTAXI") 

#Median Income
mototaxi_regr_med <-  lm(log(n_per) ~ log(Median_income), data = rgr_mototaxi,
                            na.action("na.omit"))
summary(mototaxi_regr_med)

plot(log(rgr_mototaxi$Median_income), log(rgr_mototaxi$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %MOTOTAXI")
abline(mototaxi_regr_med)

ggplot(rgr_mototaxi, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T,na.rm = F) + geom_text(aes(label=DISTRITO),
                                                           hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE MOTOTAXI",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE MOTOTAXI")

#Movilidad_Particular####
rgr_mov_particu <- regrx[which(regrx$Modalidad == "Movilidad_Particular"),]

#GINI
mov_particu_regr <-  lm(log(n_per) ~ Gini, data = rgr_mov_particu,
                     na.action("na.omit"))
summary(mov_particu_regr)

plot(rgr_mov_particu$Gini, log(rgr_mov_particu$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %PARTICULAR MOVILITY")
abline(mov_particu_regr)

ggplot(rgr_mov_particu, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 
                                                          'grey50') +
  labs(title = "RELATIONSHIP BETWEEN GINI COEFFICIENT \nAND \nMODAL SHARE PARTICULAR MOVILITY",
       x = "GINI", y = "LOG %MODAL SHARE PARTICULAR MOVILITY") 

#Median Income
mov_particu_regr_med <-  lm(log(n_per) ~ log(Median_income), data = rgr_mov_particu,
                         na.action("na.omit"))
summary(mov_particu_regr_med)

plot(log(rgr_mov_particu$Median_income), log(rgr_mov_particu$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %PARTICULAR MOVILITY")
abline(mov_particu_regr_med)

ggplot(rgr_mov_particu, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T,na.rm = F) + geom_text(aes(label=DISTRITO),
                                                           hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE PARTICULAR MOVILITY",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE PARTICULAR MOVILITY")


#Ninguno####
noon_regr <- regrx[which(regrx$Modalidad == "Ninguno"),]

#GINI
regr_noon <-  lm(log(n_per) ~ Gini, data = noon_regr,
                        na.action("na.omit"))
summary(regr_noon)

plot(noon_regr$Gini, log(noon_regr$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %NOON")
abline(regr_noon)

ggplot(noon_regr, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 
                                                          'grey50') +
  labs(title = "RELATIONSHIP BETWEEN GINI COEFFICIENT \nAND \nMODAL SHARE NOON",
       x = "GINI", y = "LOG %MODAL SHARE NOON") 

#Median Income
regr_noon_med <-  lm(log(n_per) ~ log(Median_income), data = noon_regr,
                            na.action("na.omit"))
summary(regr_noon_med)

plot(log(noon_regr$Median_income), log(noon_regr$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %NOON")
abline(regr_noon_med)

ggplot(noon_regr, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T,na.rm = F) + geom_text(aes(label=DISTRITO),
                                                           hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE NOON",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE NOON")

#Ómnibus####
omnibus_regr <- regrx[which(regrx$Modalidad == "Ómnibus"),]

#GINI
regr_omnibus <-  lm(log(n_per) ~ Gini, data = omnibus_regr,
                 na.action("na.omit"))
summary(regr_omnibus)

plot(omnibus_regr$Gini, log(omnibus_regr$n_per),
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %ONMIBUS")
abline(regr_omnibus)

ggplot(omnibus_regr, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 
                                                          'grey50') +
  labs(title = "RELATIONSHIP BETWEEN GINI COEFFICIENT \nAND \nMODAL SHARE ONMIBUS",
       x = "GINI", y = "LOG %MODAL SHARE ONMIBUS") 

#Median Income
regr_omnibus_med <-  lm(log(n_per) ~ log(Median_income), data = omnibus_regr,
                     na.action("na.omit"))
summary(regr_omnibus_med)

plot(log(omnibus_regr$Median_income), log(omnibus_regr$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %ONMIBUS")
abline(regr_omnibus_med)

ggplot(omnibus_regr, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T,na.rm = F) + geom_text(aes(label=DISTRITO),
                                                           hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE ONMIBUS",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE ONMIBUS")

#Otros####


#Taxi####
taxi_regr <- regrx[which(regrx$Modalidad == "Taxi"),]

#GINI
regr_taxi <-  lm(log(n_per) ~ Gini, data = taxi_regr,
                    na.action("na.omit"))
summary(regr_taxi)

plot(taxi_regr$Gini, log(taxi_regr$n_per),
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %TAXI")
abline(regr_taxi)

ggplot(taxi_regr, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 
                                                          'grey50') +
  labs(title = "RELATIONSHIP BETWEEN GINI COEFFICIENT \nAND \nMODAL SHARE TAXI",
       x = "GINI", y = "LOG %MODAL SHARE TAXI") 

#Median Income
regr_taxi_med <-  lm(log(n_per) ~ log(Median_income), data = taxi_regr,
                        na.action("na.omit"))
summary(regr_taxi_med)

plot(log(taxi_regr$Median_income), log(taxi_regr$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %TAXI")
abline(regr_taxi_med)

ggplot(taxi_regr, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T,na.rm = F) + geom_text(aes(label=DISTRITO),
                                                           hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE TAXI",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE TAXI")

#Tráiler####


#Tren####
tren_regr <- regrx[which(regrx$Modalidad == "Tren"),]

#GINI
regr_trem <-  lm(log(n_per) ~ Gini, data = tren_regr,
                 na.action("na.omit"))
summary(regr_trem)

plot(tren_regr$Gini, log(tren_regr$n_per),
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %TREN")
abline(regr_trem)

ggplot(tren_regr, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 
                                                          'grey50') +
  labs(title = "RELATIONSHIP BETWEEN GINI COEFFICIENT \nAND \nMODAL SHARE TREN",
       x = "GINI", y = "LOG %MODAL SHARE TREM") 

#Median Income
regr_trem_med <-  lm(log(n_per) ~ log(Median_income), data = tren_regr,
                     na.action("na.omit"))
summary(regr_trem_med)

plot(log(tren_regr$Median_income), log(tren_regr$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %TREN")
abline(regr_trem_med)

ggplot(tren_regr, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T,na.rm = F) + geom_text(aes(label=DISTRITO),
                                                           hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE TREN",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE TREN")










### Regresion con ZT#####
regrx_ZT <- full_join(General_table_ZT, income_parameter_ZT_sf,
                   by = c("ZONA_TRANSITO"), copy = T) %>% 
  distinct(id, .keep_all = T)
View(regrx_ZT)
nrow(regrx_ZT)
colnames(regrx_ZT)
regrx_ZT <- regrx_ZT[,c("DISTRITO","ZONA_TRANSITO","Modalidad","n_per",
                        "cost_per_travel","traveltime_per_trave","n_t",
                        "Gini","Median_income","sum_viaj","cost_viaje",
                        "n_t_M","Gini_MODAL","GINI_CONTRIBUTION","clasi_income")]

# Graph Summery
# Between districts
only_ZT <- regrx_ZT %>% distinct(ZONA_TRANSITO, .keep_all = T)
colnames(only_ZT)
View(only_ZT)
only_ZT$Median_income.qua <- (only_ZT$Median_income)^2
only_ZT$Travel_Budget <- only_ZT$cost_viaje/only_ZT$Median_income
only_ZT$Travel_Budget.qua <- (only_ZT$Travel_Budget)^2
only_ZT$Gini.qua <- (only_ZT$Gini)^2

only_ZT <- 
  only_ZT[!is.na(only_ZT$Gini),]
only_ZT <- 
  only_ZT[!is.na(only_ZT$Gini_MODAL),]
only_ZT <- 
  only_ZT[!is.na(only_ZT$sum_viaj),]
only_ZT <- 
  only_ZT[!is.na(only_ZT$cost_viaje),]
only_ZT <-
  only_ZT[!is.na(only_ZT$Median_income),]
only_ZT <-
  only_ZT[!is.na(only_ZT$GINI_CONTRIBUTION),]

#INDEX Graficas ordenadas por ####

modal_split_gini_vs_income_ZT <- only_ZT[,c(1,8,9,10,11,13,14,15)]
colnames(modal_split_gini_vs_income_ZT)
nrow(modal_split_gini_vs_income_ZT)
modal_split_gini_vs_income_ZT <- 
  modal_split_gini_vs_income_ZT[!is.na(modal_split_gini_vs_income_ZT$Gini),]
modal_split_gini_vs_income_ZT <- 
  modal_split_gini_vs_income_ZT[!is.na(modal_split_gini_vs_income_ZT$Gini_MODAL),]
modal_split_gini_vs_income_ZT <- 
  modal_split_gini_vs_income_ZT[!is.na(modal_split_gini_vs_income_ZT$sum_viaj),]  
modal_split_gini_vs_income_ZT <- 
  modal_split_gini_vs_income_ZT[!is.na(modal_split_gini_vs_income_ZT$cost_viaje),]
modal_split_gini_vs_income_ZT <-
  modal_split_gini_vs_income_ZT[!is.na(modal_split_gini_vs_income_ZT$Median_income),]

#Comprobar la Normalidad de la Distribution de las variables 0,001 Alpha
### GSM ###
shapiro.test(modal_split_gini_vs_income_ZT$Gini_MODAL)      
#P-Wert: 7.34e-12 Nicht-Normalverteilt
ggqqplot(modal_split_gini_vs_income_ZT$Gini_MODAL, ylab = "GMS")

### Travel Time  (TT) ###
shapiro.test(modal_split_gini_vs_income_ZT$sum_viaj)    
#P-Wert: 0.0197 Normalverteilt
ggqqplot(modal_split_gini_vs_income_ZT$sum_viaj, ylab = "Travel Time  (TT)")
### Travel Cost (TC) ###
shapiro.test(modal_split_gini_vs_income_ZT$cost_viaje) 
#P-Wert: 2.2e-16 Nicht-Normalverteil
ggqqplot(modal_split_gini_vs_income_ZT$cost_viaje, ylab = "Travel Cost  (TC)")
### Gini Coefficient by District ###
shapiro.test(modal_split_gini_vs_income_ZT$Gini)
#P-Wert: 0.005419 Normalverteilt 


### Gini Contribucion by District ###
shapiro.test(log(modal_split_gini_vs_income_ZT$GINI_CONTRIBUTION))
#P-Wert: 7.34e-12 Nicht- Normalverteilt

### Median Income by District ###
shapiro.test(modal_split_gini_vs_income_ZT$Median_income)
#P-Wert: 2.342e-10 Nicht-Normalverteilt 
shapiro.test(log(modal_split_gini_vs_income_ZT$Median_income))
#P-Wert: 7.855e-07 Nicht-Normalverteilt


#PLOT DE CORRELACIONES PARA MEJOR ORIENTACION
corrplot.mixed(cor(ordered_by_income_ZT[,c(2,3,4,5)]), 
               use = "complete.obs",lower = 'shade', upper = 'pie', 
               order = 'hclust')

#Ordered by income
ordered_by_income_ZT <- 
  modal_split_gini_vs_income_ZT[order(modal_split_gini_vs_income_ZT$
                                     Median_income), ]
ordered_by_income_ZT$idu <- as.numeric(row.names(modal_split_gini_vs_income_ZT))

# Con GMS
ggplot(ordered_by_income_ZT) + 
  geom_line(aes(x = idu,y=Gini_MODAL, col = clasi_income)) + 
  geom_point(aes(x = idu,y=Gini_MODAL)) +
  labs(y="GMS",
       x="Microzones ordered by Median Income")+ theme_classic()

cor.test(ordered_by_income_ZT$Gini_MODAL,
         ordered_by_income_ZT$Median_income, method = "pearson")
cor.test(ordered_by_income_ZT$Gini_MODAL,
         ordered_by_income_ZT$Median_income, method = "spearman")
cor.test(ordered_by_income_ZT$Gini_MODAL,
         ordered_by_income_ZT$Median_income, method = "kendall")

dat_ZT <- ordered_by_income_ZT %>% dplyr::select(Gini_MODAL, Median_income)
chart.Correlation(dat_ZT)   ### -0.1337**

#Analysis por grupos sociales
ordered_by_income_ZT_1q <- ordered_by_income_ZT[which(ordered_by_income_ZT$clasi_income ==
                                                        "1. Quartil"),]
ordered_by_income_ZT_2q <- ordered_by_income_ZT[which(ordered_by_income_ZT$clasi_income ==
                                                        "2.Quartil"),]
ordered_by_income_ZT_3q <- ordered_by_income_ZT[which(ordered_by_income_ZT$clasi_income ==
                                                        "3.Quantil"),]
ordered_by_income_ZT_4q <- ordered_by_income_ZT[which(ordered_by_income_ZT$clasi_income ==
                                                        "4.Quantil"),]

cor.test(ordered_by_income_ZT_1q$Gini_MODAL,log(ordered_by_income_ZT_1q$Median_income))
dat_ZTq1 <- ordered_by_income_ZT_1q%>% dplyr::select(Gini_MODAL, Median_income)
chart.Correlation(dat_ZTq1)   ### -0.2414445 *

cor.test(ordered_by_income_ZT_2q$Gini_MODAL,log(ordered_by_income_ZT_2q$Median_income))
dat_ZTq2 <- ordered_by_income_ZT_2q%>% dplyr::select(Gini_MODAL, Median_income)
chart.Correlation(dat_ZTq2)   ### 0 No significante

cor.test(ordered_by_income_ZT_3q$Gini_MODAL,log(ordered_by_income_ZT_3q$Median_income))
dat_ZTq3 <- ordered_by_income_ZT_3q%>% dplyr::select(Gini_MODAL, Median_income)
chart.Correlation(dat_ZTq3)   ### 0.04362756  No significante

cor.test(ordered_by_income_ZT_4q$Gini_MODAL,log(ordered_by_income_ZT_4q$Median_income))
dat_ZTq4 <- ordered_by_income_ZT_4q%>% dplyr::select(Gini_MODAL, Median_income)
chart.Correlation(dat_ZTq4)

ggplot(only_ZT, mapping = aes(x = Gini_MODAL,
                              y = log(cost_viaje/(Median_income)), color=clasi_income)) +
  geom_smooth(method = "lm" ,se = F) + 
  geom_point() +theme_classic() + 
  geom_label_repel(aes(label = DISTRITO), box.padding   = 0.35,
                   point.padding = 0.5, segment.color = 'grey50') +
  labs(title = "RELATIONSHIP BETWEEN GMS\nAND INCOME",x = "LOG MEDIAN INCOME",
       y = "GMS")


# Travel Time
ggplot(ordered_by_income_ZT) + 
  geom_line(aes(x = idu,y=sum_viaj, col = clasi_income)) + 
  geom_point(aes(x = idu,y=sum_viaj)) +
  labs(y="Travel Time",
       x="Microzones ordered by Median Income")+ theme_classic()

cor.test(ordered_by_income_ZT$sum_viaj, ordered_by_income_ZT$Median_income)
cor.test(ordered_by_income_ZT$sum_viaj,
         ordered_by_income_ZT$Median_income, method = "spearman")
cor.test(ordered_by_income_ZT$sum_viaj,
         ordered_by_income_ZT$Median_income, method = "kendall")


dat_ZT1 <- ordered_by_income_ZT%>% dplyr::select(sum_viaj, Median_income)
chart.Correlation(dat_ZT1)   ### Corelacion Negativa -0,33 *

#Analysis por grupos sociales
ordered_by_income_ZT_1q <- ordered_by_income_ZT[which(ordered_by_income_ZT$clasi_income ==
                                                  "1. Quantil"),]
ordered_by_income_ZT_2q <- ordered_by_income_ZT[which(ordered_by_income_ZT$clasi_income ==
                                                  "2.Quartil"),]
ordered_by_income_ZT_3q <- ordered_by_income_ZT[which(ordered_by_income_ZT$clasi_income ==
                                                  "3.Quantil"),]
ordered_by_income_ZT_4q <- ordered_by_income_ZT[which(ordered_by_income_ZT$clasi_income ==
                                                        "4.Quantil"),]

cor.test(ordered_by_income_ZT_1q$sum_viaj,ordered_by_income_ZT_1q$Median_income)
dat_ZT1q1 <- ordered_by_income_ZT_1q%>% dplyr::select(sum_viaj, Median_income)
chart.Correlation(dat_ZT1q1)   ### -0.2414445 *

cor.test(ordered_by_income_ZT_2q$sum_viaj,ordered_by_income_ZT_2q$Median_income)
dat_ZT1q2 <- ordered_by_income_ZT_2q%>% dplyr::select(sum_viaj, Median_income)
chart.Correlation(dat_ZT1q2)   ### 0 No significante

cor.test(ordered_by_income_ZT_3q$sum_viaj,ordered_by_income_ZT_3q$Median_income)
dat_ZT1q3 <- ordered_by_income_ZT_3q%>% dplyr::select(sum_viaj, Median_income)
chart.Correlation(dat_ZT1q3)   ### 0.04362756  No significante

cor.test(ordered_by_income_ZT_4q$sum_viaj,ordered_by_income_ZT_4q$Median_income)
dat_ZT1q4 <- ordered_by_income_ZT_4q%>% dplyr::select(sum_viaj, Median_income)
chart.Correlation(dat_ZT1q4)

#### DATOS DE INGRESO POR DISTRITOS NO MUY PRECISOS::: ANALIZAR CON MICROZONAS

#Travel Cost
ggplot(ordered_by_income_ZT) + 
  geom_line(aes(x = idu,y=cost_viaje, col = clasi_income)) + 
  geom_point(aes(x = idu,y=cost_viaje)) +
  labs(y="Travel Cost by Travel",
       x="Microzones ordered by Median Income")+ theme_classic()

cor.test(ordered_by_income_ZT$cost_viaje, 
         log(ordered_by_income_ZT$Median_income))
cor.test(ordered_by_income_ZT$cost_viaje,
         ordered_by_income_ZT$Median_income, method = "spearman")
cor.test(ordered_by_income_ZT$cost_viaje,
         ordered_by_income_ZT$Median_income, method = "kendall")

dat_ZT2 <- ordered_by_income_ZT%>% dplyr::select(cost_viaje, Median_income)
chart.Correlation(dat_ZT2)  ### Corelacion positiva 0,46** 

#Analysis por grupos sociales
ordered_by_income_ZT_1q <- ordered_by_income_ZT[which(ordered_by_income_ZT$clasi_income ==
                                                        "1. Quantil"),]
ordered_by_income_ZT_2q <- ordered_by_income_ZT[which(ordered_by_income_ZT$clasi_income ==
                                                        "2.Quartil"),]
ordered_by_income_ZT_3q <- ordered_by_income_ZT[which(ordered_by_income_ZT$clasi_income ==
                                                        "3.Quantil"),]
ordered_by_income_ZT_4q <- ordered_by_income_ZT[which(ordered_by_income_ZT$clasi_income ==
                                                        "4.Quantil"),]

cor.test((ordered_by_income_ZT_1q$cost_viaje),
         ordered_by_income_ZT_1q$Median_income)
dat_ZT2q1 <- ordered_by_income_ZT_1q%>% dplyr::select(cost_viaje, Median_income)
chart.Correlation(dat_ZT2q1)   ### -0.2144257 **

cor.test(ordered_by_income_ZT_2q$cost_viaje,
         ordered_by_income_ZT_2q$Median_income)
dat_ZT2q2 <- ordered_by_income_ZT_2q%>% dplyr::select(cost_viaje, Median_income)
chart.Correlation(dat_ZT2q2)  ## No correlacion

cor.test((ordered_by_income_ZT_3q$cost_viaje),
         ordered_by_income_ZT_3q$Median_income)
dat_ZT2q3 <- ordered_by_income_ZT_3q%>% dplyr::select(cost_viaje, Median_income)
chart.Correlation(dat_ZT2q3)  ##0.07834707 

cor.test((ordered_by_income_ZT_4q$cost_viaje),
         ordered_by_income_ZT_4q$Median_income)
dat_ZT2q4 <- ordered_by_income_ZT_4q%>% dplyr::select(cost_viaje, Median_income)
chart.Correlation(dat_ZT2q4)

#Gini Contribution
ggplot(ordered_by_income_ZT) + 
  geom_line(aes(x = idu,y=log(GINI_CONTRIBUTION), col = clasi_income)) + 
  geom_point(aes(x = idu,y=log(GINI_CONTRIBUTION))) +
  labs( y="Gini Contribution of each district to Gini Total",
        x="Districts ordered by Median Income")+ theme_classic()

cor.test(log(ordered_by_income_ZT$Median_income), 
         log(ordered_by_income_ZT$GINI_CONTRIBUTION))
cor.test(ordered_by_income_ZT$Median_income,
         log(ordered_by_income_ZT$GINI_CONTRIBUTION), method = "kendall")

dat_ZT3 <- ordered_by_income_ZT%>% dplyr::select(GINI_CONTRIBUTION, Median_income)
dat_ZT3$GINI_CONTRIBUTION <- log(dat_ZT3$GINI_CONTRIBUTION)
dat_ZT3$Median_income <- log(dat_ZT3$Median_income)
chart.Correlation(dat_ZT3)


#Gini
ggplot(ordered_by_income_ZT) + 
  geom_line(aes(x = idu,y=Gini, col = clasi_income)) + 
  geom_point(aes(x = idu,y=Gini)) +
  labs( y="Gini by district",
        x="Districts ordered by Median Income")+ theme_classic()

cor.test(ordered_by_income_ZT$Median_income, 
         ordered_by_income_ZT$Gini)
cor.test(ordered_by_income_ZT$Gini,
         ordered_by_income_ZT$Median_income, method = "kendall")

dat_ZT7 <- ordered_by_income_ZT%>% dplyr::select(Gini, Median_income)
chart.Correlation(dat_ZT7)

#ordered by Modalsplit Heterogeneity
ordered_by_Modalsplit_Heterogeneity_ZT <- 
  modal_split_gini_vs_income_ZT[order(modal_split_gini_vs_income_ZT$Gini_MODAL), ]
ordered_by_Modalsplit_Heterogeneity_ZT$idu <- as.numeric(row.names(ordered_by_Modalsplit_Heterogeneity_ZT))

# Travel Time
ggplot(ordered_by_Modalsplit_Heterogeneity_ZT) + 
  geom_line(aes(x = idu,y=sum_viaj, col = clasi_income)) +
  geom_point(aes(x = idu,y=sum_viaj)) +
  labs(y="Travel Time",
       x="Districts ordered by Modalsplit Heterogeneity")+ theme_classic()

cor.test(ordered_by_Modalsplit_Heterogeneity_ZT$sum_viaj, 
         ordered_by_Modalsplit_Heterogeneity_ZT$Gini_MODAL)
cor.test(ordered_by_Modalsplit_Heterogeneity_ZT$sum_viaj,
         ordered_by_Modalsplit_Heterogeneity_ZT$Gini_MODAL, method = "kendall")

dat_ZT4 <- ordered_by_Modalsplit_Heterogeneity_ZT%>% 
  dplyr::select(sum_viaj, Gini_MODAL)
chart.Correlation(dat_ZT4)

#Analysis por grupos sociales
ordered_by_Modalsplit_Heterogeneity_ZT_1q <- 
  ordered_by_Modalsplit_Heterogeneity_ZT[which(ordered_by_Modalsplit_Heterogeneity_ZT$clasi_income ==
                                              "1. Quartil"),]
ordered_by_Modalsplit_Heterogeneity_ZT_2q <- 
  ordered_by_Modalsplit_Heterogeneity_ZT[which(ordered_by_Modalsplit_Heterogeneity_ZT$clasi_income ==
                                              "2.Quartil"),]
ordered_by_Modalsplit_Heterogeneity_ZT_3q <- 
  ordered_by_Modalsplit_Heterogeneity_ZT[which(ordered_by_Modalsplit_Heterogeneity_ZT$clasi_income ==
                                              "3.Quantil"),]
ordered_by_Modalsplit_Heterogeneity_ZT_4q <- 
  ordered_by_Modalsplit_Heterogeneity_ZT[which(ordered_by_Modalsplit_Heterogeneity_ZT$clasi_income ==
                                                 "4.Quantil"),]

cor.test(ordered_by_Modalsplit_Heterogeneity_ZT_1q$sum_viaj,
         ordered_by_Modalsplit_Heterogeneity_ZT_1q$Gini_MODAL)
dat_ZT4q1 <- ordered_by_Modalsplit_Heterogeneity_ZT_1q%>% dplyr::select(sum_viaj, Gini_MODAL)
chart.Correlation(dat_ZT4q1) 

cor.test(ordered_by_Modalsplit_Heterogeneity_ZT_2q$sum_viaj,
         ordered_by_Modalsplit_Heterogeneity_ZT_2q$Gini_MODAL)
dat_ZT4q2 <- ordered_by_Modalsplit_Heterogeneity_ZT_2q%>% dplyr::select(sum_viaj, Gini_MODAL)
chart.Correlation(dat_ZT4q2)  

cor.test(ordered_by_Modalsplit_Heterogeneity_ZT_3q$sum_viaj,
         ordered_by_Modalsplit_Heterogeneity_ZT_3q$Gini_MODAL)
dat_ZT4q3 <- ordered_by_Modalsplit_Heterogeneity_ZT_3q%>% dplyr::select(sum_viaj, Gini_MODAL)
chart.Correlation(dat_ZT4q3)

cor.test(ordered_by_Modalsplit_Heterogeneity_ZT_4q$sum_viaj,
         ordered_by_Modalsplit_Heterogeneity_ZT_4q$Gini_MODAL)
dat_ZT4q4 <- ordered_by_Modalsplit_Heterogeneity_ZT_4q%>% dplyr::select(sum_viaj, Gini_MODAL)
chart.Correlation(dat_ZT4q4)

# Travel Cost
ggplot(ordered_by_Modalsplit_Heterogeneity_ZT) + 
  geom_line(aes(x = idu,y=log(cost_viaje/Median_income), col = clasi_income)) + 
  geom_point(aes(x = idu,y=log(cost_viaje/Median_income))) +
  labs(y="Travel Cost by Travel",
       x="Districts ordered by Modalsplit Heterogeneity")+ theme_classic()

cor.test((ordered_by_Modalsplit_Heterogeneity_ZT$cost_viaje), 
         ordered_by_Modalsplit_Heterogeneity_ZT$Gini_MODAL)
cor.test(ordered_by_Modalsplit_Heterogeneity_ZT$cost_viaje,
         ordered_by_Modalsplit_Heterogeneity_ZT$Gini_MODAL, method = "kendall")
cor.test(ordered_by_Modalsplit_Heterogeneity_ZT$cost_viaje,
         ordered_by_Modalsplit_Heterogeneity_ZT$Gini_MODAL, method = "spearman")

dat_ZT5 <- ordered_by_Modalsplit_Heterogeneity_ZT%>% 
  dplyr::select(cost_viaje, Gini_MODAL)
chart.Correlation(dat_ZT5)

#Analysis por grupos sociales
ordered_by_Modalsplit_Heterogeneity_ZT_1q <- 
  ordered_by_Modalsplit_Heterogeneity_ZT[which(ordered_by_Modalsplit_Heterogeneity_ZT$clasi_income ==
                                              "1. Quantil"),]
ordered_by_Modalsplit_Heterogeneity_ZT_2q <- 
  ordered_by_Modalsplit_Heterogeneity_ZT[which(ordered_by_Modalsplit_Heterogeneity_ZT$clasi_income ==
                                              "2.Quartil"),]
ordered_by_Modalsplit_Heterogeneity_ZT_3q <- 
  ordered_by_Modalsplit_Heterogeneity_ZT[which(ordered_by_Modalsplit_Heterogeneity_ZT$clasi_income ==
                                              "3.Quantil"),]
ordered_by_Modalsplit_Heterogeneity_ZT_4q <- 
  ordered_by_Modalsplit_Heterogeneity_ZT[which(ordered_by_Modalsplit_Heterogeneity_ZT$clasi_income ==
                                                 "4.Quantil"),]

cor.test(ordered_by_Modalsplit_Heterogeneity_ZT_1q$cost_viaje,
         ordered_by_Modalsplit_Heterogeneity_ZT_1q$Gini_MODAL)
dat_ZT5q1 <- ordered_by_Modalsplit_Heterogeneity_ZT_1q%>% dplyr::select(cost_viaje, Gini_MODAL)
chart.Correlation(dat_ZT5q1) 

cor.test(ordered_by_Modalsplit_Heterogeneity_ZT_2q$cost_viaje,
         ordered_by_Modalsplit_Heterogeneity_ZT_2q$Gini_MODAL)
dat_ZT5q2 <- ordered_by_Modalsplit_Heterogeneity_ZT_2q%>% dplyr::select(cost_viaje, Gini_MODAL)
chart.Correlation(dat_ZT5q2) 

cor.test(ordered_by_Modalsplit_Heterogeneity_ZT_3q$cost_viaje,
         ordered_by_Modalsplit_Heterogeneity_ZT_3q$Gini_MODAL)
dat_ZT5q3 <- ordered_by_Modalsplit_Heterogeneity_ZT_3q%>% dplyr::select(cost_viaje, Gini_MODAL)
chart.Correlation(dat_ZT5q3)

cor.test(ordered_by_Modalsplit_Heterogeneity_ZT_4q$cost_viaje,
         ordered_by_Modalsplit_Heterogeneity_ZT_4q$Gini_MODAL)
dat_ZT5q4 <- ordered_by_Modalsplit_Heterogeneity_ZT_4q%>% dplyr::select(cost_viaje, Gini_MODAL)
chart.Correlation(dat_ZT5q4)

# Gini Contribution
ggplot(ordered_by_Modalsplit_Heterogeneity_ZT) + 
  geom_line(aes(x = idu,y=log(GINI_CONTRIBUTION), col = clasi_income)) + 
  geom_point(aes(x = idu,y=log(GINI_CONTRIBUTION))) +
  labs(title="Monthly Time Series", 
       y="Gini Contribution to Gini within",
       x="Districts ordered by Modalsplit Heterogeneity") +theme_classic()
cor.test(log(ordered_by_Modalsplit_Heterogeneity_ZT$GINI_CONTRIBUTION), 
         ordered_by_Modalsplit_Heterogeneity_ZT$Gini_MODAL)
cor.test(log(ordered_by_Modalsplit_Heterogeneity_ZT$GINI_CONTRIBUTION),
         ordered_by_Modalsplit_Heterogeneity_ZT$Gini_MODAL, method = "kendall")

dat6 <- ordered_by_Modalsplit_Heterogeneity_ZT%>% 
  dplyr::select(GINI_CONTRIBUTION, Gini_MODAL)
dat6$GINI_CONTRIBUTION <- log(dat6$GINI_CONTRIBUTION)
chart.Correlation(dat6)


#Analysis por grupos sociales
ordered_by_Modalsplit_Heterogeneity_ZT_1q <- 
  ordered_by_Modalsplit_Heterogeneity_ZT[which(ordered_by_Modalsplit_Heterogeneity_ZT$clasi_income ==
                                              "1. Quartil"),]
ordered_by_Modalsplit_Heterogeneity_ZT_2q <- 
  ordered_by_Modalsplit_Heterogeneity_ZT[which(ordered_by_Modalsplit_Heterogeneity_ZT$clasi_income ==
                                              "2.Quartil"),]
ordered_by_Modalsplit_Heterogeneity_ZT_3q <- 
  ordered_by_Modalsplit_Heterogeneity_ZT[which(ordered_by_Modalsplit_Heterogeneity_ZT$clasi_income ==
                                              "3.Quantil"),]
ordered_by_Modalsplit_Heterogeneity_ZT_4q <- 
  ordered_by_Modalsplit_Heterogeneity_ZT[which(ordered_by_Modalsplit_Heterogeneity_ZT$clasi_income ==
                                                 "4.Quantil"),]


cor.test(log(ordered_by_Modalsplit_Heterogeneity_ZT_1q$GINI_CONTRIBUTION),
         ordered_by_Modalsplit_Heterogeneity_ZT_1q$Gini_MODAL)
dat_ZT5q1 <- ordered_by_Modalsplit_Heterogeneity_ZT_1q%>% 
  dplyr::select(GINI_CONTRIBUTION, Gini_MODAL)
dat_ZT5q1$GINI_CONTRIBUTION <- log(dat_ZT5q1$GINI_CONTRIBUTION)
chart.Correlation(dat_ZT5q1) 

cor.test(log(ordered_by_Modalsplit_Heterogeneity_2q$GINI_CONTRIBUTION),
         ordered_by_Modalsplit_Heterogeneity_2q$Gini_MODAL)
dat_ZT5q2 <- ordered_by_Modalsplit_Heterogeneity_2q%>% 
  dplyr::select(GINI_CONTRIBUTION, Gini_MODAL)
dat_ZT5q2$GINI_CONTRIBUTION <- log(dat_ZT5q2$GINI_CONTRIBUTION)
chart.Correlation(dat5q2) 

cor.test(log(ordered_by_Modalsplit_Heterogeneity_3q$GINI_CONTRIBUTION),
         ordered_by_Modalsplit_Heterogeneity_3q$Gini_MODAL)
dat_ZT5q3 <- ordered_by_Modalsplit_Heterogeneity_3q%>% 
  dplyr::select(GINI_CONTRIBUTION, Gini_MODAL)
dat_ZT5q3$GINI_CONTRIBUTION <- log(dat_ZT5q3$GINI_CONTRIBUTION)
chart.Correlation(dat_ZT5q3) 

cor.test(log(ordered_by_Modalsplit_Heterogeneity_ZT_4q$GINI_CONTRIBUTION),
         ordered_by_Modalsplit_Heterogeneity_ZT_4q$Gini_MODAL)
dat_ZT5q4 <- ordered_by_Modalsplit_Heterogeneity_ZT_4q%>% 
  dplyr::select(GINI_CONTRIBUTION, Gini_MODAL)
dat_ZT5q4$GINI_CONTRIBUTION <- log(dat_ZT5q4$GINI_CONTRIBUTION)
chart.Correlation(dat_ZT5q4) 

# Gini
ggplot(ordered_by_Modalsplit_Heterogeneity_ZT) + 
  geom_line(aes(x = idu,y=Gini, col = clasi_income)) + 
  geom_point(aes(x = idu,y=Gini)) +
  labs(title="Monthly Time Series", 
       y="Gini Coefficient by District",
       x="Districts ordered by Modalsplit Heterogeneity") +theme_classic()
cor.test(ordered_by_Modalsplit_Heterogeneity_ZT$Gini, 
         ordered_by_Modalsplit_Heterogeneity_ZT$Gini_MODAL)
dat_ZT8 <- ordered_by_Modalsplit_Heterogeneity_ZT%>% 
  dplyr::select(Gini, Gini_MODAL)
chart.Correlation(dat_ZT8)



#RELATIONSHIP BETWEEN MODAL SPLIT HETEROGENEITY INDEX \nAND GINI COEFFICIENT####
ggplot(only_ZT, mapping = aes(x = Gini, y = log(Median_income), col = clasi_income)) +
  geom_smooth(method = "glm" ,se = F) + 
  geom_point(aes(x = Gini, y = log(Median_income))) +theme_classic() +
  labs(title = "RELATIONSHIP BETWEEN MODAL SPLIT HETEROGENEITY INDEX \nAND GINI 
       COEFFICIENT",x = "GINI COEFFICIENT",
       y = "Log Median Income")

# Statistical Analysis
lm_Gini_vs_Gini_Modal_ZT <-lm(Gini_MODAL ~ poly(Gini,2)*log(Median_income), data = only_ZT)
summary(lm_Gini_vs_Gini_Modal_ZT)



# Iteraction with the media income
lm_Gini_vs_Gini_Modal_ZT <-lm(Gini_MODAL ~ Gini, data = only_ZT)
anova(lm_Gini_vs_Gini_Modal_ZT)
summary(lm_Gini_vs_Gini_Modal_ZT)


#Relationship between Gini Contribution \nand Modal Split heterogeneity Index ####
ggplot(only_ZT[which(only_ZT$clasi_income == "1. Quantil"),], mapping = aes(x = (Gini), 
                                    y = (Gini_MODAL),
                                    color=clasi_income)) +
  geom_point() + theme_classic()  +
  labs(title = "Relationship between Gini Coefficient\nand Modal Split heterogeneity Index",
       x = "Gini", 
       y = "GMS")


#Statistal Analysis
summary(lm(log(GINI_CONTRIBUTION) ~ (Gini_MODAL), 
           data = only_ZT))
xtable(summary(lm(log(GINI_CONTRIBUTION) ~ (Gini_MODAL), 
                  data = only_ZT)),auto = TRUE)


summary(lm(Gini_MODAL ~  poly(Gini,2), 
           data = only_ZT))


summary(lm(log(GINI_CONTRIBUTION) ~ Gini_MODAL*log(Median_income), 
           data = only_ZT))

xtable(summary(lm((GINI_CONTRIBUTION) ~ Gini_MODAL*log(Median_income), 
                  data = only_ZT)),auto = TRUE)


anova(lm((GINI_CONTRIBUTION) ~ Gini_MODAL*Median_income, 
           data = only_ZT))

ineq <- lm((Gini_MODAL) ~ Gini + log(GINI_CONTRIBUTION), 
           data = only_ZT)

summary(ineq)

ineq_d <- lm((Gini_MODAL) ~ Gini + log(GINI_CONTRIBUTION), 
           data = only_district)

summary(ineq_d)

library(stargazer)
stargazer(list(ineq,ineq_d),align=TRUE, no.space=TRUE)


library(car)
scatterplotMatrix( ~ Gini_MODAL+ Gini+ log(GINI_CONTRIBUTION), 
       data = only_ZT, pch = 19 ,
       diagonal = F, regLine = list(col = "green", lwd= 3),
       smooth = list(col.smooth = "red",
                     col.spread = "blue"))
install.packages("rgl")
library(rgl)
plot3d(only_ZT$Gini_MODAL, only_ZT$Gini, log(only_ZT$GINI_CONTRIBUTION))

Gini_income <- lm(Gini_MODAL ~ Gini, 
           data = only_ZT)

summary(Gini_income)


only_ZT$logincome <- log(only_ZT$Median_income)
only_district$logincome <- log(only_district$Median_income)

ggscatter(only_ZT, x = "logincome", y = "Gini_MODAL", add = "reg.line", conf.int = T,
          cor.coef = T, cor.method = "pearson", xlab  = "Gini Coefficient",
          ylab = "GMS")

ggscatter(only_district, x = "logincome", y = "Gini_MODAL", add = "reg.line", conf.int = T,
          cor.coef = T, cor.method = "pearson", xlab  = "Gini Coefficient",
          ylab = "GMS")


only_ZT$logGini_Cont <- log(only_ZT$GINI_CONTRIBUTION)
ggscatter(only_ZT, x = "logGini_Cont", y = "Gini_MODAL", add = "reg.line", conf.int = T,
          cor.coef = T, cor.method = "pearson", 
          xlab  = "Log Income Inequality between the zoned regions",
          ylab = "GMS")

summary(lm(log(GINI_CONTRIBUTION) ~ Gini, 
           data = only_ZT))

xtable(summary(lm(log(GINI_CONTRIBUTION) ~ log(sum_viaj), 
                  data = only_ZT)))


summary(lm(log(Gini) ~ (Median_income), 
           data = only_ZT))

lm_modal_Gini <- lm((Gini_MODAL) ~ (Gini), 
           data = only_ZT)
summary(lm_modal_Gini)
pred_Gini_lineal <- predict(lm_modal_Gini, type="terms")
resid_Gini_lineal <- residuals(lm_modal_Gini, type="partial")
ofl <- order(only_ZT$Gini)


plot(only_ZT$Gini[ofl],resid_Gini_lineal[ofl,"Gini"],
     ylab = "Effect of the Gini Coefficient", xlab = "Gini Coefficient", 
     main = "Marginal effect of the Gini Coefficient on the GMS", pch = 20)

lines(only_ZT$Gini[ofl],pred_Gini_lineal[ofl,"Gini"], col=2, lwd=2)

library("ggpubr")

ggscatter(only_ZT, x = "Gini", y = "Gini_MODAL", add = "reg.line", conf.int = T,
          cor.coef = T, cor.method = "pearson", xlab  = "Gini Coefficient",
          ylab = "GMS")


ggplot(only_ZT, mapping = aes(x = Gini, y = Gini_MODAL)) +
  geom_smooth(method = "lm" ,se = T) + 
  geom_point(aes(x = Gini, y = Gini_MODAL)) +theme_classic() +
  labs(title = "Relationship between GMS and Gini Coefficient",x = "Gini Coefficient",
       y = "GMS")

# Quadriert
lm_GMS_gini <- lm((Gini_MODAL) ~ poly(Gini, 2), 
                  data = only_ZT)
summary(lm_GMS_gini)
pred_Gini <- predict(lm_GMS_gini, type="terms")
resid_Gini <- residuals(lm_GMS_gini, type="partial")
ofl <- order(only_ZT$Gini)

#Effekto del Gini
pred_Gini_Y <- predict(lm_GMS_gini, type="terms")
resid_Gini_Y <- residuals(lm_GMS_gini, type="partial")
ofl <- order(only_ZT$Gini)
plot(only_ZT$Gini[ofl],resid_Gini[ofl,"poly(Gini, 2)"],
     ylab = "Effect of the Gini Coefficient", xlab = "Gini Coefficient", 
     main = "Effect of the Gini Coefficient in depence of the Gini Coefficient 
     on the GMS", pch = 20)

lines(only_ZT$Gini[ofl],pred_Gini_Y[ofl,"poly(Gini, 2)"], col=2, lwd=2)

lm_GMS_gini_y_dist <- lm((Gini_MODAL) ~ poly(Gini, 2), 
                    data = only_ZT)
summary(lm_GMS_gini_y_dist)

lm_GMS_gini_y <- lm((Gini_MODAL) ~ (Gini)*log(Median_income), 
                    data = only_ZT)
summary(lm_GMS_gini_y)


plot(only_ZT$Gini,only_ZT$Gini_MODAL)
plot(log(only_ZT$GINI_CONTRIBUTION),only_ZT$Gini_MODAL)

lm_GMS_gini_y_dis <- lm((Gini_MODAL) ~ log(Gini) + log(GINI_CONTRIBUTION), 
                    data = only_district)
summary(lm_GMS_gini_y_dis)


library(stargazer)
stargazer(lm_GMS_gini_y, lm_GMS_gini_y_dis)

## GMS relaciones
lm_GMS_gini_y <- lm(Gini_MODAL ~ poly(Gini,2) +  (Median_income) + (Median_income.qua), 
                  data = only_ZT)
summary(lm_GMS_gini_y)
plot(lm_GMS_gini_y)


lm_GMS_gini_y <- lm(Gini_MODAL ~ poly(Median_income,2), 
                    data = only_ZT)


#Effekto del Gini
pred_Gini_Y <- predict(lm_GMS_gini_y, type="terms")
resid_Gini_Y <- residuals(lm_GMS_gini_y, type="partial")
ofl <- order(only_ZT$Gini)
plot(only_ZT$Gini[ofl],resid_Gini_Y[ofl,"poly(Gini, 2)"],
     ylab = "Effect of the Gini Coefficient", xlab = "Gini Coefficient", 
     main = "Effect of the Gini Coefficient in depence of the Gini Coefficient 
     on the GMS", pch = 20)

lines(only_ZT$Gini[ofl],pred_Gini_Y[ofl,"poly(Gini, 2)"], col=2, lwd=2)


#Effekto del Income
ofl_Y <- order(only_ZT$Median_income)
plot((only_ZT$Median_income[ofl_Y]),resid_Gini_Y[ofl_Y,"Median_income"],
     ylab = "Effect of the Median Income", xlab = "Median Income", 
     main = "Effect of the Median Income in depence of the ;edian Income 
     on the GMS", pch = 20)

lines((only_ZT$Median_income[ofl_Y]),pred_Gini_Y[ofl_Y,"Median_income"], col=2, lwd=2)

#Effekto del Interaction
ofl_Y_G <- order(only_ZT$Median_income)
plot(log(only_ZT$Median_income[ofl_Y]),resid_Gini_Y[ofl_Y,"poly(Gini, 2):Median_income"],
     ylab = "Effect of the Median Income", xlab = "Median Income", 
     main = "Effect of the Median Income in depence of the ;edian Income 
     on the GMS", pch = 20)

lines(log(only_ZT$Median_income[ofl_Y]),pred_Gini_Y[ofl_Y,"poly(Gini, 2):Median_income"], col=2, lwd=2)


#### Separar por quantiles
only_ZT_q1 <- only_ZT[which(only_ZT$clasi_income == "1. Quantil"),]
only_ZT_q2 <- only_ZT[which(only_ZT$clasi_income == "2.Quartil"),]
only_ZT_q3 <- only_ZT[which(only_ZT$clasi_income == "3.Quantil"),]
only_ZT_q4 <- only_ZT[which(only_ZT$clasi_income == "4.Quantil"),]


lm_GMS_gini_y_q1 <- lm((Gini_MODAL) ~ poly(Gini, 2), 
                    data = only_ZT_q1)
summary(lm_GMS_gini_y_q1)

lm_GMS_gini_y_q2 <- lm((Gini_MODAL) ~ poly(Gini, 2), 
                       data = only_ZT_q2)
summary(lm_GMS_gini_y_q2)

lm_GMS_gini_y_q3 <- lm((Gini_MODAL) ~ poly(Gini, 2), 
                       data = only_ZT_q3)
summary(lm_GMS_gini_y_q3)

lm_GMS_gini_y_q4 <- lm((Gini_MODAL) ~ poly(Gini, 2), 
                       data = only_ZT_q4)
summary(lm_GMS_gini_y_q4)


##### Effectos del coste y tiempo de viaje promedio por distrito al median income y Gini#####
lm1_ZT <- lm(log(Median_income) ~ log(cost_viaje) + log(sum_viaj) ,data = only_ZT)
summary(lm1_ZT)


xtable(lm1_ZT)

library(stargazer)
stargazer(list(lm1_ZT,lm1),align=TRUE, no.space=TRUE)

## Curiosidades
lm13_ZT <- lm(log(sum_viaj) ~ log(Median_income) ,data = only_ZT)
summary(lm13_ZT)

lm14_ZT <- lm(log(cost_viaje) ~ log(Median_income),data = only_ZT)
summary(lm14_ZT)

lm15_ZT <- lm(log(sum_viaj) ~ log(cost_viaje),data = only_ZT)
summary(lm15_ZT)

lm15_ZT <- lm(log(sum_viaj) ~ log(cost_viaje),data = only_ZT)
summary(lm15_ZT)

lm2_ZT <- lm(Gini_MODAL  ~ log(sum_viaj), data = only_ZT)
summary(lm2_ZT)  # Se puede LOG tmb

lm3_ZT <- lm(Gini_MODAL ~ (cost_viaje), data = only_ZT)
summary(lm3_ZT)  

lm16_ZT <- lm(Gini_MODAL ~ log(Median_income),data = only_ZT)
summary(lm16_ZT)

lm4_ZT <- lm(Gini_MODAL ~ (Median_income.qua) + (Median_income), data = only_ZT)
summary(lm4_ZT)

plot(only_ZT$Gini_MODAL, type = "l")
lines(only_ZT$Gini, col = "red", type = "l")

plot(log(only_ZT$Gini_MODAL), type = "l")
lines(log(only_ZT$Median_income), col = "red", type = "l")


## Interactiones
lm5_ZT <- lm((Gini_MODAL)  ~ log(sum_viaj) + 
            (cost_viaje)/(Median_income), data = only_ZT)
summary(lm5_ZT)

xtable(lm5_ZT)

lm6_ZT <- lm((GINI_CONTRIBUTION)  ~ log(sum_viaj) + 
               ((cost_viaje)/(Median_income)), 
          data = only_ZT)
summary(lm6_ZT)

lm12_ZT <- lm((Gini_MODAL)  ~ log(cost_viaje/Median_income), 
           data = only_ZT)
summary(lm12_ZT)

lm7_ZT <- lm(Gini_MODAL  ~ log(cost_viaje)*log(sum_viaj), 
          data = only_ZT)
summary(lm7_ZT)

lm8_ZT <- lm(Gini_MODAL  ~  log(sum_viaj) + log(cost_viaje/Median_income), 
          data = only_ZT)
summary(lm8_ZT)

lm10_ZT <- lm(Gini_MODAL  ~  log(sum_viaj) + log(cost_viaje/Median_income), 
             data = only_district)

summary(lm10_ZT)

stargazer(list(lm8_ZT,lm10_ZT),align=TRUE, no.space=TRUE)

lm20_ZT <- lm(Gini_MODAL  ~  Travel_Budget.qua + Travel_Budget, 
             data = only_ZT)
summary(lm20_ZT)
stargazer(list(lm20_ZT,lm20),align=TRUE, no.space=TRUE)


xtable(lm9_ZT)


#Relationship between Median Income and Travel Costs ###
ggplot(only_ZT, mapping = aes(x = log(cost_viaje), y = log(Median_income),color=clasi_income)) +
  geom_smooth(method = "lm" ,se = F) + 
  geom_point() +theme_classic()  +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND TRAVEL COST 
       COEFFICIENT",x = "TRAVEL COSTS",
       y = "LOG MEDIAN INCOME")

##### Effectos del Heterogeinity Index al Gini Contribution#####



ggplot(regrx_ZT, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = F) + facet_wrap(~Modalidad, scales="free_x")+
  labs(title = "RELATIONSHIP BETWEEN GINI COEFFICIENT\nAND MODAL SHARE",
       x = "GINI COEFFICIENT BY DISTRICT", y = "LOG %MODAL SHARE") 

ggplot(regrx, mapping = aes(x = log(Median_income), y = log(n_per))) +
  geom_smooth(method = "lm" ,se = T) + facet_wrap(~Modalidad, scales="free_x")+
  geom_point(aes(color = Modalidad), size= 1) + theme_classic() +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND MODAL SHARE",
       x = "LOG MEDIAN INCOME BY DISTRICT", y = "LOG %MODAL SHARE") 


ggplot(regrx, mapping = aes(x = (Gini_MODAL), y = log(n_per))) +
  geom_smooth(method = "lm" ,se = F) + facet_wrap(~Modalidad, scales="free_x")+
  geom_point(aes(color = Modalidad), size= 1) + theme_classic() +
  labs(title = "RELATIONSHIP BETWEEN GMS \nAND MODAL SHARE",
       x = "GMS of each Microzone", y = "LOG %MODAL SHARE") 

ggplot(regrx_ZT, aes(x = Modalidad, y = log(Median_income))) +
  geom_boxplot()


## Dividir por Modalidad ###
table(regrx$Modalidad)


# Graph Summary 
# Between Modalities


# Auto_Particular#####
rgr_auto_particular <- regrx[which(regrx_ZT$Modalidad == "Auto_Particular"),] 

#GINI
auto_regr <-  lm(log(n_per) ~ Gini, data = rgr_auto_particular)
summary(auto_regr)
plot(rgr_auto_particular$Gini, log(rgr_auto_particular$n_per), 
     xlab = "GINI COEFFICIENT BY DISTRICTS", 
     ylab = "MODAL SPLIT LOG %AUTO")
abline(auto_regr)

ggplot(rgr_auto_particular, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = 0) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 'grey50')

#Median Income
auto_regr_med <-  lm(log(n_per) ~ log(Median_income), data = rgr_auto_particular)
summary(auto_regr_med) #SEHR SIGNIFICANT

plot(log(rgr_auto_particular$Median_income), log(rgr_auto_particular$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %AUTO")
abline(auto_regr_med)

ggplot(rgr_auto_particular, 
       mapping = aes(x = log(rgr_auto_particular$Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_text(aes(label=DISTRITO),
                                                 hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE AUTO",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE AUTO") 

# Bicicleta#####
rgr_Bicicleta <- regrx_ZT[which(regrx_ZT$Modalidad == "Bicicleta"),] 

#GINI
bici_regr <-  lm(log(n_per) ~ Gini, data = rgr_Bicicleta, na.action("na.omit"))
summary(bici_regr)   #NICHT SIGNIFIKANT


plot(rgr_Bicicleta$Gini, log(rgr_Bicicleta$n_per), 
     xlab = "GINI COEFFICIENT BY DISTRICTS", 
     ylab = "MODAL SPLIT LOG %BICICLE")
abline(bici_regr)

ggplot(rgr_Bicicleta, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 'grey50')

#Median Income
bici_regr_med <-  lm(log(n_per) ~ log(Median_income), data = rgr_Bicicleta)
summary(bici_regr_med)

plot(log(rgr_Bicicleta$Median_income), log(rgr_Bicicleta$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %BICICLE")
abline(bici_regr_med)

ggplot(rgr_Bicicleta, 
       mapping = aes(x = log(rgr_Bicicleta$Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_text(aes(label=DISTRITO),
                                                 hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE BICICLE",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE BICICLE") 

# Caminando####
rgr_Caminando <- regrx_ZT[which(regrx_ZT$Modalidad == "Caminando"),] 

#GINI
walk_regr <-  lm(log(n_per) ~ Gini, data = rgr_Caminando)
summary(walk_regr)


plot(rgr_Caminando$Gini, log(rgr_Caminando$n_per), 
     xlab = "GINI COEFFICIENT BY DISTRICTS", 
     ylab = "MODAL SPLIT LOG %WALK")
abline(walk_regr)

ggplot(rgr_Caminando, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 'grey50')


#Median Income
rgr_Caminando_med <-  lm(log(n_per) ~ log(Median_income), data = rgr_Caminando,
                         na.action("na.omit"))
summary(rgr_Caminando_med) #SIGNIFICNAT

plot(log(rgr_Caminando$Median_income), log(rgr_Caminando$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %BICICLE")
abline(rgr_Caminando_med)

ggplot(rgr_Caminando, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_text(aes(label=DISTRITO),
                                                 hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE BICICLE",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE WALK") 

# Camión#####
rgr_Camion <- regrx_ZT[which(regrx_ZT$Modalidad == "Camión"),] 

#GINI
track_regr <-  lm(log(n_per) ~ Gini, data = rgr_Camion,
                  na.action("na.omit"))
summary(track_regr)


plot(rgr_Camion$Gini, log(rgr_Camion$n_per), 
     xlab = "GINI COEFFICIENT BY DISTRICTS", 
     ylab = "MODAL SPLIT LOG %TRACKS")
abline(track_regr)

ggplot(rgr_Camion, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 'grey50')

#Median Income
rgr_track_med <-  lm(log(n_per) ~ log(Median_income), data = rgr_Camion,
                     na.action("na.omit"))
summary(rgr_track_med)

plot(log(rgr_Camion$Median_income), log(rgr_Camion$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %BICICLE")
abline(rgr_track_med)

ggplot(rgr_Camion, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_text(aes(label=DISTRITO),
                                                 hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE BICICLE",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE TRACK") 

# Camión_pequeno#####
rgr_Camion_pe <- regrx_ZT[which(regrx_ZT$Modalidad == "Camión_pequeno"),]

#GINI
track_l_regr <-  lm(log(n_per) ~ Gini, data = rgr_Camion_pe,
                    na.action("na.omit"))
summary(track_l_regr)

plot(rgr_Camion_pe$Gini, log(rgr_Camion_pe$n_per), 
     xlab = "GINI COEFFICIENT BY DISTRICTS", 
     ylab = "MODAL SPLIT LOG %LITTEL TRACKS")
abline(track_l_regr)

ggplot(rgr_Camion_pe, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 'grey50')

#Median Income
track_l_regr_ed <-  lm(log(n_per) ~ log(Median_income), data = rgr_Camion_pe,
                       na.action("na.omit"))
summary(track_l_regr_ed)

plot(log(rgr_Camion_pe$Median_income), log(rgr_Camion_pe$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %LITTLE TRACK")
abline(track_l_regr_ed)

ggplot(rgr_Camion_pe, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T,na.rm = F) + geom_text(aes(label=DISTRITO),
                                                           hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE LITTEL TRACK",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE LITTLE TRACK") 

# Colectivos#####
rgr_colectivo <- regrx_ZT[which(regrx_ZT$Modalidad == "Colectivo"),]

#GINI
col_regr <-  lm(log(n_per) ~ Gini, data = rgr_colectivo,
                na.action("na.omit"))
summary(col_regr)

plot(rgr_colectivo$Gini, log(rgr_colectivo$n_per), 
     xlab = "GINI COEFFICIENT BY DISTRICTS", 
     ylab = "MODAL SPLIT LOG %COLECTIVO")
abline(col_regr)

ggplot(rgr_colectivo, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 
                                                          'grey50') +
  labs(title = "RELATIONSHIP BETWEEN GINI COEFFICIENT \nAND \nMODAL SHARE COLECTIVO",
       x = "GINI", y = "LOG %MODAL SHARE COLECTIVO") 

#Median Income
col_regr_med <-  lm(log(n_per) ~ log(Median_income), data = rgr_colectivo,
                    na.action("na.omit"))
summary(col_regr_med)

plot(log(rgr_colectivo$Median_income), log(rgr_colectivo$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %COLECTIVO")
abline(col_regr_med)

ggplot(rgr_colectivo, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T,na.rm = F) + geom_text(aes(label=DISTRITO),
                                                           hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE COLECTIVO",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE COLECTIVO") 


# Combi#####
rgr_combi <- regrx_ZT[which(regrx_ZT$Modalidad == "Combi"),]

#GINI
combi_regr <-  lm(log(n_per) ~ Gini, data = rgr_combi,
                  na.action("na.omit"))
summary(combi_regr)

plot(rgr_combi$Gini, log(rgr_combi$n_per), 
     xlab = "GINI COEFFICIENT BY DISTRICTS", 
     ylab = "MODAL SPLIT LOG %COMBI")
abline(combi_regr)

ggplot(rgr_combi, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 
                                                          'grey50') +
  labs(title = "RELATIONSHIP BETWEEN GINI COEFFICIENT \nAND \nMODAL SHARE COMBI",
       x = "GINI", y = "LOG %MODAL SHARE COMBI") 

# Median Income 
combi_regr_med <-  lm(log(n_per) ~ log(Median_income), data = rgr_combi,
                      na.action("na.omit"))
summary(combi_regr_med)

plot(log(rgr_combi$Median_income), log(rgr_combi$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %COMBI")
abline(combi_regr_med)

ggplot(rgr_combi, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T,na.rm = F) + geom_text(aes(label=DISTRITO),
                                                           hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE COMBI",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE COMBI") 

# Metropolitano#####
rgr_metro <- regrx_ZT[which(regrx_ZT$Modalidad == "Metropolitano"),]

#GINI
metro_regr <-  lm(log(n_per) ~ Gini, data = rgr_metro,
                  na.action("na.omit"))
summary(metro_regr)


plot(rgr_metro$Gini, log(rgr_metro$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %BRT")
abline(metro_regr)

ggplot(rgr_metro, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 
                                                          'grey50') +
  labs(title = "RELATIONSHIP BETWEEN GINI COEFFICIENT \nAND \nMODAL SHARE BRT",
       x = "GINI", y = "LOG %MODAL SHARE BRT") 

#Median Income
rgr_metro_med <-  lm(log(n_per) ~ log(Median_income), data = rgr_metro,
                     na.action("na.omit"))
summary(rgr_metro_med)

plot(log(rgr_metro$Median_income), log(rgr_metro$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %BRT")
abline(rgr_metro_med)

ggplot(rgr_metro, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T,na.rm = F) + geom_text(aes(label=DISTRITO),
                                                           hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE BRT",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE BRT")


# Microbus #####
rgr_micro <- regrx_ZT[which(regrx_ZT$Modalidad == "Microbús"),]

#GINI
micro_regr <-  lm(log(n_per) ~ Gini, data = rgr_micro,
                  na.action("na.omit"))
summary(micro_regr)


plot(rgr_micro$Gini, log(rgr_micro$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %MICROBUS")
abline(micro_regr)

ggplot(rgr_micro, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 
                                                          'grey50') +
  labs(title = "RELATIONSHIP BETWEEN GINI COEFFICIENT \nAND \nMODAL SHARE MICROBUS",
       x = "GINI", y = "LOG %MODAL SHARE MICROBUS") 
#Median Income
micro_regr_med <-  lm(log(n_per) ~ log(Median_income), data = rgr_micro,
                      na.action("na.omit"))
summary(micro_regr_med)

plot(log(rgr_micro$Median_income), log(rgr_micro$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %MICROBUS")
abline(micro_regr_med)

ggplot(rgr_micro, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T,na.rm = F) + geom_text(aes(label=DISTRITO),
                                                           hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE MICROBUS",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE MICROBUS")

# Motocicleta####
rgr_motocicleta <- regrx_ZT[which(regrx_ZT$Modalidad == "Motocicleta"),]

#GINI
motocicleta_regr <-  lm(log(n_per) ~ Gini, data = rgr_motocicleta,
                        na.action("na.omit"))
summary(motocicleta_regr)


plot(rgr_motocicleta$Gini, log(rgr_motocicleta$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %MOTOCICLETA")
abline(motocicleta_regr)

ggplot(rgr_motocicleta, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 
                                                          'grey50') +
  labs(title = "RELATIONSHIP BETWEEN GINI COEFFICIENT \nAND \nMODAL SHARE MOTOCICLETA",
       x = "GINI", y = "LOG %MODAL SHARE MOTOCICLETA") 
#Median Income
motocicleta_regr_med <-  lm(log(n_per) ~ log(Median_income), data = rgr_motocicleta,
                            na.action("na.omit"))
summary(motocicleta_regr_med)

plot(log(rgr_motocicleta$Median_income), log(rgr_motocicleta$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %MOTOCICLETA")
abline(motocicleta_regr_med)

ggplot(rgr_motocicleta, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T,na.rm = F) + geom_text(aes(label=DISTRITO),
                                                           hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE MOTOCICLETA",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE MOTOCICLETA")

#Mototaxi####
rgr_mototaxi <- regrx_ZT[which(regrx_ZT$Modalidad == "Mototaxi"),]

#GINI
mototaxi_regr <-  lm(log(n_per) ~ Gini, data = rgr_mototaxi,
                     na.action("na.omit"))
summary(mototaxi_regr)

plot(rgr_mototaxi$Gini, log(rgr_mototaxi$n_per), 
     xlab = "GINI COEFFICIENT BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %MOTOTAXI")
abline(mototaxi_regr)

ggplot(rgr_mototaxi, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 
                                                          'grey50') +
  labs(title = "RELATIONSHIP BETWEEN GINI COEFFICIENT \nAND \nMODAL SHARE MOTOTAXI",
       x = "GINI", y = "LOG %MODAL SHARE MOTOTAXI") 

#Median Income
mototaxi_regr_med <-  lm(log(n_per) ~ log(Median_income), data = rgr_mototaxi,
                         na.action("na.omit"))
summary(mototaxi_regr_med)

plot(log(rgr_mototaxi$Median_income), log(rgr_mototaxi$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %MOTOTAXI")
abline(mototaxi_regr_med)

ggplot(rgr_mototaxi, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T,na.rm = F) + geom_text(aes(label=DISTRITO),
                                                           hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE MOTOTAXI",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE MOTOTAXI")

#Movilidad_Particular####
rgr_mov_particu <- regrx_ZT[which(regrx_ZT$Modalidad == "Movilidad_Particular"),]

#GINI
mov_particu_regr <-  lm(log(n_per) ~ Gini, data = rgr_mov_particu,
                        na.action("na.omit"))
summary(mov_particu_regr)

plot(rgr_mov_particu$Gini, log(rgr_mov_particu$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %PARTICULAR MOVILITY")
abline(mov_particu_regr)

ggplot(rgr_mov_particu, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 
                                                          'grey50') +
  labs(title = "RELATIONSHIP BETWEEN GINI COEFFICIENT \nAND \nMODAL SHARE PARTICULAR MOVILITY",
       x = "GINI", y = "LOG %MODAL SHARE PARTICULAR MOVILITY") 

#Median Income
mov_particu_regr_med <-  lm(log(n_per) ~ log(Median_income), data = rgr_mov_particu,
                            na.action("na.omit"))
summary(mov_particu_regr_med)

plot(log(rgr_mov_particu$Median_income), log(rgr_mov_particu$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %PARTICULAR MOVILITY")
abline(mov_particu_regr_med)

ggplot(rgr_mov_particu, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T,na.rm = F) + geom_text(aes(label=DISTRITO),
                                                           hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE PARTICULAR MOVILITY",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE PARTICULAR MOVILITY")


#Ninguno####
noon_regr <- regrx_ZT[which(regrx_ZT$Modalidad == "Ninguno"),]

#GINI
regr_noon <-  lm(log(n_per) ~ Gini, data = noon_regr,
                 na.action("na.omit"))
summary(regr_noon)

plot(noon_regr$Gini, log(noon_regr$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %NOON")
abline(regr_noon)

ggplot(noon_regr, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 
                                                          'grey50') +
  labs(title = "RELATIONSHIP BETWEEN GINI COEFFICIENT \nAND \nMODAL SHARE NOON",
       x = "GINI", y = "LOG %MODAL SHARE NOON") 

#Median Income
regr_noon_med <-  lm(log(n_per) ~ log(Median_income), data = noon_regr,
                     na.action("na.omit"))
summary(regr_noon_med)

plot(log(noon_regr$Median_income), log(noon_regr$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %NOON")
abline(regr_noon_med)

ggplot(noon_regr, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T,na.rm = F) + geom_text(aes(label=DISTRITO),
                                                           hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE NOON",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE NOON")

#Ómnibus####
omnibus_regr <- regrx_ZT[which(regrx_ZT$Modalidad == "Ómnibus"),]

#GINI
regr_omnibus <-  lm(log(n_per) ~ Gini, data = omnibus_regr,
                    na.action("na.omit"))
summary(regr_omnibus)

plot(omnibus_regr$Gini, log(omnibus_regr$n_per),
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %ONMIBUS")
abline(regr_omnibus)

ggplot(omnibus_regr, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 
                                                          'grey50') +
  labs(title = "RELATIONSHIP BETWEEN GINI COEFFICIENT \nAND \nMODAL SHARE ONMIBUS",
       x = "GINI", y = "LOG %MODAL SHARE ONMIBUS") 

#Median Income
regr_omnibus_med <-  lm(log(n_per) ~ log(Median_income), data = omnibus_regr,
                        na.action("na.omit"))
summary(regr_omnibus_med)

plot(log(omnibus_regr$Median_income), log(omnibus_regr$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %ONMIBUS")
abline(regr_omnibus_med)

ggplot(omnibus_regr, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T,na.rm = F) + geom_text(aes(label=DISTRITO),
                                                           hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE ONMIBUS",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE ONMIBUS")

#Otros####


#Taxi####
taxi_regr <- regrx_ZT[which(regrx_ZT$Modalidad == "Taxi"),]

#GINI
regr_taxi <-  lm(log(n_per) ~ Gini, data = taxi_regr,
                 na.action("na.omit"))
summary(regr_taxi)

plot(taxi_regr$Gini, log(taxi_regr$n_per),
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %TAXI")
abline(regr_taxi)

ggplot(taxi_regr, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 
                                                          'grey50') +
  labs(title = "RELATIONSHIP BETWEEN GINI COEFFICIENT \nAND \nMODAL SHARE TAXI",
       x = "GINI", y = "LOG %MODAL SHARE TAXI") 

#Median Income
regr_taxi_med <-  lm(log(n_per) ~ log(Median_income), data = taxi_regr,
                     na.action("na.omit"))
summary(regr_taxi_med)

plot(log(taxi_regr$Median_income), log(taxi_regr$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %TAXI")
abline(regr_taxi_med)

ggplot(taxi_regr, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T,na.rm = F) + geom_text(aes(label=DISTRITO),
                                                           hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE TAXI",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE TAXI")

#Tráiler####


#Tren####
tren_regr <- regrx_ZT[which(regrx_ZT$Modalidad == "Tren"),]

#GINI
regr_trem <-  lm(log(n_per) ~ Gini, data = tren_regr,
                 na.action("na.omit"))
summary(regr_trem)

plot(tren_regr$Gini, log(tren_regr$n_per),
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %TREN")
abline(regr_trem)

ggplot(tren_regr, mapping = aes(x = Gini, y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T) + geom_label_repel(aes(label = DISTRITO),
                                                        box.padding   = 0.35, 
                                                        point.padding = 0.5,
                                                        segment.color = 
                                                          'grey50') +
  labs(title = "RELATIONSHIP BETWEEN GINI COEFFICIENT \nAND \nMODAL SHARE TREN",
       x = "GINI", y = "LOG %MODAL SHARE TREM") 

#Median Income
regr_trem_med <-  lm(log(n_per) ~ log(Median_income), data = tren_regr,
                     na.action("na.omit"))
summary(regr_trem_med)

plot(log(tren_regr$Median_income), log(tren_regr$n_per), 
     xlab = "LOG MEDIAN INCOME BY DISTRICTS", 
     ylab = "LOG MODAL SPLIT LOG %TREN")
abline(regr_trem_med)

ggplot(tren_regr, 
       mapping = aes(x = log(Median_income), 
                     y = log(n_per))) +
  geom_point(aes(color = Modalidad)) + theme_classic() +
  geom_smooth(method = "lm", se = T,na.rm = F) + geom_text(aes(label=DISTRITO),
                                                           hjust=0, vjust=0, size = 2) +
  labs(title = "RELATIONSHIP BETWEEN MEDIAN INCOME \nAND \nMODAL SHARE TREN",
       x = "LOG MEDIAN INCOME", y = "LOG %MODAL SHARE TREN")







