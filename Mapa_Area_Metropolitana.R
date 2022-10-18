library(ggplot2)
library(raster)
library(dplyr)
library(readr)
library(haven)
library(utils)
library(RColorBrewer)
library(sf)
library(sp)
library(psych)
library(tidyverse)
library(stringr)
library(ggplot2)
library(ggmap)
library(ggalt)
library(fortify)

##### Clasificación de los Distritos, según los UBIGEOS DEL INEI #####

#### Provincia Constitutional Del Callao (6 DIST) ####


# CALLAO   --> 070101
# BELLAVISTA --> 070102
# CARMEN DE LA LEGUA Y REYNOSO --> 070103
# LA PERLA --> 070104
# LA PUNTA --> 070105
# VENTANILLA --> 070106

#### LIMA METROPOLITANA (43 DIST) ####
# LIMA --> 150101
# ANCON --> 150102
# ATE --> 150103
# BARRANCO --> 150104
# BREÑA --> 150105
# CARABAYLLO --> 150106
# CHACLACAYO --> 150107
# CHORRILLOS --> 150108
# CIENEGUILLA --> 150109
# COMAS --> 150110
# EL AGUSTINO --> 150111
# INDEPENDENCIA --> 150112
# JESUS MARIA --> 150113
# LA MOLINA --> 150114
# LA VICTORIA --> 150115
# LINCE --> 150116
# LOS OLIVOS --> 150117
# LURIGANCHO --> 150118
# LURIN --> 150119
# MAGDALENA DEL MAR --> 150120
# MAGDALENA VIEJA (PUEBLO LIIBRE) --> 150121
# MIRAFLORES --> 150122
# PACHACAMAC --> 150123
# PUCUSANA --> 150124
# PUENTE PIEDRA --> 150125
# PUNTA HERMOSA --> 150126
# PUNTA NEGRA --> 150127
# RIMAC --> 150128
# SAN BARTOLO --> 150129
# SAN BORJA --> 150130
# SAN ISIDRO --> 150131
# SAN JUAN DE LURIGANCHO --> 150132
# SAN JUAN DE MIRAFLORES --> 150133
# SAN LUIS --> 150134
# SAN MARTIN DE PORRES --> 150135
# SAN MIGUEL --> 150136
# SANTA ANITA --> 150137
# SANTA MARIA DEL MAR --> 150138
# SANTA ROSA --> 150139
# SANTIAGO DE SURCO --> 150140
# SURQUILLO --> 150141
# VILLA EL SALVADOR --> 150142
# VILLA MARIA DEL TRIUNFO --> 150143

dtr_cod <- cbind(c("LIMA"  , "ANCON", "ATE",   "BARRANCO","BREÑA","CARABAYLLO",
                   "CHACLACAYO","CHORRILLOS","CIENEGUILLA","COMAS",
                   "EL AGUSTINO","INDEPENDENCIA","JESUS MARIA","LA MOLINA",
                   "LA VICTORIA","LINCE","LOS OLIVOS","LURIGANCHO","LURIN",
                   "MAGDALENA DEL MAR","PUEBLO LIIBRE",
                   "MIRAFLORES","PACHACAMAC","PUCUSANA","PUENTE PIEDRA",
                   "PUNTA HERMOSA","PUNTA NEGRA","RIMAC","SAN BARTOLO",
                   "SAN BORJA","SAN ISIDRO","SAN JUAN DE LURIGANCHO",
                   "SAN JUAN DE MIRAFLORES","SAN LUIS","SAN MARTIN DE PORRES",
                   "SAN MIGUEL","SANTA ANITA","SANTA MARIA DEL MAR",
                   "SANTA ROSA","SANTIAGO DE SURCO","SURQUILLO",
                   "VILLA EL SALVADOR","VILLA MARIA DEL TRIUNFO","CALLAO",
                   "BELLAVISTA","CARMEN DE LA LEGUA Y REYNOSO","LA PERLA","LA PUNTA","VENTANILLA","MI PERU"),
                 c(as.character(c(150101:150143)), as.character(c(070101:070107))))



### Autopistas en Lima ###
rutas <- read_sf(file.choose())
rutas_sf <-rutas %>% st_transform(32721)
View(rutas_sf)
str(rutas_sf)

rutas_sf_mod <- rutas_sf %>% st_cast("LINESTRING")

str(rutas_sf_mod)

View(rutas_sf)

plot(rutas_sf_mod$geom)


st_write(rutas_sf_mod, paste0(tempdir(), "/", "n.shp"))

write.csv(rutas_sf_mod, "rutas_modized.csv")

st_read(file.choose())


?write_sf
rutas[which(rutas$type == c("primary", "secondary")),]

plot(rutas_sf$geometry[which(rutas$type == c("primary", "secondary")),], remove = T, color = "red")


land_use <- read_sf(file.choose())
table(land_use$type)
View(land_use)

land_use_sf <-land_use %>% st_transform(32721)
View(rutas_sf)




#### Mapa de Lima ####
mapa <- rgdal::readOGR(
  paste0(file.choose())
)
slotNames(mapa)
names(mapa)
head(mapa)
View(mapa)

distritos_mapa <- mapa %>% 
  dplyr::filter(PROVINCIA == c("LIMA", "CALLAO")) %>% 
  select(DISTRITO, PROVINCIA, IDDIST)


mapa_L <- mapa[which(mapa$PROVINCIA == c("LIMA")),]
mapa_C <- mapa[which(mapa$PROVINCIA == c("CALLAO")),]
mapa_LM <- rbind(mapa_L,mapa_C)
View(mapa_LM)
colnames(mapa_LM)

mapa_LM_df <- fortify(model = mapa, region = "NATCODE") %>% View

ggplot() + geom_sf(maps_LM_sf,color = "black", 
                   aes(geometry), 
                   fill = "gray90",
                   color = "gray20") + 
  geom_sf(maps_LM_sf %>% filter(DISTRITO %in% c("MI PERU")), 
          aes(),
          fill = "green",
          alpha = 0.7,
          color = "black") +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks =  element_blank(),
    axis.title = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  ggtitle("Districts of the Metropolitan Area of\nLima and Callao", 
          subtitle = "Political Division")


### En Formato SF
maps <- read_sf(file.choose())
View(maps)
maps_sf <-maps %>% st_transform(32721)

maps_L_sf <- maps_sf[which(maps_sf$PROVINCIA == c("LIMA")),]
maps_C_sf <- maps_sf[which(maps_sf$PROVINCIA == c("CALLAO")),]
maps_LM_sf <- rbind(maps_L_sf,maps_C_sf)
View(maps_LM_sf)
colnames(maps_LM_sf)

plot(maps_LM_sf$geometry, reset = F)
plot(maps_LM_sf[which(maps_LM_sf$DISTRITO == "MI PERU"),]$geometry, 
     col = "green" , add = T)



nrow(maps_LM_sf)
colnames(maps_LM_sf)
names(maps_LM_sf)[names(maps_LM_sf) == 'IDDIST'] <- 'UBIGEO'
maps_LM_sf$UBIGEO <- as.character(maps_LM_sf$UBIGEO)

View(maps_LM_sf)


# CENSOS DATA Provicia de Lima####
# X15ATOMO_01 ###
X15ATOMO_01
View(X15ATOMO_01)
colnames(X15ATOMO_01)
 
colnames(X15ATOMO_01)[colnames(X15ATOMO_01) == "CUADRO Nº 1: POBLACIÓN CENSADA, POR ÁREA URBANA Y RURAL; Y SEXO, SEGÚN DISTRITO Y EDADES SIMPLES"] <- "DISTRITO"
colnames(X15ATOMO_01)[colnames(X15ATOMO_01) == "...2"] <- "T_Pop"
colnames(X15ATOMO_01)[colnames(X15ATOMO_01) == "...3"] <- "T_Pop_M"
colnames(X15ATOMO_01)[colnames(X15ATOMO_01) == "...4"] <- "T_Pop_F"
colnames(X15ATOMO_01)[colnames(X15ATOMO_01) == "...5"] <- "T_Pop_Urb"
colnames(X15ATOMO_01)[colnames(X15ATOMO_01) == "...6"] <- "T_Pop_Urb_M"           
colnames(X15ATOMO_01)[colnames(X15ATOMO_01) == "...7"] <- "T_Pop_Urb_F"
colnames(X15ATOMO_01)[colnames(X15ATOMO_01) == "...8"] <- "T_Pop_Rur"
colnames(X15ATOMO_01)[colnames(X15ATOMO_01) == "...9"] <- "T_Pop_Rur_M"
colnames(X15ATOMO_01)[colnames(X15ATOMO_01) == "...10"] <- "T_Pop_Rur_F"



POBLATION_PROVINCIA_DE_LIMA <- X15ATOMO_01 %>% 
  filter(str_detect(DISTRITO, "DISTRITO|PROVINCIA"))


POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "PROVINCIA DE LIMA"),]$DISTRITO <- "LIMA METROPOLITANA"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO LIMA"),]$DISTRITO <- "LIMA"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO ANCÓN"),]$DISTRITO <- "ANCON"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO ATE"),]$DISTRITO <- "ATE"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO BARRANCO"),]$DISTRITO <- "BARRANCO"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO BREÑA"),]$DISTRITO <- "BREÑA"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO CARABAYLLO"),]$DISTRITO <- "CARABAYLLO"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO CHORRILLOS"),]$DISTRITO <- "CHORRILLOS"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO CIENEGUILLA"),]$DISTRITO <- "CIENEGUILLA"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO CHACLACAYO"),]$DISTRITO <- "CHACLACAYO"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO COMAS"),]$DISTRITO <- "COMAS"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO EL AGUSTINO"),]$DISTRITO <- "COMAS"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO INDEPENDENCIA"),]$DISTRITO <- "INDEPENDENCIA"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO JESÚS MARÍA"),]$DISTRITO <- "JESUS MARIA"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO LA MOLINA"),]$DISTRITO <- "LA MOLINA"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO LA VICTORIA"),]$DISTRITO <- "LA VICTORIA"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO LINCE"),]$DISTRITO <- "LINCE"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO LOS OLIVOS"),]$DISTRITO <- "LOS OLIVOS"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO LURIGANCHO"),]$DISTRITO <- "LURIGANCHO"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO LURÍN"),]$DISTRITO <- "LURIN"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO MAGDALENA DEL MAR"),]$DISTRITO <- "MAGDALENA DEL MAR"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO PUEBLO LIBRE"),]$DISTRITO <- "PUEBLO LIBRE"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO MIRAFLORES"),]$DISTRITO <- "MIRAFLORES"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO PACHACÁMAC"),]$DISTRITO <- "PACHACAMAC"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO PUCUSANA"),]$DISTRITO <- "PUCUSANA"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO PUENTE PIEDRA"),]$DISTRITO <- "PUENTE PIEDRA"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO PUNTA HERMOSA"),]$DISTRITO <- "PUNTA HERMOSA"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO PUNTA NEGRA"),]$DISTRITO <- "PUNTA NEGRA"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO RÍMAC"),]$DISTRITO <- "RIMAC"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO SAN BARTOLO"),]$DISTRITO <- "SAN BARTOLO"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO SAN BORJA"),]$DISTRITO <- "SAN BORJA"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO SAN ISIDRO"),]$DISTRITO <- "SAN ISIDRO"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO SAN JUAN DE LURIGANCHO"),]$DISTRITO <- "SAN JUAN DE LURIGANCHO"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO SAN JUAN DE MIRAFLORES"),]$DISTRITO <- "SAN JUAN DE MIRAFLORES"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO SAN LUIS"),]$DISTRITO <- "SAN LUIS"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO SAN MARTÍN DE PORRES"),]$DISTRITO <- "SAN MARTIN DE PORRES"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO SAN MIGUEL"),]$DISTRITO <- "SAN MIGUEL"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO SANTA ANITA"),]$DISTRITO <- "SANTA ANITA"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO SANTA MARÍA DEL MAR"),]$DISTRITO <- "SANTA MARIA DEL MAR"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO SANTA ROSA"),]$DISTRITO <- "SANTA ROSA"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO SANTIAGO DE SURCO"),]$DISTRITO <- "SANTIAGO DE SURCO"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO SURQUILLO"),]$DISTRITO <- "SURQUILLO"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO VILLA EL SALVADOR"),]$DISTRITO <- "VILLA EL SALVADOR"
POBLATION_PROVINCIA_DE_LIMA[which(POBLATION_PROVINCIA_DE_LIMA ==
                                    "DISTRITO VILLA MARÍA DEL TRIUNFO"),]$DISTRITO <- "VILLA MARIA DEL TRIUNFO"

View(POBLATION_PROVINCIA_DE_LIMA)          
nrow(POBLATION_PROVINCIA_DE_LIMA)


#X15ATOMO_02 
X15ATOMO_02



#X15ATOMO_03
X15ATOMO_03


# CENSOS DATA Provicia Constitucional del Callao####
#X07TOMO_01

colnames(X07TOMO_01)[colnames(X07TOMO_01) == "CUADRO Nº 1: POBLACIÓN CENSADA, POR SEXO, SEGÚN DISTRITO Y EDADES SIMPLES"] <- "DISTRITO"
colnames(X07TOMO_01)[colnames(X07TOMO_01) == "...2"] <- "T_Pop"
colnames(X07TOMO_01)[colnames(X07TOMO_01) == "...3"] <- "T_Pop_M"
colnames(X07TOMO_01)[colnames(X07TOMO_01) == "...4"] <- "T_Pop_F"


POBLATION_PROVINCIA_CALLAO <- X07TOMO_01 %>% 
  filter(str_detect(DISTRITO, "DISTRITO|PROV."))

View(POBLATION_PROVINCIA_CALLAO)


POBLATION_PROVINCIA_CALLAO[which(POBLATION_PROVINCIA_CALLAO ==
                                    "DISTRITO CALLAO"),]$DISTRITO <- "CALLAO"
POBLATION_PROVINCIA_CALLAO[which(POBLATION_PROVINCIA_CALLAO ==
                                    "DISTRITO BELLAVISTA"),]$DISTRITO <- "BELLAVISTA"
POBLATION_PROVINCIA_CALLAO[which(POBLATION_PROVINCIA_CALLAO ==
                                    "DISTRITO CARMEN DE LA LEGUA REYNOSO"),]$DISTRITO <- "CARMEN DE LA LEGUA REYNOSO"
POBLATION_PROVINCIA_CALLAO[which(POBLATION_PROVINCIA_CALLAO ==
                                    "DISTRITO LA PERLA"),]$DISTRITO <- "LA PERLA"
POBLATION_PROVINCIA_CALLAO[which(POBLATION_PROVINCIA_CALLAO ==
                                    "DISTRITO LA PUNTA"),]$DISTRITO <- "LA PUNTA"
POBLATION_PROVINCIA_CALLAO[which(POBLATION_PROVINCIA_CALLAO ==
                                    "DISTRITO VENTANILLA"),]$DISTRITO <- "VENTANILLA"
POBLATION_PROVINCIA_CALLAO[which(POBLATION_PROVINCIA_CALLAO ==
                                    "DISTRITO MI PERÚ"),]$DISTRITO <- "MI PERU"


POBLATION_AMLC <- rbind(POBLATION_PROVINCIA_DE_LIMA[,-c(5:10)],POBLATION_PROVINCIA_CALLAO)

maps_AMLC_sf <- full_join(maps_LM_sf[,-c(1:4,7:10)], 
                          POBLATION_AMLC[,-c(3,4)], by = c("DISTRITO"))
View(maps_AMLC_sf)




# SHP_JICA ####

ZT_LIMA <- read_sf(file.choose())
View(ZT_LIMA)
colnames(ZT_LIMA)
colnames(ZT_LIMA)[colnames(ZT_LIMA) == "TRAFFICZON"] <- "ZONA_TRANSITO"
ZT_LIMA$ZONA_TRANSITO <- as.character(ZT_LIMA$ZONA_TRANSITO)
colnames(ZT_LIMA)[colnames(ZT_LIMA) == "DIST"] <- "DISTRITO"
plot(ZT_LIMA$geometry)




devtools::install_github("dkahle/ggmap")
devtools::install_github("hrbrmstr/ggalt")
library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(ggmap)
library(ggalt)

?get_openstreetmap
lima <-  geocode("the white house")

microzone <- tm_shape(ZT_LIMA) +
  tm_borders() + 
  labs( title = "AMLC divided in Microzones by JICA 2013")

districts <- tm_shape(maps_LM_sf) +
  tm_borders() + 
  labs( title = "AMLC divided in District")

tmap_arrange(microzone, districts)


ggplot( data = ZT_LIMA) + tm_borders() + 
  guides(fill = guide_legend(title = "GINI COEFFIENT INTERVALS")) + 
  labs( title = "Metropolitan Area of Lima and Callao\n classified by the Gini 
        Coefficient")

