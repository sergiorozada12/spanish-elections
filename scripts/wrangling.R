#Spatial data handling
library(rgdal)
library(spdep)
library(rgeos)
#For charting
library(tmap)
library(cartogram)
library(ggplot2)
library(gridExtra)
library(GGally)
#For data loading and munging
library(readr)
library(dplyr)
library(tidyr)
#For spatial stats
library(GWmodel)
library(spdep)
#For cluster analysis
library(cluster)
#For reshape datasets
library(reshape2)
#VIF library
library(car)

######################################################################################################################
#PREPARING THE DATA

#Read the dataset with info about local authorities and group by province
sp_municipios <- read_csv("./data/spainALL.csv")
sp_municipios <- sp_municipios[-c(1,2,4,5,7)]

sp_regiones <- sp_municipios %>%
  group_by(CP) %>%
  summarise(
    Pop = sum(Population),
    total_votes = sum(TotalVotes),
    PP = sum(PP_Ptge*TotalVotes/100)/sum(TotalVotes),
    PSOE = sum(PSOE_Ptge*TotalVotes/100)/sum(TotalVotes),
    Ciudadanos = sum(Ciudadanos_Ptge*TotalVotes/100)/sum(TotalVotes),
    Podemos = sum(Podemos_Ptge*TotalVotes/100)/sum(TotalVotes),
    other = sum(Others_Ptge*TotalVotes/100)/sum(TotalVotes),
    child = sum(`Age_0-4_Ptge`*Population/100,`Age_5-9_Ptge`*Population/100,`Age_10-14_Ptge`*Population/100)/
      sum(Population),
    young = sum(`Age_15-19_Ptge`*Population/100,`Age_20-24_Ptge`*Population/100,`Age_25-29_Ptge`*Population/100)/
      sum(Population),
    adult = sum(`Age_30-34_Ptge`*Population/100,`Age_35-39_Ptge`*Population/100,`Age_40-44_Ptge`*Population/100,
                `Age_45-49_Ptge`*Population/100,`Age_50-54_Ptge`*Population/100,`Age_55-59_Ptge`*Population/100)/
      sum(Population),
    old = sum(`Age_60-64_Ptge`*Population/100,`Age_65-69_Ptge`*Population/100,`Age_70-74_Ptge`*Population/100,
              `Age_75-79_Ptge`*Population/100,`Age_80-84_Ptge`*Population/100,`Age_85-89_Ptge`*Population/100,
              `Age_90-94_Ptge`*Population/100,`Age_95-99_Ptge`*Population/100,`Age_100+_Ptge`*Population/100)/
      sum(Population),
    male = sum(ManPopulationPtge*Population/100)/sum(Population),
    female = sum(WomanPopulationPtge*Population/100)/sum(Population),
    spanish = sum(SpanishPtge*Population/100)/sum(Population),
    foreigners = sum(ForeignersPtge*Population/100)/sum(Population),
    Area = sum(Superficie)
  )

#Orense, Pontevedra, la Coruña y Lugo
sp_regiones$Podemos[32] = 0.15
sp_regiones$other[32] = sp_regiones$other - 0.15
sp_regiones$Podemos[27] = 0.16
sp_regiones$other[27] = sp_regiones$other[27]-0.16
sp_regiones$Podemos[15] = 0.23
sp_regiones$other[15] = sp_regiones$other[15]-0.23
sp_regiones$Podemos[36] = 0.25
sp_regiones$other[36] = sp_regiones$other[36]-0.25

#Valencia, Castellon y Alicante
sp_regiones$Podemos[46] = 0.27
sp_regiones$other[46] = sp_regiones$other[46]-0.27
sp_regiones$Podemos[3] = 0.22
sp_regiones$other[3] = sp_regiones$other[3]-0.22
sp_regiones$Podemos[12] = 0.24
sp_regiones$other[12] = sp_regiones$other[12]-0.24

#Cataluña
sp_regiones$Podemos[8] = 0.16
sp_regiones$other[8] = sp_regiones$other - 0.16
sp_regiones$Podemos[17] = 0.26
sp_regiones$other[17] = sp_regiones$other[27]-0.26
sp_regiones$Podemos[25] = 0.25
sp_regiones$other[25] = sp_regiones$other[15]-0.25
sp_regiones$Podemos[43] = 0.19
sp_regiones$other[43] = sp_regiones$other[36]-0.19

#Read the gdp data and join the data
sp_gdp <- read_csv("./data/spainGdp.csv")
sp_gdp <- sp_gdp[-c(2)]
sp_regiones <- inner_join(sp_regiones,sp_gdp,by=c("CP"="codigo"))

#Read the sectors data and join the data
sp_sectors <- read_csv("./data/spainSectors.csv")
sp_sectors <- sp_sectors[-c(2)]
colnames(sp_sectors) <- c("Codigo", "agriculture","industry","construction","services","unemployed")
sp_sectors$Codigo <- as.integer(sp_sectors$Codigo)
sp_sectors$agriculture <- sp_sectors$agriculture/100
sp_sectors$industry <- sp_sectors$industry/100
sp_sectors$construction <- sp_sectors$construction/100
sp_sectors$services <- sp_sectors$services/100
sp_sectors$unemployed <- sp_sectors$unemployed/100
sp_regiones <- inner_join(sp_regiones,sp_sectors,by=c("CP"="Codigo"))

#Read the shapefile with the Spain boundaries and add the previous data
sp_boundaries <- readOGR(dsn = "shapefile", layer = "Provincias_ETRS89_30N")
#Set the coordinates system
proj4string(sp_boundaries) <- CRS("+init=epsg:27700")
sp_boundaries$Codigo <- as.integer(sp_boundaries$Codigo)
sp_boundaries$Cod_CCAA <- as.integer(sp_boundaries$Cod_CCAA)
sp_boundaries@data <- inner_join(sp_boundaries@data,sp_regiones,by=c("Codigo"="CP"))
sp_elections <- sp_boundaries

#We add now two more columns
sp_elections@data <- sp_elections@data %>%
  mutate(density_pop = Pop/Area,
         right_left = ((PP + Ciudadanos)/(PP+PSOE+Ciudadanos+Podemos))-0.5,
         new_old = (Ciudadanos + Podemos))

#Save the data to a shapefile with all the data
writeOGR(dsn = "shapefile", layer = "spain_elections")

