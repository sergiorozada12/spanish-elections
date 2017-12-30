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

sp_elections <- readOGR(dsn = "shapefile", layer = "spain_elections")

###################################################################################################################

# VIF to understand the PP vote
pp_vif <- vif(lm(PP~child+young+adult+old+male+female+spanish+foreigners+unemployed+gdp+agriculture+industry+
                   construction+services+density_pop,data = sp_elections@data))

pp_vif <- vif(lm(PP~child+young+adult+old+male+female+spanish+unemployed+gdp+agriculture+industry+
                   construction+services+density_pop,data = sp_elections@data))

pp_vif <- vif(lm(PP~child+young+adult+male+female+spanish+unemployed+gdp+agriculture+industry+
                   construction+services+density_pop,data = sp_elections@data))

pp_vif <- vif(lm(PP~child+young+adult+male+female+spanish+unemployed+gdp+agriculture+industry+
                   construction+density_pop,data = sp_elections@data))

pp_vif <- vif(lm(PP~child+young+adult+female+spanish+unemployed+gdp+agriculture+industry+
                   construction+density_pop,data = sp_elections@data))

pp_vif <- vif(lm(PP~child+adult+female+spanish+unemployed+gdp+agriculture+industry+
                   construction+density_pop,data = sp_elections@data))

# VIF to understand the PSOE vote
psoe_vif <- vif(lm(PSOE~child+adult+female+spanish+unemployed+gdp+agriculture+industry+
                     construction+density_pop,data = sp_elections@data))

# VIF to understand the Podemos vote
podemos_vif <- vif(lm(Podemos~child+adult+female+spanish+unemployed+gdp+agriculture+industry+
                        construction+density_pop,data = sp_elections@data))

# VIF to understand the Ciudadanos vote
ciudadanos_vif <- vif(lm(Ciudadanos~child+adult+female+spanish+unemployed+gdp+agriculture+industry+
                           construction+density_pop,data = sp_elections@data))

# VIF to understand the trends vote
trends_vif <- vif(lm(right_left~child+adult+female+spanish+unemployed+gdp+agriculture+industry+
                       construction+density_pop,data = sp_elections@data))

# VIF to understand the new/old vote
new_vif <- vif(lm(new_old~child+adult+female+spanish+unemployed+gdp+agriculture+industry+
                    construction+density_pop,data = sp_elections@data))

###################################################################################################################
#Explore residuals of the whole model and some individual features

#These are the models
model <- lm(new_old~child+adult+female+spanish+unemployed+gdp+agriculture+industry+
              construction+density_pop,data = sp_elections@data)
modelGDP <- lm(new_old~gdp,data = sp_elections@data)
modelUnemployed <- lm(new_old~unemployed,data = sp_elections@data)
modelSpanish <- lm(new_old~spanish,data = sp_elections@data)
modelAgriculture <- lm(new_old~agriculture,data = sp_elections@data)
modelFemale <- lm(new_old~female,data = sp_elections@data)
modelAdult <- lm(new_old~adult,data = sp_elections@data)
modelDensity <- lm(new_old~density_pop,data = sp_elections@data)
modelChild <- lm(new_old~child,data = sp_elections@data)
modelIndustry <- lm(new_old~industry,data = sp_elections@data)
modelAdultAgriculture <- lm(new_old~adult+agriculture,data = sp_elections@data)
modelAdultAgricultureDensity <- lm(new_old~adult+agriculture+density_pop,data = sp_elections@data)

#These are the residuals
sp_elections@data$residTotal <- resid(model)
sp_elections@data$residGdp <- resid(modelGDP)
sp_elections@data$residUnemployed <- resid(modelUnemployed)
sp_elections@data$residSpanish <- resid(modelSpanish)
sp_elections@data$residAgriculture <- resid(modelAgriculture)
sp_elections@data$residFemale <- resid(modelFemale)
sp_elections@data$residAdult <- resid(modelAdult)
sp_elections@data$residDensity <- resid(modelDensity)
sp_elections@data$residChild <- resid(modelChild)
sp_elections@data$residIndustry <- resid(modelIndustry)
sp_elections@data$residAgricultureAdult <- resid(modelAdultAgriculture)
sp_elections@data$residAgricultureAdultDensity <- resid(modelAdultAgricultureDensity)

#Plot the residuals
tm_shape(sp_elections) + 
  tm_fill(col=c("residAdult","residAgricultureAdult","residAgricultureAdultDensity","residTotal"),style="cont",palette="RdBu", id="geo_label",size=0.2, title="") + 
  tm_facets(free.scales = FALSE)+
  tm_layout(
    frame=FALSE,
    title=c("Adult","Adult & Agriculture","Adult & Agriculture \n& Density of Population","Refined multivariate"),
    title.size=0.8,
    title.position = c("left", "top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=1,
    legend.text.size=0.6,
    legend.outside=FALSE)