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

#####################################################################################################################
#GEOGRAPHICALLY WEIGHTED STATISTICS

#Geographical variations by region of PP
gw_ss <- gwss(sp_elections, vars  =  c("PP","adult","agriculture","child","construction","density_pop","female",
                                       "gdp","industry","spanish","unemployed","services","old"),
              kernel = "bisquare", adaptive = TRUE, bw = 50, quantile = TRUE)

tm_shape(gw_ss$SDF) +
  tm_fill(col=colnames(gw_ss$SDF@data[c("Corr_PP.adult","Corr_PP.agriculture","Corr_PP.child","Corr_PP.construction",
                                        "Corr_PP.density_pop","Corr_PP.female","Corr_PP.gdp","Corr_PP.industry",
                                        "Corr_PP.services","Corr_PP.spanish","Corr_PP.unemployed","Corr_PP.old")]),
          title="gwr coefficients", style="cont",palette="PRGn", size=0.2) + 
  tm_facets(free.scales = FALSE) +
  tm_layout(
    title=c("Adult","Agriculture","Child","Construction","Density Population","Female","GDP per capita",
            "Industry","Services","Spanish","Unemployed","Old"),
    frame=FALSE,
    title.snap.to.legend=FALSE,
    title.size=1,
    title.position = c("left", "top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=1,
    legend.text.size=0.6,
    legend.outside=FALSE)

#Geographical variations by region of PSOE
gw_ss <- gwss(sp_elections, vars  =  c("PSOE","adult","agriculture","child","construction","density_pop","female",
                                       "gdp","industry","spanish","unemployed","services","old"),
              kernel = "bisquare", adaptive = TRUE, bw = 50, quantile = TRUE)

tm_shape(gw_ss$SDF) +
  tm_fill(col=colnames(gw_ss$SDF@data[c("Corr_PSOE.adult","Corr_PSOE.agriculture","Corr_PSOE.child","Corr_PSOE.construction",
                                        "Corr_PSOE.density_pop","Corr_PSOE.female","Corr_PSOE.gdp","Corr_PSOE.industry",
                                        "Corr_PSOE.services","Corr_PSOE.spanish","Corr_PSOE.unemployed","Corr_PSOE.old")]),
          title="gwr coefficients", style="cont",palette="PRGn", size=0.2) + 
  tm_facets(free.scales = FALSE) +
  tm_layout(
    title=c("Adult","Agriculture","Child","Construction","Density Population","Female","GDP per capita",
            "Industry","Services","Spanish","Unemployed","Old"),
    frame=FALSE,
    title.snap.to.legend=FALSE,
    title.size=1,
    title.position = c("left", "top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=1,
    legend.text.size=0.6,
    legend.outside=FALSE)

#Geographical variations by region of Podemos
gw_ss <- gwss(sp_elections, vars  =  c("Podemos","adult","agriculture","child","construction","density_pop","female",
                                       "gdp","industry","spanish","unemployed","services","old"),
              kernel = "bisquare", adaptive = TRUE, bw = 50, quantile = TRUE)

tm_shape(gw_ss$SDF) +
  tm_fill(col=colnames(gw_ss$SDF@data[c("Corr_Podemos.adult","Corr_Podemos.agriculture","Corr_Podemos.child","Corr_Podemos.construction",
                                        "Corr_Podemos.density_pop","Corr_Podemos.female","Corr_Podemos.gdp","Corr_Podemos.industry",
                                        "Corr_Podemos.services","Corr_Podemos.spanish","Corr_Podemos.unemployed","Corr_Podemos.old")]),
          title="gwr coefficients", style="cont",palette="PRGn", size=0.2) + 
  tm_facets(free.scales = FALSE) +
  tm_layout(
    title=c("Adult","Agriculture","Child","Construction","Density Population","Female","GDP per capita",
            "Industry","Services","Spanish","Unemployed","Old"),
    frame=FALSE,
    title.snap.to.legend=FALSE,
    title.size=1,
    title.position = c("left", "top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=1,
    legend.text.size=0.6,
    legend.outside=FALSE)

#Geographical variations by region of Ciudadanos
gw_ss <- gwss(sp_elections, vars  =  c("Ciudadanos","adult","agriculture","child","construction","density_pop","female",
                                       "gdp","industry","spanish","unemployed","services","old"),
              kernel = "bisquare", adaptive = TRUE, bw = 50, quantile = TRUE)

tm_shape(gw_ss$SDF) +
  tm_fill(col=colnames(gw_ss$SDF@data[c("Corr_Ciudadanos.adult","Corr_Ciudadanos.agriculture","Corr_Ciudadanos.child","Corr_Ciudadanos.construction",
                                        "Corr_Ciudadanos.density_pop","Corr_Ciudadanos.female","Corr_Ciudadanos.gdp","Corr_Ciudadanos.industry",
                                        "Corr_Ciudadanos.services","Corr_Ciudadanos.spanish","Corr_Ciudadanos.unemployed","Corr_Ciudadanos.old")]),
          title="gwr coefficients", style="cont",palette="PRGn", size=0.2) + 
  tm_facets(free.scales = FALSE) +
  tm_layout(
    title=c("Adult","Agriculture","Child","Construction","Density Population","Female","GDP per capita",
            "Industry","Services","Spanish","Unemployed","Old"),
    frame=FALSE,
    title.snap.to.legend=FALSE,
    title.size=1,
    title.position = c("left", "top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=1,
    legend.text.size=0.6,
    legend.outside=FALSE)

#Geographical variations by region of right and left
gw_ss <- gwss(sp_elections, vars  =  c("right_left","adult","agriculture","child","construction","density_pop","female",
                                       "gdp","industry","spanish","unemployed","services","old"),
              kernel = "bisquare", adaptive = TRUE, bw = 50, quantile = TRUE)

tm_shape(gw_ss$SDF) +
  tm_fill(col=colnames(gw_ss$SDF@data[c("Corr_right_left.adult","Corr_right_left.agriculture","Corr_right_left.child","Corr_right_left.construction",
                                        "Corr_right_left.density_pop","Corr_right_left.female","Corr_right_left.gdp","Corr_right_left.industry",
                                        "Corr_right_left.services","Corr_right_left.spanish","Corr_right_left.unemployed","Corr_right_left.old")]),
          title="gwr coefficients", style="cont",palette="PRGn", size=0.2) + 
  tm_facets(free.scales = FALSE) +
  tm_layout(
    title=c("Adult","Agriculture","Child","Construction","Density Population","Female","GDP per capita",
            "Industry","Services","Spanish","Unemployed","Old"),
    frame=FALSE,
    title.snap.to.legend=FALSE,
    title.size=1,
    title.position = c("left", "top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=1,
    legend.text.size=0.6,
    legend.outside=FALSE)

#Geographical variations by region of new and old
gw_ss <- gwss(sp_elections, vars  =  c("new_old","adult","agriculture","child","construction","density_pop","female",
                                       "gdp","industry","spanish","unemployed","services","old"),
              kernel = "bisquare", adaptive = TRUE, bw = 50, quantile = TRUE)

tm_shape(gw_ss$SDF) +
  tm_fill(col=colnames(gw_ss$SDF@data[c("Corr_new_old.adult","Corr_new_old.agriculture","Corr_new_old.child","Corr_new_old.construction",
                                        "Corr_new_old.density_pop","Corr_new_old.female","Corr_new_old.gdp","Corr_new_old.industry",
                                        "Corr_new_old.services","Corr_new_old.spanish","Corr_new_old.unemployed","Corr_new_old.old")]),
          title="gwr coefficients", style="cont",palette="PRGn", size=0.2) + 
  tm_facets(free.scales = FALSE) +
  tm_layout(
    title=c("Adult","Agriculture","Child","Construction","Density Population","Female","GDP per capita",
            "Industry","Services","Spanish","Unemployed","Old"),
    frame=FALSE,
    title.snap.to.legend=FALSE,
    title.size=1,
    title.position = c("left", "top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=1,
    legend.text.size=0.6,
    legend.outside=FALSE)