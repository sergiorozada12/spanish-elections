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
#CHLOROPLETH MAPS

#Exploring spatial variations depending on each party: PP
tm_shape(sp_elections)+
  tm_fill(col="PP",style="cont",palette="Blues",size=0.2,id="geo_label",title="")+
  tm_layout(
    title="PP percentage of vote",
    title.snap.to.legend = TRUE,
    title.size=1,
    legend.text.size=0.8,
    title.position = c("left","top"),
    legend.position = c("left","top"),
    frame = FALSE,
    legend.outside=FALSE
  )

#Exploring spatial variations depending on each party: PSOE
tm_shape(sp_elections)+
  tm_fill(col="PSOE",style="cont",palette="Reds",size=0.2,id="geo_label",title="")+
  tm_layout(
    title="PSOE percentage of vote",
    title.snap.to.legend = TRUE,
    title.size=1,
    legend.text.size=0.8,
    title.position = c("left","top"),
    legend.position = c("left","top"),
    frame = FALSE,
    legend.outside=FALSE
  )

#Exploring spatial variations depending on each party: Podemos
tm_shape(sp_elections)+
  tm_fill(col="Podemos",style="cont",palette="BuPu",size=0.2,id="geo_label",title="")+
  tm_layout(
    title="Podemos percentage of vote",
    title.snap.to.legend = TRUE,
    title.size=1,
    legend.text.size=0.8,
    title.position = c("left","top"),
    legend.position = c("left","top"),
    frame = FALSE,
    legend.outside=FALSE
  )

#Exploring spatial variations depending on each party: Ciudadanos
tm_shape(sp_elections)+
  tm_fill(col="Ciudadanos",style="cont",palette="Oranges",size=0.2,id="geo_label",title="")+
  tm_layout(
    title="Ciudadanos percentage of vote",
    title.snap.to.legend = TRUE,
    title.size=1,
    legend.text.size=0.8,
    title.position = c("left","top"),
    legend.position = c("left","top"),
    frame = FALSE,
    legend.outside=FALSE
  )

#Exploring spatial variations depending on each party: Others
tm_shape(sp_elections)+
  tm_fill(col="other",style="cont",palette="Greens",size=0.2,id="geo_label",title="")+
  tm_layout(
    title="Others percentage of vote",
    title.snap.to.legend = TRUE,
    title.size=1,
    legend.text.size=0.8,
    title.position = c("left","top"),
    legend.position = c("left","top"),
    frame = FALSE,
    legend.outside=FALSE
  )

#Exploring spatial variations depending on each party: Right and left
tm_shape(sp_elections)+
  tm_fill(col="right_left",style="cont",palette="RdBu",size=0.2,id="geo_label",title="")+
  tm_layout(
    title="Right and Left votes",
    title.snap.to.legend = TRUE,
    title.size=1,
    legend.text.size=0.8,
    title.position = c("left","top"),
    legend.position = c("left","top"),
    frame = FALSE,
    legend.outside=FALSE
  )

#Exploring spatial variations depending on each party: New and old parties
tm_shape(sp_elections)+
  tm_fill(col="new_old",style="cont",palette="RdPu",size=0.2,id="geo_label",title="")+
  tm_layout(
    title="New parties and old parties votes",
    title.snap.to.legend = TRUE,
    title.size=1,
    legend.text.size=0.8,
    title.position = c("left","top"),
    legend.position = c("left","top"),
    frame = FALSE,
    legend.outside=FALSE
  )

################################################################################################################
#BAR CHARTS

# Create ordered bar chart of result ordered by LA.
PP_data <- sort(sp_elections@data$PP, decreasing = TRUE)
PSOE_data <- sort(sp_elections@data$PSOE, decreasing = TRUE)
Podemos_data <- sort(sp_elections@data$Podemos, decreasing = TRUE)
Ciudadanos_data <- sort(sp_elections@data$Ciudadanos, decreasing = TRUE)
Right <- sort((sp_elections@data$PP+sp_elections@data$Ciudadanos)/(sp_elections@data$PP+sp_elections@data$Ciudadanos+sp_elections@data$Podemos+sp_elections@data$PSOE), decreasing = TRUE)
Old <- sort((sp_elections@data$PP+sp_elections@data$PSOE)/(sp_elections@data$PP+sp_elections@data$Ciudadanos+sp_elections@data$Podemos+sp_elections@data$PSOE), decreasing = TRUE)
index = c(1:52)

dataParties = data.frame(index,PP_data,PSOE_data,Podemos_data,Ciudadanos_data)
dataRightLeft = data.frame(index,Right,Left = 1-Right)
dataRightLeft <- melt(dataRightLeft,id.vars = "index")
dataOldNew = data.frame(index,Traditional = Old,New = 1-Old)
dataOldNew <- melt(dataOldNew,id.vars = "index")

colnames(dataRightLeft) <- c("index","Political trend","Percentage")
colnames(dataOldNew) <- c("index","Political trend","Percentage")

#Stacked votes by political party
dataParties %>%
  ggplot()+
  geom_bar(aes(x=index, y=PP_data, fill = "#328cd8"),stat="identity", width=1) +
  geom_bar(aes(x=index, y=PSOE_data,  fill = "#E74C3C"),stat="identity", width=1 ) +
  geom_bar(aes(x=index, y=Podemos_data, fill = "#8E44AD"),stat="identity", width=1 ) +
  geom_bar(aes(x=index, y=Ciudadanos_data, fill = "#ffa320" ),stat="identity", width=1) +
  scale_fill_manual(name="Parties",values=c("#328cd8","#8E44AD","#E74C3C","#ffa320"),labels=c("PP","Podemos","PSOE","Ciudadanos"))+
  theme_classic()+
  xlab("Provinces")+
  ylab("Stacked ordered vote")

#Stacked votes by right or left
dataRightLeft %>%
  ggplot(aes(x = index, y = Percentage, fill=`Political trend`)) +
  geom_bar(stat='identity')+
  theme_classic()+
  scale_fill_manual(values=c("#b0b9ff","#e92325"))+
  xlab("Provinces")+
  ylab("Stacked ordered vote for left and right")

#Stacked votes by new and old
dataOldNew %>%
  ggplot(aes(x = index, y = Percentage, fill=`Political trend`)) +
  geom_bar(stat='identity')+
  theme_classic()+
  scale_fill_manual(values=c("#f0e7bc","#834545"))+
  xlab("Provinces")+
  ylab("Stacked ordered vote for traditional and new parties")
#####################################################################################################################
#HISTOGRAMS OF THE VARIABLES

#Melt data from parties
dataPartiesMelt <- melt(dataParties,id.vars = "index")
colnames(dataPartiesMelt) <- c("index","Parties","Percentage")

#Histogram of all parties
dataPartiesMelt %>%
  ggplot(aes(Percentage,fill = Parties))+
  geom_histogram(position = "stack", binwidth=0.05)+
  scale_fill_manual(values=c("#328cd8","#E74C3C","#8E44AD","#ffa320"))+
  theme_classic()+
  xlab("Percentage of vote")+
  ylab("Number of regions voting a given percentage")

#Histograms of left and right
dataRightLeft %>%
  ggplot(aes(Percentage,fill = `Political trend`))+
  geom_histogram(position = "stack", binwidth=0.03)+
  scale_fill_manual(values=c("#b0b9ff","#e92325"))+
  theme_classic()+
  xlab("Percentage of vote")+
  ylab("Number of regions voting a given percentage")

#Histograms of new and traditional
dataOldNew %>%
  ggplot(aes(Percentage,fill = `Political trend`))+
  geom_histogram(position = "stack", binwidth=0.03)+
  scale_fill_manual(values=c("#f0e7bc","#834545"))+
  theme_classic()+
  xlab("Percentage of vote")+
  ylab("Number of regions voting a given percentage")

####################################################################################################################
#CORRELATION OF THE VARIABLES

correlations= abs(round(cor(sp_elections@data[c(6:30)],method = "pearson"),2))
correlations[upper.tri(correlations)] <- NA
corr_melt <- melt(correlations,na.rm = TRUE)
colnames(corr_melt) <- c("Variables A","Variables B","Pearson correlation")

#Plot the correlations heatmap
corr_melt %>%
  ggplot(aes(x=`Variables A`,y=`Variables B`,fill=`Pearson correlation`))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "white", high = "red", limit = c(0,1), space = "Lab", name="Pearson\nCorrelation\n")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))

####################################################################################################################
#SCATTERS OF MULTIPLES VARIABLES

#small multiples of PP vote
sp_elections@data %>%
  filter(Texto!="Ceuta")%>%
  filter(Texto!="Melilla")%>%
  gather(c(child:unemployed,density_pop,Pop), key = "expl_var", value="Variable") %>%
  ggplot(aes(x=Variable, y=PP))+
  geom_point(aes(colour=PP))+
  stat_smooth(method=lm, se=FALSE, size=1,colour="grey")+
  facet_wrap(~expl_var, scales="free")+
  scale_colour_gradient2(low = "white", high = "blue",limit = c(0,0.6))+
  theme_bw()

#small multiples of PSOE vote
sp_elections@data %>%
  filter(Texto!="Ceuta")%>%
  filter(Texto!="Melilla")%>%
  gather(c(Pop,child:unemployed,density_pop,Pop), key = "expl_var", value="Variable") %>%
  ggplot(aes(x=Variable, y=PSOE))+
  geom_point(aes(colour=PSOE))+
  stat_smooth(method=lm, se=FALSE, size=1,colour="grey")+
  scale_colour_gradient2(low = "white", high = "red",limit = c(0,0.4))+
  facet_wrap(~expl_var, scales="free")+
  theme_bw()

#small multiples of Podemos vote
sp_elections@data %>%
  filter(Texto!="Ceuta")%>%
  filter(Texto!="Melilla")%>%
  gather(c(Pop,child:unemployed,density_pop,Pop), key = "expl_var", value="Variable") %>%
  ggplot(aes(x=Variable, y=Podemos))+
  geom_point(aes(colour=Podemos))+
  stat_smooth(method=lm, se=FALSE, size=1,colour="grey")+
  scale_colour_gradient2(low = "white", high = "purple",limit = c(0,0.35))+
  facet_wrap(~expl_var, scales="free")+
  theme_bw()

#small multiples of Ciudadanos vote
sp_elections@data %>%
  filter(Texto!="Ceuta")%>%
  filter(Texto!="Melilla")%>%
  gather(c(Pop,child:unemployed,density_pop,Pop), key = "expl_var", value="Variable") %>%
  ggplot(aes(x=Variable, y=Ciudadanos))+
  geom_point(aes(colour=Ciudadanos))+
  stat_smooth(method=lm, se=FALSE, size=1,colour="grey")+
  scale_colour_gradient2(low = "white", high = "orange",limit = c(0,0.18))+
  facet_wrap(~expl_var, scales="free")+
  theme_bw()

#small multiples of right and left vote
sp_elections@data %>%
  filter(Texto!="Ceuta")%>%
  filter(Texto!="Melilla")%>%
  gather(c(Pop,child:unemployed,density_pop,Pop), key = "expl_var", value="Variable") %>%
  ggplot(aes(x=Variable, y=right_left))+
  geom_point(aes(colour=right_left))+
  stat_smooth(method=lm, se=FALSE, size=1, colour="grey")+
  scale_colour_gradient2(low = "red",mid = "white", high = "blue",limit = c(-0.2,0.2))+
  facet_wrap(~expl_var, scales="free")+
  theme_bw()

#small multiples of new and old parties vote
sp_elections@data %>%
  filter(Texto!="Ceuta")%>%
  filter(Texto!="Melilla")%>%
  gather(c(Pop,child:unemployed,density_pop,Pop), key = "expl_var", value="Variable") %>%
  ggplot(aes(x=Variable, y=new_old))+
  geom_point(aes(colour=new_old))+
  stat_smooth(method=lm, se=FALSE, size=1, colour="grey")+
  scale_colour_gradient2(low = "white", high = "magenta",limit = c(0,0.5))+
  facet_wrap(~expl_var, scales="free")+
  theme_bw()