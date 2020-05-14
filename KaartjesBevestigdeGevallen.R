library(rgdal)
library(sp)
library(rgeos)
library(stringr)
library(ggplot2)
library(maptools)
library(plyr)
library(dplyr)
library(sf)


gem <- readOGR(dsn="C:/Rscripts/Corona",
               layer="GemeentenBelgie2020")
gems <- gSimplify(gem, tol=10, topologyPreserve = TRUE)
gem <- SpatialPolygonsDataFrame(gems, data=gem@data)
rm(gems)

Bevolking <- read.csv("C:/Rscripts/Corona/Inwoners.csv", sep=";")
colnames(Bevolking)[1] <- "NSI"

#Laden dataset bevestigde gevallen
LaatsteDag <- read.csv("C:/Rscripts/Corona/200511.csv", sep="", stringsAsFactors=FALSE)
LaatsteDag <- LaatsteDag[,c(1,2,14,18)]
LaatsteDag$NIS5.<- str_remove_all(LaatsteDag$NIS5., fixed(","))
LaatsteDag$CASES <- str_remove_all(LaatsteDag$CASES, fixed("<"))

LaatsteDag$CASES <- as.numeric(LaatsteDag$CASES)
LaatsteDag$CASES[is.na(LaatsteDag$CASES)] <-  LaatsteDag$TX_RGN_DESCR_NL[is.na(LaatsteDag$CASES)]
LaatsteDag$CASES <- as.numeric(LaatsteDag$CASES)

VoorlaatsteDag <- read.csv("C:/Rscripts/Corona/200504.csv", sep="", stringsAsFactors=FALSE)
VoorlaatsteDag <- VoorlaatsteDag[,c(1,2,14,18)]
VoorlaatsteDag$NIS5.<- str_remove_all(VoorlaatsteDag$NIS5., fixed(","))
VoorlaatsteDag$CASES <- str_remove_all(VoorlaatsteDag$CASES, fixed("<"))

VoorlaatsteDag$CASES <- as.numeric(VoorlaatsteDag$CASES)
VoorlaatsteDag$CASES[is.na(VoorlaatsteDag$CASES)] <-  VoorlaatsteDag$TX_RGN_DESCR_NL[is.na(VoorlaatsteDag$CASES)]
VoorlaatsteDag$CASES <- as.numeric(VoorlaatsteDag$CASES)

#VerschilLaatsteDag
LaatsteDag$NieuweGevallen <- LaatsteDag$CASES - VoorlaatsteDag$CASES
LaatsteDag <- LaatsteDag[complete.cases(LaatsteDag),]
colnames(LaatsteDag)[1] <- "NSI"

#Laden dataset met gemeenten
gemeenten <- st_read("GemeentenBelgie2020.shp")

#Koppel Data aan kaart
gemeenten_merged <- merge(gemeenten, LaatsteDag, by="NSI")
gemeenten_merged <- merge(gemeenten_merged, Bevolking[,c(1,3)], by="NSI")

gemeenten_merged$Inwoners <- as.character(gemeenten_merged$Inwoners)
gemeenten_merged$Inwoners <- str_replace_all(gemeenten_merged$Inwoners, fixed("."), "")
gemeenten_merged$Inwoners <- as.numeric(gemeenten_merged$Inwoners)
#gemeenten_merged$Casesper100K <- gemeenten_merged$CASES/gemeenten_merged$Inwoners * 100000
gemeenten_merged$Casesper100K <- gemeenten_merged$NieuweGevallen/gemeenten_merged$Inwoners * 100000

#palette 
# library(RColorBrewer)
# pal <- brewer.pal(9, "Reds")


# PlotNieuw <- ggplot(gemeenten_merged)+
#   geom_sf(aes(fill=NieuweGevallen))+
#   scale_fill_gradient(
#     low="white", high="darkred", 
#     name="# Nieuwe Gevallen")+
#   ggtitle("Nieuwe Covid-19 gevallen 13 mei 2020")+
#   theme(text=element_text(size=24))
# 
# 
# png("Kaartjes/2005103_Nieuw.png", width = 1200, height = 1200)
# plot(PlotNieuw)
# dev.off()
# 
# PlotTotaal <- ggplot(gemeenten_merged)+
#   geom_sf(aes(fill=CASES))+
#   scale_fill_gradient(
#     low="white", high="darkblue", 
#     name="# Totale gevallen")+
#   ggtitle("Totaal Covid-19 gevallen 13 mei 2020")+
#   theme(text=element_text(size=24))
# 
# png("Kaartjes/2005103_Totaal.png", width = 1200, height = 1200)
# plot(PlotTotaal)
# dev.off()

PlotNieuwper100K <- ggplot(gemeenten_merged)+
  geom_sf(aes(fill=Casesper100K))+
  #scale_fill_gradientn(limits = c(-25,600),
    #colours = c("white", "DarkRed"),
  scale_fill_gradient(low="white", high="DarkRed", 
    name="Nieuwe gevallen per 100k inwoners")+
  ggtitle("Nieuwe Covid-19 gevallen per 100.000 inwoners - week 4/5/20-10/5/20")+
  theme(text=element_text(size=24))

png("Kaartjes/week19_NieuwPer100k.png", width = 1500, height = 1200)
plot(PlotNieuwper100K)
dev.off()
