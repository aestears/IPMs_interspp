#/////////////////////////
# Generate Demographic Data
# Alice Stears
# 7 April 2023
#/////////////////////////

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(plantTracker)
library(sf)

# Read in Data ------------------------------------------------------------
## note: all previously published data 
# read in d.f that contains all shapefiles (Megashape), saved as an .rdata object
BigShape_folder <- "../../Grad School/Research/Trait Project/Data/QuadratShapefiles/"
#megaShape <- st_read(dsn = "/Users/Alice/Dropbox/Grad School/Research/Trait Project/Data/QuadratShapefiles/MegaShape.gpkg")
load(paste0(BigShape_folder,"MegaShapeEnvironment.RData"))

megaShape <- bigShape4
# make sure it is in the correct sf format
megaShape <- st_as_sf(megaShape)
#names(megaShape)[11] <- "geometry"
#st_geometry(megaShape) <- "geometry"
# fix some 'invalid' geometries
megaShape <- st_make_valid(megaShape)
# remove rows that don't have species name info
megaShape <- megaShape[!is.na(megaShape$Species),]
# correct year format
megaShape$Year <- as.integer(megaShape$Year)
# for years where only two last digits are stored... add 1900
megaShape[megaShape$Year < 1900,]$Year <- megaShape[megaShape$Year < 1900,]$Year + 1900
# remove data for things that aren't plants
megaShape <- megaShape[!(megaShape$Species %in% c("ant hill", "Ant hill", "bare ground",
                                                  "Corner plate", "Cow pie", "Crown", "Bare ground", "Unknown Moss", "Unknown Weed", "Unknown Lichen", "Fragment", "Cymopterus spp.","Stipa spp.","Malva spp.","Cirsium spp.", "Viola spp.","Carduus spp.","Unknown Perennial", "Andropogon spp.", "Dichanthelium spp.", "depression", "dung", "fragment", "Mixed grass", "Moss", "Mushroom", "Short grass", "Unknown", "unknown", "Unknown Cactus", "unknown cactus", "Unknown forb", "Unknown Forb", "unknown forb", "Unknown Grass", "unknown grass", "Unknown grass", "unknown lichen", "unknown moss", "unknown perennial", "Unknown Seedling", "unknown seedling", "Unknown Shrub", "Unknown Shrub Point", "unknown weed")),]

# make quadrat inventory list
invTemp <- unique(st_drop_geometry(megaShape[,c( "Quad", "Year")]))
names(invTemp)[2] <- "Year_name"
invTemp$Year_value <- invTemp$Year
invTemp <- invTemp[order(invTemp$Year_name),]
invTemp <- pivot_wider(invTemp, names_from = Quad, values_from = "Year_value")
invTemp <- invTemp[,2:ncol(invTemp)]
inv <- lapply(X = invTemp, FUN = function(x) x[!is.na(x)])

# Use plantTracker::trackSpp to get demographic data ----------------------------------------------------------------
# check the data w/ checkDat
checkDat(dat = megaShape, inv = inv)

# clonal argument data.frame
clonalDF <- read.csv("../clonalDF.csv")

#### AZs data ####
# from doi: Cover and density of semi-desert grassland plants in permanent quadrats mapped from 1915 to 1947, Anderson, McClaran and Adler 2012; 10.1126/science.127.3295.414-b
tempDat <- megaShape[megaShape$Site == "AZs" ,]
tempDat <- tempDat[!duplicated(tempDat),]
AZs_demo_dorm1_buff5_buffG05 <- trackSpp(dat = tempDat, inv = inv, dorm = 1, buff = .05, buffGenet = .005, clonal = clonalDF, aggByGenet = FALSE)
## aggregate by genet, but have to do it in a loop because the dataset is too large??
for (i in 1:length(unique(tempDat$Quad))) {
  temp <- AZs_demo_dorm1_buff5_buffG05[tempDat$Quad == unique(tempDat$Quad)[i],]
  tempAgg <- aggregateByGenet(dat = temp)
  if (i == 1) {
    AZs_demo_dorm1_buff5_buffG05_aggregated <- tempAgg
  } else {
    AZs_demo_dorm1_buff5_buffG05_aggregated <- rbind(tempAgg, AZs_demo_dorm1_buff5_buffG05_aggregated)
  }
}

#data folder name
data_folder <- "./Data/"
saveRDS(AZs_demo_dorm1_buff5_buffG05_aggregated, file = paste0(data_folder, "AZs_buff5_dorm1_demoDat.RDS"))

#### CO data ####
## from Cover, density, and demographics of shortgrass steppe plants mapped 1997–2010 
# in permanent grazed and ungrazed quadrats; Chu, Norman, Flynn, Kaplan,  Lauenroth, 
# and Adler, 2013; https://doi.org/10.1890/13-0121.1

tempDat <- megaShape[megaShape$Site == "CO" ,]
# remove a duplicate
tempDat <- tempDat[!duplicated(tempDat),]
CO_demo_dorm1_buff5_buffG05 <- trackSpp(dat = tempDat, inv = inv, dorm = 1, buff = .05, buffGenet = .005, clonal = clonalDF, aggByGenet = FALSE)

# remove the observations that have an area of '0'
badTrackIDs <- CO_demo_dorm1_buff5_buffG05[st_area(CO_demo_dorm1_buff5_buffG05)==0 ,]$trackID
test <- CO_demo_dorm1_buff5_buffG05[!(CO_demo_dorm1_buff5_buffG05$trackID %in% badTrackIDs),]

# memory is exhausted when doing it all at once, so loop through
for (i in 1:length(unique(test$Quad))) {
  temp <- test[test$Quad == unique(test$Quad)[i],]
  tempAgg <- aggregateByGenet(dat = temp)
  if (i == 1) {
    CO_demo_dorm1_buff5_buffG05_aggregated <- tempAgg
  } else {
    CO_demo_dorm1_buff5_buffG05_aggregated <- rbind(tempAgg,  CO_demo_dorm1_buff5_buffG05_aggregated)
  }
}

saveRDS(CO_demo_dorm1_buff5_buffG05_aggregated, file = paste0(data_folder, "../CO_buff5_dorm1_demoDat.RDS"))

#### ID data ####
##  from Mapped quadrats in sagebrush steppe: long-term data for analyzing demographic 
# rates and plant—plant interactions, Zachmann, Moffet and Adler, 2010.; DOI: 10.2307/20788177

tempDat <- megaShape[megaShape$Site == "ID" ,]
# remove a duplicate
tempDat <- tempDat[!duplicated(tempDat),]
ID_demo_dorm1_buff5_buffG05 <- trackSpp(dat = tempDat, inv = inv, dorm = 1, buff = .05, buffGenet = .005, clonal = clonalDF, aggByGenet = FALSE)

## aggregate by genet
for (i in 1:length(unique(tempDat$Quad))) {
  temp <- ID_demo_dorm1_buff5_buffG05[ID_demo_dorm1_buff5_buffG05$Quad == unique(tempDat$Quad)[i],]
  tempAgg <- aggregateByGenet(dat = temp)
  if (i == 1) {
    ID_demo_dorm1_buff5_buffG05_aggregated <- tempAgg
  } else {
    ID_demo_dorm1_buff5_buffG05_aggregated <- rbind(tempAgg, ID_demo_dorm1_buff5_buffG05_aggregated)
  }
}

saveRDS(ID_demo_dorm1_buff5_buffG05_aggregated, file = paste0(data_folder, "../ID_buff5_dorm1_demoDat.RDS"))

#### MT data ####
## from Fourteen years of mapped, permanent quadrats in a northern mixed prairie, USA, 
# Anderson, Vermeire and  Adler 2011; DOI: 10.2307/23034897

tempDat <- megaShape[megaShape$Site == "MT" ,]
# remove a duplicate
tempDat <- tempDat[!duplicated(tempDat),]
MT_demo_dorm1_buff5_buffG05 <- trackSpp(dat = tempDat, inv = inv, dorm = 1, buff = .05, buffGenet = .005, clonal = clonalDF, aggByGenet = FALSE)

## aggregate by genet
for (i in 1:length(unique(tempDat$Quad))) {
  temp <- MT_demo_dorm1_buff5_buffG05[MT_demo_dorm1_buff5_buffG05$Quad == unique(tempDat$Quad)[i],]
  tempAgg <- aggregateByGenet(dat = temp)
  if (i == 1) {
    MT_demo_dorm1_buff5_buffG05_aggregated <- tempAgg
  } else {
    MT_demo_dorm1_buff5_buffG05_aggregated <- rbind(tempAgg, MT_demo_dorm1_buff5_buffG05_aggregated)
  }
}

saveRDS(MT_demo_dorm1_buff5_buffG05_aggregated, file = paste0(data_folder, "../MT_buff5_dorm1_demoDat.RDS"))

#### KS data ####
## from LONG-TERM MAPPED QUADRATS FROM KANSAS PRAIRIE: DEMOGRAPHIC INFORMATION FOR 
# HERBACEOUS PLANTS, Adler, Tyburczy, and Lauenroth, 2007; https://doi.org/10.1890/0012-9658(2007)88[2673:LMQFKP]2.0.CO;2

tempDat <- megaShape[megaShape$Site == "KS" ,]
# remove a duplicate
tempDat <- tempDat[!duplicated(tempDat),]
KS_demo_dorm1_buff5_buffG05 <- trackSpp(dat = tempDat, inv = inv, dorm = 1, buff = .05, buffGenet = .005, clonal = clonalDF, aggByGenet = FALSE)

## aggregate by genet
for (i in 1:length(unique(tempDat$Quad))) {
  temp <- KS_demo_dorm1_buff5_buffG05[KS_demo_dorm1_buff5_buffG05$Quad == unique(tempDat$Quad)[i],]
  tempAgg <- aggregateByGenet(dat = temp)
  if (i == 1) {
    KS_demo_dorm1_buff5_buffG05_aggregated <- tempAgg
  } else {
    KS_demo_dorm1_buff5_buffG05_aggregated <- rbind(tempAgg,KS_demo_dorm1_buff5_buffG05_aggregated)
  }
}

saveRDS(KS_demo_dorm1_buff5_buffG05_aggregated, file = paste0(data_folder, "../KS_buff5_dorm1_demoDat.RDS"))

#### NM data ####
## from Quadrat‐based monitoring of desert grassland vegetation at the Jornada 
# Experimental Range, New Mexico, 1915–2016, Christensen, James, Maxwell, 
# Slaughter, Adler, Havstad and Bestelmeyer, 2021; 10.1002/ecy.3530

tempDat <- megaShape[megaShape$Site == "NM" ,] %>%
  mutate(Year = as.integer(Year), Quad = str_to_upper(Quad))
## change the appropriate years for the NM dataset
projectYears <- read.csv("../../cross_site_analysis/Jornada_quadrat_sampling_dates.csv") %>%
  select(-X) %>%
  mutate(quadrat = str_to_upper(quadrat))

tempDat_new <- tempDat %>%
  left_join(projectYears, by = c("Year" = "year", "Quad" = "quadrat", "Month" = "month")) %>%
  mutate(Year = project_year) %>%
  select(-c(day, project_year))

# rescale the geometry to be on a scale of 1m
tempDat_new$geometry <- tempDat_new$geometry/max(st_bbox(tempDat_new))
tempDat_new$Area <- st_area(tempDat_new$geometry)

# remove a duplicate
tempDat <- tempDat[!duplicated(tempDat),]
names(inv) <- str_to_upper(names(inv))
NM_demo_dorm1_buff5_buffG05 <- trackSpp(dat = tempDat, inv = inv, dorm = 1, buff = .05, buffGenet = .005, clonal = clonalDF, aggByGenet = FALSE)

## aggregate by genet
for (i in 1:length(unique(tempDat$Quad))) {
  temp <- NM_demo_dorm1_buff5_buffG05[NM_demo_dorm1_buff5_buffG05$Quad == unique(tempDat$Quad)[i],]
  tempAgg <- aggregateByGenet(dat = temp)
  if (i == 1) {
    NM_demo_dorm1_buff5_buffG05_aggregated <- tempAgg
  } else {
    NM_demo_dorm1_buff5_buffG05_aggregated <- rbind(tempAgg,NM_demo_dorm1_buff5_buffG05_aggregated)
  }
}

saveRDS(NM_demo_dorm1_buff5_buffG05_aggregated, file = paste0(data_folder, "../NM_buff5_dorm1_demoDat.RDS"))

#### AZn data ####
## from Cover and density of southwestern ponderosa pine understory plants in 
# permanent chart quadrats (2002‐2020), Moore, Jenness, Laughlin, Strahan, Bakker, 
# Dowling and Springer, 2022; 10.1002/ecy.3661

AZn_all <- readRDS("../../Data/ChartQuadDatasets/AZn_downloaded data/AZn_readyToGo.RDS")

# make quadrat inventory list
invTemp <- unique(st_drop_geometry(AZn_all[,c( "Quad", "Year")]))
names(invTemp)[2] <- "Year_name"
invTemp$Year_value <- invTemp$Year
invTemp <- invTemp[order(invTemp$Year_name),]
invTemp <- pivot_wider(invTemp, names_from = Quad, values_from = "Year_value")
invTemp <- invTemp[,2:ncol(invTemp)]
inv <- lapply(X = invTemp, FUN = function(x) x[!is.na(x)])

# there are some rows that have a geometry of 0... should remove these
AZn_all <- AZn_all[units::drop_units(st_area(AZn_all)) != 0,]

AZn_demo_dorm1_buff5_buffG05 <- trackSpp(dat = AZn_all, inv = inv,  dorm = 1, buff = .05, buffGenet = .005,
                                         clonal = data.frame("Species" = unique(AZn_all$Species),
                                                             "clonal" = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,1,1,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0))
                                         , aggByGenet = FALSE)

## aggregate by genet
for (i in 1:length(unique(AZn_all$Quad))) {
  temp <- AZn_demo_dorm1_buff5_buffG05[AZn_demo_dorm1_buff5_buffG05$Quad == unique(AZn_all$Quad)[i],]
  tempAgg <- aggregateByGenet(dat = temp)
  if (i == 1) {
    AZn_demo_dorm1_buff5_buffG05_aggregated <- tempAgg
  } else {
    AZn_demo_dorm1_buff5_buffG05_aggregated <- rbind(tempAgg,AZn_demo_dorm1_buff5_buffG05_aggregated)
  }
}

saveRDS(AZn_demo_dorm1_buff5_buffG05_aggregated, file = paste0(data_folder, "../AZn_buff5_dorm1_demoDat.RDS"))


