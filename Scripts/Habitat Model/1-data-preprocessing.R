## a269
## Sarah Chisholm 
##
## Data prep

## Workspace ----

# Set environment variable TZ when running on AWS EC2 instance
Sys.setenv(TZ='UTC')

# Load libraries
library(sf)
library(terra)
library(tidyverse)

# Define directories
spatialDataDir <- file.path("Data", "Spatial")
tabularDataDir <- file.path("Data", "Tabular")

# Parameters
# Target crs
targetCrs <- "epsg: 3005"

# Load data
# Spatial
# VRI shapefile clipped to study area
vri <- st_read(dsn = file.path(spatialDataDir, "VRI_Cariboo_Near.gdb"), 
               layer = "VRI_Clipped_Cariboo")

# Ownership layers
# Protected areas
protectedAreas <- st_read(dsn = file.path(spatialDataDir, "CPCAD-BDCAPC_Dec2021.gdb"),
                          layer = "CPCAD_BC_Dec2021")

# Indian reserves
indianReserves <- st_read(dsn = file.path(spatialDataDir, "FederalLandsInventory2019_Internal.gdb"),
                          layer = "CanadaLandsAdministrativeBoundary_IndianReserve") 

# CWS Federal Lands
cwsLands <- st_read(dsn = file.path(spatialDataDir, "FederalLandsInventory2019_Internal.gdb"),
                    layer = "CWS_FederalLandsInventory2019_Department")

# BC Parcels
bcParcels <- st_read(dsn = file.path(spatialDataDir, "Cleaned_ParcelMapBCExtract2021.gdb"),
                     layer = "pmbc_parcel_fabric_poly_svw2021")

# Historic Cutblocks
cutblocks <- st_read(dsn = file.path(spatialDataDir, "Consolidated_Cutblocks", "Consolidated_Cutblocks", "Consolidated_Cut_Block.gdb"),
                     layer = "consolidated-cutblocks-vri-extent")

# Tabular
# ECCC sample plot data (Andrea Norris)
habitatDf <-     read_csv(file.path(tabularDataDir, "Habitat selection - full dataset (30.03.2022).csv"))

coordinatesDf <- read_csv(file.path(tabularDataDir,"stationcoords_wSHP_latlong.csv")) %>% 
                 rename(Site_coords = Site)

speciesDf <-     read_csv(file.path(tabularDataDir, "Nest tree coordinates for woodpeckers.csv")) %>% 
                 mutate(TreeID = TreeID %>% as.character) %>% 
                 rename(Bird_spp = Spp)

## Generate aspen percent cover field ----
# Filter VRI data for species fields
vriTibble <- vri %>% 
  select(FEATURE_ID, SPECIES_CD_1, SPECIES_PCT_1, SPECIES_CD_2, SPECIES_PCT_2,
         SPECIES_CD_3, SPECIES_PCT_3, SPECIES_CD_4, SPECIES_PCT_4,
         SPECIES_CD_5, SPECIES_PCT_5, SPECIES_CD_6, SPECIES_PCT_6, )

# Generate individual sf objects for species levels and filter for aspen
dominantCover1 <- vriTibble %>%
  select(FEATURE_ID, SPECIES_CD_1, SPECIES_PCT_1) %>%
  filter(SPECIES_CD_1 == "AT") %>%
  select(FEATURE_ID, SPECIES_PCT_1) %>%
  rename(AT_PCT = SPECIES_PCT_1)

dominantCover2 <- vriTibble %>%
  select(FEATURE_ID, SPECIES_CD_2, SPECIES_PCT_2) %>%
  filter(SPECIES_CD_2 == "AT") %>%
  select(FEATURE_ID, SPECIES_PCT_2) %>%
  rename(AT_PCT = SPECIES_PCT_2)

dominantCover3 <- vriTibble %>%
  select(FEATURE_ID, SPECIES_CD_3, SPECIES_PCT_3) %>%
  filter(SPECIES_CD_3 == "AT") %>%
  select(FEATURE_ID, SPECIES_PCT_3) %>%
  rename(AT_PCT = SPECIES_PCT_3)

dominantCover4 <- vriTibble %>%
  select(FEATURE_ID, SPECIES_CD_4, SPECIES_PCT_4) %>%
  filter(SPECIES_CD_4 == "AT") %>%
  select(FEATURE_ID, SPECIES_PCT_4) %>%
  rename(AT_PCT = SPECIES_PCT_4)

dominantCover5 <- vriTibble %>%
  select(FEATURE_ID, SPECIES_CD_5, SPECIES_PCT_5) %>%
  filter(SPECIES_CD_5 == "AT") %>%
  select(FEATURE_ID, SPECIES_PCT_5) %>%
  rename(AT_PCT = SPECIES_PCT_5)

dominantCover6 <- vriTibble %>%
  select(FEATURE_ID, SPECIES_CD_6, SPECIES_PCT_6) %>%
  filter(SPECIES_CD_6 == "AT") %>%
  select(FEATURE_ID, SPECIES_PCT_6) %>%
  rename(AT_PCT = SPECIES_PCT_6)

# Bind all records of aspen together as a dataframe
aspenCover <- rbind(dominantCover1, dominantCover2, dominantCover3, 
                    dominantCover4, dominantCover5, dominantCover6) %>% 
  st_drop_geometry()

# Memory management
rm(vriTibble,dominantCover1, dominantCover2, dominantCover3, 
   dominantCover4, dominantCover5, dominantCover6)

# Join the aspen percent cover field to the VRI dataset based on polygon ID
# Remove fields that aren't needed
vri <- vri %>% 
  left_join(y = aspenCover, by = "FEATURE_ID") %>% 
  select(FEATURE_ID, LINE_3_TREE_SPECIES, LINE_4_CLASSES_INDEXES, LINE_5_VEGETATION_COVER, SPECIES_CD_1, 
         BEC_ZONE_CODE, BEC_SUBZONE, BEC_VARIANT, BEC_PHASE,
         QUAD_DIAM_125, QUAD_DIAM_175,
         PROJ_AGE_1, PROJ_AGE_CLASS_CD_1, PROJ_AGE_2, PROJ_AGE_CLASS_CD_2, 
         AT_PCT, PROJECTED_DATE) %>% 
  mutate(BEC_ZONE_SUBZONE = str_c(BEC_ZONE_CODE, BEC_SUBZONE), 
         BEC_ZONE_SUBZONE = case_when(BEC_ZONE_SUBZONE == "BGxw" ~ "Bunch Grass:very dry warm",
                                      BEC_ZONE_SUBZONE == "IDFdk" ~ "Interior Douglas-fir:dry cool",
                                      BEC_ZONE_SUBZONE == "IDFxm" ~ "Interior Douglas-fir:very dry mild",
                                      BEC_ZONE_SUBZONE == "SBPSmk" ~ "Sub-Boreal Pine - Spruce:moist cool",
                                      BEC_ZONE_SUBZONE == "SBSdw" ~ "Sub-Boreal Spruce:dry warm") %>% as.factor,
         BEC_VAR_CODE = case_when(BEC_ZONE_SUBZONE == "Bunch Grass:very dry warm" ~ 1,
                                  BEC_ZONE_SUBZONE == "Interior Douglas-fir:dry cool" ~ 2,
                                  BEC_ZONE_SUBZONE == "Interior Douglas-fir:very dry mild" ~ 3,
                                  BEC_ZONE_SUBZONE == "Sub-Boreal Pine - Spruce:moist cool" ~ 4,
                                  BEC_ZONE_SUBZONE == "Sub-Boreal Spruce:dry warm" ~5) %>% as.numeric,
         LEADING_SPECIES = case_when(SPECIES_CD_1 == "AT" ~ "Trembling Aspen",
                                     SPECIES_CD_1 == "PLI" ~ "Lodgepole Pine (Interior)",
                                     SPECIES_CD_1 == "FDI" ~ "Douglas Fir Interior",
                                     SPECIES_CD_1 == "SX" ~ "Spruce Hybrid",
                                     SPECIES_CD_1 == "FD" ~ "Douglas Fir",
                                     SPECIES_CD_1 == "PL" ~ "Lodgepole Pine",
                                     SPECIES_CD_1 == "AC" ~ "Poplar",
                                     SPECIES_CD_1 == "SW" ~ "White Spruce",
                                     SPECIES_CD_1 == "ACT" ~ "Black Cottonwood",
                                     SPECIES_CD_1 == "PLC" ~ "Lodgepole Pine (Coast)",
                                     SPECIES_CD_1 == "EP" ~ "Paper Birch",
                                     SPECIES_CD_1 == "BL" ~ "Subalpine Fir",
                                     SPECIES_CD_1 == "PY" ~ "Yellow Pine",
                                     SPECIES_CD_1 == "HW" ~ "Western Hemlock") %>% as.factor,
         leadingID = case_when(SPECIES_CD_1 == "AT" ~ 1,  # Group leading species into main forest types (i.e. state classes)
                               SPECIES_CD_1 == "PLI" ~ 2,
                               SPECIES_CD_1 == "FDI" ~ 3,
                               SPECIES_CD_1 == "SX" ~ 4,
                               SPECIES_CD_1 == "FD" ~ 3,
                               SPECIES_CD_1 == "PL" ~ 2,
                               SPECIES_CD_1 == "AC" ~ 1,
                               SPECIES_CD_1 == "SW" ~ 4,
                               SPECIES_CD_1 == "ACT" ~ 1,
                               SPECIES_CD_1 == "PLC" ~ 2,
                               SPECIES_CD_1 == "EP" ~ 1,
                               SPECIES_CD_1 == "BL" ~ 3,
                               SPECIES_CD_1 == "PY" ~ 2,
                               SPECIES_CD_1 == "HW" ~ 3) %>% as.numeric) %>% 
  rename(DIAM_125 = QUAD_DIAM_125,
         DIAM_175 = QUAD_DIAM_175,
         AGE_CLASS1 = PROJ_AGE_CLASS_CD_1,
         AGE_CLASS2 = PROJ_AGE_CLASS_CD_2)

# Write shapefile to disk
st_write(vri,
         dsn = spatialDataDir, 
         layer = "vri-aspen-percent-cover",
         driver = "ESRI Shapefile",
         append = FALSE)

## Merge ECCC sample plot data ----
## Reclass Site IDs ##
#change some of the sites (off) in data to match up
habitatDf$Site<-as.character(habitatDf$Site)
habitatDf <- habitatDf %>%
  mutate(Site = ifelse(Site == "D2OFF", "D2", Site))%>%
  mutate(Site = ifelse(Site == "FOOFF", "FO", Site))%>%
  mutate(Site = ifelse(Site == "LTOFF", "LT1", Site))%>%
  mutate(Site = ifelse(Site == "LToff", "LT1", Site))%>%
  mutate(Site = ifelse(Site == "LT1/LT2", "LT1", Site))%>%
  mutate(Site = ifelse(Site == "MLF", "ML", Site))%>%
  mutate(Site = ifelse(Site == "RCOFF", "RC", Site))%>%
  mutate(Site = ifelse(Site == "RCoff", "RC", Site))%>%
  mutate(Site = ifelse(Site == "RC-off", "RC", Site))%>%
  mutate(Site = ifelse(Site == "STUMP", "RC", Site))%>%
  mutate(Site = ifelse(Site == "RLOFF", "RL", Site))%>%
  mutate(Site = ifelse(Site == "SC-off", "SC", Site))%>%
  mutate(Site = ifelse(Site == "SCOFF", "SC", Site))%>%
  mutate(Site = ifelse(Site == "SDOFF", "SD", Site))%>%
  mutate(Site = ifelse(Site == "SHOFF", "SHAC", Site))%>%
  mutate(Site = ifelse(Site == "YYOFF", "YY", Site)) %>% 
  mutate(Site = Site %>% as.factor)

# Divide habitat data set by coordinate reference systems
# Transform coordinates from WGS and UTM 10 (NAD 83) to EPSG 3005
df1 <- habitatDf %>% 
  filter(type == "available") %>% 
  mutate(Site_Point = str_c(Site, "-", Point)) %>% 
  left_join(y = coordinatesDf, by = c("Site_Point" = "SITENUM")) %>% 
  vect(geom = c("longitude", "latitude"), crs = "epsg:4326") %>% 
  project(y = "epsg:3005")

df2 <- habitatDf %>% 
  filter(type == "selected") %>% 
  left_join(y = speciesDf, by = c("Point" = "TreeID")) %>% 
  vect(geom = c("Easting (NAD83)", "Northing (NAD 83)"), crs = "epsg:26910") %>% 
  project(y = "epsg:3005")

# Combine datasets
speciesData <- rbind(df1, df2)

speciesGeomtry <- geom(speciesData)

speciesData$x <- speciesGeomtry[, 3]
speciesData$y <- speciesGeomtry[, 4]

speciesData <- speciesData %>% 
  st_as_sf() %>% 
  filter(!st_is_empty(.)) %>%
  rename(SITENUM = Site_Point) %>% 
  select(-Site_coords, -X_COORD, -Y_COORD, -`UTM Zone`) %>% 
  tibble()

st_write(speciesData, 
         dsn = spatialDataDir, 
         layer = "eccc-sample-plot-data", 
         driver = "ESRI Shapefile",
         append = FALSE)

write_csv(speciesData, 
          file.path(tabularDataDir, "Habitat selection - full dataset with coordinates.csv"))


## Append VRI data fields to sample plot data frame ----
# Leading species
# Aspen % cover
# BEC zone/subzone
# quad_diam_125
# proj_age
# age_class

# Load sample plot data as a SpatVector object
samplePlots <- vect(read_csv(file.path(tabularDataDir, "eccc-sample-plots-merged.csv")), 
                    geom = c("x", "y"), crs = "epsg:3005") %>% 
               st_as_sf()

# Select VRI fields to be appended to plot data
vriSubset <- vri %>% 
  mutate(VRI_BEC = str_c(BEC_ZONE_C, "-", BEC_SUB)) %>% 
  select(FEATURE, BEC_ZONE_C, BEC_SUB, VRI_BEC, LEADING, PROJ_AGE_1, AGE_CLASS1, DIAM_12, AT_PCT) %>% 
  rename(VRI_ID = FEATURE, 
         VRI_BEC_ZONE = BEC_ZONE_C,
         VRI_BEC_SUBZONE = BEC_SUB,
         VRI_SPP = LEADING,
         VRI_AGE = PROJ_AGE_1,
         VRI_CLASS = AGE_CLASS1,
         VRI_DIAM = DIAM_12,
         VRI_AT_PCT = AT_PCT)

# Perform spatial intersection of VRI polygons and sample plot points
samplePlotsVriVector <- st_intersection(samplePlots, vriSubset) 
samplePlotsVriTibble <- st_intersection(samplePlots, vriSubset) %>%
  tibble

# Write tabular and spatial data to disk
st_write(samplePlotsVriVector, 
         dsn = "./Data", 
         layer = "eccc-sample-plot-data-append-vri", 
         driver = "ESRI Shapefile",
         append = FALSE)

write_csv(samplePlotsVriTibbleFilter, file.path(tabularDataDir, "Habitat selection - full dataset - append VRI (12.06.2022).csv"))

## Clip raw data to analysis area ----
dissolveVri <- st_union(x = vri, by_feature = FALSE)

protectedAreas <- protectedAreas %>% 
  st_transform(crs = targetCrs)

# zzz: causing an error
protectedAreasClip <-st_intersection(x = protectedAreas, y = vri)

bcParcelsClip <- st_crop(bcParcels$geometry, vri$geometry)

