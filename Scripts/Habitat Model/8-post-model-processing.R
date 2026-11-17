## a269
## Sarah Chisholm 
## November, 2023
##
## Plot change in habitat for seven cavity-nesting species across varying 
## management scenarios

## Workspace ----

# Set environment variable TZ when running on AWS EC2 instance
Sys.setenv(TZ='UTC')

# Load libraries
library(rsyncrosim)
library(tidyverse)
library(terra)
library(patchwork)
library(ggmap)
#library(osmdata)
library(RColorBrewer)
library(ggbreak)
#library(cowplot)
#library(lemon)
library(gridExtra)
#library(ggpubr)

# Define directories
libraryDir <- "D:/ECCC/Library"
figuresDir <- "Figures"
tabularDataDir <- "Data/Tabular"

## Parameters ----
libraryFile <- file.path(libraryDir, "NestWeb Species Occurrence.ssim")
myLibrary <- ssimLibrary(libraryFile)
myProject <- rsyncrosim::project(myLibrary, "Definitions")

resultScenarios <- scenario(myLibrary, results = TRUE) %>%
  rename_with(~ str_replace(.x, "^ScenarioId$", "ScenarioID")) %>%   # rsyncrosim 2.1.x names this column ScenarioId
  filter(!str_detect(Name, "Single Cell")) %>%
  dplyr::select(ScenarioID, Name) %>% 
  mutate(Name = Name %>% str_replace("\\(.*", "") %>% str_trim(),
         ScenarioID = ScenarioID %>% as.character()) %>% 
  rename(ScenarioName = Name) # %>%
  # The scenario names in the library are now already the final labels, so the relabelling below is no longer needed
  # mutate(ScenarioName = case_when(ScenarioName == "Baseline Ownership" ~ "Business as Usual",
  #                                 ScenarioName == "Baseline Ownership Fire x2" ~ "Business as Usual, Doubled Fire",
  #                                 ScenarioName == "Aspen Protection" ~ "Landscape-Wide Indigenous Stewardship",
  #                                 ScenarioName == "Aspen Protection Fire x2" ~ "Landscape-Wide Indigenous Stewardship, Doubled Fire",
  #                                 ScenarioName == "Military Land Protection" ~ "Indigenous Protected Areas",
  #                                 ScenarioName == "Military Land Protection Fire x2" ~ "Indigenous Protected Areas, Doubled Fire"))

baseFireScenarios <- scenario(myLibrary, results = TRUE) %>%
  rename_with(~ str_replace(.x, "^ScenarioId$", "ScenarioID")) %>%
  filter(!str_detect(Name, "Single Cell")) %>%
  filter(!str_detect(Name, "Fire x2")) %>%
  pull(ScenarioID)

doubleFireScenarios <- scenario(myLibrary, results = TRUE) %>%
  rename_with(~ str_replace(.x, "^ScenarioId$", "ScenarioID")) %>%
  filter(!str_detect(Name, "Single Cell")) %>%
  filter(str_detect(Name, "Fire x2")) %>%
  pull(ScenarioID)

species <- tibble(
  SpeciesID = seq(136,142),
  SpeciesName = c("American three-toed woodpecker", "Downy woodpecker", "Hairy woodpecker",
           "Northern flicker", "Pileated woodpecker", "Red-breasted nuthatch", "Red-naped sapsucker"))

## Maps ----
### Average Habitat Change ----
#### Baseline Fire ----
# Load raw rasters
baselineRasters <-  
  datasheetSpatRaster(myProject,
                      datasheet = "stsimNestweb_OutputSpatialHabitatChangeAverage",
                      scenario = baseFireScenarios,
                      column = "FileName",
                      timestep = 2046)

# Load value breaks
breaksDf <- read_csv(file.path(tabularDataDir, "average-habitat-change-breaks.csv"))
colNames <- breaksDf %>% colnames()

# Reclassify raw habitat rasters based on breaks
binnedHabitatRasters <- rast()

for(i in seq(1:nlyr(baselineRasters))){
  rastSpeciesID <- baselineRasters[[i]] %>% 
    names %>% 
    str_replace("\\.ts.*", "") %>% 
    str_replace(".*sp", "")
 
  reclass <- as.matrix(
    breaksDf[colNames %>% str_detect(rastSpeciesID)])

  reclassifiedRast <- baselineRasters[[i]] %>%
    classify(reclass)

  binnedHabitatRasters <- c(binnedHabitatRasters, reclassifiedRast)
}

# Prep raster data for plotting
baselineData <- binnedHabitatRasters %>% 
  as.data.frame(xy = TRUE) %>% 
  na.omit %>% 
  pivot_longer(
    c(-x, -y),
    names_to = c("ScenarioID", "Species"),
    names_sep = "\\.hsca.",
    values_to = "avgChange") %>% 
    mutate(ScenarioID = ScenarioID %>% str_replace("scn", ""),
           Species = Species %>% str_replace("\\.ts.*", ""),
           Species = Species %>% str_replace("sp", "")) %>% 
  left_join(resultScenarios %>% 
              mutate(ScenarioID = ScenarioID %>% as.character())) %>% 
  left_join(species %>% 
              mutate(SpeciesID = SpeciesID %>% as.character()), 
            by = join_by(Species == SpeciesID))

habitatLevels <- baselineData %>% 
  pull(avgChange) %>% 
  unique %>% 
  sort(decreasing = TRUE)

baselineData <- baselineData %>% 
  mutate(avgChange = avgChange %>% factor(levels = habitatLevels))

# Plot data
speciesPlots <- list()
for(i in seq(1:length(species$SpeciesID))){
  aSpecies <- species$SpeciesID[i]
  
  spData <- baselineData %>% 
    filter(Species == aSpecies)
  
  if(spData$SpeciesName %>% unique == "American three-toed woodpecker") {
    spPlot <- ggplot() +
      geom_raster(data = spData, aes(x = x, y = y, fill = avgChange)) +
      facet_grid(SpeciesName ~ ScenarioName, switch = "y") +
      scale_fill_brewer(palette = "PRGn",
                        direction = -1) +
      theme_void() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            strip.text.y = element_text(angle = 90),
            legend.title = element_blank())
    } else {
      spPlot <- ggplot() +
        geom_raster(data = spData, aes(x = x, y = y, fill = avgChange)) +
        facet_grid(SpeciesName ~ ScenarioName, switch = "y") +
        scale_fill_brewer(palette = "PRGn",
                          direction = -1) +
        theme_void() +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              strip.background.x = element_blank(),
              strip.text.x = element_blank(),
              strip.text.y = element_text(angle = 90),
              legend.title = element_blank())}
  
    speciesPlots[[i]] <- spPlot
}

avgHabitatChangePlot <- wrap_plots(speciesPlots, ncol = 1, nrow = 7)
ggsave(
  filename = file.path(figuresDir, "averageHabitatChangeBaselineFireMap.png"),
  plot = avgHabitatChangePlot,
  width = 9,
  height = 15,
  dpi = 500)

#### Double Fire ----
# Load raw rasters
doubleFireRasters <-  
  datasheetSpatRaster(myProject,
                      datasheet = "stsimNestweb_OutputSpatialHabitatChangeAverage",
                      scenario = doubleFireScenarios,
                      column = "FileName",
                      timestep = 2046)

# Reclassify raw habitat rasters based on breaks
binnedHabitatRasters <- rast()

for(i in seq(1:nlyr(doubleFireRasters))){
  rastSpeciesID <- doubleFireRasters[[i]] %>% 
    names %>% 
    str_replace("\\.ts.*", "") %>% 
    str_replace(".*sp", "")
  
  reclass <- as.matrix(
    breaksDf[colNames %>% str_detect(rastSpeciesID)])
  
  reclassifiedRast <- doubleFireRasters[[i]] %>%
    classify(reclass)
  
  binnedHabitatRasters <- c(binnedHabitatRasters, reclassifiedRast)
}

# Prep raster data for plotting
doubleFireData <- binnedHabitatRasters %>% 
  as.data.frame(xy = TRUE) %>% 
  na.omit %>% 
  pivot_longer(
    c(-x, -y),
    names_to = c("ScenarioID", "Species"),
    names_sep = "\\.hsca.",
    values_to = "avgChange") %>% 
  mutate(ScenarioID = ScenarioID %>% str_replace("scn", ""),
         Species = Species %>% str_replace("\\.ts.*", ""),
         Species = Species %>% str_replace("sp", "")) %>% 
  left_join(resultScenarios %>% 
              mutate(ScenarioID = ScenarioID %>% as.character())) %>% 
  left_join(species %>% 
              mutate(SpeciesID = SpeciesID %>% as.character()), 
            by = join_by(Species == SpeciesID))

habitatLevels <- doubleFireData %>% 
  pull(avgChange) %>% 
  unique %>% 
  sort(decreasing = TRUE)

doubleFireData <- doubleFireData %>% 
  mutate(avgChange = avgChange %>% factor(levels = habitatLevels))

# Plot data
speciesPlots <- list()
for(i in seq(1:length(species$SpeciesID))){
  aSpecies <- species$SpeciesID[i]
  
  spData <- doubleFireData %>% 
    filter(Species == aSpecies)
  
  if(spData$SpeciesName %>% unique == "American three-toed woodpecker") {
    spPlot <- ggplot() +
      geom_raster(data = spData, aes(x = x, y = y, fill = avgChange)) +
      facet_grid(SpeciesName ~ ScenarioName, switch = "y") +
      scale_fill_brewer(palette = "PRGn",
                        direction = -1) +
      theme_void() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            strip.text.y = element_text(angle = 90),
            legend.title = element_blank())
  } else {
    spPlot <- ggplot() +
      geom_raster(data = spData, aes(x = x, y = y, fill = avgChange)) +
      facet_grid(SpeciesName ~ ScenarioName, switch = "y") +
      scale_fill_brewer(palette = "PRGn",
                        direction = -1) +
      theme_void() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            strip.background.x = element_blank(),
            strip.text.x = element_blank(),
            strip.text.y = element_text(angle = 90),
            legend.title = element_blank())}
  
  speciesPlots[[i]] <- spPlot
}

avgHabitatChangePlot <- wrap_plots(speciesPlots, ncol = 1, nrow = 7)
ggsave(
  filename = file.path(figuresDir, "averageHabitatChangeDoubleFireMap.png"),
  plot = avgHabitatChangePlot,
  width = 9,
  height = 15,
  dpi = 500)

### Total Ecosystem ----
tecMapData <-   
  datasheetSpatRaster(myProject,
                      datasheet = "stsimsf_OutputAverageSpatialStockGroup",
                      scenario = resultScenarios$ScenarioID %>% as.integer(),
                      filterColumn = "StockGroupID",
                      filterValue = "Total Ecosystem") %>% 
  as.data.frame(xy = TRUE) %>% 
  pivot_longer(
    cols = c(-x, -y),
    names_to = c("ScenarioID", "Timestep"), 
    names_sep = "\\.avgstkg_166.it0.ts",
    values_to = "AverageTEC") %>% 
  mutate(ScenarioID = ScenarioID %>% str_replace("scn", "")) %>% 
  left_join(resultScenarios) %>% 
  filter(Timestep == 2016 | Timestep == 2046) %>% 
  filter(ScenarioID == "247" | ScenarioID == "250")

# Define colour ramp
carbon_colors <- colorRampPalette(
  c("grey95", "#fef0d9", "#fdd49e", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#990000"))

tecPlot2016 <- tecMapData %>%
  filter(Timestep == 2016 & ScenarioID == "247") %>% 
  ggplot(aes(colour = AverageTEC)) +
  geom_raster(aes(x = x, y = y, fill = AverageTEC)) +
  scale_fill_stepsn(name = "Tons C per ha",
                    colours = carbon_colors(10),
                    breaks = c(50, 150, 200, 225, 250),
                    limits = c(0, 275)) +
  xlab("2016") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"),
        panel.background = element_rect(colour = "white"),
        #plot.background = element_rect(fill = "white"),
        #plot.background = element_line(colour = "white"),
        plot.margin = margin(4.5, 0, 4.5, 0, "cm"))

tecPlot2046Baseline <- tecMapData %>%
  filter(Timestep == 2046 & ScenarioID == "247") %>% 
  ggplot(aes(colour = AverageTEC)) +
  geom_raster(aes(x = x, y = y, fill = AverageTEC)) +
  scale_fill_stepsn(name = "Tons C per ha",
                    colours = carbon_colors(10),
                    breaks = c(50, 150, 200, 225, 250),
                    limits = c(0, 275)) +
  xlab("Baseline Ownership 2046") +
  theme_minimal() + 
  theme(axis.title.y = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"),
        panel.background = element_rect(colour = "white"))
        #plot.background = element_rect(fill = "white"))

tecPlot2046x2Fire <- tecMapData %>%
  filter(Timestep == 2046 & ScenarioID == "250") %>% 
  ggplot(aes(colour = AverageTEC)) +
  geom_raster(aes(x = x, y = y, fill = AverageTEC)) +
  scale_fill_stepsn(name = "Tons C per ha",
                    colours = carbon_colors(10),
                    breaks = c(50, 150, 200, 225, 250),
                    limits = c(0, 275)) +
  xlab("Baseline Ownership Fire x2 2046") +
  theme_minimal() + 
  theme(axis.title.y = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"),
        panel.background = element_rect(colour = "white"))
        #plot.background = element_rect(fill = "white"))

tecLegend <- g_legend(tecPlot2016 + theme(legend.position='bottom'))

tecPlot <- grid.arrange(tecPlot2016+theme(legend.position='hidden'),
             arrangeGrob(tecPlot2046Baseline+theme(legend.position='hidden'),
                         tecPlot2046x2Fire+theme(legend.position='hidden'),
                         ncol = 1),
             right = tecLegend,
             widths = c(0.55, 0.45),
             ncol = 2)

ggsave(
  filename = file.path(figuresDir, "tecMap.png"),
  plot = tecPlot,
  device = "png",
  width = 12,
  height = 10,
  dpi = 500)

### Initial State Class Map ----
initialStateClassRaster <- datasheetSpatRaster(
    scenario(myProject, scenario = resultScenarios[1,1] %>% as.integer()),
    datasheet = "stsim_OutputSpatialState",
    iteration = 1,
    timestep = 2016) %>% 
  as.data.frame(xy = TRUE)

stateClassColours <- c("#000000", "#004A95", "#003500", "#35FF35", "#00B900", 
                       "#007500", "#014E27", "#FFFF00", "#86CC02", "#00C174", 
                       "#8D8D8D")

stateClassPlot <- initialStateClassRaster %>% 
  ggplot(aes(x = x, y = y, fill = sc.it1.ts2016 %>% as.factor())) +
  geom_raster() +
  scale_fill_manual(values = stateClassColours,
                    na.value = "white") +
  labs(title = "Year 2016 Land Cover") + 
  theme_minimal() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(),
        plot.title = element_text(hjust = 0.5))

ggsave(
  filename = file.path(figuresDir, "initialStateClass.png"),
  plot = stateClassPlot,
  device = "png",
  dpi = 500)

### Habitat Probability of Occurrence Maps ----
# One figure per species (the species name and the panel descriptions go in the figure caption):
# the 2016 map (mean of the six scenarios, which share the same starting landscape) in the top left, the colour bar
# below it, and the six 2046 maps labelled a.-f. in a grid with the management scenarios in rows and the fire
# scenarios in columns:
#   a. Business as Usual                           b. Business as Usual, Doubled Fire
#   c. Landscape-Wide Indigenous Stewardship       d. Landscape-Wide Indigenous Stewardship, Doubled Fire
#   e. Indigenous Protected Areas                  f. Indigenous Protected Areas, Doubled Fire
# The maps are the mean probability of occurrence across the 40 iterations
# (stsimNestweb_OutputSpatialHabitatAverage). One colour scale per figure, shared by all seven panels.
mapScenarios <- tibble(
  ScenarioID = c(247, 250, 248, 251, 274, 275),
  Management = c("Business as Usual", "Business as Usual",
                 "Landscape-Wide Indigenous Stewardship", "Landscape-Wide Indigenous Stewardship",
                 "Indigenous Protected Areas", "Indigenous Protected Areas"),
  Fire = c("Baseline fire", "Doubled fire", "Baseline fire", "Doubled fire", "Baseline fire", "Doubled fire"),
  Label = c("a.", "b.", "c.", "d.", "e.", "f."))

# Species to map (all seven; 141 = Red-breasted nuthatch alone)
mapSpeciesIDs <- species$SpeciesID

# ColorBrewer sequential palette: "Greens", "Purples", "Blues", ...
mapPaletteName <- "Greens"

makeHabitatMapFigure <- function(speciesId, versions = c("continuous", "binned")) {
  speciesName <- species$SpeciesName[species$SpeciesID == speciesId]

  # Mean probability raster of one species, scenario and year (the GeoTIFFs are read straight from the library's
  # data folder, which does not need SyncroSim and works while the library is open in Studio)
  loadMeanHabitat <- function(scenarioId, timestep) {
    rast(file.path(paste0(libraryFile, ".data"), paste0("Scenario-", scenarioId),
                   "stsimNestweb_OutputSpatialHabitatAverage",
                   paste0("hsa.sp", speciesId, ".ts", timestep, ".tif")))
  }
  rasterToDf <- function(r) {
    as.data.frame(r, xy = TRUE, na.rm = TRUE) %>%
      setNames(c("x", "y", "probability")) %>%
      mutate(probabilityFill = ifelse(probability == 0, NA, probability))   # zeros are drawn with the grey na.value
  }

  # 2016: mean of the six scenarios; 2046: one map per scenario
  data2016 <- mapScenarios$ScenarioID %>%
    lapply(loadMeanHabitat, timestep = 2016) %>%
    rast() %>%
    mean() %>%
    rasterToDf()

  data2046 <- lapply(seq_len(nrow(mapScenarios)), function(i) {
    loadMeanHabitat(mapScenarios$ScenarioID[i], 2046) %>%
      rasterToDf() %>%
      mutate(Label = mapScenarios$Label[i])
  }) %>%
    bind_rows()

  # Shared colour scale: 0 to the 99th percentile of all panels (rounded up to 0.01)
  allProbabilities <- c(data2016$probability, data2046$probability)
  upperLimit <- ceiling(quantile(allProbabilities, 0.99) * 100) / 100

  # Single-hue sequential palette, pale (low) to saturated (high). The palest step is dropped so that
  # low-probability areas stay distinguishable from the white background. A probability of exactly 0
  # is light grey (the na.value of the fill scale; it has no legend key and is explained in the caption).
  greenSteps <- RColorBrewer::brewer.pal(9, mapPaletteName)[3:9]
  continuousScale <- scale_fill_gradientn(
    colours = greenSteps,
    limits = c(0, upperLimit),
    oob = scales::squish,
    na.value = "grey85",
    name = "Mean probability\nof occurrence",
    guide = guide_colourbar(direction = "vertical",
                            order = 1,
                            title.position = "top",
                            title.hjust = 0,
                            barwidth = unit(0.5, "cm"),
                            barheight = unit(5.5, "cm")))

  # Binned version: one class for pixels equal to 0 and five equal-area classes (quintiles, each holding the same
  # number of 90 x 90 m cells) of the non-zero probabilities, pooled over all seven panels
  nonZeroProbabilities <- allProbabilities[allProbabilities > 0]
  binQuantiles <- as.numeric(quantile(nonZeroProbabilities, probs = seq(0.2, 1, by = 0.2)))
  # Round the class limits to the fewest digits (3 to 5) that keep them distinct and above 0; the top limit is rounded up
  for(binDigits in 3:5) {
    binBreaks <- c(0, round(binQuantiles[1:4], binDigits), ceiling(binQuantiles[5] * 10^binDigits) / 10^binDigits)
    if(all(diff(binBreaks) > 0)) break
  }
  breakText <- formatC(binBreaks, format = "f", digits = binDigits)
  binLabels <- c("0", paste0(">", c("0", breakText[-c(1, length(breakText))]), " to ", breakText[-1]))
  binColours <- c("grey85", colorRampPalette(greenSteps)(length(binLabels) - 1)) %>%
    setNames(binLabels)
  assignClass <- function(probability) {
    nonZeroClass <- cut(probability, breaks = binBreaks, labels = binLabels[-1], right = TRUE) %>% as.character()
    ifelse(probability == 0, binLabels[1], nonZeroClass) %>% factor(levels = binLabels)
  }
  data2016 <- data2016 %>% mutate(probabilityClass = assignClass(probability))
  data2046 <- data2046 %>% mutate(probabilityClass = assignClass(probability))
  binnedScale <- scale_fill_manual(
    values = binColours,
    drop = FALSE,
    name = "Mean probability\nof occurrence",
    guide = guide_legend(reverse = TRUE,
                         title.position = "top",
                         title.hjust = 0,
                         keywidth = unit(0.5, "cm"),
                         keyheight = unit(0.7, "cm")))

  mapTheme <- theme_void() +
    theme(plot.margin = margin(1, 1, 1, 1),
          legend.position = "right")

  # Same extent for every panel so that all maps have the same size
  xRange <- range(data2016$x) + c(-45, 45)
  yRange <- range(data2016$y) + c(-45, 45)

  # Year headings fixed to the top of the layout
  headingPlot <- function(headingText) {
    ggplot() +
      annotate("text", x = 0, y = 0, label = headingText, size = 6, fontface = "bold") +
      theme_void() +
      theme(plot.margin = margin(0, 0, 0, 0))
  }

  # Layout: a heading row ("2016" over the left column, "2046" over the two right columns) and 30 rows of one tenth
  # of a 2046 map height. The 2046 maps a.-f. (B, C, E, F, G, H; management in rows, fire in columns) span 10 rows;
  # the 2016 map (A) spans 11 rows and is 1.1 times wider, starts 0.4 of a map height below the top of the first
  # row, and the colour bar (D) fills the rest of the left column below it.
  mapAspect <- diff(xRange) / diff(yRange)
  mapDesignMatrix <- matrix("#", nrow = 31, ncol = 3)
  mapDesignMatrix[1, 1] <- "L"
  mapDesignMatrix[1, 2:3] <- "M"
  mapDesignMatrix[2:11, 2] <- "B";  mapDesignMatrix[2:11, 3] <- "C"
  mapDesignMatrix[12:21, 2] <- "E"; mapDesignMatrix[12:21, 3] <- "F"
  mapDesignMatrix[22:31, 2] <- "G"; mapDesignMatrix[22:31, 3] <- "H"
  mapDesignMatrix[6:16, 1] <- "A"
  mapDesignMatrix[17:31, 1] <- "D"
  mapDesign <- apply(mapDesignMatrix, 1, paste, collapse = "") %>% paste(collapse = "\n")

  # One figure per colour version ("continuous" colour bar or "binned" legend)
  habitatMapFigures <- lapply(versions, function(aVersion) {
    fillVariable <- if(aVersion == "binned") "probabilityClass" else "probabilityFill"
    fillScale <- if(aVersion == "binned") binnedScale else continuousScale

    mapPanel <- function(panelData, panelLabel = NULL) {
      aPanel <- ggplot(panelData, aes(x = x, y = y, fill = .data[[fillVariable]])) +
        geom_raster() +
        coord_equal(xlim = xRange, ylim = yRange, expand = FALSE) +
        fillScale +
        mapTheme
      if(!is.null(panelLabel)) {
        aPanel <- aPanel +
          annotate("text", x = xRange[1] + 1000, y = yRange[2] - 1000, label = panelLabel,
                   hjust = 0, vjust = 1, size = 5, fontface = "bold")
      }
      aPanel
    }

    plot2016 <- mapPanel(data2016)
    panels2046 <- lapply(mapScenarios$Label, function(aLabel) {
      mapPanel(data2046 %>% filter(Label == aLabel), aLabel)
    })

    # Plots are assigned to the design letters in alphabetical order: A, B, C, D, E, F, G, H, L, M
    habitatMapFigure <- (plot2016 + panels2046[[1]] + panels2046[[2]] + guide_area() +
                           panels2046[[3]] + panels2046[[4]] + panels2046[[5]] + panels2046[[6]] +
                           headingPlot("2016") + headingPlot("2046")) +
      plot_layout(design = mapDesign,
                  guides = "collect",
                  widths = c(1.1 * 10 * mapAspect, 10 * mapAspect, 10 * mapAspect),
                  heights = c(1.5, rep(1, 30)))

    ggsave(
      filename = file.path(figuresDir,
                           paste0("habitatProbabilityMap_", speciesName %>% tolower() %>% str_replace_all(" ", "-"),
                                  if(aVersion == "binned") "_binned" else "", ".png")),
      plot = habitatMapFigure,
      width = 9,
      height = 6.7,
      dpi = 500,
      bg = "white")

    habitatMapFigure
  }) %>%
    setNames(versions)

  # Class limits and share of cells per class (all panels pooled), for the caption and for checking
  classCounts <- table(factor(c(as.character(data2016$probabilityClass), as.character(data2046$probabilityClass)),
                              levels = binLabels))
  print(tibble(Class = binLabels, Cells = as.numeric(classCounts)) %>%
          mutate(Share = round(Cells / sum(Cells), 3)))

  habitatMapFigures
}

### Habitat Probability of Occurrence Change Maps ----
# One figure per species: change in the mean probability of occurrence from 2016 to 2046 (2046 minus 2016, per scenario;
# identical to stsimNestweb_OutputSpatialHabitatChangeAverage) in a 3 x 2 grid of panels a.-f. with the same layout as
# the 2046 maps above (management scenarios in rows, fire scenarios in columns).
# Colours follow the SyncroSim map legends: purple = loss, grey = no change (exactly 0), green = gain (ColorBrewer PRGn).
# Classes: the non-zero losses and the non-zero gains (pooled over the six panels) are each split into three
# equal-area classes (terciles), so every species gets class limits that suit its own range of change.
makeHabitatChangeMapFigure <- function(speciesId) {
  speciesName <- species$SpeciesName[species$SpeciesID == speciesId]

  loadChange <- function(scenarioId) {
    dataFolder <- file.path(paste0(libraryFile, ".data"), paste0("Scenario-", scenarioId),
                            "stsimNestweb_OutputSpatialHabitatAverage")
    rast(file.path(dataFolder, paste0("hsa.sp", speciesId, ".ts2046.tif"))) -
      rast(file.path(dataFolder, paste0("hsa.sp", speciesId, ".ts2016.tif")))
  }
  changeData <- lapply(seq_len(nrow(mapScenarios)), function(i) {
    as.data.frame(loadChange(mapScenarios$ScenarioID[i]), xy = TRUE, na.rm = TRUE) %>%
      setNames(c("x", "y", "change")) %>%
      mutate(Label = mapScenarios$Label[i])
  }) %>%
    bind_rows()

  # Class limits: terciles of the losses and of the gains, rounded to the fewest digits that keep them distinct
  losses <- changeData$change[changeData$change < 0]
  gains <- changeData$change[changeData$change > 0]
  for(aDigits in 3:6) {
    lossLimits <- round(as.numeric(quantile(losses, c(1 / 3, 2 / 3))), aDigits)
    gainLimits <- round(as.numeric(quantile(gains, c(1 / 3, 2 / 3))), aDigits)
    if(all(diff(c(lossLimits, 0, gainLimits)) > 0)) break
  }
  limitText <- function(x) formatC(x, format = "f", digits = aDigits)
  changeLabels <- c(paste0("<= ", limitText(lossLimits[1])),
                    paste0("> ", limitText(lossLimits[1]), " to ", limitText(lossLimits[2])),
                    paste0("> ", limitText(lossLimits[2]), " to <0"),
                    "0",
                    paste0("> 0 to ", limitText(gainLimits[1])),
                    paste0("> ", limitText(gainLimits[1]), " to ", limitText(gainLimits[2])),
                    paste0("> ", limitText(gainLimits[2]), " to ", limitText(ceiling(max(gains) * 10^aDigits) / 10^aDigits)))
  # PRGn purples/greens; the lightest purple is a stronger shade (PRGn "#C2A5CF") so that it stays distinct from the grey
  changeColours <- c("#762A83", "#9970AB", "#C2A5CF", "grey85", RColorBrewer::brewer.pal(6, "PRGn")[4:6]) %>%
    setNames(changeLabels)
  changeData <- changeData %>%
    mutate(changeClass = case_when(
      change == 0 ~ 4,
      change < 0 ~ as.numeric(cut(change, c(-Inf, lossLimits, 0), labels = 1:3)),
      TRUE ~ 4 + as.numeric(cut(change, c(0, gainLimits, Inf), labels = 1:3))) %>%
        factor(levels = 1:7, labels = changeLabels))

  mapTheme <- theme_void() +
    theme(plot.margin = margin(1, 1, 1, 1),
          legend.position = "right")
  xRange <- range(changeData$x) + c(-45, 45)
  yRange <- range(changeData$y) + c(-45, 45)

  changePanels <- lapply(mapScenarios$Label, function(aLabel) {
    ggplot(changeData %>% filter(Label == aLabel), aes(x = x, y = y, fill = changeClass)) +
      geom_raster() +
      coord_equal(xlim = xRange, ylim = yRange, expand = FALSE) +
      annotate("text", x = xRange[1] + 1000, y = yRange[2] - 1000, label = aLabel,
               hjust = 0, vjust = 1, size = 5, fontface = "bold") +
      scale_fill_manual(values = changeColours,
                        drop = FALSE,
                        name = "Change in mean probability\nof occurrence, 2016-2046",
                        guide = guide_legend(reverse = TRUE,
                                             title.position = "top",
                                             title.hjust = 0,
                                             keywidth = unit(0.5, "cm"),
                                             keyheight = unit(0.7, "cm"))) +
      mapTheme
  })

  changeMapFigure <- wrap_plots(changePanels, ncol = 2, byrow = TRUE) +
    plot_layout(guides = "collect")

  ggsave(
    filename = file.path(figuresDir,
                         paste0("habitatProbabilityChangeMap_", speciesName %>% tolower() %>% str_replace_all(" ", "-"), ".png")),
    plot = changeMapFigure,
    width = 7.2,
    height = 6.1,
    dpi = 500,
    bg = "white")

  # Class limits and share of cells per class (all panels pooled), for the caption and for checking
  print(tibble(Class = changeLabels, Cells = as.numeric(table(changeData$changeClass))) %>%
          mutate(Share = round(Cells / sum(Cells), 3)))

  changeMapFigure
}

for(aSpecies in mapSpeciesIDs) {
  habitatMapFigures <- makeHabitatMapFigure(aSpecies)
  habitatChangeMapFigure <- makeHabitatChangeMapFigure(aSpecies)
}

## Charts ----
### TEC, NEP, NBP Plot ----
labels <- c("Landscape-Wide Indigenous Stewardship", 
            "Landscape-Wide Indigenous Stewardship, Doubled Fire",
            "Business as Usual", "Business as Usual, Doubled Fire",
            "Indigenous Protected Areas",
            "Indigenous Protected Areas, Doubled Fire", 
            "Historic")

tecData <- read_csv(file.path("Model-Outputs", "total-ecosystem-output.csv")) %>% 
  pivot_longer(cols = contains("Mean"),
               names_to = "ScenarioID",
               values_to = "Mean") %>% 
  dplyr::select(Timestep, ScenarioID, Mean) %>% 
  bind_cols(
    read_csv(file.path("Model-Outputs", "total-ecosystem-output.csv")) %>% 
      pivot_longer(cols = contains("Lower"),
                   names_to = "ScenarioIDLower",
                   values_to = "Lower") %>% 
      dplyr::select(Lower)) %>% 
  bind_cols(
    read_csv(file.path("Model-Outputs", "total-ecosystem-output.csv")) %>% 
      pivot_longer(cols = contains("Upper"),
                   names_to = "ScenarioIDUpper",
                   values_to = "Upper") %>% 
      dplyr::select(Upper)) %>% 
  mutate(ScenarioID = ScenarioID %>% str_remove("\\["),
         ScenarioID = ScenarioID %>% str_remove("\\] - Mean")) %>% 
  left_join(resultScenarios) %>% 
  mutate(Mean = Mean/1e6,
         Lower = Lower/1e6,
         Upper = Upper/1e6) %>% 
  as.data.frame()

tecData <- tecData %>% 
  bind_rows(
    tecData %>% 
      filter(ScenarioID == 247) %>% 
      mutate(Mean = case_when(Timestep <= 2021 ~ Mean),
             Lower = case_when(Timestep <= 2021 ~ Lower),
             Upper = case_when(Timestep <= 2021 ~ Upper),
             ScenarioName = "Historic",
             ScenarioID = "000")) %>% 
  mutate(ScenarioName = ScenarioName %>% 
           factor(levels = labels))

nbpData <- read_csv(file.path("Model-Outputs", "net-biome-productivity-output.csv")) %>% 
  pivot_longer(cols = contains("Mean"),
               names_to = "ScenarioID",
               values_to = "Mean") %>% 
  dplyr::select(Timestep, ScenarioID, Mean) %>% 
  bind_cols(
    read_csv(file.path("Model-Outputs", "net-biome-productivity-output.csv")) %>% 
      pivot_longer(cols = contains("Lower"),
                   names_to = "ScenarioIDLower",
                   values_to = "Lower") %>% 
      dplyr::select(Lower)) %>% 
  bind_cols(
    read_csv(file.path("Model-Outputs", "net-biome-productivity-output.csv")) %>% 
      pivot_longer(cols = contains("Upper"),
                   names_to = "ScenarioIDUpper",
                   values_to = "Upper") %>% 
      dplyr::select(Upper)) %>% 
  mutate(ScenarioID = ScenarioID %>% str_remove("\\["),
         ScenarioID = ScenarioID %>% str_remove("\\] - Mean")) %>% 
  left_join(resultScenarios) %>% 
  filter(Timestep != 2016) %>% 
  mutate(Mean = Mean/1e6,
         Lower = Lower/1e6,
         Upper = Upper/1e6) %>% 
  as.data.frame()

nbpData <- nbpData %>% 
  bind_rows(
    nbpData %>% 
      filter(ScenarioID == 247) %>% 
      mutate(Mean = case_when(Timestep <= 2021 ~ Mean),
             Lower = case_when(Timestep <= 2021 ~ Lower),
             Upper = case_when(Timestep <= 2021 ~ Upper),
             ScenarioName = "Historic",
             ScenarioID = "000")) %>% 
  mutate(ScenarioName = ScenarioName %>% 
           factor(levels = labels))

nepData <- read_csv(file.path("Model-Outputs", "net-ecosystem-productivity-output.csv")) %>% 
  pivot_longer(cols = contains("Mean"),
               names_to = "ScenarioID",
               values_to = "Mean") %>% 
  dplyr::select(Timestep, ScenarioID, Mean) %>% 
  bind_cols(
    read_csv(file.path("Model-Outputs", "net-ecosystem-productivity-output.csv")) %>% 
      pivot_longer(cols = contains("Lower"),
                   names_to = "ScenarioIDLower",
                   values_to = "Lower") %>% 
      dplyr::select(Lower)) %>% 
  bind_cols(
    read_csv(file.path("Model-Outputs", "net-ecosystem-productivity-output.csv")) %>% 
      pivot_longer(cols = contains("Upper"),
                   names_to = "ScenarioIDUpper",
                   values_to = "Upper") %>% 
      dplyr::select(Upper)) %>% 
  mutate(ScenarioID = ScenarioID %>% str_remove("\\["),
         ScenarioID = ScenarioID %>% str_remove("\\] - Mean")) %>% 
  left_join(resultScenarios) %>% 
  filter(Timestep != 2016) %>% 
  mutate(Mean = Mean/1e6,
         Lower = Lower/1e6,
         Upper = Upper/1e6) %>% 
  as.data.frame()

nepData <- nepData %>% 
  bind_rows(
    nepData %>% 
      filter(ScenarioID == 247) %>% 
      mutate(Mean = case_when(Timestep <= 2022 ~ Mean),
             Lower = case_when(Timestep <= 2022 ~ Lower),
             Upper = case_when(Timestep <= 2022 ~ Upper),
             ScenarioName = "Historic",
             ScenarioID = "000")) %>% 
  mutate(ScenarioName = ScenarioName %>% 
           factor(levels = labels))

plotLabels <- c("Landscape-Wide Indigenous Stewardship", 
                "Landscape-Wide Indigenous Stewardship, Doubled Fire",
                "Business as Usual", "Business as Usual, Doubled Fire",
                "Indigenous Protected Areas",
                "Indigenous Protected Areas, Doubled Fire")

tecPlot <- tecData %>% 
ggplot(aes(x = Timestep, y = Mean, group = ScenarioName, colour = ScenarioName)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, group = ScenarioName, fill = ScenarioName), 
              alpha = 0.15) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#1b9e77", "#b0f7e2", "#d95f02", "#e3ac81", "#6c66b0", "#b8b3f5", "grey50"),
                     name = "Scenarios",
                     limits = plotLabels) +
  scale_fill_manual(values = c("#1b9e77", "#b0f7e2", "#d95f02", "#e3ac81", "#6c66b0", "#b8b3f5", "grey50"),
                    name = "Scenarios",
                    limits = plotLabels) +
  ylab("Mt C") +
  labs(tag = "a") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        legend.position = "none")

nepPlot <- nepData %>% 
  ggplot(aes(x = Timestep, y = Mean, group = ScenarioName, colour = ScenarioName)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, group = ScenarioName, fill = ScenarioName), 
              alpha = 0.15) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#1b9e77", "#b0f7e2", "#d95f02", "#e3ac81", "#6c66b0", "#b8b3f5", "grey50"),
                     name = "Scenarios",
                     limits = labels) +
  scale_fill_manual(values = c("#1b9e77", "#b0f7e2", "#d95f02", "#e3ac81", "#6c66b0", "#b8b3f5", "grey50"),
                    name = "Scenarios",
                    limits = labels) +
  ylab("Mt CO2e") +
  labs(tag = "b") +
  theme_bw() +
  theme(axis.title.x = element_blank())

nbpPlot <- nbpData %>% 
  ggplot(aes(x = Timestep, y = Mean, group = ScenarioName, colour = ScenarioName)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, group = ScenarioName, fill = ScenarioName), 
              alpha = 0.15) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#1b9e77", "#b0f7e2", "#d95f02", "#e3ac81", "#6c66b0", "#b8b3f5", "grey50"),
                     name = "Scenarios",
                     limits = plotLabels) +
  scale_fill_manual(values = c("#1b9e77", "#b0f7e2", "#d95f02", "#e3ac81", "#6c66b0", "#b8b3f5", "grey50"),
                    name = "Scenarios",
                    limits = plotLabels) +
  ylab("Mt CO2e") +
  labs(tag = "c") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        legend.position = "none")

threePanelPlot <- wrap_plots(tecPlot, nepPlot, nbpPlot, ncol = 1, nrow = 3)
ggsave(
  filename = file.path(figuresDir, "carbon3Panel.png"),
  plot = threePanelPlot,
  device = "png",
  width = 10,
  height = 10,
  units = "in",
  dpi = 500)

### Cumulative Net Emissions Reductions ----
emissionsData <- read_csv(file.path("Model-Outputs", "cumulative-net-emissions-reduction-output.csv")) %>% 
  pivot_longer(cols = contains("Mean"),
               names_to = "ScenarioID",
               values_to = "Mean") %>% 
  dplyr::select(Timestep, ScenarioID, Mean) %>% 
  bind_cols(
    read_csv(file.path("Model-Outputs", "cumulative-net-emissions-reduction-output.csv")) %>% 
      pivot_longer(cols = contains("Lower"),
                   names_to = "ScenarioIDLower",
                   values_to = "Lower") %>% 
      dplyr::select(Lower)) %>% 
  bind_cols(
    read_csv(file.path("Model-Outputs", "cumulative-net-emissions-reduction-output.csv")) %>% 
      pivot_longer(cols = contains("Upper"),
                   names_to = "ScenarioIDUpper",
                   values_to = "Upper") %>% 
      dplyr::select(Upper)) %>% 
  mutate(ScenarioID = ScenarioID %>% str_remove("\\["),
         ScenarioID = ScenarioID %>% str_remove("\\] - Mean")) %>% 
  left_join(resultScenarios) %>% 
  mutate(Mean = Mean/1e6,
         Lower = Lower/1e6,
         Upper = Upper/1e6) %>% 
  as.data.frame()

emissionsData <- emissionsData %>% 
  bind_rows(
  emissionsData %>% 
  filter(ScenarioID == 247) %>% 
  mutate(Mean = case_when(Timestep <= 2021 ~ Mean),
         Lower = case_when(Timestep <= 2021 ~ Lower),
         Upper = case_when(Timestep <= 2021 ~ Upper),
         ScenarioName = "Historic",
         ScenarioID = "000")) %>% 
  mutate(ScenarioName = ScenarioName %>% 
           factor(levels = c("Aspen Protection", "Aspen Protection Fire x2",
                             "Baseline Ownership", "Baseline Ownership Fire x2","Military Land Protection",
                             "Military Land Protection Fire x2", "Historic")))

emissionsPlot <- emissionsData %>% 
  ggplot(aes(x = Timestep, y = Mean, group = ScenarioName, colour = ScenarioName)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, group = ScenarioName, fill = ScenarioName), 
              alpha = 0.15) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#1b9e77", "#b0f7e2", "#d95f02", "#e3ac81", "#6c66b0", "#b8b3f5", "grey50"),
                     name = "Scenarios",
                     limits = c("Aspen Protection", "Aspen Protection Fire x2",
                                "Baseline Ownership", "Baseline Ownership Fire x2","Military Land Protection",
                                "Military Land Protection Fire x2", "Historic")) +
  scale_fill_manual(values = c("#1b9e77", "#b0f7e2", "#d95f02", "#e3ac81", "#6c66b0", "#b8b3f5", "grey50"),
                    name = "Scenarios",
                    limits = c("Aspen Protection", "Aspen Protection Fire x2",
                               "Baseline Ownership", "Baseline Ownership Fire x2","Military Land Protection",
                               "Military Land Protection Fire x2", "Historic")) +
  ylab("Mt CO2e") +
  theme_bw() +
  theme(axis.title.x = element_blank())

ggsave(
  filename = file.path(figuresDir, "cumNetEmissionRedux.png"),
  plot = emissionsPlot,
  device = "png",
  width = 11,
  height = 7,
  units = "in",
  dpi = 500)

### Fires ----
#### 2017 Map ----
# Extract fire data from Baseline Ownership scenario
myScenario <- scenario(myProject, resultScenarios$ScenarioID[1] %>% as.numeric())
fire2017 <- datasheetSpatRaster(myScenario,
                                datasheet = "stsim_OutputSpatialTransition",
                                column = "Filename",
                                iteration = 1,
                                timestep = 2017,
                                filterColumn = "TransitionGroupID",
                                filterValue = "Disturbance: Fire [Type]") %>% # ID = 51
  as.data.frame(xy = TRUE) %>% 
  rename(Fire = tg_51.it1.ts2017) %>% 
  mutate(Fire = Fire %>% as_factor())

# Get fire probability map
fireProbability <- datasheetSpatRaster(myScenario,
                                       datasheet = "stsim_OutputSpatialAverageTransitionProbability",
                                       column = "Filename",
                                       timestep = 2046,
                                       filterColumn = "TransitionGroupID",
                                       filterValue = "Disturbance: Fire [Type]") %>% # ID = 51
  as.data.frame(xy = TRUE) %>% 
  rename(FireProbability = avgtp_51.it0.ts2046) # %>% 
 # mutate(Fire = Fire %>% as_factor())

# Get base map
baseMap <- ggmap(get_map(location = c(lon = -122.016, lat = 52.041), maptype = "hybrid", zoom = "auto",))

# Create map plot
fire2017Map <- fire2017 %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = Fire)) + 
  scale_fill_manual(values = c("grey95", "red"), 
                    labels = c("No Fire", "Fire")) +
  labs(caption = "2017 Fire Extent", tag = "a") +
  theme_void() +
  theme(plot.caption = element_text(hjust = 0.5, vjust = 8, size = rel(1.2)),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))
       # legend.key = element_rect(colour = "black", linewidth = 1))

# Define colour ramp
fire_colors <- colorRampPalette(
  c("grey100", "#fdbb84", "#fc8d59", "#d7301f", "#990000"))

fire2046Map <- fireProbability %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = FireProbability)) + 
  scale_fill_stepsn(name = "Mean fire probability",
                    colours = fire_colors(4),
                    breaks = c(0.0025, 0.005, 0.025, 0.035),
                    limits = c(0,0.055)) +
  labs(caption = "Annual Fire Probability (2016 to 2046)", tag = "b") +
  theme_void() +
  theme(plot.caption = element_text(hjust = 0.5, vjust = 8, size = rel(1.2)),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))


#### Timeseries chart ----
fireTimeseries <-  read_csv(file.path("Model-Outputs", "fire-transition-output.csv")) %>% 
  pivot_longer(cols = contains("Mean"),
               names_to = "ScenarioID",
               values_to = "Mean") %>% 
  dplyr::select(Timestep, ScenarioID, Mean) %>% 
  bind_cols(
    read_csv(file.path("Model-Outputs", "fire-transition-output.csv")) %>% 
      pivot_longer(cols = contains("Lower"),
                   names_to = "ScenarioIDLower",
                   values_to = "Lower") %>% 
      dplyr::select(Lower)) %>% 
  bind_cols(
    read_csv(file.path("Model-Outputs", "fire-transition-output.csv")) %>% 
      pivot_longer(cols = contains("Upper"),
                   names_to = "ScenarioIDUpper",
                   values_to = "Upper") %>% 
      dplyr::select(Upper)) %>% 
  mutate(ScenarioID = ScenarioID %>% str_remove("\\["),
         ScenarioID = ScenarioID %>% str_remove("\\] - Mean")) %>% 
  left_join(resultScenarios) %>% 
  filter(Timestep != 2016)

fireTimeseries <- fireTimeseries %>% 
  bind_rows(
    fireTimeseries %>% 
      filter(ScenarioID == 247) %>% 
      mutate(Mean = case_when(Timestep <= 2021 ~ Mean),
             Lower = case_when(Timestep <= 2021 ~ Lower),
             Upper = case_when(Timestep <= 2021 ~ Upper),
             ScenarioName = "Historic",
             ScenarioID = "000")) %>% 
  mutate(ScenarioName = ScenarioName %>% 
           factor(levels = labels))

fireLabels <- c("Business as Usual", "Business as Usual, Doubled Fire", "Historic")

fireChart <- fireTimeseries %>% 
  filter(ScenarioID == "247" | ScenarioID == "250" | ScenarioID == "000") %>% 
  ggplot(aes(x = Timestep, y = Mean, group = ScenarioName, colour = ScenarioName)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, group = ScenarioName, fill = ScenarioName), 
              alpha = 0.15,
              show.legend = TRUE) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#d95f02", "#e3ac81", "grey50"),
                     name = "Scenarios",
                     limits = fireLabels) +
  scale_fill_manual(values = c("#d95f02", "#e3ac81", "grey50"),
                    name = "Scenarios",
                    limits = fireLabels) +
  ylab("Mean annual area burned (ha)") +
  xlab("Year") + 
  labs(tag = "c") +
  theme_bw()
  
#firePlot2Panel <- wrap_plots(fire2017Map, fireChart, ncol = 1, nrow = 2)
firePlot3Panel <- wrap_plots(fire2017Map, fire2046Map, fireChart, ncol = 1, nrow = 3)

# ggsave(
#   filename = file.path(figuresDir, "fire2Panel.png"),
#   plot = firePlot2Panel,
#   device = "png",
#   width = 9,
#   height = 10,
#   dpi = 500)

ggsave(
  filename = file.path(figuresDir, "fire3Panel.png"),
  plot = firePlot3Panel,
  device = "png",
  width = 9,
  height = 15,
  dpi = 500)



## Habitat plots ----
### Tabular ----
# Get tabular habitat output
habitatOutput <- read_csv(file.path("Model-Outputs", "habitat-output.csv")) %>% 
  pivot_longer(
    cols = c(-Scenario, -Iteration, -Timestep),
    names_to = "Species",
    values_to = "Amount")

habitatOutput2016 <- habitatOutput %>% 
  filter(Timestep == 2016)

habitatOutput2026 <- habitatOutput %>% 
  filter(Timestep == 2026)

habitatOutput2046 <- habitatOutput %>% 
  filter(Timestep == 2046)

## Data Preprocessing ----
### Calculate tabular habitat change ----
habitatOutput30yrs <- habitatOutput2016 %>% 
  left_join(habitatOutput2046, 
            by = join_by(Scenario, Iteration, Species)) %>% 
  rename("InitialTimestep" = `Timestep.x`,
         "InitialAmount" = `Amount.x`,
         "FinalTimestep" = `Timestep.y`,
         "FinalAmount" = `Amount.y`) %>% 
  group_by(Scenario, Iteration, Species) %>% 
  summarise(InitialAmount = sum(InitialAmount),
            FinalAmount = sum(FinalAmount)) %>% 
  ungroup() %>% 
  mutate(HabitatChange = FinalAmount - InitialAmount,
         Scenario = Scenario %>% as.character()) %>% 
  left_join(resultScenarios, by = join_by(Scenario == ScenarioID))

## Generate box plots ----
# Axis breaks retuned (Oct 2026) for the latest habitat results: only the American
# three-toed woodpecker has a gap worth breaking (one extreme outlier of ~15,800 ha)
yaxisBreaks <-
  list(c(8000, 15000),
       c(NA),
       c(NA),
       c(NA),
       c(NA),
       c(NA),
       c(NA))

tickLabels <-
  list(c(15500, 8000, 6000, 4000, 2000, 0, -2000, -4000, -6000),
       c(NA),
       c(NA),
       c(NA),
       c(NA),
       c(NA),
       c(NA))

habitatPlots <- list()

for(i in seq(1:length(species$SpeciesName))){

  aSpecies <- species$SpeciesName[i]
  yaxisBreak <- yaxisBreaks[i]
  tickLabel <- tickLabels[[i]]

  if(aSpecies == "Downy woodpecker" |
     aSpecies == "Hairy woodpecker" |
     aSpecies == "Northern flicker" |
     aSpecies == "Pileated woodpecker" |
     aSpecies == "Red-breasted nuthatch" |
     aSpecies == "Red-naped sapsucker") {

    habitatPlot <- habitatOutput30yrs %>%
      filter(Species == aSpecies) %>%
      ggplot(aes(x = ScenarioName, y = HabitatChange, fill = ScenarioName)) +
      geom_boxplot(lwd = 0.1, outlier.size = 0.) +
      scale_fill_manual(values = c("#1b9e77", "#b0f7e2", "#d95f02", "#e3ac81", "#6c66b0", "#b8b3f5")) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      facet_wrap(~Species, nrow = 2, ncol = 4) +
      #ylab("Habitat Change 2016 to 2046 (ha)") + 
      theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title = element_blank(),
            axis.text.y.right = element_blank(), 
            axis.ticks.y.right = element_blank(),
            legend.position = "none",
            axis.text.y = element_text(size = 4.5))

    habitatPlots[[i]] <- habitatPlot
    
    speciesName <- aSpecies %>% 
      str_to_lower() %>% 
      str_replace(" ", "-")
    
    outputFilename <- str_c("habitat-change-30yrs-", speciesName, ".png")
    
    ggsave(file.path(figuresDir, outputFilename),
           habitatPlot,
           width = 2.75,
           height = 2.5,
           dpi = 500)

  } else {

    habitatPlot <- habitatOutput30yrs %>%
      filter(Species == aSpecies) %>%
      ggplot(aes(x = ScenarioName, y = HabitatChange, fill = ScenarioName)) +
      geom_boxplot(lwd = 0.1, outlier.size = 0.) +
      scale_fill_manual(values = c("#1b9e77", "#b0f7e2", "#d95f02", "#e3ac81", "#6c66b0", "#b8b3f5")) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      facet_wrap(~Species, nrow = 2, ncol = 4) +
      scale_y_continuous(breaks = tickLabel) +
      scale_y_break(yaxisBreak) +
      #ylab("Habitat Change 2016 to 2046 (ha)") + 
      theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title = element_blank(),
            axis.text.y.right = element_blank(), 
            axis.ticks.y.right = element_blank(),
            legend.position = "none",
            axis.text.y = element_text(size = 4.5))

    habitatPlots[[i]] <- habitatPlot
    
    speciesName <- aSpecies %>% 
      str_to_lower() %>% 
      str_replace(" ", "-")
    
    outputFilename <- str_c("habitat-change-30yrs-", speciesName, ".png")
    
    ggsave(file.path(figuresDir, outputFilename),
           habitatPlot,
           width = 2.75,
           height = 2.5,
           dpi = 500)
    
  }
}

# Move the legend into the empty eighth facet cell (7 species in a 2 x 4 grid), left-aligned
# with the left edge of the Northern Flicker panel directly above it
legendInEmptyPanel <- function(plot) {
  legend <- gtable::gtable_filter(ggplotGrob(plot + theme(legend.position = "right")), "guide-box-right")
  gt <- ggplotGrob(plot + theme(legend.position = "none"))
  flicker <- gt$layout[gt$layout$name == "panel-4-1", ]
  empty <- gt$layout[gt$layout$name == "panel-4-2", ]
  legend$vp <- grid::viewport(x = 0, just = c("left", "centre"),
                              width = sum(legend$widths), height = sum(legend$heights))
  gtable::gtable_add_grob(gt, legend, t = empty$t, b = empty$b, l = flicker$l, r = flicker$r,
                          clip = "off", name = "legend-in-empty-panel")
}

# No broken axes
habitatChange30yrsPlotVaryingScales <- habitatOutput30yrs %>%
  ggplot(aes(x = ScenarioName, y = HabitatChange, fill = ScenarioName)) +
  geom_boxplot(lwd = 0.1, outlier.size = 0.) +
  scale_fill_manual(values = c("#1b9e77", "#b0f7e2", "#d95f02", "#e3ac81", "#6c66b0", "#b8b3f5"),
                    labels = scales::label_wrap(28)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~Species, scale = "free_y", nrow = 2, ncol = 4) +
  ylab("Habitat Change 2016 to 2046 (ha)") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        # legend title is the same size as the y-axis title; labels and keys are scaled to match
        legend.title = element_text(size = rel(1)),
        legend.text = element_text(size = rel(0.875)),
        legend.key.size = unit(0.55, "cm"),
        legend.margin = margin(0, 0, 0, 0))

# ggsave(file.path(figuresDir, "habitat-change-30yrs-ha-boxplot.png"),
#        habitatChange30yrsPlot,
#        width = 10,
#        height = 6,
#        dpi = 300)

habitatChange30yrsPlotVaryingScales <- legendInEmptyPanel(habitatChange30yrsPlotVaryingScales)

ggsave(file.path(figuresDir, "habitat-change-30yrs-ha-boxplot-varying-scales.png"),
       habitatChange30yrsPlotVaryingScales,
       width = 12,
       height = 5,
       dpi = 500)

### 20 Year Period ----
habitatOutput20yrs <- habitatOutput2026 %>% 
  left_join(habitatOutput2046, 
            by = join_by(Scenario, Iteration, Species)) %>% 
  rename("InitialTimestep" = `Timestep.x`,
         "InitialAmount" = `Amount.x`,
         "FinalTimestep" = `Timestep.y`,
         "FinalAmount" = `Amount.y`) %>% 
  group_by(Scenario, Iteration, Species) %>% 
  summarise(InitialAmount = sum(InitialAmount),
            FinalAmount = sum(FinalAmount)) %>% 
  ungroup() %>% 
  mutate(HabitatChange = FinalAmount - InitialAmount,
         Scenario = Scenario %>% as.character()) %>% 
  left_join(resultScenarios, by = join_by(Scenario == ScenarioID))

# Axis breaks retuned (Oct 2026) for the latest habitat results: only the American
# three-toed woodpecker has a gap worth breaking (one extreme outlier of ~11,600 ha)
yaxisBreaks <-
  list(c(6000, 11000),
       c(NA),
       c(NA),
       c(NA),
       c(NA),
       c(NA),
       c(NA))

tickLabels <-
  list(c(11500, 6000, 4000, 2000, 0, -2000, -4000),
       c(NA),
       c(NA),
       c(NA),
       c(NA),
       c(NA),
       c(NA))

habitatPlots <- list()

for(i in seq(1:length(species$SpeciesName))){
  
  aSpecies <- species$SpeciesName[i]
  yaxisBreak <- yaxisBreaks[i]
  tickLabel <- tickLabels[[i]]
  
  if(aSpecies == "Downy woodpecker" |
     aSpecies == "Hairy woodpecker" |
     aSpecies == "Northern flicker" |
     aSpecies == "Pileated woodpecker" |
     aSpecies == "Red-breasted nuthatch" |
     aSpecies == "Red-naped sapsucker") {
    
    habitatPlot <- habitatOutput20yrs %>%
      filter(Species == aSpecies) %>%
      ggplot(aes(x = ScenarioName, y = HabitatChange, fill = ScenarioName)) +
      geom_boxplot(lwd = 0.1, outlier.size = 0.) +
      scale_fill_manual(values = c("#1b9e77", "#b0f7e2", "#d95f02", "#e3ac81", "#6c66b0", "#b8b3f5")) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      facet_wrap(~Species, nrow = 2, ncol = 4) +
      #ylab("Habitat Change 2016 to 2046 (ha)") + 
      theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title = element_blank(),
            axis.text.y.right = element_blank(), 
            axis.ticks.y.right = element_blank(),
            legend.position = "none",
            axis.text.y = element_text(size = 4.5))
    
    habitatPlots[[i]] <- habitatPlot
    
    speciesName <- aSpecies %>% 
      str_to_lower() %>% 
      str_replace(" ", "-")
    
    outputFilename <- str_c("habitat-change-20yrs-", speciesName, ".png")
    
    ggsave(file.path(figuresDir, outputFilename),
           habitatPlot,
           width = 2.75,
           height = 2.5,
           dpi = 500)
    
  } else {
    
    habitatPlot <- habitatOutput20yrs %>%
      filter(Species == aSpecies) %>%
      ggplot(aes(x = ScenarioName, y = HabitatChange, fill = ScenarioName)) +
      geom_boxplot(lwd = 0.1, outlier.size = 0.) +
      scale_fill_manual(values = c("#1b9e77", "#b0f7e2", "#d95f02", "#e3ac81", "#6c66b0", "#b8b3f5")) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      facet_wrap(~Species, nrow = 2, ncol = 4) +
      scale_y_continuous(breaks = tickLabel) +
      scale_y_break(yaxisBreak) +
      #ylab("Habitat Change 2016 to 2046 (ha)") + 
      theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title = element_blank(),
            axis.text.y.right = element_blank(), 
            axis.ticks.y.right = element_blank(),
            legend.position = "none",
            axis.text.y = element_text(size = 4.5))
    
    habitatPlots[[i]] <- habitatPlot
    
    speciesName <- aSpecies %>% 
      str_to_lower() %>% 
      str_replace(" ", "-")
    
    outputFilename <- str_c("habitat-change-20yrs-", speciesName, ".png")
    
    ggsave(file.path(figuresDir, outputFilename),
           habitatPlot,
           width = 2.75,
           height = 2.5,
           dpi = 500)
    
  }
}





# habitatChange20yrsPlot <- habitatOutput20yrs %>% 
#   ggplot(aes(x = Scenario, y = HabitatChange, fill = Scenario)) +
#   geom_boxplot(lwd = 0.1, outlier.size = 0.) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   facet_wrap(~Species) +
#   ylab("Habitat Change 2026 to 2046 (ha)") + 
#   theme_bw() +
#   theme(axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.title.x = element_blank())

# Axis break for the American three-toed woodpecker panel of the overview plot: one large outlier
# (BAU, doubled fire) leaves an empty stretch between 6,000 and 10,000 ha, so that stretch is squashed
# to a thin gap. Values up to breakLower are unchanged and values above breakUpper are shifted down,
# the axis is still labelled with the real values. Only this species has data above 6,000 ha, so the
# other panels are unaffected.
breakLower <- 6000
breakUpper <- 10000
breakGap <- 900   # height (in ha) of the gap that remains on the axis (tall enough for the two bound labels)
axisBreakTrans <- scales::new_transform(
  "axis_break",
  transform = function(y) {
    ifelse(y <= breakLower, y,
           ifelse(y >= breakUpper, y - (breakUpper - breakLower) + breakGap,
                  breakLower + (y - breakLower) * breakGap / (breakUpper - breakLower)))
  },
  inverse = function(y) {
    ifelse(y <= breakLower, y,
           ifelse(y >= breakLower + breakGap, y + (breakUpper - breakLower) - breakGap,
                  breakLower + (y - breakLower) * (breakUpper - breakLower) / breakGap))
  },
  breaks = function(limits) {
    b <- scales::extended_breaks()(limits)
    b <- b[b <= breakLower | b >= breakUpper]   # no tick labels inside the break
    if(max(limits) > breakLower) {              # panels that reach the break also label its two bounds
      b <- b[abs(b - breakLower) > 1000 & abs(b - breakUpper) > 1000]
      b <- c(b, breakLower, breakUpper)
    }
    sort(b)
  })

habitatChange20yrsPlotVaryingScales <- habitatOutput20yrs %>% 
  ggplot(aes(x = ScenarioName, y = HabitatChange, fill = ScenarioName)) +
  # white band drawn over the squashed stretch of the axis (American three-toed woodpecker panel only)
  geom_rect(data = tibble(Species = "American three-toed woodpecker"),
            aes(xmin = -Inf, xmax = Inf, ymin = breakLower, ymax = breakUpper),
            inherit.aes = FALSE, fill = "white") +
  geom_boxplot(lwd = 0.1, outlier.size = 0.) +
  scale_fill_manual(values = c("#1b9e77", "#b0f7e2", "#d95f02", "#e3ac81", "#6c66b0", "#b8b3f5"),
                    labels = scales::label_wrap(28)) +
  scale_y_continuous(transform = axisBreakTrans) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~Species, scale = "free_y", nrow = 2, ncol = 4) +
  ylab("Habitat Change 2026 to 2046 (ha)") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        # legend title is the same size as the y-axis title; labels and keys are scaled to match
        legend.title = element_text(size = rel(1)),
        legend.text = element_text(size = rel(0.875)),
        legend.key.size = unit(0.55, "cm"),
        legend.margin = margin(0, 0, 0, 0))

habitatChange20yrsPlotVaryingScales <- legendInEmptyPanel(habitatChange20yrsPlotVaryingScales)

ggsave(file.path(figuresDir, "habitat-change-20yrs-ha-boxplot-varying-scales.png"),
       habitatChange20yrsPlotVaryingScales,
       width = 12,
       height = 5,
       dpi = 500)
