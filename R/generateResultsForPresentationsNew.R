# Generate results for presentations
# i.e., CIMP presentation: 
if (FALSE){
  
  library("Require")
  Require("raster")
  Require("reproducible")
  Require("rgdal")
  Require("terra")
  Require("data.table")
  Require("sf")
  Require("tictoc")
  Require("lattice")
  Require("rasterVis")
  Require("viridis")
  Require("maptools")
  Require("googledrive")
  
  grepMulti <- function(x, patterns, unwanted = NULL) {
    rescued <- sapply(x, function(fun) all(sapply(X = patterns, FUN = grepl, fun)))
    recovered <- x[rescued]
    if (!is.null(unwanted)){
      discard <- sapply(recovered, function(fun) all(sapply(X = unwanted, FUN = grepl, fun)))
      afterFiltering <- recovered[!discard]
      return(afterFiltering)
    } else {
      return(recovered)
    }
  }

  drive_auth(email = "tati.micheletti@gmail.com")
  caribouFolder <- "~/projects/anthroDisturbance_NT/outputs"
  googleFolder <- "1KANtCvdN8KvSEOokMzRkET2B-rwaI9U8"
  
  # Load disturbances and predictedCaribou
  allScenarios <- c("Current", "Double", "Triple")
  allPopScenarios <- c("top_10%", "bottom_10%")
  
  source("~/projects/anthroDisturbance_NT/R/outterFuns.R")
  studyArea <- terra::aggregate(reproducible::Cache(studyAreaGenerator,
                                                                url = "https://drive.google.com/file/d/1x_fQEKHW2nGbqo1JvCpDwmVuTPYAavl3",
                                                                archive = "BIO_ENR_WFE_BorealCaribou_RangePlanRegions_2020.zip",
                                                                targetFile = "BIO_ENR_WFE_BorealCaribou_RangePlanRegions_2020.shp",
                                                                large = FALSE, 
                                                                destPath = caribouFolder))
  studyAreaHerds <- reproducible::Cache(herdsStudyArea, destPath = caribouFolder)
  studyAreaHerdsArea <- terra::expanse(terra::aggregate(studyAreaHerds), unit = "km", transform = FALSE)
  shortProvinceName = "NT"
  climateScenario <- "CanESM5_SSP370"
  replicateRun <- "run01" # run02, run03, run04, run05

  # One table disturbances per herd, polygon and year
  totalDisturbance <- rbindlist(lapply(allScenarios, function(Scenario){
    DT <- rbindlist(lapply(allPopScenarios, function(PopScenario){
      ScenarioB <- switch(Scenario, 
                          "Current" = 0.2, 
                          "Double" = 0.4, 
                          "Triple" = 0.6)
      disturbanceScenario <- paste0(ScenarioB, "_herds")
      runName <- paste(shortProvinceName, climateScenario, disturbanceScenario, replicateRun, sep = "_")
      PopScenarioB <- switch(PopScenario, 
                             "top_10%" = "0.9-1", 
                             "bottom_10%" = "0-0.1")
      filePath <- list.files(path = file.path(caribouFolder, runName), 
                             pattern = paste0("disturbances_Q_", PopScenarioB), 
                             full.names = TRUE)
      disturbances_year2041 <- readRDS(filePath)
      DT <- rbindlist(lapply(names(disturbances_year2041), function(YEAR){
        DT <- rbindlist(lapply(names(disturbances_year2041[[YEAR]]), function(SHP){
          DT <- rbindlist(lapply(names(disturbances_year2041[[YEAR]][[SHP]]), function(HERD){
            dt <- cbind(data.table(SHAPEFILE = SHP,
                                   HERD = HERD,
                                   YEAR = YEAR,
                                   POPULATION_SCENARIO = PopScenario,
                                   DISTURBANCE_SCENARIO = Scenario), 
                        as.data.table(disturbances_year2041[[YEAR]][[SHP]][[HERD]]))
            return(dt)
          }))
        }))
      }))
    }))
  }))
  # write.csv(x = as.data.frame(totalDisturbance), file = file.path(caribouFolder, "totalDisturbances.csv"))
  boo <- rbindlist(lapply(allScenarios, function(Scenario){
    DT <- rbindlist(lapply(allPopScenarios, function(PopScenario){
      ScenarioB <- switch(Scenario, 
                          "Current" = 0.2, 
                          "Double" = 0.4, 
                          "Triple" = 0.6)
      disturbanceScenario <- paste0(ScenarioB, "_herds")
      runName <- paste(shortProvinceName, climateScenario, disturbanceScenario, replicateRun, sep = "_")
      PopScenarioB <- switch(PopScenario, 
                             "top_10%" = "0.9-1", 
                             "bottom_10%" = "0-0.1")
      filePath <- list.files(path = file.path(caribouFolder, runName), 
                             pattern = paste0("predictedCaribou_Q_", PopScenarioB), 
                             full.names = TRUE)
      predictedBoo_year2041 <- readRDS(filePath)
      DT <- rbindlist(lapply(names(predictedBoo_year2041), function(YEAR){
            dt <- cbind(data.table(YEAR = YEAR,
                                   POPULATION_SCENARIO = PopScenario,
                                   DISTURBANCE_SCENARIO = Scenario), 
                        as.data.table(predictedBoo_year2041[[YEAR]]))
            return(dt)
          }))
    }))
  }))
  boo[, average_femaleSurvival := as.numeric(average_femaleSurvival)]
  boo[, average_recruitment := as.numeric(average_recruitment)]
  boo[, stdErr_femaleSurvival := as.numeric(stdErr_femaleSurvival)]
  boo[, stdErr_recruitment := as.numeric(stdErr_recruitment)]
  boo[, annualLambda := as.numeric(annualLambda)]
  boo[, annualLambdaMax := as.numeric(annualLambdaMax)]
  boo[, annualLambdaMin := as.numeric(annualLambdaMin)]
  # write.csv(x = boo, file = file.path(caribouFolder, "populationGrowth.csv"))

  # Making a disturbance map # One example for each disturbance scenario
  disturbance <- rbindlist(lapply(allScenarios, function(Scenario){
    DT <- rbindlist(lapply(allPopScenarios, function(PopScenario){
      ScenarioB <- switch(Scenario, 
                          "Current" = 0.2, 
                          "Double" = 0.4, 
                          "Triple" = 0.6)
      disturbanceScenario <- paste0(ScenarioB, "_herds")
      runName <- paste(shortProvinceName, climateScenario, disturbanceScenario, replicateRun, sep = "_")
      fls <- list.files(file.path(caribouFolder, runName), 
                        pattern = "bufferedAnthDist_500m", 
                        full.names = TRUE)
      allDist <- raster::stack(lapply(rev(fls), raster))
      if (FALSE) {
      tempRas <- allDist[[1]]
      tempRas[!is.na(tempRas)] <- 0
      
      tempRas[allDist[[1]] == 1] <- 1
      tempRas[allDist[[2]] == 1] <- 2
      tempRas[allDist[[3]] == 1] <- 3
      tempRas[allDist[[4]] == 1] <- 4
      
      r <- tempRas
      r <- as.factor(r)
      rat <- levels(r)[[1]]
      rat[["YearDisturbance"]] <- c("no disturbance","2051", "2041","2031", "up to 2021")
      levels(r) <- rat
      
      library("Require")
      Require("ggplot2")
      Require("dplyr")
      Require("tidyverse")
      Require("sf") 
      Require("data.table") 
      Require("terra") 
      Require("spData")
      Require("tidyterra")
      
      ras <- rast(r)
      ras[ras == 0] <- NA
      
      p <- ggplot() + 
        geom_spatvector(data = studyArea) +
        geom_spatvector(data = studyAreaHerds) +
        geom_spatraster(data = ras) +
        # scale_fill_manual(breaks = c("no disturbance", "2041", "2031", "2021", "2011"),
        #                   values = c("lightgrey", "purple", "red", "orange", "yellow"),
        #                   name = "Year of Disturbance", na.value = "transparent") +
        scale_fill_discrete(name = "Year of Disturbance", na.value = "transparent") +
        ggtitle(paste0("Disturbances generated -- ", ScenarioB, "% of study area per year"))
      
      ggsave(filename = paste0("herds_", Scenario,".png"),
             plot = p,
             device = "png",
             path = caribouFolder)
    }

tb41 <- table(allDist[[1]][])
tb31 <- table(allDist[[2]][])
tb21 <- table(allDist[[3]][])
tb11 <- table(allDist[[4]][])

# % Disturbance across the whole area
      Dist <- data.table(Year = c(2011, 2021, 2031, 2041),
                         disturbedPixels = c(tb11["1"], tb21["1"], tb31["1"], tb41["1"]))
      Dist[, totalPix := 1673994]
      Dist[, newPercDist := (disturbedPixels/totalPix)*100]
      Dist[, Scenario := Scenario]
      return(Dist)
      }))
        }))
  disturbance <- unique(disturbance)
  write.csv(x = disturbance, file = file.path(caribouFolder, "simpleDisturbances.csv"))
  
  # NWT Map
  raster::plot(studyArea,
               axes = FALSE, 
               box = FALSE)
  raster::plot(studyAreaHerds, 
               col = viridisLite::viridis(NROW(studyAreaHerds)),
               axes = FALSE, 
               box = FALSE, add = TRUE)
  dev.copy(png, file.path(caribouFolder, 'HerdsMap.png'))
  dev.off()
  
  # Disturbances per sector
  allSectors <- c("settlements_settlements", "oilGas_seismicLines", 
                  "forestry_cutblocks", "oilGas_oilGas") 
  disturbancePerSector <- rbindlist(lapply("Current", function(Scenario){ #allScenarios
      DT2 <- rbindlist(lapply(allSectors, function(sectr){
        message(paste0("Building disturbance table for ", sectr))
        ScenarioB <- switch(Scenario,
                            "Current" = 0.2,
                            "Double" = 0.4,
                            "Triple" = 0.6)
        disturbanceScenario <- paste0(ScenarioB, "_herds")
        runName <- paste(shortProvinceName, climateScenario, 
                         disturbanceScenario, replicateRun, sep = "_")

        filePath11 <- list.files(path = file.path(caribouFolder, runName), 
                                 pattern = paste0("disturbances_", sectr, "_IC_",
                                                  runName, ".shp"), 
                                 full.names = TRUE)
        filePath41 <- list.files(path = file.path(caribouFolder, runName), 
                                 pattern = paste0("disturbances_", sectr, "_2041_",
                                                  runName, ".shp"), 
                                 full.names = TRUE)
        if (length(filePath41) == 0){
          filePath41 <- list.files(path = file.path(caribouFolder, runName), 
                                   pattern = paste0("disturbances_", sectr, "_2041_",
                                                    runName, ".tif"), 
                                   full.names = TRUE)
          shp2041 <- rast(filePath41)
          shp2041[shp2041 == 0] <- NA
          shp2041 <- as.polygons(shp2041)
          
        } else {
          shp2041 <- vect(filePath41)
        }
        shp2011 <- vect(filePath11)
        # Buffer 500
        shpB2011 <- terra::buffer(shp2011, width = 500)
        shpB2041 <- terra::buffer(shp2041, width = 500)
        # Aggregate
        shpBA2011 <- terra::aggregate(shpB2011, dissolve = TRUE)
        shpBA2041 <- terra::aggregate(shpB2041, dissolve = TRUE)
        # Calculate Area
        area2011 <- terra::expanse(shpBA2011, unit = "km", transform = FALSE)
        area2041 <- terra::expanse(shpBA2041, unit = "km", transform = FALSE)
        
        totDist2011 <- round(100*(area2011/studyAreaHerdsArea),2)
        totDist2041 <- round(100*(area2041/studyAreaHerdsArea),2)
        changeOverArea <- (totDist2041-totDist2011)
        yearlyOverArea <- (totDist2041-totDist2011)/30 # Years
        percIncreaseDist <- 100*(totDist2011/totDist2041)
        
        DT <- data.table(Sector = sectr,
                         area2011 = area2011,
                         area2041 = area2041,
                         totDist2011 = totDist2011,
                         totDist2041 = totDist2041,
                         changeOverArea = changeOverArea,
                         yearlyOverArea = round(yearlyOverArea, 4),
                         percIncreaseDist = round(percIncreaseDist, 4))
        return(DT)
        }))
      return(DT2)
  }))
  write.csv(x = disturbancePerSector, file = file.path(caribouFolder, "disturbanceSectors.csv"))
  
  # Caribou plots
  source("~/projects/anthroDisturbance_NT/modules/caribouPopGrowthModel/R/plotCaribouPopGrowthScenarios.R")
  
  plotsCaribouPopGrowth <- plotCaribouPopGrowthScenarios(startTime = 2011,
                                   currentTime = 2041,
                                   endTime = 2041,
                                   resultsMainFolder = caribouFolder, # Pass this if outside of module
                                   climateModel = "CanESM5_SSP370",
                                   studyArea = studyAreaHerds,
                                   predictedCaribou = NULL,
                                   studyAreaName = "NT",
                                   scenarios = c(0.2, 0.4, 0.6),
                                   runNameShort = "herds",
                                   yearSimulationStarts = 2011,
                                   reps = "run01",
                                   outputFolder = caribouFolder,
                                   timeSpan = "annual") # Optional = "timeStep" (normally every 10y)
  
  # Upload
  library("googledrive")
  fls <- list.files(path = caribouFolder, pattern = ".png", full.names = TRUE)
  fls2 <- list.files(path = caribouFolder, pattern = ".csv", full.names = TRUE)
  fls3 <- list.files(path = fld, pattern = "bufferedAnth", full.names = TRUE)
  fls <- c(fls, fls2)
  lapply(fls3, drive_upload, path = as_id(googleFolder))
  

