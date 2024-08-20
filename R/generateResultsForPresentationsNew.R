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
  googleFolder <- "1P31iMp_-Q1HXLcx9gQ5AhxMvk5e6GQOX"
  
  # Load disturbances and predictedCaribou
  allScenarios <- c("Current")
  # allScenarios <- c("Current", "Double", "Triple") ############################## UNCOMMENT WHEN TESTS ARE DONE
  allPopScenarios <- c("bottom_10%")
  # allPopScenarios <- c("top_10%", "bottom_10%") ############################## UNCOMMENT WHEN TESTS ARE DONE
  
  
  source("~/projects/anthroDisturbance_NT/R/outterFuns.R")
  studyArea <- terra::aggregate(reproducible::Cache(studyAreaGenerator,
                                                                url = "https://drive.google.com/file/d/1x_fQEKHW2nGbqo1JvCpDwmVuTPYAavl3",
                                                                archive = "BIO_ENR_WFE_BorealCaribou_RangePlanRegions_2020.zip",
                                                                targetFile = "BIO_ENR_WFE_BorealCaribou_RangePlanRegions_2020.shp",
                                                                large = FALSE, 
                                                                destPath = caribouFolder))
  studyAreaArea <- terra::expanse(terra::aggregate(studyArea), unit = "km", transform = FALSE)
  studyAreaHerds <- reproducible::Cache(herdsStudyArea, destPath = caribouFolder)
  studyAreaHerdsArea <- terra::expanse(terra::aggregate(studyAreaHerds), unit = "km", transform = FALSE)
  shortProvinceName = "NT"
  climateScenario <- "CanESM5_SSP370"
  replicateRun <- "run01" # run02, run03, run04, run05
  # replicateRun <- "run01" # run02, run03, run04, run05  ############################## UNCOMMENT WHEN TESTS ARE DONE
  calculateForHerds <- TRUE  
  # One table disturbances per herd, polygon and year # NOTE: ONLY WORKS IF SIMULATION HAS JUST BEEN RAN... 
  # External point is not valid error. Sigh.
  totalDisturbance <- rbindlist(lapply(allScenarios, function(Scenario){
    DT <- rbindlist(lapply(allPopScenarios, function(PopScenario){
      ScenarioB <- switch(Scenario, 
                          "Current" = 0.2, 
                          "Double" = 0.4, 
                          "Triple" = 0.6)
      # disturbanceScenario <- paste0(ScenarioB, "_herds")
      disturbanceScenario <- paste0(ScenarioB, "_NT1")
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
  disturbance <- rbindlist(lapply(allScenarios, function(Scenario) {
    DT <- rbindlist(lapply(allPopScenarios, function(PopScenario) {
      ScenarioB <- switch(
        Scenario,
        "Current" = 0.2,
        "Double" = 0.4,
        "Triple" = 0.6
      )
      # disturbanceScenario <- paste0(ScenarioB, "_herds")
      disturbanceScenario <- paste0(ScenarioB, "_NT1")
      runName <-
        paste(shortProvinceName,
              climateScenario,
              disturbanceScenario,
              replicateRun,
              sep = "_")
      fls <- list.files(file.path(caribouFolder, runName),
                        pattern = "bufferedAnthDist_500m",
                        full.names = TRUE)
      allDist <- raster::stack(lapply(rev(fls), raster))
      if (TRUE) {
        tempRas <- allDist[[1]]
        tempRas[!is.na(tempRas)] <- 0
        
        tempRas[allDist[[1]] == 1] <- 1
        tempRas[allDist[[2]] == 1] <- 2
        tempRas[allDist[[3]] == 1] <- 3
        tempRas[allDist[[4]] == 1] <- 4
        tempRas[allDist[[5]] == 1] <- 5
        
        r <- tempRas
        r <- as.factor(r)
        rat <- levels(r)[[1]]
        rat[["YearDisturbance"]] <-
          c("no disturbance", "2051", "2041", "2031", "2021", "up to 2011")
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
          ggtitle(paste0(
            "Disturbances generated -- ",
            ScenarioB,
            "% of study area per year"
          ))
        
        ggsave(
          filename = paste0("herds_", Scenario, ".png"),
          plot = p,
          device = "png",
          path = caribouFolder
        )
      }
      if (calculateForHerds) {
        # Need to crop to the herds and maks the rest out and only then extract the values
        allDist2 <- postProcessTo(allDist, studyAreaHerds)
        allDist <- allDist2
      }
      
      tb41 <- table(allDist[[1]][])
      tb31 <- table(allDist[[2]][])
      tb21 <- table(allDist[[3]][])
      tb11 <- table(allDist[[4]][])
      tb01 <- table(allDist[[5]][])
      
      # % Disturbance across the whole area
      Dist <- data.table(
        Year = c(2011, 2021, 2031, 2041, 2051),
        disturbedPixels = c(tb01["1"], tb11["1"], tb21["1"], tb31["1"], tb41["1"])
      )
      pixTot <- if (calculateForHerds) 1673994 else 7094697
      Dist[, totalPix := pixTot] # Full study area = 7094697, herds = 1673994
      Dist[, newPercDist := (disturbedPixels / totalPix) * 100]
      Dist[, Scenario := Scenario]
      return(Dist)
    }))
  }))
  disturbance <- unique(disturbance)
  # EXTRA
  for (i in 2:NROW(disturbance))
    disturbance[i, prevDist := disturbance[i-1, newPercDist]]
  disturbance[,diffGrowth := newPercDist-prevDist]
  disturbance[, averagePercDistPerYear := round(sum(disturbance$diffGrowth, na.rm = TRUE)/40, 2)]
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
                  "forestry_cutblocks", "oilGas_oilGas", "roads_roads", 
                  "oilGas_pipeline", "mining_mining", "Energy_powerLines")
  #"Energy_windTurbines" doesn't exist in the beginning in the area. Makes no sense to calculate it.
  yearStart <- "IC"
  yearStartNum <- 2011
  yearEnd <- 2051
  ScenarioC <- "NT1"
  disturbancePerSector <- rbindlist(lapply(allScenarios, function(Scenario){
      DT2 <- rbindlist(lapply(allSectors, function(sectr){
        message(paste0("Building disturbance table for ", sectr))
        filenameDT <- file.path(caribouFolder, paste0(Scenario,
                                                      "_", sectr,"_disturbanceTable.csv"))
        if (!exists(filenameDT)){
        ScenarioB <- switch(Scenario,
                            "Current" = 0.2,
                            "Double" = 0.4,
                            "Triple" = 0.6)
        disturbanceScenario <- paste(ScenarioB, ScenarioC, sep = "_")
        folderRunName <- paste(shortProvinceName, climateScenario, 
                         disturbanceScenario, replicateRun, sep = "_")
        runName <- paste(shortProvinceName, climateScenario, 
                         disturbanceScenario, "run01", sep = "_") # SHORTCUT WHEN FOLDER DOESN'T HAVE THE SAME NAME AS FILES -- REVIEW!
        filePathStart <- list.files(path = file.path(caribouFolder, folderRunName), 
                                 pattern = paste0("disturbances_", sectr, "_",yearStart,"_",
                                                  runName, ".shp"), 
                                 full.names = TRUE)
        filePathEnd <- list.files(path = file.path(caribouFolder, folderRunName), 
                                 pattern = paste0("disturbances_", sectr, "_",yearEnd,"_",
                                                  runName, ".shp"), 
                                 full.names = TRUE)
        if (length(filePathEnd) == 0){
          filePathEnd <- list.files(path = file.path(caribouFolder, folderRunName), 
                                   pattern = paste0("disturbances_", sectr, "_",yearEnd,"_",
                                                    runName, ".tif"), 
                                   full.names = TRUE)
          shpEnd <- rast(filePathEnd)
          shpEnd[shpEnd == 0] <- NA
          shpEnd <- as.polygons(shpEnd)
          
        } else {
          shpEnd <- vect(filePathEnd)
        }
        shpStart <- vect(filePathStart)
        # Buffer 500
        shpBStart <- terra::buffer(shpStart, width = 500)
        shpBEnd <- terra::buffer(shpEnd, width = 500)
        # Aggregate
        shpBAStart <- terra::aggregate(shpBStart, dissolve = TRUE)
        shpBAEnd <- terra::aggregate(shpBEnd, dissolve = TRUE)
        # Herds area
        
        shpBAStartH <- reproducible::postProcessTo(shpBAStart, studyAreaHerds)
        shpBAEndH <- reproducible::postProcessTo(shpBAEnd, studyAreaHerds)
        
        # Calculate Area
        areaStart <- terra::expanse(shpBAStart, unit = "km", transform = FALSE)
        areaEnd <- terra::expanse(shpBAEnd, unit = "km", transform = FALSE)
        areaStartH <- terra::expanse(shpBAStartH, unit = "km", transform = FALSE)
        areaEndH <- terra::expanse(shpBAEndH, unit = "km", transform = FALSE)
        
        
        totDistStart <- round(100*(areaStart/studyAreaArea),2)
        totDistEnd <- round(100*(areaEnd/studyAreaArea),2)
        changeOverArea <- (totDistEnd-totDistStart)
        yearlyOverArea <- (totDistEnd-totDistStart)/(yearEnd-yearStartNum) # Years
        percIncreaseDist <- 100*(totDistStart/totDistEnd)
        yearlyPercIncreaseDist <- percIncreaseDist/(yearEnd-yearStartNum)
        yearlyPropIncreaseDist <- yearlyPercIncreaseDist/100
        
        totDistStartH <- round(100*(areaStart/studyAreaHerdsArea),2)
        totDistEndH <- round(100*(areaEnd/studyAreaHerdsArea),2)
        changeOverAreaH <- (totDistEnd-totDistStart)
        yearlyOverAreaH <- (totDistEnd-totDistStart)/(yearEnd-yearStartNum) # Years
        percIncreaseDistH <- 100*(totDistStart/totDistEnd)
        yearlyPercIncreaseDistH <- percIncreaseDistH/(yearEnd-yearStartNum)
        yearlyPropIncreaseDistH <- yearlyPercIncreaseDistH/100
        
        DT <- data.table(Sector = sectr,
                         areaStart = areaStart,
                         areaEnd = areaEnd,
                         totDistStart = totDistStart,
                         totDistEnd = totDistEnd,
                         changeOverStudyArea = changeOverArea,
                         yearlyChangeOverStudyArea = round(yearlyOverArea, 4),
                         percIncreaseDist = round(percIncreaseDist, 4),
                         yearlyPercIncreaseDistTotalArea = yearlyPercIncreaseDist,
                         yearlyPropIncreaseDistTotalArea = yearlyPropIncreaseDist,
                         
                         areaStartHerds = areaStartH,
                         areaEndHerds = areaEndH,
                         totDistStartHerds = totDistStartH,
                         totDistEndHerds = totDistEndH,
                         changeOverHerdsArea = changeOverAreaH,
                         yearlyChangeOverHerdsArea = round(yearlyOverAreaH, 4),
                         percIncreaseDistHerdsArea = round(percIncreaseDistH, 4),
                         yearlyPercIncreaseDistHerdsArea = yearlyPercIncreaseDistH,
                         yearlyPropIncreaseDistHerdsArea = yearlyPropIncreaseDistH)
        
        write.csv(x = DT, file = filenameDT)
        } else {
          DT <- data.table::fread(filenameDT)
        }
        return(DT)
        }))
      return(DT2)
  }))
  write.csv(x = disturbancePerSector, file = file.path(caribouFolder, "disturbanceSectorsAccurate.csv"))
  
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
  

}


# Assess the total disturbance 

