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
bufferCells <- function(ras, valToLookFor, newValue){
  rDT <- data.table(pixelID = 1:ncell(ras),
                    vals = as.numeric(values(ras)))
  whichCells <- rDT[vals == valToLookFor, pixelID]
  ADJ <- unique(na.omit(as.numeric(adjacent(ras, cells = whichCells, 
                                            directions = "queen", 
                                            include = TRUE))))
  ras[ADJ] <- newValue
  return(ras)
}

drive_auth(email = "tati.micheletti@gmail.com")
caribouFolder <- "~/projects/anthroDisturbance_NT/outputs/NT_CanESM5_SSP370_0.00625_run01/"
disturbancesFolder <- "~/projects/anthroDisturbance_NT/outputs/NT_CanESM5_SSP370_0.00625_run01/"
googleFolder <- "1i6mhPoVFPJyrXB0WCx9cLwZpPOfC0mB9"

# One table disturbances per herd, polygon and year
  DT <- rbindlist(lapply(names(disturbances_year2041), function(YEAR){
    DT <- rbindlist(lapply(names(disturbances_year2041[[YEAR]]), function(SHP){
      DT <- rbindlist(lapply(names(disturbances_year2041[[YEAR]][[SHP]]), function(HERD){
        dt <- cbind(data.table(SHAPEFILE = SHP,
                             HERD = HERD,
                             YEAR = YEAR), 
                  as.data.table(disturbances_year2041[[YEAR]][[SHP]][[HERD]]))
      return(dt)
    }))
  }))
}))

# Making a disturbance map
fls <- list.files(caribouFolder, pattern = "500m", full.names = TRUE)
fls2 <- fls[2:5]
allDist <- raster::stack(lapply(rev(fls2), raster))

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

library(raster)
library(rasterVis)
levelplot(r, col.regions = c("lightgrey", "yellow", "orange", "red", "purple"), xlab="", ylab="")

# % Disturbance across the whole area
Dist <- data.table(table(r[]))
Dist[, totalPix := sum(N)]
Dist[, Year := c(0, 2051, 2041, 2031, 2021)]
Dist[, newPercDist := (N/totalPix)*100]
Dist2 <- Dist[2:5, ]
Dist2[, V1 := NULL]
names(Dist2)[names(Dist2) == "N"] <- "disturbedPix"
setkey(Dist2, "Year")
Dist2[, cummDisturbance := cumsum(newPercDist)]

# Extract disturbances and changes across polygons
library("googledrive")
fls <- list.files(path = "~/projects/caribouPopGrowth_disturbance/outputs/run1/", pattern = ".tif", full.names = T)
fls2 <- list.files(path = "~/projects/caribouPopGrowth_disturbance/outputs/run1/", pattern = ".rds", full.names = T)
fls <- c(fls, fls2)
lapply(fls, drive_upload, path = as_id("1C68BZU6eMF1FaXCOsyVnO8BVBqEsbElB"))


# Load Disturbance FROM FILE! (NOTE: if simulations of disturbance and caribou were done together, 
# this can be updated, but will still work, provided the correct location for the disturbance files 
# was passed above to disturbancesFolder)

# Rasters: oil, mines, forestry, wind
# Shapefiles: sett, roads, seismic, pipelines, powerlines
# NOT DIRECTLY USED: otherPolys, otherLines

shapesNames <- c("settlements", "roads", "seismicLines", "pipeline", "powerLines")
rasNames <- c("oilGas", "mining", "forestry", "windTurbines")
env <- environment()

## SHAPEFILES
lapply(shapesNames, function(SHP){
  allForRas <- grepMulti(x = list.files(path = disturbancesFolder, 
                                        full.names = TRUE), 
                         patterns = c("disturbances_", SHP, ".shp"), 
                         unwanted = "_IC")
  assign(x = SHP, value = lapply(allForRas, shapefile), pos = env)
  print(paste0(SHP, " loading done!"))
})

## RASTERS
lapply(rasNames, function(RAS){
  allForRas <- grepMulti(x = list.files(path = disturbancesFolder, 
                                        full.names = TRUE), 
                         patterns = c("disturbances_", RAS, ".tif"), 
                         unwanted = ".xml")
  assign(x = RAS, value = stack(lapply(allForRas, raster)), pos = env)
  print(paste0(RAS, " loading done!"))
})

# Template Raster
templateRas <- oilGas[[1]]
templateRas[!is.na(templateRas)] <- 0
names(templateRas) <- "templateRaster"

# LOAD STUDY AREAS

SA <- prepInputs(url = "https://drive.google.com/open?id=1fYvNPwovjNtTABoGcegrvdFGkNfCUsxf", 
                 destinationPath = caribouFolder)
SA <- vect(SA)
SArep <- terra::project(SA, raster::crs(templateRas))
Ede <- buffer(SArep, width = 10^5)

Boo <- prepInputs(url = "https://drive.google.com/file/d/1x_fQEKHW2nGbqo1JvCpDwmVuTPYAavl3/view?usp=sharing", 
                  destinationPath = caribouFolder)
Boo <- vect(Boo)
Boo <- terra::project(Boo, crs(templateRas))

Herds <- prepInputs(url = "https://drive.google.com/file/d/18xEvO5ktCSRLRZC3KrsymTBMh_4MBB7Z/view?usp=sharing", 
                    destinationPath = caribouFolder)
Herds <- vect(Herds)
Herds <- terra::project(Herds, crs(templateRas))
Herds <- terra::crop(Herds, Boo)
Herds <- terra::subset(Herds, Herds$PROV_TERR %in% c("NWT"))

# Generate just maps for reference
if (FALSE){
  ## PRELIMINARY RESULTS
  # Rasters: oil, mines, forestry, wind
  # Shapefiles: sett, roads, seismic, pipelines, powerlines
  # NOT DIRECTLY USED: otherPolys, otherLines
  
  rasNames <- c("oil", "mines", "forestry", "wind")
  shapesNames <- c("sett", "roads", "seismic", "pipelines", "powerlines")
  
  # Cute NWT Map
  par(bg = NA)
  raster::plot(Boo, 
               col = "#072C62",
               axes = FALSE, 
               box = FALSE)
  legend(x = 0.7*xmax(Boo),
         y = ymax(Boo),
         legend = "Northwest\nTerritories",
         fill = "#072C62",
         cex = 0.95,
         box.lwd = 0)
  dev.copy(png,'NT_1.png')
  dev.off()
  
  par(bg = NA)
  raster::plot(Boo, 
               col = viridisLite::viridis(NROW(Boo)),
               axes = FALSE, 
               box = FALSE)
  legend(x = xmax(Boo), y = ymax(Boo), 
         legend = Boo$REGION,
         fill = viridisLite::viridis(NROW(Boo)),
         cex = 0.95,
         box.lwd = 0)
  dev.copy(png,'NT_2.png')
  dev.off()
  
  par(bg = NA)
  
  Box <- aggregate(Boo)
  raster::plot(Box, 
               col = "white",
               axes = FALSE, 
               box = FALSE)
  raster::plot(Herds, 
               add = TRUE,
               col = viridisLite::viridis(NROW(Herds)),
               axes = FALSE, 
               box = FALSE)
  dev.copy(png,'NT_4.png')
  dev.off()
  
  par(bg = NA)
  raster::plot(Boo, 
               col = viridisLite::viridis(NROW(Boo)),
               axes = FALSE, 
               box = FALSE)
  legend(x = xmax(Boo), y = ymax(Boo),
         legend = Boo$REGION,
         fill = viridisLite::viridis(NROW(Boo)),
         cex = 0.95,
         box.lwd = 0)
  dev.copy(png,'NT_6.png')
  dev.off()
  
  raster::plot(Boo, 
               col = viridisLite::viridis(NROW(Boo)),
               axes = FALSE, 
               box = FALSE)
  dev.copy(png,'NT_6.png')
  dev.off()
  
  par(bg = "white")
  
}

# 1. Current rate of change of individual sectors over the study area
if (FALSE){
  # Calculate how many 1's we have in each map
  # Calculate how many 0's we have in each map
  # Do 1 over 0 to calculate the rate of change for each of the sectors
  
  rateOfChange <- rbindlist(lapply(c(rasNames, shapesNames), function(TYPE){ 
    tic(paste0("Time elapsed for ", TYPE, ":"))
    dist <- get(TYPE)
    if (class(dist) == "RasterStack"){
      dist[] <- dist[]
      tb <- rbindlist(lapply(1:nlayers(dist), function(N){
        TB <- data.table(table(dist[[N]][]))
        TB2 <- data.table(Zero = TB$N[1],
                          One = TB$N[2])
        TB2[, Year := 2001+(N*10)]
        return(TB2)
      }))
      tb[, total := Zero + One]
      tb[, percArea := One/total]
    } else {
      tb <- rbindlist(lapply(1:length(dist), function(N){
        TB <- data.table(Zero = sum(expanse(Boo)), 
                         One = sum(expanse(vect(dist[[N]]))))
        TB[, Year := 2001+(N*10)]
        return(TB)
      }))
      tb[, percArea := One/Zero]
    }
    changePerc <- numeric(NROW(tb))
    for (i in 1:NROW(tb)){
      changePerc[i+1] <- (tb$percArea[(i+1)]-tb$percArea[(i)])/tb$percArea[(i)]
    }
    tb[, rateChange := na.omit(changePerc)]
    changePercFirst <- numeric(NROW(tb))
    for (i in 1:NROW(tb)){
      changePercFirst[i+1] <- (tb$percArea[(i+1)]-tb$percArea[(1)])/tb$percArea[(1)]
    }
    tb[, totalRateChange := na.omit(changePercFirst)]
    tb[, sector := TYPE]
    DT <- tb[, c("sector", "Year", "percArea", "rateChange", "totalRateChange")]
    toc()
    return(DT)
  }))
  changesFile <- file.path(disturbancesFolder, "rateOfChange.csv")
  options(scipen = 100)
  DT <- rateOfChange[Year %in% c(2011, 2051) & percArea > 0,]
  DT[, percArea := percArea*100]
  DT[, rateChange := rateChange*100]
  DT[, totalRateChange := totalRateChange*100]
  write.csv(DT, changesFile)
  drive_upload(media = changesFile, path = as_id(googleFolder))
}

# 2. Rate of change of buffered disturbances for all sectors within caribou ranges
# For year 2011 and 2051:
if (FALSE){
  
  # FOR SHAPEFILES, BUFFER AND CONVERT TO RASTERS 
  shpRas <- lapply(shapesNames, function(TYPE){
    # Buffer all shapefiles
    # Convert shapefiles to rasters
    tic(paste0("Time elapsed for converting ", TYPE, " from shp to raster:"))
    shp <- get(TYPE)
    first <- shp[[1]]
    last <- shp[[length(shp)]]
    bFirst <- aggregate(buffer(first, width = 500))
    bLast <- aggregate(buffer(last, width = 500))
    bFirstSF <- st_as_sf(bFirst)
    bLastSF <- st_as_sf(bLast)
    bFirstRas <- fasterize::fasterize(sf = bFirstSF, raster = templateRas)
    names(bFirstRas) <- TYPE
    bLastRas <- fasterize::fasterize(sf = bLastSF, raster = templateRas)
    names(bLastRas) <- TYPE
    toc()
    return(list(Year2011 = bFirstRas, Year2051 = bLastRas))
  })
  
  # Organize the list!
  Year2011shp <- raster::stack(lapply(shpRas, `[[`, "Year2011"))
  names(Year2011shp) <- paste0(shapesNames, "_2011")
  writeRaster(Year2011shp, filename = file.path(disturbancesFolder, "buffDistShapeAsRas_2011"), 
              format = "GTiff")
  Year2051shp <- raster::stack(lapply(shpRas, `[[`, "Year2051"))
  names(Year2051shp) <- paste0(shapesNames, "_2051")
  writeRaster(Year2051shp, filename = file.path(disturbancesFolder, "buffDistShapeAsRas_2051"), 
              format = "GTiff")
  
  # FOR RASTERS, JUST BUFFER
  ras <- lapply(rasNames, function(TYPE){
    tic(paste0("Time elapsed for buffering ", TYPE, ": "))
    r <- get(TYPE)
    first <- r[[1]]
    last <- r[[nlayers(r)]]
    first <- rast(first)
    last <- rast(last)
    first[] <- first[]
    last[] <- last[]
    # bFirst <- terra::buffer(first, width = 500) # ~~~~~~~~~~~~> SUPER SLOW!!!
    # bLast <- terra::buffer(last, width = 500) # ~~~~~~~~~~~~> SUPER SLOW!!!
    
    # Below works because we are interested in 500m (ECCC guideline) which is,
    # coincidentaly, double the resolution of the current rasters (250m)
    # This should be generalized for other resolutions before used so!
    # To generalize, we need to identify the res/2 and then iterate over 
    # the number of pixels we need --> we reuse the identified adjecent cells and 
    # identify their adjecent ones until the total buffer is reached. Might end up 
    # being too time consuming, though. Should see the 'buffer' mechanism for bigger 
    # buffers. 
    bFirst <- bufferCells(ras = first, 
                          valToLookFor = 1, 
                          newValue = 1)
    bLast <- bufferCells(ras = last, 
                         valToLookFor = 1, 
                         newValue = 1)
    
    # Buffer is taking a huge amount of time. 
    # Finding the adjecent cells in all directions of the cells that are == 1
    # is much quicker. Then convert those to 1
    toc()
    return(list(Year2011 = raster(bFirst), 
                Year2051 = raster(bLast)))
  })
  
  Year2011ras <- stack(lapply(ras, `[[`, "Year2011"))
  names(Year2011ras) <- paste0(rasNames, "_2011")
  writeRaster(Year2011ras, filename = file.path(disturbancesFolder, "buffDistRas_2011"), 
              format = "GTiff")
  Year2051ras <- stack(lapply(ras, `[[`, "Year2051"))
  names(Year2051ras) <- paste0(rasNames, "_2051")
  writeRaster(Year2051ras, filename = file.path(disturbancesFolder, "buffDistRas_2051"), 
              format = "GTiff")
  
  # Join shape as rasters and other rasters
  Year2011 <- stack(c(Year2011shp, Year2011ras))
  Year2051 <- stack(c(Year2051shp, Year2051ras))
  
  # Convert all pixels that are disturbed to 1, and non-disturbed to 0
  Year2011_all <- templateRas
  for (i in 1:nlayers(Year2011)){
    tic(paste0("Updating ", names(Year2011)[i], ": "))
    Year2011_all[which(Year2011[[i]][] == 1)] <- 1
    toc()
  }
  
  Year2051_all <- templateRas
  for (i in 1:nlayers(Year2051)){
    tic(paste0("Updating ", names(Year2051)[i], ": "))
    Year2051_all[which(Year2051[[i]][] == 1)] <- 1
    toc()
  }
  
  # Summarize by range polygon in terms of pixels & range plan
  # FOR 2011 ##########################################################
  Year2011 <- rast(Year2011_all)
  # BOO SHAPEFILE
  booDist2011 <- terra::extract(x = Year2011, y = Boo)
  booDist2011_DT <- data.table(booDist2011)
  names(booDist2011_DT) <- c("groupID", "boo_2011")
  booDist2011_DT <- merge(booDist2011_DT, data.table(groupID = 1:NROW(Boo),
                                                     Boo[["REGION"]]))
  names(booDist2011_DT) <- c("groupID", "boo_2011", "REGION")
  cols <- c("notDisturbed", "disturbed")
  booDist2011_DT[, (cols) := .(sum(boo_2011 == 0, na.rm = TRUE), 
                               sum(boo_2011 == 1, na.rm = TRUE)), 
                 by = "REGION"]
  Boo_2011 <- unique(booDist2011_DT[, c("REGION", "notDisturbed", "disturbed")])
  Boo_2011[, Year := 2011]
  Boo_2011[, proportionDisturbed := disturbed/(notDisturbed+disturbed)]
  Boo_2011[, totalAreaHa := expanse(Boo, unit="ha")]
  
  # HERD SHAPEFILE
  herdsDist2011 <- terra::extract(x = Year2011, y = Herds)
  herdsDist2011 <- data.table(herdsDist2011)
  names(herdsDist2011) <- c("groupID", "herd_2011")
  herdsDist2011 <- merge(herdsDist2011, data.table(groupID = 1:NROW(Herds),
                                                   Herds[["HERD"]]))
  names(herdsDist2011) <- c("groupID", "herd_2011", "REGION")
  cols <- c("notDisturbed", "disturbed")
  herdsDist2011[, (cols) := .(sum(herd_2011 == 0, na.rm = TRUE), 
                              sum(herd_2011 == 1, na.rm = TRUE)), 
                by = "REGION"]
  Herd_2011 <- unique(herdsDist2011[, c("REGION", "notDisturbed", "disturbed")])
  Herd_2011[, Year := 2011]
  Herd_2011[, proportionDisturbed := disturbed/(notDisturbed+disturbed)]
  Herd_2011[, totalAreaHa := expanse(Herds, unit="ha")]
  
  # FOR 2051 ##########################################################
  Year2051 <- rast(Year2051_all)
  # BOO SHAPEFILE
  booDist2051 <- terra::extract(x = Year2051, y = Boo)
  booDist2051_DT <- data.table(booDist2051)
  names(booDist2051_DT) <- c("groupID", "boo_2051")
  booDist2051_DT <- merge(booDist2051_DT, data.table(groupID = 1:NROW(Boo),
                                                     Boo[["REGION"]]))
  names(booDist2051_DT) <- c("groupID", "boo_2051", "REGION")
  cols <- c("notDisturbed", "disturbed")
  booDist2051_DT[, (cols) := .(sum(boo_2051 == 0, na.rm = TRUE), 
                               sum(boo_2051 == 1, na.rm = TRUE)), 
                 by = "REGION"]
  Boo_2051 <- unique(booDist2051_DT[, c("REGION", "notDisturbed", "disturbed")])
  Boo_2051[, Year := 2051]
  Boo_2051[, proportionDisturbed := disturbed/(notDisturbed+disturbed)]
  Boo_2051[, totalAreaHa := expanse(Boo, unit="ha")]
  
  # HERD SHAPEFILE
  herdsDist2051 <- terra::extract(x = Year2051, y = Herds)
  herdsDist2051 <- data.table(herdsDist2051)
  names(herdsDist2051) <- c("groupID", "herd_2051")
  herdsDist2051 <- merge(herdsDist2051, data.table(groupID = 1:NROW(Herds),
                                                   Herds[["HERD"]]))
  names(herdsDist2051) <- c("groupID", "herd_2051", "REGION")
  cols <- c("notDisturbed", "disturbed")
  herdsDist2051[, (cols) := .(sum(herd_2051 == 0, na.rm = TRUE), 
                              sum(herd_2051 == 1, na.rm = TRUE)), 
                by = "REGION"]
  Herd_2051 <- unique(herdsDist2051[, c("REGION", "notDisturbed", "disturbed")])
  Herd_2051[, Year := 2051]
  Herd_2051[, proportionDisturbed := disturbed/(notDisturbed+disturbed)]
  Herd_2051[, totalAreaHa := expanse(Herds, unit="ha")]
  
  # JOIN ALL DATA!
  distTableRaw <- file.path(disturbancesFolder, "disturbedTableByRegionRaw.csv")
  distTableFinal <- file.path(disturbancesFolder, "disturbedTableByRegion.csv")
  DT <- rbind(Boo_2011, Boo_2051,
              Herd_2011, Herd_2051)
  write.csv(x = DT, file = distTableRaw)
  DT[, c("notDisturbed", "disturbed") := NULL]
  DTc <- dcast(DT, REGION + totalAreaHa ~ Year, value.var = "proportionDisturbed")
  DTc[, totalDisturbanceChange := ((`2051`-`2011`)/`2011`)]  # Total increase over 2011 in 40 years
  DTc[, disturbanceChangePerYear := ((`2051`-`2011`)/`2011`)/(2051-2011)] # Avergare yearly increase between 2011 and 2051 
  DTc[, disturbanceChangePerYearPerc := disturbanceChangePerYear*100]
  names(DTc)[names(DTc) == 2011] <- "disturbedPercent2011"
  names(DTc)[names(DTc) == 2051] <- "disturbedPercent2051"
  write.csv(x = DTc, file = distTableFinal)
  
  # Upload
  drive_upload(media = distTableRaw, path = as_id(googleFolder))
  drive_upload(media = distTableFinal, path = as_id(googleFolder))
}

# 3. Time series of total disturbance buffered
if (FALSE){
  # Get maps from previous calcs
  both <- Year2011+Year2051
  both <- crop(both, Boo)
  both <- mask(both, Boo)
  both[both == 2] <- 2051
  both[both == 1] <- 2011
  both[is.na(both)] <- 0
  bothRat <- as.factor(both)
  bothRat <- raster(bothRat)
  att <- "ID"
  
  Pal <- c("white", "cornflowerblue", "darkslateblue") 
  
  plot(bothRat, col = Pal)
  bothRat <- rast(bothRat)
  bothRat <- mask(bothRat, Boo)
  
  png(filename = file.path(disturbancesFolder, "totalDisturbance.png"),
      width = 21, height = 29,
      units = "cm", res = 300)
  par(bg = NA)
  raster::plot(bothRat, 
               main = "Total disturbance buffered in 2011 and 2051",
               col = c("white", "orange", "darkslateblue"),
               axes = FALSE, 
               box = FALSE)
  plot(Boo, add = TRUE, lwd = 1, 
       border = "black")
  dev.off()
  # Upload
  drive_upload(media = file.path(disturbancesFolder, "totalDisturbance.png"), 
               path = as_id(googleFolder))
}

# 4. Time series of non-buffered disturbance types (Edéhzhíe National Wildlife Area region)
if (FALSE){
  nonBufferedDist <- lapply(c(rasNames, shapesNames), function(TYPE){
    tic(paste0("Time elapsed for ", TYPE, ":"))
    dist <- get(TYPE)
    if (class(dist) == "RasterStack"){
      dist[] <- dist[]
      allYears <- lapply(rev(1:nlayers(dist)), function(INDEX){
        d <- dist[[INDEX]]
        d <- rast(d)
        # Crop and mask to Ede
        d <- crop(d, Ede)
        d <- mask(d, Ede)
        # Convert to SpatVect
        distVec <- as.polygons(d,
                               trunc = TRUE, 
                               dissolve = TRUE, 
                               values = FALSE,
                               na.rm = TRUE, 
                               na.all = TRUE, 
                               extent = FALSE)
        # Add data info
        Y <- 2001+(INDEX*10)
        distVec[["TYPE"]] <- TYPE
        distVec[["YEAR"]] <- Y 
        distVec[["Type_Year"]] <- paste0(TYPE, "_", Y)
        distVec[["sizeHa"]] <- expanse(distVec, unit = "ha")
        distVec[["Class"]] <- TYPE  
        distVec <- subset(x = distVec, subset = distVec$sizeHa == min(distVec$sizeHa), )
        return(distVec)
      })
    } else {
      allYears <- lapply(rev(1:length(dist)), function(INDEX){
        d <- dist[[INDEX]]
        if (class(d) != "SpatVector")
          d <- vect(d)
        # Crop and mask to Ede
        d <- crop(d, Ede)
        distVec <- mask(d, Ede)
        # Add data info
        Y <- 2001+(INDEX*10)
        distVec[["TYPE"]] <- TYPE
        distVec[["YEAR"]] <- Y 
        distVec[["Type_Year"]] <- paste0(TYPE, "_", Y)
        distVec[["sizeHa"]] <- expanse(distVec, unit = "ha")
        # If not polygon, convert
        if (geomtype(distVec) != "polygons")
          distVec <- buffer(distVec, width = 7.5) ## The 7.5 is the half of the 15m, resolution of original data
        return(distVec)
      })
    }
    allYearsShp <- do.call(rbind, allYears)
    toc()
    # PLOTTING
    whichPlot <- "YEAR"
    par(bg = NA)
    plot(x = allYearsShp,
         y = whichPlot,
         type = "classes",
         col = rainbow(length(unlist(allYearsShp[[whichPlot]]))),
         border = rainbow(length(unlist(allYearsShp[[whichPlot]]))),
         box = FALSE,
         axes = FALSE,
         main = TYPE
    )
    plot(Ede, add = TRUE, border = "black")
    dev.copy(png, file.path(disturbancesFolder, paste0("disturbance_",TYPE,".png")))
    dev.off()
    return(allYearsShp)
  })
  nonBufferedDist <- do.call(rbind, nonBufferedDist)
  
  writeOGR(obj = as(st_as_sf(nonBufferedDist), "Spatial"), dsn = file.path(disturbancesFolder), 
           layer = "allDisturbances_allYears", 
           driver = "ESRI Shapefile")
  
  # UPLOAD
  fls <- list.files(disturbancesFolder, full.names = TRUE, 
                    pattern = "allDisturbances_allYears")
  lapply(fls, drive_upload, path = as_id("1C68BZU6eMF1FaXCOsyVnO8BVBqEsbElB"))
  
  # Now subset year == 2011 and plot all types, and year == 20151 and plot all types
  allDist2011 <- subset(nonBufferedDist, nonBufferedDist[["YEAR"]] == 2011)
  whichPlot <- "TYPE"
  par(bg = NA)
  COLS <- c("red", "orange", "goldenrod1", "forestgreen", "blue", "navy", "purple")
  for (N in 1:length(unique(allDist2011$TYPE)[!unique(allDist2011$TYPE) %in% c("wind", 
                                                                               "powerlines")])){
    i <- unique(allDist2011$TYPE)[!unique(allDist2011$TYPE) %in% c("wind", 
                                                                   "powerlines")][N]
    print(paste0("Making the figure for ", i))
    plot.window(xlim = c(xmin(Ede), xmax(Ede)), 
                ylim = c(ymin(Ede), ymax(Ede)))
    plot(x = allDist2011[allDist2011$TYPE == i],
         y = whichPlot,
         type = "classes",
         col = COLS[N],
         border = COLS[N],
         box = FALSE,
         axes = FALSE,
         main = NA
    )
    plot(Ede, add = TRUE, border = "black")
    dev.copy(png,file.path(disturbancesFolder, paste0("disturbance_",i,"2011.png")))
    dev.off()
  }
  writeOGR(obj = as(st_as_sf(allDist2011), "Spatial"), dsn = file.path(disturbancesFolder), 
           layer = "all_Disturbances_2011", 
           driver = "ESRI Shapefile")
  
  allDist2051 <- subset(nonBufferedDist, nonBufferedDist[["YEAR"]] == 2051)
  for (N in 1:length(unique(allDist2051$TYPE)[!unique(allDist2051$TYPE) %in% c("wind", 
                                                                               "powerlines")])){
    i <- unique(allDist2051$TYPE)[!unique(allDist2051$TYPE) %in% c("wind", 
                                                                   "powerlines")][N]
    print(paste0("Making the figure for ", i))
    plot.window(xlim = c(xmin(Ede), xmax(Ede)), 
                ylim = c(ymin(Ede), ymax(Ede)))
    plot(x = allDist2051[allDist2051$TYPE == i],
         y = whichPlot,
         type = "classes",
         col = COLS[N],
         border = COLS[N],
         box = FALSE,
         axes = FALSE,
         main = NA
    )
    plot(Ede, add = TRUE, border = "black")
    dev.copy(png, file.path(disturbancesFolder, paste0("disturbance_",i,"2051.png")))
    dev.off()
  }
  writeOGR(obj = as(st_as_sf(allDist2051), "Spatial"), dsn = file.path(disturbancesFolder), 
           layer = "all_Disturbances_2051", 
           driver = "ESRI Shapefile")
  
  # UPLOAD
  fls <- list.files(disturbancesFolder, full.names = TRUE, 
                    pattern = "allDisturbances_allYears|all_Disturbances_2051|all_Disturbances_2011")
  lapply(fls, drive_upload, path = as_id("1C68BZU6eMF1FaXCOsyVnO8BVBqEsbElB"))
  
  # UPLOAD FIGURES
  fls <- list.files(disturbancesFolder, full.names = TRUE, 
                    pattern = ".png")
  lapply(fls, drive_upload, path = as_id("1C68BZU6eMF1FaXCOsyVnO8BVBqEsbElB"))
}
}