replaceListShp <- function(disturbanceList, 
                        updatedLayers){
  externalLays <- lapply(names(disturbanceList), function(Sector) {
    # Layers available for updates
    upToDate <- updatedLayers[which(names(updatedLayers) == Sector)] # As I have repeated names (i.e., for 
    # energy I have 2 layers named Energy, one for windTurbines and one for powerLines), I need a 
    # more specific approach to select the layers. Just referencing to the Sector leaves the second
    # one out.
    if (is.null(unlist(upToDate, use.names = FALSE))){
      layName <- names(upToDate)
      # Currently disturbed NULL
      unified <- disturbanceList[[Sector]][[layName]]
      return(unified)
    } else {
      names(upToDate) <- NULL # We need to remove names and unlist in case these two lists repeat them.
      upToDate <- unlist(upToDate)
      # Merge the updated lists with the current disturbance ones:
      # Get all potential names. Will be used internally and externally in the lapply
      potentialLayName <- names(disturbanceList[[Sector]])[grepl(x = names(disturbanceList[[Sector]]), 
                                                                 pattern = "potential")]
      internalLays <- lapply(names(upToDate), function(layName) {
        # Currently disturbed 
        pastDist <- disturbanceList[[Sector]][[layName]]
        # Updated disturbances
        currDist <- upToDate[[layName]]
        
        if (any(is.null(pastDist),
                is.null(currDist))) { # If one is missing, just return the other
          unified <- c(pastDist, currDist)[[1]]
        } else { # Need to merge them!
          # NOTE: Some "lines" (i.e., seismicLines) become polygons as there is no generation of 
          # new lines, but instead the enlargement of the currently existing ones.
          # Other lines, however (i.e., roads) don't need to become polygons as they don't need to be used 
          # as such. In the case of the seismic lines, we can easily just replace the layers, as the previous
          # will contain the current. No actual "merge" is necessary.
          if (class(pastDist) != class(currDist)){ # Objects are of different classes
            # This is likely for generated stuff (raster) from polygons of potential
            print("Objects are of different classes. This is likely for generated stuff (raster) from polygons of potential.")
            if (all(!class(pastDist) %in% c("RasterLayer", "SpatRaster"),
                    class(currDist) %in% c("RasterLayer", "SpatRaster"))) {
              # It is considerably quicker to convert polys to rasters than the other way around
              pastDist <- sf::st_as_sf(x = pastDist)
              pastDist$Polys <- 1
              polysField <- "Polys"
              rasTemp <- if (is(currDist, "RasterLayer")) currDist else raster::raster(currDist)
              pastDist <- suppressWarnings(fasterize::fasterize(sf = st_collection_extract(pastDist, "POLYGON"),
                                                                raster = rasTemp, 
                                                                field = polysField))
              pastDist <- if (is(pastDist, "RasterLayer")) rast(pastDist) else pastDist
              unified <- currDist
              unified[pastDist == 1] <- 1
            } else {
              stop(paste0("New case of combination of past and present layers. ",
                          "Past layer of class ", class(pastDist), 
                          " while new layer of class ", class(currDist),
                          ". Code development is needed!"))
            }
          } else { # Objects share the same class
            if (class(pastDist) %in% c("RasterLayer", "SpatRaster")){
              unified <- pastDist
              unified[currDist == 1] <- 1
            } else {
              if (geomtype(pastDist) == geomtype(currDist)) { # Works only if both are vectors!
                # CASE 1: same type of geometry: means we generated the new disturbances and can just combine them:
                if (geomtype(pastDist) == "lines"){
                  tictoc::tic(paste0("Elapsed time for merging ", layName, " (", Sector, ")"))
                  unified <- rbind(pastDist, currDist)
                  #TODO Should I aggregate lines just for consistency? It also works when I don't...
                  unified <- terra::aggregate(unified) # This does lose all info and converts all geometries to one.
                  unified[["Class"]] <- as.character(unique(currDist[["Class"]]))
                  tictoc::toc()
                } else { # For polygons
                  tictoc::tic(paste0("Elapsed time for merging ", layName, " (", Sector, ")"))
                  unified <- rbind(pastDist, currDist) # Doubles the features when we need to overlay/unify these
                  unified <- terra::aggregate(unified) # This does lose all info and converts all geometries to one.
                  unified[["Class"]] <- as.character(unique(currDist[["Class"]]))
                  tictoc::toc()
                }
              } else {
                # CASE 2: different type of geometry: means we enlarged the new disturbances and need to 
                # combine them (in most cases this will likely mean replace them):
                pastDist <- terra::buffer(pastDist, width = 0.00001)
                currDist <- terra::buffer(currDist, width = 0.00001)
                tictoc::tic(paste0("Elapsed time for merging ", layName, " (", Sector, ")"))
                unified <- rbind(pastDist, currDist) # Doubles the features when we need to overlay/unify these
                unified <- terra::aggregate(unified) # This does lose all info and converts all geometries to one.
                # However, I don't think it is a problem in my case.
                # If I need to differenciate, I can work on this a
                # bit more and attribute a specific value for each
                # geometry and pass the `by` argument to the aggregate.
                unified[["Class"]] <- as.character(unique(currDist[["Class"]]))
                # Other options tried:
                # unified <- terra::union(pastDist, currDist) # Makes a list of all combinations of all geometries. Not what I want.
                # unified <- terra::intersect(pastDist, currDist) # No way! Way too long
                # unified <- c(pastDist, currDist) # Nope! Not what I am looking for  
                # unified <- rbind(pastDist, currDist) # Doubles the features when we need to overlay/unify these
                tictoc::toc()
              }
            }
          }
        }
        if (all(which(names(upToDate) == layName) == 1,
                length(potentialLayName) > 0)) {
          # if layName is the first one in the lapply, add the potential ones too
          unified <- append(disturbanceList[[Sector]][potentialLayName], unified)
        }
        return(unified)
      })
    }
    # Need to unlist whatever is listed here
    internalLays <- unlist(internalLays)
    names(internalLays) <- c(potentialLayName, names(upToDate))
    return(internalLays)
  })
  names(externalLays) <- names(disturbanceList)
  return(externalLays)
}