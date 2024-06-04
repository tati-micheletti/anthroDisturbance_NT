# # ALTERNATIVE TO CURRENT IDEA. Wanted to save the progress. WIP.
# 
# if (ORIGIN == "seismicLines"){
#   # 1.   
#   
#   
#   # 1. Here we create a random study area based on the center point for the area where 
#   #    the disturbance is more likely to happen, calling it newDist
#   #    1.1. Find the nearest feature in Lay (original seismic lines layer) to the centerPoint
#   # nearestLine <- terra::nearest(x = centerPoint, 
#   #                                     y = Lay, 
#   #                                     pairs = FALSE, 
#   #                                     lines = TRUE)
#   shortDistance <- TRUE
#   while(shortDistance){
#     #    1.2. Make sure the distance is more than 50m apart. If not, fix it.
#     nearestLinePoint <- terra::nearest(x = centerPoint, 
#                                        y = Lay, 
#                                        pairs = FALSE, 
#                                        lines = FALSE)
#     # nearestLinePoint might have NA in to_id. Not sure how this happens.
#     if (is.na(nearestLinePoint[["to_id"]])){ 
#       #  But if it doesn, we can loop through Lay and try to match the line that
#       #  matches exactly the extent of nearestLinePoint. Then we can subset this line
#       #  directly from Lay [UPDATE] This is below (commented out), unfortunately, not an option as it 
#       #  crashes RStudio.
#       # lineInQuestion <- unlist(lapply(1:NROW(Lay), function(ROW){
#       #   message(paste0("Checking feature ", ROW, " of ", NROW(Lay), "(", 
#       #                  round(100*(ROW/NROW(Lay)), 2),"%)"))
#       #   LaySub <- Lay[ROW,]
#       #   matched <- if (ext(LaySub) == ext(nearestLinePoint)) TRUE else FALSE
#       #   if (matched) return(Lay[ROW,]) else return(NULL)
#       # }))
#       # My next idea is to just generate then another point and make a lineInQuestion! Why not?!
#       # 1. Buffer the point to the length you want, can use Size!
#       centerPointBuff <- terra::buffer(centerPoint, witdth = Size)
#       browser()
#       # 2. Then extract a random point from the circle created
#       point2
#       # 3. Make the line with the first and last point! TADA.
#       lineInQuestion <- as.lines(x = rbind(centerPoint, point2))
#       # 4. Check the distance between the second point and the nearest line
#       point2Dist <- terra::nearest(x = point2, 
#                                    y = Lay, 
#                                    pairs = FALSE, 
#                                    lines = FALSE)
#       shortDistance <- any(nearestLinePoint[["distance"]] < 50,
#                            point2Dist[["distance"]] < 50)
#       existingLine <- FALSE
#     } else {
#       lineInQuestion <- Lay[nearestLinePoint[["to_id"]],]
#       shortDistance <- nearestLinePoint[["distance"]] < 50
#       existingLine <- TRUE
#     }
#     if (shortDistance)
#       centerPoint <- terra::spatSample(potLayTopValid, size = 1, method = "random")
#   }
#   #    1.3. Draw the parallel line to the chosen feature with a length of Size
#   pointsLine <- terra::as.points(lineInQuestion)
#   groupLines <- seismicGrids(Line = lineInQuestion, 
#                              centerPoint = centerPoint, 
#                              originalLayer = Lay,
#                              linesLength = dParOri[["disturbanceSize"]],
#                              howMany = c(10, 5),
#                              existingLine = existingLine)
#   
#   # 2. If this is related to buffered area, we need to buffer the line by 500m, calling 
#   #    it newDistBuff
#   
#   
#   # How do I create the new line?!?! maybe the point can be the center of it,
#   # and I just need to extend both size to Size/2?
#   # IDEAS:
#   # Find the nearest existing feature and draw a parallel line with the given length?
#   # Maybe give it an option of making batches? I feel like this might be slow...
#   
#   # newDist NEEDS to be buffered, first by 3m and then by by 500m, if disturbanceRateRelatesToBufferedArea 
#   # Although dParOri[["resolutionVector"]] has vector resolution, seismic lines are much slimmer.
#   # In the past, they used to be placed 300–500 m apart, and 5–10 m wide. 
#   # Nowadays, the average is 3m wide (2-4, usually not more than 5.5m) and about 50–100m apart 
#   # (Dabros et al., 2018 - DOI: 10.1139/er-2017-0080)
#   newDist
#   areaChosenTotal <- areaChosenTotal + terra::expanse(newDist)
#   
#   
# }