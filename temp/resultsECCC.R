# resultsECCC
# bufferedDisturbances, maskOutLinesFromPolys, aggregateSameDisturbances
# [ ] Check bufferedDisturbances = TRUE, maskOutLinesFromPolys = TRUE, aggregateSameDisturbances = TRUE
TTT <- disturbanceInfoFromECCC(studyArea = studyArea, 
                                    RTM = RTM,
                                    totalstudyAreaVAreaSqKm = totalstudyAreaVAreaSqKm,
                                    classesAvailable = classesAvailable,
                                    destinationPath = Paths$inputPath,
                                    bufferedDisturbances = TRUE,
                                    maskOutLinesFromPolys = TRUE,
                                    aggregateSameDisturbances = TRUE)
# [ ] Check bufferedDisturbances = FALSE, maskOutLinesFromPolys = FALSE, aggregateSameDisturbances = FALSE
FFF <- disturbanceInfoFromECCC(studyArea = studyArea, 
                               RTM = RTM,
                               totalstudyAreaVAreaSqKm = totalstudyAreaVAreaSqKm,
                               classesAvailable = classesAvailable,
                               destinationPath = Paths$inputPath,
                               bufferedDisturbances = FALSE,
                               maskOutLinesFromPolys = FALSE,
                               aggregateSameDisturbances = FALSE)

# [ ] Check bufferedDisturbances = TRUE, maskOutLinesFromPolys = TRUE, aggregateSameDisturbances = FALSE
TTF <- disturbanceInfoFromECCC(studyArea = studyArea, 
                               RTM = RTM,
                               totalstudyAreaVAreaSqKm = totalstudyAreaVAreaSqKm,
                               classesAvailable = classesAvailable,
                               destinationPath = Paths$inputPath,
                               bufferedDisturbances = TRUE,
                               maskOutLinesFromPolys = TRUE,
                               aggregateSameDisturbances = FALSE)
# [ ] Check bufferedDisturbances = FALSE, maskOutLinesFromPolys = FALSE, aggregateSameDisturbances = TRUE
FFT <- disturbanceInfoFromECCC(studyArea = studyArea, 
                               RTM = RTM,
                               totalstudyAreaVAreaSqKm = totalstudyAreaVAreaSqKm,
                               classesAvailable = classesAvailable,
                               destinationPath = Paths$inputPath,
                               bufferedDisturbances = FALSE,
                               maskOutLinesFromPolys = FALSE,
                               aggregateSameDisturbances = TRUE)
# [ ] Check bufferedDisturbances = TRUE, maskOutLinesFromPolys = FALSE, aggregateSameDisturbances = FALSE
TFF <- disturbanceInfoFromECCC(studyArea = studyArea, 
                               RTM = RTM,
                               totalstudyAreaVAreaSqKm = totalstudyAreaVAreaSqKm,
                               classesAvailable = classesAvailable,
                               destinationPath = Paths$inputPath,
                               bufferedDisturbances = TRUE,
                               maskOutLinesFromPolys = FALSE,
                               aggregateSameDisturbances = FALSE)
# [ ] Check bufferedDisturbances = FALSE, maskOutLinesFromPolys = TRUE, aggregateSameDisturbances = TRUE 
FTT <- disturbanceInfoFromECCC(studyArea = studyArea, 
                               RTM = RTM,
                               totalstudyAreaVAreaSqKm = totalstudyAreaVAreaSqKm,
                               classesAvailable = classesAvailable,
                               destinationPath = Paths$inputPath,
                               bufferedDisturbances = FALSE,
                               maskOutLinesFromPolys = TRUE,
                               aggregateSameDisturbances = TRUE)

# [ ] Check bufferedDisturbances = TRUE, maskOutLinesFromPolys = FALSE, aggregateSameDisturbances = TRUE
TFT <- disturbanceInfoFromECCC(studyArea = studyArea, 
                               RTM = RTM,
                               totalstudyAreaVAreaSqKm = totalstudyAreaVAreaSqKm,
                               classesAvailable = classesAvailable,
                               destinationPath = Paths$inputPath,
                               bufferedDisturbances = TRUE,
                               maskOutLinesFromPolys = FALSE,
                               aggregateSameDisturbances = TRUE)

# [ ] Check bufferedDisturbances = FALSE, maskOutLinesFromPolys = TRUE, aggregateSameDisturbances = FALSE
FTF <- disturbanceInfoFromECCC(studyArea = studyArea, 
                               RTM = RTM,
                               totalstudyAreaVAreaSqKm = totalstudyAreaVAreaSqKm,
                               classesAvailable = classesAvailable,
                               destinationPath = Paths$inputPath,
                               bufferedDisturbances = FALSE,
                               maskOutLinesFromPolys = TRUE,
                               aggregateSameDisturbances = FALSE)
allResults <- list(TTT = TTT,
                   FFF = FFF,
                   TFF = TFF,
                   FTT = FTT,
                   TFT = TFT,
                   FTF = FTF,
                   TTF = TTF,
                   FFT = FFT)

allResReady <- lapply(names(allResults), function(nms){
  message(paste0("Converting ", nms))
  each <- allResults[[nms]]
  indList <- lapply(names(each), function(nm){
    ea <- each[[nm]]
    ea <- as.data.frame(ea)
    print(class(ea))
    return(ea)
  })
  names(indList) <- names(each)
  return(indList)
})
names(allResReady) <- names(allResults)

qs::qsave(x = allResReady, file = file.path(dirname(Paths$outputPath), "testingBufferMaskingAggregate.qs"))

