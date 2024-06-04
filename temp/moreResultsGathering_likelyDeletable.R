source("R/outterFuns.R")
studyArea <- terra::aggregate(reproducible::Cache(studyAreaGenerator,
                                                 url = "https://drive.google.com/file/d/1x_fQEKHW2nGbqo1JvCpDwmVuTPYAavl3",
                                                 archive = "BIO_ENR_WFE_BorealCaribou_RangePlanRegions_2020.zip",
                                                 targetFile = "BIO_ENR_WFE_BorealCaribou_RangePlanRegions_2020.shp",
                                                 large = FALSE, 
                                                 destPath = "~/data"))
rasterToMatch <- reproducible::Cache(rtmGenerator, sA = studyArea, 
                                    destPath = "~/data",
                                    useSAcrs = TRUE,
                                    large = FALSE,
                                    tags = "RTM_NT1")

outPath <- "/home/tmichele/projects/anthroDisturbance_NT/outputs/NT_CanESM5_SSP370_0.4_NT1_run02"

ys <- c(2021, 2031, 2041, 2051)
buffDist <- lapply(ys, function(Y){
  ras <- terra::rast(file.path(outPath, paste0("bufferedAnthDist_500m_",Y,".tif")))
  return(ras)
})
names(buffDist) <- paste0("Year", ys)
terra::plot(rast(buffDist))


listRas <- lapply(names(buffDist)[2:5], function(rName){
  ras <- buffDist[[rName]]
  tb <- table(ras[])
  totPix <- sum(tb)
  disturbed <- tb["1"]
  percentDisturbed <- 100*(disturbed/totPix)
  newVal <- if (rName == "Year2021") 2021 else 10
  ras[ras == 1] <- newVal
  return(list(percentDisturbed = percentDisturbed,
              yearRas = ras))
  })

allRas <- lapply(listRas, function(X) X[["yearRas"]])
allVals <- unlist(lapply(listRas, function(X) X[["percentDisturbed"]]))
percentYearlyChangeRate <- numeric()
for (i in 1:3){
  percentYearlyChangeRate[i] <- (allVals[i+1]-allVals[i])/10
}
names(percentYearlyChangeRate) <- c("2021-2031", "2031-2041", "2041-2051")

fullRas <- sum(allRas[[1]], allRas[[2]], allRas[[3]], allRas[[4]])