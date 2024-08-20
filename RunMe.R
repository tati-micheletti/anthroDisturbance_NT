################### PACKAGE INSTALLATION

getOrUpdatePkg <- function(p, minVer = "0") {
  if (!isFALSE(try(packageVersion(p) < minVer, silent = TRUE) )) {
    repo <- c("predictiveecology.r-universe.dev", getOption("repos"))
    install.packages(p, repos = repo)
  }
}

getOrUpdatePkg("Require", "0.3.1.9089")
getOrUpdatePkg("SpaDES.project", "0.1.0.9003")
getOrUpdatePkg("reproducible", "2.1.1.9002")
getOrUpdatePkg("SpaDES.core", "2.1.5.9000")

################### RUNAME

if (SpaDES.project::user("tmichele")) setwd("~/projects/anthroDisturbance_NT/")

terra::terraOptions(tempdir = "~/scratch/terra")

################ SPADES CALL

shortProvinceName = "NT"
climateScenario <- "CanESM5_SSP370"
replicateRun <- "run05" # run02, run03, run04, run05
# The names of replicates NEED to be as stated above as we download the matching files from GDrive
# for fire --> MAKE A UNIT TEST FOR IT. Google drive on line getSimulationDataFromGDrive.R#21 will fail!
dist <- 0.2 # 0.4, 0.6
distMod <- if (is(dist, "numeric")) dist else NULL
popQuant <- c(0, 0.1) # c(0.9, 1)
disturbanceScenario <- paste0(dist, "_NT1")
runName <- paste(shortProvinceName, climateScenario, disturbanceScenario, replicateRun, sep = "_")

out <- SpaDES.project::setupProject(
  runName = runName,
  paths = list(projectPath = "anthroDisturbance_NT",
               scratchPath = "~/scratch",
               outputPath = file.path("outputs", runName)),
  modules =c(
      "tati-micheletti/getReadySimulationFiles@main",
      "tati-micheletti/anthroDisturbance_DataPrep@main",
      "tati-micheletti/potentialResourcesNT_DataPrep@main",
      "tati-micheletti/anthroDisturbance_Generator@main"#,
      # "tati-micheletti/caribouPopGrowth_disturbance@main"#,
      # "tati-micheletti/caribouPopGrowthModel@master"
      ),
  options = list(spades.allowInitDuringSimInit = TRUE,
                 reproducible.cacheSaveFormat = "rds",
                 gargle_oauth_email = if (user("tmichele")) "tati.micheletti@gmail.com" else NULL,
                 gargle_oauth_cache = ".secrets",
                 gargle_oauth_client_type = "web", # Without this, google authentication didn't work when running non-interactively!
                 use_oob = FALSE,
                 repos = "https://cloud.r-project.org",
                 SpaDES.project.fast = FALSE,
                 reproducible.gdalwarp = TRUE,
                 reproducible.inputPaths = if (user("tmichele")) "~/data" else NULL,
                 reproducible.destinationPath = if (user("tmichele")) "~/data" else NULL,
                 reproducible.useMemoise = TRUE
                 ),
  times = list(start = 2011,
               end = 2051),
  functions = "tati-micheletti/anthroDisturbance_NT@main/R/outterFuns.R",
  authorizeGDrive = googledrive::drive_auth(cache = ".secrets"),
  shortProvinceName = shortProvinceName,
  studyArea = terra::aggregate(reproducible::Cache(studyAreaGenerator,
                                                   url = "https://drive.google.com/file/d/1x_fQEKHW2nGbqo1JvCpDwmVuTPYAavl3",
                                                   archive = "BIO_ENR_WFE_BorealCaribou_RangePlanRegions_2020.zip",
                                                   targetFile = "BIO_ENR_WFE_BorealCaribou_RangePlanRegions_2020.shp",
                                                   large = FALSE, 
                                                   destPath = paths[["inputPath"]])),
  rasterToMatch = reproducible::Cache(rtmGenerator, sA = studyArea, 
                                      destPath = paths[["inputPath"]],
                                      useSAcrs = TRUE,
                                      large = FALSE,
                                      tags = "RTM_NT1"), 
  listSACaribou = makeListSACaribou(studyArea, destPath = paths[["inputPath"]]),
  params = list(getReadySimulationFiles = list(gDriveFolder = "1lqIjwQQ8CU6l5GJezC9tVgs0Uz0dv-FD", 
                                               climateScenario = climateScenario, 
                                               replicateRun = replicateRun,
                                               lastYearSimulations = times[["end"]],
                                               runInterval = 10),
                anthroDisturbance_Generator = list(.inputFolderFireLayer = paths[["outputPath"]],
                                                   .runName = runName,
                                                   totalDisturbanceRate = distMod,
                                                   siteSelectionAsDistributing = "seismicLines",
                                                   probabilityDisturbance = list("seismicLines" = data.table::data.table(structure(list(
                                                     Potential = c(8, 9, 6, 2, 5, 7, 3, 4),
                                                     percAreaDisturbed = c(0.340036460849957, 0.0667589126650313,
                                                                           0.151091052755128, 0.0365264546249524,
                                                                           0.170480348222102, 0.205459396372943,
                                                                           0.00681769021602527, 0.0228296842938622)), 
                                                     row.names = c(NA, -8L), class = "data.frame"))), 
                                                   runInterval = 10,
                                                   saveInitialDisturbances = TRUE,
                                                   seismicLineGrids = 500,
                                                   growthStepEnlargingLines = 20,
                                                   growthStepEnlargingPolys = 0.3),
                caribouPopGrowth_disturbance = list(overwriteDisturbanceLayer = FALSE,
                                                    disturbancesFolder = file.path(paths[["outputPath"]]),
                                                    .runInterval = 10),
                caribouPopGrowthModel = list(.runName = runName,
                                             .growthInterval = 10,
                                             useQuantiles = popQuant,
                                             climateModel = climateScenario)
                ),
  packages = c("googledrive", 'RCurl', 'XML', 'igraph', 'qs',
               "PredictiveEcology/SpaDES.core@development (>= 2.1.5.9000)",
               "PredictiveEcology/reproducible@development (>= 2.1.1.9002)",
               "PredictiveEcology/Require@simplify4 (>= 0.3.1.9089)"),
  useGit = "sub",
  loadOrder = c(
                "getReadySimulationFiles",
                "anthroDisturbance_DataPrep", "potentialResourcesNT_DataPrep", "anthroDisturbance_Generator"
                # , "caribouPopGrowth_disturbance", "caribouPopGrowthModel"
                ),
  outputs =  data.frame(objectName = c("disturbances"
                                       # ,"predictedCaribou"
                                       ),
                        file = c(paste0("disturbances_Q_", paste(popQuant, collapse = "-"), "_year",times$end,".rds")
                                 # ,paste0("predictedCaribou_Q_", paste(popQuant, collapse = "-"), "_year",times$end,".rds")
                                 ),
                        saveTime = c(rep(times$end, times = 2)))
  )

snippsim <- do.call(SpaDES.core::simInitAndSpades, out)
