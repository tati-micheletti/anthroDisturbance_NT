# runMe small study area in NWT
################### RUNAME

if (SpaDES.project::user("tmichele")) setwd("~/projects/anthroDisturbance_NT/")

terra::terraOptions(tempdir = "~/scratch/terra")

################ SPADES CALL

shortProvinceName = "NT"
climateScenario <- "CanESM5_SSP370"
replicateRun <- "run01" # run02, run03, run04, run05
dist <- 0.6#, 0.2, 0.4, 0.6
distMod <- if (is(dist, "numeric")) dist else NULL
popQuant <- c(0, 0.1)
disturbanceScenario <- paste0(dist, "_herds")
runName <- paste(shortProvinceName, climateScenario, disturbanceScenario, replicateRun, sep = "_")

out <- SpaDES.project::setupProject(
  runName = runName,
  paths = list(projectPath = "anthroDisturbance_NT",
               scratchPath = "~/scratch",
               outputPath = file.path("outputs", runName)),
  modules =
    c(
      "tati-micheletti/getReadySimulationFiles@main",
      # "tati-micheletti/anthroDisturbance_DataPrep@main",
      # "tati-micheletti/potentialResourcesNT_DataPrep@main",
      # "tati-micheletti/anthroDisturbance_Generator@main",
      "tati-micheletti/caribouPopGrowth_disturbance@main",
      "tati-micheletti/caribouPopGrowthModel@master"
      ),
  options = list(spades.allowInitDuringSimInit = TRUE,
                 reproducible.cacheSaveFormat = "rds",
                 gargle_oauth_email = if (user("tmichele")) "tati.micheletti@gmail.com" else NULL,
                 gargle_oauth_cache = ".secrets",
                 gargle_oauth_client_type = "web", # Without this, google authentication didn't work when running non-interactively!
                 use_oob = FALSE,
                 repos = "https://cloud.r-project.org",
                 SpaDES.project.fast = TRUE,
                 reproducible.gdalwarp = TRUE,
                 reproducible.inputPaths = if (user("tmichele")) "~/data" else NULL,
                 reproducible.destinationPath = if (user("tmichele")) "~/data" else NULL,
                 reproducible.useMemoise = TRUE),
  times = list(start = 2011,
               end = 2041),
  functions = "tati-micheletti/anthroDisturbance_NT@main/R/outterFuns.R",
  authorizeGDrive = googledrive::drive_auth(cache = ".secrets"),
  shortProvinceName = shortProvinceName,
  studyArea = terra::aggregate(reproducible::Cache(herdsStudyArea,
                                                   destPath = paths[["inputPath"]])),
  rasterToMatch = reproducible::Cache(rtmGenerator, sA = studyArea, 
                                      destPath = paths[["inputPath"]],
                                      useSAcrs = TRUE,
                                      large = FALSE,
                                      tags = "RTM_herds"), 
  listSACaribou = list("NWT_Herds" = reproducible::Cache(herdsStudyArea, destPath = paths[["inputPath"]])),
  params = list(#.globals = list(.useCache = c(".inputObjects", "init")),
    getReadySimulationFiles = list(#.useCache = c(".inputObjects", "init"),
      gDriveFolder = "1lqIjwQQ8CU6l5GJezC9tVgs0Uz0dv-FD", 
      climateScenario = climateScenario, 
      replicateRun = replicateRun,
      lastYearSimulations = times[["end"]],
      runInterval = 10),
    # anthroDisturbance_DataPrep = list(#.useCache = c(".inputObjects", "init")
    #   checkList = FALSE, # False as I making the list
    #   # disturbanceListName = "disturbances_NT1",
    #   checkDisturbanceProportions = FALSE,
    #   studyAreaName = "herds"
    # ),
    # potentialResourcesNT_DataPrep = list(#.useCache = c(".inputObjects", "init")
    #   ),
    # anthroDisturbance_Generator = list(#.useCache = c(".inputObjects", "init"),
    #   .inputFolderFireLayer = paths[["outputPath"]],
    #   .runName = runName,
    #   totalDisturbanceRate = distMod,
    #   runInterval = 10,
    #   saveInitialDisturbances = TRUE,
    #   growthStepEnlargingLines = 50,
    #   growthStepEnlargingPolys = 0.7)#,
    caribouPopGrowth_disturbance = list(#.useCache = c(".inputObjects", "init"),
                                        overwriteDisturbanceLayer = FALSE,
                                        disturbancesFolder = paths[["outputPath"]],
                                        .runInterval = 10),
    caribouPopGrowthModel = list(#.useCache = c(".inputObjects", "init"),
                                 .runName = runName,
                                 .growthInterval = 10,
                                 useQuantiles = popQuant,
                                 climateModel = climateScenario)
  ),
  packages = c("googledrive", 'RCurl', 'XML', 'igraph', 'qs',
               "PredictiveEcology/SpaDES.core@sequentialCaching (>= 2.0.3.9002)",
               "PredictiveEcology/reproducible@modsForLargeArchives (>= 2.0.10.9010)",
               "PredictiveEcology/Require@development (>= 0.3.1.9015)"),
  useGit = "sub",
  loadOrder = c("getReadySimulationFiles", 
                # "anthroDisturbance_DataPrep", "potentialResourcesNT_DataPrep", "anthroDisturbance_Generator",
                "caribouPopGrowth_disturbance", "caribouPopGrowthModel"),
  outputs =  data.frame(objectName = c("disturbances",
                                       "predictedCaribou"),
                        file = c(paste0("disturbances_Q_", paste(popQuant, collapse = "-"), "_year2041.rds"),
                                 paste0("predictedCaribou_Q_", paste(popQuant, collapse = "-"), "_year2041.rds")),
                        saveTime = c(rep(times$end, times = 2)))
)

snippsim <- do.call(SpaDES.core::simInitAndSpades, out) 
