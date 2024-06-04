# getOrUpdatePkg <- function(p, minVer, repo) {
#   if (!isFALSE(try(packageVersion(p) < minVer, silent = TRUE) )) {
#     if (missing(repo)) repo = c("predictiveecology.r-universe.dev", getOption("repos"))
#     install.packages(p, repos = repo)
#   }
# }

# getOrUpdatePkg("Require", "0.3.1.9015")
# getOrUpdatePkg("SpaDES.project", "0.0.8.9027")

################### RUNAME

if (SpaDES.project::user("tmichele")) setwd("~/projects/anthroDisturbance_NT/")

terra::terraOptions(tempdir = "~/scratch/terra")

################ SPADES CALL

shortProvinceName = "NT"
climateScenario <- "CanESM5_SSP370"
replicateRun <- "run05" # run02, run03, run04, run05
dist <- 0.2# 0.4, 0.6
distMod <- if (is(dist, "numeric")) dist else NULL
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
      "tati-micheletti/anthroDisturbance_Generator@main"),
      # "tati-micheletti/caribouPopGrowth_disturbance@main"),#,
      # "tati-micheletti/caribouPopGrowthModel@master"),
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
  times = list(start = 2021,
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
  params = list(#.globals = list(.useCache = c(".inputObjects", "init")),
                getReadySimulationFiles = list(#.useCache = c(".inputObjects", "init"),
                                               gDriveFolder = "1lqIjwQQ8CU6l5GJezC9tVgs0Uz0dv-FD", 
                                               climateScenario = climateScenario, 
                                               replicateRun = replicateRun,
                                               lastYearSimulations = times[["end"]],
                                               runInterval = 10),
                anthroDisturbance_DataPrep = list(#.useCache = c(".inputObjects", "init")
                                                  checkList = FALSE, # False as I making the list
                                                  # disturbanceListName = "disturbances_NT1",
                                                  checkDisturbanceProportions = FALSE
                  ),
                potentialResourcesNT_DataPrep = list(#.useCache = c(".inputObjects", "init")
                  ),
                anthroDisturbance_Generator = list(#.useCache = c(".inputObjects", "init"),
                                                   .inputFolderFireLayer = paths[["outputPath"]],
                                                   .runName = runName,
                                                   totalDisturbanceRate = distMod,
                                                   runInterval = 10,
                                                   saveInitialDisturbances = TRUE,
                                                   growthStepEnlargingLines = 30,
                                                   growthStepEnlargingPolys = 0.7)#,
                # caribouPopGrowth_disturbance = list(#.useCache = c(".inputObjects", "init"),
                #                                     overwriteDisturbanceLayer = FALSE,
                #                                     disturbancesFolder = paths[["outputPath"]],
                #                                     .runInterval = 10),
                # caribouPopGrowthModel = list(#.useCache = c(".inputObjects", "init"),
                #                              .runName = runName,
                #                              .growthInterval = 10)
                ),
  packages = c("googledrive", 'RCurl', 'XML', 'igraph', 'qs',
               "PredictiveEcology/SpaDES.core@sequentialCaching (>= 2.0.3.9002)",
               "PredictiveEcology/reproducible@modsForLargeArchives (>= 2.0.10.9010)",
               "PredictiveEcology/Require@development (>= 0.3.1.9015)"),
  useGit = "sub",
  loadOrder = c(
                "getReadySimulationFiles", 
                "anthroDisturbance_DataPrep", "potentialResourcesNT_DataPrep", "anthroDisturbance_Generator"#,
                # "caribouPopGrowth_disturbance", "caribouPopGrowthModel"
                ),
  outputs =  data.frame(objectName = c("disturbances",
                                       "predictedCaribou"),
                        saveTime = c(rep(times$end, times = 2)))
  )

snippsim <- do.call(SpaDES.core::simInitAndSpades, out) 