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
replicateRun <- "run01" # run02, run03, run04, run05
dist <- 0.4 # "BAU, 0.4, 0.6
distMod <- if (is(dist, "numeric")) dist else NULL
# popQuant <- c(0, 0.1) # I shouldn't have to pass like this because we need the values from James!
disturbanceScenario <- paste0(dist, "_NT1")
runName <- paste(shortProvinceName, climateScenario, disturbanceScenario, replicateRun, sep = "_")

out <- SpaDES.project::setupProject(
  runName = runName,
  paths = list(projectPath = "anthroDisturbance_NT",
               scratchPath = "~/scratch",
               outputPath = file.path("outputs", runName)),
  modules =
    c(
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
               end = 2071),
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
                                               lastYearSimulations = times[["end"]]),
                anthroDisturbance_DataPrep = list(#.useCache = c(".inputObjects", "init")
                  ),
                potentialResourcesNT_DataPrep = list(#.useCache = c(".inputObjects", "init")
                  ),
                anthroDisturbance_Generator = list(#.useCache = c(".inputObjects", "init"),
                                                   .inputFolderFireLayer = paths[["outputs"]],
                                                   .runName = runName,
                                                   totalDisturbanceRate = distMod,
                                                   runInterval = 10,
                                                   saveInitialDisturbances = TRUE)#,
                # caribouPopGrowth_disturbance = list(#.useCache = c(".inputObjects", "init"),
                #                                     overwriteDisturbanceLayer = FALSE,
                #                                     disturbancesFolder = paths[["outputs"]],
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
  loadOrder = c("getReadySimulationFiles", 
                "anthroDisturbance_DataPrep", "potentialResourcesNT_DataPrep", "anthroDisturbance_Generator",
                "caribouPopGrowth_disturbance", "caribouPopGrowthModel"),
  outputs =  data.frame(objectName = c("disturbances",
                                       "predictedCaribou"),
                        saveTime = c(rep(times$end, times = 2)))
  )

snippsim <- do.call(SpaDES.core::simInitAndSpades, out) 
