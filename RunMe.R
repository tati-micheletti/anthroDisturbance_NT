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
climateScenario <- "CanESM5_SPP370"
replicateRun <- "run01"
runName <- paste(shortProvinceName, climateScenario, replicateRun, sep = "_")

out <- SpaDES.project::setupProject(
  runName = runName,
  paths = list(projectPath = "anthroDisturbance_NT",
               scratchPath = "~/scratch",
               outputPath = file.path("outputs", runName)),
  modules =
    c("tati-micheletti/getReadySimulationFiles@main",
      "tati-micheletti/anthroDisturbance_DataPrep@main",
      "tati-micheletti/potentialResourcesNT_DataPrep@main",
      "tati-micheletti/anthroDisturbance_Generator@main",
      "tati-micheletti/caribouPopGrowth_disturbance@main",
      "tati-micheletti/caribouPopGrowthModel@master"),
  options = list(spades.allowInitDuringSimInit = TRUE,
                 reproducible.cacheSaveFormat = "rds",
                 gargle_oauth_email = if (user("tmichele")) "tati.micheletti@gmail.com" else NULL,
                 SpaDES.project.fast = TRUE,
                 reproducible.gdalwarp = TRUE,
                 reproducible.inputPaths = if (user("tmichele")) "~/data" else NULL,
                 reproducible.destinationPath = if (user("tmichele")) "~/data" else NULL,
                 reproducible.useMemoise = TRUE),
  times = list(start = 2021,
               end = 2071),
  functions = "tati-micheletti/anthroDisturbance_NT@main/R/outterFuns.R",
  shortProvinceName = shortProvinceName,
  studyArea = terra::aggregate(reproducible::Cache(studyAreaGenerator,
                                                   url = "https://drive.google.com/file/d/1RPfDeHujm-rUHGjmVs6oYjLKOKDF0x09",
                                                   archive = "NT1_BCR6.zip",
                                                   targetFile = "NT1_BCR6.shp",
                                                   large = TRUE, 
                                       destPath = paths[["inputPath"]])),
  rasterToMatch = reproducible::Cache(rtmGenerator, sA = studyArea, 
                                      destPath = paths[["inputPath"]],
                                      useSAcrs = TRUE,
                                      large = TRUE,
                                      tags = "RTM"), 
  # listSACaribou = reproducible::Cache(makeListSACaribou, studyArea),
  listSACaribou = makeListSACaribou(studyArea),
  params = list(#.globals = list(.useCache = c(".inputObjects", "init")),
                getReadySimulationFiles = list(.useCache = c(".inputObjects", "init"),
                                               gDriveFolder = "1t6032ggUC__jzaJs5H39LW6iFfrqlK_T", 
                                               climateScenario = climateScenario, 
                                               replicateRun = replicateRun),
                anthroDisturbance_DataPrep = list(.useCache = c(".inputObjects", "init")),
                potentialResourcesNT_DataPrep = list(.useCache = c(".inputObjects", "init")),
                anthroDisturbance_Generator = list(.useCache = c(".inputObjects", "init"),
                                                   .inputFolderFireLayer = paths[["outputs"]],
                                                   .runName = runName),
                caribouPopGrowth_disturbance = list(.useCache = c(".inputObjects", "init"),
                                                    overwriteDisturbanceLayer = FALSE),
                caribouPopGrowthModel = list(#.useCache = c(".inputObjects", "init"),
                                             .runName = runName,
                                             .growthInterval = 10)
                ),
  
  packages = c("googledrive", 'RCurl', 'XML', 'igraph', 'qs',
               "PredictiveEcology/SpaDES.core@sequentialCaching (>= 2.0.3.9002)",
               "PredictiveEcology/reproducible@modsForLargeArchives (>= 2.0.10.9010)"),
  useGit = "sub",
  loadOrder = c("getReadySimulationFiles", 
                "anthroDisturbance_DataPrep", "potentialResourcesNT_DataPrep", "anthroDisturbance_Generator",
                "caribouPopGrowth_disturbance", "caribouPopGrowthModel"),
  outputs =  data.frame(objectName = c("disturbances",
                                       "predictedCaribou"),
                        saveTime = c(rep(times$end, times = 2)))
  )

snippsim <- do.call(SpaDES.core::simInitAndSpades, out)
