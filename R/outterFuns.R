studyAreaGenerator <- function(url = NULL, destPath = NULL, Crs = NULL, 
                               large = FALSE,
                               targetFile = "edehzhie_boundary.shp", 
                               archive = NULL, alsoExtract = "all",
                               tags = NULL){
  if (is.null(url)) # Edehzhie study area
    url <- "https://drive.google.com/open?id=1fYvNPwovjNtTABoGcegrvdFGkNfCUsxf"
  if (is.null(destPath))
    destPath <- tempdir()
  if (is.null(Crs))
    # Crs <- "PROJCRS[\"unknown\",\n    BASEGEOGCRS[\"unknown\",\n        DATUM[\"Unknown_based_on_GRS80_ellipsoid\",\n            ELLIPSOID[\"GRS 1980\",6378137,298.257222101004,\n                LENGTHUNIT[\"metre\",1],\n                ID[\"EPSG\",7019]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]],\n    CONVERSION[\"Lambert Conic Conformal (2SP)\",\n        METHOD[\"Lambert Conic Conformal (2SP)\",\n            ID[\"EPSG\",9802]],\n        PARAMETER[\"Latitude of false origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-95,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",49,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",77,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]]]"
    Crs <- "PROJCRS[\"Canada_Albers_Equal_Area_Conic\",\n    BASEGEOGCRS[\"NAD83\",\n        DATUM[\"North American Datum 1983\",\n            ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n                LENGTHUNIT[\"metre\",1]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"Degree\",0.0174532925199433]]],\n    CONVERSION[\"Canada_Albers_Equal_Area_Conic\",\n        METHOD[\"Albers Equal Area\",\n            ID[\"EPSG\",9822]],\n        PARAMETER[\"Latitude of false origin\",40,\n            ANGLEUNIT[\"Degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-96,\n            ANGLEUNIT[\"Degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",50,\n            ANGLEUNIT[\"Degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",70,\n            ANGLEUNIT[\"Degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Not known.\"],\n        AREA[\"Canada - onshore and offshore - Alberta; British Columbia; Manitoba; New Brunswick; Newfoundland and Labrador; Northwest Territories; Nova Scotia; Nunavut; Ontario; Prince Edward Island; Quebec; Saskatchewan; Yukon.\"],\n        BBOX[38.21,-141.01,86.46,-40.73]],\n    ID[\"ESRI\",102001]]"
    studyArea <- reproducible::prepInputs(url = url,
                                        archive = archive, 
                                        targetFile = targetFile,
                                        destinationPath = destPath,
                                        fun = "terra::vect",
                                        userTags = c("objectName:studyArea", tags))
  studyArea <- reproducible::projectTo(from = studyArea, projectTo = Crs)
  if (large){
    studyArea <- terra::buffer(x = studyArea, width = 20000)
    message(crayon::green("Study area large sucessfully created!"))
  } else {
    message(crayon::green("Study area sucessfully created!"))
  }
  return(studyArea)
}

rtmGenerator <- function(url = NULL, destPath = NULL, 
                         Crs = NULL, sA = NULL, tags = NULL,
                         large = FALSE, useSAcrs = FALSE){
  if (is.null(url)) # NWT raster to match
    url <- "https://drive.google.com/file/d/11yCDc2_Wia2iw_kz0f0jOXrLpL8of2oM"
  if (is.null(destPath))
    destPath <- tempdir()
    RTM <- reproducible::prepInputs(url = url,
                                  destinationPath = destPath,
                                  studyArea = sA, 
                                  useSAcrs = useSAcrs,
                                  fun = "terra::rast",
                                  userTags = c("objectName:RTM", tags))
  if (large){
    message(paste0("RTM is being enlarged to match the large study area large. If the study area ",
                   "passed is smaller, the rasterToMatch will be returned unchanged, except for ",
                   "the non-NA values, which will be converted to 1 in either case."))
    # Here we check that if study area is larger than the raster, we enlarge it, fill it in, and 
    # re-crop/re-mask
    RTM <- terra::extend(RTM, terra::ext(sA)) # x is spatRaster, y = SpatExtent of study Area
    RTM[] <- 1
    RTM <- reproducible::postProcess(RTM,
                                     studyArea = sA,
                                     userTags = c("objectName:RTM", "Enlarged"))
  }
  message(crayon::green(paste0("RTM", ifelse(large, " large", ""), " sucessfully created!")))
  return(RTM)
}

makeListSACaribou <- function(studyArea){
  
  # Create needed objects
  
  Boreal_Caribou_Revised_Study_Areas_2020 <- "https://drive.google.com/file/d/1FNQKCjKhZIsr5rzWGfTvJ74K2KexN3um"
  BIO_ENR_WFE_BorealCaribou_RangePlanRegions_2020 <- "https://drive.google.com/file/d/1x_fQEKHW2nGbqo1JvCpDwmVuTPYAavl3"
  BIO_ENR_WFE_BorealCaribou_GNWT_NT1_range_2020 <- "https://drive.google.com/file/d/1VRSolnXMYPrkdBhNofeR81dCu_NTBSgf"
  Masterfile_meta_042908_ACCNAd83_ <- "https://drive.google.com/file/d/1byKv69wjoISWNMDsemzQacGln7kGWqyq"
  
  sa1 <- reproducible::prepInputs(url = Boreal_Caribou_Revised_Study_Areas_2020,
                    destinationPath = paths[["inputPath"]],
                    targetFile = "Boreal_Caribou_Revised_Study_Areas_2020.shp",
                    studyArea = studyArea,
                    fun = "terra::vect")
  sa2 <- reproducible::prepInputs(url = BIO_ENR_WFE_BorealCaribou_RangePlanRegions_2020,
                    destinationPath = paths[["inputPath"]],
                    targetFile = "BIO_ENR_WFE_BorealCaribou_RangePlanRegions_2020.shp",
                    studyArea = studyArea,
                    fun = "terra::vect")
  sa3 <- reproducible::prepInputs(url = BIO_ENR_WFE_BorealCaribou_GNWT_NT1_range_2020,
                    destinationPath = paths[["inputPath"]],
                    targetFile = "BIO_ENR_WFE_BorealCaribou_GNWT_NT1_range_2020.shp",
                    studyArea = studyArea,
                    fun = "terra::vect")
  sa4 <- reproducible::prepInputs(url = Masterfile_meta_042908_ACCNAd83_,
                    destinationPath = paths[["inputPath"]],
                    targetFile = "Masterfile_meta_042908_ACCNAd83_.shp",
                    # studyArea = studyArea, # Not reprojecting. Weird.
                    fun = "terra::vect")

  sa4 <- terra::subset(sa4, sa4[["PROV_TERR"]] == "NWT")
  sa4 <- terra::project(x = sa4, terra::crs(studyArea)) 
  listSACaribou <- c(sa1, sa2, sa3, sa4)
  names(listSACaribou) <- c("Revised_Study_Areas", "RangePlanRegions", "GNWT_NT1", "Herds_NT")
  return(listSACaribou)
}


