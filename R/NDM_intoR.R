# Importing NDM data into R

################ ndm_gridtopoly function ################

## This function loads the ndm grid output file in XY coordinates and creates a polygon shapefile from it. 

## Arguments: 
## 1) filepath = filepath of the NDM grid output files.
## 2) dg = spatial scale in degrees. This argument goes into the argument 'width' of the function rgeos::gBuffer, which is then divided by 2. 
## 3) save_area.shp = default = FALSE. Save area as shapefile using function rgdal::writeOGR.
## 4) save.directory = directory in which you want to save the shapefile.
## 5) area_name = default: "Area_id". Name of the area shapefile. 

ndm_gridtopoly <- function(filepath, degree, save.area.shp = FALSE, save.directory, area.shp.name = "Area_id") { 
  
  require(sp)
  require(rgdal)
  require(rgeos)
  
  # x is a vector whose elements correspond to each line in the gridxy.txt archive.
  x <- readLines(con = filepath)
  
  # Initializing data.frame
  # ini_df colums: "id" = index for coordinates belongind to the same grid, "ord" = points joining order, 
  #"long" = longitude, "lat" = latitude. 
  # NOTE: "ord" can be deleted. It was created to account for joining order in drawing the cell polygons if necessary.
  
  ini_df <- data.frame(id = character(),
                       ord = double(),
                       long = character(),
                       lat = character())
  
  # Counting for n = id of the points belonging to the same cell, and y = order for joining points in a cell.
  n <- 0
  y <- 1
  
  # Loop for organizing vector elements in their corresponding places inside the data.frame 
  # Regex: "(^[0-9]+) (.[0-9.]+) (.[0-9.]+)" describes the pattern of element of the vector that
  # contain 3 numbers, one identifier and two coordinates.
  # Regex: "^(.[0-9.]+) (.[0-9.]+)" describes the patter of the vector element that contains only
  # both coordinates.
  
  for (i in seq_along(x)) {
    
    if (grepl("(^[0-9]+) (.[0-9.]+) (.[0-9.]+)", x[i])) {
      ini_df <- rbind(ini_df, data.frame(id = sub("(^[0-9]+) (.[0-9.]+) (.[0-9.]+)", "\\1", x[i]),
                                         ord = y,
                                         long = sub("(^[0-9]+) (.[0-9.]+) (.[0-9.]+)", "\\2", x[i]),
                                         lat = sub("(^[0-9]+) (.[0-9.]+) (.[0-9.]+)", "\\3", x[i])))
      
      n <- n + 1
      y <- y + 1
      
    }
    
    if (grepl("^(.[0-9.]+) (.[0-9.]+)$", x[i])) {
      ini_df <- rbind(ini_df, data.frame(id = n,
                                         ord = y,
                                         long = sub("^(.[0-9.]+) (.[0-9.]+)$", "\\1", x[i]),
                                         lat = sub("^(.[0-9.]+) (.[0-9.]+)$", "\\2", x[i])))
      y <- y + 1
      
    }
    
    # Condition to restart the count of y for each cell.
    if (y == 6) {
      
      y
      y <- 1
      
    }
    
  }
  
  # Changing columns class from character to numeric. gsub, grep, and grepl work with characters. 
  ini_df[,1] <- as.double(as.character(ini_df[,1]))
  ini_df[,3] <- as.double(as.character(ini_df[,3]))
  ini_df[,4] <- as.double(as.character(ini_df[,4]))
  
  ini_df
  
  ## Creating a grid shapefile
  
  centroid_df <- data.frame(id = double(),
                            long = double(),
                            lat = double())
  
  for (i in unique(ini_df$id)) {
    
    int_df <- subset(ini_df, subset = id == i)
    centroid_df <- rbind(centroid_df, data.frame(id = i,
                                                 long = mean(int_df[1:4, 3]),
                                                 lat = mean(int_df[1:4, 4])))
    
    centroid_df
    
  }
  
  df = data.frame(id = centroid_df[,1])
  centroid_spdf <- sp::SpatialPointsDataFrame(centroid_df[,2:3], df, proj4string = CRS(as.character(NA)))
  
  
  grid <- rgeos::gBuffer(centroid_spdf, byid = T, id = centroid_df[,1],
                         width = degree/2, capStyle = "SQUARE")
  
  # Saving area shapefile.
  if (save.area.shp == TRUE) {
    
    rgdal::writeOGR(grid, dsn = save.directory, layer = area.shp.name, driver = "ESRI Shapefile")
    
  }
  
  # printing grid object data
  grid
  
}


################ ndm_pointtopoly function ################

## This function loads the ndm point xy output file and creates a polygon shapefile from it. 

## Arguments:
## 1) filepath = filepath of the NDM point xy output files.
## 3) save_point.shp = default = FALSE. Save area as shapefile using function rgdal::writeOGR.
## 4) save.directory = directory in which you want to save the shapefile.
## 5) point.shape.name = default: "Area_id". Name of the area shapefile. 

ndm_pointtopoly <- function (filepath, save.point.shp = FALSE, save.directory, point.shape.name = "Points_id") {
  
  require(sp)
  require(rgdal)
  
  CASpp <- read.table(file = filepath, header = TRUE, sep = "\t")
  ndm_coord <- CASpp[, 2:3]
  CASpp_spdf <- SpatialPointsDataFrame(ndm_coord, CASpp, proj4string = CRS(as.character(NA)))
  
  if (save.point.shp == TRUE) {
    
    rgdal::writeOGR(CASpp_spdf, dsn = save.directory, layer = point.shape.name, driver = "ESRI Shapefile")
    
  }
  
  CASpp_spdf
  
}


################ ndm_listgrid.files & ndm_listxy.files functions ################

## It creates a list for the grid.txt files and a list of area names based on the file name, which is based on my own name-code.

## Area name code: It considers the scale of anlysis, consensus area ndm code, relative spatial localtion, and type of archive (i.e grids or points)
## Example: 1dgD_CA0_AF-grid -> 1dg = grid size; 
##                              D = Default analysis; 
##                              CA0 = Consensus area number 0;
##                              AF = A relative name for the approximate geographical region where the area is located.
##                              grid = ndm grid output file type.
## General regular expression = "^.*grid.txt 

## This function is not generalizable for other archives. 

## Arguments:
## 1) path = filepath where the ndm grid/xy outputs are stored. 
## 2) degree = grid size.

## Output:
## files(1,2 or 3)dg: list of file paths. Object is saved in the R environment.
## anames(1,2, or 3)dg: list of area names. Object is saved in the R environment.

ndm_listgrid.files <- function (path, degree) {
  
  # This function is specifically designed for my own code of file names: 
  # degreeAnalysis_ConsensusArea#_AreaName-grid.txt => Example: "1dgT_CA0_AF-grid.txt". General Regex = "^.*grid.txt 
  # Arguments: 
  # (i) 'path' is the filepath whre the ndm grid outputs are stored. 
  # (ii) 'degree': It is the spatial degree of analysis which is refered at the beggining of the archive name.
  
  if (degree == 1) {
    
    files1dg <<- list.files(path = path, pattern = "^1.*grid.txt", full.names = TRUE)
    anames1dg <<- gsub(pattern = "^.*(1dg.*).txt", replacement = "\\1", files1dg)
    message(paste(length(files1dg), "files listed in 'files1dg'.", length(anames1dg), "area names generated and saved in 'anames1dg'."))
    
  } 
  
  if (degree == 2) {
    
    files2dg <<- list.files(path = path, pattern = "^2.*grid.txt", full.names = TRUE)
    anames2dg <<- gsub(pattern = "^.*(2dg.*).txt", replacement = "\\1", files2dg)
    message(paste(length(files2dg), "files listed in 'files2dg'.", length(anames2dg), "area names generated and saved in 'anames2dg'."))
    
  } 
  
  if (degree == 3) {
    
    files3dg <<- list.files(path = path, pattern = "^3.*grid.txt", full.names = TRUE)
    anames3dg <<- gsub(pattern = "^.*(3dg.*).txt", replacement = "\\1", files3dg)
    message(paste(length(files3dg), "files listed in 'files3dg'.", length(anames3dg), "area names generated and saved in 'anames3dg'."))
    
  } 
  
}

## This function does the same but for xy files

ndm_listxy.files <- function (path, degree) {
  
  if (degree == 1) {
    
    xy_files1dg <<- list.files(path = path, pattern = "^1.*xy.txt", full.names = TRUE)
    xy_names1dg <<- gsub(pattern = "^.*(1dg.*).txt", replacement = "\\1", xy_files1dg)
    message(paste(length(xy_files1dg), "files listed in 'xy_files1dg'.", length(xy_names1dg), "area names generated and saved in 'xy_names1dg'."))
    
  } 
  
  if (degree == 2) {
    
    xy_files2dg <<- list.files(path = path, pattern = "^2.*xy.txt", full.names = TRUE)
    xy_names2dg <<- gsub(pattern = "^.*(2dg.*).txt", replacement = "\\1", xy_files2dg)
    message(paste(length(xy_files2dg), "files listed in 'xy_files2dg'.", length(xy_names2dg), "area names generated and saved in 'xy_names2dg'."))
    
  } 
  
  if (degree == 3) {
    
    xy_files3dg <<- list.files(path = path, pattern = "^3.*xy.txt", full.names = TRUE)
    xy_names3dg <<- gsub(pattern = "^.*(3dg.*).txt", replacement = "\\1", xy_files3dg)
    message(paste(length(xy_files3dg), "files listed in 'xy_files3dg'.", length(xy_names3dg), "area names generated and saved in 'xy_names3dg'."))
    
  } 
  
}


############### ndm_dataparsing function  ###############

## This function loads the ndm grid output file and creates a data frame in which each row is an observation per consensus area. 

## Arguments: 
## 1) ndmfile = filepath of the NDM grid output file.
## 2) grid_size = spatial scale in degrees.
## 3) ana_param = code to name the set of parameters used. Default value "D" standing for default set of parameters in NDM.
## 4) create_matrix = FALSE (default); it creates data matrices in raster format with WGS84 projection.
## 5) matrix_extent = an object of formal class extent with the extension of the data.

ndm_dataparsing <- function (ndmfile, grid_size, ana_param = "D", create_matrix = FALSE, matrix_extent) {
  
  ### Regex for the introductory line of the information about consensus areas
  ## .*\s([0-9]+),.*(any|each).*TOTAL: ([0-9]+).*
  ## Three groups: first, cut of similarity; second, consensus rule; third, total number of consensus areas.
  
  ### Regex for the first line of every consensus area
  ## ^\Consensus area ([0-9]+).*from ([0-9]+) areas.* ([0-9]+).*
  ## Two groups: first, area number; second, species number in consensus area.
  
  ### Regex for the species line
  ## \s+([0-9]+)\s(\w+\s\w+).*(\d\.\d\d\d)-(\d\.\d\d\d).*
  ## Four groups: first, species ndm code; second, species name; third, species minimum score in consensus area;
  ## fourth, species maximum score in area.
  
  ### Regex for species line when there is only one endemicity score value (only one area in the Consensus area)
  ##"^\\s+([0-9]+)\\s(\\w+\\s\\w+).*:\\s\\((\\d.\\d\\d\\d)\\)"
  ## Three groups: First, species ndm code; second, species name; third, species score in consensus area.
  
  ### Regex for groups (genera and phylogroups)
  ## \s+([0-9]+)\s(GRP-[0-9]+).*(\d\.\d\d\d)-(\d\.\d\d\d).*
  ## Four groups: first, group ndm code; second, group name; third, group minimum score in consensus area;
  ## fourth, group maximum score in area.
  
  ### Regex for groups line when there is only one endemicity score value (only one area in the Consensus area)
  ## \s+([0-9]+)\s(GRP-[0-9]+).*:\s\((\d\.\d\d\d)\).*
  ## Three groups: First, group ndm code; second, group name; third, group score in consensus area.
  
  library(raster)
  
  # Reading NDM-VNDM output file
  
  ndm_out <- readLines(ndmfile)
  
  # Getting the first line of each table-matrix
  mat_ini <- grep("CELL", ndm_out)
  
  # Defining a vector to subset and extract table-matrices
  mark <- vector("integer", length(ndm_out))
  mark[mat_ini] <- 1
  mark <- cumsum(mark)
  
  # Splitting into different table-matrices
  splitted <- split(ndm_out, mark)
  Total_Records_Cell_vec <- unlist(splitted[2])
  Number_Species_Cell_vec <- unlist(splitted[3])
  Number_AssSpecies_Cell_interm_vec <- unlist(splitted[4])
  
  # Subsetting Number_AssSpecies_Cell_vec to extract last matrix and Information about Areas, Consensus Areas,
  # and Endemic Species using the same code used above.
  
  mat_ini2 <- grep("INFORMATION", Number_AssSpecies_Cell_interm_vec)
  mark2 <- vector("integer", length(Number_AssSpecies_Cell_interm_vec))
  mark2[mat_ini2] <- 1
  mark2 <- cumsum(mark2)
  splitted2 <- split(Number_AssSpecies_Cell_interm_vec, mark2)
  Number_AssSpecies_Cell_vec <- unlist(splitted2[1])
  Number_AssSpecies_Cell_vec <- Number_AssSpecies_Cell_vec[-length(Number_AssSpecies_Cell_vec)]
  
  ## Creating NDM data matrices
  
  if (create_matrix == TRUE) {
    
    # Creating matrices
    
    Total_Records_Cell_mat <- as.matrix(read.table(textConnection(Total_Records_Cell_vec),
                                                   skip = 1, row.names = 1, header=TRUE))
    
    Number_Species_Cell_mat <- as.matrix(read.table(textConnection(Number_Species_Cell_vec),
                                                    skip = 1, row.names = 1, header=TRUE))
    
    Number_AssSpecies_Cell_mat <- as.matrix(read.table(textConnection(Number_AssSpecies_Cell_vec),
                                                       skip = 1, row.names = 1, header=TRUE))
    
    # Creating rasters for each matrix
    
    Total_Records_Cell_rast <- raster(Total_Records_Cell_mat)
    Number_Species_Cell_rast <- raster(Number_Species_Cell_mat)
    Number_AssSpecies_Cell_rast <- raster(Number_AssSpecies_Cell_mat)
    
    # Setting extent. Values extracted from NDM-VNDM
    
    Total_Records_Cell_rast <- setExtent(Total_Records_Cell_rast, matrix_extent)
    Number_Species_Cell_rast <- setExtent(Number_Species_Cell_rast, matrix_extent)
    Number_AssSpecies_Cell_rast <- setExtent(Number_AssSpecies_Cell_rast, matrix_extent)
    
    # Setting projection compatible with the background map imported using the function rgdal::readOGR
    
    projection(Total_Records_Cell_rast) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    projection(Number_Species_Cell_rast) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    projection(Number_AssSpecies_Cell_rast) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    
    # return matrix objects
    
    Records_cell <<- Total_Records_Cell_rast
    Species_cell <<- Number_Species_Cell_rast
    Assumed_cell <<- Number_AssSpecies_Cell_rast
    
  }
  
  
  ### Initializing empty data.frames
  ## Final data.frame template
  ini_df <- data.frame(NDM_SppCode = numeric(),
                       Species = character(),
                       Spatial_Scale = character(),
                       Analysis = character(),
                       Consensus_Rule = character(),
                       Number_CAs = character(),
                       Consensus_Area = character(),
                       Individual_Areas = numeric(),
                       Number_Spp = numeric(),
                       Cons_Area_Min_Score = numeric(),
                       Cons_Area_Max_Score = numeric(),
                       Spp_Min_Score = numeric(),
                       Spp_Max_Score = numeric(),
                       Groups = character(),
                       stringsAsFactors = FALSE)
  
  ## Consensus rule information data.frame
  consensus_info_df <- data.frame(cutoff = character(),
                                  rule = character(),
                                  nCAs = character(),
                                  stringsAsFactors = FALSE)
  
  ## Consensus area information data.frame
  ca_info_df <- data.frame(name_area = character(),
                           nindiv_areas = character(),
                           nspp_area = character(),
                           stringsAsFactors = FALSE)
  
  ## Loop to extract information from the ndm_ouput by using the list object splitted2, which was created above to extract data matrices.
  for (i in seq_along(splitted2[[3]])) {
    
    # Creating consensus rule information data.frame.
    if (grepl("^.*\\s([0-9]+),.*(any|each).*TOTAL: ([0-9]+).*$", splitted2[[3]][[i]]) == TRUE) {
      
      consensus_info_df <- data.frame(cutoff = sub("^.*\\s([0-9]+),.*(any|each).*TOTAL: ([0-9]+).*$", "\\1", splitted2[[3]][[i]]),
                                      rule = sub("^.*\\s([0-9]+),.*(any|each).*TOTAL: ([0-9]+).*$", "\\2", splitted2[[3]][[i]]),
                                      nCAs = sub("^.*\\s([0-9]+),.*(any|each).*TOTAL: ([0-9]+).*$", "\\3", splitted2[[3]][[i]]))
      
    }
    
    # Creating consensus area information data.frame.
    if (grepl("^Consensus area ([0-9]+).*from ([0-9]+) areas.* ([0-9]+).*$", splitted2[[3]][[i]]) == TRUE) {
      
      ca_info_df <- data.frame(name_area = paste("CA", sub("^Consensus area ([0-9]+).*from ([0-9]+) areas.* ([0-9]+).*$", "\\1",
                                                           splitted2[[3]][[i]])),
                               nindiv_areas = sub("^Consensus area ([0-9]+).*from ([0-9]+) areas.* ([0-9]+).*$", "\\2",
                                                  splitted2[[3]][[i]]),
                               nspp_area = sub("^Consensus area ([0-9]+).*from ([0-9]+) areas.* ([0-9]+).*$", "\\3",
                                               splitted2[[3]][[i]]))
    }
    
    # Creating rows in the general data.frame from species that appears in consensus areas for several individual areas.
    if (grepl("^\\s+([0-9]+)\\s(\\w+\\s\\w+).*(\\d\\.\\d\\d\\d)-(\\d\\.\\d\\d\\d).*$", splitted2[[3]][[i]]) == TRUE) {
      
      ini_df <- rbind(ini_df, data.frame(NDM_SppCode = sub("^\\s+([0-9]+)\\s(\\w+\\s\\w+).*(\\d\\.\\d\\d\\d)-(\\d\\.\\d\\d\\d).*$",
                                                           "\\1", splitted2[[3]][[i]]),
                                         Species = sub("^\\s+([0-9]+)\\s(\\w+\\s\\w+).*(\\d\\.\\d\\d\\d)-(\\d\\.\\d\\d\\d).*$",
                                                       "\\2", splitted2[[3]][[i]]),
                                         Spatial_Scale = grid_size,
                                         Analysis = ana_param,
                                         Consensus_Rule = ifelse(consensus_info_df[["rule"]] == "any",
                                                                 paste("Loose", consensus_info_df[["cutoff"]]),
                                                                 paste("Strict",consensus_info_df[["cutoff"]])),
                                         Number_CAs = consensus_info_df[["nCAs"]],
                                         Consensus_Area = ca_info_df[["name_area"]],
                                         Individual_Areas = ca_info_df[["nindiv_areas"]],
                                         Number_Spp = ca_info_df[["nspp_area"]],
                                         Cons_Area_Min_Score = "NA",
                                         Cons_Area_Max_Score = "NA",
                                         Spp_Min_Score = sub("^\\s+([0-9]+)\\s(\\w+\\s\\w+).*(\\d\\.\\d\\d\\d)-(\\d\\.\\d\\d\\d).*$",
                                                             "\\3", splitted2[[3]][[i]]),
                                         Spp_Max_Score = sub("^\\s+([0-9]+)\\s(\\w+\\s\\w+).*(\\d\\.\\d\\d\\d)-(\\d\\.\\d\\d\\d).*$",
                                                             "\\4", splitted2[[3]][[i]]),
                                         Groups = "0"))
      
    }
    
    # Creating rows in the general data.frame from species that appears in consensus areas for only one individual area.
    if (grepl("^\\s+([0-9]+)\\s(\\w+\\s\\w+).*:\\s\\((\\d.\\d\\d\\d)\\)$", splitted2[[3]][[i]]) == TRUE) {
      
      ini_df <- rbind(ini_df, data.frame(NDM_SppCode = sub("^\\s+([0-9]+)\\s(\\w+\\s\\w+).*:\\s\\((\\d.\\d\\d\\d)\\)$",
                                                           "\\1", splitted2[[3]][[i]]),
                                         Species = sub("^\\s+([0-9]+)\\s(\\w+\\s\\w+).*:\\s\\((\\d.\\d\\d\\d)\\)$",
                                                       "\\2", splitted2[[3]][[i]]),
                                         Spatial_Scale = grid_size,
                                         Analysis = ana_param,
                                         Consensus_Rule = ifelse(consensus_info_df[["rule"]] == "any",
                                                                 paste("Loose", consensus_info_df[["cutoff"]]),
                                                                 paste("Strict",consensus_info_df[["cutoff"]])),
                                         Number_CAs = consensus_info_df[["nCAs"]],
                                         Consensus_Area = ca_info_df[["name_area"]],
                                         Individual_Areas = ca_info_df[["nindiv_areas"]],
                                         Number_Spp = ca_info_df[["nspp_area"]],
                                         Cons_Area_Min_Score = "NA",
                                         Cons_Area_Max_Score = "NA",
                                         Spp_Min_Score = sub("^\\s+([0-9]+)\\s(\\w+\\s\\w+).*:\\s\\((\\d.\\d\\d\\d)\\)$",
                                                             "\\3", splitted2[[3]][[i]]),
                                         Spp_Max_Score = sub("^\\s+([0-9]+)\\s(\\w+\\s\\w+).*:\\s\\((\\d.\\d\\d\\d)\\)$",
                                                             "\\3", splitted2[[3]][[i]]),
                                         Groups = "0"))
      
    }
    
    # Creting rows for groups of species: genera and phylogroups (consensus from several individual areas)
    if (grepl("^\\s+([0-9]+)\\s(GRP-[0-9]+).*(\\d\\.\\d\\d\\d)-(\\d\\.\\d\\d\\d).*$", splitted2[[3]][[i]]) == TRUE) {
      
      ini_df <- rbind(ini_df, data.frame(NDM_SppCode = sub("^\\s+([0-9]+)\\s(GRP-[0-9]+).*(\\d\\.\\d\\d\\d)-(\\d\\.\\d\\d\\d).*$",
                                                           "\\1", splitted2[[3]][[i]]),
                                         Species = sub("^\\s+([0-9]+)\\s(GRP-[0-9]+).*(\\d\\.\\d\\d\\d)-(\\d\\.\\d\\d\\d).*$",
                                                       "\\2", splitted2[[3]][[i]]),
                                         Spatial_Scale = grid_size,
                                         Analysis = ana_param,
                                         Consensus_Rule = ifelse(consensus_info_df[["rule"]] == "any",
                                                                 paste("Loose", consensus_info_df[["cutoff"]]),
                                                                 paste("Strict",consensus_info_df[["cutoff"]])),
                                         Number_CAs = consensus_info_df[["nCAs"]],
                                         Consensus_Area = ca_info_df[["name_area"]],
                                         Individual_Areas = ca_info_df[["nindiv_areas"]],
                                         Number_Spp = ca_info_df[["nspp_area"]],
                                         Cons_Area_Min_Score = "NA",
                                         Cons_Area_Max_Score = "NA",
                                         Spp_Min_Score = sub("^\\s+([0-9]+)\\s(GRP-[0-9]+).*(\\d\\.\\d\\d\\d)-(\\d\\.\\d\\d\\d).*$",
                                                             "\\3", splitted2[[3]][[i]]),
                                         Spp_Max_Score = sub("^\\s+([0-9]+)\\s(GRP-[0-9]+).*(\\d\\.\\d\\d\\d)-(\\d\\.\\d\\d\\d).*$",
                                                             "\\4", splitted2[[3]][[i]]),
                                         Groups = "1"))
      
    }
    
    # Creting rows for groups of species: genera and phylogroups (consensus from only one individual area)
    if (grepl("^\\s+([0-9]+)\\s(GRP-[0-9]+).*:\\s\\((\\d\\.\\d\\d\\d)\\).*$", splitted2[[3]][[i]]) == TRUE) {
      
      ini_df <- rbind(ini_df, data.frame(NDM_SppCode = sub("^\\s+([0-9]+)\\s(GRP-[0-9]+).*:\\s\\((\\d\\.\\d\\d\\d)\\).*$",
                                                           "\\1", splitted2[[3]][[i]]),
                                         Species = sub("^\\s+([0-9]+)\\s(GRP-[0-9]+).*:\\s\\((\\d\\.\\d\\d\\d)\\).*$",
                                                       "\\2", splitted2[[3]][[i]]),
                                         Spatial_Scale = grid_size,
                                         Analysis = ana_param,
                                         Consensus_Rule = ifelse(consensus_info_df[["rule"]] == "any",
                                                                 paste("Loose", consensus_info_df[["cutoff"]]),
                                                                 paste("Strict",consensus_info_df[["cutoff"]])),
                                         Number_CAs = consensus_info_df[["nCAs"]],
                                         Consensus_Area = ca_info_df[["name_area"]],
                                         Individual_Areas = ca_info_df[["nindiv_areas"]],
                                         Number_Spp = ca_info_df[["nspp_area"]],
                                         Cons_Area_Min_Score = "NA",
                                         Cons_Area_Max_Score = "NA",
                                         Spp_Min_Score = sub("^\\s+([0-9]+)\\s(GRP-[0-9]+).*:\\s\\((\\d\\.\\d\\d\\d)\\).*$",
                                                             "\\3", splitted2[[3]][[i]]),
                                         Spp_Max_Score = sub("^\\s+([0-9]+)\\s(GRP-[0-9]+).*:\\s\\((\\d\\.\\d\\d\\d)\\).*$",
                                                             "\\3", splitted2[[3]][[i]]),
                                         Groups = "1"))
      
    }
  }
  
  return(ini_df)
  
}

############### ndm_kindpatt function  ###############

## This function categorize consensus areas into Core-like and Gradient-like patterns using the IE score.
## Ideally, if the endemic species of an area have a perfect fit to it, the IE score of the area would be equal to the number
## of endemic species. However, different degrees of sympatry are possible so this ideal is not generally fulfilled. 
## Core-like patterns are those whose IE score is equal or higher than the 66% of the IE if they had a perfect fit with the area. 
## Contrarywise, Gradient-like patterns are those whose IE score is less than the 66% of the IE if they had a perfect fit with the area.
## This is a description of how tighly aggregated and fitted are endemic species in a pattern. In certain way, it tries to operationalize
## the concepts of Core of Congruence and Maximum Region of Endemicity from DaSilva et al (2017).

## Arguments:
## 1) x: Number of endemic species in pattern
## 2) iescore: IE score of the consensus area.
## 3) p: (Default argument: p = 0.6) threshold value to define core-like and gradient-like patterns

ndm_kindpatt <- function(x, iescore, p = 0.6) {
  
  coreindex <- x*p
  
  pattern <- ifelse(iescore >= coreindex, "Core", "Gradient") 
  
  return(pattern)
}