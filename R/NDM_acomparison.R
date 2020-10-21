## Functions to compare consensus areas

############### Function ndm_exploratory #####

## Arguments:

# ca_dsn: filepath to consensus area layer
# ca_layer: consensus area layer name
# point_dsn: filepath to consensus area endemic species points
# point_layer: point layer name
# basemap_dsn: filepath to geographical base map
# basemap_layer: geograhical map layer name
# ca_data: consenus area data frame with all relevant information. Species column should be a ordered factor.
# ca_scale: spatial scale of the consensus area
# CA_name: ndm consensus area name
# ca_color: color of consensus area
# ca_alpha: transparency of consensus area

ndm_explore <- function(ca_layer, ca_dsn, basemap_dsn, basemap_layer, ca_data,
                         ca_color = "blue", ca_alpha = 0.5) {
  
  # Selecting name
  if (grepl(ca_layer, pattern = ".*1dgD.*")) {
    x <- gsub(ca_layer, pattern = ".*1dgD_(.*)_.*-grid", replacement = "\\1")
    y <- paste("D1", x, sep = "")
    z <- sub("([A-Z])([0-9]+)", "\\1 \\2", x)
    w <- gsub(ca_layer, pattern = ".*(1)dgD_(.*)_.*-grid", replacement = "\\1")
  }
  
  if (grepl(ca_layer, pattern = ".*2dgD.*")) {
    x <- gsub(ca_layer, pattern = ".*2dgD_(.*)_.*-grid", replacement = "\\1")
    y <- paste("D2", x, sep = "")
    z <- sub("([A-Z])([0-9]+)", "\\1 \\2", x)
    w <- gsub(ca_layer, pattern = ".*(2)dgD_(.*)_.*-grid", replacement = "\\1")
  }
  
  if (grepl(ca_layer, pattern = ".*3dgD.*")) {
    x <- gsub(ca_layer, pattern = ".*3dgD_(.*)_.*-grid", replacement = "\\1")
    y <- paste("D3", x, sep = "")
    z <- sub("([A-Z])([0-9]+)", "\\1 \\2", x)
    w <- gsub(ca_layer, pattern = ".*(3)dgD_(.*)_.*-grid", replacement = "\\1")
  }
  
  if (grepl(ca_layer, pattern = ".*1dgT.*")) {
    x <- gsub(ca_layer, pattern = ".*1dgT_(.*)_.*-grid", replacement = "\\1")
    y <- paste("T1", x, sep = "")
    z <- sub("([A-Z])([0-9]+)", "\\1 \\2", x)
    w <- sub("([A-Z]+)([0-9]+)", "\\2", x)
  }
  
  if (grepl(ca_layer, pattern = ".*2dgT.*")) {
    x <- gsub(ca_layer, pattern = ".*2dgT_(.*)_.*-grid", replacement = "\\1")
    y <- paste("T2", x, sep = "")
    z <- sub("([A-Z])([0-9]+)", "\\1 \\2", x)
    w <- sub("([A-Z]+)([0-9]+)", "\\2", x)
  }
  
  if (grepl(ca_layer, pattern = ".*3dgT.*")) {
    x <- gsub(ca_layer, pattern = ".*3dgT_(.*)_.*-grid", replacement = "\\1")
    y <- paste("T3", x, sep = "")
    z <- sub("([A-Z])([0-9]+)", "\\1 \\2", x)
    w <- sub("([A-Z]+)([0-9]+)", "\\2", x)
  }
  
  if (grepl(ca_layer, pattern = ".*1dgS.*")) {
    x <- gsub(ca_layer, pattern = ".*1dgS_(.*)_.*-grid", replacement = "\\1")
    y <- paste("S1", x, sep = "")
    z <- sub("([A-Z])([0-9]+)", "\\1 \\2", x)
    w <- sub("([A-Z]+)([0-9]+)", "\\2", x)
  }
  
  if (grepl(ca_layer, pattern = ".*2dgS.*")) {
    x <- gsub(ca_layer, pattern = ".*2dgD_(.*)_.*-grid", replacement = "\\1")
    y <- paste("D2", x, sep = "")
    z <- sub("([A-Z])([0-9]+)", "\\1 \\2", x)
    w <- sub("([A-Z]+)([0-9]+)", "\\2", x)
  }
  
  if (grepl(ca_layer, pattern = ".*3dgS.*")) {
    x <- gsub(ca_layer, pattern = ".*3dgS_(.*)_.*-grid", replacement = "\\1")
    y <- paste("S3", x, sep = "")
    z <- sub("([A-Z])([0-9]+)", "\\1 \\2", x)
    w <- sub("([A-Z]+)([0-9]+)", "\\2", x)
  }
  
  if (grepl(ca_layer, pattern = ".*1dgH.*")) {
    x <- gsub(ca_layer, pattern = ".*1dgH_(.*)_.*-grid", replacement = "\\1")
    y <- paste("H1", x, sep = "")
    z <- sub("([A-Z])([0-9]+)", "\\1 \\2", x)
    w <- sub("([A-Z]+)([0-9]+)", "\\2", x)
  }
  
  if (grepl(ca_layer, pattern = ".*2dgH.*")) {
    x <- gsub(ca_layer, pattern = ".*2dgH_(.*)_.*-grid", replacement = "\\1")
    y <- paste("H2", x, sep = "")
    z <- sub("([A-Z])([0-9]+)", "\\1 \\2", x)
    w <- sub("([A-Z]+)([0-9]+)", "\\2", x)
  }
  
  if (grepl(ca_layer, pattern = ".*3dgH.*")) {
    x <- gsub(ca_layer, pattern = ".*3dgH_(.*)_.*-grid", replacement = "\\1")
    y <- paste("H3", x, sep = "")
    z <- sub("([A-Z])([0-9]+)", "\\1 \\2", x)
    w <- sub("([A-Z]+)([0-9]+)", "\\2", x)
  }
  
  # Projection: WGS84
  wgs84.proj4 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  # Loading data
  ca_grid <- readOGR(dsn = ca_dsn, layer = ca_layer, p4s = wgs84.proj4)
  # point_pol <- st_read(dsn = point_dsn, layer = point_layer)
  Amer <- readOGR(dsn = basemap_dsn, layer = basemap_layer, p4s = wgs84.proj4)
  df_ndmexp <- read.csv(ca_data)
  
  # Defining species plotting in elevation graph by ascendind altitudinal range
  
  XXX <-  df_ndmexp %>%
    dplyr::select(GENUS, Species, elev_min, elev_max) %>%
    group_by(GENUS) %>%
    arrange(desc(elev_min), elev_max, .by_group = TRUE) %>%
    mutate(sp_randiff = elev_max - elev_min) %>%
    distinct(Species, sp_randiff, .keep_all = TRUE) %>%
    arrange(sp_randiff)
  
  df_ndmexp$Species <- factor(df_ndmexp$Species, 
                              levels = XXX$Species)
  rm(XXX)
  
  # Processing layers to plot with ggplot2
  Amer_df <- fortify(Amer)
  
  ca_grid <- st_as_sf(ca_grid)
  ca_pol <- st_union(ca_grid)
  ca_pol <- as_Spatial(ca_pol)
  ca_pol_df <- fortify(ca_pol)
  
  # # Creating convex hullp
  # point_ch <- st_convex_hull(st_union(point_pol))
  # ploint_ch <- as_Spatial(point_ch)
  # point_ch_df <- fortify(point_ch)
  
  # Selecting a  vector of consensus area species
  ca_spp <- df_ndmexp %>% 
    filter(Spatial_Scale == x, Consensus_Area == z) %>%
    dplyr::select(4) %>%
    pull()
  
  # Elevation graphical object
  ca_elev <- df_ndmexp %>% 
    filter(Spatial_Scale == w, Consensus_Area == z) %>%
    ggplot(aes(x = Species, y = elevation)) + 
    geom_linerange(aes(ymin = elev_min, ymax = elev_max, col = Species), lwd = 1) +
    geom_point(aes(x = Species, y = elev_median), col = "red", size = 1, alpha = 1, shape = 17)  +
    scale_color_viridis_d() +
    scale_y_continuous(breaks = seq(0, 5000, by = 500)) +
    geom_hline(yintercept = 750, lty = 2, lwd = 0.2) +
    geom_hline(yintercept = 1800, lty = 2, lwd = 0.2) +
    geom_hline(yintercept = 3600, lty = 2, lwd = 0.2) +
    theme_minimal() +
    labs(x = "Species",
         y = "msnm",
         title = "Elevation") +
    theme(axis.text.x = element_blank(),
          legend.position = "none",
          legend.text = element_text(face = "italic"),
          panel.grid = element_blank(), 
          panel.background = element_rect(fill = "white"),
          strip.text = element_text(face = "bold.italic"))
  
  # Base map
  CAbase_map <-  ggplot(Amer_df, aes(x = long, y = lat, group = group)) + 
    geom_polygon(fill = "white", 
                 col = "black") +
    coord_cartesian(xlim = c(-111, -34),
                    ylim = c(-37, 39),
                    expand = TRUE) +
    labs(x = "", y = "") +
    # caption = "NDM-VNDM v3\nAmerica land shapefile 1:10 - Natural Earth Data\nLonglat Projection") +
    theme_linedraw() +
    theme(panel.grid = element_blank())
  
  ## Consensus areas maps
  
  Cons_Area_map <- CAbase_map +
    geom_polygon(data = ca_pol_df, 
                 aes(x = long, y = lat, group = group),
                 fill = alpha(ca_color, ca_alpha),
                 colour = "black",
                 lwd = 0.3) +
    # geom_polygon(data = point_ch_df, # convex hull for CA
    #              aes(x = long, y = lat, group = group),
    #              fill = alpha("red", 0.3),
    #              colour = "black",
    #              lwd = 0.3) +
    annotation_custom(grob = ggplotGrob(ca_elev), 
                      xmin = -115, xmax = -85, 
                      ymin = -40, ymax = -10) +
    labs(title = paste("Consensus Area", y, "\nGrid size", w, "x", w, "degrees"))
  
  Cons_Area_map
  
}


############### Function ndm_cellnumb ##################

## Funtion to count the number of cells per consensus area from grid polygons
## It produces a dataframe for consensus areas information with the extra column CA_size. CA_size shows the size of the consensus area
## as the number of grid cells that comform it.

## Arguments:
# 1) df_cas: dataframe with the information about consensus areas.
# 2) dg: spatial scale of the analysis in degrees. 
# 3) ana: the kind of heuristic search performed in this work. "D" for Default, "T" for Tolerant, "S" for strict, and "H" for hard.
# 4) shp.dns: the directory of the grid polygones shapefiles

ndm_cellnumb <- function (df_cas, dg, ana, shp.dns) {
  
  library(rgdal)
  
  df_cas <- df_cas %>%
    filter(Analysis == ana, Spatial_Scale == dg) %>%
    distinct(Consensus_Area, .keep_all = TRUE) %>%
    dplyr::select(-1, -2, -3, -6, -7, -13, -14, -15)
  
  ca_name_vec <- unlist(df_cas[, "Consensus_Area"], use.names = FALSE)
  ca_name_vec <- gsub("\\s", "", ca_name_vec)
  files_list <- list.files(path = shp.dns, pattern = paste(".*", "(", dg, "dg", ana, ".*)-grid.shp", sep = ""), full.names = TRUE)
  layers_names <- gsub(files_list, pattern = paste(".*", "(", dg, "dg", ana, ".*).shp", sep = ""), replacement = "\\1")
  
  cell_number_vec <- vector(length = length(ca_name_vec))
  
  for (i in seq_along(ca_name_vec)) {
    
    index <- grep(paste(ca_name_vec[i], "[_||-]", sep = ""), layers_names)
    shp_name <- layers_names[index[1]]
    AM_GRID <- readOGR(dsn = shp.dns, layer = shp_name)
    cell_number_vec[i] <- length(AM_GRID)
    
  }
  
  cell_number_vec
  
  df_cas[, 8] <- cell_number_vec
  
  colnames(df_cas) <- c("Spatial_Scale", "Analysis", "Consensus_Area", "Individual_Areas", "Number_Spp", "Cons_Area_Min_Score",
                        "Cons_Area_Max_Score", "CA_Size")
  df_cas <- df_cas[, c(1, 2, 3, 8, 4, 5, 6, 7)]
  
  return(df_cas)
  
}

############### Function ndm_Aeffect ##################

## This function takes as input the dataframe produced by ndm_cellnumb and produce a dataframe in the format needed to 
## compare among heristic searches results at the same spatial scale. 

## Arguments:
## 1) df: dataframe from ndm_cellnumb

## ndm_Acode composed from Analysis, Spatial_Scale, Consensus_Area

ndm_Aeffect <- function(df) {
  
  require(dplyr)
  
  x <- df %>%
    mutate(ndmA_code = paste(Analysis, Spatial_Scale, Consensus_Area, sep = "")) %>%
    dplyr::select(ndmA_code, CA_Size, Number_Spp)
  
  x[1] <-  gsub("\\s", "", unlist(x[, "ndmA_code"], use.names = FALSE))
  
  x
  
}

############### Function ndm_compacross ##################

## This function qualitatively represent the effect of the analysis over size and species number at an specific spatial scale.

## Arguments:
## 1) equival_CA_df: dataframe with equivalencies among consensus areas in different heuristic searches but at the same spatial scale.
## 2) ana_d: dataframe for heuristic search D obtained from ndm_effect
## 2) ana_t: dataframe for heuristic search T obtained from ndm_effect
## 2) ana_s: dataframe for heuristic search S obtained from ndm_effect
## 2) ana_h: dataframe for heuristic search H obtained from ndm_effect

ndm_compacross <- function(equival_CA_df, ana_d, ana_t, ana_s, ana_h) {
  
  # equival_CA_df: data.frame with equivalences between consensus areas
  # x: vector corresponding to a row extracted from equival_CA_df
  # arguments: ana_d, ana_t, ana_s, ana_h: data.frames from ndm_Aeffect
  
  y <- equival_CA_df
  
  y[, "SppDT"] <- NA
  y[, "SppDS"] <- NA
  y[, "SppDH"] <- NA
  y[, "SppTS"] <- NA
  y[, "SppSH"] <- NA
  y[, "gridDT"] <- NA
  y[, "gridDS"] <- NA
  y[, "gridDH"] <- NA
  y[, "gridTS"] <- NA
  y[, "gridSH"] <- NA
  
  for (i in seq_along(equival_CA_df[[1]])) {
    
    x <- unlist(equival_CA_df[i, ], use.names = FALSE)
    
    indexD <- which(ana_d[[1]] == x[2])
    
    if (!is.na(x[3])) {
      
      indexT <- which(ana_t[[1]] == x[3])
      
      if (ana_d[indexD, ]["Number_Spp"] == ana_t[indexT, ]["Number_Spp"]) { y$SppDT[i] <- "D=T" }
      
      if (ana_d[indexD, ]["Number_Spp"] > ana_t[indexT, ]["Number_Spp"]) { y$SppDT[i] <- "D>T" }
      
      if (ana_d[indexD, ]["Number_Spp"] < ana_t[indexT, ]["Number_Spp"]) { y$SppDT[i] <- "D<T" }
      
      if (ana_d[indexD, ]["CA_Size"] == ana_t[indexT, ]["CA_Size"]) { y$gridDT[i] <- "D=T" }
      
      if (ana_d[indexD, ]["CA_Size"] > ana_t[indexT, ]["CA_Size"]) { y$gridDT[i] <- "D>T" }
      
      if (ana_d[indexD, ]["CA_Size"] < ana_t[indexT, ]["CA_Size"]) { y$gridDT[i] <- "D<T" }
      
    } else {
      y$SppDT[i] <- "NA"
      y$gridDT[i] <- "NA"
    }
    
    if (!is.na(x[4])) {
      
      indexS <- which(ana_s[[1]] == x[4])
      
      if (ana_d[indexD, ]["Number_Spp"] == ana_s[indexS, ]["Number_Spp"]) { y$SppDS[i] <- "D=S" }
      
      if (ana_d[indexD, ]["Number_Spp"] > ana_s[indexS, ]["Number_Spp"]) { y$SppDS[i] <- "D>S" }
      
      if (ana_d[indexD, ]["Number_Spp"] < ana_s[indexS, ]["Number_Spp"]) { y$SppDS[i] <- "D<S" }
      
      if (ana_d[indexD, ]["CA_Size"] == ana_s[indexS, ]["CA_Size"]) { y$gridDS[i] <- "D=S" }
      
      if (ana_d[indexD, ]["CA_Size"] > ana_s[indexS, ]["CA_Size"]) { y$gridDS[i] <- "D>S" }
      
      if (ana_d[indexD, ]["CA_Size"] < ana_s[indexS, ]["CA_Size"]) { y$gridDS[i] <- "D<S" }
      
    } else {
      y$SppDS[i] <- "NA"
      y$gridDS[i] <- "NA"
    }
    
    if (!is.na(x[5])) {
      
      indexH <- which(ana_h[[1]] == x[5])
      
      if (ana_d[indexD, ]["Number_Spp"] == ana_h[indexH, ]["Number_Spp"]) { y$SppDH[i] <- "D=H" }
      
      if (ana_d[indexD, ]["Number_Spp"] > ana_h[indexH, ]["Number_Spp"]) { y$SppDH[i] <- "D>H" }
      
      if (ana_d[indexD, ]["Number_Spp"] < ana_h[indexH, ]["Number_Spp"]) { y$SppDH[i] <- "D<H" }
      
      if (ana_d[indexD, ]["CA_Size"] == ana_h[indexH, ]["CA_Size"]) { y$gridDH[i] <- "D=H" }
      
      if (ana_d[indexD, ]["CA_Size"] > ana_h[indexH, ]["CA_Size"]) { y$gridDH[i] <- "D>H" }
      
      if (ana_d[indexD, ]["CA_Size"] < ana_h[indexH, ]["CA_Size"]) { y$gridDH[i] <- "D<H" }
      
    } else {
      y$SppDH[i] <- "NA"
      y$gridDH[i] <- "NA"
    }
    
    if (!is.na(x[3]) && !is.na(x[4])) {
      
      indexT <- which(ana_t[[1]] == x[3])
      indexS <- which(ana_s[[1]] == x[4])
      
      if (ana_t[indexT, ]["Number_Spp"] == ana_s[indexS, ]["Number_Spp"]) { y$SppTS[i] <- "T=S" }
      
      if (ana_t[indexT, ]["Number_Spp"] > ana_s[indexS, ]["Number_Spp"]) { y$SppTS[i] <- "T>S" }
      
      if (ana_t[indexT, ]["Number_Spp"] < ana_s[indexS, ]["Number_Spp"]) { y$SppTS[i] <- "T<S" }
      
      if (ana_t[indexT, ]["CA_Size"] == ana_s[indexS, ]["CA_Size"]) { y$gridTS[i] <- "T=S" }
      
      if (ana_t[indexT, ]["CA_Size"] > ana_s[indexS, ]["CA_Size"]) { y$gridTS[i] <- "T>S" }
      
      if (ana_t[indexT, ]["CA_Size"] < ana_s[indexS, ]["CA_Size"]) { y$gridTS[i] <- "T<S" }
      
    } else {
      y$SppTS[i] <- "NA"
      y$gridTS[i] <- "NA"
    }
    
    if (!is.na(x[4]) && !is.na(x[5])) {
      
      indexS <- which(ana_s[[1]] == x[4])
      indexH <- which(ana_h[[1]] == x[5])
      
      if (ana_s[indexS, ]["Number_Spp"] == ana_h[indexH, ]["Number_Spp"]) { y$SppSH[i] <- "S=H" }
      
      if (ana_s[indexS, ]["Number_Spp"] > ana_h[indexH, ]["Number_Spp"]) { y$SppSH[i] <- "S>H" }
      
      if (ana_s[indexS, ]["Number_Spp"] < ana_h[indexH, ]["Number_Spp"]) { y$SppSH[i] <- "S<H" }
      
      if (ana_s[indexS, ]["CA_Size"] == ana_h[indexH, ]["CA_Size"]) { y$gridSH[i] <- "S=H" }
      
      if (ana_s[indexS, ]["CA_Size"] > ana_h[indexH, ]["CA_Size"]) { y$gridSH[i] <- "S>H" }
      
      if (ana_s[indexS, ]["CA_Size"] < ana_h[indexH, ]["CA_Size"]) { y$gridSH[i] <- "S<H" }
      
    } else {
      y$SppSH[i] <- "NA"
      y$gridSH[i] <- "NA"
    }
    
  }
  
  return(y)
  
}

############### Function ndm_comptest ##################

## This function compares the species composition of the consensus areas found at the Default analysis against the equivalent consensus
## areas found in the other analyses. It produces a dataframe with the percentage of scoring species retained from the Default analysis
## and the percentage of new species gained per pattern per analysis (i.e. D against T, S, and H)

## Arguments: 
## 1) equival_CA_df: dataframe with equivalencies among consensus areas in different heuristic searches but at the same spatial scale.
## 2) row_comp: The row in equival_CA_df where the specific comparison between consensus areas is found.
## 3) df_cas: dataframe with the information about consensus areas.

ndm_comptest <- function(equival_CA_df, row_comp, df_cas) {
  
  df_cas <- df_cas %>%
    mutate(Ca_name = gsub(" ", "", paste(Analysis, Spatial_Scale, Consensus_Area), fixed = TRUE)) %>%
    dplyr::select(Ca_name, Species)
  
  Default <- df_cas %>%
    filter(Ca_name %in% equival_CA_df[row_comp, 2]) %>%
    dplyr::select(2) %>%
    pull()
  
  Tolerant <- df_cas %>%
    filter(Ca_name %in% equival_CA_df[row_comp, 3])  %>%
    dplyr::select(2) %>%
    pull()
  
  Strict <- df_cas %>%
    filter(Ca_name %in% equival_CA_df[row_comp, 4]) %>%
    dplyr::select(2) %>%
    pull()
  
  Hard <- df_cas %>%
    filter(Ca_name %in% equival_CA_df[row_comp, 5])  %>%
    dplyr::select(2) %>%
    pull()
  
  DTSH_df <- data.frame(CA_Name = pull(equival_CA_df[row_comp, 2]),
                        Comparison = c("DtoT", "DtoS", "DtoH"),
                        Spp_in_D = length(Default),
                        Spp_in_X = unlist(lapply(list(Tolerant, Strict, Hard), length)),
                        Identical_spp = vector(mode = "double", length = 3),
                        Different_spp = vector(mode = "double", length = 3))
  
  DTSH_df[1, 5] <- sum(Default %in% Tolerant)
  DTSH_df[2, 5] <- sum(Default %in% Strict)
  DTSH_df[3, 5] <- sum(Default %in% Hard)
  
  DTSH_df[1, 6] <- sum(!Tolerant %in% Default)
  DTSH_df[2, 6] <- sum(!Strict %in% Default)
  DTSH_df[3, 6] <- sum(!Hard %in% Default)
  
  DTSH_df <- DTSH_df %>%
    mutate(Retained_spp = format(Identical_spp / Spp_in_D * 100, digits = 4),
           New_spp = format((Different_spp / Spp_in_X) * 100, digits = 4))
  
  DTSH_df$Retained_spp <- as.numeric(DTSH_df$Retained_spp)
  DTSH_df$New_spp <- as.numeric(DTSH_df$New_spp)
  
  return(DTSH_df)
  
}

ndm_comptestDS <- function(equival_CA_df, row_comp, df_cas) {
  
  df_cas <- df_cas %>%
    mutate(Ca_name = gsub(" ", "", paste(Analysis, Spatial_Scale, Consensus_Area), fixed = TRUE)) %>%
    dplyr::select(Ca_name, Species)
  
  Default <- df_cas %>%
    filter(Ca_name %in% equival_CA_df[row_comp, 2]) %>%
    dplyr::select(2) %>%
    pull()
  
  Strict <- df_cas %>%
    filter(Ca_name %in% equival_CA_df[row_comp, 3])  %>%
    dplyr::select(2) %>%
    pull()
  
  DTSH_df <- data.frame(D = pull(equival_CA_df[row_comp, 2]),
                        S = pull(equival_CA_df[row_comp, 3]),
                        Spp_in_D = length(Default),
                        Spp_in_S = length(Strict),
                        Identical_spp = vector(mode = "double", length = 1),
                        Different_spp = vector(mode = "double", length = 1))
  
  DTSH_df[1, 5] <- sum(Default %in% Strict)
  DTSH_df[1, 6] <- sum(!Strict %in% Default)
  
  DTSH_df <- DTSH_df %>%
    mutate(Retained_spp = format(Identical_spp / Spp_in_D * 100, digits = 4),
           New_spp = format((Different_spp / Spp_in_S) * 100, digits = 4))
  
  DTSH_df$Retained_spp <- as.numeric(DTSH_df$Retained_spp)
  DTSH_df$New_spp <- as.numeric(DTSH_df$New_spp)
  
  return(DTSH_df)
  
}

############### Function ndm_areasize ##################

# This function calculates the absolute difference in grid cells between areas. 

## Arguments:
## 1) spp_compDs: Dataframe from ndm_comptestDS
## 2) Ddf and Sdf: dataframes from ndm_cellnumb.

ndm_areasize <- function(spp_compDS, Ddf, Sdf) {
  
  spp_compDS$Size_D <- 0
  spp_compDS$Size_S <- 0
  spp_compDS$Size_difference <- 0
  
  dfd <- Ddf
  dfs <- Sdf
  
  skip_diff <- 0
  
  for (i in seq_along(spp_compDS$D)) {
    
    a <- dfd %>%
      filter(ndmA_code == spp_compDS$D[i]) %>%
      dplyr::select(CA_Size) %>%
      pull()
    
    if (length(a) == 0) {
      
      a <- 0
      
      skip_diff <- 1
      
    }
    
    b <- dfs %>%
      filter(ndmA_code == spp_compDS$S[i]) %>%
      dplyr::select(CA_Size) %>%
      pull()
    
    if (length(b) == 0) {
      
      b <- 0
      
      skip_diff <- 1
      
    }
    
    spp_compDS$Size_D[i] <- a
    
    spp_compDS$Size_S[i] <- b
    
    if (skip_diff == 1) {
      
      spp_compDS$Size_difference[i] <- 0
      
    } else {
      
      spp_compDS$Size_difference[i] <- abs(a - b)
      
    }
    
    skip_diff <- 0
    
  }
  
  
  spp_compDS$D <- as.character(spp_compDS$D)
  spp_compDS$S <- as.character(spp_compDS$S) 
  
  spp_compDS <- spp_compDS %>%
    mutate(new_pat = case_when(
      New_spp == 0 & Retained_spp < 100 ~ "Subsets from D",
      New_spp == 0 & Retained_spp == 100 ~ "Identical", #Subsets from D
      New_spp != 0 ~ "Mixed subsets",
      is.nan(New_spp) | is.nan(Retained_spp) ~ "Lost"
    ),
    D = ifelse(is.na(D), " ", D),
    S = ifelse(is.na(S), " ", S),
    pair_comp = paste(D, S, sep = "-"),
    Retained_spp = ifelse(is.nan(Retained_spp), 0, Retained_spp)
    )
  
  return(spp_compDS)
  
}

############### Functions ndm_upsetlist, ana_listing, scale_listing, scaleAna_liting ###############

## UPSETR package to quickly visualize consensus areas that share species, and VennDiagram package to spot elements at intersections.
## UPSETR does not allow to see elements in the intersections, VennDiagram package functions are used for that purpose.
## UPSETR requires input as class list. The following set of functions allow to create lists discriminating by
## scale and analysis.

# This is the standard way to create a truth table for venn set relationships.
# Each element of the list corresponds to a vector with species from a consensus area.
# x <- list(a = H_UPSET_list[[1]], b = H_UPSET_list[[2]], c = H_UPSET_list[[15]])
# get.venn.partitions(x, force.unique = TRUE)

## Arguments: 
## x = spread tibble with columns as Consensus areas and with rows defined by a unique identifier.
# Tibble filled with NDM_Spp_Code with empty cells scored as 'NA'.

ndm_upsetlist <- function (x) {
  
  UPSET_list <- list()
  
  for (i in 1:length(names(x))) {
    vec_na <- pull(x[,i])
    vec <- vec_na[!is.na(vec_na)]
    UPSET_list[[i]] <- vec
    names(UPSET_list)[i] <- names(x)[i]
    
    if (length(UPSET_list) == length(names(x))) {
      return(UPSET_list)
    }
  }
  
}

## Function to list sets by analysis

## Arguments:
## 1) df_cas: dataframe with the information about consensus areas.
## 2) ana: heuristic search "D", "T", "S" or "H".
## 3) scl: spatial scale 1, 2 or 3

ana_listing <- function (df_cas, ana) {
  
  library(tidyr)
  library(dplyr)
  
  df_cas %>%
    dplyr::select(NDM_SppCode, Spatial_Scale, Analysis, Consensus_Area) %>%
    filter(Analysis == ana) %>%
    mutate(CA_code = paste(Spatial_Scale, Consensus_Area, sep = ""),
           ID = seq_along(NDM_SppCode)) %>%
    dplyr::select(-2, -3, -4) %>%
    spread(key = CA_code, value = NDM_SppCode) %>%
    dplyr::select(-1) %>%
    ndm_upsetlist()
}

## Function scale_listing to create lists by scale of analysis

## Arguments:
## 1) df_cas: dataframe with the information about consensus areas.
## 2) scl: spatial scale 1, 2 or 3

scale_listing <- function (df_cas, scl) {
  
  library(tidyr)
  library(dplyr)
  
  df_cas %>%
    dplyr::select(NDM_SppCode, Spatial_Scale, Analysis, Consensus_Area) %>%
    filter(Spatial_Scale == scl) %>%
    mutate(CA_code = paste(Analysis, Consensus_Area, sep = ""),
           ID = seq_along(NDM_SppCode)) %>%
    dplyr::select(-2, -3, -4) %>%
    spread(key = CA_code, value = NDM_SppCode) %>%
    dplyr::select(-1) %>%
    ndm_upsetlist()
}

## Funcion scaleAna_listing creating lists by analyses and scale

## Arguments:
## 1) df_cas: dataframe with the information about consensus areas.
## 2) ana: heuristic search "D", "T", "S" or "H".
## 3) scl: spatial scale 1, 2 or 3

scaleAna_listing <- function(df_cas, ana, scl) {
  
  library(tidyr)
  
  df_cas %>%
    dplyr::select(NDM_SppCode, Spatial_Scale, Analysis, Consensus_Area) %>%
    filter(Analysis == ana, Spatial_Scale == scl) %>%
    mutate(CA_code = paste(Spatial_Scale, Consensus_Area, sep = ""),
           ID = seq_along(NDM_SppCode)) %>%
    dplyr::select(-2, -3, -4) %>%
    spread(key = CA_code, value = NDM_SppCode) %>%
    dplyr::select(-1) %>%
    ndm_upsetlist()
}

# Function ndm_upset to create a generic plot for consensus areas

## This function wrapped the code of the functions above with the UpSetR function to produce the figure showing conflicting areas.

## Arguments:
## 1) df_cas: dataframe with the information about consensus areas.
## 2) ana: heuristic search "D", "T", "S" or "H".
## 3) scl: spatial scale 1, 2 or 3

ndm_upset <- function(df_cas, ana, scl) {
  
  library(dplyr)
  library(UpSetR)
  
  ndm_upsetlist <- function (x) {
    
    UPSET_list <- list()
    
    for (i in 1:length(names(x))) {
      vec_na <- pull(x[,i])
      vec <- vec_na[!is.na(vec_na)]
      UPSET_list[[i]] <- vec
      names(UPSET_list)[i] <- names(x)[i]
      
      if (length(UPSET_list) == length(names(x))) {
        return(UPSET_list)
      }
    }
    
  }
  
  scaleAna_listing <- function(.df_cas, .ana, .scl) {
    
    library(tidyr)
    
    .df_cas %>%
      dplyr::select(NDM_SppCode, Spatial_Scale, Analysis, Consensus_Area) %>%
      filter(Analysis == .ana, Spatial_Scale == .scl) %>%
      mutate(CA_code = paste(Spatial_Scale, Consensus_Area, sep = ""),
             ID = seq_along(NDM_SppCode)) %>%
      dplyr::select(-2, -3, -4) %>%
      spread(key = CA_code, value = NDM_SppCode) %>%
      dplyr::select(-1) %>%
      ndm_upsetlist()
  }
  
  upset_ca_list <- scaleAna_listing(.df_cas = ca_df, .ana = ana, .scl = scl)
  
  if (scl == 1) {
    
    names(upset_ca_list) <- c("D1CA 0", "D1CA 1", "D1CA 2", "D1CA 3", "D1CA 4", "D1CA 5", "D1CA 6", "D1CA 7", "D1CA 8", "D1CA 9")
    
  } else {
    
    if (scl == 2) {
      
      names(upset_ca_list) <- c("D2CA 0", "D2CA 1", "D2CA 10", "D2CA 11", "D2CA 12", "D2CA 13", "D2CA 14", "D2CA 15", "D2CA 16", "D2CA 17",
                                "D2CA 18", "D2CA 19", "D2CA 2", "D2CA 20", "D2CA 21", "D2CA 22", "D2CA 23", "D2CA 24", "D2CA 25", "D2CA 26",
                                "D2CA 27", "D2CA 3", "D2CA 4", "D2CA 5", "D2CA 6", "D2CA 7", "D2CA 8", "D2CA 9")
      
    } else {
      
      names(upset_ca_list) <- c("D3CA 0", "D3CA 1", "D3CA 10", "D3CA 11", "D3CA 12", "D3CA 13", "D3CA 14", "D3CA 15", "D3CA 16", "D3CA 17",
                                "D3CA 18", "D3CA 19", "D3CA 2", "D3CA 20", "D3CA 21", "D3CA 22", "D3CA 23", "D3CA 24", "D3CA 25", "D3CA 26",
                                "D3CA 27", "D3CA 28", "D3CA 29", "D3CA 3", "D3CA 30", "D3CA 31", "D3CA 32", "D3CA 33", "D3CA 4", "D3CA 5", 
                                "D3CA 6", "D3CA 7", "D3CA 8", "D3CA 9")
      
    }
    
  }
  
  upset(fromList(upset_ca_list), mb.ratio = c(0.55, 0.45), nsets = length(upset_ca_list),  order.by = "degree",  nintersects = NA,
        mainbar.y.label = "Shared species", sets.x.label = "Species per Consensus Area")
  
}

############### Function conflict_elevation ######

## This function graphics a boxplot for all the species of two, three or five conflicting consensus areas.
## All species of the same consensus area have the same color (viridis palette, alpha = 0.5), but conflicting species are shown in red.

## Arguments:
## 1) ca_df: dataframe with consensus area information. It must contain the folowing columns:
##    Spatial_Scale = It contains the spatial scale of the consensus area in degrees (ex. 1, 2 or 3 degrees)
##    Consensus_Area = It contains the name of the consensus area to which a species belongs as it is given by NDM (ex. "CA 1", "CA 20").
##      In this dataframe each row is an observation of a species in a consensus area, thus, it should cotain repeated values for the Species names field.
##
## 2) elevat_df: dataframe with elevation values. It must contain the following columns:
##    NAME1 = Column with species names.
##    elevation = Column with elevation values.
##    Each row represents an observation.
##
## 3) con_scale: spatial scale of the consensus area in conflict
## 4) ca_names: a character vector with lengths 2, 3 or 5 containing the names of the consensus areas in conflic. (ex. c("CA 1", "CA 20"))

## Observations:
## There is a character vector named "plot_order" inside the function. This vector is used as levels for plot order. It follows an increasing
## order given by the size of the linear altitudinal range of each species. Further development of this function may include code to generate this
## vector automatically.


conflict_elevation <- function(ca_df, elevat_df, con_scale, ca_names) {
  
  library(dplyr)
  library(ggplot2)
  library(VennDiagram)
  library(viridis)
  
  
  ## Vector to define levels and plot order in boxplot chart. Order given by increasing size of the altitudinal linear range.
  plot_order <- c("Adenocalymma nervosum", "Adenocalymma apetiolatum", "Adenocalymma sastrei", "Adenocalymma cidii", "Adenocalymma ubatubense", 
                  "Adenocalymma flavum", "Amphilophium perbracteatum", "Amphilophium pauciflorum", "Amphilophium reticulatum", "Anemopaegma pachyphyllum", 
                  "Anemopaegma heringeri", "Anemopaegma villosum", "Bignonia cararensis", "Fridericia subverticillata", "Fridericia subexserta", 
                  "Mansoa longicalyx", "Pachyptera ventricosa", "Pleonotoma fissicalyx", "Pleonotoma dendrotricha", "Tanaecium apiculatum", 
                  "Adenocalymma velutinum", "Adenocalymma lineare", "Bignonia cuneata", "Anemopaegma granvillei", "Amphilophium cuneifolium", 
                  "Anemopaegma jucundum", "Pleonotoma longiflora", "Pachyptera aromatica", "Pyrostegia millingtonioides", "Adenocalymma saulense", 
                  "Tynanthus espiritosantensis", "Cuspidaria cinerea", "Adenocalymma juliae", "Adenocalymma molle", "Amphilophium lohmanniae", 
                  "Pleonotoma fluminensis", "Fridericia crassa", "Adenocalymma adenophorum", "Mansoa angustidens", "Pleonotoma exserta", 
                  "Pleonotoma bracteata", "Adenocalymma arthropetiolatum", "Anemopaegma salicifolium", "Cuspidaria bracteolata", "Fridericia lauta", 
                  "Pleonotoma echitidea", "Adenocalymma chocoense", "Amphilophium magnoliifolium", "Cuspidaria monophylla", "Adenocalymma longilineum", 
                  "Bignonia neouliginosa", "Amphilophium frutescens", "Tanaecium exitiosum", "Martinella iquitoensis", "Tynanthus sastrei", 
                  "Tynanthus densiflorus", "Adenocalymma album", "Adenocalymma gibbosum", "Amphilophium cremersii", "Anemopaegma mirabile", 
                  "Tanaecium revillae", "Amphilophium campinae", "Adenocalymma calcareum", "Adenocalymma subspicatum", "Fridericia elegans", 
                  "Bignonia phellosperma", "Anemopaegma flavum", "Anemopaegma patelliforme", "Fridericia carichanensis", "Adenocalymma moringifolium", 
                  "Anemopaegma ionanthum", "Pachyptera erythraea", "Lundia helicocalyx", "Tanaecium crucigerum", "Adenocalymma mirabile", 
                  "Tynanthus croatianus", "Tynanthus macranthus", "Mansoa minensis", "Mansoa gentryi", "Dolichandra dentata", "Fridericia pliciflora", 
                  "Tanaecium duckei", "Tanaecium ornithophilum", "Tanaecium bilabiatum", "Adenocalymma bullatum", "Adenocalymma albiflorum", 
                  "Amphilophium occidentale", "Adenocalymma gracielzae", "Bignonia costata", "Fridericia spicata", "Tynanthus panurensis", 
                  "Adenocalymma magnificum", "Adenocalymma subincanum", "Fridericia japurensis", "Anemopaegma nebulosum", "Bignonia microcalyx", 
                  "Fridericia oligantha", "Lundia laevis", "Amphilophium laeve", "Tanaecium affine", "Mansoa ivanii", "Adenocalymma ackermannii", 
                  "Adenocalymma bracteosum", "Fridericia whitei", "Fridericia fagoides", "Amphilophium pilosum", "Amphilophium parkeri", 
                  "Fridericia subincana", "Martinella insignis", "Tanaecium paradoxum", "Lundia gardneri", "Bignonia sordida", "Adenocalymma schomburgkii", 
                  "Fridericia prancei", "Adenocalymma cristicalyx", "Tynanthus pubescens", "Bignonia sanctae-crucis", "Adenocalymma biternatum", 
                  "Fridericia grosourdyana", "Amphilophium blanchetii", "Adenocalymma tanaeciicarpum", "Anemopaegma robustum", "Anemopaegma longipetiolatum",
                  "Adenocalymma hirtum", "Fridericia caudigera", "Amphilophium nunezii", "Cuspidaria lachnaea", "Adenocalymma uleanum", "Bignonia pterocalyx",
                  "Cuspidaria multiflora", "Amphilophium rodriguesii", "Cuspidaria bracteata", "Tanaecium caudiculatum", "Fridericia oxycarpa",
                  "Amphilophium elongatum", "Anemopaegma foetidum", "Cuspidaria octoptera", "Anemopaegma longidens", "Pleonotoma pavettiflora", 
                  "Fridericia formosa", "Adenocalymma flaviflorum", "Fridericia mollis", "Anemopaegma santaritense", "Tanaecium jaroba", 
                  "Xylophragma heterocalyx", "Mansoa hirsuta", "Cuspidaria emmonsii", "Tynanthus labiatus", "Adenocalymma subsessilifolium", 
                  "Bignonia potosina", "Tynanthus fasciculatus", "Tanaecium neobrasiliense", "Adenocalymma cinereum", "Amphilophium dasytrichum", 
                  "Anemopaegma album", "Fridericia nigrescens", "Lundia damazioi", "Bignonia capreolata", "Adenocalymma contractum", "Adenocalymma dusenii", 
                  "Anemopaegma chrysoleucum", "Fridericia nicotianiflora", "Xylophragma platyphyllum", "Anemopaegma setilobum", "Bignonia binata", 
                  "Pleonotoma orientalis", "Amphilophium gnaphalanthum", "Anemopaegma goyazense", "Cuspidaria cratensis", "Adenocalymma allamandiflorum", 
                  "Mansoa parvifolia", "Tanaecium parviflorum", "Cuspidaria argentea", "Tynanthus guatemalensis", "Tynanthus micranthus", 
                  "Fridericia egensis", "Anemopaegma pabstii", "Bignonia ramentacea", "Fridericia podopogon", "Adenocalymma apparicianum", 
                  "Amphilophium stamineum", "Fridericia mutabilis", "Anemopaegma acutifolium", "Anemopaegma alatum", "Adenocalymma aurantiacum", 
                  "Cuspidaria subincana", "Fridericia limae", "Fridericia tynanthoides", "Tanaecium cyrtanthum", "Bignonia bracteomana", 
                  "Amphilophium scabriusculum", "Adenocalymma dichilum", "Bignonia campanulata", "Fridericia leucopogon", "Fridericia arthrerion", 
                  "Cuspidaria inaequalis", "Fridericia bahiensis", "Cuspidaria weberbaueri", "Mansoa onohualcoides", "Adenocalymma hypostictum", 
                  "Fridericia fanshawei", "Bignonia lilacina", "Fridericia rego", "Adenocalymma coriaceum", "Anemopaegma parkeri", "Bignonia uleana", 
                  "Anemopaegma orbiculatum", "Xylophragma myrianthum", "Mansoa lanceolata", "Lundia obliqua", "Adenocalymma alboaurantiacum", 
                  "Lundia virginalis", "Adenocalymma scabriusculum", "Adenocalymma cymbalum", "Mansoa alliacea", "Xylophragma harleyi", 
                  "Bignonia decora", "Bignonia longiflora", "Adenocalymma salmoneum", "Adenocalymma hatschbachii", "Anemopaegma prostratum", 
                  "Amphilophium obovatum", "Fridericia craterophora", "Pleonotoma castelnaei", "Mansoa hymenaea", "Fridericia costaricensis", 
                  "Mansoa paganuccii", "Amphilophium pulverulentum", "Fridericia simplex", "Anemopaegma laeve", "Fridericia tuberculata", 
                  "Pleonotoma stichadenia", "Pleonotoma clematis", "Amphilophium porphyrotrichum", "Anemopaegma brevipes", "Amphilophium bauhinioides", 
                  "Tanaecium tetramerum", "Adenocalymma nodosum", "Anemopaegma glaucum", "Anemopaegma citrinum", "Fridericia cinerea", 
                  "Fridericia cuneifolia", "Dolichandra unguiculata", "Bignonia neoheterophylla", "Adenocalymma candolleanum", "Cuspidaria simplicifolia", 
                  "Anemopaegma arvense", "Pleonotoma jasminifolia", "Adenocalymma paulistarum", "Lundia longa", "Anemopaegma gracile", "Pleonotoma melioides",
                  "Amphilophium bracteatum", "Stizophyllum perforatum", "Fridericia dispar", "Tanaecium dichotomum", "Adenocalymma peregrinum", 
                  "Fridericia trachyphylla", "Cuspidaria pulchella", "Fridericia trailii", "Dolichandra hispida", "Perianthomega vellozoi", 
                  "Fridericia triplinervia", "Cuspidaria sceptrum", "Adenocalymma aspericarpum", "Amphilophium chocoense", "Cuspidaria convoluta", 
                  "Tynanthus cognatus", "Mansoa difficilis", "Tanaecium truncatum", "Adenocalymma macrophyllum", "Amphilophium granulosum", 
                  "Mansoa glaziovii", "Fridericia cinnamomea", "Cuspidaria pulchra", "Bignonia callistegioides", "Anemopaegma paraense", 
                  "Fridericia conjugata", "Adenocalymma validum", "Mansoa standleyi", "Amphilophium steyermarkii", "Adenocalymma pubescens", 
                  "Xylophragma pratense", "Lundia erionema", "Anemopaegma velutinum", "Lundia spruceana", "Dolichandra steyermarkii", 
                  "Adenocalymma divaricatum", "Anemopaegma scabriusculum", "Xylophragma corchoroides", "Cuspidaria lateriflora", "Adenocalymma dugandii", 
                  "Dolichandra cynanchoides", "Anemopaegma oligoneuron", "Bignonia noterophila", "Amphilophium racemosum", "Anemopaegma grandifolium", 
                  "Amphilophium dusenianum", "Bignonia nocturna", "Pleonotoma tetraquetra", "Adenocalymma magdalenense", "Amphilophium arenarium", 
                  "Fridericia samydoides", "Fridericia erubescens", "Amphilophium dolichoides", "Fridericia pearcei", "Amphilophium mansoanum", 
                  "Amphilophium falcatum", "Anemopaegma chamberlaynii", "Bignonia hyacinthina", "Adenocalymma impressum", "Adenocalymma sessile", 
                  "Adenocalymma pedunculatum", "Anemopaegma floridum", "Fridericia poeppigii", "Cuspidaria floribunda", "Stizophyllum inaequilaterum", 
                  "Amphilophium lactiflorum", "Cuspidaria lasiantha", "Manaosella cordifolia", "Tanaecium xanthophyllum", "Anemopaegma insculptum", 
                  "Stizophyllum riparium", "Adenocalymma bracteatum", "Adenocalymma marginatum", "Tynanthus polyanthus", "Tanaecium pyramidatum", 
                  "Mansoa verrucifera", "Adenocalymma patulum", "Callichlamys latifolia", "Anemopaegma hilarianum", "Fridericia platyphylla", 
                  "Amphilophium monophyllum", "Dolichandra chodatii", "Lundia nitidula", "Fridericia viscida", "Anemopaegma colombianum", 
                  "Amphilophium buccinatorium", "Anemopaegma karstenii", "Dolichandra uncata", "Pyrostegia venusta", "Bignonia prieurii", 
                  "Fridericia floribunda", "Lundia densiflora", "Xylophragma seemannianum", "Bignonia corymbosa", "Martinella obovata", 
                  "Fridericia schumanniana", "Fridericia speciosa", "Pleonotoma variabilis", "Anemopaegma chrysanthum", "Mansoa sagotii", 
                  "Tanaecium tetragonolobum", "Adenocalymma grandifolium", "Anemopaegma rugosum", "Fridericia chica", "Lundia corymbifera", 
                  "Fridericia patellifera", "Fridericia mollissima", "Bignonia diversifolia", "Fridericia pubescens", "Amphilophium sandwithii", 
                  "Amphilophium aschersonii", "Pachyptera kerere", "Adenocalymma trifoliatum", "Amphilophium crucigerum", "Adenocalymma bracteolatum", 
                  "Fridericia truncata", "Dolichandra quadrivalvis", "Adenocalymma apurense", "Amphilophium laxiflorum", "Bignonia magnifica", 
                  "Adenocalymma cladotrichum", "Fridericia candicans", "Lundia puberula", "Tynanthus schumannianus", "Bignonia sciuripabulum", 
                  "Amphilophium carolinae", "Bignonia aequinoctialis", "Anemopaegma puberulum", "Amphilophium ecuadorense", "Dolichandra unguis-cati", 
                  "Tanaecium selloi", "Fridericia florida", "Amphilophium pannosum", "Amphilophium paniculatum")
  
  ## Function designed to operate with conflicts between 2, 3 or 5 consensus areas.
  
  if (length(ca_names) == 2) {
    
    ca1 <- ca_df %>% 
      filter(Spatial_Scale == con_scale, Consensus_Area == ca_names[1]) %>% 
      pull(Species) 
    
    ca1 <- plot_order[which(plot_order %in% ca1)]
    
    ca2 <- ca_df %>% 
      filter(Spatial_Scale == con_scale, Consensus_Area == ca_names[2]) %>% 
      pull(Species)
    
    ca2 <- plot_order[which(plot_order %in% ca2)]
    
    # Selecting species in conflict and reordering plotting order so species in conflict be in the middle
    ca_tot <- list(a = ca1, b = ca2)
    con_vec <- get.venn.partitions(ca_tot, force.unique = T)[4][[1]][[1]]
    
    ca1 <- ca1[which(!ca1 %in% con_vec)]
    ca2 <- ca2[which(!ca2 %in% con_vec)]
    
    cas_ord <- c(ca1, con_vec, ca2)
    
    # Subsetting elevation dataframe by consensus areas' species
    ca_sppelev <- elevat_df %>%
      filter(NAME1 %in% cas_ord)
    
    # Ordering plot by cas_ord
    ca_sppelev$NAME1 <- factor(ca_sppelev$NAME1, 
                               levels = cas_ord)
    
    # Conflicts elevation plot
    conflict_elevation_plot <- ca_sppelev %>%
      ggplot(aes(x = NAME1, y = elevation)) + 
      geom_boxplot()  +
      geom_boxplot(data = elevat_df[elevat_df$NAME1 %in% con_vec, ], 
                   aes(x = NAME1, y = elevation), 
                   fill = "red") +
      geom_boxplot(data = elevat_df[elevat_df$NAME1 %in% ca1, ], 
                   aes(x = NAME1, y = elevation), 
                   fill = "#440154FF",
                   alpha = 0.5) +
      geom_boxplot(data = elevat_df[elevat_df$NAME1 %in% ca2, ], 
                   aes(x = NAME1, y = elevation), 
                   fill = "#FDE725FF",
                   alpha = 0.5) +
      scale_color_viridis_d() +
      scale_y_continuous(breaks = seq(0, 5000, by = 500)) +
      geom_hline(yintercept = 750, lty = 2, lwd = 0.2) +
      geom_hline(yintercept = 1800, lty = 2, lwd = 0.2) +
      geom_hline(yintercept = 3600, lty = 2, lwd = 0.2) +
      theme_minimal() +
      labs(y = "msnm",
           x = NULL) +
      theme(axis.text.x = element_text(angle = 65, hjust = 1, face = "italic"),
            panel.grid = element_blank(), 
            panel.background = element_rect(fill = "white"))
    
  }
  
  if (length(ca_names) == 3) {
    
    ca1 <- ca_df %>% 
      filter(Spatial_Scale == con_scale, Consensus_Area == ca_names[1]) %>% 
      pull(Species) 
    
    ca1 <- plot_order[which(plot_order %in% ca1)]
    
    ca2 <- ca_df %>% 
      filter(Spatial_Scale == con_scale, Consensus_Area == ca_names[2]) %>% 
      pull(Species)
    
    ca2 <- plot_order[which(plot_order %in% ca2)]
    
    ca3 <- ca_df %>% 
      filter(Spatial_Scale == con_scale, Consensus_Area == ca_names[3]) %>% 
      pull(Species)
    
    ca3 <- plot_order[which(plot_order %in% ca3)]
    
    # Selecting species in conflict and reordering plotting order so species in conflict be in the middle
    ca_tot <- list(a = ca1, b = ca2, c = ca3)
    
    conflict_spp <- get.venn.partitions(ca_tot, force.unique = T)
    
    con_vec <- c(conflict_spp[5][[1]][[1]],
                 conflict_spp[5][[1]][[2]],
                 conflict_spp[5][[1]][[3]],
                 conflict_spp[5][[1]][[5]])
    
    ca1 <- ca1[which(!ca1 %in% con_vec)]
    ca2 <- ca2[which(!ca2 %in% con_vec)]
    ca3 <- ca3[which(!ca3 %in% con_vec)]
    
    cas_ord <- c(ca1, con_vec, ca2, ca3)
    
    # Subsetting elevation dataframe by consensus areas' species
    ca_sppelev <- elevat_df %>%
      filter(NAME1 %in% cas_ord)
    
    # Ordering plot by cas_ord
    ca_sppelev$NAME1 <- factor(ca_sppelev$NAME1, 
                               levels = cas_ord)
    
    # Conflicts elevation plot
    conflict_elevation_plot <- ca_sppelev %>%
      ggplot(aes(x = NAME1, y = elevation)) + 
      geom_boxplot()  +
      geom_boxplot(data = elevat_df[elevat_df$NAME1 %in% con_vec, ], 
                   aes(x = NAME1, y = elevation), 
                   fill = "red") +
      geom_boxplot(data = elevat_df[elevat_df$NAME1 %in% ca1, ], 
                   aes(x = NAME1, y = elevation), 
                   fill = "#440154FF",
                   alpha = 0.5) +
      geom_boxplot(data = elevat_df[elevat_df$NAME1 %in% ca2, ], 
                   aes(x = NAME1, y = elevation), 
                   fill = "#5DC863FF",
                   alpha = 0.5) +
      geom_boxplot(data = elevat_df[elevat_df$NAME1 %in% ca3, ], 
                   aes(x = NAME1, y = elevation), 
                   fill = "#FDE725FF",
                   alpha = 0.5) +
      scale_color_viridis_d() +
      scale_y_continuous(breaks = seq(0, 5000, by = 500)) +
      geom_hline(yintercept = 750, lty = 2, lwd = 0.2) +
      geom_hline(yintercept = 1800, lty = 2, lwd = 0.2) +
      geom_hline(yintercept = 3600, lty = 2, lwd = 0.2) +
      theme_minimal() +
      labs(y = "msnm",
           x = NULL) +
      theme(axis.text.x = element_text(angle = 65, hjust = 1, face = "italic"),
            panel.grid = element_blank(), 
            panel.background = element_rect(fill = "white"))
    
  }
  
  if (length(ca_names) == 5) {
    
    ca1 <- ca_df %>% 
      filter(Spatial_Scale == con_scale, Consensus_Area == ca_names[1]) %>% 
      pull(Species) 
    
    ca1 <- plot_order[which(plot_order %in% ca1)]
    
    ca2 <- ca_df %>% 
      filter(Spatial_Scale == con_scale, Consensus_Area == ca_names[2]) %>% 
      pull(Species)
    
    ca2 <- plot_order[which(plot_order %in% ca2)]
    
    ca3 <- ca_df %>% 
      filter(Spatial_Scale == con_scale, Consensus_Area == ca_names[3]) %>% 
      pull(Species)
    
    ca3 <- plot_order[which(plot_order %in% ca3)]
    
    ca4 <- ca_df %>% 
      filter(Spatial_Scale == con_scale, Consensus_Area == ca_names[4]) %>% 
      pull(Species)
    
    ca4 <- plot_order[which(plot_order %in% ca4)]
    
    ca5 <- ca_df %>% 
      filter(Spatial_Scale == con_scale, Consensus_Area == ca_names[5]) %>% 
      pull(Species)
    
    ca5 <- plot_order[which(plot_order %in% ca5)]
    
    # Selecting species in conflict and reordering plotting order so species in conflict be in the middle
    ca_tot <- list(a = ca1, b = ca2, c = ca3, d = ca4, e = ca5)
    
    conflict_spp <- get.venn.partitions(ca_tot, force.unique = T)
    
    con_vec <- c(conflict_spp[7][[1]][[1]],
                 conflict_spp[7][[1]][[2]],
                 conflict_spp[7][[1]][[3]],
                 conflict_spp[7][[1]][[4]],
                 conflict_spp[7][[1]][[5]],
                 conflict_spp[7][[1]][[6]],
                 conflict_spp[7][[1]][[7]],
                 conflict_spp[7][[1]][[8]],
                 conflict_spp[7][[1]][[9]],
                 conflict_spp[7][[1]][[10]],
                 conflict_spp[7][[1]][[11]],
                 conflict_spp[7][[1]][[12]],
                 conflict_spp[7][[1]][[13]],
                 conflict_spp[7][[1]][[14]],
                 conflict_spp[7][[1]][[15]],
                 conflict_spp[7][[1]][[17]],
                 conflict_spp[7][[1]][[18]],
                 conflict_spp[7][[1]][[19]],
                 conflict_spp[7][[1]][[20]],
                 conflict_spp[7][[1]][[21]],
                 conflict_spp[7][[1]][[22]],
                 conflict_spp[7][[1]][[23]],
                 conflict_spp[7][[1]][[25]],
                 conflict_spp[7][[1]][[26]],
                 conflict_spp[7][[1]][[27]],
                 conflict_spp[7][[1]][[29]])
    
    ca1 <- ca1[which(!ca1 %in% con_vec)]
    ca2 <- ca2[which(!ca2 %in% con_vec)]
    ca3 <- ca3[which(!ca3 %in% con_vec)]
    ca4 <- ca4[which(!ca4 %in% con_vec)]
    ca5 <- ca5[which(!ca5 %in% con_vec)]
    
    cas_ord <- c(ca1, ca2, con_vec, ca3, ca4, ca5)
    
    # Subsetting elevation dataframe by consensus areas' species
    ca_sppelev <- elevat_df %>%
      filter(NAME1 %in% cas_ord)
    
    # Ordering plot by cas_ord
    ca_sppelev$NAME1 <- factor(ca_sppelev$NAME1, 
                               levels = cas_ord)
    
    # Conflicts elevation plot
    conflict_elevation_plot <- ca_sppelev %>%
      ggplot(aes(x = NAME1, y = elevation)) + 
      geom_boxplot()  +
      geom_boxplot(data = elevat_df[elevat_df$NAME1 %in% con_vec, ], 
                   aes(x = NAME1, y = elevation), 
                   fill = "red") +
      geom_boxplot(data = elevat_df[elevat_df$NAME1 %in% ca1, ], 
                   aes(x = NAME1, y = elevation), 
                   fill = "#440154FF",
                   alpha = 0.5) +
      geom_boxplot(data = elevat_df[elevat_df$NAME1 %in% ca2, ], 
                   aes(x = NAME1, y = elevation), 
                   fill = "#3B528BFF",
                   alpha = 0.5) +
      geom_boxplot(data = elevat_df[elevat_df$NAME1 %in% ca3, ], 
                   aes(x = NAME1, y = elevation), 
                   fill = "#21908CFF",
                   alpha = 0.5) +
      geom_boxplot(data = elevat_df[elevat_df$NAME1 %in% ca4, ], 
                   aes(x = NAME1, y = elevation), 
                   fill = "#5DC863FF",
                   alpha = 0.5) +
      geom_boxplot(data = elevat_df[elevat_df$NAME1 %in% ca5, ], 
                   aes(x = NAME1, y = elevation), 
                   fill = "#FDE725FF",
                   alpha = 0.5) +
      scale_color_viridis_d() +
      scale_y_continuous(breaks = seq(0, 5000, by = 500)) +
      geom_hline(yintercept = 750, lty = 2, lwd = 0.2) +
      geom_hline(yintercept = 1800, lty = 2, lwd = 0.2) +
      geom_hline(yintercept = 3600, lty = 2, lwd = 0.2) +
      theme_minimal() +
      labs(y = "msnm",
           x = NULL) +
      theme(axis.text.x = element_text(angle = 65, hjust = 1, face = "italic"),
            panel.grid = element_blank(), 
            panel.background = element_rect(fill = "white"))
  }
  
  conflict_elevation_plot
  
}

############### Function ndm_visconf ##################

## Function to visualize conflicting consensus areas, conflicting species, and elevation

## Arguments:
##
## 1) layer_names: Vector with the names of consensus areas in conflict as elements.
## 2) shp_dns: file path to the directory that contains the shapefiles of consensus areas.
## 3) alfa: default = 0.3. numeric value controlling the transparency of the colors of consensus areas.
## 4) plot_title = character vector with the title of the conflict plot.
## 5) ca_df: dataframe with consensus area information about IE scores. It must posses the columns:
##          Spatial_Scale: column with an integer indicating the spatial scale among 1, 2, or 3. 
##          Species: species names.
##          Cons_Area_Min_Score, Cons_Area_Max_Score, Spp_Min_Score, Spp_Max_Score: with values of the species and consensus area IE score.
## 6) elevat_df: dataframe with information of altitude for each species record. It must possess the following columns:
##          NAME1: column with species names.
##          elevation: elevation data for each record.
## 7) con_scale: spatial scale at which the conflicts are observed.
## 8) ca_name: character vector with the name of the consensus area.

## Observations:
## There is a character vector named "plot_order" inside the function. This vector is used as levels for plot order. It follows an increasing
## order given by the size of the linear altitudinal range of each species. Further development of this function may include code to generate this
## vector automatically.

ndm_visconf <- function(layer_names, shp_dns = "./output/GIS/40perc/", alfa = 0.3, plot_title, ca_df, elevat_df, con_scale, ca_names) {
  
  library(grid)
  library(ggplot2)
  library(viridis)
  
  # map of conflicts
  ndm_conflictplot <- function(shp_dns = shp_dns, layer_names = layer_names, alfa = alfa, plot_title = plot_title) {
    
    require(rgdal)
    require(sp)
    require(sf)
    require(ggplot2)
    require(dplyr)
    
    # Internal functions
    
    # fortyfing function
    fortifying_ca <- function(x) {
      x <- st_as_sf(x)
      ca_pol <- st_union(x)
      ca_pol <- as_Spatial(ca_pol)
      ca_pol_df <- fortify(ca_pol)
      return(ca_pol_df)
    }
    
    # naming function
    naming_ca <- function(x) {
      
      if (grepl(x, pattern = ".*1dgD.*")) {
        x <- gsub(x, pattern = ".*1dgD_(.*)_.*-grid", replacement = "\\1")
        y <- paste("D1", x, sep = "")
        return(y)
      }
      
      if (grepl(x, pattern = ".*2dgD.*")) {
        x <- gsub(x, pattern = ".*2dgD_(.*)_.*-grid", replacement = "\\1")
        y <- paste("D2", x, sep = "")
        return(y)
      }
      
      if (grepl(x, pattern = ".*3dgD.*")) {
        x <- gsub(x, pattern = ".*3dgD_(.*)_.*-grid", replacement = "\\1")
        y <- paste("D3", x, sep = "")
        return(y)
      }
      
      if (grepl(x, pattern = ".*1dgS.*")) {
        x <- gsub(x, pattern = ".*1dgS_(.*)_.*-grid", replacement = "\\1")
        y <- paste("S1", x, sep = "")
        return(y)
      }
      
      if (grepl(x, pattern = ".*2dgS.*")) {
        x <- gsub(x, pattern = ".*2dgD_(.*)_.*-grid", replacement = "\\1")
        y <- paste("D2", x, sep = "")
        return(y)
      }
      
      if (grepl(x, pattern = ".*3dgS.*")) {
        x <- gsub(x, pattern = ".*3dgS_(.*)_.*-grid", replacement = "\\1")
        y <- paste("S3", x, sep = "")
        return(y)
      }
      
      
    }
    
    # Loading layers
    
    wgs84.proj4 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    Amer <- readOGR(dsn = "./data/GIS/", layer = "continent", p4s = wgs84.proj4)
    Morrone <- readOGR(dsn = "./data/GIS/", layer = "Lowenberg_Neto_2014", p4s = wgs84.proj4) # Morrone's 2014 Neotropical Region
    
    Amer_df <- fortify(Amer)
    Morrone_df <- fortify(Morrone)
    
    #Base map
    Base_map <-  ggplot(Amer_df, aes(x = long, y = lat, group = group)) + 
      geom_polygon(fill = "white", 
                   col = "black") +
      geom_polygon(data = Morrone_df,
                   aes(x = long, y = lat, group = group),
                   fill = "white",
                   col = "black",
                   size = 0.1) +
      coord_cartesian(xlim = c(-111, -34),
                      ylim = c(-37, 39),
                      expand = TRUE) +
      labs(x = NULL, y = NULL) +
      theme_linedraw() +
      theme(panel.grid = element_blank())
    
    
    if (length(layer_names) == 2) {
      
      ca1 <- readOGR(dsn = shp_dns, layer = layer_names[1], p4s = wgs84.proj4)
      ca2 <- readOGR(dsn = shp_dns, layer = layer_names[2], p4s = wgs84.proj4)
      
      ca1_poldf <- fortifying_ca(ca1)
      ca2_poldf <- fortifying_ca(ca2)
      
      ca1_name <- naming_ca(layer_names[1])
      ca2_name <- naming_ca(layer_names[2])
      
      ## Drawing legend
      
      legend_labels <- c(ca1_name,
                         ca2_name)
      
      legend_position <- c(0.78, 0.71)
      
      text_legend <- textGrob(label = legend_labels, x = 0.6, y = legend_position, just = "left") 
      
      rect_position_y <- c(0.78, 0.71)
      rect_position_x <- rep(0.55, length(rect_position_y))
      
      square_legend <- rectGrob(x = rect_position_x, y = rect_position_y, 
                                width = 0.05, height = 0.05,
                                gp = gpar(fill = viridis(2, alpha = alfa)))
      
      grob_legend <- grobTree(text_legend, square_legend, name = "jp_legendgrob")
      
      ## Conflict map
      Conf_map <- Base_map +
        geom_polygon(data = ca1_poldf, 
                     aes(x = long, y = lat, group = group),
                     fill = alpha("#440154FF", alfa),
                     colour = "black",
                     size = 0.3) +
        geom_polygon(data = ca2_poldf, 
                     aes(x = long, y = lat, group = group),
                     fill = alpha("#FDE725FF", alfa),
                     colour = "black",
                     size = 0.3) +
        annotation_custom(grob = grob_legend, 
                          xmin = -80, xmax = -20, 
                          ymin = 0, ymax = 50) +
        labs(title = plot_title)
    }
    
    if (length(layer_names) == 3) {
      
      ca1 <- readOGR(dsn = shp_dns, layer = layer_names[1], p4s = wgs84.proj4)
      ca2 <- readOGR(dsn = shp_dns, layer = layer_names[2], p4s = wgs84.proj4)
      ca3 <- readOGR(dsn = shp_dns, layer = layer_names[3], p4s = wgs84.proj4)
      
      ca1_poldf <- fortifying_ca(ca1)
      ca2_poldf <- fortifying_ca(ca2)
      ca3_poldf <- fortifying_ca(ca3)
      
      ca1_name <- naming_ca(layer_names[1])
      ca2_name <- naming_ca(layer_names[2])
      ca3_name <- naming_ca(layer_names[3])
      
      ## Drawing legend
      
      legend_labels <- c(ca1_name,
                         ca2_name,
                         ca3_name)
      
      legend_position <- c(0.78, 0.71, 0.64)
      
      text_legend <- textGrob(label = legend_labels, x = 0.6, y = legend_position, just = "left") 
      
      rect_position_y <- c(0.78, 0.71, 0.64)
      rect_position_x <- rep(0.55, length(rect_position_y))
      
      square_legend <- rectGrob(x = rect_position_x, y = rect_position_y, 
                                width = 0.05, height = 0.05,
                                gp = gpar(fill = viridis(3, alpha = alfa)))
      
      grob_legend <- grobTree(text_legend, square_legend, name = "jp_legendgrob")
      
      ## Conflict map
      Conf_map <- Base_map +
        geom_polygon(data = ca1_poldf, 
                     aes(x = long, y = lat, group = group),
                     fill = alpha("#440154FF", alfa),
                     colour = "black",
                     size = 0.3) +
        geom_polygon(data = ca2_poldf, 
                     aes(x = long, y = lat, group = group),
                     fill = alpha("#5DC863FF", alfa),
                     colour = "black",
                     size = 0.3) +
        geom_polygon(data = ca3_poldf, 
                     aes(x = long, y = lat, group = group),
                     fill = alpha("#FDE725FF", alfa),
                     colour = "black",
                     size = 0.3) +
        annotation_custom(grob = grob_legend, 
                          xmin = -80, xmax = -20, 
                          ymin = 0, ymax = 50) +
        labs(title = plot_title)
    }
    
    if (length(layer_names) == 5) {
      
      ca1 <- readOGR(dsn = shp_dns, layer = layer_names[1], p4s = wgs84.proj4)
      ca2 <- readOGR(dsn = shp_dns, layer = layer_names[2], p4s = wgs84.proj4)
      ca3 <- readOGR(dsn = shp_dns, layer = layer_names[3], p4s = wgs84.proj4)
      ca4 <- readOGR(dsn = shp_dns, layer = layer_names[4], p4s = wgs84.proj4)
      ca5 <- readOGR(dsn = shp_dns, layer = layer_names[5], p4s = wgs84.proj4)
      
      ca1_poldf <- fortifying_ca(ca1)
      ca2_poldf <- fortifying_ca(ca2)
      ca3_poldf <- fortifying_ca(ca3)
      ca4_poldf <- fortifying_ca(ca4)
      ca5_poldf <- fortifying_ca(ca5)
      
      ca1_name <- naming_ca(layer_names[1])
      ca2_name <- naming_ca(layer_names[2])
      ca3_name <- naming_ca(layer_names[3])
      ca4_name <- naming_ca(layer_names[4])
      ca5_name <- naming_ca(layer_names[5])
      
      ## Drawing legend
      
      legend_labels <- c(ca1_name,
                         ca2_name,
                         ca3_name,
                         ca4_name,
                         ca5_name)
      
      legend_position <- c(0.78, 0.71, 0.64, 0.57, 0.5)
      
      text_legend <- textGrob(label = legend_labels, x = 0.6, y = legend_position, just = "left") 
      
      rect_position_y <- c(0.78, 0.71, 0.64, 0.57, 0.5)
      rect_position_x <- rep(0.55, length(rect_position_y))
      
      square_legend <- rectGrob(x = rect_position_x, y = rect_position_y, 
                                width = 0.05, height = 0.05,
                                gp = gpar(fill = viridis(5, alpha = alfa)))
      
      grob_legend <- grobTree(text_legend, square_legend, name = "jp_legendgrob")
      
      
      ## Conflict map
      Conf_map <- Base_map +
        geom_polygon(data = ca1_poldf, 
                     aes(x = long, y = lat, group = group),
                     fill = alpha("#440154FF", alfa),
                     colour = "black",
                     size = 0.3) +
        geom_polygon(data = ca2_poldf, 
                     aes(x = long, y = lat, group = group),
                     fill = alpha("#3B528BFF", alfa),
                     colour = "black",
                     size = 0.3) +
        geom_polygon(data = ca3_poldf, 
                     aes(x = long, y = lat, group = group),
                     fill = alpha("#21908CFF", alfa),
                     colour = "black",
                     size = 0.3) +
        geom_polygon(data = ca4_poldf, 
                     aes(x = long, y = lat, group = group),
                     fill = alpha("#5DC863FF", alfa),
                     colour = "black",
                     size = 0.3) +
        geom_polygon(data = ca5_poldf, 
                     aes(x = long, y = lat, group = group),
                     fill = alpha("#FDE725FF", alfa),
                     colour = "black",
                     size = 0.3) +
        annotation_custom(grob = grob_legend, 
                          xmin = -80, xmax = -20, 
                          ymin = 0, ymax = 50) +
        labs(title = plot_title)
    }
    
    
    Conf_map
  }
  
  conflict_plot <- ndm_conflictplot(shp_dns = shp_dns, layer_names = layer_names, alfa = alfa, plot_title = plot_title)
  
  # elevation boxplot and table with IE scores for conflicting species.
  conflict_elevation <- function(ca_df = ca_df, elevat_df = elevat_df, con_scale = con_scale, ca_names = ca_names) {
    
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(VennDiagram)
    library(viridis)
    library(grid)
    library(gridExtra)
    library(gtable)
    
    
    ## Vector to define levels and plot order in boxplot chart. Order given by increasing size of the altitudinal linear range.
    plot_order <- c("Adenocalymma nervosum", "Adenocalymma apetiolatum", "Adenocalymma sastrei", "Adenocalymma cidii", "Adenocalymma ubatubense", 
                    "Adenocalymma flavum", "Amphilophium perbracteatum", "Amphilophium pauciflorum", "Amphilophium reticulatum", "Anemopaegma pachyphyllum", 
                    "Anemopaegma heringeri", "Anemopaegma villosum", "Bignonia cararensis", "Fridericia subverticillata", "Fridericia subexserta", 
                    "Mansoa longicalyx", "Pachyptera ventricosa", "Pleonotoma fissicalyx", "Pleonotoma dendrotricha", "Tanaecium apiculatum", 
                    "Adenocalymma velutinum", "Adenocalymma lineare", "Bignonia cuneata", "Anemopaegma granvillei", "Amphilophium cuneifolium", 
                    "Anemopaegma jucundum", "Pleonotoma longiflora", "Pachyptera aromatica", "Pyrostegia millingtonioides", "Adenocalymma saulense", 
                    "Tynanthus espiritosantensis", "Cuspidaria cinerea", "Adenocalymma juliae", "Adenocalymma molle", "Amphilophium lohmanniae", 
                    "Pleonotoma fluminensis", "Fridericia crassa", "Adenocalymma adenophorum", "Mansoa angustidens", "Pleonotoma exserta", 
                    "Pleonotoma bracteata", "Adenocalymma arthropetiolatum", "Anemopaegma salicifolium", "Cuspidaria bracteolata", "Fridericia lauta", 
                    "Pleonotoma echitidea", "Adenocalymma chocoense", "Amphilophium magnoliifolium", "Cuspidaria monophylla", "Adenocalymma longilineum", 
                    "Bignonia neouliginosa", "Amphilophium frutescens", "Tanaecium exitiosum", "Martinella iquitoensis", "Tynanthus sastrei", 
                    "Tynanthus densiflorus", "Adenocalymma album", "Adenocalymma gibbosum", "Amphilophium cremersii", "Anemopaegma mirabile", 
                    "Tanaecium revillae", "Amphilophium campinae", "Adenocalymma calcareum", "Adenocalymma subspicatum", "Fridericia elegans", 
                    "Bignonia phellosperma", "Anemopaegma flavum", "Anemopaegma patelliforme", "Fridericia carichanensis", "Adenocalymma moringifolium", 
                    "Anemopaegma ionanthum", "Pachyptera erythraea", "Lundia helicocalyx", "Tanaecium crucigerum", "Adenocalymma mirabile", 
                    "Tynanthus croatianus", "Tynanthus macranthus", "Mansoa minensis", "Mansoa gentryi", "Dolichandra dentata", "Fridericia pliciflora", 
                    "Tanaecium duckei", "Tanaecium ornithophilum", "Tanaecium bilabiatum", "Adenocalymma bullatum", "Adenocalymma albiflorum", 
                    "Amphilophium occidentale", "Adenocalymma gracielzae", "Bignonia costata", "Fridericia spicata", "Tynanthus panurensis", 
                    "Adenocalymma magnificum", "Adenocalymma subincanum", "Fridericia japurensis", "Anemopaegma nebulosum", "Bignonia microcalyx", 
                    "Fridericia oligantha", "Lundia laevis", "Amphilophium laeve", "Tanaecium affine", "Mansoa ivanii", "Adenocalymma ackermannii", 
                    "Adenocalymma bracteosum", "Fridericia whitei", "Fridericia fagoides", "Amphilophium pilosum", "Amphilophium parkeri", 
                    "Fridericia subincana", "Martinella insignis", "Tanaecium paradoxum", "Lundia gardneri", "Bignonia sordida", "Adenocalymma schomburgkii", 
                    "Fridericia prancei", "Adenocalymma cristicalyx", "Tynanthus pubescens", "Bignonia sanctae-crucis", "Adenocalymma biternatum", 
                    "Fridericia grosourdyana", "Amphilophium blanchetii", "Adenocalymma tanaeciicarpum", "Anemopaegma robustum", "Anemopaegma longipetiolatum",
                    "Adenocalymma hirtum", "Fridericia caudigera", "Amphilophium nunezii", "Cuspidaria lachnaea", "Adenocalymma uleanum", "Bignonia pterocalyx",
                    "Cuspidaria multiflora", "Amphilophium rodriguesii", "Cuspidaria bracteata", "Tanaecium caudiculatum", "Fridericia oxycarpa",
                    "Amphilophium elongatum", "Anemopaegma foetidum", "Cuspidaria octoptera", "Anemopaegma longidens", "Pleonotoma pavettiflora", 
                    "Fridericia formosa", "Adenocalymma flaviflorum", "Fridericia mollis", "Anemopaegma santaritense", "Tanaecium jaroba", 
                    "Xylophragma heterocalyx", "Mansoa hirsuta", "Cuspidaria emmonsii", "Tynanthus labiatus", "Adenocalymma subsessilifolium", 
                    "Bignonia potosina", "Tynanthus fasciculatus", "Tanaecium neobrasiliense", "Adenocalymma cinereum", "Amphilophium dasytrichum", 
                    "Anemopaegma album", "Fridericia nigrescens", "Lundia damazioi", "Bignonia capreolata", "Adenocalymma contractum", "Adenocalymma dusenii", 
                    "Anemopaegma chrysoleucum", "Fridericia nicotianiflora", "Xylophragma platyphyllum", "Anemopaegma setilobum", "Bignonia binata", 
                    "Pleonotoma orientalis", "Amphilophium gnaphalanthum", "Anemopaegma goyazense", "Cuspidaria cratensis", "Adenocalymma allamandiflorum", 
                    "Mansoa parvifolia", "Tanaecium parviflorum", "Cuspidaria argentea", "Tynanthus guatemalensis", "Tynanthus micranthus", 
                    "Fridericia egensis", "Anemopaegma pabstii", "Bignonia ramentacea", "Fridericia podopogon", "Adenocalymma apparicianum", 
                    "Amphilophium stamineum", "Fridericia mutabilis", "Anemopaegma acutifolium", "Anemopaegma alatum", "Adenocalymma aurantiacum", 
                    "Cuspidaria subincana", "Fridericia limae", "Fridericia tynanthoides", "Tanaecium cyrtanthum", "Bignonia bracteomana", 
                    "Amphilophium scabriusculum", "Adenocalymma dichilum", "Bignonia campanulata", "Fridericia leucopogon", "Fridericia arthrerion", 
                    "Cuspidaria inaequalis", "Fridericia bahiensis", "Cuspidaria weberbaueri", "Mansoa onohualcoides", "Adenocalymma hypostictum", 
                    "Fridericia fanshawei", "Bignonia lilacina", "Fridericia rego", "Adenocalymma coriaceum", "Anemopaegma parkeri", "Bignonia uleana", 
                    "Anemopaegma orbiculatum", "Xylophragma myrianthum", "Mansoa lanceolata", "Lundia obliqua", "Adenocalymma alboaurantiacum", 
                    "Lundia virginalis", "Adenocalymma scabriusculum", "Adenocalymma cymbalum", "Mansoa alliacea", "Xylophragma harleyi", 
                    "Bignonia decora", "Bignonia longiflora", "Adenocalymma salmoneum", "Adenocalymma hatschbachii", "Anemopaegma prostratum", 
                    "Amphilophium obovatum", "Fridericia craterophora", "Pleonotoma castelnaei", "Mansoa hymenaea", "Fridericia costaricensis", 
                    "Mansoa paganuccii", "Amphilophium pulverulentum", "Fridericia simplex", "Anemopaegma laeve", "Fridericia tuberculata", 
                    "Pleonotoma stichadenia", "Pleonotoma clematis", "Amphilophium porphyrotrichum", "Anemopaegma brevipes", "Amphilophium bauhinioides", 
                    "Tanaecium tetramerum", "Adenocalymma nodosum", "Anemopaegma glaucum", "Anemopaegma citrinum", "Fridericia cinerea", 
                    "Fridericia cuneifolia", "Dolichandra unguiculata", "Bignonia neoheterophylla", "Adenocalymma candolleanum", "Cuspidaria simplicifolia", 
                    "Anemopaegma arvense", "Pleonotoma jasminifolia", "Adenocalymma paulistarum", "Lundia longa", "Anemopaegma gracile", "Pleonotoma melioides",
                    "Amphilophium bracteatum", "Stizophyllum perforatum", "Fridericia dispar", "Tanaecium dichotomum", "Adenocalymma peregrinum", 
                    "Fridericia trachyphylla", "Cuspidaria pulchella", "Fridericia trailii", "Dolichandra hispida", "Perianthomega vellozoi", 
                    "Fridericia triplinervia", "Cuspidaria sceptrum", "Adenocalymma aspericarpum", "Amphilophium chocoense", "Cuspidaria convoluta", 
                    "Tynanthus cognatus", "Mansoa difficilis", "Tanaecium truncatum", "Adenocalymma macrophyllum", "Amphilophium granulosum", 
                    "Mansoa glaziovii", "Fridericia cinnamomea", "Cuspidaria pulchra", "Bignonia callistegioides", "Anemopaegma paraense", 
                    "Fridericia conjugata", "Adenocalymma validum", "Mansoa standleyi", "Amphilophium steyermarkii", "Adenocalymma pubescens", 
                    "Xylophragma pratense", "Lundia erionema", "Anemopaegma velutinum", "Lundia spruceana", "Dolichandra steyermarkii", 
                    "Adenocalymma divaricatum", "Anemopaegma scabriusculum", "Xylophragma corchoroides", "Cuspidaria lateriflora", "Adenocalymma dugandii", 
                    "Dolichandra cynanchoides", "Anemopaegma oligoneuron", "Bignonia noterophila", "Amphilophium racemosum", "Anemopaegma grandifolium", 
                    "Amphilophium dusenianum", "Bignonia nocturna", "Pleonotoma tetraquetra", "Adenocalymma magdalenense", "Amphilophium arenarium", 
                    "Fridericia samydoides", "Fridericia erubescens", "Amphilophium dolichoides", "Fridericia pearcei", "Amphilophium mansoanum", 
                    "Amphilophium falcatum", "Anemopaegma chamberlaynii", "Bignonia hyacinthina", "Adenocalymma impressum", "Adenocalymma sessile", 
                    "Adenocalymma pedunculatum", "Anemopaegma floridum", "Fridericia poeppigii", "Cuspidaria floribunda", "Stizophyllum inaequilaterum", 
                    "Amphilophium lactiflorum", "Cuspidaria lasiantha", "Manaosella cordifolia", "Tanaecium xanthophyllum", "Anemopaegma insculptum", 
                    "Stizophyllum riparium", "Adenocalymma bracteatum", "Adenocalymma marginatum", "Tynanthus polyanthus", "Tanaecium pyramidatum", 
                    "Mansoa verrucifera", "Adenocalymma patulum", "Callichlamys latifolia", "Anemopaegma hilarianum", "Fridericia platyphylla", 
                    "Amphilophium monophyllum", "Dolichandra chodatii", "Lundia nitidula", "Fridericia viscida", "Anemopaegma colombianum", 
                    "Amphilophium buccinatorium", "Anemopaegma karstenii", "Dolichandra uncata", "Pyrostegia venusta", "Bignonia prieurii", 
                    "Fridericia floribunda", "Lundia densiflora", "Xylophragma seemannianum", "Bignonia corymbosa", "Martinella obovata", 
                    "Fridericia schumanniana", "Fridericia speciosa", "Pleonotoma variabilis", "Anemopaegma chrysanthum", "Mansoa sagotii", 
                    "Tanaecium tetragonolobum", "Adenocalymma grandifolium", "Anemopaegma rugosum", "Fridericia chica", "Lundia corymbifera", 
                    "Fridericia patellifera", "Fridericia mollissima", "Bignonia diversifolia", "Fridericia pubescens", "Amphilophium sandwithii", 
                    "Amphilophium aschersonii", "Pachyptera kerere", "Adenocalymma trifoliatum", "Amphilophium crucigerum", "Adenocalymma bracteolatum", 
                    "Fridericia truncata", "Dolichandra quadrivalvis", "Adenocalymma apurense", "Amphilophium laxiflorum", "Bignonia magnifica", 
                    "Adenocalymma cladotrichum", "Fridericia candicans", "Lundia puberula", "Tynanthus schumannianus", "Bignonia sciuripabulum", 
                    "Amphilophium carolinae", "Bignonia aequinoctialis", "Anemopaegma puberulum", "Amphilophium ecuadorense", "Dolichandra unguis-cati", 
                    "Tanaecium selloi", "Fridericia florida", "Amphilophium pannosum", "Amphilophium paniculatum")
    
    ## Function designed to operate with conflicts between 2, 3 or 5 consensus areas.
    
    if (length(ca_names) == 2) {
      
      ca1 <- ca_df %>% 
        filter(Spatial_Scale == con_scale, Consensus_Area == ca_names[1]) %>% 
        pull(Species) 
      
      ca1 <- plot_order[which(plot_order %in% ca1)]
      
      ca2 <- ca_df %>% 
        filter(Spatial_Scale == con_scale, Consensus_Area == ca_names[2]) %>% 
        pull(Species)
      
      ca2 <- plot_order[which(plot_order %in% ca2)]
      
      # Selecting species in conflict and reordering plotting order so species in conflict be in the middle
      ca_tot <- list(a = ca1, b = ca2)
      con_vec <- get.venn.partitions(ca_tot, force.unique = T)[4][[1]][[1]]
      
      ca1 <- ca1[which(!ca1 %in% con_vec)]
      ca2 <- ca2[which(!ca2 %in% con_vec)]
      
      cas_ord <- c(ca1, con_vec, ca2)
      
      # Subsetting elevation dataframe by consensus areas' species
      ca_sppelev <- elevat_df %>%
        filter(NAME1 %in% cas_ord)
      
      # Ordering plot by cas_ord
      ca_sppelev$NAME1 <- factor(ca_sppelev$NAME1, 
                                 levels = cas_ord)
      
      # Dataframe with conflicting species
      
      conspp_df <- ca_df %>% 
        filter(Species %in% con_vec, Spatial_Scale == con_scale) %>%
        mutate(Ca_IE_range = paste(Cons_Area_Min_Score, Cons_Area_Max_Score,
                                   sep = " - "),
               Spp_IE_range = paste(Spp_Min_Score, Spp_Max_Score, sep = " - ")) %>%
        dplyr::select(Species, Consensus_Area, Spp_IE_range) %>% 
        filter(Consensus_Area %in% ca_names) %>%
        spread(key = Consensus_Area, value = Spp_IE_range)
      
      ca_rangeval <- ca_df %>% 
        filter(Species %in% con_vec, Spatial_Scale == con_scale, Consensus_Area %in% ca_names) %>%
        mutate(Ca_IE_range = paste(Cons_Area_Min_Score, Cons_Area_Max_Score,
                                   sep = " - "),
               Spp_IE_range = paste(Spp_Min_Score, Spp_Max_Score, sep = " - ")) %>%
        dplyr::select(Consensus_Area, Ca_IE_range)
      
      ca_rangeval$Consensus_Area <- gsub("^CA ([0-9]+)", paste("D", con_scale, "CA", "\\1", sep = ""), ca_rangeval$Consensus_Area)
      
      ca_rangeval <- unite(ca_rangeval, coldf_names, Consensus_Area, Ca_IE_range, sep = "\n")
      
      ca_rangeval <- unique(ca_rangeval)
      
      colnames(conspp_df) <- c("Species", ca_rangeval[1,], ca_rangeval[2,])
      
      # Dataframe of conflicting species as gridtable
      
      con_df <- tableGrob(conspp_df, rows = NULL, theme = ttheme_minimal())
      con_df <- gtable_add_grob(con_df,
                                grobs = segmentsGrob(x0 = unit(0,"npc"),
                                                     y0 = unit(0,"npc"),
                                                     x1 = unit(1,"npc"),
                                                     y1 = unit(0,"npc"), 
                                                     gp = gpar(lwd = 1)), 
                                t = 1, b = nrow(con_df), l = 1, r = ncol(con_df))
      
      
      con_df <- gtable_add_grob(con_df,
                                grobs = rectGrob(gp = gpar(fill = NA, lwd = 1)), 
                                t = 1, b = 1, l = 1, r = ncol(con_df))
      
      # Conflicts elevation plot
      conflict_elevation_plot <- ca_sppelev %>%
        ggplot(aes(x = NAME1, y = elevation)) + 
        geom_boxplot()  +
        geom_boxplot(data = elevat_df[elevat_df$NAME1 %in% con_vec, ], 
                     aes(x = NAME1, y = elevation), 
                     fill = "red") +
        geom_boxplot(data = elevat_df[elevat_df$NAME1 %in% ca1, ], 
                     aes(x = NAME1, y = elevation), 
                     fill = "#440154FF",
                     alpha = 0.5) +
        geom_boxplot(data = elevat_df[elevat_df$NAME1 %in% ca2, ], 
                     aes(x = NAME1, y = elevation), 
                     fill = "#FDE725FF",
                     alpha = 0.5) +
        scale_color_viridis_d() +
        scale_y_continuous(breaks = seq(0, 5000, by = 500)) +
        geom_hline(yintercept = 750, lty = 2, lwd = 0.2) +
        geom_hline(yintercept = 1800, lty = 2, lwd = 0.2) +
        geom_hline(yintercept = 3600, lty = 2, lwd = 0.2) +
        theme_minimal() +
        labs(y = "msnm",
             x = NULL) +
        theme(axis.text.x = element_blank(),
              panel.grid = element_blank(), 
              panel.background = element_rect(fill = "white"))
      
    }
    
    if (length(ca_names) == 3) {
      
      ca1 <- ca_df %>% 
        filter(Spatial_Scale == con_scale, Consensus_Area == ca_names[1]) %>% 
        pull(Species) 
      
      ca1 <- plot_order[which(plot_order %in% ca1)]
      
      ca2 <- ca_df %>% 
        filter(Spatial_Scale == con_scale, Consensus_Area == ca_names[2]) %>% 
        pull(Species)
      
      ca2 <- plot_order[which(plot_order %in% ca2)]
      
      ca3 <- ca_df %>% 
        filter(Spatial_Scale == con_scale, Consensus_Area == ca_names[3]) %>% 
        pull(Species)
      
      ca3 <- plot_order[which(plot_order %in% ca3)]
      
      # Selecting species in conflict and reordering plotting order so species in conflict be in the middle
      ca_tot <- list(a = ca1, b = ca2, c = ca3)
      
      conflict_spp <- get.venn.partitions(ca_tot, force.unique = T)
      
      con_vec <- c(conflict_spp[5][[1]][[1]],
                   conflict_spp[5][[1]][[2]],
                   conflict_spp[5][[1]][[3]],
                   conflict_spp[5][[1]][[5]])
      
      ca1 <- ca1[which(!ca1 %in% con_vec)]
      ca2 <- ca2[which(!ca2 %in% con_vec)]
      ca3 <- ca3[which(!ca3 %in% con_vec)]
      
      cas_ord <- c(ca1, con_vec, ca2, ca3)
      
      # Subsetting elevation dataframe by consensus areas' species
      ca_sppelev <- elevat_df %>%
        filter(NAME1 %in% cas_ord)
      
      # Ordering plot by cas_ord
      ca_sppelev$NAME1 <- factor(ca_sppelev$NAME1, 
                                 levels = cas_ord)
      
      # Dataframe with conflicting species
      
      conspp_df <- ca_df %>% 
        filter(Species %in% con_vec, Spatial_Scale == con_scale) %>%
        mutate(Ca_IE_range = paste(Cons_Area_Min_Score, Cons_Area_Max_Score,
                                   sep = " - "),
               Spp_IE_range = paste(Spp_Min_Score, Spp_Max_Score, sep = " - ")) %>%
        dplyr::select(Species, Consensus_Area, Spp_IE_range) %>% 
        filter(Consensus_Area %in% ca_names) %>%
        spread(key = Consensus_Area, value = Spp_IE_range)
      
      ca_rangeval <- ca_df %>% 
        filter(Species %in% con_vec, Spatial_Scale == con_scale, Consensus_Area %in% ca_names) %>%
        mutate(Ca_IE_range = paste(Cons_Area_Min_Score, Cons_Area_Max_Score,
                                   sep = " - "),
               Spp_IE_range = paste(Spp_Min_Score, Spp_Max_Score, sep = " - ")) %>%
        dplyr::select(Consensus_Area, Ca_IE_range)
      
      ca_rangeval$Consensus_Area <- gsub("^CA ([0-9]+)", paste("D", con_scale, "CA", "\\1", sep = ""), ca_rangeval$Consensus_Area)
      
      ca_rangeval <- unite(ca_rangeval, coldf_names, Consensus_Area, Ca_IE_range, sep = "\n")
      
      ca_rangeval <- unique(ca_rangeval)
      
      colnames(conspp_df) <- c("Species", ca_rangeval[1,], ca_rangeval[2,], ca_rangeval[3,])
      
      # Dataframe of conflicting species as gridtable
      
      con_df <- tableGrob(conspp_df, rows = NULL, theme = ttheme_minimal())
      con_df <- gtable_add_grob(con_df,
                                grobs = segmentsGrob(x0 = unit(0,"npc"),
                                                     y0 = unit(0,"npc"),
                                                     x1 = unit(1,"npc"),
                                                     y1 = unit(0,"npc"), 
                                                     gp = gpar(lwd = 1)), 
                                t = 1, b = nrow(con_df), l = 1, r = ncol(con_df))
      
      
      con_df <- gtable_add_grob(con_df,
                                grobs = rectGrob(gp = gpar(fill = NA, lwd = 1)), 
                                t = 1, b = 1, l = 1, r = ncol(con_df))
      
      # Conflicts elevation plot
      conflict_elevation_plot <- ca_sppelev %>%
        ggplot(aes(x = NAME1, y = elevation)) + 
        geom_boxplot()  +
        geom_boxplot(data = elevat_df[elevat_df$NAME1 %in% con_vec, ], 
                     aes(x = NAME1, y = elevation), 
                     fill = "red") +
        geom_boxplot(data = elevat_df[elevat_df$NAME1 %in% ca1, ], 
                     aes(x = NAME1, y = elevation), 
                     fill = "#440154FF",
                     alpha = 0.5) +
        geom_boxplot(data = elevat_df[elevat_df$NAME1 %in% ca2, ], 
                     aes(x = NAME1, y = elevation), 
                     fill = "#21908CFF",
                     alpha = 0.5) +
        geom_boxplot(data = elevat_df[elevat_df$NAME1 %in% ca3, ], 
                     aes(x = NAME1, y = elevation), 
                     fill = "#FDE725FF",
                     alpha = 0.5) +
        scale_color_viridis_d() +
        scale_y_continuous(breaks = seq(0, 5000, by = 500)) +
        geom_hline(yintercept = 750, lty = 2, lwd = 0.2) +
        geom_hline(yintercept = 1800, lty = 2, lwd = 0.2) +
        geom_hline(yintercept = 3600, lty = 2, lwd = 0.2) +
        theme_minimal() +
        labs(y = "msnm",
             x = NULL) +
        theme(axis.text.x = element_blank(),
              panel.grid = element_blank(), 
              panel.background = element_rect(fill = "white"))
      
    }
    
    if (length(ca_names) == 5) {
      
      ca1 <- ca_df %>% 
        filter(Spatial_Scale == con_scale, Consensus_Area == ca_names[1]) %>% 
        pull(Species) 
      
      ca1 <- plot_order[which(plot_order %in% ca1)]
      
      ca2 <- ca_df %>% 
        filter(Spatial_Scale == con_scale, Consensus_Area == ca_names[2]) %>% 
        pull(Species)
      
      ca2 <- plot_order[which(plot_order %in% ca2)]
      
      ca3 <- ca_df %>% 
        filter(Spatial_Scale == con_scale, Consensus_Area == ca_names[3]) %>% 
        pull(Species)
      
      ca3 <- plot_order[which(plot_order %in% ca3)]
      
      ca4 <- ca_df %>% 
        filter(Spatial_Scale == con_scale, Consensus_Area == ca_names[4]) %>% 
        pull(Species)
      
      ca4 <- plot_order[which(plot_order %in% ca4)]
      
      ca5 <- ca_df %>% 
        filter(Spatial_Scale == con_scale, Consensus_Area == ca_names[5]) %>% 
        pull(Species)
      
      ca5 <- plot_order[which(plot_order %in% ca5)]
      
      # Selecting species in conflict and reordering plotting order so species in conflict be in the middle
      ca_tot <- list(a = ca1, b = ca2, c = ca3, d = ca4, e = ca5)
      
      conflict_spp <- get.venn.partitions(ca_tot, force.unique = T)
      
      con_vec <- c(conflict_spp[7][[1]][[1]],
                   conflict_spp[7][[1]][[2]],
                   conflict_spp[7][[1]][[3]],
                   conflict_spp[7][[1]][[4]],
                   conflict_spp[7][[1]][[5]],
                   conflict_spp[7][[1]][[6]],
                   conflict_spp[7][[1]][[7]],
                   conflict_spp[7][[1]][[8]],
                   conflict_spp[7][[1]][[9]],
                   conflict_spp[7][[1]][[10]],
                   conflict_spp[7][[1]][[11]],
                   conflict_spp[7][[1]][[12]],
                   conflict_spp[7][[1]][[13]],
                   conflict_spp[7][[1]][[14]],
                   conflict_spp[7][[1]][[15]],
                   conflict_spp[7][[1]][[17]],
                   conflict_spp[7][[1]][[18]],
                   conflict_spp[7][[1]][[19]],
                   conflict_spp[7][[1]][[20]],
                   conflict_spp[7][[1]][[21]],
                   conflict_spp[7][[1]][[22]],
                   conflict_spp[7][[1]][[23]],
                   conflict_spp[7][[1]][[25]],
                   conflict_spp[7][[1]][[26]],
                   conflict_spp[7][[1]][[27]],
                   conflict_spp[7][[1]][[29]])
      
      ca1 <- ca1[which(!ca1 %in% con_vec)]
      ca2 <- ca2[which(!ca2 %in% con_vec)]
      ca3 <- ca3[which(!ca3 %in% con_vec)]
      ca4 <- ca4[which(!ca4 %in% con_vec)]
      ca5 <- ca5[which(!ca5 %in% con_vec)]
      
      cas_ord <- c(ca1, ca2, con_vec, ca3, ca4, ca5)
      
      # Subsetting elevation dataframe by consensus areas' species
      ca_sppelev <- elevat_df %>%
        filter(NAME1 %in% cas_ord)
      
      # Ordering plot by cas_ord
      ca_sppelev$NAME1 <- factor(ca_sppelev$NAME1, 
                                 levels = cas_ord)
      
      # Dataframe with conflicting species
      
      conspp_df <- ca_df %>% 
        filter(Species %in% con_vec, Spatial_Scale == con_scale) %>%
        mutate(Ca_IE_range = paste(Cons_Area_Min_Score, Cons_Area_Max_Score,
                                   sep = " - "),
               Spp_IE_range = paste(Spp_Min_Score, Spp_Max_Score, sep = " - ")) %>%
        dplyr::select(Species, Consensus_Area, Spp_IE_range) %>% 
        filter(Consensus_Area %in% ca_names) %>%
        spread(key = Consensus_Area, value = Spp_IE_range)
      
      ca_rangeval <- ca_df %>% 
        filter(Species %in% con_vec, Spatial_Scale == con_scale, Consensus_Area %in% ca_names) %>%
        mutate(Ca_IE_range = paste(Cons_Area_Min_Score, Cons_Area_Max_Score,
                                   sep = " - "),
               Spp_IE_range = paste(Spp_Min_Score, Spp_Max_Score, sep = " - ")) %>%
        dplyr::select(Consensus_Area, Ca_IE_range)
      
      ca_rangeval$Consensus_Area <- gsub("^CA ([0-9]+)", paste("D", con_scale, "CA", "\\1", sep = ""), ca_rangeval$Consensus_Area)
      
      ca_rangeval <- unite(ca_rangeval, coldf_names, Consensus_Area, Ca_IE_range, sep = "\n")
      
      ca_rangeval <- unique(ca_rangeval)
      
      colnames(conspp_df) <- c("Species", ca_rangeval[1,], ca_rangeval[2,], ca_rangeval[3,], ca_rangeval[4,], ca_rangeval[5,])
      
      # Dataframe of conflicting species as gridtable
      
      con_df <- tableGrob(conspp_df, rows = NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.7)),
                                                                         colhead = list(fg_params=list(cex = 0.7)),
                                                                         rowhead = list(fg_params=list(cex = 0.7))))
      con_df <- gtable_add_grob(con_df,
                                grobs = segmentsGrob(x0 = unit(0,"npc"),
                                                     y0 = unit(0,"npc"),
                                                     x1 = unit(1,"npc"),
                                                     y1 = unit(0,"npc"), 
                                                     gp = gpar(lwd = 1)), 
                                t = 1, b = nrow(con_df), l = 1, r = ncol(con_df))
      
      
      con_df <- gtable_add_grob(con_df,
                                grobs = rectGrob(gp = gpar(fill = NA, lwd = 1)), 
                                t = 1, b = 1, l = 1, r = ncol(con_df))
      
      # Conflicts elevation plot
      conflict_elevation_plot <- ca_sppelev %>%
        ggplot(aes(x = NAME1, y = elevation)) + 
        geom_boxplot()  +
        geom_boxplot(data = elevat_df[elevat_df$NAME1 %in% con_vec, ], 
                     aes(x = NAME1, y = elevation), 
                     fill = "red") +
        geom_boxplot(data = elevat_df[elevat_df$NAME1 %in% ca1, ], 
                     aes(x = NAME1, y = elevation), 
                     fill = "#440154FF",
                     alpha = 0.5) +
        geom_boxplot(data = elevat_df[elevat_df$NAME1 %in% ca2, ], 
                     aes(x = NAME1, y = elevation), 
                     fill = "#3B528BFF",
                     alpha = 0.5) +
        geom_boxplot(data = elevat_df[elevat_df$NAME1 %in% ca3, ], 
                     aes(x = NAME1, y = elevation), 
                     fill = "#21908CFF",
                     alpha = 0.5) +
        geom_boxplot(data = elevat_df[elevat_df$NAME1 %in% ca4, ], 
                     aes(x = NAME1, y = elevation), 
                     fill = "#5DC863FF",
                     alpha = 0.5) +
        geom_boxplot(data = elevat_df[elevat_df$NAME1 %in% ca5, ], 
                     aes(x = NAME1, y = elevation), 
                     fill = "#FDE725FF",
                     alpha = 0.5) +
        scale_color_viridis_d() +
        scale_y_continuous(breaks = seq(0, 5000, by = 500)) +
        geom_hline(yintercept = 750, lty = 2, lwd = 0.2) +
        geom_hline(yintercept = 1800, lty = 2, lwd = 0.2) +
        geom_hline(yintercept = 3600, lty = 2, lwd = 0.2) +
        theme_minimal() +
        labs(y = "msnm",
             x = NULL) +
        theme(axis.text.x = element_blank(),
              panel.grid = element_blank(), 
              panel.background = element_rect(fill = "white"))
    }
    
    plot_boxtable <- arrangeGrob(conflict_elevation_plot, con_df,
                                 top = 1, bottom = 1, left = 1, right = 1)
    
    #return(plot_boxtable)
    
  }
  
  conf_elev_plot <- conflict_elevation(ca_df = ca_df, elevat_df = elevat_df, con_scale = con_scale, ca_names = ca_names)
  
  lay <- matrix(c(1, 1, 2, 2, 2,
                  1, 1, 3, 3, 3), nrow = 2, byrow = TRUE)
  
  composition <- grid.arrange(conflict_plot, conf_elev_plot[1], conf_elev_plot[2], layout_matrix = lay, respect = TRUE)
  
  return(composition)
  
}


############### Function ndm_contrast ##########

## This function compares consensus areas versus a specified biogeographical regionalization.
## Two measures are given:
## 1) Biogeographical region homogeneity (Hb): It refers to the proportion of the consensus area covering a particular bioregion. It is calculated by
##    calculating the intersecting area between de consensus area and the bioregion, divided by the area of the bioregion, and 
##    multiplied by 100. The value expresses the percentage of the bioregion covered by the specified consensus area.
##
## 2) Consensus area homogeneity (Hc): It refers to the proportion of the bioregion convering the consensus area. It expresses how much of the consensus area
##    is inside the bioregion. The greater the value, the greater the proportion covered by one bioregion; the lesser the value, the greater
##    the proportion covered by several bioregions. It is calculated by estimating the intersecting area between de consensus area and the
##    bioregion, divided by the area of the consensus area, divided by the area of the bioregion, and multiplied by 100.
##
## NOTE: This is inspired in the mapcurves method implemented in the sabre R package (Nowosad and Stepinski. 2018). However, our taks here is
## to compare an individual area against a regionalization and not comparing two regionalizations.

## Arguments:
## ca_dsn = directory in which the consensus area layer is saved
## ca_layer = name of consensus area layer
## reg_file = file path to the shape file with the regionalization. Attribute table must contain a column called "Regions" where the names
## of biogeographical areas area stores. If the shapefile for Morrone corresponds to Lowenberg-Neto et al. 2014, the named colums
## already included in this file are appropriate for this function. 

ndm_contrast <- function(ca_dsn, ca_layer, reg_file, reg_layer) {
  
  library(dplyr)
  library(rgdal)
  library(sf)
  library(rmapshaper)
  
  wgs84.proj4 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  ca_pol <- readOGR(dsn = ca_dsn, layer = ca_layer, p4s = wgs84.proj4)
  ca_pol <- st_as_sf(ca_pol)
  ca_pol <- st_union(ca_pol)
  
  # Selecting name
  if (grepl(ca_layer, pattern = ".*1dgD.*")) {
    x <- gsub(ca_layer, pattern = ".*1dgD_(.*)_.*-grid", replacement = "\\1")
    y <- paste("D1", x, sep = "")
  }
  
  if (grepl(ca_layer, pattern = ".*2dgD.*")) {
    x <- gsub(ca_layer, pattern = ".*2dgD_(.*)_.*-grid", replacement = "\\1")
    y <- paste("D2", x, sep = "")
  }
  
  if (grepl(ca_layer, pattern = ".*3dgD.*")) {
    x <- gsub(ca_layer, pattern = ".*3dgD_(.*)_.*-grid", replacement = "\\1")
    y <- paste("D3", x, sep = "")
  }
  
  # Creating dataframe with area name
  ca_pol <- st_sf(name = y, ca_pol)
  
  # Reading biogeographic regionalization shapefile
  Reg <- st_read(reg_file)
  Reg <- st_transform(Reg, crs = st_crs(ca_pol, asText = TRUE))
  
  # Obtaining the intersection of the consensus area with each biogeographical area
  CA_and_Reg <- st_intersection(ca_pol, Reg)
  
  # Extracting names of the biogeographical areas. Columns according to Morrone or using the column "Region" from attribute table in shapefile
  Neo_region <- unique(CA_and_Reg$REGION)
  Neo_region <- Neo_region[!is.na(Neo_region)]
  subreg <- unique(CA_and_Reg$Subregio_1) 
  subreg <- subreg[!is.na(subreg)]
  domini <- unique(CA_and_Reg$Dominions)
  domini <- domini[!is.na(domini)]
  provin <- unique(CA_and_Reg$Province_1)
  provin <- provin[!is.na(provin)]
  gen_region <- unique(CA_and_Reg$Region)
  gen_region <- gen_region[!is.na(gen_region)]
  # Calculating consensus area homogeneity and bioregion homogeneity 
  
  # Neotropical region in Morrone's proposal
  vec_hNReg <- vector(mode = "numeric", length = length(Neo_region))
  vec_cNReg <- vector(mode = "numeric", length = length(Neo_region))
  for (i in seq_along(Neo_region)) {
    
    a_inter_NReg <- CA_and_Reg %>%
      filter(REGION == Neo_region[i])  %>%
      st_union() %>%
      st_area()
    
    a_total_NReg <- Reg %>%
      filter(REGION == Neo_region[i]) %>%
      st_union() %>%
      st_area()  
    
    Homo_NReg <- round(a_inter_NReg / st_area(ca_pol) * 100, digits = 4) 
    Comp_NReg <- round(a_inter_NReg / a_total_NReg * 100, digits = 4) 
    
    vec_hNReg[i] <- Homo_NReg 
    vec_cNReg[i] <- Comp_NReg 
    
  }
  NReg_Comp_Hom_df <- cbind(Bioregion = as.character(Neo_region), Hb = vec_cNReg, Hc = vec_hNReg)
  
  # Subregions in Morrone's proposal
  vec_hsubr <- vector(mode = "numeric", length = length(subreg))
  vec_csubr <- vector(mode = "numeric", length = length(subreg))
  for (i in seq_along(subreg)) {
    
    a_inter_subr <- CA_and_Reg %>%
      filter(Subregio_1 == subreg[i])  %>%
      st_union() %>%
      st_area()
    
    a_total_subr <- Reg %>%
      filter(Subregio_1 == subreg[i]) %>%
      st_union() %>%
      st_area()  
    
    Homo_subr <- round(a_inter_subr / st_area(ca_pol) * 100, digits = 4) 
    Comp_subr <- round(a_inter_subr / a_total_subr * 100, digits = 4) 
    
    vec_hsubr[i] <- Homo_subr 
    vec_csubr[i] <- Comp_subr 
    
  }
  subr_Comp_Hom_df <- cbind(Bioregion = as.character(subreg), Hb = vec_csubr, Hc = vec_hsubr)
  
  # Dominions in Morrone's proposal
  vec_hdomi <- vector(mode = "numeric", length = length(domini))
  vec_cdomi <- vector(mode = "numeric", length = length(domini))
  for (i in seq_along(domini)) {
    
    a_inter_domi <- CA_and_Reg %>%
      filter(Dominions == domini[i])  %>%
      st_union() %>%
      st_area()
    
    a_total_domi <- Reg %>%
      filter(Dominions == domini[i]) %>%
      st_union() %>%
      st_area()  
    
    Homo_domi <- round(a_inter_domi / st_area(ca_pol) * 100, digits = 4) 
    Comp_domi <- round(a_inter_domi / a_total_domi * 100, digits = 4) 
    
    vec_hdomi[i] <- Homo_domi
    vec_cdomi[i] <- Comp_domi
    
  }
  domi_Comp_Hom_df <- cbind(Bioregion = as.character(domini), Hb = vec_cdomi, Hc = vec_hdomi)
  
  # Provinces in Morrone's proposal
  vec_hprov <- vector(mode = "numeric", length = length(provin))
  vec_cprov <- vector(mode = "numeric", length = length(provin))
  for (i in seq_along(provin)) {
    
    a_inter_prov <- CA_and_Reg %>%
      filter(Province_1 == provin[i])  %>%
      st_union() %>%
      st_area()
    
    a_total_prov <- Reg %>%
      filter(Province_1 == provin[i]) %>%
      st_union() %>%
      st_area()  
    
    Homo_prov <- round(a_inter_prov / st_area(ca_pol) * 100, digits = 4) 
    Comp_prov <- round(a_inter_prov / a_total_prov * 100, digits = 4) 
    
    vec_hprov[i] <- Homo_prov
    vec_cprov[i] <- Comp_prov
    
  }
  prov_Comp_Hom_df <- cbind(Bioregion = as.character(provin), Hb = vec_cprov, Hc = vec_hprov)
  
  # Region in other biogeographical proposals: Gentry in this case
  vec_hgentry <- vector(mode = "numeric", length = length(gen_region))
  vec_cgentry <- vector(mode = "numeric", length = length(gen_region))
  for (i in seq_along(gen_region)) {
    
    a_inter_gen <- CA_and_Reg %>%
      filter(Region == gen_region[i])  %>%
      st_union() %>%
      st_area()
    
    a_total_gen <- Reg %>%
      filter(Region == gen_region[i]) %>%
      st_union() %>%
      st_area()  
    
    Homo_gentry <- round(a_inter_gen / st_area(ca_pol) * 100, digits = 4) 
    Comp_gentry <- round(a_inter_gen / a_total_gen * 100, digits = 4) 
    
    vec_hgentry[i] <- Homo_gentry
    vec_cgentry[i] <- Comp_gentry
    
  }
  gentry_Comp_Hom_df <- cbind(Bioregion = as.character(gen_region), Hb = vec_cgentry, Hc = vec_hgentry)
  
  # Creating a dataframe for all the comparisons
  Comp_Hom_df <- rbind(NReg_Comp_Hom_df, subr_Comp_Hom_df, domi_Comp_Hom_df, prov_Comp_Hom_df, gentry_Comp_Hom_df)
  Comp_Hom_df <- cbind(Consensus_area = y, Comp_Hom_df)
  
  # Some fields in Morrone's shapefile attribute table are in blank for the columns specified above.
  # This is so because some regions are not classified under less inclusive bioeographical areas such as dominions.
  Comp_Hom_df <- Comp_Hom_df[which(complete.cases(Comp_Hom_df)), ]
  
  return(Comp_Hom_df)
  
}

