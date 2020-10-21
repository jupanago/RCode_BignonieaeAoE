### ndm_visualization: Script with functions to visualize ndm outputs.

################ Function ndm_datamatrix_plot ################

## This function plot the ndm data matrices

## Arguments:
## 1) cm_shpfile: filepath of the shapefile to crop the ndm raster data matrix. Projection: wgs83
## 2) ndm_raster: raster file containing the a data matrix from NDM. This object is obtained after applying the function ndm_dataparsing().
## 3) ndm_title: plot title corresponding to the raster data matrix.
## 4) legend_title: type of values displayed in the raster.
## 5) save_plot = "NOT": "YES" to save the plot in the ./figs directory.

## Output: A graphical object from ggplot or a png image in ./figs directory.

ndm_datamatrix_plot <- function(cm_shpfile, ndm_raster, ndm_title, legend_title, save_plot = "NOT") {
  
  # Libraries
  library(raster)
  library(sf)
  library(rgdal)
  library(dplyr)
  library(ggplot2)
  library(ggthemes)
  
  # Shapefile
  amerland <- st_read(cm_shpfile)
  
  # Raster manipulation to produce a dataframe with the adequate formar for ggplot2
  ras_df <- ndm_raster %>%
    crop(amerland) %>%
    #mask(amerland) %>% # To mask the raster with the shapefile. Avoided because it does not represent well the data. 
    as("SpatialPixelsDataFrame") %>%
    as.data.frame()
  
  colnames(ras_df) <- c("value", "x", "y")
  
  # Getting the extend of the raster to adjust plot extent.
  ext <- extent(ndm_raster)
  
  # ggplot plot
  ras_plot <- ggplot() +
    geom_tile(data = ras_df, aes(x = x, y = y, fill = value), alpha = 0.8) +
    geom_sf(data = amerland, fill = NA, color = "black") +
    coord_sf(xlim = c(ext[1] + 5, ext[2] - 5), ylim = c(ext[3] + 5, ext[4] - 4)) +
    scale_fill_viridis_c() +
    theme_map() +
    theme(title = element_text(face = "bold", size = 10),
          legend.title = element_text(face = "plain")) +
    labs(title = ndm_title,
         fill = legend_title)
  
  if (save_plot == "YES") {
    
    png(paste0("./figs/", ndm_title, ".png"), width = 800, height = 800, res = 120)
    print(ras_plot)
    dev.off()
    
  } else {
    
    return(ras_plot)
    
  }
  
}




################ Function ndm_EIdumbell ################

## Funtion to draw a dumbbell diagram for consensus areas endemicity index range

## Arguments:

## 1) ca_df: consensus areas dataframe
## 2) ana: analysis
## 3) scl: spatial scale among 1, 2, or 3 degrees

ndm_EIdumbell <- function(ca_df, ana, scl) {
  
  library(dplyr)
  library(ggplot2)
  library(ggalt)
  
  ca_df[45:47, 12] <- 2.63839 # Adding a decimal point
  
  if (scl == "1") {
    
    ca_df$Area_name <- paste(ca_df$Analysis, ca_df$Spatial_Scale, ca_df$Consensus_Area, sep = "")
    ca_df$Area_name <- gsub(" ", "", ca_df$Area_name)
    
    IEdumbell <- ca_df %>%
      filter(Analysis == ana, Spatial_Scale == scl) %>%
      ggplot(aes(x = Cons_Area_Min_Score,
                 xend = Cons_Area_Max_Score,
                 y = reorder(Area_name, Individual_Areas))) +
      geom_dumbbell(color = "gray50",
                    size = 0.5,
                    size_x = 1,
                    size_xend = 1) +
      scale_x_continuous(breaks = seq(2, 22, 0.25)) +
      geom_text(aes(x = Cons_Area_Max_Score + 0.07, y = Area_name, label = paste(Individual_Areas, Number_Spp, sep = " | ")),
                size = 2.5) +
      theme_light() +
      theme(panel.grid.major.x=element_line(size = 0.05),
            panel.grid.major.y=element_blank(),
            axis.text.y = element_text(size = 8)) +
      labs(y = NULL,
           x = "Endemicity Index")
    
  } else {
    
    if (scl == 2) {
      
      ca_df$Area_name <- paste(ca_df$Analysis, ca_df$Spatial_Scale, ca_df$Consensus_Area, sep = "")
      ca_df$Area_name <- gsub(" ", "", ca_df$Area_name)
      
      IEdumbell <- ca_df %>%
        filter(Analysis == ana, Spatial_Scale == scl) %>%
        ggplot(aes(x = Cons_Area_Min_Score,
                   xend = Cons_Area_Max_Score,
                   y = reorder(Area_name, Individual_Areas))) +
        geom_dumbbell(color = "gray50",
                      size = 0.5,
                      size_x = 1,
                      size_xend = 1) +
        scale_x_continuous(breaks = seq(2, 22, 1)) +
        geom_text(aes(x = Cons_Area_Max_Score + 0.5, y = Area_name, label = paste(Individual_Areas, Number_Spp, sep = " | ")),
                  size = 2.5) +
        theme_light() +
        theme(panel.grid.major.x=element_line(size = 0.05),
              panel.grid.major.y=element_blank(),
              axis.text.y = element_text(size = 8)) +
        labs(y = NULL,
             x = "Endemicity Index")
      
    } else {
      
      ca_df$Area_name <- paste(ca_df$Analysis, ca_df$Spatial_Scale, ca_df$Consensus_Area, sep = "")
      ca_df$Area_name <- gsub(" ", "", ca_df$Area_name)
      
      IEdumbell <- ca_df %>%
        filter(Analysis == ana, Spatial_Scale == scl) %>%
        ggplot(aes(x = Cons_Area_Min_Score,
                   xend = Cons_Area_Max_Score,
                   y = reorder(Area_name, Individual_Areas))) +
        geom_dumbbell(color = "gray50",
                      size = 0.5,
                      size_x = 1,
                      size_xend = 1) +
        scale_x_continuous(breaks = seq(2, 22, 1)) +
        geom_text(aes(x = Cons_Area_Max_Score + 0.9, y = Area_name, label = paste(Individual_Areas, Number_Spp, sep = " | ")),
                  size = 2.5) +
        theme_light() +
        theme(panel.grid.major.x=element_line(size = 0.05),
              panel.grid.major.y=element_blank(),
              axis.text.y = element_text(size = 8)) +
        labs(y = NULL,
             x = "Endemicity Index")
      
    }
    
  }
  
  return(IEdumbell)
  
}

## The same but showing both analyses together at once.
ndm_IEdumbell2 <- function(x, spat_scale) {
  
  library(patchwork)
  
  d <- x %>%
    filter(Analysis == "D", Spatial_Scale == spat_scale) %>%
    mutate(Area_name = paste0(Analysis, Spatial_Scale, Consensus_Area)) %>%
    distinct(Area_name, .keep_all = TRUE)
  
  d$Area_name <- gsub(pattern = "\\s", replacement = "", x = d$Area_name)
  
  s <- x %>%
    filter(Analysis == "S", Spatial_Scale == spat_scale) %>%
    mutate(Area_name = paste0(Analysis, Spatial_Scale, Consensus_Area)) %>%
    distinct(Area_name, .keep_all = TRUE)
  
  s$Area_name <- gsub(pattern = "\\s", replacement = "", x = s$Area_name)
  
  compeffect_df <- rbind(d, s) %>%
    mutate(CA_range_diff = Cons_Area_Max_Score - Cons_Area_Min_Score,
           Spp_range_diff = Spp_Max_Score - Spp_Min_Score)
  
  if (spat_scale == 1) {textadjust <- 0.2}
  if (spat_scale == 2) {textadjust <- 0.8}
  if (spat_scale == 3) {textadjust <- 1}
  
  
  
  dumbellplot <- compeffect_df %>%
    ggplot(aes(x = Cons_Area_Min_Score,
               xend = Cons_Area_Max_Score,
               y = reorder(Area_name, Cons_Area_Max_Score))) +
    ggalt::geom_dumbbell(aes(color = Analysis),
                         size = 0.5,
                         size_x = 1,
                         size_xend = 1) +
    scale_x_continuous(breaks = seq(2, 22, 1)) +
    geom_text(aes(x = Cons_Area_Max_Score + textadjust, y = Area_name, label = paste(Individual_Areas, Number_Spp, sep = " | ")), # 1dg:0.2 2dg:0.8 3dg:1
              size = 2.5) +
    scale_color_viridis_d(option = "E", end = 0.7) +
    theme_bw() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          panel.grid.major.x=element_line(size = 0.05),
          panel.grid.major.y=element_blank(),
          axis.text.y = element_text(size = 8)) +
    labs(y = NULL,
         x = "Index of Endemicity",
         title = paste0("Consensus Areas at ", spat_scale, "°"))
  
  minIEplot <- compeffect_df %>%
    ggplot(aes(x = Analysis, y = Number_Spp, group = Analysis, fill = Analysis)) +
    geom_violin(fill = NA, aes(color = Analysis)) +
    geom_boxplot(width = 0.1, alpha = 0.5) +
    geom_jitter(size = 0.5, width = 0.2, aes(color = Analysis)) +
    scale_color_viridis_d(option = "E", end = 0.7) +
    scale_fill_viridis_d(option = "E", end = 0.7) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          legend.position = "none") +
    labs(y = "Min IE")
  
  maXIEplot <- compeffect_df %>%
    ggplot(aes(x = Analysis, y = Cons_Area_Max_Score, group = Analysis, fill = Analysis)) +
    geom_violin(fill = NA, aes(color = Analysis)) +
    geom_boxplot(width = 0.1, alpha = 0.5) +
    geom_jitter(size = 0.5, width = 0.2, aes(color = Analysis)) +
    scale_color_viridis_d(option = "E", end = 0.7) +
    scale_fill_viridis_d(option = "E", end = 0.7) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          legend.position = "none") +
    labs(y = "Max IE")
  
  rangeIEplot <- compeffect_df %>%
    ggplot(aes(x = Analysis, y = CA_range_diff, group = Analysis, fill = Analysis)) +
    geom_violin(fill = NA, aes(color = Analysis)) +
    geom_boxplot(width = 0.1, alpha = 0.5) +
    geom_jitter(size = 0.5, width = 0.2, aes(color = Analysis)) +
    scale_color_viridis_d(option = "E", end = 0.7) +
    scale_fill_viridis_d(option = "E", end = 0.7) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          legend.position = "none") +
    labs(y = "IE range")
  
  lay_jp <- c("AAAABB
            AAAACC
            AAAADD")
  
  dumbelAoEDS <- dumbellplot + maXIEplot + minIEplot + rangeIEplot + 
    plot_layout(design = lay_jp, guides = "collect") +
    plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
    theme(plot.tag = element_text(face = "bold"))
  
  return(dumbelAoEDS)
  
}

################ Function ndm_kpatern_plot ################

## This function produces a bar graph that counts the number of gradient and core like patterns per spatial scale and Areas interval. 
## Areal interval refers to categories that group consensus areas by the number of individual areas they represent. I defined three 
## categories: equal or less than two individual areas ("x < or = 2"), between two and 10 individual areas (2 < x < 10), and more 
## than 10 (x >10).

ndm_kpatern_plot <- function(df_cas, analysis) {
  
  library(dplyr)
  library(ggplot2)
  
  ndm_kindpatt <- function(x, iescore, p = 0.6) {
    
    coreindex <- x*p
    
    pattern <- ifelse(iescore >= coreindex, "Core", "Gradient") 
    
    return(pattern)
  }
  
  ca_df_pat <- df_cas %>%
    filter(Analysis == analysis) %>%
    dplyr::select(Spatial_Scale, Consensus_Area, Number_Spp, Individual_Areas, Cons_Area_Max_Score) %>%
    mutate(Kind_pattern = ndm_kindpatt(x = Number_Spp, iescore = Cons_Area_Max_Score, p = 0.66),
           Area_interval = ifelse(Individual_Areas <= 2, "< or = 2", 
                                  ifelse(Individual_Areas > 2 & Individual_Areas <= 10, "2 < x < 10", "> 10"))) %>%
    unique()
  
  kinds_patter_plot <- ca_df_pat %>%
    ggplot(aes(x = Kind_pattern, y = 1)) +
    facet_grid(Area_interval ~ Spatial_Scale) +
    geom_col(width = 0.5) +
    scale_y_continuous(breaks = seq(0, 20, 2)) +
    theme_light() + 
    theme(axis.text.x = element_text(size = 15, family = "Helvetica")) +
    labs(x = NULL,
         y = NULL,
         title = paste("Kinds of patterns: Heuristic search", analysis, sep = " "))
  
  return(kinds_patter_plot)
  
}

################ Function ndm_plotca ################

## This function drwas a map of the consensus area. Plotting the Morrone Regionalization is optional.

## Arguments:
## 1) shp.layers.dns: Directory where the shapefiles of the consensus areas are stored.
## 2) area.layer.name: name of the shapefile layer of the consensus area (without the .shp ending).
## 3) ca_Scal: Consensus area scale.
## 4) acolor: color to paint the consensus area.
## 5) alfa: transparency index for area color. [0-1]
## 6) Morrone_R: (Default Morrone_R = "NO") "YES" for drawing Morrone's regionalization of the Neotropical region.

ndm_plotca <- function(shp.layers.dns, area.layer.name, ca_Scale, acolor = "white", alfa = 0.1, Morrone_R = "NO") {
  
  require(rgdal)
  require(sp)
  require(sf)
  require(ggplot2)
  library(ggthemes)
  require(dplyr)
  
  # Loading layers
  
  wgs84.proj4 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  Amer <- readOGR(dsn = "./data/GIS/", layer = "Amer_land", p4s = wgs84.proj4)
  
  if (Morrone_R == "YES") {
    
    Morrone <- readOGR(dsn = "./data/GIS/", layer = "Lowenberg_Neto_2014", p4s = wgs84.proj4) # Morrone's 2014 Neotropical Region
    Morrone_df <- fortify(Morrone)
    
  }
  
  ca_grid <- readOGR(dsn = shp.layers.dns, layer = area.layer.name, p4s = wgs84.proj4)
  
  # Processing layers to plot with ggplot2
  
  Amer_df <- fortify(Amer)
  
  ca_grid <- st_as_sf(ca_grid)
  ca_pol <- st_union(ca_grid)
  ca_pol <- as_Spatial(ca_pol)
  ca_pol_df <- fortify(ca_pol)
  
  # Giving names to map
  
  if (grepl(area.layer.name, pattern = ".*1dgD.*")) {
    x <- gsub(area.layer.name, pattern = ".*1dgD_(.*)_.*-grid", replacement = "\\1")
    y <- paste("D1", x, sep = "")
    y
  }
  
  if (grepl(area.layer.name, pattern = ".*2dgD.*")) {
    x <- gsub(area.layer.name, pattern = ".*2dgD_(.*)_.*-grid", replacement = "\\1")
    y <- paste("D2", x, sep = "")
    y
  }
  
  if (grepl(area.layer.name, pattern = ".*3dgD.*")) {
    x <- gsub(area.layer.name, pattern = ".*3dgD_(.*)_.*-grid", replacement = "\\1")
    y <- paste("D3", x, sep = "")
    y
  }
  
  if (grepl(area.layer.name, pattern = ".*1dgT.*")) {
    x <- gsub(area.layer.name, pattern = ".*1dgT_(.*)_.*-grid", replacement = "\\1")
    y <- paste("T1", x, sep = "")
    y
  }
  
  if (grepl(area.layer.name, pattern = ".*2dgT.*")) {
    x <- gsub(area.layer.name, pattern = ".*2dgT_(.*)_.*-grid", replacement = "\\1")
    y <- paste("T2", x, sep = "")
    y
  }
  
  if (grepl(area.layer.name, pattern = ".*3dgT.*")) {
    x <- gsub(area.layer.name, pattern = ".*3dgT_(.*)_.*-grid", replacement = "\\1")
    y <- paste("T3", x, sep = "")
    y
  }
  
  if (grepl(area.layer.name, pattern = ".*1dgS.*")) {
    x <- gsub(area.layer.name, pattern = ".*1dgS_(.*)_.*-grid", replacement = "\\1")
    y <- paste("S1", x, sep = "")
    y
  }
  
  if (grepl(area.layer.name, pattern = ".*2dgS.*")) {
    x <- gsub(area.layer.name, pattern = ".*2dgD_(.*)_.*-grid", replacement = "\\1")
    y <- paste("D2", x, sep = "")
    y
  }
  
  if (grepl(area.layer.name, pattern = ".*3dgS.*")) {
    x <- gsub(area.layer.name, pattern = ".*3dgS_(.*)_.*-grid", replacement = "\\1")
    y <- paste("S3", x, sep = "")
    y
  }
  
  if (grepl(area.layer.name, pattern = ".*1dgH.*")) {
    x <- gsub(area.layer.name, pattern = ".*1dgH_(.*)_.*-grid", replacement = "\\1")
    y <- paste("H1", x, sep = "")
    y
  }
  
  if (grepl(area.layer.name, pattern = ".*2dgH.*")) {
    x <- gsub(area.layer.name, pattern = ".*2dgH_(.*)_.*-grid", replacement = "\\1")
    y <- paste("H2", x, sep = "")
    y
  }
  
  if (grepl(area.layer.name, pattern = ".*3dgH.*")) {
    x <- gsub(area.layer.name, pattern = ".*3dgH_(.*)_.*-grid", replacement = "\\1")
    y <- paste("H3", x, sep = "")
    y
  }
  
  ## ggplot2 base map graphical object
  
  if (Morrone_R == "YES") {
    
    CAbase_map <-  ggplot(Amer_df, aes(x = long, y = lat, group = group)) + 
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
      labs(x = "", y = "") +
      theme_linedraw() +
      theme(panel.grid = element_blank())
    
  } else {
    
    CAbase_map <-  ggplot(Amer_df, aes(x = long, y = lat, group = group)) + 
      geom_polygon(fill = "white", 
                   col = "black") +
      coord_cartesian(xlim = c(-111, -34),
                      ylim = c(-37, 39),
                      expand = TRUE) +
      labs(x = "", y = "") +
      theme_linedraw() +
      theme(panel.grid = element_blank())
    
  }
  
  ## Consensus area map
  
  Cons_Area_map <- CAbase_map +
    geom_polygon(data = ca_pol_df, 
                 aes(x = long, y = lat, group = group),
                 fill = alpha(acolor, alfa),
                 colour = "black",
                 size = 0.3) +
    # geom_polygon(data = point_ch_df, # convex hull for CA
    #              aes(x = long, y = lat, group = group),
    #              fill = alpha("red", 0.3),
    #              colour = "black",
    #              lwd = 0.3) +
    labs(title = y) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  return(Cons_Area_map)
  
}





################ Function ndm_compchanges ####

## This function draw the boxplots for the output of ndm_areasize showing changes
## in species composition and grid number per compararison pair of consensus areas.

## Arguments:
## 1) x = data frame from ndm_areasize

ndm_compchanges <- function(x) {
  
  library(ggrepel)
  
  x$new_pat <- factor(x$new_pat, levels = c("Subsets from D","Mixed subsets", "Identical", "Lost"), ordered = TRUE)
  
  effectspp_plot <- x %>%
    #filter(new_pat == "Subsets from D") %>%
    ggplot(aes(x = 1, y = Retained_spp, label = pair_comp)) +
    geom_boxplot() +
    geom_point(aes(colour = D), size = 1) +
    geom_text_repel(aes(colour = D, label = pair_comp), size = 2.5, box.padding = 0.55, point.padding = 0.5) +
    # geom_text_repel(aes(colour = D), size = 2.5, position = position_jitter(width = 1.5), seed = 123) +
    #scale_color_manual(values = x$jp_pal, aesthetics = c("colour")) +
    facet_wrap(~ new_pat) +
    scale_color_viridis_d(option = "E", end = 0.8) +
    scale_y_continuous(breaks = seq(0, 100, 5)) +
    theme_linedraw() +
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank()) +
    labs(y = "% spp retained from D to S")
  
  effectsize_plot <- x %>%
    #filter(new_pat == "Subsets from D") %>%
    ggplot(aes(x = 1, y = Size_difference, label = pair_comp)) +
    geom_boxplot() +
    geom_point(aes(colour = D), size = 1) +
    geom_text_repel(aes(colour = D, label = pair_comp), size = 2.5, box.padding = 0.55, point.padding = 0.5) +
    # geom_text_repel(aes(colour = D), size = 2.5, position = position_jitter(width = 2.5), seed = 123) +
    #scale_color_manual(values = x$jp_pal, aesthetics = c("colour")) +
    facet_wrap(~ new_pat) +
    scale_color_viridis_d(option = "E", end = 0.8) +
    theme_linedraw() +
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank()) +
    labs(y = "Absolute difference in grid cells #")
  
  
  boxplots_compchange <- list(retained_spp = effectspp_plot, grid_differece =  effectsize_plot)
  
  return(boxplots_compchange)
  
}

################ Function ndm_sumvis ####

## This function creates a bar diagram for the summary of the results produced 
## by ndm_casumm.

## Arguments:
## 1) sum_df = data frame from ndm_casumm

ndm_sumvis <- function(sum_df) {
  
  sum_df$Analysis <- factor(Summ_info$Analysis, levels = c("D","T","S", "H"), labels = c("D","T","S", "H"))
  
  ## Number of species defining areas per analysis
  Spp_areas <-sum_df %>%
    ggplot(aes(x = Analysis, y = Spp)) +
    facet_wrap(~ Spatial_Scale) +
    geom_col() +
    labs(title = "Number of scoring species per analysis") +
    theme(plot.title = element_text(family = "Helvetica", face = "bold", size = 10))
  
  ## Number of individual areas per analysis
  Ind_Ar <-sum_df %>%
    ggplot(aes(x = Analysis, y = Indiv_Areas)) +
    facet_wrap(~Spatial_Scale) +
    geom_col() +
    labs(title = "Number of individual areas per analysis") +
    theme(plot.title = element_text(family = "Helvetica", face = "bold", size = 10))
  
  ## Number of Consensus Areas per analysis
  Con_Ar <-sum_df %>%
    ggplot(aes(x = Analysis, y = Cons_Areas)) +
    facet_wrap(~Spatial_Scale) +
    geom_col() +
    labs(title = "Number of Consensus Areas per analysis") +
    theme(plot.title = element_text(family = "Helvetica", face = "bold", size = 10))
  
  plot1 <- ggarrange(Spp_areas, Ind_Ar, Con_Ar, labels = c("(a)", "(b)", "(c)"), ncol = 3, nrow = 1)
  
  return(plot1)
  
}


################ Function ndm_ambvis ####

## This function creates a bar diagram for the summary of the ambiguity in the
## results.

## Arguments:
## 1) sum_df = data frame from ndm_casumm

ndm_ambvis <- function(amb_df) {
  
  amb_df$Analysis <- factor(amb_df$Analysis, levels = c("D","S"), labels = c("D","S"))
  
  CA_conflicts <- amb_df %>%
    ggplot(aes(x = Spatial_Scale, y = n_Conflicts)) +
    facet_wrap(~ Analysis) +
    geom_col() +
    labs(title = "Ambiguity",
         y = "Cases") +
    theme(plot.title = element_text(family = "Helvetica", face = "bold", size = 10))
  
  CA_in_conflicts <- amb_df %>%
    ggplot(aes(x = Spatial_Scale, y = cas_in_conf)) +
    facet_wrap(~ Analysis) +
    geom_col() +
    labs(title = "Cons. Areas with ambiguity",
         y = "Cons. Areas") +
    theme(plot.title = element_text(family = "Helvetica", face = "bold", size = 10))
  
  CA_cleanpat <- amb_df %>%
    ggplot(aes(x = Spatial_Scale, y = cas_out_conf)) +
    facet_wrap(~ Analysis) +
    geom_col() +
    labs(title = "Cons. areas without ambiguity",
         y = "Consensus Areas") +
    theme(plot.title = element_text(family = "Helvetica", face = "bold", size = 10))
  
  Spp_in_conflicts <- amb_df %>%
    ggplot(aes(x = Spatial_Scale, y = spp_in_conf)) +
    facet_wrap(~ Analysis) +
    geom_col() +
    labs(title = "Ambiguous species",
         y = "Spp") +
    theme(plot.title = element_text(family = "Helvetica", face = "bold", size = 10))
  
  (plot5 <- ggarrange(CA_conflicts, CA_in_conflicts, CA_cleanpat, Spp_in_conflicts,
                      labels = c("(a)", "(b)", "(c)", "(d"),
                      ncol = 2, nrow = 2))
  
  
}

