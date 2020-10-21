
### Processing VNDM-NDM outputs

## Tasks will be divided into: 
## I) Producing VNDM-NDM input files
## II) Importing VNDM-NDM outputs into R
## III) A posteriori exploration of consensus areas
## IV) Drawing maps

## Scripts are organized in separate files as follows:
## 1) Functions: Source files with the functions design for different tasks.
## 2) Workflow (This script): General pipeline of the analysis.
## 3) Additional scripts detailing the exploration of the consensus areas.

## This R project is organized in the following system of directories:
## ./Working_directory/ -> Working directory
##      data/           -> Data files
##      doc/            -> Working manuscripts and markdown files
##      output/         -> Outputs of the analysis
##      figs/           -> Figures
##      R/              -> R Scripts
##
## The content from ./figs and ./output can completely reproduced from ./R.

## VNDM-NDM outputs nomenclature.
## Output files, figures, and Consensus areas in GIS format were saved using the 
## following nomenclature.This nomenclature were designed to trace the VNDM-NDM
## outputs to their respective analyses, spatial scale, and their approximated 
## geographical location. In order to reproduce the data manipulation below the 
## nomenclature must be strictly followed.

## Area name code: 
## It considers the spatial scale of analysis or grid size, the NDM code of the 
## consensus area, relative geographical location, and type of GIS file
## (i.e grids or points) produced using VNDM-NDM save options. 

## Example: 1dgD_CA0_AF-grid 
##    1dg = grid size; 
##    D = Default analysis; 
##    CA0 = Consensus area number 0;
##    AF = A name for the geographical region where the area is located.
##    grid/point = ndm grid output file type.
##
## General regular expression = "^.*grid.txt" 

# RStudio Observation: Press "Alt + O" to collapse all the R code sections to
# have and overlook of all the steps to process VNDM-NDM data.

# 0. Loading the functions to explore the VNDM-NDM results and R libraries ####

source("./R/NDM_inputfiles.R")
source("./R/NDM_intoR.R")
source("./R/NDM_visualization.R")
source("./R/NDM_tables.R")
source("./R/NDM_acomparison.R")

# Libraries

## The following libraries are used in this R project. 
## Please install them first before executing the code below.

# install.packages(c("readxl", "tidyverse", "ggpubr", "gtable", "ggthemes", 
#                    "ggalt", "viridis",  "raster", "rgdal", "sp", "sf", 
#                    "rmapshaper", "UpSetR", "VennDiagram", "grid", "gridExtra",
#                    "patchwork", "ggrepel", "lwgeom"))

library(tidyverse)
library(readxl)
library(ggpubr)
library(raster)

#------------------------- I) IMPORTING DATA INTO VNDM-NDM ------------------------- 
# 1. Select working directory or R project ####

# setwd("./foo") ## This is not necessary if you are working inside the RStudio project

# 2. Importing database into R ####

Total_DB <- read_excel("./data/Bignonieae_Database_sample.xlsx") 
# NOTE_0: This is a sample with the genera Pachyptera and Tanaecium. My database 
# had the following columns: 
# NAME1: for species names.
# XCOOR: latitude coordinate.
# YCOOR: longitude coordinate.
# All the functions used in this script requires that these column names 
# are used. 

# 3. Selecting unique records ####

Unique_DB <- Total_DB %>% distinct(NAME1, YCOOR, XCOOR, .keep_all = TRUE)

# Filling decimals to standardize up to 6 decimal digits.

Unique_DB$XCOOR <- format(round(Unique_DB$XCOOR, 6), nsmall = 6)
Unique_DB$YCOOR <- format(round(Unique_DB$YCOOR, 6), nsmall = 6)

# 4. Creating a list of coordinates in the .xyd format ####

## The function ndm_xyd() creates two files: NDM_DB.xyd, which contains species 
## occurrences in the format .xyd; and the file Spnames_list.txt which contains 
## the list of species and their respective NDM code.
## The original files of NDM_DB.xyd and Spnames_List.txt can be found in ./data
ndm_xyd(Unique_DB)

# 5. Loading the list of species and NDM code into a data.frame ####

spplist <- ndm_spplist("./data/Spnames_list.txt")

#------------------------- II) IMPORTING VNDM-NDM UTPUTS INTO R ------------------------- 
# 1. Creating polygons from NDM grid output file at each spatial scale ####
# 1.1. Creating list of files and consensus areas names ####

# NOTE_1: The nomenclature of areas were specifically designed to trace back the
## analyses done and the approximated geographical location.

ndm_listgrid.files(path = "./data/GIS/40perc/", degree = 1)
ndm_listgrid.files(path = "./data/GIS/40perc/", degree = 2)
ndm_listgrid.files(path = "./data/GIS/40perc/", degree = 3)

# 1.2 Creating polygons from grid file in batch ####

## The function ndm_gridtopoly() creates the polygon shapefiles of consensus 
## areas from the grid coordinates ouput of NDM, and saves them in 
## "./output/GIS/40perc/". A copy of the ouput of this function can be found in
## "./data/GIS/Consensus_Areas_Shapefiles/".

pmap(list(file = files1dg,
          degree = 1,
          save.area.shp = TRUE,
          save.directory = "./output/GIS/40perc/",
          area.shp.name = anames1dg), ndm_gridtopoly)

pmap(list(file = files2dg,
          degree = 2,
          save.area.shp = TRUE,
          save.directory = "./output/GIS/40perc/",
          area.shp.name = anames2dg), ndm_gridtopoly)

pmap(list(file = files3dg,
          degree = 3,
          save.area.shp = TRUE,
          save.directory = "./output/GIS/40perc/",
          area.shp.name = anames3dg), ndm_gridtopoly)


# 2. Consensus areas data frames and NDM data matrices from NDM output file ####

# NOTE_2: I was unable to extract automatically the values for the range of IE 
# in consensus areas. Those vales were extracted manually from the images of the  
# consensus areas. The final data frame is stored in the file: 
# "./data/ndm_outputs/CA_Loose40_data.xlsx"

# NOTE_3: Visualization for Default analysis is provided. To visualize the Strict
# analysis change the parameter 'ana_param' from "D" to "S" in the function 
# ndm_datamatrix_plot().

# 2.1. Default ####

# ndm_dataparsing function: Parsing the output text file into R
# ndmfile = c("./1dgD_total.out", "./2dgD_total.out", "./3dgD_total.out")
# grid_size = c(1, 2, 3)
e_ndm <- raster::extent(c(-111, -31, -40, 40))

df_2D <- ndm_dataparsing(ndmfile = "./data/ndm-outputs/Loose40/2dgD_total.out", 
                         grid_size = 2, ana_param = "D", 
                         create_matrix = TRUE, matrix_extent = e_ndm) 

# Visualization of matrices at 2 degrees

ndm_datamatrix_plot(cm_shpfile = "./data/GIS/Amer_land.shp", 
                    ndm_raster = Records_cell, ndm_title = "Sampling", 
                    legend_title = "Records")
ndm_datamatrix_plot(cm_shpfile = "./data/GIS/Amer_land.shp", 
                    ndm_raster = Species_cell, ndm_title = "Richness", 
                    legend_title = "Species")
ndm_datamatrix_plot(cm_shpfile = "./data/GIS/Amer_land.shp", 
                    ndm_raster = Assumed_cell, ndm_title = "Filling effect", 
                    legend_title = "Assumed\nspecies")

# 2.3. Strict ####

# The same as the Default analysis.
df_2S <- ndm_dataparsing(ndmfile = "./data/ndm-outputs/Loose40/2dgS_total.out",
                         grid_size = 2, ana_param = "S", create_matrix = TRUE, 
                         matrix_extent = e_ndm) 


#------------------------- III) EXPLORING CONSENSUS AREAS A POSTERIORI -------------------------
# 1. Summarizing results ####

# Loading consensus areas information data frame. This dataframe was built from
# both the results obtained above and the manual extraction of EI score range
# from consensus areas images.
ca_df <- read_excel("./data/ndm-outputs/Loose40/CA_Loose40_dataDS.xlsx")

spp_list <- ndm_spplist(filepath = "./output/Spnames_list.txt")

# 1.1. Summary of consensus areas information ####
(Summ_info <- ndm_casum(ca_df))

# 1.2. Looking at results: Consensus areas per analysis per scale ####

plot1 <- ndm_sumvis(Summ_info)
# Saving plot 1
png(filename = "./figs/plot1.png", width = 1200, height = 500, res = 120)
print(plot1)
dev.off()

# 1.3. Visualizing IE scores ####

## NOTE_4: A Dumbell chart facilitates the comparison of IE scores ranges of 
## consensus areas.
## In the figures below, the black numbers stand for the number of Individual 
## areas (left) and endemic species (right) per consensus area.

## a) Comparison of the IE range values of consensus areas from D ans S analyses.
## The numbers next to the line are the number of individual areas (left) and
## the number of scoring species (right) in the consensus area.
## b) Distribution of maximum IE values across consensus areas.
## c) Distribution of minimum IE values across consensus areas.
## d) The range of IE values across consensus areas.

## To see the results from different scales:
## spat_scale = c(1, 2, 3)

ndm_IEdumbell2(x = ca_df, spat_scale = 2)

# Saving plot 2
png(filename = "./figs/plot2.png", width = 1200, height = 1000, res = 150)
print(ndm_IEdumbell2(x = ca_df, spat_scale = 2))
dev.off()

# 1.5. List of scoring species with irregular behavior through scales ####

# Changing the grid size to account for spatial scale might create inconsistencies
# depending on the species species distribution is represented. Occurrence points
# might lay closer or nearer of the grids. This may cause that some species
# appear and reappear at intermediate spatial scales. Something that can
# happen too when the species distribution is poorly sampled. Interactions of 
# poor sampling and fill radii in VNDM can create this inconsistencies. 

ndm_spplost(ca_df, analysis = "D")
ndm_spplost(ca_df, analysis = "S")

# 1.6. List of species that did not score for any pattern across scales ####
ndm_sppout(x = ca_df, analysis = "D")
ndm_sppout(x = ca_df, analysis = "S")

# 2. Analyzing the effect of the analyses on grid cells and species numbers ####

# Loading data
ca_df <- read_excel("./data/ndm-outputs/Loose40/CA_Loose40_dataDS.xlsx")

# 2.1. Counting cells per consensus area ####

# Analyses at 1 degree 
D1 <- ndm_cellnumb(df_cas = ca_df, dg = "1", ana = "D", 
                   shp.dns = "./data/GIS/Consensus_Areas_Shapefiles/") %>% 
  ndm_Aeffect()
S1 <- ndm_cellnumb(df_cas = ca_df, dg = "1", ana = "S", 
                   shp.dns = "./data/GIS/Consensus_Areas_Shapefiles/") %>% 
  ndm_Aeffect()

# Analyses at 2 degrees
D2 <- ndm_cellnumb(df_cas = ca_df, dg = "2", ana = "D",
                   shp.dns = "./data/GIS/Consensus_Areas_Shapefiles/") %>%
  ndm_Aeffect()
S2 <- ndm_cellnumb(df_cas = ca_df, dg = "2", ana = "S", 
                   shp.dns = "./data/GIS/Consensus_Areas_Shapefiles/") %>% 
  ndm_Aeffect()

# Analyses at 3 degrees
D3 <- ndm_cellnumb(df_cas = ca_df, dg = "3", ana = "D",
                   shp.dns = "./data/GIS/Consensus_Areas_Shapefiles/") %>% 
  ndm_Aeffect()
S3 <- ndm_cellnumb(df_cas = ca_df, dg = "3", ana = "S", 
                   shp.dns = "./data/GIS/Consensus_Areas_Shapefiles/") %>%
  ndm_Aeffect()

# 2.2. Differences in size and species number among consensus areas ####

## NOTE_7: The comparison is done using only consensus areas located over the same
## geographical region and with similar species composition (equivalent areas).
## A function to automatically cluster areas by approximate geographical region
## is under development. Therefore, the equivalent consensus areas among
## analyses were chosen by visual inspection and saved in: 
## "./data/Compacross_DS.xlsx"

## NOTE_8: Each row of this data frame contains an individual comparison between
## a consensus area in the Default analysis and its equivalent area in the
## Strict analysis. The columns describe the specific Analysis, and the effect 
## on species number and size of the consensus areas. 

# Species composition changes
# Change the sheet argument to access the analyses at different scales:
# sheet = c("1dg", "2dg", "3dg")
compana_df <- read_excel("./data/Compacross_DS.xlsx", sheet = "2dg")

purrr::map(.x = seq_along(compana_df$D), .f = ndm_comptestDS, df_cas = ca_df, 
           equival_CA_df = compana_df) %>% 
  do.call(what = "rbind") %>%
  head(n = 10)

# Grid size changes

compositionDS <- purrr::map(.x = seq_along(compana_df$D), .f = ndm_comptestDS, 
                            df_cas = ca_df, equival_CA_df = compana_df) %>% 
  do.call(what = "rbind") 

# Remember to change the Ddf and Sdf arguments accordingly:
# Ddf = c(D1, D2, D3)
# Sdf = c(S1, S2, S3). 
# Use always the same spatial scale in both arguments (D1 and S1).
effect_df <- ndm_areasize(spp_compDS = compositionDS, Ddf = D2, Sdf = S2) 

# Plots 
changes_boxplot <- ndm_compchanges(x = effect_df)
changes_boxplot[1] 
changes_boxplot[2]

# Saving plot 3:

png(filename = "./figs/plot3_1.png", width = 800, height = 800, res = 120)
print(changes_boxplot[1])
dev.off()

png(filename = "./figs/plot3_2.png", width = 800, height = 800, res = 120)
print(changes_boxplot[1])
dev.off()


# 3. Analyzing ambiguity between consensus areas at the Default search. ####

## NOTE_9: There is a ambiguity any time two or more consensus areas shared one 
## or more scoring species. For a detailed script to check the ambiguous species 
## in the Default and Strict analysis please refer to .R/Steps_AmbiguousSpp.R 

## NOTE_10 Ambiguous species can be identified using the intersection set 
## operator. However, using a Venn Diagram to visualize ambiguity is not the best 
## option because they can be difficult to read if more than 3 consensus areas
## shared species. The package UpsetR allows the visualization of complex
## Venn Diagrams UpSetR (Gehlenborg, 2019). It depicts at once all the possible
## cases of ambiguity between consensus areas and gives information about the 
## size of consensus areas, the number of areas per intersection, and 
## the number of shared scoring species (i.e. a ambiguous species.) The 
## information gathered from UpsetR diagrams and the .R/NEWMAN_AmbiguousSpp.R
## script were summarized and saved in: 
## "./data/ndm-outputs/Loose40/Summ_conflictsDS.csv"

# 3.1. Visualizing ambiguity in consensus areas ####

library(UpSetR)

# Loading data
ca_df <- read_excel("./data/ndm-outputs/Loose40/CA_Loose40_dataDS.xlsx")

# 3.1. UpSetR diagram ####

# 2 degrees
# To see the other scales: 
# scl = c(1, 2, 3)
# Only available for the Default Analysis. The Strict Analysis will be 
# implemented later into de ndm_upset function.

(plot4 <- ndm_upset(df_cas = ca_df, ana = "D", scl = 2))

# Saving plot 4
png(filename = "./figs/plot4.png", width = 800, height = 1100, res = 120)
print(plot4)
dev.off()


# 3.2. Summarizing ambiguity information ####

# Loading dataframe

(Summ_ambiguity <- read.csv("./data/ndm-outputs/Loose40/Summ_conflictsDS.csv"))

# Summary figure
plot5 <- ndm_ambvis(Summ_ambiguity)
# Saving plot 5
png(filename = "./figs/plot5.png", width = 1000, height = 1000, res = 130)
print(plot5)
dev.off()

# 3.3. Exploring ambiguity ####

## NOTE_11: A plot to visualize the elevation profile of the species from 
## consensus areas with ambiguous species is produced here. This plot allows
## the simultaneous visualization of map of the consensus areas with ambiguity, 
## an elevation boxplot for all their endemic species, and a table showing the
## range of IE scores for the consensus areas and the ambiguous species. This 
## function wraps different functions to plot each one of these components. 

## NOTE_12: The idea behind this kind of figure is to have the opportunity to 
## visually identify preferences in the altitudinal belt of the endemic species
## and see whether some cases of ambiguity occur between areas occurring 
## preferentially at specific altitudinal belts. The same idea can be extended 
## to traits to look for commonalities among endemic species. A function for this
## is under development.

# Loading data
## NOTE_13: The names of species with compound epithets were not parsed correctly 
## by the ndm_dataparsing function. I am correcting them here.
## This corrections were not necessary in the analyses above. 

ca_df <- read_excel("./data/ndm-outputs/Loose40/CA_Loose40_dataDS.xlsx")
ca_df <- ca_df %>% mutate(
  Species = replace(Species, Species == "Dolichandra unguis",
                    "Dolichandra unguis-cati"),
  Species = replace(Species, Species == "Bignonia sanctae", 
                    "Bignonia sanctae-crucis")) %>%
  as.data.frame()

# Elevation data
data_elev <- read.csv("./data/Elevation_Bignonieae.csv")

# Additional data relevant to produce exploratory plot
{
  # Shapefile layer names separated by cases of ambiguity
  layer_names <- list(
    conf1 = c("1dgD_CA2_AF-grid", "1dgD_CA3_AF-grid"),
    conf2 = c("1dgD_CA0_AF-grid", "1dgD_CA5_AF-grid", "1dgD_CA8_AF-grid"),
    conf3 = c("2dgD_CA0_AF-grid", "2dgD_CA5_DD-grid", "2dgD_CA19_DD-grid"),
    conf4 = c("2dgD_CA0_AF-grid", "2dgD_CA5_DD-grid"),
    conf5 = c("2dgD_CA0_AF-grid", "2dgD_CA19_DD-grid"),
    conf6 = c("2dgD_CA5_DD-grid", "2dgD_CA19_DD-grid"),
    conf7 = c("2dgD_CA0_AF-grid", "2dgD_CA5_DD-grid", "2dgD_CA8_AF-grid"),
    conf8 = c("2dgD_CA0_AF-grid", "2dgD_CA8_AF-grid"),
    conf9 = c("2dgD_CA0_AF-grid", "2dgD_CA1_AF-grid"),
    conf10 = c("2dgD_CA0_AF-grid", "2dgD_CA2_DD-grid"),
    conf11 = c("2dgD_CA0_AF-grid", "2dgD_CA6_AFDD-grid"),
    conf12 = c("2dgD_CA0_AF-grid", "2dgD_CA9_AF-grid"),
    conf13 = c("2dgD_CA0_AF-grid", "2dgD_CA13_AF-grid"),
    conf14 = c("2dgD_CA0_AF-grid", "2dgD_CA15_AF-grid"),
    conf15 = c("2dgD_CA13_AF-grid", "2dgD_CA21_AF-grid"),
    conf16 = c("3dgD_CA2_THDD-grid", "3dgD_CA5_SESouth-grid", "3dgD_CA10_AF-grid", "3dgD_CA19_AF-grid", "3dgD_CA23_AF-grid"),
    conf17 = c("3dgD_CA2_THDD-grid", "3dgD_CA19_AF-grid", "3dgD_CA23_AF-grid"),
    conf18 = c("3dgD_CA5_SESouth-grid", "3dgD_CA10_AF-grid"),
    conf19 = c("3dgD_CA5_SESouth-grid", "3dgD_CA19_AF-grid"),
    conf20 = c("3dgD_CA5_SESouth-grid", "3dgD_CA23_AF-grid"),
    conf21 = c("3dgD_CA5_SESouth-grid", "3dgD_CA16_DD-grid"),
    conf22 = c("3dgD_CA5_SESouth-grid", "3dgD_CA4_DD-grid"),
    conf23 = c("2dgD_CA7_AM-grid", "2dgD_CA18_AM-grid"),
    conf24 = c("3dgD_CA3_AM-grid", "3dgD_CA0_AM-grid", "3dgD_CA12_AM-grid"),
    conf25 = c("3dgD_CA3_AM-grid", "3dgD_CA0_AM-grid"),
    conf26 = c("3dgD_CA3_AM-grid", "3dgD_CA12_AM-grid"),
    conf27 = c("3dgD_CA3_AM-grid", "3dgD_CA14_AM-grid", "3dgD_CA15_AM-grid"),
    conf28 = c("3dgD_CA3_AM-grid", "3dgD_CA14_AM-grid"),
    conf29 = c("3dgD_CA3_AM-grid", "3dgD_CA15_AM-grid"),
    conf30  = c("3dgD_CA14_AM-grid", "3dgD_CA15_AM-grid"),
    conf31 = c("3dgD_CA14_AM-grid", "3dgD_CA24_AM-grid"),
    conf32 = c("3dgD_CA0_AM-grid", "3dgD_CA17_AM-grid"),
    conf33 = c("3dgD_CA3_AM-grid", "3dgD_CA9_AM-grid"),
    conf34 = c("3dgD_CA7_AM-grid", "3dgD_CA20_AM-grid"),
    conf35 = c("3dgD_CA7_AM-grid", "3dgD_CA32_AM-grid"),
    conf36 = c("3dgD_CA28_AM-grid", "3dgD_CA32_AM-grid"),
    conf37 = c("3dgD_CA17_AM-grid", "3dgD_CA22_Sam-grid"),
    conf38 = c("3dgD_CA17_AM-grid", "3dgD_CA14_AM-grid"),
    conf39 = c("3dgD_CA17_AM-grid", "3dgD_CA26_AM-grid"),
    conf40 = c("3dgD_CA6_AM-grid", "3dgD_CA11_AM-grid", "3dgD_CA30_AM-grid"),
    conf41 = c("3dgD_CA6_AM-grid", "3dgD_CA11_AM-grid"),
    conf42 = c("3dgD_CA6_AM-grid", "3dgD_CA30_AM-grid"),
    conf43 = c("3dgD_CA11_AM-grid", "3dgD_CA14_AM-grid"),
    conf44 = c("3dgD_CA11_AM-grid", "3dgD_CA3_AM-grid"),
    conf45 = c("3dgD_CA11_AM-grid", "3dgD_CA29_NWVen-grid"),
    conf46 = c("3dgD_CA8_NWPac-grid", "3dgD_CA13_NWCol-grid"),
    conf47 = c("3dgD_CA8_NWPac-grid", "3dgD_CA29_NWVen-grid"), 
    conf48 = c("3dgD_CA25_NWMeso-grid", "3dgD_CA33_NWMeso3-grid"),
    conf49 = c("3dgD_CA25_NWMeso-grid", "3dgD_CA31_NWMeso2-grid"),
    conf50 = c("3dgD_CA33_NWMeso3-grid", "3dgD_CA31_NWMeso2-grid"))
  
  # Areas names
  ca_names <- mapply(gsub, ".*CA([0-9]+).*", "CA \\1", layer_names)
  
  # Scale of cases of ambiguity
  con_scale <- c(rep(x = c(1, 2, 3), times = c(2, 13, 35)))
  con_scale[c(23, 37:39)] <- 2
}  

# Use this index to plot any of the 50 cases of ambiguity across different scales
# The cases in which ambiguous species occupied preferentially an altitudinal 
# belt are: c(2, 3, 4, 5, 6, 9, 10, 27, 48, 49)
i <- 3 # case 3 out of 50

(plot6 <- ndm_visconf(shp_dns = "./data/GIS/Consensus_Areas_Shapefiles/", 
            layer_names = layer_names[[i]], 
            alfa = 0.5, 
            plot_title = "plot title", 
            ca_df = ca_df %>% filter(Analysis %in% c("D")), 
            elevat_df = data_elev, 
            con_scale = con_scale[i], 
            ca_names = ca_names[[i]]) # Zoom the image in RStudio Plot window 
)

# Only available for the Default Analysis. The Strict Analysis will be 
# implemented later into de ndm_visconf function.


# Saving plot 6
png(filename = "./figs/plot6.png", width = 1500, height = 600, res = 120)
grid::grid.draw(plot6)
dev.off()

# 4. Comparing consensus areas against previous biogeographical regionalizations ####

## NOTE_14: Generally, areas of endemism are compared to the biogeographical  
## units of regionalization schemes by visual inspection of maps. The idea here
## is to do it quantitatively. This was inspired in the mapcurves method 
## implemented in the sabre R package to compare categorical maps (Nowosad and 
## Stepinski, 2018). However, our taks here is simpler as it implies to compare
## an individual consensus area against a biogeographical unit instead of complete
## regionalizations. The main diference is that mapcurves requires that the 
## spatial objects under comparison have the same extension, a condition 
## difficult to meet here. Therefore, I created the function ndm_contrast() 
## to adapt the general logic of the comparison made in mapcurves to individual
## areas and complete regionalizations with different extents. The idea is to 
## calculate how much of the consensus area is covered by the different
## biogeographical units in a regionalisation scheme, and conversely how much of 
## the biogeographical unit is covered by the consensus area. When two polygons
## are compared their different shapes make that the proportion of the area of 
## intersection between them be different for each one of them. So, considering 
## these proportional areas of intersection can give us an idea of how well the 
## two polygons fit into each other. Nowosad and Stepinski (2018) did this using
## more sophisticated methods and named these measures as Completeness and 
## Homogeneity. To develop the function ndm_contrast() I used those same terms, 
## but for the writing of the manuscript of Patterns of Endemism in Bignonieae I
## used the terms Uniformity of Consensus Area and Uniformity of the 
## Biogeographical unit to avoid unjustified associations with their method. 
## Therefore, the dataframe will have the columns Completeness and Homogeneity 
## that stand for Uniformity of the Biogeographical unit (Ub in manuscript) and 
## Uniformity of Consensus area (Uc in manuscript), respectively. To represent 
## the spatial congruence (Sc in manuscript) between the Consensus area and the 
## biogeographical unit I took the average of Uc and Ub. 

## NOTE_16: The ndm_contrast() function requires to use a shapefile for the 
## biogeographical regionalisation, and the shapefile of the consensus area. 

# 4.1. Calculating the Uniformity of consensus areas and biogeographical units ####

# Consensus areas directory
conA_dsn <- "./data/GIS/Consensus_Areas_Shapefiles/"

# Consensus areas layer names for the Default analysis
# (72 consensus areas at 3 spatial scales)
ca_layer_names <- list.files(path = "./data/GIS/Consensus_Areas_Shapefiles/", 
                             pattern = "[1-3]dgD.*grid.shp") %>%
  gsub(pattern = "([1-3]dgD.*grid).shp", replacement = "\\1")

# Morrone Regionalization (Lowenberg-Neto, 2014)
Morrone <- "./data/GIS/Lowenberg_Neto_2014.shp"

# Gentry Regionalization (JPNG)
Gentry <- "./data/GIS/Gentry_Phytogeography.shp"

# Index to identify the consensus area to be compared (Change it!)
i <- 11 # 1 out of 72

# Example with Morrone
(ex_Morrone <- ndm_contrast(ca_dsn = conA_dsn, ca_layer = ca_layer_names[i], reg_file = Morrone))

# Example with Gentry
(ex_Gentry <- ndm_contrast(ca_dsn = conA_dsn, ca_layer = ca_layer_names[i], reg_file = Gentry))

# 4.2. Visualizing Uniformity and Spatial congruence ####
# 4.2.1. Calculating spatial congruence. ####

# Prepearing dataframes: Morrone
# Loading the dataframe produced by ndm_contrast()

MOR_CONTRAST <- read.csv("./data/CONTRAST_MORR.csv", 
                         stringsAsFactors = FALSE) %>%
  mutate(Scale = ifelse(grepl("D1.*", Consensus_area), 1, 
                        ifelse(grepl("D2.*", Consensus_area), 2, 3)),
         Hierarchy = ifelse(grepl(".*province", Bioregion), "Province", 
                            ifelse(grepl(".*subregion", Bioregion), 
                                   "Subregion", "Dominion"))) %>% 
  group_by(Consensus_area) %>%
  mutate(Intersections = n(),
         Congruence = (as.numeric(Hb) + as.numeric(Hc)) / 2) %>%
  group_by(Consensus_area, Hierarchy) %>%
  mutate(Intersect_by = n())

head(MOR_CONTRAST)

# Prepearing dataframes: Gentry
GEN_CONTRAST <- read.csv("./data/CONTRAST_GEN.csv", 
                         stringsAsFactors = FALSE) %>%
  mutate(Scale = ifelse(grepl("D1.*", Consensus_area), 1, 
                        ifelse(grepl("D2.*", Consensus_area), 2, 3)),
         Hierarchy = "Phytogeographical region") %>% 
  group_by(Consensus_area) %>%
  mutate(Intersections = n(),
         Congruence = (Hb + Hc)/2)

head(GEN_CONTRAST)

# 4.2.2. Heatmaps of uniformity ####

## NOTE_17: Using heatmaps for Uniformity facilitate the illustration of how 
## congruent consensus areas and biogeographical units are.
## The heatmaps below show the uniformity of the Biogeographical region 
## because it is more intuitive and easy to interpret.
## These heatmaps are divided by spatial scale. Consensus areas are in the 
## y-axis and biogeographical units in the x-axis.
## The consensus areas are ordered from the bottom to the top in ascending order
## by the number of biogeographical units they intercepted.

# Gentry Heatmap of Uniformity of Biogeographical Region
{
  # Factors for ploting
  fact_new_interm <- GEN_CONTRAST[order(GEN_CONTRAST$Intersections), ]
  
  GEN_CONTRAST$Consensus_area <- factor(GEN_CONTRAST$Consensus_area, 
                                        levels = unique(fact_new_interm$Consensus_area))
  
  
  GEN_CONTRAST$Bioregion <- 
    factor(GEN_CONTRAST$Bioregion, 
        levels = c("West Indies", "Mexico and Central America","Northern Andes", 
                      "Southern Andes", "Northern Colombia and Venezuela", 
                      "Guyana Highlands", "Guiana Subregion", "Amazonia", 
                      "Cerrado and associated dry areas", "Coastal Brazil"))   
  
  Gentry_plot <- GEN_CONTRAST %>%
    #filter(Congruence >= 40) %>%
    ggplot(aes(x = Bioregion, y = Consensus_area, fill = Hb)) +
    geom_tile() +
    scale_fill_gradient2(name = "Ub", 
                         low = "#440154FF",
                         mid = "#21908CFF",
                         high = "#FDE725FF",
                         midpoint = 50) +
    facet_wrap(~Scale) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 9, angle = 45, hjust = 1,
                                     margin = margin(t = 5, r = 0, b = 0, l = 0)),
          axis.text.y = element_text(size = 7, hjust = 1,
                                     margin = margin(t = 0, r = 5, b = 0, l = 0)),
          strip.background = element_rect(fill = "white"),
          strip.text = element_text(face = "bold"),
          legend.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 11)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 25))  
  
  Gentry_plot
}

# Saving plot 7
png(filename = "./figs/plot7.png", width = 1300, height = 1000, res = 100)
print(Gentry_plot)
dev.off()

# Morrone Heatmap of Uniformity of Biogeographical Region: Subregion
{
  # Factors for plotting
  subreg_df <- MOR_CONTRAST %>%
    filter(Hierarchy == "Subregion") %>%
    group_by(Consensus_area)
  
  fact_new_interm <- subreg_df[order(subreg_df$Intersect_by), ]
  
  subreg_df$Consensus_area <- 
    factor(subreg_df$Consensus_area, 
           levels = unique(fact_new_interm$Consensus_area))
  
  subreg_df$Bioregion <- 
    factor(subreg_df$Bioregion, 
           levels = c("Antillean subregion", "Brazilian subregion", 
                      "Chacoan subregion"))
  
  Subregion_plot <- subreg_df %>%
    ggplot(aes(x = Bioregion, y = Consensus_area, fill = Hb)) +
    geom_tile() +
    scale_fill_gradient2(name = "Ub", 
                         low = "#440154FF",
                         mid = "#21908CFF",
                         high = "#FDE725FF",
                         midpoint = 50) +
    facet_wrap(~Scale) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 11, angle = 45, hjust = 1,
                                     margin = margin(t = 5, r = 0, b = 0, l = 0)),
          axis.text.y = element_text(size = 7, hjust = 1,
                                     margin = margin(t = 0, r = 5, b = 0, l = 0)),
          strip.background = element_rect(fill = "white"),
          strip.text = element_text(face = "bold"),
          legend.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 11)) 
  
  Subregion_plot
  
}

# Saving plot 8
png(filename = "./figs/plot8.png", width = 800, height = 1000, res = 100)
print(Subregion_plot)
dev.off()

# Morrone Heatmap of Uniformity of Biogeographical Region: Dominion
{
  # Factors for plotting
  
  dom_df <- MOR_CONTRAST %>%
    filter(Hierarchy == "Dominion") %>%
    group_by(Consensus_area)
  
  fact_new_interm <- dom_df[order(dom_df$Intersect_by), ]
  
  dom_df$Consensus_area <- 
    factor(dom_df$Consensus_area, 
           levels = unique(fact_new_interm$Consensus_area))
  
  dom_df$Bioregion <- 
    factor(dom_df$Bioregion, 
    levels = c("Mexican Transition Zone", "Mesoamerican dominion", 
               "Pacific dominion", "Boreal Brazilian dominion", 
               "South Brazilian dominion", "South-eastern Amazonian dominion",
               "Chacoan dominion", "Parana dominion", 
               "South American Transition Zone"))
  
  Dominion_plot <- dom_df %>%
    ggplot(aes(x = Bioregion, y = Consensus_area, fill = Hb)) +
    geom_tile() +
    scale_fill_gradient2(name = "Ub", 
                         low = "#440154FF",
                         mid = "#21908CFF",
                         high = "#FDE725FF",
                         midpoint = 50) +
    facet_wrap(~Scale) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 10, angle = 45, hjust = 1, 
                                     margin = margin(t = 5, r = 0, b = 0, l = 0)),
          axis.text.y = element_text(size = 7, hjust = 1,
                                     margin = margin(t = 0, r = 5, b = 0, l = 0)),
          strip.background = element_rect(fill = "white"),
          strip.text = element_text(face = "bold"),
          legend.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 11)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 20))
  
  Dominion_plot
}

# Saving plot 9
png(filename = "./figs/plot9.png", width = 1400, height = 1000, res = 100)
print(Dominion_plot)
dev.off()

# Morrone Heatmap of Uniformity of Biogeographical Region: Provinces
{
  # Factors for plotting
  prov_df <- MOR_CONTRAST %>%
    filter(Hierarchy == "Province") %>%
    group_by(Consensus_area)
  
  fact_new_interm <- prov_df[order(prov_df$Intersect_by), ]
  
  prov_df$Consensus_area <- 
    factor(prov_df$Consensus_area, 
                                   levels = unique(fact_new_interm$Consensus_area))
  
  
  
  prov_df$Bioregion <- 
    factor(prov_df$Bioregion, 
    levels = c("Bahama province", "Cuban province", "Cayman Islands province", 
               "Jamaica province", "Hispaniola province", "Puerto Rico province",
               "Lesser Antilles province", "Sierra Madre Occidental province", 
               "Sierra Madre Oriental province", 
               "Transmexican Volcanic Belt province",
               "Sierra Madre del Sur province", "Chiapas Highlands province", 
               "Pacific Lowlands province", "Balsas Basin province", 
               "Veracruzan province", "Yucatán Peninsula province", 
               "Mosquito province", "Guatuso-Talamanca province", 
               "Puntarenas-Chiriquí province","Chocó-Darién province", 
               "Guajira province", "Venezuelan province", "Trinidad province",
               "Magdalena province", "Sabana province", "Cauca province", 
               "Western Ecuador province", "Ecuadorian province", 
               "Napo province", "Imerí province", "Pantepui province", 
               "Guianan Lowlands province", "Roraima province", "Pará province",
               "Ucayali province", "Madeira province", "Rondônia province", 
               "Yungas province", "Xingu-Tapajós province",  "Caatinga province", 
               "Cerrado province", "Chacoan province", "Pampean province",
               "Atlantic province", "Parana Forest province", 
               "Araucaria Forest province", "Paramo province",
               "Prepuna province", "Puna province", "Desert province", 
               "Atacaman province", "Cuyan High Andean Province", 
               "Monte province"))
  
  
  Province_plot <- prov_df %>%
    ggplot(aes(x = Bioregion, y = Consensus_area, fill = Hb)) +
    geom_tile() +
    scale_fill_gradient2(name = "Ub", 
                         low = "#440154FF",
                         mid = "#21908CFF",
                         high = "#FDE725FF",
                         midpoint = 50) +
    facet_wrap(~Scale) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 5, angle = 45, hjust = 1, 
                                     margin = margin(t = 5, r = 0, b = 0, l = 0)),
          axis.text.y = element_text(size = 7, hjust = 1, 
                                     margin = margin(t = 0, r = 5, b = 0, l = 0)),
          strip.background = element_rect(fill = "white"),
          strip.text = element_text(face = "bold"),
          legend.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 11)) 
  
  Province_plot
}

# Saving plot 10
png(filename = "./figs/plot10.png", width = 1400, height = 1000, res = 100)
print(Province_plot)
dev.off()

# 4.2.3. Graphical summaries of spatial congruence ####

## NOTE_18: A plot of the spatial congruence against the number of comparisons 
## between consensus areas and biogeographical units can be useful to show the 
## distribution of values of spatial congruence (or Ub or Uc).  

# Example of Comparison vs Congruence plot segregated by biogeographical units
{
  # GEN_CONTRAST and MOR_CONTRAST were created in 4.2.1
  
  # Joining dataframes of Morrone and Gentry
  GENMOR <- rbind(GEN_CONTRAST, MOR_CONTRAST) %>%
    group_by(Hierarchy) %>%
    mutate(med = median(Congruence)) 
  
  compcong_plot <- GENMOR %>%
    ggplot(aes(x = Congruence), group = Hierarchy) +
    geom_histogram(color = "white", bins = 10) +
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    scale_y_continuous(breaks = seq(0, 200, 25)) +
    coord_cartesian(xlim = c(0, 100), expand = TRUE) +
    facet_wrap(~Hierarchy) +
    geom_vline(aes(xintercept = med, group = Hierarchy), 
               colour = "red", lty = 2) +
    labs(y = "Comparisons",
         x = "Spatial congruence") +
    # coord_flip() +
    theme_light() +
    theme(strip.background = element_rect(fill = "gray"),
          strip.text = element_text(face = "bold", colour = "black"),
          strip.placement = "outside")
  
  compcong_plot
}

# Saving plot 11
png(filename = "./figs/plot11.png", width = 800, height = 800, res = 120)
print(compcong_plot)
dev.off()

# Example of a stacked histogram showing the relative numbers of comparison 
# per kind of biogeographical unit
{
  # GEN_CONTRAST and MOR_CONTRAST were created in 4.2.1
  
  # Joining dataframes of Morrone and Gentry
  GENMOR <- rbind(GEN_CONTRAST, MOR_CONTRAST) %>%
    group_by(Hierarchy) %>%
    mutate(med = median(Congruence))
  
  GENMOR$Hierarchy <- 
    factor(GENMOR$Hierarchy, 
           levels = c("Province", "Dominion", "Phytogeographical region",
                      "Subregion"), ordered = TRUE)
  
  histcompcong_plot <- GENMOR %>%
    ggplot(aes(x = Congruence, fill = Hierarchy)) +
    geom_histogram(position = "stack", binwidth = 10, color = "black") +
    scale_fill_grey(start = 0.2, end = 0.9) +
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    scale_y_continuous(breaks = seq(0, 350, 25)) +
    theme_light() +
    labs(fill = "Biogeographical unit",
         y = "Comparisons")
  
  histcompcong_plot
}

# Saving plot 12
png(filename = "./figs/plot12.png", width = 800, height = 800, res = 120)
print(histcompcong_plot)
dev.off()


#------------------------- IV) MAPS DRAWING -------------------------
# 1. Basic maps of consensus areas ####

# A copy of consensus areas shapefiles is stored in 
# "./data/GIS/Consensus_Areas_Shapefiles/"

ca_layer_names <- list.files(path = "./data/GIS/Consensus_Areas_Shapefiles/", 
                             pattern = "[1-3]dg.*grid.shp") %>%
  gsub(pattern = "([1-3]dg.*grid).shp", replacement = "\\1") 

i <- 28 # 28 out of 136 (Consensus areas across all analyses D and S)

(plot_ca <- ndm_plotca(shp.layers.dns = "./data/GIS/Consensus_Areas_Shapefiles/", 
                       area.layer.name = ca_layer_names[i], 
                       ca_Scale = 1, acolor = "darkgreen", alfa = 0.6))

# Saving plot 13
png(filename = "./figs/plot13.png", width = 800, height = 800, res = 120)
print(plot_ca)
dev.off()



