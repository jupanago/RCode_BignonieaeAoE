## NDM_tables

## Summarizing consensus areas information

################ ndm_casumm function ################

## This function creates a dataframe summarizing consensus area information for each analysis and scale.
## It gives the number of individual areas, species, groups, consensus areas, and ambiguous species per analysis and scale.

## Arguments:
## 1) x: A dataframe obtained from ndm_dataparsing function. 

ndm_casum <- function(x) {
  
  library(dplyr)
  
  ## Counting the number of areas formed by genera and phylogroups per analysis
  Grps <- x %>%
    group_by(Analysis, Spatial_Scale) %>%
    summarise(Groups_number = sum(Groups))
  
  Summary_info <- x %>%
    distinct(Analysis, Spatial_Scale, Consensus_Area, .keep_all = TRUE) %>%
    group_by(Analysis, Spatial_Scale) %>%
    # filter(Analysis %in% c("H", "S", "T")) %>%
    summarise(Indiv_Areas = sum(Individual_Areas),
              Spp = sum(Number_Spp),
              Groups = sum(Groups),
              Cons_Areas = n())
  
  Spp_uniq_number <- x %>%
    group_by(Analysis, Spatial_Scale) %>%
    summarise(Spp = length(unique(Species)))
  
  Summary_info$Spp <- Spp_uniq_number$Spp
  
  Summary_info$Groups[3] <- Grps$Groups_number[3]
  Summary_info$Spp[3] <- Summary_info$Spp[3] - Summary_info$Groups[3]
  
  Summary_info$Groups[6] <- Grps$Groups_number[6]
  Summary_info$Spp[6] <- Summary_info$Spp[6] - Summary_info$Groups[6]
  
  ### Ambiguous consensus area data
  
  CA_amb <- function(x, scl, ana) {
    
    ## Function to show consensus areas with ambiguous species for an specified analysis.
    ## Ambiguous species are those that appear as part of several consensus areas, therefore, they are duplicated records.
    ## x is a data.frame where the species part of consensus areas are in column named 'Species'
    ## spscale and analys are arguments for my own areas and analysis nomenclature. They indicate scale and analysis, respectively.
    
    require(dplyr)
    
    y <- x %>%
      filter(Spatial_Scale == scl, Analysis == ana) %>%
      dplyr::select(Species)
    
    index <- which(duplicated(y$Species))
    spp_vec <- unique(y$Species[index])
    
    
    Con_Areas_amb <- x %>% 
      filter(Spatial_Scale == scl, Analysis == ana) %>% 
      filter(Species %in% spp_vec) %>% 
      pull(Consensus_Area) %>% 
      unique() %>% 
      length()
    
    Con_Areas_amb
    
  }
  
  CAambD1 <- CA_amb(x, scl = "1", ana = "D")
  CAambD2 <- CA_amb(x, scl = "2", ana = "D")
  CAambD3 <- CA_amb(x, scl = "3", ana = "D")
  
  CAambS1 <- CA_amb(x, scl = "1", ana = "S")
  CAambS2 <- CA_amb(x, scl = "2", ana = "S")
  CAambS3 <- CA_amb(x, scl = "3", ana = "S")
  
  CAamb_vec <- c(CAambD1, CAambD2, CAambD3, CAambS1, CAambS2, CAambS3)
  
  Summary_info$Cons_Areas_amb <- CAamb_vec
  
  ### Ambiguous species data
  
  spp_conflict <- function(x, scl, ana) {
    
    ## Function to show ambiguous species for an specified analysis.
    ## Ambiguous species are those that appear as part of several consensus areas, therefore, they are duplicated records.
    ## x is a data.frame where the species part of consensus areas are in column named 'Species'
    ## spscale and analys are arguments for my own areas and analysis nomenclature. They indicate scale and analysis, respectively.
    
    require(dplyr)
    
    y <- x %>%
      filter(Spatial_Scale == scl, Analysis == ana) %>%
      dplyr::select(Species)
    
    index <- which(duplicated(y$Species))
    unique(y$Species[index])
    
  }
  
  SppD1 <- spp_conflict(x, scl = "1", ana = "D")
  SppD2 <- spp_conflict(x, scl = "2", ana = "D")
  SppD3 <- spp_conflict(x, scl = "3", ana = "D")
  
  SppS1 <- spp_conflict(x, scl = "1", ana = "S")
  SppS2 <- spp_conflict(x, scl = "2", ana = "S")
  SppS3 <- spp_conflict(x, scl = "3", ana = "S")
  
  conflict_list <- list(SppD1, SppD2, SppD3, SppS1, SppS2, SppS3)
  
  Summary_info$spp_amb <- sapply(conflict_list, length, simplify = "array")
  
  return(Summary_info)
  
}
## Loading data

################ ndm_spplost function ################

## This function gives a dataframe of species with inconsistent behavior across different spatial scales.
## These species with irregular behavior are generally species with very low sampling.
## The data frame has the columns NDM_SppCode, which gives the number of the species in the NDM analysis; Species, the name of the species;
## and scale_change, which says in which scale the species appeared and in what scale it disappeared. From 1 to 2 means the species was
## part of the pattern at one degree but not at two degrees. 

## Arguments:
## 1) x = A dataframe obtained from ndm_dataparsing function that includes all the analyses at different spatial scales. 
## 2) analysis = the kind of analysis performed in this work. "D" for Default and "S" for strict.

ndm_spplost <- function(x, analysis) {
  
  library(dplyr)
  
  ## Scoring species per spatial scale in the default analysis
  in1 <- x %>%
    filter(Analysis == analysis, Spatial_Scale == "1") %>%
    dplyr::select(2, 3)
  
  in2 <- x %>%
    filter(Analysis == analysis, Spatial_Scale == "2") %>%
    dplyr::select(2, 3)
  
  in3 <- x %>%
    filter(Analysis == analysis, Spatial_Scale == "3") %>%
    dplyr::select(2, 3)

  ## Species difference between scales 1dif2, 1dif3, and 2dif3
  
  # noin1 <- setdiff(spp_list[,2], in1[[2]])
  # noin2 <- setdiff(spp_list[,2], in2[[2]])
  # noin3 <- setdiff(spp_list[,2], in3[[2]])
  
  diff1_2 <- setdiff(in1[[2]], in2[[2]])
  #setdiff(noin2, noin1)
  
  diff1_3 <- setdiff(in1[[2]], in3[[2]])
  #setdiff(noin3, noin1) %in% in1[[2]]
  
  diff2_3 <- setdiff(in2[[2]], in3[[2]])
  #setdiff(noin3, noin2)
  
  spp_lost <- c(diff1_2, diff1_3, diff2_3)
  
  scale_change <- c(rep("From 1 to 2", length(diff1_2)), rep("From 1 to 3", length(diff1_3)), rep("From 2 to 3", length(diff2_3)))
  
  
  spp_lost <- tibble(Spp_lost = spp_lost,
                     Scales = scale_change)
  
  return(spp_lost)
  
}

################ ndm_sppout function ################

## This function gives the list of species that do not scored for any pattern accros scales

## Arguments: 
## 1) x = A dataframe obtained from ndm_dataparsing function that includes all the analyses at different spatial scales.
## 2) analysis = the kind of heuristic search performed in this work. "D" for Default and "S" for strict.

ndm_sppout <- function(x, analysis) {
  
  library(dplyr)
  
  ## Scoring species per spatial scale in the default analysis
  in1 <- x %>%
    filter(Analysis == analysis, Spatial_Scale == "1") %>%
    dplyr::select(2, 3)
  
  in2 <- x %>%
    filter(Analysis == analysis, Spatial_Scale == "2") %>%
    dplyr::select(2, 3)
  
  in3 <- x %>%
    filter(Analysis == analysis, Spatial_Scale == "3") %>%
    dplyr::select(2, 3)
  
  # Species that do not form part of any pattern across scales
  noin1 <- setdiff(spp_list[,2], in1[[2]])
  noin2 <- setdiff(spp_list[,2], in2[[2]])
  noin3 <- setdiff(spp_list[,2], in3[[2]])
  
  
  spp_outvec <- intersect(intersect(noin1, noin2), noin3)
  
  spp_out <- tibble(species = spp_outvec)
  
  return(spp_out)
  
}


