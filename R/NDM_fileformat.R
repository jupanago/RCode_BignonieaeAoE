### Preparing NDM.xyd file for BIG_DB

## Importing Bignonieae database to R

setwd("/Volumes/HD 500GB/R_projects/Endemism/")
library(dplyr)

# Complete database
Total_DB <- read_excel("./Data/Localidades_Total_v12.xlsx")

# Localities' database
Locality_DB <- dplyr::select(Total_DB, ID, NAME1, XCOOR, YCOOR, COUNTRY, MAJOR_AREA, MINOR_AREA, LOCALITY)

# Clean Localities' database with unique locality records
Unique_DB <- Locality_DB %>% distinct(NAME1, YCOOR, XCOOR, .keep_all = TRUE)
write.csv(Unique_DB, "./output/Unique_DB")

#Number of unique locality records per species.
DB_Status_unique <- Locality_DB %>% 
  group_by(NAME1) %>% 
  summarise(xyrecords = n(),
            unique_localities = n_distinct(YCOOR, XCOOR))

glimpse(DB_Status)


## Preparing databse file formats to VNDM/NDM

# Filling decimals to complete 6 decimal digits.

Unique_DB$XCOOR <- format(round(Unique_DB$XCOOR, 6), nsmall = 6)
Unique_DB$YCOOR <- format(round(Unique_DB$YCOOR, 6), nsmall = 6)
glimpse(Unique_DB)


##________________________________________________________________________________________
##________________________________________________________________________________________

# Listing records and extracting names in the right format: NAME1 \n list{XCOOR, YCOOR}.
# Two outputs: NDM_DB, species and coordinates in the require format for VNDM/NDM. Spnames_list, the list of species names as requiered for VNDM/NDM.

NDM_list <- function(x) {
  i <- 1
  name_vec <- unique(x$NAME1)
  
  repeat{
    
    if (is.na(x$NAME1[i] != x$NAME1[i + 1]) == TRUE) {
      break
    }
    
    if (x$NAME1[i] %in% name_vec) {
      print(paste("sp", which(name_vec == x$NAME1[i]) - 1, "[", x$NAME1[i], "]"))
      form_name <- paste("sp", which(name_vec == x$NAME1[i]) - 1, "[", x$NAME1[i], "]")
    }
    
    cat(form_name, file = "./output/NDM_DB", fill = TRUE, append = TRUE)
    cat(form_name, file = "./output/Spnames_list", fill = TRUE, append = TRUE)
    
    while (x$NAME1[i] == x$NAME1[i + 1]) {
      print(paste(x$YCOOR[i], x$XCOOR[i]))
      cat(x$YCOOR[i], x$XCOOR[i], file = "./output/NDM_DB", fill = TRUE, append = TRUE)
      i <- i + 1
      
      if (i == length(x$NAME1)) {
        break
      }
    }
    
    if (x$NAME1[i] != x$NAME1[i + 1] | is.na(x$NAME1[i] != x$NAME1[i + 1]) == TRUE) {
      print(paste(x$YCOOR[i], x$XCOOR[i]))
      cat(x$YCOOR[i], x$XCOOR[i], file = "./output/NDM_DB", fill = TRUE, append = TRUE)
      cat("" , file = "./output/NDM_DB", fill = TRUE, append = TRUE)
      i <- i + 1
    } 
  }
}

NDM_list(Unique_DB)

##________________________________________________________________________________________
##________________________________________________________________________________________

## Species list with NDM code

# Function to import Spnames_list file

ndm_spplist <- function(filepath) {
  
  x <- readLines(filepath)
  
  ini_df <- data.frame(NDM_SppCode = numeric(),
                       Species = character(), 
                       stringsAsFactors = FALSE)
  
  for (i in seq_along(x)) {
    
    if (grepl("sp\\s([0-9]+)\\s.\\s(\\w+\\s\\w+).*", x[i]) == TRUE) {
      
      ini_df <- rbind(ini_df, data.frame(NDM_SppCode = sub("sp\\s([0-9]+)\\s.\\s(\\w+\\s\\w+).*", "\\1", x[i]),
                                         Species = sub("sp\\s([0-9]+)\\s.\\s(\\w+\\s\\w+).*", "\\2", x[i])))
      
    }
  }
  
  return(ini_df)
  
}

spp_list <- ndm_spplist(filepath = "./output/Spnames_list")