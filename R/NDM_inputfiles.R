
################ ndm_xyd function ################

## It creates input .xyd coordinate file from the occurrence database. 

## Arguments: 
## x = a data.frame with columns:
## 1) NAME1: Species names
## 2) XCOOR: Latitude in decimals
## 3) YCOOR: Longitude in decimals

## Output:
## 1) NDM_BD: A text file with the coordinates listed under the species names. Species names in NDM code format 'sp # [ Sp name ]'
## 2) Spnames_list: A text file with the list of species and NDM code. 


ndm_xyd <- function(x) {
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
    
    cat(form_name, file = "./output/NDM_DB.xyd", fill = TRUE, append = TRUE)
    cat(form_name, file = "./output/Spnames_list.txt", fill = TRUE, append = TRUE)
    
    while (x$NAME1[i] == x$NAME1[i + 1]) {
      print(paste(x$YCOOR[i], x$XCOOR[i]))
      cat(x$YCOOR[i], x$XCOOR[i], file = "./output/NDM_DB.xyd", fill = TRUE, append = TRUE)
      i <- i + 1
      
      if (i == length(x$NAME1)) {
        break
      }
    }
    
    if (x$NAME1[i] != x$NAME1[i + 1] | is.na(x$NAME1[i] != x$NAME1[i + 1]) == TRUE) {
      print(paste(x$YCOOR[i], x$XCOOR[i]))
      cat(x$YCOOR[i], x$XCOOR[i], file = "./output/NDM_DB.xyd", fill = TRUE, append = TRUE)
      cat("" , file = "./output/NDM_DB.xyd", fill = TRUE, append = TRUE)
      i <- i + 1
    } 
  }
}


################ ndm_spplist function ################

## It creates a dataframe from the species list with NDM code.

## Arguments:
## 1) filepath = file path from the Spnames_list file obtained as an output from ndm_xyd function.

## Output:
## A data.frame with columns 
## 1) NDM_SppCode: Species number code in VNDM-NDM
## 2) Species: Species name

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



