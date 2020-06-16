library(rlist)
library(readxl)
library(jsonlite)
library(stringr)


datain <- read_excel("~/datain.xlsm", sheet = "Tabelle1")
days <- colnames(datain)[5:ncol(datain)]


extractPerDay <- function(day){
  lstTotal <- list()
  
  i <- 0
  for(i in 1:length(datain$`Country/Region`)){
    lstBuffer <- list()
    #i <- match(country, datain$`Country/Region`)
    line <- datain[i,]
    
    if(is.na(datain$`Province/State`[[i]])){
      lstBuffer[["state"]] <- ""
      }
    else{
     lstBuffer[["state"]] <- unlist(datain$`Province/State`[[i]])
    }
    
    lstBuffer[["pos"]] <- c(as.numeric(datain$Long[i]), as.numeric(datain$Lat[i]))
    lstBuffer[["country"]] <- unlist(datain$`Country/Region`[i])
    lstBuffer[["date"]] <- day
    lstBuffer["confirmed"] <- as.double(line[day])
    #print(i)
    #element <- c(as.character(state), as.character(pais), date, pos, rate)
    lstTotal <- list.append(lstTotal, lstBuffer)
  }
  return(lstTotal)
}


fetch <- lapply(days, extractPerDay)

newJson <- toJSON(fetch, .na="string", auto_unbox = TRUE)
newJson <- str_replace(newJson, "\\[\\[", "[")
newJson <- str_replace(newJson, "\\]\\]", "]")
newJson <- str_replace_all(newJson, "\\}\\],\\[\\{", "\\},\\{")


write(newJson, "C:\\Users\\enado\\covid\\input.json")