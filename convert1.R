#' This script for convert from 14°10.508' N to 14.175133
#'  
#

# Load the packages

require(XLConnect)
require(gsubfn)
require(plyr)
require(reshape)
require(reshape2)
require(stringr)
# Load file from exel format this symbol° is the most troblem when it convert from excel to csv or text file

file <- file.path("/Users/iSith/Documents/R.github/DegreeConvert/data.xlsx")

wb <- loadWorkbook(file)

data <- readWorksheet(wb, sheet=1) # this file contains this format 

names(data)

N = 3 # difine what is the column that you want to convert 

n <- ncol(data)      

for (i in 1:2){
        GPS <- data[[N-1+i]]
        mat <- str_split_fixed(GPS, "'", 2)
        z <- sapply((str_split(mat[,1], "[°]")), as.numeric)
        DG <- z[1, ] + z[2, ]/60
        mat[,2] <- str_trim(mat[,2], side = "both")
        coeff <- if(mat[,2] == 'E'){ 
                coeff = 1
        } 
        else { if(mat[,2] == 'N'){
                coeff = 1
        }else { coeff = -1}
        }         
        data[n+i] <- DG*coeff
        names(data)[n+i] <- paste('DG', names(data[N+i-1]), sep = "_" )
}


