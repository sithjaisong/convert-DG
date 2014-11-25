#' This script for convert from 14°10.508' N to 14.175133

# Load the packages

require(XLConnect) # Load file from exel format this symbol° is the most troblem when it convert from excel to csv or text file
require(stringr)

file <- file.path("/Users/iSith/Documents/R.github/DegreeConvert/data.xlsx")

wb <- loadWorkbook(file)

data <- readWorksheet(wb, sheet=1) # this file contains this format 
head(data) # this is the format like like 14°10.480' N 

names(data)
N = 3 # difine what is the column that you want to convert Lat or Long but they should be adcent coulumn

n <- ncol(data)      

for (i in 1:2){
        GPS <- data[[N-1+i]] # select the data and store in one 
        mat <- str_split_fixed(GPS, "'", 2) # split it into two coulumns one for degree and another for direction
        #--------------convert the degree minute to decimal degree---------
        z <- sapply((str_split(mat[,1], "[°]")), as.numeric)
        DG <- z[1, ] + z[2, ]/60
        #---------------clean the spacebar of charecter---------------------
        mat[,2] <- str_trim(mat[,2], side = "both")
        #------------------- convert to coefficient-----------------------
        #----------IF [Latitude_Direction]="South" THEN -1 ELSE 1 ----
        #----------IF [Longitude_Direction]="West" THEN -1 ELSE 1 ----
        #Note: Refernce:http://kb.tableausoftware.com/articles/knowledgebase/convert-latitude-longitude
        coeff <- if(mat[,2] == 'E'){ 
                coeff = 1
        } 
        else { if(mat[,2] == 'N'){
                coeff = 1
        }else { coeff = -1}
        }
        #---------------combine the result in the the data---------------
        data[n+i] <- DG*coeff
        names(data)[n+i] <- paste('DG', names(data[N+i-1]), sep = "_" )
}


