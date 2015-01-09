##############################################################################
# titile        : combcert1.R;
# purpose       : convert from 14°10.508' (Degree Minute) N to 14.175133 (Decimal Degree);
# producer      : Sith Jaisong and A. H. Sparks;
#                 Plant Disease Management team
#                 CESD, IRRI;
# last update   : In Los Baños, 25 Nov. 2014;
# inputs        : excel or text or csv files containing the data of Lattitude or Longtiture;
# outputs       : files will have new two columns at the last columns
# remarks 1     : For idea, please follow the example files (data1.xls);
# Licence:      : GPL2;
##############################################################################
#' This script for convert from 14,10.508N to 14.175133

# Load the packages

require(stringr)

data <- read.csv(file = file.path("~/Documents/R.github/convert-DG/data1.csv"), header= T, stringsAsFactor = F) # change the file.path whare your files are 

head(data) # this is the format like like 14°10.480' N 

names(data)
N = 6 # difine what is the column that you want to convert Lat or Long but they should be adcent coulumn

n <- ncol(data)      
i=1
for (i in 1:2){
        GPS <- data[[N-1+i]] # select the data and store in one 
        mat <- str_split_fixed(GPS, "'", 2) # split it into two coulumns one for degree and another for direction
        #--------------convert the degree minute to decimal degree---------
        z <- sapply((str_split(mat[,1], "[,]")), as.numeric)
        DG <- z[1, ] + z[2, ]/60
        #---------------clean the spacebar of charecter---------------------
        mat[,2] <- str_trim(mat[,2], side = "both")
        #------------------- convert to coefficient-----------------------
        #----------IF [Latitude_Direction]="South" THEN -1 ELSE 1 ----
        #----------IF [Longitude_Direction]="West" THEN -1 ELSE 1 ----
        #Note: Refernce:http://kb.tableausoftware.com/articles/knowledgebase/convert-latitude-longitude
        coeff <- ifelse(as.data.frame(mat[,2])=='N',1, 
                        ifelse(as.data.frame(mat[,2]) == 'E', 1, -1 )
                        )
                        
                        #---------------combine the result in the the data---------------
                        data[n+i] <- DG*coeff
                        names(data)[n+i] <- paste('DG', names(data[N+i-1]), sep = "_" )
}
#eos
