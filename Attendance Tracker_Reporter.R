# Center Attendance Tracker & Reporter

## Sets the Working Directory to the Desktop
setwd("/Users/lorenzoramirez/Desktop")

## Install supporting packages
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("tidyverse")
library(tidyverse)


## Read in Table 
attendance <- read.csv("ALLIES Daily Attendance.csv")

## Remove Empty Columns from table
attendance2 <-  attendance[, -17:-39]

## Getting details of the data table
head(attendance2)
dim(attendance2)

## Unique entries for visitors
Uniq <- unique(attendance2$Email)
sum(table(Uniq))

## Makes a table to report total visits per month by year & graphs it
Month_Table <- table(attendance2$Month, attendance2$Year) # Total visits per month by year
barplot(Month_Table, main = "Visitors by Month & Year", legend = 
            rownames(Month_Table), col = c("mediumpurple", "lightgoldenrod3", "lightgoldenrodyellow", 
                                           "deeppink2", "cyan4", "white", 
                                           "cornflowerblue", "aquamarine", "orange", 
                                           "deeppink4", "dark green", "grey"), 
                    ylab = "Number of Visitors")



## Takes the Reasons for Visiting & Reports them Monthly by year
attendance_summary <- function(ReasonName, FUN = sum) {
    tapply(attendance2[, ReasonName], attendance2[,1:2], sum)
}

## Character Vector detailing the 'reasons' (i.e. column names) of attendance table
reasons <- c("Advising", "Hang.Out", "Food", "Printing", "Supplies", "Studying", "Lending.Library", "OTHER")

## Loops through each reason passed and runs the attendancesummary function to output a table per reason
for(i in 1:length(reasons)) {
    RR <- reasons[[i]]
    ADSum <- attendance_summary(RR)
    print(reasons[[i]])
    print(ADSum)  
}
