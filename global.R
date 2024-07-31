library(shiny)
library(psrcplot)
library(plotly)
library(bs4Dash)
library(data.table)
#library(tidyverse)
library(DT)
library(here)
library(shinycssloaders)

install_psrc_fonts()

alldata <- rbind(fread("data/LUVit.csv"), 
                 fread("data/OFM.csv"), 
                 fread("data/REMIv3.1.csv", header = TRUE),
                 fill = TRUE)
setnames(alldata, c("Main Measure", "Detailed Measure"), c("category", "variable"))

alldata.long <- melt(alldata[, c("Source", "category", "variable", "Region", "Age", "Race", "Units", 
                            as.character(1980:2060)), with = FALSE],
                id.vars = c("Source", "category", "variable", "Region", "Age", "Race", "Units"),
                variable.name = "year", variable.factor = FALSE)
alldata.long <- alldata.long[(startsWith(Age, "All Ages") | is.na(Age)) & (startsWith(Race, "All Races") | is.na(Race))]
alldata.long[, year := as.integer(year)]
alldata.long[Region %in% c("All Regions", "Region Total"), Region := "Region"]
for(cnty in c("King", "Pierce", "Snohomish", "Kitsap"))
    alldata.long[startsWith(Region, cnty), Region := cnty] 

#source('modules/functions.R') # read functions file first

# run all files in the modules sub-directory
module_files <- list.files('modules', full.names = TRUE)
sapply(module_files, source)

