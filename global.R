library(shiny)
library(psrcplot)
library(plotly)
library(bs4Dash)
library(data.table)
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
alldata.long <- alldata.long[!is.na(value)]
alldata.long[Source == "LUV-it", Source := "LUVit"]
sources <- unique(alldata.long[, Source])
ordered.sources <- c(rev(sort(sources[startsWith(sources, "REMI")])), "OFM 2022", "LUVit")
alldata.long[, Source := factor(Source, levels = ordered.sources)]

variables.lu <- unique(alldata[category %in%  c("Population", "Employment"), 
                               .(category, variable, variable_name = `variable`)])
variables.lu <- variables.lu[category != "Population" | variable == "Total Population"]

vars.cat <- unique(variables.lu$category)

# master list
dtype.choice <- c("Counts" = "total",
                  "Annual Change" = "delta",
                  "Percent Annual Change" = "percent_delta",
                  "5y moving average" = "moving_average"
)

# photo list
psrc_photos <- c('bellevuetransitcenter.jpg',
                 'canyon_road.png',
                 'linkbeaconhillstn.jpg',
                 'mtrainierparadisehikers.jpeg',
                 'redmond-housing_0.jpg',
                 'st_northgate_trim.png',
                 'street-intersection.jpeg',
                 'transitorienteddevelopment.jpeg')

