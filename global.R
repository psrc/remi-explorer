library(shiny)
library(psrcplot)
library(plotly)
library(bs4Dash)
library(data.table)
library(DT)
library(here)
library(shinycssloaders)

install_psrc_fonts()

alldata <- rbind(fread("data/luvit.csv"), 
                 fread("data/ofm.csv"), 
                 fread("data/remi_v31.csv", header = TRUE), 
                 fread("data/remi_v32.csv", header = TRUE), 
                 fill = TRUE)
setnames(alldata, c("Main Measure", "Detailed Measure"), c("category", "variable"))

alldata.long <- melt(alldata[, c("Source", "category", "variable", "Region", "Age", "Race", "Units", 
                            as.character(1980:2060)), with = FALSE],
                id.vars = c("Source", "category", "variable", "Region", "Age", "Race", "Units"),
                variable.name = "year", variable.factor = FALSE)

alldata.long <- alldata.long[(startsWith(Race, "All Races") | is.na(Race)) & 
                                 (category == "Population" & variable == "Total Population" | category != "Population")]
alldata.long[variable == "Total Population" & !is.na(Age) & !startsWith(Age, "All Ages"), variable := Age]

alldata.long[, year := as.integer(year)]
for(cnty in c("King", "Pierce", "Snohomish", "Kitsap"))
    alldata.long[startsWith(Region, cnty), Region := cnty] 
alldata.long <- alldata.long[!is.na(value)]
alldata.long[Source == "LUV-it", Source := "LUVit"]
sources <- unique(alldata.long[, Source])
ordered.sources <- c(rev(sort(sources[startsWith(sources, "REMI")])), "OFM 2022", "LUVit")
alldata.long[, Source := factor(Source, levels = ordered.sources)]
all.xvalues <- seq(1980, max(alldata.long$year), by = 5)

variables.lu <- unique(alldata.long[category %in%  c("Population", "Employment"), 
                               .(category, variable, variable_name = `variable`)])

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

