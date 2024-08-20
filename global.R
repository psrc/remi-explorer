library(shiny)
library(psrcplot)
library(plotly)
library(bs4Dash)
library(data.table)
library(DT)
library(here)
library(shinycssloaders)

install_psrc_fonts()

alldata <- rbind(fread("data/ofm.csv", header = TRUE), 
                 fread("data/remi_v31.csv", header = TRUE), 
                 fread("data/remi_v32.csv", header = TRUE), 
                 fread("data/luvit.csv"), 
                 fill = TRUE)
setnames(alldata, c("Main Measure", "Detailed Measure"), c("category", "variable"))

alldata[startsWith(Age, "All"), Age := "All Ages"] # consolidate Age labels for all ages

alldata <- alldata[!duplicated(alldata, by = c("Source", "category", "variable", "Region", "Age", "Race", "Gender", "Units"))]

alldata.long <- melt(alldata[, c("Source", "category", "variable", "Region", "Age", "Race", "Units", "Gender",
                            as.character(1980:2060)), with = FALSE],
                id.vars = c("Source", "category", "variable", "Region", "Age", "Race", "Units", "Gender"),
                variable.name = "year", variable.factor = FALSE)

# add rows for all races if missing
alldata.long[, has_all_races := sum(Race == "All Races" | is.na(Race)), by = c("Source", "category", "variable", "Region", "Age", "Units", "Gender", "year")]
alldata.long <- rbind(alldata.long, alldata.long[has_all_races == 0, .(Race = "All Races", value = sum(value)), 
                                                 by = c("Source", "category", "variable", "Region", "Age", "Units", "Gender", "year")],
                      fill = TRUE)[, has_all_races := NULL]
alldata.long <- alldata.long[(startsWith(Race, "All Races") | is.na(Race)) & 
                                 (category == "Population" & variable %in% c("Total Population", "Total Migrants") | category != "Population")]
alldata.long[variable == "Total Population" & !is.na(Age) & !startsWith(Age, "All Ages"), variable := Age]

alldata.long[, year := as.integer(year)]
for(cnty in c("King", "Pierce", "Snohomish", "Kitsap"))
    alldata.long[startsWith(Region, cnty), Region := cnty] 
alldata.long <- alldata.long[!is.na(value)]
alldata.long[Source == "LUV-it", Source := "LUVit"]
sources <- unique(alldata.long[, Source])
ordered.sources <- c(rev(sort(sources[startsWith(sources, "REMI")])), "OFM 2022", "LUVit")
ordered.sources.for.pyr <- ordered.sources[!ordered.sources %in% "LUVit"]
alldata.long[, Source := factor(Source, levels = ordered.sources)]

alldata.long[, Age := gsub("Ages ", "", Age)]

pyr.index.oag <- with(alldata.long, Gender %in% c("Male", "Female")  & ! startsWith(Age, "All")  & ! startsWith(Age, "Total"))
oag.index <- with(alldata.long, pyr.index.oag & grepl('+', Age, fixed = TRUE))
pyr.index <- pyr.index.oag & with(alldata.long, grepl('-', Age))
alldata.long[oag.index, `:=`(lower.age.limit = as.integer(gsub("+", "", Age, fixed = TRUE)), upper.age.limit = 0)]
alldata.long[pyr.index, lower.age.limit := as.integer(sapply(strsplit(alldata.long[pyr.index]$Age, '-'), function(x) x[[1]]))]
alldata.long[pyr.index, upper.age.limit := as.integer(sapply(strsplit(alldata.long[pyr.index]$Age, '-'), function(x) x[[2]]))]
alldata.long[, is.pyramid := ifelse(!is.na(lower.age.limit) & (upper.age.limit == 0 | upper.age.limit - lower.age.limit < 5), TRUE, FALSE)]
alldata.long[is.pyramid == TRUE, mid.age := ifelse(upper.age.limit == 0, lower.age.limit, lower.age.limit + (upper.age.limit - lower.age.limit)/2)] 

alldata.pyramid <- alldata.long[is.pyramid == TRUE][, is.pyramid := NULL]
alldata.trends <- alldata.long[is.pyramid == FALSE & (is.na(Gender) | Gender %in% "Total")][, is.pyramid := NULL]
    
all.xvalues <- seq(1980, max(alldata.long$year), by = 5)

alldata.trends[category == "Employment", variable := gsub("REMI", "REMI (BEA)", variable)]

variables.lu <- unique(alldata.trends[category %in%  c("Population", "Employment"), 
                               .(category, variable, variable_name = `variable`)])

# reorder variables
variables.lu <- rbind(variables.lu[category == "Population"],
                      variables.lu[category == "Employment"][order(variable)]
                      )

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

plot_height <- "500px"


psrc_style_modified <- function() {
    font <- "Poppins"
    
    ggplot2::theme(
        
        #Text format:
        #This sets the font, size, type and color of text for the chart's title
        plot.title = ggplot2::element_text(family=font,
                                           face="bold",
                                           size=13, 
                                           color='#4C4C4C'),
        plot.title.position = "plot",
        
        #This sets the font, size, type and color of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
        plot.subtitle = ggplot2::element_text(family=font,
                                              size=12,
                                              margin=ggplot2::margin(9,0,9,0)),
        
        #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
        plot.caption =  ggplot2::element_text(family=font,
                                              size=10,
                                              face="italic",
                                              color="#4C4C4C",
                                              hjust=0),
        plot.caption.position = "plot",
        
        #Legend format
        #This sets the position and alignment of the legend, removes a title and background for it and sets the requirements for any text within the legend.
        legend.position = "bottom",
        legend.background = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(family=font,
                                            size=12,
                                            color="#4C4C4C"),
        
        #Blank background
        #This sets the panel background as blank, removing the standard grey ggplot background color from the plot
        panel.background = ggplot2::element_blank(),
        
        #Strip background sets the panel background for facet-wrapped plots to PSRC Gray and sets the title size of the facet-wrap title
        strip.background = ggplot2::element_rect(fill="#BCBEC0"),
        strip.text = ggplot2::element_text(size  = 12,  hjust = 0),
        
        #Axis format
        #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
        axis.title = ggplot2::element_text(family=font, size=12, color="#2f3030"),
        axis.text = ggplot2::element_text(family=font, size=11, color="#2f3030"),
        axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
        axis.ticks = ggplot2::element_blank(),
        #axis.line = ggplot2::element_blank(),
        
        #Grid lines
        #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines.
        #panel.grid.minor = ggplot2::element_blank(),
        #panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
        #panel.grid.major.x = ggplot2::element_blank(),
        
        panel.grid.minor = ggplot2::element_line(color="#cbcbcb"),
        panel.grid.major = ggplot2::element_line(color="#cbcbcb")
    )
}