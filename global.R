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
                 fread("data/remi_v33.csv", header = TRUE), 
                 fread("data/luvit.csv", header = TRUE), 
                 fread("data/ECO_Model_REF_2018.csv", header = TRUE),
                 fill = TRUE)

# extract scenarios
remi.scenario.files <- list.files("data", "remi_scenario_.*csv")
#names(remi.scenarios) <- gsub("remi_scenario_|.csv", "", remi.scenarios)
#remi.scenarios <- as.list(remi.scenarios)

# add scenarios to data
all.scenarios <- NULL
for(scen in 1:length(remi.scenario.files)){
    thisdat <- fread(file.path("data", remi.scenario.files[scen]), header = TRUE)
    if(!colnames(thisdat)[1] == "Source") # for tables that start at the second row
        thisdat <- fread(file.path("data", remi.scenario.files[scen]), header = TRUE, skip = 1)
    all.scenarios <- rbind(all.scenarios, thisdat, fill = TRUE)
}
remi.scenarios <- unique(all.scenarios$Source)

alldata <- rbind(alldata, all.scenarios, fill = TRUE)                     

setnames(alldata, c("Main Measure", "Detailed Measure"), c("category", "variable"))

alldata[startsWith(Age, "All"), Age := "All Ages"] # consolidate Age labels for all ages

alldata[category == "Population" & startsWith(variable, "Household"), category := "Households"]

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
                                 (category == "Population" & (variable %in% c("Total Population", "Total Migrants", "Retired Migrants",
                                                                              "International Migrants", "Economic Migrants", "Special Populations Migration"#,
                                                                              #"Household Population", "Households"
                                                                              ) #| 
                                                                  #grepl("^Retired Migrants$|^International Migrants$|^Economic Migrants|^Special Populations Migration", variable)
                                                              ) | 
                                      category != "Population")]
alldata.long[variable == "Total Population" & !is.na(Age) & !startsWith(Age, "All Ages"), variable := Age]
alldata.long[category == "Labor Force", variable := gsub(" - Total", "", variable)]

alldata.long[, year := as.integer(year)]
for(cnty in c("King", "Pierce", "Snohomish", "Kitsap"))
    alldata.long[startsWith(Region, cnty), Region := cnty] 
alldata.long <- alldata.long[!is.na(value)]
alldata.long[Source == "LUV-it", Source := "LUVit"]
alldata.long[Source == "ECO_Model_REF_2018", Source := "ECO Model 2018"]
sources <- unique(alldata.long[, Source])

ordered.sources <- c(rev(sort(sources[startsWith(sources, "REMI") & ! sources %in% remi.scenarios])), "OFM 2022", "ECO Model 2018", "LUVit")
ordered.sources.for.pyr <- ordered.sources[!ordered.sources %in% "LUVit"]
alldata.long[, Source := factor(Source, levels = c(ordered.sources, remi.scenarios))]

alldata.long[, Age := gsub("Ages ", "", Age)]

# add aggregated age categories
# 0-14, 15-64
alldata.long[, addage := ifelse(Age %in% c("0-4", "5-9", "10-14"), 1, ifelse(Age %in% c("15-19", "20-64"), 2, 0))]
add.alldata.long <- alldata.long[, .(value = sum(value), .N), by = c("Source", "category", "Region", "addage", "Race", "Units", "Gender", "year")][
    addage > 0 & ((addage == 1 & N == 3) | (addage == 2 & N == 2))][, N := NULL]
add.alldata.long[addage == 1, `:=`(variable = "Ages 0-14", Age = "0-14")]
add.alldata.long[addage == 2, `:=`(variable = "Ages 15-64", Age = "15-64")]
alldata.long <- rbind(alldata.long, add.alldata.long)[, addage := NULL]

# pyramid data
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

# extract variable names
variables.lu <- unique(alldata.trends[category %in%  c("Population", "Households", "Employment", "Labor Force"), 
                               .(category, variable, variable_name = `variable`)])


# reorder variables
# Pop ordering
r <- 1
for(var in c("Total Population", "Households", "Household Population")){
    variables.lu[variable == var, rank := r]
    r <- r+1
}
variables.lu[is.na(rank), rank := r:(sum(is.na(rank))+r-1)]
variables.lu <- rbind(variables.lu[category == "Population"][order(rank)],
                      variables.lu[category == "Employment"][order(variable)],
                      variables.lu[category == "Households"][order(rank)],
                      variables.lu[category == "Labor Force"]
                      )

vars.cat <- unique(variables.lu$category)

# master list
dtype.choice <- c("Counts" = "total",
                  "Annual Change" = "delta",
                  "Percent Annual Change" = "percent_delta",
                  "5y moving average" = "moving_average"
                 )
tooltip.digits <- c(total = 0, delta = 1, percent_delta = 2, moving_average = 2, 
                    functions = 3) 

# photo list
psrc_photos <- list.files("www", "*.jpg")
# psrc_photos <- c('bellevuetransitcenter.jpg',
#                  'canyon_road.png',
#                  'linkbeaconhillstn.jpg',
#                  'mtrainierparadisehikers.jpeg',
#                  'redmond-housing_0.jpg',
#                  'st_northgate_trim.png',
#                  'street-intersection.jpeg',
#                  'transitorienteddevelopment.jpeg')

plot_height <- "500px"


psrc_style_modified <- function() {
    # like psrcplot::psrc_style with some axes and grid options turned off
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
        #This sets the text font, size and colour for the axis text, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
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

make_interactive_modified <- function(p, title=NULL, subtitle=NULL, remove_labs = TRUE){
    # like psrcplot::make_interactive with vertical hovermode 
    
    x.vals <- length(ggplot2::layer_scales(p)$x$range$range)                                         # Number of x categories in ggplot object
    x.pos <- ggplot2::layer_scales(p)$x$position                                                     # Left or bottom (i.e. bar or column chart)
    geom_list <- sapply(p$layers, function(x) class(x$geom)[1])                                      # Used to differentiate between chart types  
    #hover_yn <- if("GeomBar" %in% geom_list){NULL}else{"x"}
    #hover_yn <- "y unified"
    hover_yn <- "closest"
    vlift <- if("GeomBar" %in% geom_list){1.10}else{1.05}
    
    if(remove_labs)
        p <- p + ggplot2::theme(axis.title = ggplot2::element_blank())                                   # Remove Bar labels and axis titles
    m <- list(l = 50, r = 50, b = 200, t = 200, pad = 4)
    p <- plotly::ggplotly(p, tooltip=c("text"), autosize = T, margin = m)                            # Make Interactive
    p <- plotly::style(p, hoverlabel=list(font=list(family="Poppins", size=11, color="white")))      # Set Font for Hover-Text
    p <- plotly::layout(p, xaxis=list(tickfont=list(family="Poppins", size=11, color="#2f3030")))    # Format X-Axis
    p <- plotly::layout(p, yaxis=list(tickfont=list(family="Poppins", size=11, color="#2f3030")))    # Format Y-Axis
    
    # Turn on Legend
    # if labels are rotated, they might run into the legend now?
    p <- plotly::layout(p,
                        legend=list(orientation="h", xanchor="center", xref="container", x=0.5, 
                                    y=if(remove_labs) -0.10 else -0.2,         
                                    title="", font=list(family="Poppins", size=11, color="#2f3030"),
                                    pad=list(b=50, t=50)),
                        hovermode = hover_yn)
    
    
    p <- plotly::layout(p, title= list(text = ""))                                                   # Remove Plotly Title
    
    if(!(is.null(title)) & !(is.null(subtitle))) {                                                   # If there is both title and subtitle
        
        p <- plotly::layout(p, 
                            annotations = list(x= 0 , y = vlift + 0.05, text = title,                  # -- add the title, located high enough for room for subtitle
                                               xref='paper', yref='paper', showarrow = FALSE, 
                                               font = list(family="Poppins Black",size=14, color="#4C4C4C")))
        p <- plotly::layout(p, 
                            annotations = list(x= 0, y = vlift, text = subtitle,                       # -- then add the subtitle 
                                               showarrow = FALSE, xref='paper', yref='paper', 
                                               font=list(family="Poppins",size=12, color="#4C4C4C")))
    }else if(!(is.null(title)) & is.null(subtitle)) {                                                # If there is no Subtitle
        
        p <- plotly::layout(p, 
                            annotations = list(x= 0, y = vlift, text = title,                          # -- just add the title
                                               xref='paper', yref='paper', showarrow = FALSE,
                                               font = list(family="Poppins Black",size=14, color="#4C4C4C")))
    }
    return(p)
}
