# Add indicators of Households and Household Population for indicator/scenario files
library(data.table)

source("tools.R")

ind_input_file <- "ofm.csv" 
#ind_input_file <- "remi_v32.csv"
ind_input_file <- "remi_v31.csv"
#ind_input_file <- "remi_scenario_LUVit_pop.csv"
#ind_input_file <- "remi_scenario_LUVit_emp_cnty_adj_mig.csv"
#ind_input_file <- "remi_scenario_gma_pop.csv" 
#ind_input_file <- "remi_scenario_higher_amenity.csv" 
#ind_input_file <- "remi_scenario_ofmmatch_labmigoff.csv"

overwrite.existing.indicator <- TRUE
by.gender <- TRUE

data.from.census <- FALSE
data.year <- 2023
acs.year <- 2023
#ind_output_file <- "remi_scenario_ofm23.csv"
#ind_output_file <- "ofm23.csv"
ind_output_file <- ind_input_file

# data.from.census <- TRUE
# data.year <- 2020
# acs.year <- 2020
# ind_output_file <- "ofm_out.csv"

# data.from.census <- TRUE
# data.year <- 2020
# acs.year <- 2023
# ind_output_file <- "remi_scenario_ofm23mix.csv" 


data.file <- if(data.from.census) "census2020_hhpop.csv" else "acs5_hhpop.csv"

indicators <- c("Household Population", "Households")
alldatw <- fread(ind_input_file, header = TRUE)
alldat <- melt(alldatw, id.vars = c("Source", "Main Measure", "Detailed Measure", "Region", "Age", "Race", "Units", "Gender"),
               variable.name = "year", variable.factor = FALSE)
alldat[, year := as.integer(year)]

ind <- "Household Population"
if(ind %in% indicators && overwrite.existing.indicator)
    alldat <- alldat[`Detailed Measure` != ind]

if(ind %in% indicators && !ind %in% alldat[, `Detailed Measure`]){
    # derive HH pop
    gqest <- fread(data.file)
    hhpop <- compute.hhpop(alldat[`Main Measure` == "Population" & `Detailed Measure` == "Total Population" & 
                                      Gender == "Total" & startsWith(Age, "All Ages") & Race == "All Races"],
                           gqest = gqest, yr = data.year)
    alldat <- rbind(alldat, hhpop)
}

ind <- "Households"
if(ind %in% indicators && overwrite.existing.indicator)
    alldat <- alldat[`Detailed Measure` != ind]

if(ind %in% indicators & !ind %in% alldat[, `Detailed Measure`]){
    # commerce method for deriving households
    acsdata <- fread("acs_hhpop_by_age.csv")
    if(is.null(gqest)) fread(data.file)
    dt <- alldat[`Main Measure` == "Population" & `Detailed Measure` == "Total Population" & Race == "All Races" & !startsWith(Age, "All Ages")]
    if(by.gender)
        dt <- dt[Gender != "Total"]
    else dt <- dt[Gender == "Total"]
    hh <- compute.households(dt, 
                             hhpopdt = alldat[`Main Measure` == "Households" & `Detailed Measure` == "Household Population" & 
                                                Gender == "Total" & startsWith(Age, "All Ages") & Race == "All Races"],
                             acs = acsdata, gqest = gqest, 
                             base.year = data.year, acs.year = acs.year, target.year = 2050)
    alldat <- rbind(alldat, hh)
}
# there seems to be some duplicates, so remove them
alldat <- alldat[!duplicated(alldat, by = c("Source", "Main Measure", "Detailed Measure", "Region", "Age", "Race", "Gender", "Units", "year"))]

# convert to wide format
alldatout <- dcast(alldat, Source + `Main Measure` + `Detailed Measure` + Region + Age + Race + Gender + Units ~ year,
                 value.var = "value")
#if(!data.from.census && data.year == 2023 && acs.year == 2023) alldatout[, Source := "OFM(est23)"]
#if(data.from.census && acs.year == 2023 && data.year == 2020) alldatout[, Source := "OFM(est20-23)"]

fwrite(alldatout, file = ind_output_file)
