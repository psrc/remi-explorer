library(data.table)
library(readxl)

# what should this scenario be called
scenario.name <- "LUVit_emp_cnty_adj_mig" 

# name of the Excel file containing REMI results, exported directly from REMI
remi.results.file <- "data/Dashboard_Indicators_LUVitEmpSecCntyAltMigSpeedAdj.xlsx"
#remi.results.file <- "~/T/2025Q1/Hana/REMI/Dashboard_Indicators_LUVitEmpSecCntyAltMigSpeedAdj.xlsx"

# names of the individual sheets that we want to include in the resulting dataset
# and the corresponding value in the "Main Measure" column 
# (as that's not part of the exported data) 
sheets <- list("Population by Age Group and Cou" = "Population",
               "Labor Force By Race and County" = "Labor Force",
               "Annual Pop Chg by Source" = "Population",
               "Components of Population Change" = "Population",
               "Special Populations" = "Population",
               "Personal Income Components by C" = "Income",
               "GDP Profile" = "GDP",
               "Price Profile" = "Price",
               "Population By 5 Year Ages Race " = "Population",
               "Employment by Sector and County" = "Employment"
               )

# for checking, see the names of all sheets in the file
# excel_sheets(remi.results.file)

# columns names from all sheets that are not time columns 
id.vars <- c("Category", "Industry", "Region", "Race", "Age", "Gender", "Units")

alldat <- NULL # this will hold all data
# iterate over sheets
for(sh in names(sheets)){
    # read the meta info from the first few rows of the sheet
    remi.meta <- data.table(read_xlsx(remi.results.file, sheet = sh, skip = 1, n_max = 1))
    # read the data in the sheet
    remi.output <- data.table(read_xlsx(remi.results.file, sheet = sh, skip = 4))
    # columns that we need and that are in the meta info and not in the main dataset
    extra.cols <- setdiff(colnames(remi.meta), c("Comparison Type", "Forecast"))
    # add those columns into the main dataset
    for(col in extra.cols)
        remi.output <- remi.output[, (col) := remi.meta[[col]]]
    # convert into long format
    remil <- melt(remi.output, id.vars = intersect(colnames(remi.output), id.vars), variable.name = "year",
                  variable.factor = FALSE)
    # for the employment dataset keep it for later
    # (assumes there is just one Employment sheet; change this if it's not the case)
    if(sheets[[sh]] == "Employment") {
        remil[, Category := Industry][, Industry := NULL] # rename Industry -> Category
        remil[startsWith(Category, "Const Res"), Category := "Const Res"]
        remil[startsWith(Category, "All Industries"), Category := "All Sectors w Military"]
        remil[startsWith(Category, "Retail"), Category := "Retail (no 722)"]
        remiemp <- remil[as.integer(year) > 2021] # for later keep only current and projected years
        # aggregate FIRE and Services
        remil[Category %in% c("FIRE", "Services"), value := sum(value), by = c("Region", "Units", "year")]
        remil <- remil[Category != "FIRE"][Category == "Services", Category := "FIRE and Services"]
        remil[, Category := paste(Category, "REMI")] # add "REMI" to the Category value
    }
    # add the "Main Measure" column and add this dataset to the previous results
    alldat <- rbind(alldat, 
                    remil[, `:=`(`Main Measure` = sheets[[sh]])], fill = TRUE
                    )
}
# process employment
# read employment converter
converter <- fread("employment_converter.csv")
converter[, mult := WAQB2022 * REF18fct] # apply TotEmp factors to 2022 W&S
for(cnty in unique(converter$Region)) # fill in the "All Industries" rows
    converter[Region == cnty, mult := ifelse(Category == "All Sectors w Military", sum(mult), mult)]

# in the employment data rename a few Region and Category values to make it 
# consistent with the converter dataset
remiemp[, Region := gsub("All Regions", "Region", Region)]

# compute the growth rates to 2022 for all years
remiemp[remiemp[year == 2022], growth.rate := value/i.value, on = c("Category","Region", "Units")]

# merge in the 2022 values from the converter
remiemp[converter, conv2022 := i.mult, on = c("Category", "Region")]

# apply the growth rate to the 2022 values
remiemp[, new.value := conv2022 * growth.rate]

# aggregate FIRE and Services
remiemp[Category %in% c("FIRE", "Services"), Category := "FIRE and Services"] # give the two categories the same name
remiempagg <- remiemp[, .(value = sum(new.value)), by = c("Category","Region", "Units", "year")][
    , `Main Measure` := "Employment"] # aggregate and set the "Main Measure" 

# add the employment dataset to all the other data
alldat <- rbind(alldat, remiempagg, fill = TRUE)

# add the Source column
alldat[, Source := scenario.name]

# rename the Category column
setnames(alldat, "Category", "Detailed Measure")

# rename All Regions -> Region
alldat[startsWith(Region, "All Regions"), Region := "Region"]

# rename Population -> Total Population
alldat[`Detailed Measure` == "Population", `Detailed Measure` := "Total Population"]

# make Units consistent
alldat[startsWith(Units, "Thousands"), Units := "Thousands"]

# fix Age, Race & Gender
alldat[is.na(Age), Age := "All Ages"]
alldat[is.na(Race), Race := "All Races"]
alldat[is.na(Gender), Gender := "Total"]

# there seems to be some duplicates, so remove them
alldat <- alldat[!duplicated(alldat, by = c("Source", "Main Measure", "Detailed Measure", "Region", "Age", "Race", "Gender", "Units", "year"))]

# convert to wide format
alldatw <- dcast(alldat, Source + `Main Measure` + `Detailed Measure` + Region + Age + Race + Gender + Units ~ year,
                 value.var = "value")

# store into a file
fwrite(alldatw, file = paste0("remi_scenario_", scenario.name, ".csv"))
