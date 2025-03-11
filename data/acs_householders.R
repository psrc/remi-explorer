library(psrccensus)
library(data.table)
library(magrittr)

counties <- c("King", "Kitsap", "Pierce", "Snohomish", "Region") # for ordering rows
# census
cgq <- get_decennial_recs(geography="county", variables="P5_001N", 
                          sumfile = "pl", years=2020)  %>% setDT()       # Group quarters
cpop <- get_decennial_recs(geography="county", variables="DP1_0001C", 
                           sumfile = "dp", years=2020)  %>% setDT()      # Population
chh <- get_decennial_recs(geography="county", variables="DP1_0158C", 
                          sumfile = "dp", years=2020)  %>% setDT()        # Households

final_census <- merge(merge(
                  cpop[, .(year, NAME, Pop = value)],
                  chh[, .(year, NAME, HH = value)], by = c("year", "NAME")),
                cgq[, .(year, NAME, GQ = value)], by = c("year", "NAME")
                )
final_census[, HHpop := Pop - GQ][, name := factor(gsub(" County, Washington", "", NAME), counties)][, NAME := NULL]
final_census <- final_census[order(name)]
fwrite(final_census, file = "census2020_hhpop.csv")

# 5-year ACS
acs.years <- 2009:2023
total_pop   <- get_acs_recs(geography="county",                         # Total population
                            table.names="B01003", 
                            years=acs.years, 
                            acs.type="acs5")   %>% setDT() 

hh   <- get_acs_recs(geography="county",                                # Households
                            table.names="B25003", 
                            years=acs.years, 
                            acs.type="acs5")   %>% setDT()

hhpop <- get_acs_recs(geography="county",                                # Household population
                      table.names="B25008", 
                      years=acs.years, 
                      acs.type="acs5")   %>% setDT()

# gq <- get_acs_recs(geography="county",                                # Group quarters
#                       table.names="B26001", 
#                       years=c(2020,2023), 
#                       acs.type="acs5")   %>% setDT()

final5 <- merge(merge(
                  total_pop[, .(year, name, Pop = estimate)],
                  hh[label %in% c("Estimate!!Total:", "Estimate!!Total"), .(year, name, HH = estimate)], 
                by = c("year", "name")),
                  hhpop[label  %in% c("Estimate!!Total:", "Estimate!!Total"), .(year, name, HHpop = estimate)],
               by = c("year", "name"))
final5[, GQ := Pop - HHpop][, name := factor(gsub(" County", "", name), counties)]
final5 <- final5[order(name)]
fwrite(final5, file = "acs5_hhpop.csv")

# 1-year acs
total_pop   <- get_acs_recs(geography="county",                         # Total population
                            table.names="B01003", 
                            years=c(2023), 
                            acs.type="acs1")   %>% setDT() 

hh   <- get_acs_recs(geography="county",                                # Households
                     table.names="B25003", 
                     years=c(2023), 
                     acs.type="acs1")   %>% setDT()

hhpop <- get_acs_recs(geography="county",                                # Household population
                      table.names="B25008", 
                      years=c(2023), 
                      acs.type="acs1")   %>% setDT()

final1 <- merge(merge(
            total_pop[, .(year, name, Pop = estimate)],
            hh[label == "Estimate!!Total:", .(year, name, HH = estimate)], 
        by = c("year", "name")),
            hhpop[label == "Estimate!!Total:", .(year, name, HHpop = estimate)],
            by = c("year", "name"))
final1[, GQ := Pop - HHpop][, name := factor(gsub(" County", "", name), counties)]
final1 <- final1[order(name)]

fwrite(final1, file = "acs1_hhpop.csv")

tmp <- merge(census[, .(name, `2020CensusHHsize` = hhsize, `2020CensusGQ2Pop` = gq_pop)], 
             final5[year == 2023, .(name, `2023ACSHHsize` = hhsize, `2023ACSGQ2Pop` = gq_pop)], 
             by = "name")
tmp[, name := factor(name, counties)]
tmp <- tmp[order(counties)]
setcolorder(tmp, c(1, 2, 4, 3, 5))
