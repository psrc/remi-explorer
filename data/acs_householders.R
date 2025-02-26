library(psrccensus)
library(stringr)
library(dplyr)
library(data.table)
library(magrittr)

age_categories <- c("under 5 years", "5 to 9 years", "10 to 14 years", "15 to 24 years", 
                    "25 to 34 years", "35 to 44 years", "45 to 54 years", "55 to 59 years", 
                    "60 to 64 years", "65 to 74 years", "75 to 84 years", "85 years and over")

safe_rgx_extract <- function(var, rgx){
    val <- if_else(str_detect(var, rgx), str_replace(str_extract(var, rgx), rgx, "\\1"), "")
}

# Create an age category lookup table
age_lookup <- suppressWarnings(data.table(
    age_label=age_categories,
    age_min=coalesce(as.integer(safe_rgx_extract(age_categories, "(^\\d+)")), 0),
    age_max=coalesce(as.integer(safe_rgx_extract(age_categories, "(\\d+) years$")), 120)
))

# Helper function
sum_by_age <- function(dt){
    dtx <- setDT(dt) %>% .[grepl('years', label)] %>%
        .[, `:=`(age =str_extract(label, "(?<=!!)[\\w ]+$"))] %>%                  # Separate age from label components]  
        .[, `:=`(age1=as.integer(safe_rgx_extract(age, "(?:Under )?(\\d+)")),
                 age2=as.integer(safe_rgx_extract(age, "(\\d+)(?: years)?(?: and over)?$")))] %>% 
        .[age_lookup, age:=age_label, on=.(age1 >= age_min, age2 <= age_max)] %>%  # Lookup standard age categories
        setnames("name","county") 
    
    rs <- dtx[!is.na(age), 
              .(sum_estimate=sum(estimate), 
                sum_moe=tidycensus::moe_sum(moe, estimate, na.rm=TRUE)),         # Summarize
              by=c("year", "county", "age")]
} 

householder <- get_acs_recs(geography="county",                                # Householders by age
                            table.names="B25007", 
                            years=c(2020,2023), 
                            acs.type="acs5") %>% sum_by_age()

hh_pop      <- get_acs_recs(geography="county",                                # All household population by age
                            table.names=c("B26101"),
                            years=c(2020,2023), 
                            acs.type="acs5")  %>% setDT() %>%
    .[, subvar:=as.integer(stringr::str_extract(variable,"(?<=_)\\d+$"))] %>%    
    .[between(subvar, 2, 44)]                                                    # Keep only total and topline GQ
hh_pop[between(subvar, 35, 44), estimate:=estimate * -1]                       # HH pop is total - GQ
hh_pop %<>% sum_by_age()

total_pop   <- get_acs_recs(geography="county",                                # Total population by age
                            table.names="B01001", 
                            years=c(2020,2023), 
                            acs.type="acs5") %>% sum_by_age()
