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
        .[age_lookup, age:=age_label, on=.(age1 >= age_min, age2 <= age_max)]  # Lookup standard age categories
    
    dtx[!is.na(age), 
              .(sum_estimate=sum(estimate), 
                sum_moe=tidycensus::moe_sum(moe, estimate, na.rm=TRUE)),         # Summarize
              by=c("year", "age")]
} 

householder <- gq <- tot_pop <- NULL
for(yr in c(2020, 2023)){
    householder <- rbind(householder, 
                         tidycensus::get_acs(state = "Washington", geography = "state", 
                           year = yr, survey = "acs5", 
                           table = "B25007", cache_table = TRUE)  %>% 
        psrccensus:::label_acs_variables("B25007", yr, "acs5") %>% 
            setDT() %>% .[, year := yr]
    )
    gq <- rbind(gq,
                    tidycensus::get_acs(state = "Washington", geography = "state", 
                                        year = yr, survey = "acs5", 
                                        table = "B26101", cache_table = TRUE)  %>% 
                        psrccensus:::label_acs_variables("BB26101", yr, "acs5") %>% setDT() %>%
                        .[, subvar:=as.integer(stringr::str_extract(variable,"(?<=_)\\d+$"))] %>%    
                        .[subvar %in% c(2:11, 35:44)] %>% .[, year := yr]
                    )
    tot_pop <- rbind(tot_pop, 
                         tidycensus::get_acs(state = "Washington", geography = "state", 
                                             year = yr, survey = "acs5", 
                                             table = "B01001", cache_table = TRUE)  %>% 
                             psrccensus:::label_acs_variables("B01001", yr, "acs5") %>% 
                             setDT() %>% .[, year := yr]
                    )
}

householder_sum <- sum_by_age(householder)
gq_sum <- sum_by_age(gq)
hh_pop <- sum_by_age(tot_pop)
hh_pop[gq_sum, sum_estimate := sum_estimate - i.sum_estimate, on = .(year, age)]
