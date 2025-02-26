library(psrccensus)
library(stringr)
library(dplyr)
library(data.table)
library(magrittr)

age_categories <- c("Under 15 years", "15 to 24 years", 
                    "25 to 34 years", "35 to 44 years", "45 to 54 years", 
                    "55 to 64 years", "65 to 74 years", "75 to 84 years", "85 years and over")


safe_rgx_extract <- function(var, rgx){
    val <- if_else(str_detect(var, rgx), str_replace(str_extract(var, rgx), rgx, "\\1"), "")
}

# Create an age category lookup table
age_lookup <- suppressWarnings(data.table(
    age_label=age_categories,
    age_min=coalesce(as.integer(safe_rgx_extract(age_categories, "(^\\d+)")), 0),
    age_max=coalesce(as.integer(safe_rgx_extract(age_categories, "(\\d+) years$")), 120)
))

# assigns min and max of the corresponding age range
assign_age_range <- function(dt){
    dt[grepl("^Under ", age),  `:=`(age1=as.integer(safe_rgx_extract(age, "(?:Under )(\\d+)")) -1,
                                     age2=as.integer(safe_rgx_extract(age, "(?:Under )(\\d+)")) -1)]
    dt[!grepl("^Under ", age), `:=`(age1=as.integer(safe_rgx_extract(age, "(\\d+)")),
                                     age2=as.integer(safe_rgx_extract(age, "(\\d+)(?: years)?(?: and over)?$")))] 
}

# Helper function
sum_by_age <- function(dt){
    dtx <- setDT(dt) %>% .[grepl('years', label)] %>%
        .[, `:=`(age =str_extract(label, "(?<=!!)[\\w ]+$"))]                    # Separate age from label components]
    assign_age_range(dtx)
    dtx[age_lookup, age:=age_label, on=.(age1 >= age_min, age2 <= age_max)]   # Recode using standard age categories
   
    dtx[!is.na(age), 
              .(sum_estimate=sum(estimate), 
                sum_moe=tidycensus::moe_sum(moe, estimate, na.rm=TRUE)),         # Summarize
              by=c("year", "age")]
} 

householder <- hh_pop <- tot_pop <- NULL
for(yr in c(2020, 2023)){
    householder <- rbind(householder, 
                         tidycensus::get_acs(state = "Washington", geography = "state", 
                           year = yr, survey = "acs5", 
                           table = "B25007", cache_table = TRUE)  %>% 
        psrccensus:::label_acs_variables("B25007", yr, "acs5") %>% 
            setDT() %>% .[, year := yr]
    )
    hh_pop <- rbind(hh_pop,
                    tidycensus::get_acs(state = "Washington", geography = "state", 
                                        year = yr, survey = "acs5", 
                                        table = "B26101", cache_table = TRUE)  %>% 
                        psrccensus:::label_acs_variables("BB26101", yr, "acs5") %>% setDT() %>%
                        .[, subvar:=as.integer(stringr::str_extract(variable,"(?<=_)\\d+$"))] %>%    
                        .[subvar %in% c(2:11, 35:44)] %>% .[, year := yr]
                    )
}
# sum for selected age groups
householder_sum <- sum_by_age(householder)
hh_pop[between(subvar, 35, 44), estimate:=estimate * -1]        # HH pop is total - GQ
hh_pop_sum <- sum_by_age(hh_pop)
tot_pop_sum <- sum_by_age(hh_pop[estimate > 0])

# assemble final table
output_table <- merge(merge(householder_sum[, .(year, age, householders = sum_estimate)],
                            hh_pop_sum[, .(year, age, HHpop = sum_estimate)],
                                by = c("year", "age"), all = TRUE),
                      tot_pop_sum[, .(year, age, Pop = sum_estimate)],
                            by = c("year", "age"), all = TRUE
                        )
output_table[is.na(householders), householders := 0]

# re-write age groups as intervals
assign_age_range(output_table)
output_table[age1 == min(age1), age1 := 0]
output_table[, age := paste(age1, age2, sep = "-")]
output_table[age2 == max(age2), age:= paste0(age1, "+")]
output_table <- output_table[order(year, age1)][, `:=`(age1 = NULL, age2 = NULL)]

# export
fwrite(output_table, file = "acs_hhpop_by_age.csv")
