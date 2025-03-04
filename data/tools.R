extract.gq <- function(gqdt, year){
    #popcol <- paste0(year, "Pop")
    #gqcol <- paste0(year, "GQpop")
    #hhcol <- paste0(year, "HH")
    ## rename columns
    #setnames(gqdt, c(popcol, gqcol, hhcol), c("pop", "gq", "hh"))
    # add row for the Region 
    return(rbind(gqdt[, .(Region = County, pop, gq, hh)], 
                 gqdt[, .(Region = "Region", pop = sum(pop), gq = sum(gq), hh = sum(hh))])[Region != "Region", Region := paste(Region, "County")])
}

compute.hhpop <- function(dt, gqest, yr = 2020){
    gq <- gqest[year == yr]
    setnames(gq, "name", "Region")
    gq[, ratio := GQ/Pop][Region != "Region", Region := paste(Region, "County")]
    dt[gq, `:=`(value = value - value*i.ratio, `Detailed Measure` = "Household Population"), on = "Region"]
    return(dt)   
}

compute.households <- function(dt, acs, gqest, template, base.year = 2022, target.year = NULL){
    if(is.null(target.year)) target.year <- max(as.integer(dt$year))
    # create a table of age groups that should be summed to the desired age categories
    agesdt <- cbind(acs[, age], acs[, tstrsplit(age, "-")])
    colnames(agesdt) <- c("age", "start_age", "end_age")
    agesdt[start_age == "85+", `:=`(start_age = 85, end_age = 200)][
        , `:=`(start_age = as.integer(start_age), end_age = as.integer(end_age))]
    
    start <- mapply(seq, agesdt$start_age, agesdt$end_age, MoreArgs = list(by = 5))
    age.groups <- sapply(start, function(x) paste(x, x+4, sep = "-"))
    names(age.groups) <- agesdt$age
    age.groups[["85+"]] <- "85+"
    age.categories <- NULL
    for(a in names(age.groups))
        age.categories <- rbind(age.categories, data.table(Age = age.groups[[a]], age = a))
    
    # assign the broader age groups to dt                        
    dt[, Age := gsub("Ages ", "", Age)]
    dt[age.categories, age := i.age, on = "Age"]
    hhpop <- merge(dt[!is.na(age), .(value = sum(value)), by = .(Region, age, year)], 
                   acs[, .(age, ratio_hhpop_pop = HHpop/Pop, ratio_hher_hhpop = householders / HHpop)], 
                   by = "age")
    hhpop[, hhpop := value * ratio_hhpop_pop][, hhmod := hhpop * ratio_hher_hhpop]
    hhsize <- hhpop[, .(hhpop, hhsizemod = sum(hhpop)/sum(hhmod)), by = .(Region, year)]
    hhsize[hhsize[year == base.year], hhsize_ratio := hhsizemod / i.hhsizemod, on = "Region"]

    # get the base HH size
    gq <- gqest[year == 2020]
    setnames(gq, "name", "Region")
    gq[, hhsize := (Pop - GQ)/HH]
    hhsize[gq, hhsize := i.hhsize * hhsize_ratio, on = "Region"]
    template[hhsize, `:=`(value = i.hhpop / i.hhsize, `Detailed Measure` = "Households"), on = c("Region", "year")]
    return(template)
}