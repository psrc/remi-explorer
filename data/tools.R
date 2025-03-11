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

compute.households <- function(dt, hhpopdt, acs, gqest, base.year = 2020, target.year = NULL, acs.year = 2020){
    # implements Exhibit 19 from https://deptofcommerce.app.box.com/s/chqj8wk1esnnranyb3ewzgd4w0e5ve3a
    if(is.null(target.year)) target.year <- max(as.integer(dt$year))
    # create a table of age groups that should be summed to the desired age categories
    agesdt <- unique(acs[, .(age)])
    agesdt <- cbind(agesdt, agesdt[, tstrsplit(age, "-")])
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
    # Steps F, G
    hhpop <- merge(dt[!is.na(age), .(value = sum(value)), by = .(Region, age, Gender, year)], 
                   acs[, .(age, Gender, ratio_hhpop_pop = HHpop/Pop)], 
                   by = c("age", "Gender"))
    hhpop <- merge(hhpop, acs[Gender == "Total", 
                              .(age, ratio_hher_hhpop = householders / HHpop)],
                   by = "age")
                   
    # Steps H, I
    hhpop[, hhpop := value * ratio_hhpop_pop][, hhmod := hhpop * ratio_hher_hhpop]
    # Steps J, K
    hhsize <- hhpop[, .(hhpop = sum(hhpop), hhsizemod = sum(hhpop)/sum(hhmod)), by = .(Region, year)]
    # Step L
    hhsize[hhsize[year == base.year], hhsize_ratio := hhsizemod / i.hhsizemod, on = "Region"]

    # get the base HH size
    gq <- gqest[year == data.year]
    setnames(gq, "name", "Region")
    gq[, hhsize := (Pop - GQ)/HH][Region != "Region", Region := paste(Region, "County")]
    # Step M
    hhsize[gq, hhsize := i.hhsize * hhsize_ratio, on = "Region"]
    hhpopdt[hhsize, `:=`(value = value / i.hhsize, `Detailed Measure` = "Households"), on = c("Region", "year")]
    return(hhpopdt)
}