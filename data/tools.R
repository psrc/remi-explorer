compute.hhpop <- function(dt, gqest, year = 2024){
    popcol <- paste0(year, "Pop")
    gqcol <- paste0(year, "GQpop")
    setnames(gqest, c(popcol, gqcol), c("pop", "gq"))
    gqest <- rbind(gqest[, .(Region = County, pop, gq)], 
                   gqest[, .(Region = "Region", pop = sum(pop), gq = sum(gq))])
    gqest[, ratio := gq/pop][Region != "Region", Region := paste(Region, "County")]
    dt[gqest, `:=`(value = value - value*i.ratio, `Detailed Measure` = "Household Population"), on = "Region"]
    return(dt)   
}
