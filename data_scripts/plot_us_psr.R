library(wpp2024)

data(popAge1dt, popprojAge1dt)

# extract US age-specific pop data
us <- rbind(popAge1dt[country_code == 840, .(year, age, pop)], 
            popprojAge1dt[country_code == 840, .(year = as.integer(year), age, pop)])[year < 2051]

p2064 <- us[age > 19 & age < 65, .(p2064 = sum(pop)), by = .(year)] # sum pop of 20-64 years old
p65p <- us[age > 64, .(p65p = sum(pop)), by = .(year)] # sum pop of 65+ 
psr <- merge(p2064, p65p, by = "year")[, psr := p2064/p65p] # merge and compute the potential support ratio

with(psr[year > 1999], plot(year, psr, type = "b", main = "US national PSR"))
