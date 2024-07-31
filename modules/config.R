
## Read data
#variables.lu <- c("Population", "Employment")
variables.lu <- unique(alldata[category %in%  c("Population", "Employment"), 
                               .(category, variable, variable_name = `variable`)])
variables.lu <- variables.lu[category != "Population" | variable == "Total Population"]

#values.lu <- read.dt(dbtable.values, 'table_name')
#values.lu<- values.lu[order(value_order)]

vars.cat <- unique(variables.lu$category)

# master list
dtype.choice <- c("Total" = "total",
                  "Delta" = "delta",
                  "Percent delta" = "percent_delta",
                  "5y moving average" = "moving_average"
                )
# xtab sublist: dimensions
#dtype.choice.xtab <- dtype.choice[c(1:2, 5, 4, 7)]
#col.headers <- c("sample_count", "estimate", "estMOE", "share", "MOE")

# xtab sublist: facts
#dtype.choice.xtab.facts <- dtype.choice[c(9, 8, 7)]
#col.headers.facts <-  c("median", "MOE", "sample_count")


# stab sublist
# change to named indices
#dtype.choice.stab <- dtype.choice[c(1:2, 6, 3,7)]
dtype.choice.stab.vis <- dtype.choice

#min_float <- 0
#max_float <- 200
#hist_breaks<- c(0,1,3,5,10,20,30,45,60,180)
#hist_breaks_labels<-c('0 to 1', '1 to 3', '3 to 5', '5 to 10', '10 to 20', '20 to 30', '30 to 45', '45 to 60', '60 to 180')
#hist_breaks_num_trips<-c(-.01,0,2,4,6,8,10,12,14,16,18,20,100)
#hist_breaks_num_trips_labels<-c('0', '1-2', '3-4', '5-6', '7-8', '9-10', '11-12', '13-14', '14-16', '17-18', '19-20', '20-100')

# photo list
psrc_photos <- c('bellevuetransitcenter.jpg',
                 'canyon_road.png',
                 'linkbeaconhillstn.jpg',
                 'mtrainierparadisehikers.jpeg',
                 'redmond-housing_0.jpg',
                 'st_northgate_trim.png',
                 'street-intersection.jpeg',
                 'transitorienteddevelopment.jpeg')
