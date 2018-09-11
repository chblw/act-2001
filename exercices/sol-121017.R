
library(RCurl)

# Import dataset ----------------------------------------------------------

dataset <- read.csv(text=getURL("https://cdn.rawgit.com/actrisk/actrisk-data/b69662ae/flood_canada.csv"))
years <- 1909:2008

# a) ----------------------------------------------------------------------

count_data <- sapply(years, function(t) sum(dataset$Year == t))

# b) ----------------------------------------------------------------------

plot(years, cumsum(count_data), type = "l")
