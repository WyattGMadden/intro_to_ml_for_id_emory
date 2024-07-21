#install.packages(tsiR)
library(tsiR)
lon <- twentymeas[["London"]]

cases <- read.csv("cases_urban.csv")
births <- read.csv("births_urban.csv")

mea <- read.csv("measlesUKUS.csv")

mea$births <- mea$rec
mea$time <- mea$decimalYear
mea$city <- mea$loc
mea_uk <- mea[mea$country == "UK",]

measles <- mea_uk[, c("time", "cases", "births", "pop", "city", "lat", "lon")]
measles$city <- paste(toupper(substring(measles$city, 1, 1)), tolower(substring(measles$city, 2)), sep = "")

write.csv(measles, "measles.csv", row.names = FALSE)
