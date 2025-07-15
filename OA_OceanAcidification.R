## Kachemak Bay Ocean Acidificationn data
rm (list = ls())
oa1 <- read.csv("~/GISdata/LCI/Ocean_Acidification_OA/OA2015-2018.csv")
oa2 <- read.csv("~/GISdata/LCI/Ocean_Acidification_OA/OA2017-2021.csv")


summary (as.Date (oa1$Date))
summary (as.Date (oa2$sample.date))


oa <- oa1 [, c(1, 2, 6, 7, 8, 10, 11, 12, 13)]
oT <- oa2[, c(4, 3, 6, 5, 7
  , 9 ## is 9 TCO2 = TA??
  , 10, 14, 16)]
names (oT) <- names (oa)
oa <- rbind (oa, oT); rm (oT)

cor (oa [, c(4:ncol(oa))], use = "pairwise.complete.obs")




oa$Date <- as.Date(oa$Date)
oa$jday <- format (oa$Date, format = "%j")
oa$Station <- substr(oa$X.Sample_ID, 1, 5)
oa$Station <- factor (gsub ("_$", "", oa$Station))
summary (oa$Station)

## omega-A and pH: 0.89
## pCO2 and omegaA: -0.56
## ph and pCO2: -0.79
## TA and Salinity: 0.66

## start with oa2$omega Arag and pH

plot (pH..total. ~ Date, oa)
plot (pH..total. ~ jday, oa)

plot (Omega_Arag ~ Date, oa)
plot (Omega_Arag ~ jday, oa)

plot (Omega_Arag ~ Sample.Temp..deg_C., oa)
plot (log (Omega_Arag) ~ log(pCO2..uatm.), oa)


## only oa2 has station field
oa2$station <- factor (oa2$description)

oVar <- 7:16
oa2$jday <- as.numeric (format (as.Date (oa2$sample.date), format = "%j"))
oa2$year <- as.numeric (format (as.Date (oa2$sample.date), format = "%Y"))
# oa2 <- oa2 [order (oa2$station, oa2$year,  oa2$jday),]
oa2 <- oa2 [order (oa2$station, oa2$jday, oa2$year), ]

pdf ("~/tmp/LCI_noaa/media/EVOS/oa.pdf")
for (i in oVar) {
  plot (oa2$jday, oa2 [, i], type = "n", main = paste (names (oa2)[i], "2017-2021")
    , xlab = "day of year", ylab = "")
  for (j in 1:length (levels (oa2$station))) {
    sD <- subset (oa2, station == levels (oa2$station)[j])
    lines (sD$jday, sD[, i], col = j)
  }
}
dev.off()


oa2$year <- factor (format (as.Date (oa2$sample.date), format = "%Y"))
pdf ("~/tmp/LCI_noaa/media/EVOS/oa-year.pdf")
for (i in oVar) {
  plot (oa2$jday, oa2 [, i], type = "n", main = paste (names (oa2)[i], "2017-2021")
    , xlab = "day of year", ylab = "")
  for (j in 1:length (levels (oa2$year))) {
    sD <- subset (oa2, year == levels (oa2$year)[j])
    lines (sD$jday, sD[, i], col = j)
  }
}
dev.off()
