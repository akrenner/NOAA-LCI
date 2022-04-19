# CTD clalibration

## interpolate 4141 2016 calibration

ctd <- data.frame (year = c (2016, 2012, 2018)
                   , M = c (NA, 21.5887, 22.114)
                   , B = c(NA, -1.2306,-1.283)
)

na.approx (ctd$M, ctd$year)
na.approx (ctd$B, ctd$year)
