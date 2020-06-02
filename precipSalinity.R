## precipSalinity.R
#
# call precipitation and salinity scripts to make combined 
# plot (3 panels)


# pdf ("~/tmp/LCI_noaa/media/SaltyRain.pdf", height = 8, width = 6)
# par (mfrow = c(3,1)
#      , mar = c (4,4,1,1)
# )
pdf ("~/tmp/LCI_noaa/media/SaltyRain.pdf", height = 10, width = 6)
par (mfrow = c(4,1)
     , mar = c (3.5,4,0.5,1)
)
source ("airTemp.R")
source ("rainy.R")
source ("salinityAnnual.R")
dev.off()

# EOF