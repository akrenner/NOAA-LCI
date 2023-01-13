
## graphics of nutrients for EVOS report
rm (list=ls())

## 1. link to physical oceanography data.

## -- seasonality
## -- add to signature data: time series?

nut <- read.csv ("~/GISdata/LCI/CookInletKachemakBay_Nutrients_2021.csv")

## cleanup
nut$Date <- as.Date(nut$Date)
nut$Station <- ifelse (nut$Station == "9_6", "AlongBay_6", nut$Station)
nut$Station <- ifelse (nut$Station == "4_4", "AlongBay_3", nut$Station)
nut$Station <- factor (nut$Station)



## exploratory analysis
summary (nut$Station)
summary (nut$Depth_m)
# hist (nut$Depth_m)
summary (nut$Depth_m > 10)
summary (nut$Date)


summary (subset (nut$Station, nut$Depth_m > 10))
summary (subset (nut$Station, nut$Depth_m < 10))
## AlongBay_10, AlongBay_3, AlongBay_6 are the only stations with > 4 samples at surface




## seasonality of PO4, and NOx, and CHLa
## surface
nutS <- subset (nut, Depth_m < 10)

pdf ("~/tmp/nutrientsSeason.pdf")
## one color per station (3)
## one panel per nutrient (3)


par (mfrow=c(2,2))
stations <- levels (nutS$Station)[summary (nutS$Station) > 10]
for (i in 1:length (stations)){
  plot (NOx_mg.L~Date, nutS, subset=Station==stations [i], type="l")
}
dev.off()


## compare surface and deep


