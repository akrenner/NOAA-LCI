
## graphics of nutrients for EVOS report
rm (list=ls())

## 1. link to physical oceanography data.

## -- seasonality
## -- add to signature data: time series?

nut <- read.csv ("~/GISdata/LCI/CookInletKachemakBay_Nutrients_2021.csv")

## cleanup
nut$Date <- as.Date(nut$Date)
# nut$Station <- ifelse (nut$Station == "9_6", "AlongBay_6", nut$Station)
nut$Station <- ifelse (nut$Station == "4_4", "AlongBay_3", nut$Station)
nut$Station <- factor (nut$Station)
nut <- nut [with (nut, order (Station, Date, Depth_m, Replicate)),]


## deal with 2 samples -- take lower sample
sampleID <- paste (nut$Station, nut$Date, nut$Depth_m)

## exploratory analysis
summary (nut$Station)
summary (nut$Depth_m)
summary (nut$Depth_m > 10)
summary (nut$Date)

dir.create("~/tmp/LCI_noaa/media/EVOS/", recursive=TRUE, showWarnings=FALSE)
pdf ("~/tmp/LCI_noaa/media/EVOS/depthHisto.pdf")
hist (subset (nut, Depth_m > 10) $Depth_m, main="Depth distribution of deep samples"
      , xlab="depth [m]")
dev.off()

summary (subset (nut$Station, nut$Depth_m > 10))
summary (subset (nut$Station, nut$Depth_m < 10))
## AlongBay_10, AlongBay_3, AlongBay_6 are the only stations with > 4 samples at surface




## seasonality of PO4, and NOx, and CHLa
require ("RColorBrewer")

stations <- levels (nut$Station)[summary (nut$Station) > 10][c(2,3,1)]
nutL <- names (nut)[c(5,7,9,11)]
nutE <- c (expression (NH[3]), expression (NO[x]), expression (PO[4]), "Si")

## add chlorophyll?
if (1){
  nutL <- c(nutL, names (nut)[13])
  nutE <- c (nutE, expression (Chl~a))
}

pdf ("~/tmp/LCI_noaa/media/EVOS/nutrientsSeason.pdf")
## one color per station (3)
## one panel per nutrient (4)
par (mfrow=c(2,2), mar=c(3, 5, 4, 1))

for (i in 1:length (nutL)){
  nut$x <- nut [,which (names (nut)==nutL[i])]

  # summary (with (nut, factor (paste (Station, Date, Depth_m))))
  nutA <- aggregate (x~Station+Date+Depth_m, nut, FUN=mean, na.rm=TRUE) ## use mean, min, max ?? XXX
  # nut <- aggregate (x~Station+Date+Depth_m, nut, FUN=max, na.rm=TRUE) ## use mean, min, max ?? XXX
  # nut <- aggregate (x~Station+Date+Depth_m, nut, FUN=min, na.rm=TRUE) ## use mean, min, max ?? XXX
  # nutA <- nut
  nutA <- nutA [with (nutA, order (Date)),]
  nutS <- subset (nutA, Depth_m < 10)
  nutD <- subset (nutA, Depth_m > 10)

  plot (x~Date, nut, type="n", main=nutE[i], xlab="", ylab="")
  if (i %% 2 == 1){
    title (ylab=expression (mg~l^-1))
}
  for (j in 1:length (stations)){
    lines (x~Date, nutS, subset=Station==stations [j], lwd=3, col=brewer.pal(length (stations), "Set2")[j])
    lines (x~Date, nutD, subset=Station==stations [j], lwd=3, col=brewer.pal(length (stations), "Set2")[j]
           , lty="dashed")
    if (i==1){
      legend ("topright", lwd=3, col=c(brewer.pal (length (stations), "Set2"), rep ("gray",2))
              , legend=c(stations, "surface", "50 m")
              , lty=c(rep ("solid", length (stations)+1), "dashed")
              , bty="n", ncol=2)
    }
  }
}
dev.off()



## QAQC
nut [which.max (nut$NH3_mg.L),]


## compare surface and deep


