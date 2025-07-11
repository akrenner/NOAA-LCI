## nutrient data is sampled only about monthly; not working within existing
## plotting framework. Consider custom approach. Worth the pain=?

# setwd ("~/myDocs/amyfiles/NOAA-LCI/")
if (!exists ("quarterly")){
  rm (list=ls())
  quarterly <- TRUE
}



# load ("~/tmp/LCI_noaa/cache/SeldTemp.RData")  ## from SeldoviaTemp.R
maO <- 31*2  # 7 days certainly not working, 14 days not enough either
qntl=c(0.9)
pMA <- TRUE
currentYear <- as.numeric (format (Sys.Date(), "%Y"))-1
currentCol <- c ("green", "lightgreen", "hotpink")



source ("annualPlotFct.R")
nut <- getSWMP ("kachdnut") # nutrients
nut$dateTime <- time_vec (nut$datetimestamp, station_code="kachdnut"
                          , tz_only=FALSE)
nut <- addTimehelpers (nut)
# nut$jday <- nut$month  # dirty trick, but may work?
# nut <- fixGap (nut)  ## a ood idea here?

## PO4, NH4, NO23, CHLA
vars <- c("PO4F", "NH4F"
          # , "NO2F", "NO3F"
          , "NO23F", "CHLA_N")


pdf ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-Nutrients.pdf")
par (mfrow=c(2,2), mar=c(3,4,3,1))

for (i in seq_along (vars)){
  sL <- prepDF (dat=nut, varName=tolower (vars [i]), maO=maO
                , currentYear=currentYear, qntl=qntl)
  sL <- subset (sL, !is.na (sL [,2]))
  if (0){
    aPlot (sL, tolower (vars [i]), currentCol=currentCol
           , ylab=paste (vars [i], "concentration")
           , main="")

    cLegend ("topleft", inset=0.05
             , mRange=c (min (nut$year), currentYear -1)
             , currentYear=currentYear
             , cYcol=currentCol # "blue"
             , qntl=qntl [1]
    )
    axis (1, at=366, labels=FALSE)
  }
  # plot (as.formula (paste0 ("MA_", tolower (vars [i]), "~jday"))
  #       , sL, type="l")
  plot (as.formula (paste0 (tolower (vars [i]), "~month"))
        , nut, pch=19
        # , col=ifelse (year == currentYear, "red", "black")
        , col=year
        )
  }
dev.off()


cat ("Finished nutrients\n")
# EOF
