
## precipitation -- standardized plot
rm (list = ls())
setwd ("~/myDocs/amyfiles/NOAA-LCI/")


maO <- 31  # 7 days certainly not working, 14 days not enough either
# maO <- 1
qntl = c(0.9) #, 0.8)
currentYear <- as.numeric (format (Sys.Date(),"%Y"))-1
currentCol <- c("red", "magenta")
# currentCol <- "red"
SWMP <- TRUE
# SWMP <- FALSE



source ("annualPlotFct.R")

if (SWMP){
  load ("~/tmp/LCI_noaa/cache/metDat.RData") # from annual-wind.R -- SWMP
}else{
  source ("noaaWeather.R")
  load ("~/tmp/LCI_noaa/cache/HomerAirport.RData") # from noaaWeather.R -- Airport
}


# plot (subset (hmr$totprcp, hmr$year < 2005), type = "l")
# plot (totprcp~datetimestamp, data = hmr, subset = year < 2006, type = "l")

## QCQA moved to windTrend.R


## units: use both, C and F (F = 2nd axis on the right)


## aggregate data
tDay <- prepDF (varName = "atemp", dat = hmr, maO = maO, qntl = qntl)

#
## plot
# pdf ("~/tmp/LCI_noaa/media/sa-airTemp.pdf", width = 9, height = 6)
x <- mkdirs("~/tmp/LCI_noaa/media/StateOfTheBay/"); rm (x)
pdf (paste0 ("~/tmp/LCI_noaa/media/StateOfTheBay/sa-airTemp-", ifelse (SWMP, "LE", "AP"), ".pdf"), width = 9, height = 6)
par (mar = c(4,4,1,4))
aPlot (tDay, "atemp"
       , ylab = expression ('air'~'temperature '~'['*degree~'C'*']')
       , currentCol = currentCol, MA = TRUE)
# mtext ("Fahrenheit", side = 4, line = par ("mgp")[1])
# fAxis(with (tDay, c(max_atemp, min_atemp, cYMA_atemp, pYMA_atemp)))
# for (i in 1:length (levels (factor (hmr$year)))){
#   lines (atemp~jday, subset (hmr, year == levels (factor (hmr$year))[i]))
# }
fAxis (c (-15, 15), mT = expression ('air'~'temperature '~'['*degree~'F'*']'))
box()
## legend
cLegend ("bottomright", inset = 0.05
         , currentYear = currentYear, mRange = c (min (hmr$year), currentYear-1)
         , cYcol = currentCol
         , title = paste (maO, "day moving average")
         , qntl = qntl
)
dev.off()








## find troubles
if (0){
if (1){
  yL <- levels (factor (hmr$year))
  pdf ("~/tmp/LCI_noaa/media/precipX.pdf", width = 9, height = 6)

  for (i in 1:length (yL)){
    tDay <- prepDF (varName = "totprcp", dat = subset (hmr, hmr$year != yL [i])  # but but year in Q
                    , maO = 31, qntl = c(0.5, 0.75)
    )
    with (tDay, annualPlot (MA_totprcp, maL1_totprcp, maU1_totprcp, pYMA_totprcp, jday
                            , currentCol = "blue", yCaption = "daily rainfall [mm]"))
    title (main = paste0 ("long-term (grey) = all years but", yL [i]))
    box()
  }
  dev.off()
}




## test == plot
longMean <- tDay$MA_totprcp
percL <- tDay$perL1_totprcp
percU <- tDay$perU1_totprcp
jday <- tDay$jday
current <- tDay$pY_totprcp
yCaption = ""
currentCol = "red"
perc2L = NA
perc2U = NA


## test -- prep DF
varName <- "totprcp"
dat <- hmr
sumFct <- function (x){mean (x, na.rm = TRUE)}
maO <- 31
currentYear <- 2019
qntl <- c(0.75)

}

cat ("Finished airTemp.R\n")
# EOF
