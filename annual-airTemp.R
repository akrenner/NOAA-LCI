
## precipitation -- standardized plot
rm (list = ls())
setwd ("~/myDocs/amyfiles/NOAA-LCI/")


maO <- 31  # 7 days certainly not working, 14 days not enough either
# maO <- 1
qntl = c(0.9) #, 0.8) 
currentYear <- as.numeric (format (Sys.Date(),"%Y"))-1
currentCol <- c("red", "magenta")
SWMP <- TRUE
SWMP <- FALSE



source ("annualPlotFct.R")

if (SWMP){
  load ("~/tmp/LCI_noaa/cache/metDat.RData") # from windTrend.R -- SWMP
}else{
  # source ("noaaWeather.R")
  load ("~/tmp/LCI_noaa/cache/HomerAirport.RData") # from noaaWeather.R -- Airport
}


# plot (subset (hmr$totprcp, hmr$year < 2005), type = "l")
# plot (totprcp~datetimestamp, data = hmr, subset = year < 2006, type = "l")

## QCQA moved to windTrend.R


## adjust units
## change C to F
hmr$atempF <- hmr$atemp * 9/5 + 32

## aggregate data
tDay <- prepDF (varName = "atempF", dat = hmr, maO = maO, qntl = qntl)

## plot
# pdf ("~/tmp/LCI_noaa/media/sa-airTemp.pdf", width = 9, height = 6)
pdf (paste0 ("~/tmp/LCI_noaa/media/sa-airTemp-", ifelse (SWMP, "LE", "AP"), ".pdf"), width = 9, height = 6)

aPlot (tDay, "atempF" 
       #, ylab = paste0 ("air temperature [", expression (~degree~F), "]")
       , ylab = expression('air'~'temperature'~'['*degree~'F'*']')
       #, ylab = "air temperature [̊F]"
       , currentCol = currentCol, MA = TRUE)
# box()
## legend
cLegend ("topleft", inset = 0.05
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
