## test-bed for CTD section plotting
## troubleshoot issues in CTD-sections.R and CTDwall.R


rm (list = ls()); load ("~/tmp/LCI_noaa/cache/ctdwall0.RData")  # from CTDsections.R
require ("oce")

################ define variables and their ranges #########################
# now in CTDsectionFcts.R --? no, need oRange in here and rest is dependend on it

oVars <- c ("temperature"
            , "salinity" #, "sigmaTheta"
            , "turbidity"
            # , "logTurbidity"
            , "fluorescence" #, "chlorophyll"
            , "PAR"
            #, "logPAR"
            #, "logFluorescence"
            , "O2perc"
)

## see https://github.com/jlmelville/vizier
# install.packages("remotes")
# remotes::install_github("jlmelville/vizier")
require ('vizier')
oCol <- list (
  turbo # oceColorsTemperature
  , oceColorsSalinity #, oceColorsDensity
  , oceColorsTurbidity
  , oceColorsChlorophyll
  , oceColorsPAR  #, turbo #
  , oceColorsOxygen
)
oCol <- function (i, N = NULL){
  #oCol <- list (
  outC <- list (
    # turbo (N)
    oceColorsTemperature (N)
    , oceColorsSalinity (N) #, oceColorsDensity
    , oceColorsTurbidity (N)
    , oceColorsChlorophyll (N)
    , oceColorsPAR (N)  #, turbo #
    , oceColorsOxygen (N)
  )
  outC [[i]]
}

oRange <- t (sapply (c ("Temperature_ITS90_DegC", "Salinity_PSU" #, "Density_sigma.theta.kg.m.3"
                        , "turbidity"
                        # , "logTurbidity"
                        , "Fluorescence_mg_m3" #, "PAR.Irradiance"
                        , "logPAR"
                        , "O2perc")
                     , FUN = function(vn){range (poAll [,which (names (poAll) == vn)], na.rm = TRUE)
                       #  quantile (poAll [,which (names (poAll) == vn)], na.rm = TRUE, c(0.01, 0.99), type = 8)
                     }))
if (length (oVars) != length (oCol)){stop ("fix the code above: one color for each variable")}
###########################################################################



source ("CTDsectionFcts.R")  # get pSec to plot sections




## tests
#if (1){
summary (factor (subset (poAll, year == 2018)$survey))
# xC <- subset (poAll, survey == "2018-07-26")
xC <- subset (poAll, survey == "2018-08-23")
#  summary (factor (xC$Transect))
xC <- subset (poAll, (Transect == "9"))
#  summary (xC$Transect)
# xC <- subset (poAll, (Transect == "3")&(DateISO == "2012-03-14"))
xCo <- sectionize (xC)

# xCg <- sectionGrid(xCo, p=standardDepths(3), method = "boxcar", trim = TRUE)

#  plot (xCo)
#  pSec (xCo, 1, zcol = oCol [[1]])
pSec (xCo, 1, zcol = oceColorsTemperature)


pSec (xCo, 1, zcol = turbo (20), custcont = c(10, 11, 11.1))
pSec (xCo, 1, zcol = turbo (20), custcont = c(10, 11, 11.1))

pSec (xCo, 1, zcol = oce.colorsTemperature)
pSec (xCo, 1, zcol = oceColorsTemperature (5))

rm (xC, xCo)
#}




## QAQC
f2n <- function (x){as.numeric (as.character (x))}
pdf ("~/tmp/LCI_noaa/media/CTDtests/turb-attenuation.pdf")
plot (turbidity~year, poAll)
plot (attenuation~year, poAll)
plot (turbidity~isoTime, poAll, col = poAll$year)
abline (h = 0, col = "blue")
plot (attenuation~isoTime, poAll, col = poAll$year)
abline (h = 0, col = "blue")
plot (turbidity~Depth.saltwater..m., poAll, col = poAll$year, pch = 19)
#      , pch = ifelse (f2n (poAll$year) < 2017, 19, 2))
# legend ("topright", legend = levels (poAll$year), col = levels (poAll$year)
#         , pch = ifelse (f2n (levels (poAll$year)) < 2017, 19, 2))

plot (attenuation~Depth.saltwater..m., poAll, col = poAll$year, pch = 19)
#      , pch = ifelse (f2n (poAll$year) < 2017, 19,1))
# legend ("topright", legend = levels (poAll$year), col = levels (poAll$year)
#         , pch = ifelse (f2n (levels (poAll$year)) < 2017, 19, 1))

par (mfrow = c(2,1))
x <- hist (poAll$attenuation)
hist (poAll$turbidity) #, breaks = x$breaks)
dev.off()

# plot (attenuation~turbidity, poAll)
#
# 4141: attenuation
# 5028: turbidity
#
# table (poAll$CTD.serial, !is.na (poAll$attenuation))
# table (poAll$CTD.serial, !is.na (poAll$turbidity))

png ("~/tmp/LCI_noaa/media/CTDtests/PAR-attenuation.png", width = 900, height = 900, res = 120)
plot (attenuation~PAR.Irradiance, poAll, col = year
      , pch = ifelse (f2n (poAll$year) < 2017, 19, 2))
# plot (turbidity~PAR.Irradiance, poAll, col = year
#       , pch = ifelse (f2n (poAll$year) < 2017, 19, 2))
abline (h = 0, col = "blue")
legend ("topright", legend = levels (poAll$year), col = levels (poAll$year)
        , pch = ifelse (f2n (levels (poAll$year)) < 2017, 19, 2))
# plot (Fluorescence_mg_m3~PAR.Irradiance, poAll)
dev.off()
