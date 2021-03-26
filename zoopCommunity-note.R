#!/usr/bin/env Rscript

## zooplankton comparison across years and seasons
## after many iterations, have this cleared-up for the publication/note on
## annual cycle and spring progression



rm (list = ls()); load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData") # from dataSetup.R
set.seed (8)
Require ("sp")
Require ("raster")
Require ("vegan")
dir.create ("~/tmp/LCI_noaa/media/2019-zoopCircle/", recursive = TRUE, showWarnings = FALSE)

PDF <- function (fN, ...){
  pdf (paste0 ("~/tmp/LCI_noaa/media/2019-zoopCircle/", fN, ".pdf"), ...)
}

### standardize survey effort and distribution!!
## pick out stations that were present in each year-season combination
year_season <- with (zooCenv@data, paste (Year, season, sep = "-"))
sttn <- with (zooCenv@data, paste (Transect, Station, sep = "-"))

summary (duplicated(zooCenv$SampleID_H))
summary (duplicated (zooCenv$SampleID))
## remove or leave-in 6 duplicated stations?

## year_season <- subset (year_season, year_season != "NA-NA")
## sttn <- subset (sttn, sttn != "-")
xT <- table (year_season, sttn)
print (t (xT))
stCount <- apply (xT, 2, FUN = function (x){sum (x > 0)})
print (sort (stCount, decreasing = TRUE))
print (length (levels (factor (year_season))))

## based on above, select all of Transect 9, 4, 6-5, 7-20, 3-13, and 3-4
# keepSt <- sttn %in% names (which (stCount > 16)) # 100 FALSE, 325 TRUE
keepSt <- sttn %in% names (which (stCount >= 10)) # 100 FALSE, 325 TRUE
if (1){                                          # remove stations sampled only rarely
    zooC <- subset (zooC, keepSt)
    zooCenv <- subset (zooCenv, keepSt)
    ## remove zero-species
    zS <- apply (zooC, 2, sum)
    zooC <- zooC [,which (zS > 0)]
    rm (zS)
}
rm (year_season, sttn, xT, stCount, keepSt)




# require (vegan)
nM <- metaMDS (zooC, distance = "bray", k = 2, try = 200, trymax = 500, parallel = nCPUs)  ## check! that k=2 from k=3 doesn't result in major change!!


save.image ("~/tmp/LCI_noaa/cache/zoopC1.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/zoopC1.RData")

## cluster

## k-means
# kM <- kmeans (zooC, centers = 4)

# load ("~/tmp/LCI_noaa/cache/tempAnomalyMonth.RData") # gets tempM
load ("~/tmp/LCI_noaa/cache/dailyTempAnomalies.RData") # gets tempDay from SeldoviaTemp.R
zooCenv$month <- as.numeric (strftime (zooCenv$timeStamp, format = "%m"))

tempDATE <- with (tempDay, paste (Day, month.abb [month], year-2000, sep = "-")) # somewhat dirty! clean up
tempMatch <- match (zooCenv$Date, tempDATE); rm (tempDATE)

zooCenv$TempAnom <- tempDay$days90 [tempMatch]
tempDay$warmCat <- with (tempDay, cut (days90    ## need to update days90 -- lots of NAs!
                                       , breaks = c(-100
                                                    , mean (days90, na.rm = TRUE)+c(-1,1)*
                                                      (sd (days90, na.rm = TRUE)/4)
                                                    , 100)
                                       , labels = c("cold", "neutral", "warm")))
zooCenv@data$warmCat <- tempDay$warmCat [tempMatch]

## apply warm/cold categories only to sampled stations -- update temp graphs accordingly
zooCenv@data$warmCatL <- cut (zooCenv$TempAnom
                      , breaks = c(-100
                                 , mean (zooCenv$TempAnom, na.rm = TRUE)+c(-1,1)*
                                   sd (zooCenv$TempAnom, na.rm = TRUE)/4
                      , 100)
                      , labels = c("cold", "neutral", "warm"))
zooCenv@data$Sal  <- tempDay$SalD365 [tempMatch]
# print (summary (zooCenv))
Require (sp)
# print (with (zooCenv@data, summary (Sal)))
zooCenv@data$SalCat <- with (zooCenv@data, cut (Sal
                                    , breaks = c(-10, mean (Sal, na.rm = TRUE)+c(-1,1)*sd (Sal, na.rm = TRUE)/4, 100)
                                    , labels = c("fresher", "average", "saltier")))
# zooCenv$SalCat <- tempDay$SalCat  [match (zooCenv$Date, tempDATE)]
zooCenv@data$SalAnom <- tempDay$SalAnom [tempMatch]
zooCenv$Temp <- tempDay$Temp [tempMatch] # really should be MA
rm (tempMatch)
print (names (zooCenv@data))



## replot SeldoviaTemp graph -- re-assigning temperatures
                                # seldovia temperature
PDF ("SeldTempAnomaly-recategorize", width = 12, height = 7)
# pdf ("~/tmp/LCI_noaa/media/2019/SeldTempAnomaly-recategorize.pdf", width = 12, height = 7)
  nArgs <- with (tempDay, ifelse ((month == 1)&(Day == 1)&(year %% 3 == 0), year, NA))
poDay <- with (tempDay, as.POSIXct (paste (year, month, Day, sep = "-")))
tempDay$AJ <- tempDay$days90 - mean (zooCenv$TempAnom) # set zero to mean of samples
tempDay$AJ <- tempDay$days90

nRange <- sd (tempDay$AJ)/4

barplot (tempDay$AJ~poDay
       , space = 0
       , col = ifelse (tempDay$AJ < (mean (tempDay$AJ)-nRange), "blue"
                     , ifelse (tempDay$AJ < (mean (tempDay$AJ)+nRange)
                             , "gray", "red"))
##       , col = ifelse (tempDay$AJ < 0, "blue", "red")
       , border = NA
       , xlab = "time"
       , ylab = "90 day moving average temperature anomaly [?C]"
       , axes = FALSE
       , names.arg = "" #nArgs
        # , ylim = c(-2.6, 2.5)
         )
axis (2)
axis (1, at = which (!is.na (nArgs)), labels = FALSE) # subset (nArgs, !is.na (nArgs)))
axis (1, at = which (!is.na (nArgs)) + 182, tick = FALSE, labels = subset (nArgs, !is.na (nArgs)))
abline (v = which (with (tempDay, (month == 1) & (Day == 1)))  ## XXX missing data in 2005-Jan XXX
                , lty = "dashed", col = "gray")

box()
## mark zoop surveys
tempDay$Sample <- strftime (poDay, "%Y-%m-%d") %in% strftime (zooCenv$isoDate, "%Y-%m-%d")
# points (which (tempDay$Sample), -2.7, pch = 19)
axis (3, at = which (tempDay$Sample), label = FALSE)
abline (h = mean (tempDay$AJ) + c(1, -1)* nRange, col = "gray", lty = "dashed")
dev.off()
rm (tempDay, nArgs, poDay, nRange)







## surveys by warm/cold categorie
sampleWC <- aggregate (warmCat~season+Year, data = zooCenv, FUN = summary)
## table for methods
sampleWC <- cbind (sampleWC [,2:1], as.data.frame (sampleWC [,3]))
write.csv (sampleWC, file = "~/tmp/LCI_noaa/media/2019/zoopTempsamplingSummary.csv", row.names = FALSE)
rm (sampleWC)


nMScores <- scores (nM, "sites") #, choices = c(1,2,3))
spScores <- scores (nM, "species")
head (spScores [order (spScores [,1], spScores [,2]),])
tail (spScores [order (spScores [,1], spScores [,2]),])


## nMCol <- nMScores
## nMCol <- factor (zooCenv$Transect)
## nMCol <- factor (zooCenv$warmCat)
## nMCol <- factor (zooCenv$Year)
nMCol <- zooCenv$season



save.image ("~/tmp/LCI_noaa/cache/zoopCommVar.RData")
## rm (list = ls()); load ("~/tmp/LCI_noaa/cache/zoopCommVar.RData") #; require (vegan)




cH <- function (fac,colC, pts = nMScores [,1:2], hull = FALSE){
    X <- subset (pts, fac)
    envX <- subset (zooCenv, fac)
   # X <- subset (X, envX$Transect != 9)
    hpts <- chull (X)
    hpts <- c(hpts, hpts [1])
    Require (mixtools)
    ## collect points of ellipse and draw filled polygon
    if (hull){
        ## lines (X [hpts,], col = colC, lwd = 2)
        polygon (X [hpts,], col = adjustcolor (colC, alpha.f = 0.4))
    }else{
        elpt <- ellipse (mu = colMeans (X), sigma = cov (X), alpha = 0.05
           , col = colC, lwd = 2
           , draw = FALSE
           )
         polygon (elpt, col = adjustcolor (colC, alpha.f = 0.4))
    }
}


## plot T9 only, by month, using nMDS as calculated above -> find best seasonal cut-offs

TransN <- c ("9", "4", "3", "7", "6")

## color by N/S of transect
# 9:  S=1, N = 10 (spit)
# 4:  7 = N, S =
zooCenv$transO <- ifelse (zooCenv$Transect == "9" & zooCenv$Station == "10", "N", "")
zooCenv$transO <- ifelse (zooCenv$Transect == "9" & zooCenv$Station == "1", "S", zooCenv$transO)
zooCenv$transO <- ifelse (zooCenv$Transect == "4" & zooCenv$Station == "7", "N", zooCenv$transO)
zooCenv$transO <- ifelse (zooCenv$Transect == "4" & zooCenv$Station == "2", "S", zooCenv$transO)

## language for diversity -- what we did  (that first)
##
# 1st week of May: LCI survey
# Isabella: semester by the bay student. Needs qGIS help

Require ("colorspace")
## mCol  <- brewer.pal (12, "Spectral")
mCol <- rainbow_hcl (12)


PDF ("Zoop_nMDS-T9monthly")

## first subset?
T9 <- subset (nMScores, zooCenv$Transect == 9)
## recalc nMDS axes
## T9 <- scores (metaMDS (subset (zooC, zooCenv$Transect == 9), distance = "bray"
##                      , k = 2, try = 200, trymax = 500, parallel = 12)
##               , "sites")
T9env <- subset (zooCenv, zooCenv$Transect == 9)
T9env$month <- factor (T9env$month)
T9env$zoopSum <- rowSums (subset (zooC, zooCenv$Transect == 9))

## test densities -- something is wrong
## plot (volSample~month, T9env)           # volSamples not right!!
# plot (zoopSum~month, T9env)           # volSamples not right!!

par (mar = c(5,4,2,1) + 0.1)
plot (T9 [,1:2], col = adjustcolor (mCol, 0.7)[T9env$month]
      , pch = 19 # as.numeric (factor (T9env$Station))
      ##, cex = (10* (T9env$zoopSum/T9env$volSample) / max (T9env$zoopSum/T9env$volSample))+0.2
      #   , cex = 10 * (T9env$zoopSum / max (T9env$zoopSum))
      , main = paste0 ("Transect=", i)
)
for (i in 1:length (levels (T9env$month))){
  cH (T9env$month == levels (T9env$month)[i], mCol [i], pts = T9 [,1:2], hull = TRUE)
}
# legend ("topleft", legend = month.abb, pch = 19, col = mCol)
text (aggregate (T9~T9env$month, FUN = mean)[,2:3], month.abb)
dev.off()
# write.csv (data.frame (T9, T9env, subset (zooC, zooCenv$Transect == 9))
#          , file = "~/tmp/LCI_noaa/cache/zoopT9monthly.csv", row.names = FALSE)
rm (T9, T9env)



## model to dig into this further?
## progression of spring compared to spring temp

spDF <- data.frame (nMDS1 = nMScores [,1], zooCenv)
# spDF <- subset (spDF, month %in% 2:4 & Transect == "9")
spDF <- subset (spDF, month %in% 2:4)
spDF <- subset (spDF, month %in% 3)
sLM <- lm (nMDS1 ~ TempAnom, spDF)


# Require ("glmm")  ## better
Require ("MCMCglmm")
# sLM <- glmm (nMDS1 ~ month + Temp, random = nMDS1~ Match_Name, data = sLM, varcomps.names = "month")
sLM <- MCMCglmm (nMDS1 ~ month + Temp, random = nMDS1~ Match_Name, data = sLM)
# MCMCglmm (count ~ surveyYear + 1, random = ~ us (1+SurveyYear):location
#           data, mD, family = "poisson"
#           nitt = 2000, thin = 10, burnin = 5000, pr = TRUE, pl = FALSE)



## better: glmm, random factor = station
summary (sLM)


PDF ("SpringTemp")
plot (nMDS1~TempAnom, data = spDF)
# abline (sLM)
nData <- data.frame (TempAnom = seq (min (spDF$TempAnom, na.rm = TRUE), max (spDF$TempAnom, na.rm = TRUE)
                                     , length.out = 100))
sLMp <- predict (sLM, nData, se.fit = TRUE)
lines (nData$TempAnom, sLMp$fit, lwd = 2)
lines (nData$TempAnom, sLMp$fit - sLMp$se.fit, lty = "dashed")
lines (nData$TempAnom, sLMp$fit + sLMp$se.fit, lty = "dashed")
dev.off()
rm (T9, T9env, spDF)





mT9 <- aggregate (T9~month, T9env, FUN = mean)
mT9 <- aggregate (as.matrix (zooC)~month, data = zooCenv, subset = Transect == 9, FUN = mean)

Require ("factoextra")
if (0){
    PDF ("2019/Zoop_nSeasons")
    fviz_nbclust (mT9, kmeans, method = "gap_stat")
                                        # fviz_nbclust (mT9, hclust, method = "gap_stat")
    dev.off()
    kM <- kmeans (mT9, 4)
    cbind (month.abb, kM$cluster)
    cbind (month.abb, kmeans (mT9, 3)$cluster)
    rm (kM)
}

Require (magrittr) # for pipe!
## consider using a different distance to focus on composition rather than abundance
zoo.hc <- mT9 %>%
    scale() %>%
    dist (method = "manhattan") %>%
    hclust (method = "ward.D2")
# PDF ("Zoop_season-cluster", width = 6, height = 6)
pdf ("~/tmp/LCI_noaa/media/2019/Zoop_season-cluster.pdf", width = 6, height = 6)
par (las = 1)
fviz_dend(zoo.hc, k = 4, # Cut in four groups
          cex = 1, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE, # Add rectangle around groups
          lwd = 1.5,
          # cex = 2,
          main = "", ylab = "", axes = FALSE
          )
dev.off()
save (zoo.hc, file = paste0 (dirL [4], "zooClust.RData"))
rm (zoo.hc, mT9, T9, T9env, mCol)


## explore spatial independence
# PDF ("2019/zoop-all-cluster")
pdf (paste0 (dirL[3], "/2019/zoopall-cluster.pdf"))
# row.names(zooC) <- zooCenv$SampleID ## duplicated sample IDs! ok??
zoo.hc <- zooC %>%
  #  aggregate(x, list (zooCenv$Sampling.location)) %>%
  subset (subset = zooCenv$season == "summer") %>%
  scale() %>%
  dist (method = "manhattan") %>%
  hclust (method = "ward.D2")
zoo.hc$labels <- subset (zooCenv$SampleID_H, zooCenv$season == "summer")
fviz_dend (zoo.hc, k = 4, cex = 0.1, color_labels_by_k = TRUE, rect=TRUE)
# fviz_cluster()
dev.off()
rm (zoo.hc)



Seasonal <- function (month){
    month <- as.numeric (month)
    cut (month, breaks = c(-13,0,2,4,8,10, 25)
            , labels = c ("fall", "winter", "spring", "summer", "fall", "winter"))
}
cbind (month.abb, as.character (Seasonal (1:12)))


nMCol <- Seasonal (zooCenv$month)
Require ("RColorBrewer")
fCol <- brewer.pal (length (levels (nMCol)), "Dark2")
# fCol <- adjustcolor (fCol, alpha.f = 0.4)
## annual zooplankton cycle


PDF ("2019/Zoop_nMDS_seasonal-all")
# pdf ("~/tmp/LCI_noaa/media/2019/Zoop_nMDS_seasonal2.pdf")
# png ("~/tmp/LCI_noaa/media/2019/Zoop_nMDS_seasonal.png")
plot (nMScores [,1:2], type = "n")
## add convex hulls
for (i in 1:length (levels (nMCol))){
    cH (nMCol == levels (nMCol)[i], fCol [i], hull = TRUE)
}
points (nMScores [,1:2], col = fCol [as.numeric (nMCol)]
      , pch = ifelse (zooCenv$Transect == 9, 1, 19)
      # , cex = ifelse (zooCenv$Transect == 4, 2, 1)
        )
text (aggregate (nMScores [,1:2]~nMCol, FUN = mean)[,2:3], levels (nMCol))
## pSym <- c(25,1,17)
## legend ("bottomleft", legend = levels (factor (zooCenv$warmCat))
##       , pch = pSym [1:length (levels (zooCenv$warmCat))]
##       , bty = "n")
legend ("topleft", legend = levels (nMCol)
        ##, col = 1:length (levels (nMCol))
        , col = fCol, pch = 19, bty = "n")
legend ("topright", legend = c("T9", "others")
        , pch = c(1, 19)
#        , pt.cex = c(1, 2, 1)
        , bty = "n"
        )
dev.off()
## system ("convertHQ ~/tmp/LCI_noaa/media/2019/Zoop_nMDS_seasonal.pdf ~/tmp/LCI_noaa/media/2019/Zoop_nMDS_seasonal.png")



zooCenv$Transect <- factor (zooCenv$Transect)
zooCenv$warmCat <- factor (zooCenv$warmCat)
zooCenv$Year <- factor (zooCenv$Year)

save.image ("~/tmp/LCI_noaa/cache/zoopCommInSplot.RData")
## rm (list = ls()); load ("~/tmp/LCI_noaa/cache/zoopCommInSplot.RData") #; require (vegan)




save.image ("~/tmp/LCI_noaa/cache/zoopCommModel.RData")
## rm (list = ls()); load ("~/tmp/LCI_noaa/cache/zoopCommModel.RData") #; Require (vegan)
# print (names (zooCenv@data))



## finer resolution than months?
## plot actual day-of year against nMDS axes 1+2
Require ("sp")

zooCenv@data$jDate <- as.numeric (format (zooCenv@data$timeStamp, format = "%j"))
# nMScores <- nMScores [order (zooCenv$timeStamp),]
# zooC <- zooC []
# zooCenv <- zooCenv [order (zooCenv$timeStamp),]
PDF ("nmds1daily")
# Require ("lattice")
# wideT <- data.frame (jDate = zooCenv$jDate, nMScores)
# longT <- with (wideT, data.frame (jDate = rep (jDate, 2)
#   , axes = rep (c(1,2), each = nrow (wideT))
#   , nMDS = c(NMDS1,NMDS2)
# ))
# xyplot (nMDS~jDate | axes, data = longT, as.table = FALSE)
par (mfrow = c(2,1), mar = c (5, 4,0.5,0.1))
plot (nMScores [,1]~zooCenv$jDate, xlab = "julian date", ylab = "nMDS-1"
      , col = zooCenv$Year, pch = 19, subset = zooCenv$Transect == "9")
plot (nMScores [,2]~zooCenv$jDate, xlab = "julian date", ylab = "nMDS-2"
      , col = zooCenv$Year, pch = 19, subset = zooCenv$Transect == "9")
legend ("topleft", legend = levels (zooCenv$Year), pch = 19
        , col = levels (zooCenv$Year)
        , bty = "n", ncol = 4)
# for (i in 1:length (levels (zooCenv$Year))){
#   lines (nMScores [,2]~zooCenv$jDate, subset = zooCenv$Year == levels (zooCenv$Year)[i])
# }
dev.off()



## models of nMDS and above variables
## varName <- c("TempAnom", "Sal", "Temp", "SalAnom")
## for (i in 1:length (varName)){
##     print (summary (lm (as.formula (paste0 ("nMScores [,1]~", varName [i], "+ season")), data = zooCenv@data)))
##     print (summary (lm (as.formula (paste0 ("nMScores [,1]~", varName [i], "* month")), data = zooCenv@data)))
##     ## print (summary (lm (as.formula (paste0 ("nMScores [,1]~", varName, "* isoDate"))
## }
## rm (varName, i)


# MCMCglmm (count ~ surveyYear + 1, random = ~ us (1+SurveyYear):location
#           data, mD, family = "poisson"
#           nitt = 2000, thin = 10, burnin = 5000, pr = TRUE, pl = FALSE)

Require ("AICcmodavg")
zooCenv@data$month <- factor (zooCenv@data$month)
Modnames <- c ("season+TempAnom", "season*TempAnom", "season+Sal", "season*Sal"
             , "season+Temp", "season+SalAnom"
             , "season+TempAnom+Sal", "season", "month"
             , "season+Transect", "season+Transect+TempAnom"
             , "season*TempAnom+Transect"
             , "month*TempAnom"
             , "month*TempAnom+Transect"
             , "month+TempAnom"
               , "month+Sal", "month+Sal+Transect", "month*Sal+Transect", "month*Sal*Transect"
             , "1")
mdlL <- paste0 ("nMScores [,1]~", Modnames)
Cand.mod <- lapply (mdlL, FUN = function (m){
    lm (as.formula (m), data = zooCenv@data)
})
aicTx <- aictab (cand.set = Cand.mod, modnames = Modnames, sort = FALSE)
aicTx$R2 <- sapply (mdlL, FUN = function (m){
#    summary (lm (as.formula (m), data = zooCenv@data))$adj.r.squared
    summary (lm (as.formula (m), data = zooCenv@data))$r.squared
}
)

aicTb <- with (aicTx, data.frame (K, AICc = round (AICc, 1), Delta_AICc = round (Delta_AICc, 1)
                                , AICcWt = round (AICcWt, 3)
                                # , Cum.Wt = round (cumsum (aicTx$AICcWt), 3)
                                , LL = round (LL, 1), R2 = round (R2, 3)))
row.names (aicTb) <- aicTx$Modnames
print (aicTb [order (aicTb$AICc),])
write.csv (aicTb [order (aicTb$AICc),], file = "~/tmp/LCI_noaa/media/2019/Zoop_aic.csv")

## parameters of best model
## print (summary (lm (as.formula (mdlL [which.min (aicTx$AICc)]), data = zooCenv@data)))
bM <- summary (lm (as.formula (mdlL [which.min (aicTx$AICc)]), data = zooCenv@data))
print (bM)
write.csv (round (bM$coefficients, 4), file = "~/tmp/LCI_noaa/media/2019/Zoop_BestAICcMdl.csv")

rm (aicTb, bM, aicTx, mdlL)



## interaction.plot(x.factor, trace.factor, response, fun = mean,
##                  type = c("l", "p", "b", "o", "c"), legend = TRUE,
##                  trace.label = deparse1(substitute(trace.factor)),
##                  fixed = FALSE,
##                  xlab = deparse1(substitute(x.factor)),
##                  ylab = ylabel,
##                  ylim = range(cells, na.rm = TRUE),
##                  lty = nc:1, col = 1, pch = c(1:9, 0, letters),
##                  xpd = NULL, leg.bg = par("bg"), leg.bty = "n",
##                  xtick = FALSE, xaxt = par("xaxt"), axes = TRUE,
##                  ...)
## Arguments




if (0){## quantify variability within seasons, comparing warm/cold (why??)
sDz <- aggregate (nMScores [,1:3]~season+warmCat, data = zooCenv, FUN = sd)
Require (lattice)
# bargraph
PDF ("2019/Zoop_seasonal-SD")
for (i in 1:3){
    barchart (sDz [,2+i]~warmCat|season, data = sDz, ylab = paste ("SD nMDS", i))
}
## par (mfrow = c(2,2))
## for (i in 1:3){
##     barplot (nMScores [,i]~season+wrmCat, data = zooCenv)
## }
dev.off()
rm (sDz)
}


## how many years per season/temp category
aggregate (Year~season+warmCat, data = zooCenv, FUN = function (x){length (levels (factor (x)))})
## how many stations per season/temp category
#aggregate (paste0 (Transect, Station)~season+warmCat, data = zooCenv, FUN = function (x){
aggregate (Match_Name~season+warmCat, data = zooCenv, FUN = function (x){
  length (levels (factor (x)))})

if (0){
PDF ("2019/Zoop_intraseasonal-nMDS-boxplots")
for (j in 1:3){
    par (mfrow = c(2,2))
    for (i in 1:length (levels (zooCenv$season))){
        ## vioplot (nMScores [,1]~zooCenv$warmCat
        boxplot (nMScores [,j]~zooCenv$warmCat
               , subset = zooCenv$season == levels (zooCenv$season)[i]
               , xlab = levels (zooCenv$season)[i]
               , ylab = ""
                 ##, horizontal = TRUE
               , notch = TRUE, varwidth = TRUE
                 )
    }
    mtext (paste ("nMDS axis", j), outer = TRUE)
}
Require (lattice)
bwplot (nMScores[,1]~zooCenv$warmCat|zooCenv$season)
boxplot (nMScores [,1]~zooCenv$warmCat+zooCenv$season, notch = TRUE)
Require (vioplot)
vioplot (nMScores [,1]~zooCenv$warmCat+zooCenv$season)
dev.off()
}




## which species are drivers -> table?
rm (pSym, cH, nMScores)
cat ("\n\n### spring--fall ###\n")
print (head (sort (scores (nM, "species")[,1])))
print (tail (sort (scores (nM, "species")[,1])))

cat ("\n\n### spring--fall ###\n")
print (head (sort (scores (nM, "species")[,2])))
print (tail (sort (scores (nM, "species")[,2])))


if (0){            # plot not ready yet
## canonical correspondence analysis WITHIN seasons
PDF ("2019/Zoop_intraseasonal-CCA")
par (mfrow = c(2,2))
for (i in 1:length (levels (zooCenv$season))){
    ccaZ <- cca (zooC~zooCenv$TempAnom, subset = zooCenv$season == levels (zooCenv$season)[i])
    plot (ccaZ, main = levels (zooCenv$season)[i])
}
# plot (scores (ccaZ, display = "sites"), col= nMCol, pch = 19)




## nNDS betwee years (and seasons?)
zooY <- aggregate (zooC, by = list (zooCenv$Year), FUN = sum)
zY <- zooY$Group.1
zooY <- zooY [,2:ncol (zooY)]
row.names (zooY) <- zY
rm (zY)

## remove rare species?
# zooY <- zooY [,which (apply (zooY, 2, max) > 1)


Require (vegan)
nm <- metaMDS (zooY, try = 20, trymax = 100, parallel = 12)

PDF ("2019/Zoop_YearsNMDS")
plot (nm, display = "sites", shrink = TRUE, type = "t" ) #, labels = zY)
dev.off()

}




## make high-res png files for report in MS Word
# dir.create ("~/tmp/LCI_noaa/media/2019-png", showWarnings = FALSE, recursive = TRUE)
# pdfFl <- list.files ("~/tmp/LCI_noaa/media/2019/", "*.pdf", full.names = TRUE)
# pngFl <- gsub (".pdf$", ".png", pdfFl)
# pngFl <- gsub ("2019/", "2019-png/", pngFl)
# Require (parallel)
# outP <- mclapply (1:length (pdfFl), function (i){
#     oP <- system (paste ("~/bin/convertHQ", pdfFl [i], pngFl [i])
#                                 , intern = TRUE, ignore.stdout = TRUE)
# }, mc.cores = nCPUs)
# rm (pdfFl, pngFl, outP)

if (0){
  unlink (paste0 (dirL[3], "/2019-zoop"), recursive = TRUE, force = TRUE)
  file.rename(paste0 (dirL[3], "/2019"), paste0 (dirL[3], "/2019-zoop"))
}

print (Sys.time())

## EOF
