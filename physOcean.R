#!/usr/bin/env Rscript

## physical oceanography -- based on zoop script "zoopCommunity.R"  


rm (list = ls()); load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData") # from dataSetup.R
set.seed (8)
Require (sp)
Require (vegan)
Require (raster)
dir.create ("~/tmp/LCI_noaa/media/2019/", recursive = TRUE, showWarnings = FALSE)


possEnv <- poSS
possC <- poSS@data [,which (names (poSS@data) == "maxDepth") 
                    : which (names (poSS@data) == "pcDepth")
                    # : which (names (poSS@data) == "PARdepth1p")
                    ]


zooC <- possC
zooCenv <- possEnv
rm (possC, possEnv)

### standardize survey effort and distribution!! 
## pick out stations that were present in each year-season combination
year_season <- with (zooCenv@data, paste (Year, season, sep = "-"))
# sttn <- with (zooCenv@data, paste (Transect, Station, sep = "-"))
sttn <- zooCenv@data$Match_Name

xT <- table (year_season, sttn)
# print (t (xT))
stCount <- apply (xT, 2, FUN = function (x){sum (x > 0)})
print (sort (stCount, decreasing = TRUE))
print (length (levels (factor (year_season))))

## based on above, select all of Transect 9, 4, 6-5, 7-20, 3-13, and 3-4
# keepSt <- sttn %in% names (which (stCount > 16)) # 100 FALSE, 325 TRUE
# keepSt <- sttn %in% names (which (stCount >= 10)) # 100 FALSE, 325 TRUE
## in zoop, kept 10 -- here only 15 year-season-levels => keep >= 6
keepSt <- sttn %in% names (which (stCount >= 6))  # 43 FALSE, 414 TRUE (6) -- or >= 8 59:398

if (1){                                          # remove stations sampled only rarely
  zooC <- subset (zooC, keepSt)
  zooCenv <- subset (zooCenv, keepSt)
  ## remove zero-species
  zS <- apply (zooC, 2, sum)
  zooC <- zooC [,which (zS > 0)]
  rm (zS)
  ## also remove empty stations
  zooCenv <- subset (zooCenv, rowSums (zooC) > 0)
  zooC <- subset (zooC, rowSums (zooC) > 0)
  
  ## XXX tmp XXX!!!
  # remove outlier
  zooCenv <- zooCenv [-68,]
  zooC <- zooC [-68,]
}
rm (year_season, sttn, xT, stCount, keepSt)




# require (vegan)
if (.Platform$OS.type == "windows"){
  # Require ("parallel")
  # cl <- makeCluster("PSOCK", )
  #     clusterEvalQ (cl, library (vegan))
}

nM <- metaMDS (zooC, distance = "bray", k = 3, try = 200, trymax = 500, parallel = nCPUs)

if (.Platform$OS.type == "windows"){
  #  rm (cl)
}

## tmp addition
##
save.image("~/tmp/LCI_noaa/cache/phyto1stop.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/phyto1stop.RData")
##
## end of tmp addition

## cluster

## k-means
# kM <- kmeans (zooC, centers = 4)

# load ("~/tmp/LCI_noaa/cache/tempAnomalyMonth.RData") # gets tempM
load ("~/tmp/LCI_noaa/cache/dailyTempAnomalies.RData") # gets tempDay from SeldoviaTemp.R
zooCenv$month <- as.numeric (strftime (zooCenv$timeStamp, format = "%m"))

# tempDATE <- with (tempDay, paste (Day, month.abb [month], year-2000, sep = "-")) # somewhat dirty! clean up
## phyCenv has better date format than zooCenv
tempDATE <- with (tempDay, paste (year
                                  , formatC (month, width = 2, format = "d", flag = "0")
                                  , Day, sep = "-"))
tempMatch <- match (zooCenv$Date, tempDATE); rm (tempDATE)

zooCenv$TempAnom <- tempDay$days90 [tempMatch]
tempDay$warmCat <- with (tempDay, cut (days90
                                       , breaks = c(-100, mean (days90)+c(-1,1)*(sd (days90)/4), 100)
                                       , labels = c("cold", "neutral", "warm")))
zooCenv@data$warmCat <- tempDay$warmCat [tempMatch]

## apply warm/cold categories only to sampled stations -- update temp graphs accordingly
zooCenv@data$warmCatL <- cut (zooCenv$TempAnom
                              , breaks = c(-100
                                           , mean (zooCenv$TempAnom, na.rm = TRUE)+
                                             c(-1,1)*sd (zooCenv$TempAnom, na.rm = TRUE)/4
                                           , 100)
                              , labels = c("cold", "neutral", "warm"))
zooCenv@data$Sal  <- tempDay$SalD365 [tempMatch]
# print (summary (zooCenv))
Require (sp)
# print (with (zooCenv@data, summary (Sal)))
zooCenv@data$SalCat <- with (zooCenv@data, cut (Sal
                                                , breaks = c(-10, mean (Sal, na.rm = TRUE)+
                                                               c(-1,1)*sd (Sal, na.rm = TRUE)/4, 100)
                                                , labels = c("fresher", "average", "saltier")))
# zooCenv$SalCat <- tempDay$SalCat  [match (zooCenv$Date, tempDATE)]
zooCenv@data$SalAnom <- tempDay$SalAnom [tempMatch]
zooCenv$Temp <- tempDay$Temp [tempMatch] # really should be MA
rm (tempMatch)
print (names (zooCenv@data))






# ## replot SeldoviaTemp graph -- re-assigning temperatures
#                                 # seldovia temperature
# # PDF ("2019/SeldTempAnomaly-recategorize", width = 12, height = 7)
# pdf ("~/tmp/LCI_noaa/media/2019/SeldTempAnomaly-recategorize.pdf", width = 12, height = 7)
#   nArgs <- with (tempDay, ifelse ((month == 1)&(Day == 1)&(year %% 3 == 0), year, NA))
# poDay <- with (tempDay, as.POSIXct (paste (year, month, Day, sep = "-")))
# tempDay$AJ <- tempDay$days90 - mean (zooCenv$TempAnom) # set zero to mean of samples
#  tempDay$AJ <- tempDay$days90
# 
# nRange <- sd (tempDay$AJ)/4
# 
# 
# barplot (tempDay$AJ~poDay
#        , space = 0
#        , col = ifelse (tempDay$AJ < (mean (tempDay$AJ)-nRange), "blue"
#                      , ifelse (tempDay$AJ < (mean (tempDay$AJ)+nRange)
#                              , "gray", "red"))
# ##       , col = ifelse (tempDay$AJ < 0, "blue", "red")
#        , border = NA
#        , xlab = "time"
#        , ylab = "90 day moving average temperature anomaly [Â°C]"
#        , axes = FALSE
#        , names.arg = "" #nArgs
#         # , ylim = c(-2.6, 2.5)
#          )
# axis (2)
# axis (1, at = which (!is.na (nArgs)), labels = FALSE) # subset (nArgs, !is.na (nArgs)))
# axis (1, at = which (!is.na (nArgs)) + 182, tick = FALSE, labels = subset (nArgs, !is.na (nArgs)))
# abline (v = which (with (tempDay, (month == 1) & (Day == 1)))
#   , lty = "dashed", col = "gray")
# box()
# ## mark zoop surveys
# tempDay$Sample <- strftime (poDay, "%Y-%m-%d") %in% strftime (zooCenv$isoDate, "%Y-%m-%d")
# # points (which (tempDay$Sample), -2.7, pch = 19)
# axis (3, at = which (tempDay$Sample), label = FALSE)
# abline (h = mean (tempDay$AJ) + c(1, -1)* nRange, col = "gray", lty = "dashed")
# dev.off()
# rm (tempDay, nArgs, poDay, nRange)







## surveys by warm/cold categorie
sampleWC <- aggregate (warmCat~season+Year, data = zooCenv, FUN = summary)
## table for methods
sampleWC <- cbind (sampleWC [,2:1], as.data.frame (sampleWC [,3]))
write.csv (sampleWC, file = "~/tmp/LCI_noaa/media/2019/zoopTempsamplingSummary.csv", row.names = FALSE)
rm (sampleWC)


nMScores <- scores (nM, "sites") #, choices = c(1,2,3))
## nMCol <- nMScores
## nMCol <- factor (zooCenv$Transect)
## nMCol <- factor (zooCenv$warmCat)
## nMCol <- factor (zooCenv$Year)
nMCol <- zooCenv$season



save.image ("~/tmp/LCI_noaa/cache/phytoCommVar.RData")
## rm (list = ls()); load ("~/tmp/LCI_noaa/cache/phytoCommVar.RData") #; require (vegan)




T9 <- subset (nMScores, zooCenv$Match_Name == "Kasitsna_Dock")
T9env <- subset (zooCenv, zooCenv$Match_Name == "Kasitsna_Dock")
T9 <- subset (zooC, zooCenv$Transect == "9")
T9env <- subset (zooCenv, zooCenv$Transect == "9")

T9 <- as.matrix (T9)
mT9 <- aggregate (T9~month, T9env, FUN = mean)
# mT9 <- aggregate (as.matrix (zooC)~month, data = zooCenv, subset = Transect == 9, FUN = mean)

Require ("factoextra")
Require (magrittr) # for pipe!
zoo.hc <- mT9 %>%
  scale() %>%
  dist (method = "manhattan") %>%
  hclust (method = "ward.D2")
PDF ("2019/Phyto_season-cluster")
fviz_dend(zoo.hc, k = 4, # Cut in four groups
          cex = 1, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)
dev.off()
rm (zoo.hc, mT9, T9, T9env, nMCol)















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
## delete that part here -- no extensive phytoplankton samples for T9 
## (or at the very least, would need to break-out Transect)




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


# PDF ("2019/Zoop_nMDS_seasonal")
png ("~/tmp/LCI_noaa/media/2019/Zoop_nMDS_seasonal.png")
pSym <- c(25,1,17)
plot (nMScores [,1:2], type = "n")
legend ("topleft", legend = levels (nMCol)
        ##, col = 1:length (levels (nMCol))
        , col = fCol, pch = 19, bty = "n")
text (aggregate (nMScores [,1:2]~nMCol, FUN = mean)[,2:3], levels (nMCol))
### -- ERROR -- variable length differ 
### other issue: NMDS-1 has bad outlier
#########
identify (nMScores [,1:2])
badD <- c(68, 70, 262)
zooC [badD,]
## legend ("bottomleft", legend = levels (factor (zooCenv$warmCat))
##       , pch = pSym [1:length (levels (zooCenv$warmCat))]
##       , bty = "n")
legend ("topright", legend = c("T9", "others")
        , pch = c(1, 19), bty = "n")
## add convex hulls
for (i in 1:length (levels (nMCol))){
  cH (nMCol == levels (nMCol)[i], fCol [i], hull = TRUE)
}
points (nMScores [,1:2], col = fCol [as.numeric (nMCol)]
        , pch = 19 # ifelse (zooCenv$Transect == 9, 1, 19)
)
dev.off()
## system ("convertHQ ~/tmp/LCI_noaa/media/2019/Zoop_nMDS_seasonal.pdf ~/tmp/LCI_noaa/media/2019/Zoop_nMDS_seasonal.png")



# zooCenv$Transect <- factor (zooCenv$Transect)
zooCenv$warmCat <- factor (zooCenv$warmCat)
zooCenv$Year <- factor (zooCenv$Year)

save.image ("~/tmp/LCI_noaa/cache/phytoCommInSplot.RData")
## rm (list = ls()); load ("~/tmp/LCI_noaa/cache/phytoCommInSplot.RData") #; require (vegan)

## within-season plots
plotInSeason <- function (pdfN, plotCat = "warmCat"
                          , colP # = 1:length (levels (which Cat))
                          , hull = TRUE, legLoc = "topleft"
                          , reScale = FALSE
){
  Require ("raster")
  if (!(plotCat %in% names (zooCenv))){stop("plotCat=", plotCat, "needs to be a name in zooCenv, like", names (zooCenv))}
  #    for (k in 1:2){
  for (k in 1){
    PDF (paste0 ("2019/", pdfN, k))
    par (mfrow = c(2,2))
    for (i in 1:length (levels (zooCenv$season))){
      if (reScale){
        Require (vegan)
        cat ("\n##\n", i, levels (zooCenv$season) [i], "\n\n##\n")
        subSc <- metaMDS (subset (zooC
                                  , zooCenv$season == levels (zooCenv$season)[i])
                          , distance = "bray", k = 3, trymax = 100, try = 50, parallel = 12)
        subSc <- scores (subSc, "sites")[,matrix (c(1,2,2,3), nrow = 2)[,k]]
      }else{
        subSc <- subset (nMScores [,matrix (c(1,2,2,3), nrow = 2)[,k]] # subset axis 2/3 as y-axis
                         , zooCenv$season == levels (zooCenv$season)[i])
      }
      ze <- subset (zooCenv@data, zooCenv$season == levels (zooCenv$season)[i])
      ze$Cat <- factor (ze [,which (names (ze) == plotCat)])
      if (length (levels (ze$Cat)) != length (colP)){
        warning ("number of levels unequal to number of colors")
      }
      plot (subSc, col = colP [as.numeric (ze$Cat)]
            , main = levels (ze$season)[i]
            , pch = 19
            , axes = FALSE)
      box()
      if (i %in% c(1,3)){axis (2)}
      if (i %in% c(3,4)){axis (1)}
      if (i == 1){
        legend (legLoc, legend = levels (ze$Cat), col = colP [1:length (levels (ze$Cat))]
                , pch = 19, bty = "n")
      }
      #            nL <- as.numeric (levels (factor (as.numeric (ze$Cat))))
      #            for (j in 1:length (nL)){
      for (j in 1:length (levels (ze$Cat))){
        cH (fac = ze$Cat == levels (ze$Cat)[j]
            , colC = colP [j]
            , pts = subSc, hull = hull)
      }
      rm (subSc)
    }
    dev.off()
  }
}


## warm/cold
plotInSeason (pdfN = "Zoop_intraseasonal-nMDS_warm-cold"
              , plotCat = "warmCat"
              , colP = c ("blue", "gray", "red")
              , hull = TRUE, legLoc = "bottomleft")

## compare location (Transects; Stations)
Require ("RColorBrewer")
# plotInSeason ("Zoop_intraseasonal-nMDS_location"
#             , plotCat = "Transect"
#             , colP = rainbow_hcl (length (levels (factor (zooCenv$Transect))))
#                                         # brewer.pal (length (levels (zooCenv$Transect)), name = "Set3")  
#             , hull = TRUE, legLoc = "bottomleft")

Require ("colorspace")
plotInSeason ("Zoop_intraseasonal-nMDS_years"
              , plotCat = "Year"
              , colP =  rainbow_hcl (length (levels (zooCenv$Year)))  
              , hull = TRUE, legLoc = "bottomleft")

# print (summary (zooCenv$SalCat))
plotInSeason ("Zoop_intraseasonal-nMDS_Sal"
              , plotCat = "SalCat"
              , colP = brewer.pal (length (levels (zooCenv$SalCat)), name = "Dark2")
              , hull = TRUE, legLoc = "bottomleft")

# plotInSeason ("Zoop_intraseasonal-reScalenMDS_location"
#             , plotCat = "Transect"
#             , colP = rainbow_hcl (length (levels (factor (zooCenv$Transect))))
#             , hull = TRUE, legLoc = "bottomleft"
#             , reScale = TRUE
#               )
plotInSeason ("Zoop_intraseasonal-reScalenMDS_years"
              , plotCat = "Year"
              , colP = rainbow_hcl (length (levels (zooCenv$Year)))  
              , hull = TRUE #, legLoc = "bottomleft"
              , reScale = TRUE
)

rm (plotInSeason)
###


save.image ("~/tmp/LCI_noaa/cache/phytoCommModel.RData")
## rm (list = ls()); load ("~/tmp/LCI_noaa/cache/phytoCommModel.RData") #; Require (vegan)
# print (names (zooCenv@data))


## models of nMDS and above variables
## varName <- c("TempAnom", "Sal", "Temp", "SalAnom")
## for (i in 1:length (varName)){
##     print (summary (lm (as.formula (paste0 ("nMScores [,1]~", varName [i], "+ season")), data = zooCenv@data)))
##     print (summary (lm (as.formula (paste0 ("nMScores [,1]~", varName [i], "* month")), data = zooCenv@data)))
##     ## print (summary (lm (as.formula (paste0 ("nMScores [,1]~", varName, "* isoDate"))
## }
## rm (varName, i)

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




## which species are drivers
rm (pSym, cH, nMScores)
cat ("\n\n### spring--fall ###\n")
print (sort (scores (nM, "species")[,1]))
cat ("\n\n### spring--fall ###\n")
print (sort (scores (nM, "species")[,2]))


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

unlink (paste0 (dirL[3], "/2019-phytop"), recursive = TRUE, force = TRUE)
file.rename(paste0 (dirL[3], "/2019"), paste0 (dirL[3], "/2019-phytop"))

print (Sys.time())

## EOF
