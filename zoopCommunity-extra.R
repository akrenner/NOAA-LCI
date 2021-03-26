#!/usr/bin/env Rscript

## zooplankton comparison across years

rm (list = ls()); load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData") # from dataSetup.R
set.seed (8)
Require ("sp")
Require ("raster")
Require ("vegan")
dir.create ("~/tmp/LCI_noaa/media/2019-zoop/", recursive = TRUE, showWarnings = FALSE)

PDF <- function (fN, ...){
  pdf (paste0 ("~/tmp/LCI_noaa/media/2019-zoop/", fN, ".pdf"), ...)
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
nM <- metaMDS (zooC, distance = "bray", k = 3, try = 200, trymax = 500, parallel = nCPUs)  ## check! that k=2 from k=3 doesn't result in major change!!


save.image ("~/tmp/LCI_noaa/cache/zoopC1.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/zoopC1.RData")
zooCenv$month <- as.numeric (strftime (zooCenv$timeStamp, format = "%m"))



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





## language for diversity -- what we did  (that first)
##
# 1st week of May: LCI survey
# Isabella: semester by the bay student. Needs qGIS help

Require ("colorspace")
## mCol  <- brewer.pal (12, "Spectral")
mCol <- rainbow_hcl (12)








# nMCol <- Seasonal (zooCenv$month)
# fCol <- brewer.pal (length (levels (nMCol)), "Dark2")
nMCol <- factor (zooCenv$month)
Require ("RColorBrewer")
fCol <- rainbow_hcl (12, alpha = 0.5)
# fCol <- adjustcolor (fCol, alpha.f = 0.4)
## annual zooplankton cycle

# PDF ("2019/Zoop_nMDS_seasonal")
pdf ("~/tmp/LCI_noaa/media/2019-zoop/Zoop_nMDS_seasonalX.pdf")
# png ("~/tmp/LCI_noaa/media/2019/Zoop_nMDS_seasonal.png")
plot (nMScores [,1:2], type = "n")
## add convex hulls
for (i in 1:length (levels (nMCol))){
    cH (nMCol == levels (nMCol)[i], fCol [i], hull = TRUE)
}
points (nMScores [,1:2], col = fCol [as.numeric (nMCol)]
      , cex = ifelse (zooCenv$Match_Name == "9_6", 2, 1)
      , pch = 19
      #, cex = ifelse (zooCenv$Transect == 4, 2, 1)
        )
# T4 <- nMScore [order (zooCenv$month),]
# # T4 <- nMScores [grep ("^4_", row.names (nMScores)),]
# T4 <- T4 [grep ("^4_", row.names (T4)),]
# lines (T4 [,1:2])
# text (aggregate (nMScores [,1:2]~nMCol, FUN = mean)[,2:3], levels (nMCol))
text (aggregate (nMScores [,1:2]~nMCol, FUN = mean)[,2:3], month.abb)
## pSym <- c(25,1,17)
## legend ("bottomleft", legend = levels (factor (zooCenv$warmCat))
##       , pch = pSym [1:length (levels (zooCenv$warmCat))]
##       , bty = "n")
legend ("topright", legend = c("T9-6", "others")
        , pch = 19 #c(1, 19, 19)
        , pt.cex = c(2, 1)
        , bty = "n"
        )
dev.off()
## system ("convertHQ ~/tmp/LCI_noaa/media/2019/Zoop_nMDS_seasonal.pdf ~/tmp/LCI_noaa/media/2019/Zoop_nMDS_seasonal.png")




pdf ("~/tmp/LCI_noaa/media/2019-zoop/Zoop_nMDS-T=All.pdf")
plot (nMScores, cex = 0.4, col = adjustcolor (mCol, 0.7)[])
dev.off()




## EOF
