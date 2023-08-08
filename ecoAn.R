#!/usr/bin/env Rscript

## ecoAn.R
## correlations and aother analysis of bird, plankton, ecological data

## BUG:  birdS, poSS, and zooC all need same number of rows
## => match by poSS, fill in NAs as appropriate

## KISS!
## bubble plot of correlations!! 

if (!dir.exists ("~/tmp/LCI_noaa/media/")){dir.create ("~/tmp/LCI_noaa/media/")}

rm (list = ls()); load ("~/tmp/LCI_noaa/cache/dataSetupEnd.RData") # from dataSetup.R


## sniff tests
summary (subset (phyp$Pseudo.nitzschia, phyp$Year > 2014)) # 3x higher
summary (subset (phyp$Pseudo.nitzschia, phyp$Year < 2014))

summary (subset (zooC$Siphonophora, zooCenv$Year > 2014))
summary (subset (zooC$Siphonophora, zooCenv$Year < 2014))

summary (subset (zooC$Megalopa, zooCenv$Year > 2014))
summary (subset (zooC$Megalopa, zooCenv$Year < 2014))

summary (subset (birdS$COMU, birdS$Year > 2014))
summary (subset (birdS$COMU, birdS$Year < 2014))

summary (subset (birdS$BLKI, birdS$Year > 2014))
summary (subset (birdS$BLKI, birdS$Year < 2014))

summary (subset (birdS$COMU, birdS$Year > 2014))
summary (subset (birdS$COMU, birdS$Year < 2014))

summary (subset (birdS$TUPU, birdS$Year > 2014))
summary (subset (birdS$TUPU, birdS$Year < 2014))

# summary (subset (zooC$BLKI, birdS$Year < 2014))



require (vegan)
require (sp)


## strip sp info -- necessary and wise = ?? 
poSS <- poSS@data
phyp <- phyp@data

## summary statistics for BOEM report
writeSummary <- function (cM, rnd = 0, fn){
    comAb <- data.frame (mean = apply (cM, 2, mean, na.rm = TRUE)
                       , SE = apply (cM, 2, sd, na.rm = TRUE)/sqrt (nrow (cM)))
    comAb <- round (comAb [order (comAb$mean, decreasing = TRUE)[1:20],], rnd)
    names (comAb)[2] <- paste ("SE N=", nrow (cM), "", sep = "")
    comAb <- data.frame (species = row.names (comAb), comAb)
  #  comAb$species <- gsub (".([a-z]+)", " \\1", comAb$species)
    write.csv (comAb, file = paste ("~/tmp/LCI_noaa/media/", fn, sep = ""), row.names = FALSE)
    return (comAb)
}

zooAb <- writeSummary (zooC/zooCenv$Water.Sampled..m3., 2, "BOEMzoop_top20.csv")
# zooAb
rm (zooAb)

phypC <- phyp [,which (names (phyp) == "Alexandrium.spp.") :
                which (names (phyp) == "unknown.pennate.diatom")] #/phyp$Total.cell.count
phypAb <- writeSummary (phypC, 4, "BOEMphyp_top20.csv")
# phypAb
rm (phypAb)



## indicator species of warm waters
## compare 2012 and 13 to 2014 and 15
## cut-off time = ?

## consider season and sites as random variables
## warm-cold is the one we care about
## assuming things got warmin in January 2014, following
## https://accap.uaf.edu/sites/default/files/OA_Holderied_2016-11-30-KachemakBay-OA-monitoring.pdf

## warmcold <- ifelse (timeStamp > as.POSIXct ("2014-01-01 0:00"), "warm", "cold")

## cleanup names -- move to dataSetup? 
names (zooC) <- gsub ("[()\\.']", "", names (zooC))
names (zooC) <- gsub ("\\s", "_", names (zooC))
zooS <- cbind (zooCenv@data, zooC)
names (phyp) <- gsub ("\\.$", "", names (phyp))
names (phyp) <- gsub ("\\.\\.", ".", names (phyp))
names (phyp) <- gsub ("\\.", "_", names (phyp))
phyp <- phyp [,1:(grep ("unknown_pennate_diatom", names (phyp)))]

## prorate unidentified
aC <- function (oldN, data, FUN = sum){
    dM <- as.matrix (data)
    agN <- apply (data [,which (colnames (dM) %in% oldN)], 1, FUN)    
#    return (agN)
    dfN <- cbind (data [,! names (data) %in% oldN], agN)
    return (dfN)
}
## birdS <- aC (c("SOSH", "STSH", "UNDS"), birdS)
## birdS <- aC (c("KIMU", "MAMU", "UNML"), birdS)
## birdS <- aC (c("SOSH", "STSH", "UNDS"), birdS)
## birdS <- aC (c("SOSH", "STSH", "UNDS"), birdS)
## birdS <- aC (c("SOSH", "STSH", "UNDS"), birdS)
# print (names (birdS))



## subsample to address spatial autocorrelation? 
## should scale post.mean by overall mean?!  (not if on log-scale?)
save.image ("~/tmp/LCI_noaa/cache/mcmcPrep.RData")
### rm (list = ls()); load ("~/tmp/LCI_noaa/cache/mcmcPrep.RData"); require ("parallel")


require ("MCMCglmm")
set.seed (7)
indS <- function (spp = "BLKI", df = birdS, sFam = "exponential"){
    sT <- Sys.time()
## mdl <- lm (formula (paste ("log1p (", spp, ")~ warmcold + season + Match_Name")), data = birdS)
    df <- subset (df, Year > 2010)      # for birds
    df$warmcold <- ifelse (df$Year >= 2014, "warm", "cold")

    
    print (summary (df$Year))
    df$cruise <- paste (df$Year, df$season, sep = "-")
    
    if (sFam == "gaussian"){
        df [,names (df) == spp] <- scale ( df [,names (df) == spp])
        ##    do.call (paste ("df$", spp, " <- standardize (df$", spp, ")", sep = ""))
    }
    
    ## spatial distance matrix
    ## require ("geosphere")               # for distance matrix on great circle
    ## llDist <- distm (spTransform (stn
    ##                             , CRS("+proj=longlat +datum=WGS84"))[
    ##     match (df$Match_Name, stn$Match_Name),])
                                        # skip for now! poSS (physical oceanography)
                                        # is currently data.frame, not spatial.data.frame
                                        # address spatial autocorrelations -- later
    ## require ("spdep")
    ## dM <- knn2nb(knearneigh(coords, k = 4), row.names = IDs)
    ## also consider require (rstanarm) # -- stan being the latest and greatest, past WinBUGS

    require (MCMCglmm)
    mdl2n <- MCMCglmm (
        formula (paste (spp, "~ warmcold + season + 1"))
        ## , random = ~ us (1+Match_Name):season # XXX 
        # , random = ~us (1+Match_Name)
        ## Year effect: it's not nested within Match_Name (or vs versa). 
### , random = Match_Name + Year
      , random = ~Match_Name + cruise
     # , random = ~Match_Name
      , data = df
      , family = sFam
      , nitt = 50e3, thin = 10, burnin = 5e3 # yields 5000 samples. 10h
#   , nitt = 200, thin = 3, burnin = 50 # , pr = TRUE # min test.  -- 10 min
        ## default: nit = 13e3, thin = 10, burnin = 3e3  ## birds take about 3 h
      , verbose = FALSE
    )
    ## warm-cold estimates
    x <- summary (mdl2n)$solutions
    ## get post.means, 95% CI, and post.mean of seasonsummer (base is winter)
    ## extract those to compare change due to warm/cold to seasonal change
    wcCI <- c(x [grep ("warm", rownames (x)), 1:3]
            , summer = x [grep ("summer", rownames (x)), 1]
            , summer_lCI= x [grep ("summer", rownames (x)), 2]
            , summer_uCI= x [grep ("summer", rownames (x)), 3]
            , interc = x [1,1]         # measure of overall abundance? -- NOT!
            , abund = mean (df [,which (names (df) == spp)])
              )

    if (sFam != "gaussian"){wcCI <- -1*wcCI} # DIRTY hack! Why axes flipped = ?? XXX

    
    wcDF <- data.frame (species = spp, matrix (wcCI, nrow = 1))
    names (wcDF)[2:ncol (wcDF)] <- names (wcCI)
    wcDF$species <- as.character (wcDF$species)
    wcDF$ratio <- wcDF$post.mean / wcDF$summer # no need for CI of ratio
    
    ## cat (spp, "took", round (difftime (Sys.time(), sT, units = "mins"), 1)
    ##    , "min. w/c: ", wcCI [1], "\n")
    return (wcDF)    
}

require ("parallel")
require ("dplyr")
if (0){
indS ("BLKI")
indS ("SEOT")
indS ("COMU")
indS ("MAMU")
indS ("SST", poSS, sFam = "gaussian")
indS (names (zooS)[33], df = zooS)
indS (names (phyp)[33], df = phyp)
mdlL <- mclapply (names (birdS)[which (names (birdS) == "ALTE"):5] #ncol (birdS)]
          , indS, mc.cores = nCPUs)
}




sTime <- Sys.time()             # this part takes a long time. hours!

mdlW <- function (nV, df, ...){
    mdlL <- mclapply (nV, FUN = indS, mc.cores = nCPUs, df = df, ...)
    names (mdlL) <- nV
    require (dplyr)
    bind_rows (mdlL)
}


poAn <- mdlW (names (poSS)[grep ("SST", names (poSS))[1]:(ncol (poSS)-1)], poSS
            , sFam = "gaussian")
birdAn <- mdlW (names (birdS)[which (names (birdS) == "ALTE"):ncol (birdS)], birdS)

### trouble shooting
if (0){
    colIx <- (which (names (zooS) == "Year")+1) : ncol (zooS)
for (i in 1:length (colIx)){
    cat (i, names (zooS)[colIx [i]], "\n")
    x <- mdlW (names (zooS)[colIx [i]], zooS)
}
    }
### end trouble shooting

    
zooAn <- mdlW (names (zooS)[(which (names (zooS) == "Year")+1):ncol (zooS)], zooS)
phyAn <- mdlW (names (phyp)[grep ("Alexandrium", names (phyp))[1]:ncol (phyp)], phyp)

save.image ("~/tmp/LCI_noaa/cache/mcmc.RData")
### rm (list = ls()); load ("~/tmp/LCI_noaa/cache/mcmc.RData"); require ("parallel")



## some tests
require (dplyr)
bAn2 <- bind_rows (
    lapply (names (birdS)[which (names (birdS) == "ALTE"):ncol (birdS)]
              , df = birdS, FUN = function (spp, df){
                  df$warmcold <- ifelse (df$Year >= 2014, "warm", "cold")
                  mdl <- lm (formula (paste ("log1p (", spp, ")~warmcold"
                                           , sep = "")), df)
                  mdl <- lm (formula (paste (spp, "~warmcold", sep = "")), df)
                  ## print (summary (mdl))
                  ouDF <- data.frame (species = spp, wc = coefficients (mdl)[1])
              })
    )




cat ("MCMCglmm-s took", difftime (Sys.time(), sTime, units = "hours"), "hours \n")
## email notification
system (paste ("echo 'MCMCglmm finished in"
               , format (difftime (Sys.time(), sTime))
             , "' | mail mrenner@gmx.com -s 'progress notice'")
        )
print (Sys.time())
rm (mdlL, sTime)
save.image ("~/tmp/LCI_noaa/cache/mcmc3.RData")
## rm (list = ls()); load ("~/tmp/LCI_noaa/cache/mcmc3.RData")

climAn <- list (birdAn, zooAn, phyAn, poAn)
names (climAn) <- c("birds", "zoop", "phyp", "physical")
for (i in 1:length (climAn)){
    cat ("\n\n####\n", names (climAn)[i], "\n####\n")
    tDF <- climAn [[i]]
    print (tDF[order (tDF$post.mean^2, decreasing = TRUE),][1:10,1:5])
    names (tDF) <- gsub ("\\s", "_", names (tDF))
    names (tDF) <- gsub ("[\\%-]", "", names (tDF))
    tDF <- tDF [order (tDF$u95_CI-tDF$l95_CI, decreasing = TRUE),]
    tDF$species <- gsub ("\\_", " ", tDF$species)
    tDF$species <- gsub ("Pseudo\\s", "Pseudo-", tDF$species)
    ## pdf (paste ("~/tmp/LCI_noaa/media/clim_", names (climAn)[i]
    ##           , "t%03d.pdf", sep = ""), onefile = FALSE)
    ## hist (tDF$ratio)
    ## hist (tDF$post.mean)
    ## dev.off()
##  require (car)                       # add histograms to side of scatter plot
    pdf (paste ("~/tmp/LCI_noaa/media/", names (climAn)[i], "_warmcold.pdf", sep = "")
#       , height = 4, width = 4 ## for report
       , height = 6, width = 6 ## for talk (or paper?)
         )
    plot (post.mean ~ summer, tDF #, main = names (climAn)[i]
        , type = "n"
        , xlab = "summer (vs winter) effect", ylab = "warm year effect"
        , xlim = range (c(tDF$summer_uCI, tDF$summer_lCI))
        , ylim = range (c(tDF$l95_CI, tDF$u95_CI))
          , asp = 1
          )
    ## abline (v = 0)
    abline (h=0, v = 0, lty = "dashed", col = "gray", lwd = 2)
    abline (a=0,b=1)          # perfect positive correlation
                                        # regression line of climate and seasonal components
    oM <- lm (post.mean~summer, tDF)
    print (summary (oM))
    nDat <- data.frame (summer = seq (min (tDF$summer), max (tDF$summer), length.out = 100)
                      , post.mean = seq (min (tDF$post.mean), max (tDF$post.mean), length.out = 100))
    poM <- predict (oM, nDat, se.fit = TRUE)
    ##    nDat$post.mean <- predict (oM, nDat)
    nDat$post.mean <- poM$fit
    nDat$lSE <- poM$fit-poM$se.fit
    nDat$uSE <- poM$fit+poM$se.fit
    lines (post.mean~summer, nDat, col = "blue", lwd = 2)
    lines (lSE~summer, nDat, col = "blue", lty = "dashed", lwd =2)
    lines (uSE~summer, nDat, col = "blue", lty = "dashed", lwd = 2)
    
    ## if (names (climAn)[i] == "physical"){
    if (1){                             # color of diamonds
        cusCol <- "lightblue"
    }else{
        ## neither pretty, nor working yet! 
        x <- log10 (tDF$abund)
        hc <- heat.colors (20)
        x.renormed <- floor(100*((x-min(x))/(diff(range(x))+1e-2)))
        colors.grayscale <- paste("gray",x.renormed,sep="")
        ## cusCol <- heat.colors (nrow (x))
        cusCol <- colors.grayscale
    }

    ## polygon-area
    pArea <- with (tDF, (summer_uCI - summer_lCI) * (u95_CI - l95_CI))
    ## plot large polygons first -- sort by area
#    tDF <- tDF [order (abs (tDF$post.mean), decreasing = TRUE),] # fix order of plot
    tDF <- tDF [order (pArea, decreasing = FALSE),]
    for (j in 1:nrow (tDF)){
        xj <- tDF [j,]
        ## mark 95% CI as diamonds
        ## colored by relative log-abundance?
        polygon (x = c (xj$summer, xj$summer_uCI, xj$summer, xj$summer_lCI)
               , y = c (xj$u95_CI, xj$post.mean, xj$l95_CI, xj$post.mean)
               , col =  cusCol
                 )
        ## lines (tDF$summer [c(j,j)], c(tDF$l95_CI [j], tDF$u95_CI [j]), col = "blue")
        ## lines (c (tDF$summer_lCI [j], tDF$summer_uCI [j]), tDF$post.mean [c(j,j)], col = "blue")
    }
    tmax <- tDF [order (tDF$summer, decreasing = TRUE),]
    tmax <- tmax [c(1:5, (nrow (tmax)-3):nrow (tmax)),]
    with (tmax, text (summer, post.mean, labels = gsub ("_", " ", tmax$species), cex = 0.8))
    tmax <- tDF [order (tDF$post.mean, decreasing = TRUE),]
    tmax <- tmax [c(1:5, (nrow (tmax)-3):nrow (tmax)),]
    with (tmax, text (summer, post.mean, labels = gsub ("_", " ", tmax$species), cex = 0.8))
    rm (tmax)
    dev.off()
#    print (summary (lm (post.mean~summer, tDF)))

    pdf (paste ("~/tmp/LCI_noaa/media/", names (climAn)[i], "_histClimate.pdf", sep = ""))
    climRat <- tDF$post.mean/tDF$summer
    hist (climRat, main = "", xlab = "regime/summer")
    abline (v = mean (climRat), col = "blue")
    require ("latex2exp")
    mtext (paste ("mean:", round (mean (climRat), 1)), side = 3, at = mean (climRat), line = 0)    
#    mtext (TeX (paste ("$\bar{x}=", round (mean (climRat), 1), "$")), side = 1, at = mean (climRat), line = 0)
#    mtext (expression (paste ("bar (x) = ", round (mean (climRat), 1))), side = 1, at = mean (climRat), line = 0)
    box()
    dev.off()
    
  rm (tDF, oM)
}



## summarize indicator species
## are birdS overlapping?!





# require ("indicspecies")
# ?IndVal

## require ("vegan")
## ?indpower



## rm (list = ls()); load ("~/tmp/LCI_noaa/cache/mcmc3.RData")





## match nrow to physOs
phypC <- phypC [match (poSS$SampleID, phyp$SampleID),]
zooC <- zooC [match (poSS$SampleID, zooCenv$SampleID),]
birdS <- birdS [match (poSS$SampleID, birdS$SampleID), grep ("^[A-Z]{4}$", names (birdS))]


cMtx <- function (df){
    ## remove empty species
    df <- as.matrix (df)
    df [is.na (df)] <- 0
    dfR <- df [,which (colSums (df) > 0)]
    return (dfR)
}

## this really should be BEFORE match nrow to physOs! XXX 
poM <- cMtx (poSS [,which (names (poSS) == "SST"):which (names (poSS) == "pcDepth")])
phyM <- cMtx (phypC)
zooM <- cMtx (zooC)
birdM <- cMtx (birdS)
rm (cMtx)


dim (poM)
dim (phyM)
dim (zooM)
dim (birdM)

summary (poM)
summary (phyM)
summary (zooM)
summary (birdM)

save.image ("~/tmp/LCI_noaa/cache/BirdMR1.RData")
# load ("~/tmp/LCI_noaa/cache/BirdMR1.RData")

## aggregate to categories to avoid cca to blow up (all zeros)
aC <- function (oldN, data, FUN = sum){
    dM <- as.matrix (data)
    agN <- apply (data [,which (colnames (dM) %in% oldN)], 1, FUN)
    return (agN)
}
birdMr <- with (as.data.frame (birdM), cbind (BLKI
                                              , gull = aC (c("GHGU", "GLGU", "GWGU", "HEGU"), birdM) # GHGU = GWGUxHEGU? 
                                            , POJA
                                            , brMU = aC (c ("MAMU", "KIMU", "UNMU"), birdM)
                                            , CAAU                                            
                                            ## , loon = aC (c("COLO", "PALO", "YBLO"), birdM)
                                              , COMU, PIGU
                                            ## , largeAldic = aC (c("COMU", "PIGU"), birdM)
                                            , puffin = aC (c ("HOPU", "TUPU"), birdM)
                                            , FTSP
                                            , NOFU
                                            , shearwater = aC (c("SOSH", "STSH", "UNDS"), birdM)
                                            , WWSC
                                              ))

redM <- function (mtx, N = 10){
    ##  reduce community matrix to the N most common (most frequently encountered) species
    sFreq <- apply (mtx, 2, function (x){sum (x > 0)})
    mtx <- mtx [, order (sFreq, decreasing = TRUE)]
    return (mtx [,1:N])
}

noZ <- function (mtx){
    ## return indices of mtx rows with row sums > 0
    which (rowSums (mtx) > 0)
}
    


## DCA
## CCA

## bird vs zoop matrix
## zoop vs phyto matrix
## phyto vs physOc matrix

ccaM <- function (X, Y, ...){
    require (vegan)
    if (nrow (X) != nrow (Y)){cat ("rows don't match\n"); stop ()}
    i <- noZ (Y)
    mdl <- cca (Y [i,] ~ X [i,]) #, ..., na.action = na.omit)
    mdl2 <- rda (X [i,], Y [i,]) # X and Y swap?
    ## cca (redM (X, 10) ~ redM (Y, 10), ..., na.action = na.omit)
    print (mdl)
    print (mdl2)
    plot (mdl2)
#    plot (mdl)   
}


pdf ("~/tmp/LCI_noaa/media/CCA%02d.pdf", onefile = FALSE)

ccaM (phyM,  poM)
ccaM (zooM, phyM)
ccaM (birdMr, zooM)

ccaM (birdMr, phyM)
ccaM (birdMr,  poM)
ccaM (birdM, zooM)

## or better: rda?
dev.off()




### AICc test, as in seabirds -- outsource to ... 
## modelL <- c ("scr~year"
##              , "scr~temp"
##            , "scr~year+temp"
##              )


q()










## mantel test

## correlations of everything --- weird, check!  === things ain't right! 
coR <- function (...){cor (..., use = "pairwise.complete.obs", method = "pearson")} # "spearman")}

c1 <- coR (birdS, zooC)
c2 <- coR (birdS, poSS)
c3 <- coR (zooC, poSS)

clnNA <- function (cM){
    cM1 <- cM [,apply (cM, 2, function (x){!all (is.na (x))})]
    cM2 <- cM1 [apply (cM1, 1, function (x){!all (is.na (x))}),]
    return (cM2)
}

c1 <- clnNA (c1)
c2 <- clnNA (c2)
c3 <- clnNA (c3)

## require (elipse)
## ?plotcorr


summary (t(c1))
summary (c2)
summary (c3)

wM <- function (x){which.max (x^2)}
birdZoo <- data.frame (bird = row.names (c1), zoop = colnames (c1)[apply (c1, 1, wM)]
          , cor = c1 [apply (c1, 1, wM)])
birdPO <- data.frame (bird = row.names (c2), PO = colnames (c2)[apply (c2, 1, wM)]
                    , cor = c2 [apply (c2, 1, wM)])
zooPO <- data.frame (zoop = row.names (c3), PO = colnames (c2)[apply (c3, 1, wM)]
                    , cor = c2 [apply (c3, 1, wM)])

birdZoo [order (birdZoo$cor^2, decreasing = TRUE),]
birdPO [order (birdPO$cor^2, decreasing = TRUE),]
zooPO [order (zooPO$cor^2, decreasing = TRUE),]


## as dataSetup.RData, but including list of 20 most abundant birds, phyp, zoop
save.image ("~/tmp/LCI_noaa/cache/ecoAn.RData")


cat ("\n\n\nend of ecoAn.R \n\n\n")
