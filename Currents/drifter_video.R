## animated video of drifter tracks


rm (list=ls()); load ("~/tmp/LCI_noaa/cache/drifter/drifterSetup.Rdata")
# drift <- read.csv ("~/tmp/LCI_noaa/data-products/drifter_cleaned.csv.gz")
# outpath <- "~/tmp/LCI_noaa/media/drifter/"


## patch up drift
# drift <-as.data.frame (drift)
# drift <- st_as_sf (drift, coords=c("Longitude", "Latitude")   ### why not keep if for drift?
#                    , dim="XY", remove=FALSE, crs=4326) %>%
#   st_transform(projection)



## -------- animation with trail for each deployment ------------- ##


## need to adjust frame margin and font sizes, if video resolution were to increase
resolu <- c (resW=1920, resH=1080, frameR=24) ## HD+ 1920 x 1080, HD: 1280x729, wvga: 1024x576 -- FR=7?
# resolu <- c (resW=1024, resH=576, frameR=24)
# resolu <- c (resW=1080, resH=576, frameR=24) ## HD+ 1920 x 1080, HD: 1280x729, wvga: 1024x576
# resolu <- c (resW=1280, resH=768, frameR=24) ## WXGA, computer, 16:9
# resolu <- c (resW=1024, resH=600, frameR=24) ## WSVGA, computer, 16:9


test <- TRUE
test <- FALSE


# drift$deploy <- factor (drift$Deployment)
drift$deploy <- factor (drift$deployV2)
dir.create(paste0 (outpath, "drifterVideo/"), showWarnings=FALSE, recursive=TRUE)


## bare av
dPlot <- function (i, replace=FALSE){
  require ("sf")
  require ("av")
  require ("RColorBrewer")

  tailL <- 30
  # frameR <- 7
  hC <- rev (RColorBrewer::brewer.pal(9, "YlOrRd")[2:9])
  video_file <- paste0 (outpath, "drifterVideo/", levels (drift$deploy) [i], ".mp4")

  makeVideo <- function (i){
    dI <- subset (drift, deploy == levels (drift$deploy)[i])
    if (nrow (dI) > 5){
    dstV <- c (0, sf::st_distance(x=st_geometry(dI)[1:(nrow (dI)-1),] # for running summary
                              , y=st_geometry (dI)[2:nrow (dI),]
                              , by_element=TRUE))

    ## interpolate -- here or earlier

    ## select appropriately sized coastline
    if (test){wMap <- worldM}else{
      if (st_transform(dI, crs=4326) %>%
          st_geometry() %>%
          st_coordinates() %>%
          min () < -154){
        wMap <- worldMb
      }else{
        wMap <- worldM
      }
    }

    av::av_capture_graphics({
      # par (ask=FALSE)
      devAskNewPage (ask=FALSE)
      par (mar= c(5.5,2,4,2)+0.1)
      mLine <- 3.25
#     for (j in seq_len (nrow (dI))){
      for (j in 2:nrow (dI)){
        # plotBG()
        #        plot (st_geometry(dI), type="n")
        plot (st_geometry(dI)[1:min (c (j + tailL, nrow (dI))),], type="n")
        u <- par ("usr")  # coordinates of plotting area
        rect (u[1], u[3], u[2], u[4], col="lightblue", border=NA)
        plot (wMap, add=TRUE, col = "beige")
        ## add tail
        tL <- min (c (j, tailL))
        st_linestring(st_coordinates (st_geometry (dI)[1:j])) %>% plot (add=TRUE, lwd=0.7, col="black")
        for (k in seq_along (hC)){
          tL2 <- min (j-1, tailL %/% k)
          st_linestring(st_coordinates (st_geometry (dI)[(j-tL2):j])) %>% plot (add=TRUE, lwd=k*3, col=hC[k])
        }
        plot (st_geometry(dI)[[j]], add=TRUE, col = "red", pch=19, cex=3)
        mtext (paste0 ("day ", floor (difftime(dI$DeviceDateTime[j], dI$DeviceDateTime [1], units="days"))
                       , "\n", format (dI$DeviceDateTime [j], "%Y-%m-%d %H:%M"), " UTC")
               , side=1, outer=FALSE, line=mLine, cex=2)
        mtext (paste ("speed:", sprintf ("%.3f", round (dstV[j]/dI$dT_sec[j], 2)), "m/s\n",
                      "cumulative distance:", round (sum (dstV [1:j])/1e3, 0), "km")
               , side=1, outer=FALSE, line=mLine, adj = 1, cex=2)
        mtext (paste0 ("Kasitsna Bay Lab, NCCOS, NOAA\n", "Direct questions to Reid Brewer: reid.brewer@noaa.gov")
               , side=1, outer=FALSE, line=mLine, adj=0, cex=2)
        title (main = paste0 (dI$DeviceName [1], ", depth: ", dI$drogue_depth [1], "m"), cex.main=2)
        ## add virtual particle from particle trajectory tool
        box()
      }
    }, output=video_file, width=resolu [1], height=resolu [2], framerate=resolu [3]
    # , vfilter=paste0 ('framerate=fps=', resolu [3])
    )
    }
  }

  if (isTRUE (replace)){## overwrite existing files
    makeVideo (i)
  }else if (file.exists(video_file)){
    cat (video_file)
  }else{
    makeVideo (i)
  }
}

dPlot (8, replace=TRUE)




## optimize parallel processing; big deploys first
dpl <- drift$deploy
dLvls <- seq_along(levels (drift$deploy))  # big files first

if (!test){
dLvls <- dLvls [order (sapply (dLvls, function (i){subset (dpl, dpl==levels (dpl)[i]) |>
    length()}), decreasing=!test)]
}
if (test){
  dLvls <- c(324, 194, 290, 287)
}
rm (dpl)

require ("parallel") ## better with parallelly???
ncores <- detectCores()
if (.Platform$OS.type=="unix"){
# if (0){
  result <- mclapply(dLvls, dPlot, mc.cores=ncores)
}else{
  cl <- makeCluster (ncores)
  clusterExport (cl, varlist=c ("drift", "worldM", "worldMb", "outpath", "resolu", "test"))
  result <- parLapplyLB (cl, dLvls, dPlot)
  stopCluster (cl); rm (cl)
}


rm (dPlot, dLvls)

## compare distance to CIOFS particles: need to be able to upload file with
## positions and times of particle deployment

