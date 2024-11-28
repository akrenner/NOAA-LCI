## animated video of drifter tracks


rm (list=ls()); load ("~/tmp/LCI_noaa/cache/drifter/drifterSetup.Rdata")
## -------- animation with trail for each deployment ------------- ##


test <- TRUE
test <- FALSE


# driftP <- subset (driftP, off_deploy==FALSE)
driftP$deploy <- factor (driftP$deploy)
dir.create(paste0 (outpath, "/drifterVideo/"), showWarnings=FALSE, recursive=TRUE)


## bare av
dPlot <- function (i, replace=FALSE){
  require ("sf")
  require ("av")
  require ("RColorBrewer")

  tailL <- 30
  frameR <- 7
  hC <- rev (RColorBrewer::brewer.pal(9, "YlOrRd")[2:9])
  video_file <- paste0 (outpath, "drifterVideo/driftAnimationAV_", i, ".mp4")

  makeVideo <- function (i){
    dI <- subset (driftP, deploy == levels (driftP$deploy)[i])
    dstV <- c (0, st_distance(x=st_geometry(dI)[1:(nrow (dI)-1),] # for running summary
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
      par (mar=c(3,2,4,2)+0.1)
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
          # tL2 <- min (c (j, tL%/%k))
          # tL2 <- 1 + j - ifelse (tL%/%k > j, tL%/%k, j)
          # st_linestring(st_coordinates (st_geometry (dI)[tL2:j])) %>% plot (add=TRUE, lwd=k, col=hC[k])
          tL2 <- min (j-1, tailL %/% k)
          st_linestring(st_coordinates (st_geometry (dI)[(j-tL2):j])) %>% plot (add=TRUE, lwd=k*1.5, col=hC[k])
        }
        plot (st_geometry(dI)[[j]], add=TRUE, col = "red", pch=19, cex=2)

        # mtext (paste0 ("day ", floor (difftime(dI$DeviceDateTime[j], dI$DeviceDateTime [1], units="days"))
        #                , "\n", format (dI$DeviceDateTime [j], "%Y-%m-%d %H:%M"), " UTC\n")
        #        , side=1, outer=TRUE, line=-2)
        # mtext (paste ("cumulative distance:", round (sum (dstV [1:j])/1e3, 0), "km\n"
        #               , "speed:", sprintf ("%.3f", round (dstV[j]/dI$dT_sec[j], 2)), "m/s")
        #        , side=1, outer=FALSE, line=1.5, adj = 1)
        mtext (paste0 ("day ", floor (difftime(dI$DeviceDateTime[j], dI$DeviceDateTime [1], units="days"))
                       , "\n", format (dI$DeviceDateTime [j], "%Y-%m-%d %H:%M"), " UTC")
               , side=1, outer=FALSE, line=1.5)
        mtext (paste ("cumulative distance:", round (sum (dstV [1:j])/1e3, 0), "km\n"
                      , "speed:", sprintf ("%.3f", round (dstV[j]/dI$dT_sec[j], 2)), "m/s")
               , side=1, outer=FALSE, line=1.5, adj = 1)
        mtext (paste0 ("Kasitsna Bay Lab, NCCOS, NOAA\n", "Direct questions to Reid Brewer: reid.brewer@noaa.gov")
               , side=1, outer=FALSE, line=1.5, adj=0)
        title (main = paste0 (dI$DeviceName [1], ", depth: ", dI$drogue_depth [1], "m"))
        ## add virtual particle from particle trajectory tool
        box()
      }
    }, output=video_file, width=resolu [1], height=resolu [2], framerate=frameR
    # , vfilter=paste0 ('framerate=fps=', resolu [3])
    )
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



require ("parallel")
ncores <- detectCores()

dpl <- driftP$deploy
dLvls <- seq_along(levels (driftP$deploy))  # big files first
dLvls <- dLvls [order (sapply (dLvls, function (i){subset (dpl, dpl==levels (dpl)[i]) |>
    length()}), decreasing=!test)]
rm (dpl)

if (.Platform$OS.type=="unix"){
# if (0){
  result <- mclapply(dLvls, dPlot, mc.cores=ncores)
}else{
  cl <- makeCluster (ncores)
  clusterExport (cl, varlist=c ("driftP", "worldM", "worldMb", "outpath", "resolu"))
  result <- parLapplyLB (cl, dLvls, dPlot)
  stopCluster (cl); rm (cl)
}


rm (dPlot, dLvls)

## compare distance to CIOFS particles: need to be able to upload file with
## positions and times of particle deployment

