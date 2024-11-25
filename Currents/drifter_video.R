## animated video of drifter tracks


rm (list=ls()); load ("~/tmp/LCI_noaa/cache/drifter/drifterSetup.Rdata")
## -------- animation with trail for each deployment ------------- ##

tailL <- 30
frameR <- 7

driftP$deploy <- factor (driftP$deploy)
dir.create(paste0 (outpath, "/drifterVideo/"), showWarnings=FALSE, recursive=TRUE)

## bare av
dPlot <- function (i){
  require ("sf")
  require ("av")

  video_file <- paste0 (outpath, "drifterVideo/driftAnimationAV_", i, ".mp4")
  if (file.exists(video_file)){cat (video_file)}else{  ## don't overwrite existing files

    dI <- subset (driftP, deploy == levels (driftP$deploy)[i])

    ## interpolate -- here or earlier

    ## select appropriately sized coastline
    if (1){
    if (st_transform(dI, crs=4326) %>%
        st_geometry() %>%
        st_coordinates() %>%
        min () < -155){
      wMap <- worldMb
    }else{
      wMap <- worldM
    }
    }else{wMap <- worldM}

    av::av_capture_graphics({
      par (ask=FALSE)
      for (j in seq_len (nrow (dI))){
        # plotBG()
#        plot (st_geometry(dI), type="n")
        plot (st_geometry(dI)[1:min (c (j + 5, nrow (dI))),], type="n")
#        plot (wMap, type="n")
        plot (wMap, add=TRUE, col = "beige")
        ## add tail
        tL <- min (c (j, tailL))
        st_linestring (st_coordinates (st_geometry(dI)[1:j])) %>%
          plot (add=TRUE, lwd=0.5, col = "gray")
        st_linestring (st_coordinates (st_geometry(dI)[(j-tL):j])) %>%
          plot (add=TRUE, lwd=1)
        st_linestring (st_coordinates (st_geometry(dI)[(j-(tL%/%2)):j])) %>%
          plot (add=TRUE, lwd=2)
        st_linestring (st_coordinates (st_geometry(dI)[(j-(tL%/%4)):j])) %>%
          plot (add=TRUE, lwd=3)
        plot (st_geometry(dI)[[j]], add=TRUE, col = "red", pch=19, cex=1.2)
        legend ("topright", bty="n", box.col=NA
                , legend=paste0 ("day ", floor (difftime(dI$DeviceDateTime[j]
                                                         , dI$DeviceDateTime [1], units="days"))
                                 , "\n", format (dI$DeviceDateTime [j], "%Y-%m-%d %H:%M")))
        title (main = paste0 (dI$DeviceName [1], "\ndepth: ", dI$drogue_depth [1], "m"))
        ## add virtual particle from particle trajectory tool
        box()
      }
    }, output=video_file, width=resolu [1], height=resolu [2], framerate=frameR
    # , vfilter=paste0 ('framerate=fps=', resolu [3])
    )
  }
  # utils::browseURL(video_file)
}


# dPlot (2)  ## for testing

# for (i in seq_along (levels (driftP$deploy))){
#   dPlot (i)
# }
require ("parallel")
ncores <- detectCores()

dpl <- driftP$deploy
dLvls <- seq_along(levels (driftP$deploy))
dLvls <- dLvls [order (sapply (dLvls, function (i){subset (dpl, dpl==levels (dpl)[i]) |>
    length()}), decreasing=TRUE)]
rm (dpl)

# if (.Platform$OS.type=="unix"){
if (0){
  result <- mclapply(dLvls, dPlot, mc.cores=ncores)
}else{
  cl <- makeCluster (ncores)
  clusterExport (cl, varlist=c ("driftP", "tailL", "worldM", "worldMb", "outpath", "resolu", "frameR"))
  result <- parLapplyLB (cl, dLvls, dPlot)
  stopCluster (cl); rm (cl)
}


rm (tailL, frameR, dPlot, dLvls)

## compare distance to CIOFS particles: need to be able to upload file with
## positions and times of particle deployment

