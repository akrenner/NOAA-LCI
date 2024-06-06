## plot Drifter tracks


rm (list=ls())

require ("stars")
require ("RColorBrewer")
require ("dplyr")

load ("~/tmp/LCI_noaa/cache/drifterSetup.Rdata")


## products:
## geoTIFF (for QGIS): grid of max speed (one for surface, one for sub-surface)
## interactive map of select drifters



wRes <- 800

## color by device -- simple
png (filename=paste0 (outpath, "drifterPlot_byID.png")
     , width=wRes, height=hRes)
# , width=wRes, height=hRes)
# pdf (filename=paste0 (outpath, "drifterPlot_byID.png")
#      , width=16, height=9)
plotBG(2)
drift %>%
  filter (year==2022) %>%
  #  mutate (col=brewer.pal (8, "Set2")[.$DeviceName]) %>%
  select (DeviceName) %>%
  mutate (DeviceName = factor (DeviceName)) %>%
  plot (add=TRUE, pch=19, cex=0.5
  )
dev.off()






## -----------------------------------------------------------------------------
## color by speed (or age of deployment)
png (filename=paste0 (outpath, "drifterPlot_speed.png")
     , width=wRes, height=hRes)
par (mfrow =c(1,2))
plotBG()

x <- subset (drift, (speed_ms < 3))$speed_ms
brks <- seq (min (x), max (x), length.out=20)
x <- sqrt (x)
# brks <- (seq (min (x), max (x), length.out=20))^2

drift %>%
  filter (speed_ms < 3) %>%
  filter (speed_ms > 0) %>%
  filter (deployDepth < 5) %>%
  select (speed_ms) %>%
  plot (add=TRUE
        , pch=19  ## by deployDepth
        , type="p"
        , alpha=0.5
        , breaks=brks
        ## key for colors
        # ,graticule=TRUE
  )
title (main = "speed: surface")

plotBG()
drift %>%
  filter (speed_ms < 3) %>%
  filter (speed_ms > 0) %>%
  filter (deployDepth > 5) %>%
  select (speed_ms) %>%
  plot (add=TRUE
        , pch=19  ## by deployDepth
        , type="p"
        , alpha=0.5
        , breaks=brks
        ## key for colors
        # ,graticule=TRUE
  )
title (main = "speed: 15 m depth")
dev.off()
rm (brks, x)


## age of deployment
png (filename=paste0 (outpath, "drifterPlot_age.png")
     , width=wRes, height=hRes)
plotBG()
drift %>%
  filter (speed_ms < 3) %>%
  #  filter (deployDepth < 5) %>%
  select (days_in_water) %>%
  plot (add=TRUE
        , pch=19  ## by deployDepth
        , cex=1
        , type="p"
        , alpha=0.5
        , nbreaks=100
        #        , key.pos=
        #        , breaks=brks
  )
title (main="time")
dev.off()
# rm (brks)




## test whether a legend can be shown
if (0){
  #  require ("ggmap")
  # m <- get_map ("Alaska" #, zoom=1
  #               , source="osm"
  #               , color="color")
  drift %>% select (days_in_water) %>%
    plot(axes=TRUE
         , graticule=TRUE
         , col_graticule="gray"
         #, bgMap= ## class ggmap, e.g. googlex
    )
}

## plot each individual deployment
dir.create(paste0 (outpath, "/deployment/"), recursive=TRUE, showWarnings=FALSE)

plotDrift <- function (i){
  png (filename=paste0 (outpath, "/deployment/", levels (drift$deploy)[i], ".png")
       , width=wRes, height=hRes)
  dR <- drift %>%
    #  filter (speed_ms < 3) %>%
    filter (deploy == levels (drift$deploy)[i]) %>%
    # select (days_in_water)
    select (speed_ms)

  plotBG(dr=dR)
  plot (dR, add=TRUE
        , pch=19  ## by deployDepth
        , cex=1, type="p"
        , alpha=0.5
        , nbreaks=100
        #        , key.pos=
        #        , breaks=brks
  )
  #  legend ("topleft", fill)
  # https://stackoverflow.com/questions/9314658/colorbar-from-custom-colorramppalette

  title (main=levels (drift$deploy)[i])
  dev.off()
}


for (i in seq_along(levels (drift$deploy))){
  print (i)
  plotDrift (i)
}

# library (parallels)
# require ('parallelly')


if (0){
  plot (speed_ms~topo, drift, pch=19, col = add.alpha("black", 0.1))
  lS <- loess(speed_ms~topo, drift)
  nD <- data.frame (topo=seq(-200, 0, length.out=100))
  lines (nD$topo, predict (lS, newdata=nD), col = "red")
}

## plot drifter
# for (i in 1:seq_along (levels (drift$deploy))){
#   dr <- subset (drift, deploy==levels (drift$deploy)[i])
#   if (nrow (dr)>2){
#   plot (st_geometry (dr), add=TRUE, type="l", lwd=3, col=dr$col)
#   plot (st_geometry (dr), add=TRUE, pch=20, cex=0.1)
#   }
# }








## -----------------------------------------------------------------------------
## animation of drifter tracks
# see https://hansenjohnson.org/post/animate-movement-in-r/

## plotly: interactive, but restrictive
## gganimate: easy, but restrictive
## googleVis: interactive? Needs flash
## animate:  flexible, but web-based. Not it! https://kcf-jackson.github.io/animate/
# require ("animate") ## simply combine frames -- most flexible
## output to HTML (with controls), mp4, gif, LaTeX

## animation tasks:
# 1. plot point by point
# 2. point by point, building up a tail from start
# 3. point by point with x-day long tail

# device <- animate$new()  ## real-time animation on html?

## or require ("gifski") (part of animate?) or require ("av")

## see https://cran.r-project.org/web/packages/animate/vignettes/introduction.html -- NO



## subset to min example
## start with small test! This takes time.




# setTimeLimit (elapsed=60*10
#               , transient=TRUE) # 10 min limit

if (0){
  tD <- tempdir()
  png (paste0 (tD, "/frame%04d.png"), width = wRes, height=hRes, res=120)
  par (ask=FALSE)
  for (i in 1:nrow (drift)){
    plotBG()
    plot (subset (drift, 1:nrow (drift) <= i), add=TRUE)
    # arrows (x, y, ...)
  }
  dev.off()
  png_files <- list.files(tD, "*.png")
  gif_file <- paste0 (outpath, "driftAnimation.gif")
  gifski (png_files, gif_file, width=wRes, height=hRes
          , delay=1/frameR, loop=0)
  unlink(tD, recursive=TRUE)
  utils::browseURL(gif_file)

  require ("av")
  av_encode_video (input=gif_file
                   , output=paste0 (outpath, "driftAnimation.mp4")
                   , framerate=frameR)




  ## bare av
  video_file <- paste0 (outpath, "driftAnimationAV.mp4")
  av::av_capture_graphics({
    par (ask=FALSE)
    for (i in 1:nrow (drift)){
      plotBG()
      plot (subset (drift, 1:nrow (drift) <= i), add=TRUE)
      # arrows (x, y, ...)
    }
  }, video_file, wRes, hRes, res=144, vfilter='framerate=fps=10')
  utils::browseURL(video_file)

}

for (i in 1:5) alarm()

## -----------------------------------------------------------------------------
## EOF
