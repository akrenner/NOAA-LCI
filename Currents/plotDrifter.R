## plot Drifter tracks


rm (list=ls()); load ("~/tmp/LCI_noaa/cache/drifter/drifterSetup.Rdata")

require ("stars")
require ("RColorBrewer")
require ("dplyr")



## products:
## geoTIFF (for QGIS): grid of max speed (one for surface, one for sub-surface)
## nice map of Kachemak Bay max-speed grid
## interactive map of select drifters





## -----------------------------------------------------------------------------
## revert to R base-graphics -- keep it simple and flexible


plotBG <- function(downsample=0, dr=drift){
  ## start background details for plot
  nbox <- st_bbox (dr)
  pA <- projection == st_crs (4326)

  ## static plot of drifter tracks, colored by deployment
  par(mar=c(3,3,4,1))
  plot (st_geometry (worldM), col="beige", border=NA
        , xlim=nbox[c(1,3)], ylim=nbox[c(2,4)]
        , axes=pA
        , main="")
  # rm (pA, nbox)

  ## add google/bing map background -- ggmap an option?? needs token :(

  ## add bathymetry/topography
  if (0){
    depth <- st_as_stars(ifelse (mar_bathy$topo > 0, NA, mar_bathy$topo * -1)
                         , dimensions = attr(mar_bathy, "dimensions"))  ## move up if actually using!
    plot (depth, add=TRUE
          , nbreaks=100
          , compact=TRUE  ## still smothers everyting
          , col=colorRampPalette(c("lightblue", "darkblue"), interpolate="spline", bias=1)
          , main=""
    )
    ## add land back on
    plot (st_geometry (worldM), add=TRUE, col="beige") ## find a brewer color -- or satelite BG
  }else{
    ## see https://www.benjaminbell.co.uk/2019/08/bathymetric-maps-in-r-colour-palettes.html
    ## use topo colors for mar_bathy
    plot (mar_bathy, add=TRUE
          , downsample=downsample
          , col=c(colorRampPalette (c("darkblue", "lightblue"))(80)
                  , add.alpha (terrain.colors(100), 0.15))
          , breaks=c(seq (-400, 0, by=5), seq (0, 1000, by=10))
          , main=""
    )
    ## add land back on -- to tone down land
    # plot (st_geometry (worldM), add=TRUE, col=add.alpha("beige", 0.8), border=NA) ## find a brewer color -- or satelite BG
  }

  ## add bathymetry contours -- don't
  if (0){
    plot (st_contour (mar_bathy, contour_lines=TRUE
                      , breaks=seq (-500, -50, by=50))
          , add=TRUE, col="blue")
  }
}





resW <- 800

## color by device -- simple
png (filename=paste0 (outpath, "drifterPlot_byID.png")
     , width=resW, height=resH)
# , width=resW, height=resH)
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
     , width=resW, height=resH)
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
     , width=resW, height=resH)
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
dir.create(paste0 (outpath, "deployment/"), recursive=TRUE, showWarnings=FALSE)

plotDrift <- function (i){
  dR <- drift %>%
    #  filter (speed_ms < 3) %>%
    filter (deploy == levels (drift$deploy)[i]) %>%
    # select (days_in_water)
    select (speed_ms)
  # if (nrow (dR) > 3)
  png (filename=paste0 (outpath, "deployment/"
                        # ,i #
                        , gsub (":","-", levels (drift$deploy)[i])
                        , ".png")
       , width=resW, height=resH)
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













if (0){
  if (0){  ## fast IDW or gstat?
    ## fast IDW
    # https://geobrinkmann.com/post/iwd/
    ## not accelerated under windows -- worth the installation trouble?
    ## should use kriging for data-product output
    ## ideally, grid would be 95%-ile -- revert to gstat?

    # shoreline -- assume and force shoreline speed to be 0  -- only needed for interpolation, not for averaging
    # add shoreline to drifter speeds
    if (0){
      drift_sf <- worldM %>%
        st_sf () %>%
        st_simplify(dTolerance=0.01) %>%  ## in meters
        st_cast ("MULTIPOLYGON") %>%
        sf::st_coordinates() %>%
        as.data.frame() %>%
        st_as_sf (coords=c("X", "Y"), dim="XY", remove=TRUE, crs=4326) %>%
        st_transform(projection) %>%
        mutate (speed_ms = 0) %>%
        mutate (IDn = -1) %>%
        select (speed_ms, IDn) %>%
        rbind (drift_sf)
    }


    p <- require ('GVI')  ## for sf_to_rast
    if (!p){
      require ("remotes")
      remotes::install_github("STBrinkmann/GVI")
    }; rm (p)

    # save.image ("~/tmp/LCI_noaa/cache/drifter/drifterSpeedMap0.Rdata")
    require ('parallel')
    nCores <- detectCores()-1

    speed1 <- sf_to_rast (observer=drift_sf, v="speed_ms"
                          , aoi=st_sf (seaA)
                          , max_distance=10e3
                          , raster_res=5e3
                          , beta=3
                          , progress=TRUE
                          , cores=nCores  # no effect on windows? uses openMP?
    ) %>%
      st_as_stars () %>%  ## terra raster to stars
      st_warp(crs=projection)
  }else{
    ## consider using gstat to specify aggregating function = max
    require ("gstat")
    ## see https://mgimond.github.io/Spatial/interpolation-in-r.html
    ## create an empty grid see https://michaeldorman.github.io/starsExtra/reference/make_grid.html
    require ("starsExtra")
    grd <- starsExtra::make_grid (drift_sf, res=20e3)  # 1 km grid -- takes a while

    if (0){ ## serial processing?
      speedO <- gstat::idw (speed_ms~1, drift_sf, newdata=grd, idp=2.0
                            # , nmax=10e3
                            ,
      )
      ## clip to seaA
    }else{
      ## parallel interpolation
      ## see https://gis.stackexchange.com/questions/237672/how-to-achieve-parallel-kriging-in-r-to-speed-up-the-process
      vg <- variogram (speed_ms~1, drift_sf)
      mdl <- fit.variogram (vg, model=vgm (1, "Exp", 90, 1))
      # plot (vg, model=mdl)

      require ("parallel")
      nCores <- detectCores ()-1
      parts <- split (x=1:length (grd), f=1:nCores)

      if (.Platform$OS.type=="unix"){
        speedP <- mclapply(1:nCores, FUN=function (x){
          # gstat::idw (speed_ms~1, drift_sf, newdata=grd [parts[[x]],], idp=2.0, nmax=10e3)  ## requires sp class?
          krige (formula=speed_ms~1, locations=drift_sf, newdata=grd [parts[[x]],], model=mdl)
        })
      }else{
        cl <- makeCluster (nCores)
        clusterExport(cl=cl, varlist = c("grd", "drift_sf"), envir = .GlobalEnv)
        clusterEvalQ (cl=cl, expr = c(library ('sf'), library ('gstat'), library ('stars')))
        pX <- parLapply(cl=cl, X=1:nCores, FUN=function (x){
          krige (formula=speed_ms~1, locations=drift_sf, newdata=grd [parts[[x]],], model=mdl)
        })
        stopCluster (cl)
      }
      ## rbind grid--stars version of maptools
    }
  }
}



if (0){  ## redundant -- more kriging??
  ## variogram = excruciatingly slow -- subsample input?!  Parallelize
  v_mod_OK <- autofitVariogram(speed_ms~1, as(drift_sf, "Spatial"))$var_model
  # plot (v_mod_OK)

  # Create grid
  grid <- st_as_stars(st_bbox(st_buffer(drift_sf, 0.001)))  ## set resolution ?

  # Interpolation model
  g = gstat(formula = speed_ms~1, model = v_mod_OK$var_model, data = drift_sf)
  # plot (v_mod_OK, g) ## parameter cutoff needs to be specified


  if(0){
    require ('parallel')
    nCores <- detectCores()-1
    cl <- makeCluster (nCores)
    parts <- split (x=1:length (grid), f=1:nCores)
    clusterExport (cl=cl, varlist=c("drift_sf", "grid", "v_mod_OK", "g"), envir = .GlobalEnv)
    clusterEvalQ(cl = cl, expr = c(library('sf'), library('gstat')))
    parallelX <- parLapply(cl = cl, X = 1:no_cores, fun = function(x){
      krige(formula=log(zinc)~1, locations = drift_sf, newdata=grid[parts[[x]],], model = g)
    })
    stopCluster(cl)
  }

  # Interpolate
  z = predict(g, grid)  ## slow -- go back to parallel version?
  ## just do an IDW from the start -- for simplicity!

  # Plot
  plot(z, col = hcl.colors(12, "Spectral"), reset = FALSE)
  plot(st_geometry(drift_sf), add = TRUE)
  # text(st_coordinates(kerpensample_sf), as.character(round(kerpensample_sf$Z, 1)), pos = 3, add = TRUE)

  # grd <- drift %>%
  #   st_as_sf (coords=c("Longitude", "Latitude"), dim="XY", remove=FALSE, crs=4326) %>%
  #     st_bbox () |>
  #   st_as_stars (dx=1000) |>
  #   st_crop (drift)
  #
  # # https://r-spatial.org/book/12-Interpolation.html
  # require ('gstat')
  # v <- variogram (speed_ms~1, drift)
  save.image("~/tmp/LCI_noaa/cache/drifter/drifterKrige.RData")
  # load ("~/tmp/LCI_noaa/cache/drifter/drifterKrige.RData")
}










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
  png (paste0 (tD, "/frame%04d.png"), width = resW, height=resH, res=120)
  par (ask=FALSE)
  for (i in 1:nrow (drift)){
    plotBG()
    plot (subset (drift, 1:nrow (drift) <= i), add=TRUE)
    # arrows (x, y, ...)
  }
  dev.off()
  png_files <- list.files(tD, "*.png")
  gif_file <- paste0 (outpath, "driftAnimation.gif")
  gifski (png_files, gif_file, width=resW, height=resH
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
  }, video_file, resW, resH, res=144, vfilter='framerate=fps=10')
  utils::browseURL(video_file)

}

for (i in 1:5) alarm()

## -----------------------------------------------------------------------------
## EOF
