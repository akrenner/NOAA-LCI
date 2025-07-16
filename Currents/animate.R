## animate

## drifter tracks:
# - output frames from QGIS
# 	- use time-variable
# - combine frames with GIMP, following https://www.gimp.org/tutorials/Simple_Animations/
# 	- open frames as Layers
# 	- Filter -> Animate -> Optimize for GIF
# 	- Export -> gif -> set delay (here 0.1 s)
# 	- to email, use "Insert Image"

# if doing this more often
# 	- ImageMagick !
# 	- or figure out to do this in R from scratch (incl. GIS)


## need admin to install ImageMagick, -> do it in R instead
## read a folder full of files. Output them as frames of an animation (gif)

# see https://www.listendata.com/2019/05/create-animation-in-r-learn-with.html



## ----------------------------------------------------------
## animation options:
# gganimate: good for ggplot2, but doesn't work with ggOceanMaps (yet). 2024
# animation: 2.7 2021 , github same age. Requires ImageMagick or GraphicsMagick or avconv or FFmpeg
# plotly: mainly for interactive web graphics. Version 4.x, 2024
# gifski in R: convert video frames to GIF. Requires Rust. (2023) -- maybe that's the ticket!
# tmap: too limited options for animation

## may need the flexibility of package animation??
## need to interpolate positions to make better animation


## tmaps: works, but animation potential quite limited
## ggplot2 -- need to adapt reading in world polygon to existing examples (sub borders) -- do that!
## go back to base graphics -- what I know? Want nices graticules.


# use ggOceanMaps to make map (has nice cartography)
# gganimate to animate it -- don't work together :(
#
# products:
#   1. animation of drifter track (n concurrent drifters)
#   2. static map of all drifters combined


## ggplot map:  https://bookdown.org/nicohahn/making_maps_with_r5/docs/ggplot2.html
## ggOceanMaps https://mikkovihtakari.github.io/ggOceanMaps/ (nice cartography, will need external bathymetry)
##             https://aen-r-workshop.github.io/4-ggOceanMaps/ggOceanMaps_workshop.html#1
## animation: https://gganimate.com/
## fancy background for land: https://jakob.schwalb-willmann.de/basemaps/







require ("png")
require ("tidyverse")
require ("gifski") ## install
require ("gganimate") ## install

frms <- list.files("C:/Users/Martin.Renner/Desktop/drifter-animation/")



animate (p, nframes = length (frms), fps = 10)
anim_save ("~/tmp/LCI_noaa/media/drifter.gif", p)
