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

require ("png")
require ("tidyverse")
require ("gifski") ## install
require ("gganimate") ## install

frms <- list.files("C:/Users/Martin.Renner/Desktop/drifter-animation/")



animate (p, nframes = length (frms), fps=10)
anim_save ("~/tmp/LCI_noaa/media/drifter.gif", p)