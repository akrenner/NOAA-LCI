## Demo-script to try out settings
##

## files in package:
## aggregated CTD data: ctdwallSetup.RData
## utility functions:   CTDsectionFcts.R
## this current file:   I-CTDsections-example.R

## Run entire script ("Source" button), after changing sv, tn, and ov, as
## desired, all within the "interactive user selection of transect, date,
## variable" heading.


## output: print to graphics device rather than image file
## load data from .RData file rather than csv files. The latter would add much
## code to select relevant files and match locations.


## ---------------------------- general set-up ------------------------------ ##
rm (list = ls())
if (file.exists("~/tmp/LCI_noaa/cache/ctdwallSetup.RData")){
  base::load ("~/tmp/LCI_noaa/cache/ctdwallSetup.RData")  # from CTDwall-setup.R
  ## bundle all required files into a zip archive
  tD <- "~/tmp/LCI_noaa/cache/tCTD-example"
  dir.create(tD, showWarnings=FALSE, recursive=TRUE)
  file.copy("CTDsectionFcts.R", tD)
  file.copy("I-CTDsections-example.R", tD)
  file.copy("~/tmp/LCI_noaa/cache/ctdwallSetup.RData", tD)
  # file.copy ("~/GISdata/LCI/bathymetry/KBL-bathymetry/KBL-bathymetry_GWA-area_50m_EPSG3338.tiff", tD)
  # file.copy ("~/src/oce_1.7-3.tar.gz", "~/tmp/LCI_noaa/cache/CTDexample/oce_1.7-3.tar.gz")
  unlink ("~/tmp/LCI_noaa/cache/CTDexample.zip")
  zip (zipfile="~/tmp/LCI_noaa/cache/CTDexample.zip",
       , files=dir (tD, full.names=TRUE), extras="-j") # -j drops directories in zip file
  unlink (tD, recursive=TRUE); rm (tD)
}else{
  if (!file.exists ("ctdwallSetup.RData")){
    setwd(choose.dir(caption = "Select folder containing ctdwallSetup.RData"))
  }
  base::load ("ctdwallSetup.RData")
}
# pks <- c("sf", "dplyr", "stars", "oce")
pks <- c("oce")
for (i in seq_along(pks)){
  tst <- require (pks[i], character.only=TRUE)
  if (!tst){install.packages (pks [i], dependencies=TRUE, quiet=TRUE,
                              repos="https://cloud.r-project.org")}
}
rm (pks, i, tst)


source ("CTDsectionFcts.R")  # get pSec to plot sections



## ------- interactive user selection of transect, date, variable ------- ##
## pick date and transect
cat ("Available survey dates: \n")
print (levels (poAll$survey))
sv <- 18                                        # user to pick index number


cat ("Selected date:", levels (poAll$survey) [sv], "\n")
if (sv > length (levels (poAll$survey))){stop ("sv has to be smaller than ", 1+length (levels (poAll$survey)))}
s <- subset (poAll, survey == levels (poAll$survey)[sv])
s$Transect <- factor (s$Transect)

print (levels (s$Transect))
tn <- 1        # user to pick index number

cat ("Selected Transect", levels (s$Transect)[tn], "\n")
if (tn > length (levels (s$Transect))){stop ("tn has to be smaller than ", 1+length (levels (s$Transect)))}
s <- subset (s, Transect==levels (s$Transect) [tn])
s$survey <- factor (s$survey)
s$Match_Name <- factor (s$Match_Name)
cat ("Available stations to plot:", length (levels (s$Match_Name)), "\n")

cat ("Available variables to plot:\n")
print (oVarsF)
ov <- 1                                # user to pick index number

cat ("Selected variable to plot: ", oVarsF [ov], "\n")

## --------------- end of interactive user selection -------------------- ##




## ------------------- wrap interactive part into Shiny app ------------- ##
if (0){
  require ("shiny")
  if (interactive()){
    ui <- fluidPage (
      numericInput ("sv", "Survey", 1, min=1, max=7)
    )
    server <- function (input, output){
      output$value <- renderText ({input$tn})
    }
    shinyApp (ui, server)
  }
}
## ------------------------------ end of shiny --------------------------- ##




# for (sv in iX){
#  s <- subset (poAll, survey == levels (poAll$survey)[sv]) # for testing -- eventually move up for efficiency

  # cat ("Available transects on", levels (poAll$survey)[sv], ":\n")
  # print (levels (s$Transect))
  # cat ("set tn to an index representing the desired transect\n")
  # tn <- 1


    ## doubly-used stations:
  if (0){
    # 4-3 = AlongBay-3
    # 9-6 = AlongBay-6
    if (levels (s$Transect)[tn] == "AlongBay"){
      s$Transect [(s$Transect == "4") & (s$Station == "3")] <- "AlongBay"
      s$Transect [(s$Transect == "9") & (s$Station == "6")] <- "AlongBay"

      ## extended AlongBay Transect
      # AB-3, AB_S-2, AB_S-1, AB_S-0:  T6_S02, T7_S22, AB_SPTGM, AB_SPOGI
      fS <- c ("6_2", "7_22", "AlongBay_PTGR", "AlongBay_POGI")
      # nS <- -3:0
      for (k in 1:length (fS)){
        s$Transect [which (s$Match_Name == fS [k])] <- "AlongBay" ## no need to change station name
      }
    }
    if (levels (s$Transect)[tn] == "4"){
      s$Transect [(s$Transect == "AlongBay") & (s$Station == "3")] <- "4"
    }
    if (levels (s$Transect)[tn] == "9"){
      s$Transect [(s$Transect == "AlongBay") & (s$Station == "6")] <- "9"
    }
  }
    ## transect from station list for use with plot.section
    transectTemplate <- with (subset (stn, Line == levels (s$Transect)[tn]),
                              data.frame (station=Match_Name
                                          , longitude=Lon_decDegree
                                          , latitude=Lat_decDegree))

    # phT <- subset (s, Transect == levels (s$Transect)[tn])
     phT <- s

    if (length (levels (factor (phT$Match_Name))) > 2){ ## shouldn't be necessary -- what's up with Transect = NA??
      xC <- phT
      ## arrange ctd data into sections
      ## define section -- see section class http://127.0.0.1:16810/library/oce/html/section-class.html

      # png (paste0 ("~/tmp/LCI_noaa/media/CTDsections/sectionImages/", levels (poAll$survey)[sv]
      #              , " T-", levels (s$Transect)[tn]
      #              # , "%02d
      #              ,".png")
      #      , height = 8.5*200, width = 11*200, res = 300  # landscape
      #      # , height = 11*200, width = 8.5*200, res = 300 # portrait
      # )
      # layout (matrix (1:9, 3, byrow = FALSE)) # across, then down

      xCo <- sectionize (xC)


        # if (ov %in% c(4,5,6)){ # fix scale for O2, fluorescence, logPAR ## add buoyancy (8)?
        if (0){
          zR <- oRange [ov,]
        }else{
          cDF <- with (xC, data.frame (Temperature_ITS90_DegC, Salinity_PSU
                                       , Density_sigma.theta.kg.m.3
                                       , turbidity
                                       , Fluorescence_mg_m3, logPAR
                                       , Oxygen_umol_kg
                                       # , Oxygen_sat.perc.
                                        , bvf
          ))
          cDF <- sapply (1:ncol (cDF), function (i){ifelse (!is.finite (cDF[,i]), NA, cDF[,i])})
          # zR <- range (cDF [,ov], na.rm = TRUE); rm (cDF)
          zR <- quantile (cDF [,ov], probs = c(0.05, 0.95), na.rm = TRUE); rm (cDF)
        }
        # ov = 3 (turbidity), sv =7 fails. (order of x, y:  all values NA or stuck)
        if (all (is.na (zR))){
          plot (1:10, type = "n")
          text (5,5, labels = "no good data")
        }else{
          pSec (xCo
                , N = oVarsF [ov]      # logPAR does not plot
                , zCol = oCol3 [[ov]]
                , zlim = zR
                , zbreaks=NULL # change this for salinity; others?
                , custcont=10, labcex=0.6
                , showBottom=TRUE
          )
          rm (zR)
        }
        ## mark PAR at night
        #   if (oVars [ov] == "PAR"){
        #     if (is.night(xCo@data [[1]][[1]]))
        #       box (lwd = 4, col = "navy")
        #   }


      if (s$Transect[tn] == "AlongBay"){mt <- ""}else{mt <- "T"}
      mtext (paste0 (mt, levels (s$Transect)[tn], " ", levels (poAll$survey)[sv])
             , side = 3, outer = TRUE, line = -1.0, cex =1); rm (mt)

      # plot (xCo  ## large LCI map -- trouble to keep range constant -- start from scratch??
      #       , which = 99
      #       , coastline = "coastlineWorldFine"
      #       , showStations = TRUE
      #       , gird = TRUE
      #       , map.xlim = range (poAll$longitude_DD) # +c(-0.5, 0.5)
      #       # , map.ylim = range (poAll$latitude_DD)+c(-0.3, 0.3)
      #       ## , map.xlim = c(-154, -151)
      #       ## , map.ylim = c(57.5, 60.1)
      #       , clatitude = mean (range (poAll$latitude_DD)) # 59.4
      #       , clongitude = mean (range (poAll$longitude_DD)) # -152
      #       , span = 200
      #       # , showSpine = TRUE
      # )
      # if (0){  ## omit this map -- need the space
      #   plot (xCo
      #         , which = 99
      #         , coastline = "coastlineWorldFine"  ## add hi-res topography?
      #         , showStations = TRUE
      #         , showStart = TRUE
      #         , gird = TRUE
      #         # , col = "red"
      #   )
      # }
     # dev.off()
    }
#  }
# }


# EOF
