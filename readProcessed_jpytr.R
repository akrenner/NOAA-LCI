## run this within Jupyter on workspace
## read processed csv files on server and output concatenated file

# fN <- list.files ("~/GISdata/LCI/CTD/3_processed/", ".csv", full.names = TRUE)
fN <- list.files(path = "../CTD\ Data/3_Processed\ Data\ (CSV\ files)/",
  pattern = "*.csv", recursive = TRUE, full.names = TRUE)

save ("test.RData")
# load ("test.RDataTmp")

if (0) { # troubleshooting
  fieldNames <- sapply (1:length (fN), FUN = function(i) {
    tF <- read.fwf (fN [i], width = 550)
    strtLine <- grep ("^\"Temperature", tF$V1) - 1
    if (length (strtLine) == 0) {
      strtLine <- grep ("^\"Depth", tF$V1) - 1
    }
    agF <- read.csv (fN [i], na.string = "-9999", skip = strtLine)
    return (names (agF))
  })
  sapply (1:length (fieldNames), function(i) {length (fieldNames [[i]])})

  sapply (1:length (fieldNames), function(i) {grep ("sigma", fieldNames [[i]], value = TRUE)})
}

for (i in 1:length (fN)) {
  tF <- read.fwf (fN [i], width = 550)
  strtLine <- grep ("^\"Temperature", tF$V1) - 1
  if (length (strtLine) == 0) {
    strtLine <- grep ("^\"Depth", tF$V1) - 1
  }
  agF1 <- read.csv (fN [i], na.string = "-9999", skip = strtLine)

  File.Name = gsub ("/Data/CTD\ Data//3_processed\ Data (CSV\ files)/", "", fN [i])

  fStrS <- strsplit (File.Name, "_")
  Date <- paste (fStrS [[1]][1], fStrS[[1]][2], sep = "-")
  Transect <- fStrS [[1]][3]
  Station <- fStrS [[1]][4]
  cat (File.Name, "\n")
  agF <- with (agF1, data.frame (File.Name
    , Date = as.POSIXct (gsub ("_", "-", Date))
    , Transect
    , Station
    , Temperature..ITS.90..deg.C.
    #       , Conductivity..uS.cm.
    , Pressure..Strain.Gauge..db.
    , Oxygen..SBE.43..mg.l.
    , PAR.Irradiance..Biospherical.Licor
    #       , Density..density..kg.m.3.
    #       , Specific.Conductance..uS.cm.
    , Salinity..Practical..PSU.
    #       , Oxygen.Saturation..Garcia...Gordon..ml.l.
    #       , Potential.Temperature..ITS.90..deg.C.
    #        , Density..sigma.t..kg.m.3..
    #       , Descent.Rate..m.s...WS...2
    #       , flag
  )
  )
  agF$Fluorescence.mg.m.3 <- agF1 [, grep ("^Fluorescence", names (agF1))]
  agF$Depth.salt.water <- agF1 [, grep ("^Depth", names (agF1))]
  agF$Density..sigma.theta..kg.m.3. <- agF1 [, grep ("sigma", names (agF1))]

  if ("Oxygen.Saturation..Garcia...Gordon..mg.l." %in% names (agF1)) {
    agF$Oxygen.Saturation..Garcia...Gordon..mg.l. <-
      agF1$Oxygen.Saturation..Garcia...Gordon..mg.l.
  } else {
    agF$Oxygen.Saturation..Garcia...Gordon..mg.l. <- agF1$Oxygen.Saturation..Garcia...Gordon..ml.l. * 1.42905 # per USGS memo 2011.03
  }

  if ("Upoly.0..ECO.FLNTUS" %in% names (agF1)) {
    agF$Upoly.0..ECO.FLNTUS <- agF1$Upoly.0..ECO.FLNTUS
  } else {
    agF$Upoly.0..ECO.FLNTUS <- NA
  }

  if (i == 1) {
    physOc <- agF
  } else {
    physOc <- rbind (physOc, agF)
  }
}


rm (agF, agF1, i, fN)

save.image ("processed.RData")
