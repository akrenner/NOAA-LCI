## spit-out CTD metadata of sample transect
rm (list = ls())


fL <- list.files("~/GISdata/LCI/CTD-processing/Workspace/", pattern = "2018_06-22_T9", recursive = TRUE
  , full.names = TRUE)


aD <- sapply (seq_along(fL), function(i) {readLines(fL [i], n = 200)})
mt <- sapply (seq_len(ncol(aD)), function(i) {grep ("^\\* cast", aD [, i], value = TRUE)})

strt <- as.numeric (substr(mt, 41, 46))
stp <- as.numeric (substr(mt, 51, 56))
smpls <- stp - strt

plot (-1 * smpls, type = "l")




load ("~/tmp/LCI_noaa/cache/ctdwallSetup.RData")   # from CTDwallSetup.R
# px <- subset (poAll, )
px <- poAll [grep ("2018_06-22_T9", poAll$File.Name), ]

px <- subset (poAll, Transect == 9)
px <- subset (px, Date == "2018-06-22")
summary (factor (px$File.Name))
