## query acess DB
rm (list=ls())

# if (.Platform$OS.type == "unix"){
#   require (tidyr)
#   adb <- list()
#   AT <- system ("mdb-tables ~/GISdata/LCI/EVOS_LTM_tables/EVOS_LTM.accdb",intern=TRUE) %>%
#     strsplit(" +") %>%
#     unlist()
#   for (i in 1:length (AT)){
#     adb [[i]] <-
#       system (paste0 ("mdb-export ~/GISdata/LCI/EVOS_LTM_tables/EVOS_LTM.accdb ", AT [i])) %>%
#       read.csv(file=stdin())
#   }
# }else{
# }

atra <- read.csv("~/GISdata/LCI/EVOS_LTM_tables/manualExport/tblAllTransectsCTD.txt")
tra <- read.csv("~/GISdata/LCI/EVOS_LTM_tables/manualExport/tblTransectEvent.txt") ## trans
sta <- read.csv("~/GISdata/LCI/EVOS_LTM_tables/manualExport/tblStationEvent.txt") ## station and trans
sam <- read.csv("~/GISdata/LCI/EVOS_LTM_tables/manualExport/tblSampleEvent.txt")  ## station and sample
# ctd <- read.csv("~/GISdata/LCI/EVOS_LTM_tables/manualExport/tblCTDall.txt") ## sampleEv


sam$Type <- toupper (sam$Type)
sam$Type <- ifelse (sam$Type=="OCEAN ACIDIFICATION", "OA", sam$Type)
sam$Type <- ifelse (sam$Type=="CD", "CTD", sam$Type)
sam$Type <- factor (sam$Type)
summary (sam$Type)

##
sta$Transect <- tra$Transect [match (sta$TransectEvent, tra$TransectEvent)]
sam$Transect <- sta$Transect  [match (sam$StationEvent, sta$StationEvent)]

sam$dateF <- sam$Date
# sam$dateF <- sam$DateTimeStamp

samx <- sam
## sam <- samx
sam <- subset (sam, nchar (dateF) > 3)
badD <- numeric()
for (i in 1:nrow (sam)){
  x <- try (as.POSIXct(sam$dateF[i]))
  if (class (x)[1]=="try-error"){badD <- c (badD, i)}
}
sam$dateF [badD]
if (length (badD) > 0){
  sam <- subset (sam, 1:nrow (sam) != badD)
}
rm (badD, i, x)

sam$month <- format (as.POSIXct(sam$dateF), "%m")
sam$year <- format (as.POSIXct(sam$dateF), "%Y")


## --- summarize sampling schedule -----
sam <- subset (sam, year > 2016)
sType <- c("CTD","NUTRIENTS","OA","PHYTOPLANKTON","ZOOPLANKTON")
sam <- subset (sam, Type %in% sType)


if (0){
## reshape sam to wide
samC <- with (sam, data.frame (year=factor (year), month=factor (month)
                               , Transect=factor (Transect), Type, Depth))
reshape (samC, idvar=c ("year", "month", "Transect"), timevar="Type", direction="wide")
reshape (samC, idvar=c ("Transect"), timevar="Type", direction="wide")


require ("tidyr")
# spread (samC, key=Type, value=Depth)
# samC %>% pivot_wider(names_from=c(month,year,Transect) ,values_from=Depth)
}


sampleDF <- data.frame (expand.grid(month=1:12, year=2015:format (Sys.time(), "%Y")))
sampleDF <- data.frame (expand.grid(Transect=levels (factor (sam$Transect))
                                    , month=1:12, year=2017:2021))  ## manual set range!
sampleDF <- cbind (sampleDF, matrix (0, nrow=nrow (sampleDF)
                                     , ncol=length (sType)
                                     , dimnames = list (1:nrow (sampleDF)
                                                        , sType)))

## populate sampleDF with db records
for (i in 1:nrow (sampleDF)){
  for (j in 1:length (sType)){
    sC <- subset (sam, Transect==sampleDF$Transect [i]) %>%
                  subset (year==sampleDF$year[i]) %>%
      subset (month==sampleDF$month[i]) %>%
      subset (Type == sType [j]) %>%
      nrow()
    sampleDF [i,j+3] <- sC
  }
}
xT <- as.matrix (sampleDF [,4:ncol (sampleDF)])
rownames (xT) <- apply (sampleDF[,1:3], 1, paste, collapse="-")


xAxis <- function (side=3, Cx=1,...){
  axis (side=side, at = ((1:ncol (xT))-1)/(ncol (xT)-1)
        , labels = colnames(xT), tick = FALSE, cex.axis = Cx, ...) # axis is 0:1
}

yAxis <- function (lab, mx, CA=1, ...){
  axis (2, at = ((1:nrow (mx))-1)/(nrow (mx)-1)
        , labels = rev (lab)
        , tick = FALSE, cex.axis = CA # any bigger and labels will skip
        , ...)
}

png ("~/tmp/LCI_noaa/media/sampleTable.png", height=11*100, width=5*100, res=100)
par (las=1, mar=c(1,8,4,1))
image (t (ifelse (xT>0, 1,0)), axes=FALSE)
xAxis ()
yAxis (rownames(xT), xT)
dev.off()





sam$Type <- factor (sam$Type)
for (i in 1:length (levels (sam$Type))){
  cat ("\n\n", levels (sam$Type)[i], "\n")
  sT <- subset (sam, Type == levels (sam$Type)[i])
  print (aggregate(StationEvent~month+year+Transect, sT, FUN=length))
}


