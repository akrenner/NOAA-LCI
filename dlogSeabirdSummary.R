## summarize dlog observations
rm (list = ls())
dF <- list.files("~/GISdata/LCI/dlog-seabirds/", "csv"
  , full.names = TRUE)

# bdat <- sapply (dF, function (x){read.csv (x)})
# bdat <- lapply (dF, read.csv)
# do rbind ...
bdat <- read.csv (dF [1], skipNul = TRUE
  , strip.white = TRUE
) # fileEncoding = "UTF16LE")


bsum <- aggregate (Numbe ~ Spp, bdat, sum, na.rm = TRUE)
fL <- read.csv("~/GISdata/data/fourletter-bird-codes.csv")
bsum$common <- fL$common [match (bsum$Spp, fL$forL)]

bdat$tCount <- ifelse (bdat$Type == "USER" & bdat$OnOffSurvy == "YES"
  , bdat$Numbe, NA)
bdat$tCount <- ifelse (bdat$Behavior == "FLYING", 0, bdat$tCount)
# bdat$tCount <- ifelse ((bdat$Behavior == "SCAN") |
#                          (bdat$Behavior == "WATER")
#                        , bdat$Numbe, 0)
bdat$tCount <- ifelse ((bdat$Bin == 9) | is.na (bda$Bin), 0, bdat$tCount)

head (subset (bdat, Type == "USER")[, c(15, 16, 17, 20, 24, 36)])
subset (bdat, Type == "USER")[, c(15, 16, 17, 20, 24, 36)]
head (bdat[, c(15, 16, 17, 20, 24, 36)])

# is.na (bdat$tCount) <- is.na (factor (bdat$Spp))
tmpcount <- aggregate (tCount ~ Spp, data = bdat, FUN = sum) # , na.rm = TRUE)$tCount
# bsum$tcount <- aggregate (tCount~Spp, data = bdat, FUN = sum)$tCount # , na.rm = TRUE)$tCount
bsum$tcount <- tmpcount$tCount [match (bsum$Spp, tmpcount$Spp)]


print (bsum [order (bsum$tcount, decreasing = TRUE), c(3, 2, 4)])



# plot (Latitude~Longitude, bdat)
## EOF
