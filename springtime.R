## timing of spring of temperature and fluorescence + phytoplanton
## use SWMP data

# fit logistic or similar curve, then take inflection points


setwd ("~/myDocs/amyfiles/NOAA-LCI/")
rm (list = ls()); load ("~/tmp/LCI_noaa/cache/SeldTemp.RData")  ## from SeldoviaTemp.R
require (zoo)


## QAQC ---  report and fix!!
sldvia$chlfluor <- ifelse (sldvia$chlfluor > 20, NA, sldvia$chlfluor)
sldviaS$chlfluor <- ifelse (sldviaS$chlfluor > 20, NA, sldviaS$chlfluor)
homer$chlfluor <- ifelse (homer$chlfluor > 20, NA, homer$chlfluor)
homerS$chlfluor <- ifelse (homerS$chlfluor > 20, NA, homerS$chlfluor)



## auto and cross-correlations of Seldovia and Homer temperature, salinity, fluorescence

## spectrum
require ("psd")

# x <- ifelse (is.na (oT$x), 0, oT$x)
# pst <- pspectrum (x)
# xSp <- psdcore (oT$x, na.action = na.omit)
#
# require (specFFT.R)
# spec.fft (x)



vN <- c(2,4:7,11:13)
names (homer)[vN]

## spectrum only
# pdf (paste0 ("~/tmp/LCI_noaa/media/SWMP-spectra", ".pdf"))
png ("~/tmp/LCI_noaa/media/SWMP-spectra%02d.png", width = 800, height = 800, res = 120)
par (mar = c(3,4,2,1))
for (i in vN){
  oT <- homer [,c(1,i)]
  cat (names (homer)[i], "\n")
  ## spectrum -- require ("psd")
  x <- ifelse (is.na (oT [,2]), 0, oT [,2])
  x <- scale (x)
  xSp <- spectrum (x, main = names (homer)[i], plot = TRUE
                   , ylim = c(1e-8, 1e5)) # slow! run this all in parallel?
  # plot (xSp)
}
dev.off()
merge.png.pdf ("~/tmp/LCI_noaa/media/SWMP-spectra.pdf"
               , pngFiles = list.files ("~/tmp/LCI_noaa/media", "SWMP-spectra\\d+.png", full.names = TRUE)
               , deletePngFiles=TRUE)



for (i in vN){
  pdf (paste0 ("~/tmp/LCI_noaa/media/SWMP", names (homer)[i], ".pdf"))
  oT <- homer [,c(1,i)]
  oT$x <- homerS[match (oT$datetimestamp, homerS$datetimestamp),i]
  oT$Sx <- sldvia[match (oT$datetimestamp, sldvia$datetimestamp),i]
  oT$SSx <- sldviaS[match (oT$datetimestamp, sldviaS$datetimestamp),i]

  cat (names (homer)[i], "\n")
  ccf (oT [,2], oT$Sx, lag.max = 4*24*30*2, na.action = na.pass, type = "correlation"
       , main = paste (names (homer)[i], "Homer-Seldovia deep crosscorrelation")
       , axes = FALSE, xlab = "lag [weeks]")
  axis (1, at = c (-9:9) * 4*24*7, labels = -9:9)
  axis (2); box()
  abline (v = 0, col = "gray")
  try ({ccf (oT [,2], oT$x, lag.max = 4*24*30, na.action = na.pass,
       main = paste (names (homer)[i], "Homer-Homer shallow crosscorrelation")
       , axes = FALSE, xlag = "lag 'weeks")
  axis (1, at = c (-9:9) * 4*24*7, labels = -9:9)
  axis (2); box()
  abline (v = 0, col = "gray")})
  acf (oT [,2], na.action = na.pass, lag.max = 4*24*30, type = "correlation"
       , main = paste (names (homer)[i], "Homer autocorrelation")
       , axes = FALSE, xlab = "lag [weeks]")
  axis (1, at = c (-9:9) * 4*24*7, labels = -9:9)
  axis (2); box()
  abline (v = 4*24*c(1:7))
  # ## spectrum -- require ("psd")
  # x <- ifelse (is.na (oT [,2]), 0, oT [,2])
  # xSp <- spectrum (x) # slow! run this all in parallel?
  # plot (xSp)
    try (plot (oT [,2], oT$x, main = paste (names (homer)[i], "Homer deep vs shallow")
        , xlab = "homer deep", ylab = "homer shallow"))
  ## use July data only, comparing Homer and Seldovia
  oT$M <- format (oT$datetimestamp, "%m")
  oT <- subset (oT, M == "07")

  try (plot (oT [,2], oT$Sx
             , main = paste (names (homer)[i], "Homer vs Seldovia -- July only")
             , xlab = "Homer deep", ylab = "Seldovia deep", asp=1))
 dev.off()
}






## start with sldvia (not sldviaS)

sldvia$year <- factor (sldvia$year)

# s <- subset (sldvia, year == levels (sldvia$year)[15] & (month <= 7))
#
# s2 <- aggregate (temp~jday, FUN = mean, na.rm = TRUE)
# s2$chlfluor <- aggregate (chlfluor~jday, FUN = mean, na.rm = TRUE)$chlfluor
# s<- s2
#
# s$chlfluor <- na.approx(s$chlfluor, x = s$datetimestamp) ## no NA for cumulative functions!
#
# s$cumF <- cumsum (s$chlfluor)
# plot (temp~jday, data = s, type = "l")
#
# plot (cumF~jday, data = s, type = "l")
# plot (cumsum(chlfluor)~jday, data = s, type = "l")


## logistic/sigmoidal curve
logF <- function (L, k, x0, b, x){
  L/(1+exp (-k*(x-x0)))+b
}

prepDF <- function (df, i){
  require("zoo")
  s <- subset (df, year == levels (df$year)[i])
  s <- subset (s, month %in% 2:8)
#  s <- subset (s, month <= 9)
  s$jday <- s$jday + as.numeric (format (s$datetimestamp, "%H"))/24 +
    as.numeric (format (s$datetimestamp, "%M"))/24 / 60
  s$chlfluor <- na.approx(s$chlfluor, x = s$jday, na.rm = FALSE)
  s$cumFlu <- cumsum(s$chlfluor)
  return (s)
}


# starting values
svt <- list (L = 12, k = -1, x0 = 90, b = 5)
svf <- list (L = 100, k = -1, x0 = 90, b = 5)

# fit <- nls (temp~logF (L, k, x0, b, x = jday), data = s
#             , start = sv)
# x <- seq (1:210)
# fit.val <- data.frame (jday = x, temp = predict(fit, list (jday = x)))
# lines (temp~jday, fit.val)
# rm (sv, logF, fit)
#
#
# ## extract values, esp. x0
# coefficients (fit)
# summary (fit)$parameters[3,1:2]


cT <- t (sapply (1:length (levels (sldvia$year)), function (i){
  s <- prepDF (sldvia, i)
  if (all (is.na (s$temp))){rep (NA, 2)}else{
    fit <- try (nls (temp~logF (L, k, x0, b, x = jday), data = s, start = svt))
    if (class (fit) != "try-error"){
      summary (fit)$parameters [3,1:2]
    }else{
      rep (NA, 2)
    }}
}))

cF <- t (sapply (1:length (levels (sldvia$year)), function (i){
  s <- prepDF (sldvia, i)
  if (all (is.na (s$cumFlu))){rep (NA, 2)}else{
    fit <- try (nls (cumFlu~logF (L, k, x0, b, x = jday), data = s, start = svt))
    if (class (fit) != "try-error"){
      summary (fit)$parameters [3,1:2]
    }else{
      rep (NA, 2)
    }}
}))


for (i in 1:length (levels (sldvia$year))){
  s <- prepDF (sldvia, i)
  cat (i, " ")
  if (all (is.na (s$temp))){rep (NA, 2)}else{
    fit <- try (nls (temp~logF (L, k, x0, b, x = jday), data = s, start = svt))
    plot (temp~jday, s)
    plot (cumFlu~jday, s)

        # if (class (fit) != "try-error"){
    #   summary (fit)$parameters [3,1:2]
    # }else{
    #   rep (NA, 2)
    # }
    }
}


pdf ("~/tmp/LCI_noaa/media/sprintTimeFluoresc.pdf", height = 6, width = 12)
par (mfrow = c(1,2))
sL <- list (sldvia, sldviaS, homer, homerS)
for (j in 1:length (sL)){
  # s <- sldvia
  s <- sL [[j]]
  # s$jtime <- s$jday + as.numeric (format (s$datetimestamp, "%H"))/24 +
  #   as.numeric (format (s$datetimestamp, "%M"))/24 / 60
  # s <- s [order (s$jtime),]
  # plot (cumsum (chlfluor)~jtime, s)

  s <- aggregate (chlfluor~jday+year, sldvia, FUN = mean, na.rm = TRUE)
  s <- aggregate (chlfluor~jday, s, FUN = mean, na.rm = TRUE)
  plot (cumsum(chlfluor)~jday, s, lwd = 2, type = "l")
  plot (chlfluor~jday, s, lwd = 2, type = "l")
}
dev.off()


pdf ("~/tmp/LCI_noaa/media/sprintTimeFluoAnn.pdf", height = 6, width = 12)
par (mfrow = c(1,2))
sL <- list (sldvia, sldviaS, homer, homerS)
for (i in 1:length (levels (sldvia$year))){
  s <- subset (sldvia, year == levels (sldvia$year)[i])
  if (!all (is.na (s$chlfluor))){
  plot (cumsum(chlfluor)~jday, s, lwd = 2, type = "l")
  plot (chlfluor~jday, s, lwd = 2, type = "l", main = levels (sldvia$year)[i])
}}
dev.off()


cbind (as.numeric (levels (sldvia$year)),
sapply (1:length (levels (sldvia$year)), function (i){
sum (!is.na (subset (homerS, year == levels (sldvia$year)[[i]])$chlfluor))
}))

cbind (as.numeric (levels (sL)),
       aggregate (chlfluor~year, sL[[i]], FUN = function (x){
         XXX
       }))





pdf ("~/tmp/LCI_noaa/media/springtime.pdf")
for (i in 2:length (levels (sldvia$year))){
  s <- prepDF (sldvia, i)
  plot (temp~datetimestamp, data = s, main = levels (sldvia$year)[i])
  fit <- nls (temp~logF (L, k, x0, b, x = jday), data = s, start = svt)
  fit.val <- data.frame (jday = x, temp = predict(fit, list (jday = x)))
  lines (temp~jday, fit.val, lwd = 2, col = "orange")
}
rm (i, s, fit, fit.val)

for (i in 2:length (levels (sldvia$year))){
  s <- prepDF (sldvia, i)
  s$cumFluor <- cumsum (s$chlfluor)
  x <- try (plot (cumFluor~jday, data = s, main = levels (sldvia$year)[i]))
  if (class (x) != "try-error"){
    fit <- nls (cumFluor~logF (L, k, x0, b, x = jday), data = s, start = svf)
    fit.val <- data.frame (jday = x, cumFluor = predict(fit, list (jday = x)))
    lines (cumFluor~jday, fit.val, lwd = 2, col = "green")
  }
}

dev.off()

rm (i, s, fit, fit.val)