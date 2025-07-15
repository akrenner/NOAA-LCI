## timing of spring of temperature and fluorescence + phytoplanton
## use SWMP data

# fit logistic or similar curve, then take inflection points


## temperature: difference between shallow and deep spring transition

## impute NAs from long-term mean
## combine Seldovia&Homer?? -- later, keep it simple for now





if (.Platform$OS.type != "unix") {setwd ("~/myDocs/amyfiles/NOAA-LCI/")} else {
  setwd ("~/Documents/amyfiles/NOAA/NOAA-LCI/")
}
rm (list = ls()); load ("~/tmp/LCI_noaa/cache/SeldTemp.RData")  ## from SeldoviaTemp.R
dir.create("~/tmp/LCI_noaa/media/spring/", recursive = TRUE, showWarnings = FALSE)
require (zoo)



as.num <- function(x) {
  if (class (x) == "factor") {
    x <- as.numeric (levels (x))[x]
  }
  x
}


## fill in all necessary NAs -- move this to SeldoviaTemp.R ?
## do this after aggregation to dailys?
## need to make sure even climatologies have all dates

# insertMA <- function (df, start = as.POSIXct ("2001-07-12 08:00 -9"), end = Sys.time()){
#   dtsmp <- start + 15*60 * (1:floor (as.numeric (difftime (end, start, units = "hours"))*4))
#   newDf <- data.frame (df [match (dtsmp, df$datetimestamp),])
#   newDf$datetimestamp <- dtsmp
#   if (length (levels (factor (df$station))) > 1){stop ("more than one station in DF")}
#   newDf$station <- df$station [1]
#   newDf$jday <- as.numeric (strftime (newDf$datetimestamp, format = "%j"))
#   newDf$week <- as.numeric (strftime(newDf$datetimestamp, format='%U'))
#   newDf$week <- as.numeric (strftime(newDf$datetimestamp, format='%m'))
#   newDf$year <- as.numeric (strftime (newDf$datetimestamp, format = '%Y'))
#   newDf
# }
#
# sldvia <- insertMA (sldvia)
# sldviaS <- insertMA (sldviaS)
# homer <- insertMA (homer)
# homerS <- insertMA (homerS)
# rm (insertMA)


# dtsmp <- min (sL$datetimestamp, na.rm = TRUE) + 15*60 *
#   1:floor (as.numeric (difftime (max (sL$datetimestamp), min (sL$datetimestamp), units = "hours"))*4)
# sldvia <- data.frame (datetimestamp = dtsmp, sldvia [match (dtsmp, sldvia$datetimestamp),])
# sldviaS <- data.frame (datetimestamp = dtsmp, sldviaS [match (dtsmp, sldviaS$datetimestamp),])
# homer <- data.frame (datetimestamp = dtsmp, homer [match (dtsmp, homer$datetimestamp),])
# homerS <- data.frame (datetimestamp = dtsmp, homerS [match (dtsmp, homerS$datetimestamp),])
# sL <- data.frame (datetimestamp = dtsmp, sL [match (dtsmp, sL$datetimestamp),])
# rm (dtsmp)



vN <- c(2, 4:7, 11:13)
names (homer)[vN]

## spectrum only
if (0) {
  # pdf (paste0 ("~/tmp/LCI_noaa/media/SWMP-spectra", ".pdf"))
  ## what are the main frequencies in TS autocorrelations? -- just exploratory
  ## what can be learned from fluourescence, any use of CTD-fluorescence? Length of bloom?
  png ("~/tmp/LCI_noaa/media/spring/SWMP-spectra%02d.png", width = 800, height = 800, res = 120)
  par (mar = c(3, 4, 2, 1))
  for (i in vN) {
    oT <- homer [, c(1, i)]
    cat (names (homer)[i], "\n")
    ## spectrum --
    if (1) {
      require ("psd")
      x <- ifelse (is.na (oT [, 2]), 0, oT [, 2])
      x <- scale (x)
      xSp <- spectrum (x, main = names (homer)[i], plot = TRUE
        , ylim = c(1e-8, 1e5)) # slow! run this all in parallel?
    } else {
      require ("spectral") # can handle NAs
      xSp <- spec.fft (y = as.numeric (scale (oT [, 2])), x = as.numeric (oT$datetimestamp))
    }
    plot (xSp)
  }
  dev.off()
  require ("grid")
  merge.png.pdf ("~/tmp/LCI_noaa/media/spring/SWMP-spectra.pdf"
    , pngFiles = list.files ("~/tmp/LCI_noaa/media", "SWMP-spectra\\d+.png", full.names = TRUE)
    , deletePngFiles = TRUE)


  for (i in vN) {
    ## scale of fluorescence and other measures? length of blooms?
    ## relationship between homer-seldovia, shallow-deep
    pdf (paste0 ("~/tmp/LCI_noaa/media/spring/SWMP-TSanalysis", names (homer)[i], ".pdf"))
    oT <- homer [, c(1, i)]
    oT$x <- homerS[match (oT$datetimestamp, homerS$datetimestamp), i]
    oT$Sx <- sldvia[match (oT$datetimestamp, sldvia$datetimestamp), i]
    oT$SSx <- sldviaS[match (oT$datetimestamp, sldviaS$datetimestamp), i]

    cat (names (homer)[i], "\n")
    ccf (oT [, 2], oT$Sx, lag.max = 4 * 24 * 30 * 2, na.action = na.pass, type = "correlation"
      , main = paste (names (homer)[i], "Homer-Seldovia deep crosscorrelation")
      , axes = FALSE, xlab = "lag [weeks]")
    axis (1, at = c (-9:9) * 4 * 24 * 7, labels = -9:9)
    axis (2); box()
    abline (v = 0, col = "gray")
    try ({ccf (oT [, 2], oT$x, lag.max = 4 * 24 * 30, na.action = na.pass,
      main = paste (names (homer)[i], "Homer-Homer shallow crosscorrelation")
      , axes = FALSE, xlag = "lag 'weeks")
    axis (1, at = c (-9:9) * 4 * 24 * 7, labels = -9:9)
    axis (2); box()
    abline (v = 0, col = "gray")})
    acf (oT [, 2], na.action = na.pass, lag.max = 4 * 24 * 30, type = "correlation"
      , main = paste (names (homer)[i], "Homer autocorrelation")
      , axes = FALSE, xlab = "lag [weeks]")
    axis (1, at = c (-9:9) * 4 * 24 * 7, labels = -9:9)
    axis (2); box()
    abline (v = 4 * 24 * c(1:7))
    # ## spectrum -- require ("psd")
    # x <- ifelse (is.na (oT [,2]), 0, oT [,2])
    # xSp <- spectrum (x) # slow! run this all in parallel?
    # plot (xSp)
    try (plot (oT [, 2], oT$x, main = paste (names (homer)[i], "Homer deep vs shallow")
      , xlab = "homer deep", ylab = "homer shallow"))
    ## use July data only, comparing Homer and Seldovia
    oT$M <- format (oT$datetimestamp, "%m")
    oT <- subset (oT, M == "07")

    try (plot (oT [, 2], oT$Sx
      , main = paste (names (homer)[i], "Homer vs Seldovia -- July only")
      , xlab = "Homer deep", ylab = "Seldovia deep", asp = 1))
    dev.off()
  }
  rm (vN)
}




## start with sldvia (not sldviaS)
save.image("~/tmp/LCI_noaa/cache/swmpTScach.RData")
## rm (list = ls ()); load ("~/tmp/LCI_noaa/cache/swmpTScach.RData")


if (0) {
  ## nonlinear tryouts
  sldvia$t <- as.numeric (sldvia$datetimestamp)
  per <- 365.25 * 24 * 3600
  reslm <- lm (temp ~ sin (2 * pi / per * t) + cos (2 * pi / per * t), data = sldvia)
  lines (fitted (reslm) ~ sldvia$t, col = 4, lty = 2)




  s <- subset (sldvia, year == levels (sldvia$year)[15] & (month <= 7))

  s2 <- aggregate (temp ~ jday, FUN = mean, na.rm = TRUE)
  s2$chlfluor <- aggregate (chlfluor ~ jday, FUN = mean, na.rm = TRUE)$chlfluor
  s <- s2

  s$chlfluor <- na.approx(s$chlfluor, x = s$datetimestamp) ## no NA for cumulative functions!

  s$cumF <- cumsum (s$chlfluor)
  plot (temp ~ jday, data = s, type = "l")

  plot (cumF ~ jday, data = s, type = "l")
  plot (cumsum(chlfluor) ~ jday, data = s, type = "l")
}



## fit sine curve
## see https://stats.stackexchange.com/questions/60994/fit-a-sinusoidal-term-to-data
## use non-linear fit, rather than
## lm (y ~ sin (2*pi / per*t)+ cos (2*pi/per*t))
## for ease of interpreting parameters


# y <- c(11.622967, 12.006081, 11.760928, 12.246830, 12.052126, 12.346154, 12.039262, 12.362163, 12.009269, 11.260743, 10.950483, 10.522091,  9.346292,  7.014578,  6.981853,  7.197708,  7.035624,  6.785289, 7.134426,  8.338514,  8.723832, 10.276473, 10.602792, 11.031908, 11.364901, 11.687638, 11.947783, 12.228909, 11.918379, 12.343574, 12.046851, 12.316508, 12.147746, 12.136446, 11.744371,  8.317413, 8.790837, 10.139807,  7.019035,  7.541484,  7.199672,  9.090377,  7.532161,  8.156842,  9.329572, 9.991522, 10.036448, 10.797905)
# t <- 18:65
# res <- nls(y ~ A*sin(omega*t+phi)+C, data=data.frame(t,y)
#            , start=list(A=1,omega=2*pi/20,phi=1,C=1))
# co <- coef(res)
# fit <- function(x, a, b, c, d) {a*sin(b*x+c)+d}
# # Plot result
# plot(x=t, y=y)
# curve(fit(x, a=co["A"], b=co["omega"], c=co["phi"], d=co["C"]), add=TRUE ,lwd=2, col="steelblue")

## fit sign curve
# set.seed(3)
# sineF <- function (A, w, t0, omega)
#
# sineFit <- nls (temp ~ A *sin (omega*jday+phi)+C,
#                 data = sldvia, start = list (A = 1, omega = 365, C = 1, phi = 1))
# co <- coef (sineFit)
# fit <- function (x,a,b,c,d){a*sin(a*x+c)+d}





if (0) {
  plot (temp ~ jday, sldvia)
  curve (fit (x = sldvia$temp, a = co ["A"], b = co ["omega"], c = co ["phi"], d = co ["C"])
    , add = TRUE, lwd = 2, col = "steel")
}

###############################
## logistic/sigmoidal curve ##
###############################

logF <- function(L, k, x0, b, x) {
  L / (1 + exp (-k * (x - x0))) + b
}

sineF <- function(A, omega, phi, x, C) {
  A * sin (omega * x + phi) + C
}



# starting values
svt <- list (L = 12, k = -1, x0 = 90, b = 5)
svf <- list (L = c(10, 500), k = c(-10, -1, 1, 10), x0 = 200, b = c (0, 500))
svf <- list (L = c(10, 500, 10e4), k = c(-10, -1, 1, 10), x0 = 200, b = c (0))
svf <- list (L = 1000, k = 1, x0 = 200, b = 0)
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





## insert NAs for missing days
## full time-line of all possible SWMP times
sL <- rbind (sldvia, sldviaS, homer, homerS)
rm (sldvia, sldviaS, homer, homerS)
# sL <- subset (sL, year < format (Sys.time(), "%Y"))
sL <- subset (sL, year < max (sL$year))  # can't fit data to incomplete year
## find first chlfluor measurement -- remove all empty years prior to first instrument installation (Feb 2007)
sL <- subset (sL, datetimestamp >= min (subset (sL, !is.na (chlfluor))$datetimestamp))


sL$station <- factor (sL$station)
sL$year <- factor (sL$year)
sL$jday <- factor (sL$jday)


#  plot (chlfluor~datetimestamp, sL, subset = station == "SeldoviaShallow")

## climatology per station
# cliFL <- lapply (1:length (levels (sL$station)), FUN = function (i){
#   x <- subset (sL, station == levels (sL$station)[i])
#   s <- aggregate (chlfluor~jday+year, x, FUN = mean, na.rm = TRUE)
#   s <- aggregate (chlfluor~jday, s, FUN = mean, na.rm = TRUE)
#   s
# })
# summary (cliFL [[1]])

## means of means: daily means, then climatologies of daily means
cliFL <- aggregate (chlfluor ~ station + jday + year, data = sL, FUN = mean, na.rm = TRUE)
cliTL <- aggregate (temp ~ station + jday + year, data = sL, FUN = mean, na.rm = TRUE)
## climatologies
cliFL <- aggregate (chlfluor ~ station + jday, data = cliFL, FUN = mean, na.rm = TRUE)
cliTL <- aggregate (temp ~ station + jday, data = cliTL, FUN = mean, na.rm = TRUE)
cliFL$chlfluorSD <- aggregate (temp ~ station + jday + year, data = sL, FUN = sd, na.rm = TRUE)$chlfluor
cliTL$tempSD <- aggregate (temp ~ station + jday, data = cliTL, FUN = sd, na.rm = TRUE)$tempSD

if (class (cliFL$jday) == "factor") {cliFL$jday <- as.num (cliFL$jday)}
summary (subset (cliFL, cliFL$station == levels (cliFL$station)[[2]]))
if (class (cliTL$jday) == "factor") {cliTL$jday <- as.num (cliTL$jday)}
summary (subset (cliTL, cliTL$station == levels (cliFL$station)[[2]]))


save.image ("~/tmp/LCI_noaa/cache/springtiming2.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/springtiming2.RData")

##############################################
## insert NAs and na.approx them XXXX
cliFL <- cliFL [order (cliFL$station, cliFL$jday), ]
cliT <- data.frame (station = factor (rep (levels (cliFL$station), each = 366))
  , jday = rep (1:366, length (levels (cliFL$station))))
cliT$chlfluor <- cliFL$chlfluor [match (paste (cliT$station, cliT$jday)
  , paste (cliFL$station, cliFL$jday))]
cliT$temp <- cliTL$temp [match (paste (cliT$station, cliT$jday)
  , paste (cliTL$station, cliTL$jday))]

cliT <- rbind (subset (cliT, (jday == 366) & (station == "HomerDeep")), cliT)
cliT$chlfluor <- na.approx(cliT$chlfluor)
cliT$temp <- na.approx(cliT$temp)
cliT <- cliT [2:nrow (cliT), ]
cliFL <- cliT; rm (cliT, cliTL)
##############################################


## fit logistic curve to each climatology
cliFit <- lapply (1:length (levels (cliFL$station)), FUN = function(j) {
  #  s <- cliFL [[j]]
  s <- subset (cliFL, station == levels (cliFL$station)[j])
  s$csum <- cumsum (s$chlfluor)
  fit <- nls (csum ~ logF (L, k, x0, b, jday), data = s, start = svf)
  fit
})
tempCliFit <- lapply (1:length (levels (cliFL$station)), FUN = function(j) {
  s <- subset (cliFL, station == levels (cliFL$station)[j])
  fit <- nls (temp ~ logF (L, k, x0, b, jday), data = s, start = svf)
  fit
})



## for each station: climatology of spring bloom
pdf ("~/tmp/LCI_noaa/media/spring/TimeFluorescClimatology-bySite.pdf", height = 6, width = 12)
par (mfrow = c(1, 2))
for (j in 1:length (levels (cliFL$station))) {
  # for (j in 1:length (cliFL)){
  s <- subset (cliFL, station == levels (cliFL$station)[j])
  s$csum <- cumsum (s$chlfluor)  ## no NAs in climatology
  plot (chlfluor ~ jday, s, lwd = 2, type = "l", col = "black"
    , main = levels (cliFL$station)[j]
    # , main = c("Seldovia-deep", "Seldovia-shallow", "Homer-deep", "Homer-shallow")[j]
    , ylab = "chlorophyll"
    , ylim = c (0, 15)
  )
  plot (csum ~ jday, s, lwd = 2, type = "l", ylab = "cumulative chlorophyl"
    , ylim = c(0, 1200))
  # fit <- nls (csum~logF (L,k,x0,b,x=jday), data = s, start = svf)
  # fit.val <- data.frame (jday = s$jday, csum = predict (fit, list (jday = s$jday)))
  # lines (csum~jday, fit.val, col = "blue")
  fit.val <- data.frame (jday = s$jday, csum = predict (cliFit [[j]], list (jday = s$jday)))
  lines (csum ~ jday, fit.val, col = "red", lwd = 2, lty = "dashed")
  # mtext (text = levels (cliFL$station)[j], outer = FALSE, line = 0, side = 1)
}
dev.off()


## same as 4x4 panel

## order factorr cliFL$station
cliFL$station <- factor (as.character (cliFL$station), ordered = TRUE)
levels (cliFL$station) <- levels (cliFL$station)[c(2, 4, 1, 3)]

require (lattice)
pdf ("~/tmp/LCI_noaa/media/spring/Climatologypanel-site_fluor.pdf"
  , width = 6, height = 4)
print (xyplot (chlfluor ~ jday | station, data = cliFL, type = "l", as.table = TRUE
  , ylab = "chlorophyll", lwd = 2, col = "green"))
dev.off()
pdf ("~/tmp/LCI_noaa/media/spring/Climatologypanel-site_temp.pdf"
  , width = 6, height = 4)
print (xyplot (temp ~ jday | station, data = cliFL, type = "l", as.table = TRUE
  , ylab = "temperature", lwd = 2, col = "blue"))
dev.off()



## same for cumulative data
pdf ("~/tmp/LCI_noaa/media/spring/fluorClimatologypanel-site-cum.pdf"
  , width = 6, height = 4)
par (mfrow = c(2, 2))
for (j in 1:length (levels (cliFL$station))) {
  s <- subset (cliFL, station == levels (cliFL$station)[j])
  s$csum <- cumsum (s$chlfluor)  ## no NAs in climatology
  plot (csum ~ jday, s, lwd = 2, type = "l", ylab = "cumulative chlorophyl"
    , ylim = c(0, 1200), main = levels (cliFL$station)[j])
  fit.val <- data.frame (jday = s$jday, csum = predict (cliFit [[j]], list (jday = s$jday)))
  lines (csum ~ jday, fit.val, col = "red", lwd = 2, lty = "dashed")
}
dev.off()
rm (svf, svt)



## summary of how many valid measurements per year
fluorAvail <- sapply (1:length (levels (sL$station)), FUN = function(j) {
  s <- subset (sL, station == levels (station)[j])  # use homerS for now
  sapply (1:length (levels (s$year)), function(i) {
    sum (!is.na (subset (s, year == levels (s$year)[[i]])$chlfluor))
  })
})
colnames (fluorAvail) <- levels (sL$station)
row.names (fluorAvail) <- levels (sL$year)
print (fluorAvail)

## ==> SeldoviaShallow is best, most data-rich source!


## fit logistic curve to individual years of Homer-Shallow (nice curve?)[2]
## or SeldoviaShallow (most data) [4]
j <- 4
j <- 2

# for (j in 1:length (levels (sL$station))){
# cat ("\n\n\n", j, "\n")

## fix NAs in sL
sLo <- sL
# sL <- sLo
sLa <- aggregate (chlfluor ~ jday + year + station, sL, FUN = mean, na.rm = TRUE)
# sLa$temp <- aggregate (temp~jday+year+station, sL, FUN = mean, na.rm = TRUE)$temp  # N of NAs differ -- more complicated to merge
sL <- sLa

## insert missing NAs -- but ONLY for partial year, not for completely missing years
sL$jday <- as.num (sL$jday)
sL$year <- as.num (sL$year)
lS <- levels (sL$station); Ny <- length (min (sL$year):max (sL$year)); Ns <- length (lS)
sL$station <- as.character (sL$station)

sLt <- data.frame (jday = rep (1:366, times = Ns * Ny, each = 1)
  , year = rep (min (sL$year):max (sL$year), Ns, each = 366)
  , station = rep (lS, each = 366 * Ny)
)

sLt$chlfluor <- sL$chlfluor [match (paste (sLt$station, sLt$jday, sLt$year)
  , paste (sL$station, sL$jday, sL$year))]
sLt$chlfluorNNa <- ifelse (is.na(sLt$chlfluor)
  , cliFL$chlfluor [match (paste0 (as.character (sLt$station), sLt$jday)
    , paste0 (as.character (cliFL$station), cliFL$jday))]
  , sLt$chlfluor)   # XXXXXXX
# sLt$climFluor <- cliFL$chlfluor [match (paste0 (as.character (sLt$station), sLt$jday)
#                                         , paste0 (as.character (cliFL$station), cliFL$jday))]
sLt$temp <- sL$temp [match (paste (sLt$station, sLt$jday, sLt$year)
  , paste (sL$station, sL$jday, sL$year))]

sLt$station <- factor (sLt$station)
sLt$year <- factor (sLt$year)

sL <- sLt; rm (sLt)
rm (lS, Ny, Ns)


i <- 8; j <- 3
yearFits <- lapply (1:length (levels (sL$station)), function(j) {
  dat <- subset (sL, station == levels (sL$station)[j])
  #   dat$year <- factor (dat$year)  ## reset year levels
  ## aggregate to jday, same as cliFL
  # dat <- aggregate (chlfluorNNa~jday+year, data = dat, FUN = mean, na.rm = TRUE)
  # dat$jday <- as.numeric (levels (dat$jday))[dat$jday]  # already numeric!

  svf <- as.list (coefficients(cliFit [[j]]))

  yFit <- lapply (1:length (levels (dat$year)), FUN = function(i) {
    s <- subset (dat, year == levels (dat$year)[i])
    if (sum (!is.na (s$chlfluor)) < 5) {  ## cause a try-error to be caught below
      fit <- try (log ("a"), silent = TRUE)
    } else {
      s$csum <- cumsum (s$chlfluorNNa)  ## use imputed version -- no NAs in chlfluorNNa
      fit <- try (nls (csum ~ logF (L, k, x0, b, x = jday), data = s, start = svf)
        , silent = TRUE)
      if (class (fit) == "try-error") {cat (paste0 ("nl fit failed, i=", i, " j="
        , j, "\n", fit, "\n\n"))}
    }
    fit
  })
  names (yFit) <- paste0 ("Y", levels (dat$year))
  yFit
})
## also fit yearly temperature data to compare t0s.
yTempFit <- lapply (1:length (levels (sL$station)), function(j) {
  dat <- subset (sL, station == levels (sL$station)[j] & jday < 250)
  svf <- as.list (coefficients(tempCliFit [[j]]))

  yFit <- lapply (1:length (levels (dat$year)), FUN = function(i) {
    s <- subset (dat, year == levels (dat$year)[i])
    if (sum (!is.na (s$temp)) < 5) { ## cause a try-error to be caught below
      fit <- try (log ("a"), silent = TRUE)
    } else {
      fit <- try (nls (temp ~ logF (L, k, x0, b, x = jday), data = s, start = svf), silent = TRUE)
    }
    fit
  })
  names (yFit) <- paste0 ("Y", levels (dat$year))
  yFit
})



yearCoef <- lapply (1:length (yearFits), function(j) {
  fits <- yearFits [[j]]
  coefs <- t (sapply (1:length (fits), function(i) {
    fit <- fits [[i]]
    if (class (fit) == "try-error") {
      cf <- as.numeric (rep (NA, 5))
      names (cf) <- c("L", "k", "x0", "b", "p365") # as.numeric (c (L=NA, k=NA,x0=NA,b=NA))
    } else {
      cf <- coefficients(fit)
      p365 <- try (predict (fit, list (jday = 365)))
      if (class (p365) == "try-error") {
        cf <- c (cf, p365 = NA)
      } else {
        cf <- c (cf, p365 = predict (fit, list (jday = 365)))
      }
    }
    # cf <- data.frame (cf, year = fits [[j]][[2]])
    cf
  }))
  # coef <- cbind (coefs, year = as.numeric (gsub ("^Y", "", names (fits))))
  #  row.names(coefs) <- names (fits)
  coefs <- data.frame (station = rep (levels (sL$station)[j], nrow (coefs))
    , year = as.numeric (rep (gsub ("^Y", "", names (fits))))
    , coefs)
  coefs
})
# names (yearCoef) <- levels (sL$station)
yearCoef <- do.call (rbind, yearCoef)
# colnames(yearCoef) <- names (svf)
aSum <- aggregate (chlfluorNNa ~ year + station, sL, sum)
yearCoef$sum <- aSum$chlfluorNNa [match (with (yearCoef, paste (station, year))
  , with (aSum, paste (station, year)))]
rm (aSum)
yearCoef



j <- 1; i <- 1
pdf ("~/tmp/LCI_noaa/media/spring/TimeFluoAnnual.pdf", height = 6, width = 12)
par (mfrow = c(1, 2))
for (j in 1:length (levels (sL$station))) {
  dat <- subset (sL, station == levels (sL$station)[j])
  dat$year <- factor (dat$year)
  for (i in 1:length (levels (dat$year))) {
    #  cat ("\n\n", i, "\n")
    s <- subset (dat, year == levels (dat$year)[i])  # use homerS for now
    if (sum (!is.na (s$chlfluor)) > 3) {
      ## first panel: data as measured/interpolated/imputed
      plot (chlfluorNNa ~ jday, s, lwd = 2, type = "l", col = "red"
        , main = levels (dat$year)[i]
        , ylab = "chlorophy/fluorescence")
      # lines (chlfluor~jday, subset (cliFL, station == levels (sL$station)[j]), lwd = 1, col = "green") # XXX
      lines (chlfluor ~ jday, s, lwd = 2, col = "black")
      # lines (climFluor~jday, s, lwd = 2, col = "black", lty = "dotted")
      legend ("topleft", col = c ("black", "red"), lwd = 2, bty = "n"
        , legend = c ("data", "missing data"))

      ## 2nd panel: cumulative fluorescence
      plot (cumsum(chlfluorNNa) ~ jday, s, lwd = 2, type = "l"
        , main = paste (dat$station [1], levels (dat$year)[i])
        , ylim = c(0, 1870)
        , xlim = c(1, 365)
        , ylab = "cumulative chlorophy/fluorescence"
      )
      legend ("topleft", col = c ("black", "blue", "red"), lty = c(1, 1, 2), lwd = 2
        , legend = c ("data", "fit", "long-term fit"), bty = "n")

      ## add line for climatology-fit
      nD <- 1:365
      #      fit.val <- data.frame (jday = s$jday, csum = predict (cliFit [[j]], list (x = s$jday)))
      fit.val <- data.frame (jday = nD, csum = predict (cliFit [[j]], list (jday = nD)))
      lines (csum ~ jday, fit.val, col = "red", lty = "dashed", lwd = 2)
      ## add line for current data fit
      fits <- yearFits [[j]][[i]]
      if (class (fits) == "try-error") {title (sub = "no year-fit")} else {
        fit.val <- try (data.frame (jday = s$jday, csum = predict (fits, list = (jday = s$jday))))
        try (lines (csum ~ jday, fit.val, col = "blue", lwd = 2))
      }
      #  print (summary(s$chlfluor))
    }
    #  print (summary (cumsum (s$chlfluor)))
    # cat ("j =", j, "  i =", i, "\n")
  }
}
dev.off()
rm (fit.val, fits, s, dat)




## parameters of temperature-fit
yearCoef$station <- factor (yearCoef$station)
pdf ("~/tmp/LCI_noaa/media/spring/timing_StationCor.pdf")
for (i in 1:length (levels (yearCoef$station))) {
  s <- subset (yearCoef, station == levels (yearCoef$station)[i])[, 2:ncol (yearCoef)]
  cat ("\n\n", levels (yearCoef$station)[i], "\n")
  print (try (cor (s, use = "p"), silent = TRUE))
  plot (sum ~ x0, s, pch = 19
    , main = levels (yearCoef$station)[i]
    , xlab = "central day of bloom"
    , ylab = "total annual chlorophyll")
  md <- try (lm (sum ~ x0, s), silent = TRUE)
  if (class (md) != "try-error") {
    nD <- list (x0 = seq (min (s$x0, na.rm = TRUE), max (s$x0, na.rm = TRUE), length.out = 100))
    mdP <- as.data.frame (predict (md, nD, interval = "confidence", level = 0.95))
    lines (nD$x0, mdP$fit, lwd = 2)
    lines (nD$x0, mdP$lwr, lty = "dashed", lwd = 2)
    lines (nD$x0, mdP$upr, lty = "dashed", lwd = 2)
  }
}
dev.off()
summary (lm (sum ~ x0 * station, yearCoef))
summary (lm (sum ~ station, yearCoef))

summary (lm (x0 ~ station, yearCoef))
aggregate (x0 ~ station, yearCoef, mean, na.rm = TRUE)


pdf ("~/tmp/LCI_noaa/media/spring/timing-stationCoef.pdf")
boxplot(sum ~ station, yearCoef, ylab = "total annual fluorescence/chlorophyll")
boxplot(x0 ~ station, yearCoef, ylab = "central day of bloom")
dev.off()


save.image ("~/tmp/LCI_noaa/cache/springtiming.RData")
# rm (list = ls()); load ("~/tmp/LCI_noaa/cache/springtiming.RData")
## add annual temperature to the coefficients

aTemp <- aggregate (temp ~ station + year, sLo, FUN = mean, na.rm = TRUE, subset = month < 6)
yearCoef$temp <- aTemp$temp [match (with (yearCoef, paste (station, year))
  , with (aTemp, paste (station, year)))]

tMdl <- lm (x0 ~ temp * station, data = yearCoef)



pdf ("~/tmp/LCI_noaa/media/spring/timingTemp.pdf", height = 4, width = 6)
par (mfrow = c(1, 3))
for (j in 2:length (levels (yearCoef$station))) {
  # j <- 3
  s <- subset (yearCoef, station == levels (yearCoef$station)[j])
  plot (temp ~ x0, s, pch = 19, xlab = "t0", main = levels (yearCoef$station)[j])
  md <- try (lm (temp ~ x0, s), silent = TRUE)
  if (class (md) != "try-error") {
    nD <- list (x0 = seq (min (s$x0, na.rm = TRUE), max (s$x0, na.rm = TRUE), length.out = 100))
    mdP <- as.data.frame (predict (md, nD, interval = "confidence", level = 0.95))
    lines (nD$x0, mdP$fit, lwd = 2)
    lines (nD$x0, mdP$lwr, lty = "dashed", lwd = 2)
    lines (nD$x0, mdP$upr, lty = "dashed", lwd = 2)
  }
}
dev.off()
summary (lm (x0 ~ station + temp, data = yearCoef))
summary (lm (x0 ~ temp, data = yearCoef))

require ("lme4")
yearCoef2 <- subset (yearCoef, station != "HomerDeep")
yearCoef2$station <- factor (yearCoef2$station)
glMdl <- lmer (x0 ~ temp + (temp | station), data = yearCoef2)




## -- just QAQC stuff -- ignore
if (0) {
  ## fluorescence and turbidity:
  plot (turb ~ as.num(jday), sLo)

  pdf ("~/tmp/LCI_noaa/media/spring/timeFluores-Turb.pdf", width = 7, height = 14)
  mC <- seq (1, 10, by = 2)
  par (mfcol = c(length (mC), 2), mar = c(3, 4, 0.5, 0.5))
  for (i in 1:length (mC)) {
    plot (chlfluor ~ turb, sLo, subset = (month == mC [i]) & (station == "HomerShallow")
      , xlab = "", xlim = c(0, 50), ylim = c(0, 150), ylab = "")
    legend ("topright", bty = "n", legend = month.abb [mC [i]], cex = 2)
    if (i == 1) {legend ("top", bty = "n", legend = "Homer", cex = 2.5)}
  }
  for (i in 1:length (mC)) {
    plot (chlfluor ~ turb, sLo, subset = (month == mC [i]) & (station == "SeldoviaShallow")
      , xlab = "", xlim = c(0, 50), ylim = c(0, 150), ylab = "")
    legend ("topright", bty = "n", legend = month.abb [mC [i]], cex = 2)
    if (i == 1) {legend ("top", bty = "n", legend = "Seldovia", cex = 2.5)}
  }
  mtext ("chlorophyll", side = 2, outer = TRUE, line = -2)
  mtext ("turbidity", side = 1, outer = TRUE, line = -2)
  dev.off()
}

# EOF
