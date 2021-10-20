## timing of spring of temperature and fluorescence + phytoplanton
## use SWMP data

# fit logistic or similar curve, then take inflection points


## temperature: difference between shallow and deep spring transition

## impute NAs from long-term mean
## combine Seldovia&Homer?? -- later, keep it simple for now





setwd ("~/myDocs/amyfiles/NOAA-LCI/")
rm (list = ls()); load ("~/tmp/LCI_noaa/cache/SeldTemp.RData")  ## from SeldoviaTemp.R
require (zoo)


## QAQC ---  report and fix!!
qaqcF <- function (x){
  # x1 <- ifelse (x > 20, NA, x)
  x1 <- x
  x2 <- na.approx (x1, xout = 1:length(x), na.rm = FALSE)
  # x2 <- ifelse (is.na (x2), 0, x2)  ## don't replace zeros -- impute them from climatology
  x2
  # na.spline (x1)
}

# sldvia$chlfluor <- ifelse (sldvia$chlfluor > 20, NA, sldvia$chlfluor)
# sldviaS$chlfluor <- ifelse (sldviaS$chlfluor > 20, NA, sldviaS$chlfluor)
# homer$chlfluor <- ifelse (homer$chlfluor > 20, NA, homer$chlfluor)
# homerS$chlfluor <- ifelse (homerS$chlfluor > 20, NA, homerS$chlfluor)
if (0){
  sldvia$chlfluor <- qaqcF (sldvia$chlfluor)
  sldviaS$chlfluor <- qaqcF (sldviaS$chlfluor)
  homer$chlfluor <- qaqcF (homer$chlfluor)
  homerS$chlfluor <- qaqcF (homerS$chlfluor)
}


vN <- c(2,4:7,11:13)
names (homer)[vN]

## spectrum only
if (0){
  # pdf (paste0 ("~/tmp/LCI_noaa/media/SWMP-spectra", ".pdf"))
  ## what are the main frequencies in TS autocorrelations? -- just exploratory
  ## what can be learned from fluourescence, any use of CTD-fluorescence? Length of bloom?
  png ("~/tmp/LCI_noaa/media/SWMP-spectra%02d.png", width = 800, height = 800, res = 120)
  par (mar = c(3,4,2,1))
  for (i in vN){
    oT <- homer [,c(1,i)]
    cat (names (homer)[i], "\n")
    ## spectrum --
    if (1){
      require ("psd")
      x <- ifelse (is.na (oT [,2]), 0, oT [,2])
      x <- scale (x)
      xSp <- spectrum (x, main = names (homer)[i], plot = TRUE
                       , ylim = c(1e-8, 1e5)) # slow! run this all in parallel?
    }else{
      require ("spectral") # can handle NAs
      xSp <- spec.fft (y = as.numeric (scale (oT [,2])), x = as.numeric (oT$datetimestamp))
    }
    plot (xSp)
  }
  dev.off()
  require ("grid")
  merge.png.pdf ("~/tmp/LCI_noaa/media/SWMP-spectra.pdf"
                 , pngFiles = list.files ("~/tmp/LCI_noaa/media", "SWMP-spectra\\d+.png", full.names = TRUE)
                 , deletePngFiles=TRUE)


for (i in vN){
  ## scale of fluorescence and other measures? length of blooms?
  ## relationship between homer-seldovia, shallow-deep
  pdf (paste0 ("~/tmp/LCI_noaa/media/SWMP-TSanalysis", names (homer)[i], ".pdf"))
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

}




## start with sldvia (not sldviaS)
save.image("~/tmp/LCI_noaa/cache/swmpTScach.RData")
## rm (list = ls ()); load ("~/tmp/LCI_noaa/cache/swmpTScach.RData")


if (0){
  sldvia$t <- as.numeric (sldvia$datetimestamp)
  per <- 365.25*24*3600
  reslm <- lm (temp~sin (2*pi /per*t)+cos (2*pi/per*t), data = sldvia)
  lines (fitted (reslm)~sldvia$t, col = 4, lty = 2)




  s <- subset (sldvia, year == levels (sldvia$year)[15] & (month <= 7))

  s2 <- aggregate (temp~jday, FUN = mean, na.rm = TRUE)
  s2$chlfluor <- aggregate (chlfluor~jday, FUN = mean, na.rm = TRUE)$chlfluor
  s<- s2

  s$chlfluor <- na.approx(s$chlfluor, x = s$datetimestamp) ## no NA for cumulative functions!

  s$cumF <- cumsum (s$chlfluor)
  plot (temp~jday, data = s, type = "l")

  plot (cumF~jday, data = s, type = "l")
  plot (cumsum(chlfluor)~jday, data = s, type = "l")
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


if (0){
plot (temp~jday, sldvia)
curve (fit (x = sldvia$temp, a = co ["A"], b = co ["omega"], c = co ["phi"], d = co ["C"])
       , add = TRUE, lwd = 2, col = "steel")
}

###############################
## logistic/sigmoidal curve ##
###############################

logF <- function (L, k, x0, b, x){
  L/(1+exp (-k*(x-x0)))+b
}

sineF <- function (A, omega, phi, x, C){
  A * sin (omega * x + phi) + C
}


prepDF <- function (df, i, mR = 2:8){
  require("zoo")
  s <- subset (df, year == levels (df$year)[i])
  s <- subset (s, month %in% mR)
  #  s <- subset (s, month <= 9)
  s$jday <- s$jday + as.numeric (format (s$datetimestamp, "%H"))/24 +
    as.numeric (format (s$datetimestamp, "%M"))/24 / 60
  # s$chlfluor <- na.approx(s$chlfluor, x = s$jday, na.rm = FALSE) ## do this earlier
  s$cumFlu <- cumsum(s$chlfluor)
  return (s)
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


# for (i in 1:length (levels (sldvia$year))){
#   s <- prepDF (sldvia, i)
#   cat (i, " ")
#   if (all (is.na (s$temp))){rep (NA, 2)}else{
#     fit <- try (nls (temp~logF (L, k, x0, b, x = jday), data = s, start = svt))
#     plot (temp~jday, s)
#     plot (cumFlu~jday, s)
#
#         # if (class (fit) != "try-error"){
#     #   summary (fit)$parameters [3,1:2]
#     # }else{
#     #   rep (NA, 2)
#     # }
#     }
# }


sL <- rbind (sldvia, sldviaS, homer, homerS)
rm (sldvia, sldviaS, homer, homerS)
# sL <- subset (sL, year < format (Sys.time(), "%Y"))
sL <- subset (sL, year < 2021)  # can't fit data to incomplete year

sL$station <- factor (sL$station)
sL$year <- factor (sL$year)
sL$jday <- factor (sL$jday)


## climatology per station
# cliFL <- lapply (1:length (levels (sL$station)), FUN = function (i){
#   x <- subset (sL, station == levels (sL$station)[i])
#   s <- aggregate (chlfluor~jday+year, x, FUN = mean, na.rm = TRUE)
#   s <- aggregate (chlfluor~jday, s, FUN = mean, na.rm = TRUE)
#   s
# })
# summary (cliFL [[1]])
cliFL <- aggregate (chlfluor~station+jday+year, data = sL, FUN = mean, na.rm = TRUE)
cliFL <- aggregate (chlfluor~station+jday, data = cliFL, FUN = mean, na.rm = TRUE)
cliFL$jday <- as.numeric (levels (cliFL$jday))[cliFL$jday]
summary (subset (cliFL, cliFL$station == levels (cliFL$station)[[1]]))


## fit logistic curve to each climatology
cliFit <- lapply (1:length (levels (cliFL$station)), FUN = function (j){
#  s <- cliFL [[j]]
  s <- subset (cliFL, station == levels (cliFL$station)[j])
  s$csum <- cumsum (s$chlfluor)
  fit <- nls (csum~logF (L,k,x0,b,x=jday), data = s, start = svf)
  fit
})


## for each station: climatology of spring bloom
pdf ("~/tmp/LCI_noaa/media/sprintTimeFluorescClimatology-bySite.pdf", height = 6, width = 12)
par (mfrow = c(1,2))
for (j in 1:length (levels (cliFL$station))){
# for (j in 1:length (cliFL)){
#  s <- cliFL [[j]]  #subset (sL, station == levels (sL$station)[j])
  s <- subset (cliFL, station == levels (cliFL$station)[j])
  s$csum <- cumsum (s$chlfluor)  ## no NAs in climatology
  plot (chlfluor~jday, s, lwd = 2, type = "l"
        , main = c("Seldovia-deep", "Seldovia-shallow", "Homer-deep", "Homer-shallow")[j])
  plot (csum~jday, s, lwd = 2, type = "l", ylab = "cumulative chlorophyl")
  # fit <- nls (csum~logF (L,k,x0,b,x=jday), data = s, start = svf)
  # fit.val <- data.frame (jday = s$jday, csum = predict (fit, list (jday = s$jday)))
  # lines (csum~jday, fit.val, col = "blue")
  fit.val <- data.frame (jday = s$jday, csum = predict (cliFit [[j]], list (jday = s$jday)))
    lines (csum~jday, fit.val, col = "red")
}
dev.off()




## summary of how many valid measurements per year
fluorAvail <- sapply (1:length (levels (sL$station)), FUN = function (j){
  s <- subset (sL, station == levels (station)[j])  # use homerS for now
  sapply (1:length (levels (s$year)), function (i){
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
sL <- aggregate (chlfluor~jday+year+station, sL, FUN = mean, na.rm = TRUE)
sL$jday <- as.numeric (levels (sL$jday))[sL$jday]

sL$chlfluorNNa <- sL$chlfluor
sL$chlfluorNNa [is.na(sL$chfluor)] <- cliFL$chlfluor [match (paste0 (as.character (sL$station), sL$jday)
                                                          , paste0 (as.character (cliFL$station)
                                                                    , cliFL$jday))][is.na (sL$chlfluor)]

yearFits <- lapply (1:length (levels (sL$station)), function (j){
  dat <- subset (sL, station == levels (sL$station)[j])
  dat$year <- factor (dat$year)  ## reset year levels

  ## aggregate to jday, same as cliFL
  #dat <- aggregate (chlfluorNNa~jday+year, data = dat, FUN = mean, na.rm = TRUE)
  # dat$jday <- as.numeric (levels (dat$jday))[dat$jday]  # already numeric!

  svf <- as.list (coefficients(cliFit [[j]]))
  # svf <- list (L = c(500, 1000, 10000), k = c (0.01, 0.02, 0.05)
  #              , x0 = c(160, 180, 200, 250), b = c (-50, -20, -10, 0))
  logF2 <- function (L,k,x0,b,x){
    L <- svf$L
    b <- svf$b
    L/(1+exp (-k*(x-x0)))+b
  }

  yFit <- lapply (1:length (levels (dat$year)), FUN = function (i){
    s <- subset (dat, year == levels (dat$year)[i])
    ## replace NA from climatology
    #    s$chlfluor <- ifelse (is.na (s$chlfluor), cliFL [[j]]$chlfluor, s$chlfluor)
    #    s$chlFluor [is.na (s$chlfluor)] <- cliFL [[j]]$chlfluor [match (s$jday, cliFL[[j]]$jday)]  # not all jdays are covered
    # s$chlFluor <- ifelse (is.na (s$chlfluor)
    #                       , cliFL [[j]]$chlfluor [match (s$jday, cliFL[[j]]$jday)]
    #                       , s$chlfluor) # not all jdays are covered

    s$csum <- cumsum (s$chlfluorNNa)  ## use imputed version
    fit <- try (nls (csum~logF (L,k,x0,b,x=jday), data = s, start = svf))
    #     fit <- try (nls (s$csum~SSlogis (s$jday, Asym, xmid, scal)))
    # if (class (fit) == "try-error"){fit <- }
    #fit <- list (fit, levels (dat$year)[i])
    fit
  })
  names (yFit) <- paste0 ("Y", levels (dat$year))
  yFit
})

yearCoef <- lapply (1:length (yearFits), function (j){
  fits <- yearFits [[j]]
  coefs <- t (sapply (1:length (fits), function (i){
    fit <- fits [[i]]
    if (class (fit) == "try-error"){
       cf <- as.numeric (rep (NA, 4))
      names (cf) <- c("L","k","x0","b") # as.numeric (c (L=NA, k=NA,x0=NA,b=NA))
    }else{
      cf <- coefficients(fit)
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
yearCoef


pdf ("~/tmp/LCI_noaa/media/sprintTimeFluoAnnual.pdf", height = 6, width = 12)
par (mfrow = c(1,2))
for (j in 1:length (levels (sL$station))){
  dat <- subset (sL, station == levels (sL$station)[j])
  for (i in 1:length (levels (dat$year))){
    #  cat ("\n\n", i, "\n")
    s <- subset (dat, year == levels (dat$year)[i])  # use homerS for now
    if (!all (is.na (s$chlfluor))){
      ## fix NAs -- impute from climatology
      sO <- s # save original values
      # s$chlfluor <- ifelse (is.na (s$chlfluor), cliFL [[j]]$chlfluor, s$chlfluor)
      s$chlFluor [is.na (s$chlfluor)] <- cliFL [[j]]$chlfluor [match (s$jday, cliFL[[j]]$jday)]  # not all jdays are covered

      plot (chlfluor~jday, s, lwd = 2, type = "l" #, col = "blue"
            , main = levels (dat$year)[i])
      plot (cumsum(chlfluorNNa)~jday, s, lwd = 2, type = "l"
            , main = paste (dat$station [1], levels (dat$year)[i]))
    #  fit.val <- data.frame (jday = s$jday, csum = predict (cliFit [[j]], list (x = s$jday)))
      fit.val <- data.frame (jday = s$jday, csum = predict (cliFit [[j]], list (x = s$jday)))
      lines (csum~jday, fit.val, col = "red")
      #  print (summary(s$chlfluor))
    }
    #  print (summary (cumsum (s$chlfluor)))
  }
}
dev.off()








## parameters of temperature-fit
sldvia <- subset (sL, station = levels (sL$station)[[j]])
sldvia$year <- factor (sldvia$year)
cT <- t (sapply (1:length (levels (sldvia$year)), function (i){
  s <- prepDF (sldvia, i)
  if (all (is.na (s$temp))){rep (NA, 4)}else{
    fit <- try (nls (temp~logF (L, k, x0, b, x = jday), data = s, start = svt))
    if (class (fit) != "try-error"){
coefficients(fit)
      # summary (fit)$parameters [3,1:2]
    }else{
      rep (NA, 4)
    }}
}))
row.names (cT) <- levels (sldvia$year)
colnames (cT) <- names (svt)
cor (cT, use = "p")


## parameters of cumFlu-fit  -- single gradient  --  fail -- attempt to use zero-length variable name
cF <- t (sapply (1:length (levels (sldvia$year)), function (i){
  s <- prepDF (sldvia, i, mR = 1:12)
  if (all (is.na (s$cumFlu))){rep (NA, 2)}else{
    fit <- try (nls (cumFlu~logF (L, k, x0, b, x = jday), data = s, start = svf))
    if (class (fit) != "try-error"){
      summary (fit)$parameters [3,1:2]
    }else{
      rep (NA, 2)
    }}
}))







if (0){  ### update or replace (already replaced??)
pdf ("~/tmp/LCI_noaa/media/springtime-temp+fluor.pdf") ## not working yet!
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
}

rm (i, s, fit, fit.val)