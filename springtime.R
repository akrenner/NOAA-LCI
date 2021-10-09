## timing of spring of temperature and fluorescence + phytoplanton
## use SWMP data

# fit logistic or similar curve, then take inflection points


## temperature: difference between shallow and deep spring transition



setwd ("~/myDocs/amyfiles/NOAA-LCI/")
rm (list = ls()); load ("~/tmp/LCI_noaa/cache/SeldTemp.RData")  ## from SeldoviaTemp.R
require (zoo)


## QAQC ---  report and fix!!
qaqcF <- function (x){
  x1 <- ifelse (x > 20, NA, x)
  x2 <- na.approx (x1, xout = 1:length(x), na.rm = FALSE)
  x2
  # na.spline (x1)
}

# sldvia$chlfluor <- ifelse (sldvia$chlfluor > 20, NA, sldvia$chlfluor)
# sldviaS$chlfluor <- ifelse (sldviaS$chlfluor > 20, NA, sldviaS$chlfluor)
# homer$chlfluor <- ifelse (homer$chlfluor > 20, NA, homer$chlfluor)
# homerS$chlfluor <- ifelse (homerS$chlfluor > 20, NA, homerS$chlfluor)
sldvia$chlfluor <- qaqcF (sldvia$chlfluor)
sldviaS$chlfluor <- qaqcF (sldviaS$chlfluor)
homer$chlfluor <- qaqcF (homer$chlfluor)
homerS$chlfluor <- qaqcF (homerS$chlfluor)



vN <- c(2,4:7,11:13)
names (homer)[vN]

## spectrum only
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
save.image("~/tmp/LCI_noaa/cache/swmpTScach.RData")
## rm (list = ()); load ("~/tmp/LCI_noaa/cache/swmpTScach.RData")
sldvia$year <- factor (sldvia$year)


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
set.seed(3)
sineF <- function (A, w, t0, omega)

sineFit <- nls (temp ~ A *sin (omega*jday+phi)+C,
                data = sldvia, start = list (A = 1, omega = 365, C = 1, phi = 1))
co <- coef (sineFit)
fit <- function (x,a,b,c,d){a*sin(a*x+c)+d}

plot (temp~jday, sldvia)
curve (fit (x = sldvia$temp, a = co ["A"], b = co ["omega"], c = co ["phi"], d = co ["C"])
       , add = TRUE, lwd = 2, col = "steel")


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
  s$chlfluor <- na.approx(s$chlfluor, x = s$jday, na.rm = FALSE)
  s$cumFlu <- cumsum(s$chlfluor)
  return (s)
}


# starting values
svt <- list (L = 12, k = -1, x0 = 90, b = 5)
svf <- list (L = c(10, 500), k = c(-10, -1, 1, 10), 1, x0 = 200, b = c (0, 500))

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


## parameters of temperature-fit
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

## parameters of cumFlu-fit  -- single gradient
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



## for each station: climatology of spring bloom

pdf ("~/tmp/LCI_noaa/media/sprintTimeFluorescClimatology-bySite.pdf", height = 6, width = 12)
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
  plot (cumsum(chlfluor)~jday, s, lwd = 2, type = "l"
        , main = c("Seldovia-deep", "Seldovia-shallow", "Homer-deep", "Homer-shallow")[i])
  plot (chlfluor~jday, s, lwd = 2, type = "l")
}
dev.off()


pdf ("~/tmp/LCI_noaa/media/sprintTimeFluoAnnual.pdf", height = 6, width = 12) ## fail - Inf/-Inf
par (mfrow = c(1,2))
# sL <- list (sldvia, sldviaS, homer, homerS)
for (i in 1:(length (levels (sldvia$year))-1)){
  cat ("\n\n", i, "\n")
  s <- subset (homerS, year == levels (sldvia$year)[i])  # use homerS for now
  if (!all (is.na (s$chlfluor))){
    plot (chlfluor~jday, s, lwd = 2, type = "l"
          , main = levels (sldvia$year)[i])
    x <- try (plot (cumsum(chlfluor)~jday, s, lwd = 2, type = "l"))
    if (class (x) == "try-error"){
      s$chlflour <- ifelse (is.na (s$chlfluor), 0, s$chlfluor)
      x <- try (plot (cumsum(chlfluor)~jday, s, lwd = 2, type = "l", col = "red"))
#      plot (1:2)
    }}
  title (main = levels (sldvia$year)[i])
#  print (summary (cumsum (s$chlfluor)))
  print (summary(s$chlfluor))
}
dev.off()


## summary of how many valid measurements per year
fluorAvail <- cbind (year = as.numeric (levels (sldvia$year))
, sldvia = sapply (1:length (levels (sldvia$year)), function (i){
sum (!is.na (subset (sldvia, year == levels (sldvia$year)[[i]])$chlfluor))
})
, sldviaS = sapply (1:length (levels (sldvia$year)), function (i){
  sum (!is.na (subset (sldvia, year == levels (sldvia$year)[[i]])$chlfluor))
})
, homer = sapply (1:length (levels (sldvia$year)), function (i){
  sum (!is.na (subset (homer, year == levels (sldvia$year)[[i]])$chlfluor))
})
, homerS = sapply (1:length (levels (sldvia$year)), function (i){
  sum (!is.na (subset (homerS, year == levels (sldvia$year)[[i]])$chlfluor))
})
)
print (fluorAvail)





pdf ("~/tmp/LCI_noaa/media/springtime.pdf") ## not working yet!
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