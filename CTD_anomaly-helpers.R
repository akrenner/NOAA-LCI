## CTD_anomaly-helpers.R
## append anomaly ranges, variable titles, color bars for use in sections and wall


## use CTDwall-setup.R as a template
# rm(list=ls()); load("~/tmp/LCI_noaa/cache/ctdanomalies.RData")
# rm(list = ls())
base::load("~/tmp/LCI_noaa/cache/ctdwallSetup.RData")  # from CTDwall-setup.R
poAll <- readRDS("~/tmp/LCI_noaa/cache/ctd_castAnomalies.rds")

oVars <- rep(oVars, 3)

## failing a better method to manipulate expressions, do this manually for now
oVars <- expression("Temperature [°C]",     #Temperature ~ "[" * ""^o ~ C * "]",
                    Salinity ~ "[" * PSU * "]",
                    Density ~ "[" * sigma[theta] * "]",
                    Oxygen ~ "[" * mu * mol ~ kg^-1 * "]",
                    "Oxygen saturation [%]",
                    "PAR",
                    Chlorophyll ~ "[" * mg ~ m^-3 * "]",
                    Turbidity ~ "[" * m^-1 * "]",
                    Buoyancy ~ frequency ~ N^2 ~ "[" * s^-2 * "]",
                    log ~ (PAR),
                    log ~ (turbidity),
                    "Temperature Anomaly [°C]", # Temperature ~ Anomaly ~ "[" * ""^o ~ C * "]",
                    Salinity ~ Anomaly ~ "[" * PSU * "]",
                    Density ~ Anomaly ~ "[" * sigma[theta] * "]",
                    "Oxygen saturation Anomaly [%]",
                    Oxygen ~ Anomaly ~ "[" * mu * mol ~ kg^-1 * "]",
                    "PAR Anomaly",
                    Chlorophyll ~ Anomaly ~ "[" * mg ~ m^-3 * "]",
                    Turbidity ~ Anomaly ~ "[" * m^-1 * "]",
                    Buoyancy ~ frequency ~ Anomaly ~ N^2 ~ "[" * s^-2 * "]",
                    log ~ (PAR) ~ Anomaly,
                    log ~ (turbidity) ~ Anomaly,
                    Temperature ~ Anomaly ~ "[" * SD * "]",
                    Salinity ~ Anomaly ~ "[" * SD * "]",
                    Density ~ Anomaly ~ "[" * SD * "]",
                    Oxygen ~ Anomaly ~ "[" * SD * "]",
                    "Oxygen Saturation Anomaly [SD",
                    PAR ~ Anomaly ~ "[" * SD * "]",
                    Chlorophyll ~ Anomaly ~ "[" * SD * "]",
                    Turbidity  ~ Anomaly ~ "[" * SD * "]",
                    Buoyancy ~ frequency ~ Anomaly ~ N^2 ~ "[" * SD * "]",
                    log ~ (PAR) ~ Anomaly ~ "[" * SD * "]",
                    log ~ (turbidity) ~ Anomal ~ "[" * SD * "]"
)

oVarsF <- c(oVarsF, paste0("an_", oVarsDFname), paste0("anS_", oVarsDFname))
oVarsDFname <- names(poAll)[which(names(poAll) == "Temperature_ITS90_DegC"):
  ncol(poAll)] # update of version from CTDwall-setup.R

oCol3 <- c(oCol3,
  lapply(seq_along(oCol3), function(x) {cmocean::cmocean("curl")}),
  lapply(seq_along(oCol3), function(x) {cmocean::cmocean("balance")}))

oRange <- t(sapply(oVarsDFname, FUN = function(vn) {
  ## ensure that anomalies are balanced
  x <- range(poAll[,which(names(poAll) == vn)], na.rm = TRUE)
  if(length(grep("^an", vn)) > 0) { # balance range on anomalies for plotting
    x <- c(-1,1)*max(abs(range(x)))
  }
  if(length(grep("anS_", vn)) > 0) {x <- c(-1,1)*3} # force scaled anomalies
  x
}))


save(oVars, oVarsDFname, oVarsF, oCol3, oRange,
  file = "~/tmp/LCI_noaa/cache/ctd_anomalies.RData")

if(!all.equal(rep(nrow(oRange),4),
  c(length(oVarsF), length(oVarsDFname), length(oVars), length(oCol3)))) {
  stop("Discrepancies in the length of objects that should be equal")}

if(0) {
cbind(rep(nrow(oRange),4),
  c(length(oVarsF), length(oVarsDFname), length(oVars), length(oCol3)))
}
