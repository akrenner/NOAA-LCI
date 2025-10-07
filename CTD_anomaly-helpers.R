## CTD_anomaly-helpers.R
## append anomaly ranges, variable titles, color bars for use in sections and wall


## use CTDwall-setup.R as a template
base::load("~/tmp/LCI_noaa/cache/ctdwallSetup.RData")  # from CTDwall-setup.R
poAll <- readRDS("~/tmp/LCI_noaa/cache/ctd_castAnomalies.rds")
# rm(list=ls()); load("~/tmp/LCI_noaa/cache/ctdanomalies.RData")

oVars <- rep(ovars, 3)
oVarsF <- c(oVarsF, paste0("an_", VarsDFname), paste0("anS_", VarsDFname))
oVarsDFname <- names(poAll)[which(names(poAll) == "Temperature_ITS90_DegC"):ncol(poAll)]

oCol3 <- c(oCol3, lapply(seq_along(oCol3), function(x) {cmocean::cmocean("curl")}),
           lapply(seq_along(oCol3), function(x) {cmocean::cmocean("balance")}))

oRange <- t(sapply(oVarsDFname, FUN = function(vn) {
  ## ensure that anomalies are balanced
  x <- range(poAll[,which(names(poAll) == vn)], na.rm = TRUE)
  if (length(grep("^an", vn)) > 0) { # balance range on anomalies for plotting
    x <- c(-1,1)*max(abs(x))
  }
  x
}))


save(oVars, oVarsF, oCol3, oRange, file = "~/tmp/LCI_noaa/cache/ctd_anomalies.RData")
