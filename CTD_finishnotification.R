## send an email when called, to notify user that run has finished.

## use sendmailR or gmailR (or 'mail'?)
## see https://www.infoworld.com/article/3398701/how-to-send-email-from-r-and-gmail.html
## and https://github.com/r-lib/gmailr


if (Sys.getenv ("USERNAME") %in% c("Martin.Renner", "martin")){
  if (!require("pacman")) install.packages("pacman"
                                           , repos = "http://cran.fhcrc.org/", dependencies = TRUE)
  Require <- pacman::p_load

  Require ("gmailr")
  ## send mail that CTD processing has finished.
}
