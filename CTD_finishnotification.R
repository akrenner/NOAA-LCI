## send an email when called, to notify user that run has finished.

## use sendmailR or gmailR (or 'mail'?)


if (Sys.getenv ("USERNAME") %in% c("Martin.Renner", "martin")){
  require ("gmailR")
  ## send mail that CTD processing has finished.
}
