## lintr this project

## M. Renner, 2025-07-14

## Use the lintr and styler packages to audit all source code in this project
## to enforce uniform programming stile and formatting.
## also consider air: https://www.tidyverse.org/blog/2025/02/air/


require ("styler")
style_file("runAll.R", strict = FALSE, indent_by = 2L, scope = 'spaces')
style_file("runAll.R", strict = FALSE, indent_by = 2L, scope = 'indention')

skipL <- c("dataSetup.R", "findFront.R", "PDO.R", "phytopCommunity.R"
  , "zoopCommunity-note.R", "zoopCommunity.R"
  , "MonthlyUpdates/rsconnect/documents/202506_monthly.qmd/")

style_dir(".", scope = "spaces", strict = FALSE, indent_by = 2L)
style_dir(".", scope = "indention", strict = FALSE, indent_by = 2L)
# style_dir(".", strict = FALSE, indent_by = 2L)



## customize linters -- to do this step-by-step
## review the .lintr file!

require("lintr")
## e.g. lint an individual file:
lint(filename = "runAll.R", cache = TRUE)
lint(filename = "I-audit_and_lint.R", cache = TRUE)
lint(filename = "SeldoviaTemp.R", cache = FALSE
  , linters = list(seq_linter())
)

## or lint the entire project:

## or lint the entire project:
lint_dir(".", cache = TRUE)

lint_dir(".", cache = TRUE
  , linters = list(seq_linter())
)
