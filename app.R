# ================================================================
# TAAORC GVC Model - Shiny launcher
# Te Ara Ahunga Ora Retirement Commission
#
# WHY THIS FILE EXISTS
#   Shiny hosting platforms (shinyapps.io, Posit Connect Cloud) and
#   shiny::runGitHub() look for a file named exactly "app.R" in the
#   directory root. The model itself lives in TAAORC_GVC_Model.R so
#   that it keeps a meaningful name; this file only launches it.
#
#   source() returns a list whose $value is the result of the last
#   expression in the sourced file. The last line of TAAORC_GVC_Model.R
#   is shinyApp(ui, server), so $value is the app object, which is what
#   a hosting platform needs app.R to evaluate to.
#
# REQUIRED FILES IN THIS SAME DIRECTORY
#   TAAORC_GVC_Model.R
#   dataset_2026.csv
#   dataset_by_income.csv
#   dataset_by_age.csv
#
#   The model reads those three CSVs by bare relative path, so they must
#   sit alongside this file. Subfolders holding the build script, source
#   spreadsheets, or documentation are fine and are simply not read.
# ================================================================

# Declared here as well as in the model so that deployment dependency
# scanners and human readers both see them without opening another file.
library(shiny)
library(dplyr)

source("TAAORC_GVC_Model.R")$value
