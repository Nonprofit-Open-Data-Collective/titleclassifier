# Shared settings for the Part VII validation scripts. Run every script from
# the package root. Data and review files live outside the repository, in
# PARTVII_DIR (default ~/Documents/PARTVII); findings and crosswalk fixes are
# recorded in the repository (data-raw/partvii-validation/).

pv_root   <- normalizePath(Sys.getenv("PARTVII_DIR", file.path(Sys.getenv("USERPROFILE", "~"), "Documents", "PARTVII")),
                           winslash = "/", mustWork = FALSE)
pv_data   <- file.path(pv_root, "data")
pv_review <- file.path(pv_root, "review")
efile_dir <- Sys.getenv("EFILE_V2_3_DIR", "C:/Users/jlecy/Documents/EFILE_BUILD_SEPT_2026/EFILE_V2_3")
pv_repo   <- "data-raw/partvii-validation"

dir.create(pv_data, recursive = TRUE, showWarnings = FALSE)
dir.create(pv_review, recursive = TRUE, showWarnings = FALSE)
