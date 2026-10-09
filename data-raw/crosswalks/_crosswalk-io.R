# Read and write the crosswalk tables in data-raw/crosswalks/. Sourced by the
# scripts that edit them (data-raw/partvii-validation/06-apply-decisions.R and
# 07-taxonomy-fixes.R). Run from the package root.

suppressMessages(library(data.table))

xwalk_dir <- "data-raw/crosswalks"
xwalk_path <- function(name) file.path(xwalk_dir, paste0(name, ".csv"))

# read.csv, not fread: a few title variants contain literal quote characters
# (DIRECTOR """"), which fread does not unescape
read_xwalk <- function(name) {
  as.data.table(utils::read.csv(xwalk_path(name), colClasses = "character",
                                na.strings = character(0), check.names = FALSE))
}

write_xwalk <- function(d, name) fwrite(d, xwalk_path(name), quote = TRUE)

# rebuild data/*.rda from the tables
rebuild_xwalks <- function() source(file.path(xwalk_dir, "build-crosswalks.R"), local = new.env())
