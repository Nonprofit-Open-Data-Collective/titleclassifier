# 00-export-google-sheet.R
# One-time export of the title-taxonomy-map Google Sheet, run 2026-10-08 when
# the sheet was retired. Kept for provenance; there is no need to run it again.
#
# Writes every tab, as is, to archive/google-sheet-2026-10-08/, and starts the
# three tables the pipeline uses (status-codes.csv, title-standardization.csv,
# title-taxonomy.csv). From now on those CSVs are the source: edit them and run
# build-crosswalks.R.
#
# The pipeline columns of the sheet matched the snapshots then bundled in
# inst/extdata/crosswalks/, except for the title-taxonomy fixes not yet entered
# in the sheet (FINDINGS.md F-001, F-012 to F-015). The title-taxonomy table is
# therefore started from that snapshot, and the other two from the sheet, which
# also has their notes columns.
#
#   Rscript data-raw/crosswalks/00-export-google-sheet.R

suppressMessages({ library(googlesheets4); library(data.table) })
gs4_deauth()
ss   <- "1iYEY2HYDZTV0uvu35UuwdgAUQNKXSyab260pPPutP1M"
dir  <- "data-raw/crosswalks"
arch <- file.path(dir, "archive", "google-sheet-2026-10-08")
dir.create(arch, recursive = TRUE, showWarnings = FALSE)

# every tab as text, exactly as the sheet shows it (header row included)
tabs <- sheet_properties(ss)$name
for (s in tabs) {
  d <- with_gs4_quiet(read_sheet(ss, sheet = s, col_types = "c", col_names = FALSE, .name_repair = "minimal"))
  d <- as.data.table(d)
  for (j in names(d)) set(d, which(is.na(d[[j]])), j, "")
  fwrite(d, file.path(arch, paste0(s, ".csv")), col.names = FALSE, quote = TRUE)
}

# the three pipeline tables
# read.csv, not fread: a few variants contain literal quote characters
# (DIRECTOR """"), which fread does not unescape
read_tab <- function(s) {
  as.data.table(utils::read.csv(file.path(arch, paste0(s, ".csv")), colClasses = "character",
                                na.strings = character(0), check.names = FALSE))
}
sc <- read_tab("status-codes")
setnames(sc, c("status.variant", "status.qualifier", "notes"))
xw <- read_tab("title-standardization")[, .(title.variant, title.standard, strata, strata.label, notes)]
tx <- fread("inst/extdata/crosswalks/xwalk-title-taxonomy.csv", colClasses = "character", na.strings = NULL)

drop_blank <- function(d) d[rowSums(d != "") > 0]
fwrite(drop_blank(sc), file.path(dir, "status-codes.csv"), quote = TRUE)
fwrite(drop_blank(xw), file.path(dir, "title-standardization.csv"), quote = TRUE)
fwrite(drop_blank(tx), file.path(dir, "title-taxonomy.csv"), quote = TRUE)
