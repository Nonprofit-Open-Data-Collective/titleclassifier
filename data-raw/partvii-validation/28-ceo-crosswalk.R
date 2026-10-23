# 28-ceo-crosswalk.R
# Apply crosswalk decisions that come from the CEO / board label set
# (ceo-sample/crosswalk-decisions.csv; FINDINGS.md F-032) to
# data-raw/crosswalks/title-standardization.csv, rebuild the package data, and
# log each change in ceo-sample/crosswalk-applied.csv. Rows of
# crosswalk-changes.csv for the same variants are updated too, so a re-run of
# 06-apply-decisions.R does not undo them.
#
#   Rscript data-raw/partvii-validation/28-ceo-crosswalk.R          (dry run)
#   Rscript data-raw/partvii-validation/28-ceo-crosswalk.R --apply

source("data-raw/crosswalks/_crosswalk-io.R")
apply <- "--apply" %in% commandArgs(trailingOnly = TRUE)
REPO <- "data-raw/partvii-validation/ceo-sample"
today <- as.character(Sys.Date())
rd <- function(f) as.data.table(utils::read.csv(f, colClasses = "character", na.strings = character(0), check.names = FALSE))

xw <- read_xwalk("title-standardization"); tx <- read_xwalk("title-taxonomy")
dc <- rd(file.path(REPO, "crosswalk-decisions.csv"))
bad <- setdiff(dc$title.standard, tx$title.standard)
if (length(bad)) stop("decisions target standards not in the taxonomy: ", paste(bad, collapse = ", "))
i <- match(dc$title.variant, xw$title.variant)
log <- data.table(title.variant = dc$title.variant, action = ifelse(is.na(i), "add", "change"),
                  previous_standard = xw$title.standard[i], previous_strata = paste(xw$strata[i], xw$strata.label[i], sep = " / "),
                  title.standard = dc$title.standard, strata = paste(dc$strata, dc$strata.label, sep = " / "),
                  reason = dc$reason, date = today)
log <- log[!(action == "change" & previous_standard == title.standard & previous_strata == strata)]
print(log[, .(title.variant, action, previous_standard, title.standard)])
if (!apply) { cat("dry run: add --apply to write\n"); quit(save = "no") }

xw2 <- copy(xw); upd <- !is.na(i)
xw2[i[upd], `:=`(title.standard = dc$title.standard[upd], strata = dc$strata[upd], strata.label = dc$strata.label[upd])]
xw2 <- rbind(xw2, dc[!upd, .(title.variant, title.standard, strata, strata.label, notes = paste("CEO / board labels", today))])
setorder(xw2, title.variant)
write_xwalk(xw2, "title-standardization")
cp <- "data-raw/partvii-validation/crosswalk-changes.csv"
cc <- rd(cp)
k <- which(cc$title.variant %in% dc$title.variant)
k <- k[cc$title.standard[k] != dc$title.standard[match(cc$title.variant[k], dc$title.variant)]]
if (length(k)) {
  cc[k, `:=`(title.standard = dc$title.standard[match(title.variant, dc$title.variant)],
             notes = paste0(notes, " [superseded by the CEO / board labels, ", today, ": see ceo-sample/crosswalk-applied.csv]"))]
  fwrite(cc, cp); message(sprintf("crosswalk-changes.csv: %d rows superseded", length(k)))
}
lf <- file.path(REPO, "crosswalk-applied.csv")
if (file.exists(lf)) log <- rbind(rd(lf), log, fill = TRUE)
fwrite(log, lf)
rebuild_xwalks()
cat("applied; package data rebuilt\n")
