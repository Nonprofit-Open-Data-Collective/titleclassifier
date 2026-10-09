# 02-soc-codes.R
# SOC coverage pass on title-taxonomy.csv (Phase 5 of data-dev/CROSSWALK-REVIEW.md,
# FINDINGS.md F-021). Run 2026-10-08; kept for provenance. Re-running changes
# nothing once the codes are in.
#
# Policy: a SOC code describes a title's function, not its rank (emp.level
# carries the rank).
# - Functional chief officers take their function's code: HR 11-3121,
#   technology 11-3021, development 11-2033, marketing 11-2021, legal 23-1011.
# - Generic leadership titles keep 11-1011 (Chief Executives) or 11-1021
#   (General and Operations Managers).
# - Titles whose function the words don't settle stay uncoded:
#   DIRECTOR, STAFF, CAPTAIN, CMO, CREATIVE DIRECTOR...
#
# Each code is given at the most specific level that fits: a detailed
# occupation (29-1141), a broad group (29-1210), or a minor group (25-1000).
# The parent groups and soc.label (the official 2018 title) are filled from
# tidy-soc-codes.csv, and the script stops on a code that isn't there. Every
# change is logged in soc-codes-log.csv.
#
#   Rscript data-raw/crosswalks/02-soc-codes.R

source("data-raw/crosswalks/_crosswalk-io.R")
tx  <- read_xwalk("title-taxonomy")
soc <- fread("data-raw/standard-occupational-classifications/tidy-soc-codes.csv", encoding = "Latin-1")
strip <- function(x) sub("^SOC-", "", x)
soc[, `:=`(maj = substr(strip(MajorGroup), 1, 2), min = strip(MinorGroup),
           brd = strip(BroadGroup), det = strip(DetailedOccupation))]

codes <- c(
  # ---- recode: functional chief officers and executives ----
  "COO" = "11-1011", "PRESIDENT" = "11-1011", "SCOUT EXECUTIVE" = "11-1011",
  "DEPUTY CEO" = "11-1011", "VICE CEO" = "11-1011",
  "CHIEF OFFICER" = "11-1021",
  "CHIEF ADMINISTRATIVE OFFICER" = "11-3012", "CHIEF ADMINISTRATION OFFICER" = "11-3012", "CAO" = "11-3012",
  "CHIEF HUMAN RESOURCES OFFICER" = "11-3121",
  "CHIEF INFORMATION OFFICER" = "11-3021", "CHIEF TECHNOLOGY OFFICER" = "11-3021",
  "CHIEF DIGITAL OFFICER" = "11-3021", "VICE PRESIDENT AND CIO" = "11-3021",
  "CHIEF DEVELOPMENT OFFICER" = "11-2033", "CHIEF ADVANCEMENT OFFICER" = "11-2033",
  "CHIEF INSTITUTIONAL ADVANCEMENT OFFICER" = "11-2033", "CHIEF PHILANTHROPY OFFICER" = "11-2033",
  "CHIEF MARKETING OFFICER" = "11-2021", "CHIEF RELATIONS OFFICER" = "11-2032",
  "CHIEF LEGAL OFFICER" = "23-1011", "CHIEF SCIENTIFIC OFFICER" = "11-9121",
  "CHIEF MECHANICAL OFFICER" = "11-9041", "CHIEF PROGRAMS OFFICER" = "11-9151",
  "CHIEF ARTISTIC OFFICER" = "27-1011", "CHIEF CURATOR" = "25-4012",
  "CHIEF COLLECTIONS OFFICER" = "25-4012", "CHIEF HISTORIAN" = "19-3093",
  "CHIEF LEARNING OFFICER" = "11-3131", "CHIEF INVESTMENT OFFICER" = "11-3031",
  "CHIEF NURSING OFFICER" = "11-9111", "CHIEF CLINICAL OFFICER" = "11-9111", "CHIEF MEDICAL OFFICER" = "11-9111",
  "CHIEF ACADEMICS OFFICER" = "11-9033",
  # ---- health ----
  "DOCTOR" = "29-1210", "MEDICAL OFFICER" = "29-1210", "PRESIDENT OF MEDICAL STAFF" = "29-1210",
  "NURSE" = "29-1141", "THERAPIST" = "29-1120", "PHARMACIST" = "29-1051", "VETERINARIAN" = "29-1131",
  "MEDICAL DIRECTOR" = "11-9111", "CLINICAL DIRECTOR" = "11-9111", "DIRECTOR OF NURSING" = "11-9111",
  "VICE PRESIDENT OF HEALTH CARE OPERATIONS" = "11-9111",
  # ---- education ----
  "PROFESSOR" = "25-1000", "FACULTY" = "25-1000", "LECTURER" = "25-1000",
  "TEACHER" = "25-2000", "INSTRUCTOR" = "25-3099", "SENIOR INSTRUCTOR" = "25-3099",
  "EDUCATION INSTRUCTOR" = "25-3099", "EDUCATOR" = "25-3099", "EDUCATION COORDINATOR" = "25-9031",
  "PRINCIPAL" = "11-9032", "ASSISTANT PRINCIPAL" = "11-9032", "SUPERINTENDENT" = "11-9032",
  "VICE PRESIDENT OF ACADEMICS" = "11-9033", "VICE PRESIDENT OF ACADEMICS AFFAIRS" = "11-9033",
  "DIRECTOR OF ACADEMICS" = "11-9033", "ASSOCIATE DEAN" = "11-9033", "UNDERGRAD DIRECTOR" = "11-9033",
  "VICE PRESIDENT OF UNIVERSITY ADVANCEMENT" = "11-2033",
  "DIRECTOR OF EDUCATION" = "11-9039", "VICE PRESIDENT OF EDUCATION" = "11-9039",
  "EDUCATION MN" = "11-9039", "VICE PRESIDENT OF EDUCATION AND GUEST EXPERIENCE" = "11-9039",
  "TRAINING DIRECTOR" = "11-3131", "TRAINING" = "13-1151", "TRAINING COORDINATOR" = "13-1151", "TRAINING OFFICER" = "13-1151",
  # ---- programs and community ----
  "DIRECTOR OF PROGRAMS" = "11-9151", "PROGRAMS DIRECTOR" = "11-9151", "SENIOR DIRECTOR OF PROGRAMS" = "11-9151",
  "VICE PRESIDENT OF PROGRAMS" = "11-9151", "PROGRAMS VICE PRESIDENT" = "11-9151",
  "PROGRAMS MANAGER" = "11-9151", "SENIOR PROGRAMS OFFICER" = "11-9151",
  "OUTREACH DIRECTOR" = "11-9151", "OUTREACH MANAGER" = "11-9151", "YOUTH DIRECTOR" = "11-9151",
  "PROGRAMS COORDINATOR" = "21-1099", "PROGRAMS OFFICER" = "21-1099", "ORGANIZER" = "21-1099",
  "VICE PRESIDENT OF EVENTS" = "13-1121", "DIRECTOR OF EVENTS" = "13-1121",
  # ---- religion ----
  "PASTOR" = "21-2011", "DEACON" = "21-2099",
  # ---- finance ----
  "ACCOUNTING DIRECTOR" = "11-3031", "DIRECTOR OF BUDGET AND ANALYTICS" = "11-3031",
  "SENIOR VICE PRESIDENT OF FINANCE" = "11-3031", "VICE PRESIDENT OF FINANCE AND ADMINISTRATION" = "11-3031",
  "VICE PRESIDENT OF LENDING" = "11-3031",
  "INVESTMENT OFFICER" = "13-2051", "FINANCE ADVISOR" = "13-2052",
  # ---- fundraising, marketing, communications ----
  "DEVELOPMENT MANAGER" = "11-2033", "SENIOR DIRECTOR OF DEVELOPMENT" = "11-2033",
  "DIRECTOR OF MAJOR GIFTS" = "11-2033", "VICE PRESIDENT OF PHILANTHROPY" = "11-2033",
  "VICE PRESIDENT OF MEMBERSHIPS" = "11-2033",
  "DEVELOPMENT OFFICER" = "13-1131", "ENDOWMENTS" = "13-1131", "MEMBERSHIP COORDINATOR" = "13-1131",
  "DIRECTOR OF MARKETING AND COMMUNICATIONS" = "11-2021", "VICE PRESIDENT OF MARKETING AND COMMUNICATIONS" = "11-2021",
  "VICE PRESIDENT OF MARKETING AND PUBLICITY" = "11-2021", "ASSISTANT DIRECTOR OF MARKETING" = "11-2021",
  "DIRECTOR OF SALES" = "11-2022",
  "DIRECTOR OF PUBLIC AFFAIRS" = "11-2032", "DIRECTOR OF PUBLICITY" = "11-2032", "VICE PRESIDENT OF PUBLICITY" = "11-2032",
  "VICE PRESIDENT OF EXTERNAL AFFAIRS" = "11-2032", "VICE PRESIDENT OF GOVERNMENT AFFAIRS" = "11-2032",
  "COMMUNITY RELATIONS" = "27-3031", "INFORMATION OFFICER" = "27-3031",
  # ---- HR, legal, compliance ----
  "HUMAN RESOURCES" = "13-1071", "RECRUITMENT" = "13-1071",
  "LAWYER" = "23-1011", "JUDGE ADVOCATE" = "23-1011", "VICE PRESIDENT OF LEGAL" = "23-1011",
  "COMPLIANCE OFFICER" = "13-1041", "CONTRACT COMPLIANCE" = "13-1041",
  # ---- general and administrative management ----
  "MANAGER" = "11-9199", "MANAGER OF OPERATIONS" = "11-1021",
  "REGIONAL DIRECTOR" = "11-1021", "REGIONAL MANAGER" = "11-1021", "DIVISION DIRECTOR" = "11-1021",
  "SENIOR VICE PRESIDENT OF OPERATIONS" = "11-1021", "VICE PRESIDENT OF COMMUNICATIONS AND OPERATIONS" = "11-1021",
  "VICE PRESIDENT OF ECONOMIC DEVELOPMENT" = "11-1021",
  "ADMINISTRATION DIRECTOR" = "11-3012",
  "VICE PRESIDENT OF HOUSING" = "11-9141", "VICE PRESIDENT OF HOUSING OPERATIONS" = "11-9141",
  "SENIOR VICE PRESIDENT OF HOUSING OPERATIONS" = "11-9141", "VICE PRESIDENT OF REAL ESTATE DEVELOPMENT" = "11-9141",
  "PROJECT MANAGER" = "13-1082",
  "OFFICE MANAGER" = "43-1011", "CHIEF CLERK" = "43-1011",
  "CLERK" = "43-9061", "ASSISTANT CLERK" = "43-9061",
  # ---- museums, libraries, arts ----
  "ARCHIVES DIRECTOR" = "25-4011", "ASSISTANT CURATOR" = "25-4012",
  "DIRECTOR OF COLLECTIONS" = "25-4012", "VICE PRESIDENT OF COLLECTIONS" = "25-4012",
  "COLLECTIONS MANAGER" = "25-4012", "DIRECTOR OF EXHIBITIONS" = "25-4012",
  "LIBRARY DIRECTOR" = "25-4022", "GENEALOGIST" = "19-3093",
  "ASSOCIATE ARTISTIC DIRECTOR" = "27-1011", "FOUNDING ARTISTIC DIRECTOR" = "27-1011",
  # ---- public safety, facilities, trades, hospitality ----
  "FIRE CHIEF" = "33-1021", "FIRE OFFICER" = "33-1021", "FIRE FIGHTER" = "33-2011",
  "GUARD" = "33-9032", "DIRECTOR OF SECURITY" = "33-1091",
  "CUSTODIAN" = "37-2011", "JANITOR" = "37-2011", "CARETAKER" = "37-2011",
  "MAINTENANCE" = "49-9071", "GOLF COURSE SUPERINTENDENT" = "37-1012",
  "LINEMAN" = "49-9051", "DISPATCHER" = "43-5030",
  "BARTENDER" = "35-3011", "CHEF" = "35-1011", "EXECUTIVE CHEF" = "35-1011",
  "GIFT SHOP MANAGER" = "41-1011", "GOLF PROFESSIONAL" = "27-2022",
  # ---- union staff ----
  "BUSINESS GAENT" = "13-1199", "ASSISTANT BUSINESS AGENT" = "13-1199", "STAFF REPRESENTATIVE" = "13-1199",
  # ---- standards added by round 2 of the Part VII review (2026-10-09) ----
  "LOAN OFFICER" = "13-2072", "ACTUARY" = "15-2011", "MISSIONARY" = "21-2099",
  "PARAMEDIC" = "29-2043", "PERFUSIONIST" = "29-9099", "PILOT" = "53-2012"
)

missing <- setdiff(names(codes), tx$title.standard)
if (length(missing)) stop("not in the taxonomy: ", paste(missing, collapse = ", "))

# the full code path and official label for a code at any level
lookup <- function(code) {
  r <- soc[det == code]; lab <- "LEV4"; lv <- 4
  if (!nrow(r)) { r <- soc[brd == code]; lab <- "LEV3"; lv <- 3 }
  if (!nrow(r)) { r <- soc[min == code]; lab <- "LEV2"; lv <- 2 }
  if (!nrow(r)) stop("SOC code ", code, " is not in tidy-soc-codes.csv")
  r <- r[1]
  list(major.group = r$maj, minor.group = r$min,
       broad.group = if (lv >= 3) r$brd else "", detailed.occupation = if (lv == 4) r$det else "",
       soc.label = r[[lab]])
}

cols <- c("soc.label", "major.group", "minor.group", "broad.group", "detailed.occupation")
log <- list()
for (s in names(codes)) {
  i <- which(tx$title.standard == s)
  new <- lookup(codes[[s]])
  for (col in cols) {
    old <- tx[[col]][i]
    if (!identical(old, new[[col]])) {
      log[[length(log) + 1]] <- data.table(title.standard = s, column = col, old = old, new = new[[col]])
      set(tx, i, col, new[[col]])
    }
  }
}
log <- rbindlist(log)
if (!nrow(log)) { message("nothing to change; the codes are already in"); quit(save = "no") }
write_xwalk(tx, "title-taxonomy")
log[, applied := as.character(Sys.Date())]
lp <- file.path(xwalk_dir, "soc-codes-log.csv")
fwrite(if (file.exists(lp)) rbind(fread(lp, colClasses = "character", na.strings = NULL), log[, lapply(.SD, as.character)]) else log, lp, quote = TRUE)
rebuild_xwalks()
message(sprintf("%d cells changed in %d titles", nrow(log), uniqueN(log$title.standard)))
