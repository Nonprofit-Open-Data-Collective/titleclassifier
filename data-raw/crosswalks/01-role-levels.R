# 01-role-levels.R
# One-time conversion of title-taxonomy.csv from the 12 role flags to two
# categorical columns (FINDINGS.md F-016, F-020). Run 2026-10-08; kept for
# provenance. It stops if the table is already converted.
#
#   emp.level   CEO | OFFICER | MANAGER | PROFESSIONAL | STAFF | blank
#   board.role  CHAIR | VICE CHAIR | SECRETARY | TREASURER | MEMBER | blank
#
# - PROFESSIONAL is a non-managerial job that is often reported because of its
#   pay (the 990's "highest compensated employee" group): physicians, lawyers,
#   professors, scientists, performers, coaches. Whether a person is a key or
#   highly compensated employee in a filing is still decided from the
#   checkboxes and pay, not by this column.
# - dir.vp is retired: VICE PRESIDENT titles and deputies go to OFFICER;
#   DIRECTOR titles go to MANAGER, except where the August routing evidence
#   (synthid/dev/dirvp_crosswalk_suggestions.csv) said OFFICER.
# - Board standards other than the five core ones become
#   title-standardization variants of the core standard for their role, and
#   their taxonomy rows are removed.
#
# build-crosswalks.R derives the legacy flags (emp, ceo, ... mem) from the two
# columns, so the pipeline's output columns are unchanged. Every decision is
# logged in role-levels-log.csv.
#
#   Rscript data-raw/crosswalks/01-role-levels.R

source("data-raw/crosswalks/_crosswalk-io.R")
tx <- read_xwalk("title-taxonomy")
xw <- read_xwalk("title-standardization")
if ("emp.level" %in% names(tx)) stop("title-taxonomy.csv is already converted")

x <- function(col) tx[[col]] == "X"
std <- tx$title.standard
level <- rep("", nrow(tx)); role <- rep("", nrow(tx)); rule <- rep("", nrow(tx))
put <- function(i, lv = "", rl = "", why) { level[i] <<- lv; role[i] <<- rl; rule[i] <<- why }
by_name <- function(names, lv = "", rl = "", why) {
  i <- match(names, std)
  if (anyNA(i)) stop("not in the taxonomy: ", paste(names[is.na(i)], collapse = ", "))
  put(i, lv, rl, why)
}

# ---- 1. employee levels from the old flags ------------------------------------
put(which(x("ceo")), "CEO", why = "ceo flag")
put(which(x("c.level") & !x("ceo")), "OFFICER", why = "c.level flag")
put(which(x("mgr")), "MANAGER", why = "mgr flag")

dv <- which(x("dir.vp"))
vp <- grepl("^((SENIOR |EXECUTIVE |SENIOR EXECUTIVE |REGIONAL |PROGRAMS )?VICE PRESIDENT|DEPUTY )", std[dv], perl = TRUE)
put(dv[vp], "OFFICER", why = "dir.vp: vice president or deputy")
put(dv[!vp], "MANAGER", why = "dir.vp: director")
sug <- read.csv("../synthid/dev/dirvp_crosswalk_suggestions.csv", colClasses = "character")
up <- intersect(std[dv[!vp]], sug$title.standard[sug$suggested_level == "OFFICER/C-LEVEL"])
by_name(up, "OFFICER", why = "dir.vp: August evidence routes up")
by_name(c("ASSISTANT VICE PRESIDENT", "ASSOCIATE VICE PRESIDENT", "JUNIOR VICE PRESIDENT"),
        "MANAGER", why = "below vice president")
by_name(c("EXECUTIVE ARTISTIC DIRECTOR", "FOUNDING ARTISTIC DIRECTOR", "PRODUCING DIRECTOR"),
        "OFFICER", why = "co-head of an arts organization")
by_name("SCOUT EXECUTIVE", "CEO", why = "chief executive of a scout council")

# spec splits into PROFESSIONAL and STAFF
sp <- which(x("spec"))
put(sp, "STAFF", why = "spec flag")
by_name(c(
  "ACCOUNTANT", "AUDITOR", "ATTORNEY", "LAWYER", "LEGAL ADVISOR", "JUDGE ADVOCATE",
  "DOCTOR", "SURGEON", "NURSE", "THERAPIST", "PRESIDENT OF MEDICAL STAFF",
  "PROFESSOR", "FACULTY", "LECTURER", "ARCHITECT", "HISTORIAN",
  "CURATOR", "SENIOR CURATOR", "INVESTMENT OFFICER",
  "ACTOR", "ATHLETE", "CONDUCTOR", "COMPOSER", "MUSICIAN",
  "BUSINESS AGENT", "ASSISTANT BUSINESS AGENT", "BUSINESS GAENT", "BUSINESS REPRESENTATIVE"),
  "PROFESSIONAL", why = "spec: non-managerial job often reported for its pay")
by_name("MEDICAL OFFICER", "PROFESSIONAL", why = "a physician, not a manager")
by_name(c("COMPTROLLER", "FINANCE OFFICER"), "MANAGER", why = "finance manager, not staff")
by_name(c("CORPORATE SECRETARY", "FOUNDER"), "OFFICER", why = "officer title")

# employees that had no level, and jobs that had no flags at all
by_name(c("EXECUTIVE SENIOR VICE PRESIDENT", "VICE PRESIDENT AND CIO", "FIRE CHIEF"), "OFFICER", why = "emp without a level")
by_name(c("CAPTAIN", "LIEUTENANT", "ASSISTANT CHIEF", "DEPUTY CHIEF"), "MANAGER", why = "fire or uniformed officer")
by_name(c("CHEF", "EXECUTIVE CHEF", "INTERN", "CONCESSIONS"), "STAFF", why = "a job with no flags")

# ---- 2. board roles ------------------------------------------------------------
b <- which(x("board"))
put(b, rl = "MEMBER", why = "board flag")
put(which(x("board") & x("pres")), rl = "CHAIR", why = "pres flag")
put(which(x("board") & x("vp")), rl = "VICE CHAIR", why = "vp flag")
put(which(x("board") & x("sec")), rl = "SECRETARY", why = "sec flag")
put(which(x("board") & x("treas") & !x("sec") & !x("vp")), rl = "TREASURER", why = "treas flag")
by_name(c("BOARD CHAIR", "COUNCIL CHAIR", "COUNCIL PRESIDENT", "PRESIDENT TREASURER"), rl = "CHAIR", why = "presiding officer")
by_name("VICE CHAIR", rl = "VICE CHAIR", why = "vice chair")
by_name("STAFF REPRESENTATIVE", "PROFESSIONAL", why = "union staff, like a business agent")
# officers of clubs, lodges and posts are board officers (DECISIONS.md P2)
by_name(c("GUILD PRESIDENT", "COMMODO"), rl = "CHAIR", why = "P2: presiding officer of a club or lodge")
by_name(c("VICE COMMANDER", "VICE REGENT", "DEPUTY GRAND", "DEPUTY GRAND KNIGHT", "DIRECTOR VICE CHAIR",
          "VICE PRESIDENT OF SOFTBALL", "VICE PRESIDENT OF STANDARDS AND VALUES", "VICE PRESIDENT OF MEMBER DEVELOPMENT",
          "VICE PRESIDENT OF MEMBER EDUCATION", "VICE PRESIDENT OF MEMBER RECRUITMENT",
          "VICE PRESIDENT OF FRATERNITY DEVELOPMENT", "VICE PRESIDENT OF ALUMNAE RELATIONS",
          "VICE PRESIDENT OF CORRESPONDENCE", "VICE PRESIDENT OF COMMITTEE RELATIONS", "VICE PRESIDENT SECRETARY"),
        rl = "VICE CHAIR", why = "P2: vice officer of a club, lodge or chapter")
by_name(c("ORIENTAL GUIDE", "FLEET CAPTAIN", "DIRECTOR ALUMNA"), rl = "MEMBER", why = "P2: ritual or club officer")
by_name("GOVERNANCE", rl = "MEMBER", why = "board domain")

tx[, `:=`(emp.level = level, board.role = role)]
if (any(nzchar(tx$emp.level) & nzchar(tx$board.role))) stop("a row has both emp.level and board.role")

# ---- 3. a COACH standard for the review to target ------------------------------
coach <- tx[1][, names(tx) := ""]
coach[, `:=`(title.standard = "COACH", domain.category = "industry-specific", domain.label = "recreation and sports",
             emp.level = "PROFESSIONAL", soc.label = "Coaches and Scouts", major.group = "27",
             minor.group = "27-2000", broad.group = "27-2020", detailed.occupation = "27-2022")]
tx <- rbind(tx, coach)
rule <- c(rule, "new standard; coach variants are left to the Part VII review (F-020)")

# ---- 4. collapse board standards into the five core standards -----------------
core <- c(CHAIR = "BOARD PRESIDENT", `VICE CHAIR` = "BOARD VICE PRESIDENT", SECRETARY = "BOARD SECRETARY",
          TREASURER = "BOARD TREASURER", MEMBER = "BOARD MEMBER")
tail_b <- tx[nzchar(board.role) & !title.standard %in% core, .(title.standard, target = core[board.role])]
xw_log <- list()
for (k in seq_len(nrow(tail_b))) {
  s <- tail_b$title.standard[k]; t <- tail_b$target[k]
  i <- which(xw$title.standard == s)
  if (length(i)) xw_log[[length(xw_log) + 1]] <- data.table(title.variant = xw$title.variant[i], old = s, new = t)
  set(xw, i, "title.standard", t)
  if (!s %in% xw$title.variant) {       # the old standard keeps matching as a variant
    xw <- rbind(xw, data.table(title.variant = s, title.standard = t, strata = "", strata.label = "",
                               notes = "F-016: former board standard"), fill = TRUE)
    xw_log[[length(xw_log) + 1]] <- data.table(title.variant = s, old = "(new variant)", new = t)
  }
}
xw_log <- rbindlist(xw_log)

log <- data.table(title.standard = tx$title.standard, emp.level = tx$emp.level, board.role = tx$board.role,
                  rule = rule, collapsed_into = tail_b$target[match(tx$title.standard, tail_b$title.standard)])
log[is.na(collapsed_into), collapsed_into := ""]
tx <- tx[!title.standard %in% tail_b$title.standard]

# ---- 5. drop the flags; write -------------------------------------------------
flags <- c("emp", "ceo", "c.level", "dir.vp", "mgr", "spec", "board", "pres", "vp", "sec", "treas", "mem")
tx[, (flags) := NULL]
setcolorder(tx, c("title.standard", "domain.category", "domain.label", "emp.level", "board.role"))
setorder(tx, title.standard)
write_xwalk(tx, "title-taxonomy")
write_xwalk(xw, "title-standardization")
fwrite(log, file.path(xwalk_dir, "role-levels-log.csv"), quote = TRUE)
fwrite(xw_log, file.path(xwalk_dir, "board-collapse-log.csv"), quote = TRUE)

# ---- 6. bring the Part VII draft decisions along -------------------------------
pv <- "data-raw/partvii-validation"
ch <- fread(file.path(pv, "crosswalk-changes.csv"), colClasses = "character", na.strings = NULL)
ch[title.standard %in% tail_b$title.standard, title.standard := tail_b$target[match(title.standard, tail_b$title.standard)]]
fwrite(ch, file.path(pv, "crosswalk-changes.csv"))   # these files use the default quoting
ta <- fread(file.path(pv, "taxonomy-additions.csv"), colClasses = "character", na.strings = NULL)
new_level <- c(`NO TITLE` = "", `FIRE OFFICER` = "MANAGER", `ASSISTANT PRINCIPAL` = "MANAGER", BARTENDER = "STAFF",
               `CHIEF OFFICER` = "OFFICER", DISPATCHER = "STAFF", FOREMAN = "MANAGER", `GOLF COURSE SUPERINTENDENT` = "MANAGER",
               `GOLF PROFESSIONAL` = "PROFESSIONAL", `HEAD OF DIVISION` = "MANAGER", LINEMAN = "STAFF",
               PHARMACIST = "PROFESSIONAL", `SENIOR FELLOW` = "PROFESSIONAL", SCIENTIST = "PROFESSIONAL",
               `TRAINING COORDINATOR` = "STAFF", VETERINARIAN = "PROFESSIONAL")
if (length(setdiff(ta$title.standard, names(new_level)))) stop("taxonomy-additions has rows this script does not know")
dom <- vapply(strsplit(ta$new_taxonomy, "/", fixed = TRUE), function(p) paste(trimws(p[1:2]), collapse = " / "), "")
ta[, new_taxonomy := ifelse(nzchar(new_level[title.standard]), paste(dom, new_level[title.standard], sep = " / "), dom)]
fwrite(ta, file.path(pv, "taxonomy-additions.csv"))

rebuild_xwalks()
message(sprintf("converted %d taxonomy rows; collapsed %d board standards (%d variant rows); added COACH",
                nrow(log), nrow(tail_b), nrow(xw_log)))
