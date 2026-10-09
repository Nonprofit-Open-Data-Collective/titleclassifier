# 03-domains.R
# Domain vocabulary pass on title-taxonomy.csv (Phase 4 of
# data-dev/CROSSWALK-REVIEW.md, FINDINGS.md F-018). Run 2026-10-08; kept for
# provenance. Re-running changes nothing once the domains are in.
#
# Writes domains.csv, the controlled list of domain.category / domain.label
# pairs, and moves every taxonomy row onto it:
# - Two new categories:
#   - executive / general management: CEOs, and officers in administration
#     whose SOC code is general management (11-1011, 11-1021) or blank.
#     Before this, 74% of employee rows sat in operations / administration.
#   - governance / board: every board title.
# - Labels that meant the same thing are merged: religious -> religion;
#   comms-pr, marketing-pr, marketing-sales -> marketing and communications.
# - Industry labels move under industry-specific (education, hospitality,
#   engineering and architecture), and programs and events under operations.
# - Rows with a blank, NA or "xxx" domain are assigned one by one.
# Every change is logged in domains-log.csv.
#
#   Rscript data-raw/crosswalks/03-domains.R

source("data-raw/crosswalks/_crosswalk-io.R")
tx <- read_xwalk("title-taxonomy")

vocab <- rbindlist(list(
  data.table(domain.category = "executive", domain.label = "general management",
             description = "top staff leadership and general-management officers"),
  data.table(domain.category = "governance", domain.label = "board",
             description = "board members and board officers"),
  data.table(domain.category = "operations",
             domain.label = c("administration", "business development", "facilities", "finance",
                              "fundraising and membership", "human resources", "legal",
                              "marketing and communications", "programs and events",
                              "research and development", "technology", "writers and editors"),
             description = "a support function found in any organization"),
  data.table(domain.category = "industry-specific",
             domain.label = c("arts and history", "education", "engineering and architecture",
                              "higher education", "hospitality", "housing", "medical", "military",
                              "public safety", "recreation and sports", "religion", "social services",
                              "utilities"),
             description = "work specific to the organization's field"),
  data.table(domain.category = "non-job title", domain.label = c("missing", "none"),
             description = c("no title, or boilerplate in place of one", "not a job title"))
))

old <- copy(tx[, .(title.standard, domain.category, domain.label)])
set_dom <- function(i, cat, lab) { set(tx, i, "domain.category", cat); set(tx, i, "domain.label", lab) }
by_name <- function(names, cat, lab) {
  i <- match(names, tx$title.standard)
  if (anyNA(i)) stop("not in the taxonomy: ", paste(names[is.na(i)], collapse = ", "))
  set_dom(i, cat, lab)
}

# ---- 1. merge labels; place labels under one category -------------------------
relabel <- c(religious = "religion", `comms-pr` = "marketing and communications",
             `marketing-pr` = "marketing and communications", `marketing-sales` = "marketing and communications",
             employee = "administration")
i <- which(tx$domain.label %in% names(relabel)); set(tx, i, "domain.label", relabel[tx$domain.label[i]])
industry <- c("education", "hospitality", "engineering and architecture")
set(tx, which(tx$domain.label %in% industry), "domain.category", "industry-specific")
ops <- vocab[domain.category == "operations", domain.label]
set(tx, which(tx$domain.label %in% ops), "domain.category", "operations")
set(tx, which(tx$domain.category == "non-job title" & tx$domain.label == "xxx"), "domain.label", "none")

# ---- 2. rows with no usable domain ---------------------------------------------
by_name(c("ASSISTANT CHIEF", "DEPUTY CHIEF", "FIRE CHIEF", "FIRE FIGHTER"), "industry-specific", "public safety")
by_name(c("CHEF", "EXECUTIVE CHEF", "CONCESSIONS"), "industry-specific", "hospitality")
by_name("CHIEF MARSHAL", "industry-specific", "military")
by_name(c("PRINCIPAL", "SUPERINTENDENT", "VICE PRESIDENT OF EDUCATION AND GUEST EXPERIENCE"), "industry-specific", "education")
by_name(c("VICE PRESIDENT OF HOUSING", "VICE PRESIDENT OF HOUSING OPERATIONS",
          "SENIOR VICE PRESIDENT OF HOUSING OPERATIONS"), "industry-specific", "housing")
by_name(c("CAO", "INTERN"), "operations", "administration")
by_name("CARETAKER", "operations", "facilities")
by_name(c("FINANCE AND ADMINISTRATION", "VICE PRESIDENT OF FINANCE AND ADMINISTRATION", "VICE PRESIDENT OF LENDING"),
        "operations", "finance")
by_name("VICE PRESIDENT AND CIO", "operations", "technology")
by_name(c("DIRECTOR OF CONTENT", "DIRECTOR OF ENGAGEMENT", "DIRECTOR OF MARKETING AND COMMUNICATIONS",
          "VICE PRESIDENT OF MARKETING AND COMMUNICATIONS", "VICE PRESIDENT OF MARKETING AND PUBLICITY"),
        "operations", "marketing and communications")
by_name(c("DIRECTOR OF CORPORATE X", "DIRECTOR OF STRATEGY AND ENGAGEMENT"), "operations", "business development")
by_name(c("CIVIC DIRECTOR", "POLITICAL DIRECTOR", "VICE PRESIDENT OF COMMUNITY"), "operations", "programs and events")
by_name(c("ALTERNATE", "AMBASSADOR", "CHIEF", "COMMISSIONER", "JUNIOR", "OUTGOING", "POLITICIAN"), "non-job title", "none")
by_name("NA", "non-job title", "missing")

# ---- 3. executive and governance -------------------------------------------------
general <- tx$detailed.occupation %in% c("11-1011", "11-1021", "") & !nzchar(tx$broad.group) | tx$broad.group %in% c("11-1010", "11-1020")
in_admin <- tx$domain.label %in% c("administration", "", "xxx", "NA")
exec <- which(tx$emp.level %in% c("CEO", "OFFICER") & in_admin & general)
set_dom(exec, "executive", "general management")
set_dom(which(nzchar(tx$board.role)), "governance", "board")

# ---- 4. check, log, write ------------------------------------------------------
off <- tx[!vocab, on = .(domain.category, domain.label)]
if (nrow(off)) stop("rows outside the vocabulary:\n", paste(off$title.standard, off$domain.category, off$domain.label, sep = " | ", collapse = "\n"))
log <- merge(old, tx[, .(title.standard, new.category = domain.category, new.label = domain.label)], by = "title.standard")
log <- log[domain.category != new.category | domain.label != new.label]
if (!nrow(log)) { message("nothing to change; the domains are already in"); quit(save = "no") }
setnames(log, c("domain.category", "domain.label"), c("old.category", "old.label"))
write_xwalk(vocab, "domains")
write_xwalk(tx, "title-taxonomy")
fwrite(log, file.path(xwalk_dir, "domains-log.csv"), quote = TRUE)
rebuild_xwalks()
message(sprintf("%d titles moved; %d domains in the vocabulary", nrow(log), nrow(vocab)))
