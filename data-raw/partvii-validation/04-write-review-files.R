# 04-write-review-files.R
# Write one review file per standardized title (TitleTxt7, before the
# crosswalk) to <PARTVII>/review/titles/<block>/<id>_<slug>.md, plus the index
# and the review queues. Each file opens with a YAML block holding the facts
# and an empty `review:` section for the reviewer's decision; the body shows
# the evidence. 05-collect-reviews.R reads the decisions back.
#
#   Rscript data-raw/partvii-validation/04-write-review-files.R [max_tier]
# max_tier (default 4) limits which tiers get a file; every title is in the
# index either way. Existing files that already hold a decision are kept.

source("data-raw/partvii-validation/_config.R")
suppressMessages(library(data.table))
suppressMessages(devtools::load_all(quiet = TRUE))
args <- commandArgs(trailingOnly = TRUE)
max_tier <- if (length(args)) as.integer(args[1]) else 4L

prof <- as.data.table(arrow::read_parquet(file.path(pv_data, "title-profiles.parquet")))
var  <- as.data.table(arrow::read_parquet(file.path(pv_data, "title-variants.parquet")))
ex   <- as.data.table(arrow::read_parquet(file.path(pv_data, "title-examples.parquet")))
xw   <- as.data.table(get_googlesheets_title_xwalk())
tx   <- as.data.table(get_googlesheets_title_taxonomy())

# suggestions already on file: the workbook's to-do tab and the board patch
sugg <- list()
todo <- tryCatch(as.data.table(readxl::read_excel("data-dev/title-taxonomy-map.xlsx", "to-do")), error = function(e) NULL)
if (!is.null(todo)) sugg$todo <- todo[, .(TitleTxt7 = title.variant, suggested = title.standard, source = "title-taxonomy-map.xlsx to-do tab")]
bp <- if (file.exists("data-dev/board_crosswalk_patch.csv")) fread("data-dev/board_crosswalk_patch.csv") else NULL
if (!is.null(bp)) sugg$board <- bp[, .(TitleTxt7 = title.variant, suggested = proposed.title.standard,
                                       source = sprintf("board_crosswalk_patch.csv (%s, confidence %s)", decision, confidence))]
sugg <- rbindlist(sugg)

# nearest crosswalk variants for titles that are missing (tiers 1-3 only: cost)
std_variants <- unique(xw$title.variant)
near <- function(x) {
  d <- stringdist::stringdist(x, std_variants, method = "jw", p = 0.1)
  o <- order(d)[1:3]
  paste(sprintf("%s -> %s (%.2f)", std_variants[o], xw$title.standard[match(std_variants[o], xw$title.variant)], d[o]), collapse = "; ")
}
# a crosswalk variant contained in the title as whole words (longest first)
contained <- function(x) {
  hits <- std_variants[nchar(std_variants) < nchar(x) & stringi::stri_detect_fixed(paste0(" ", x, " "), paste0(" ", std_variants, " "))]
  if (!length(hits)) return("")
  hits <- hits[order(-nchar(hits))][1:min(3, length(hits))]
  paste(sprintf("%s -> %s", hits, xw$title.standard[match(hits, xw$title.variant)]), collapse = "; ")
}

slug <- function(x) { s <- gsub("[^A-Z0-9]+", "-", toupper(x)); s <- gsub("^-|-$", "", s); substr(ifelse(s == "", "EMPTY", s), 1, 60) }
prof[, block := sprintf("%07d-%07d", (rank - 1) %/% 1000 * 1000 + 1, ((rank - 1) %/% 1000 + 1) * 1000)]
prof[, file := file.path("titles", block, paste0(id, "_", slug(TitleTxt7), ".md"))]

q <- function(x) { x <- ifelse(is.na(x), "", as.character(x)); paste0('"', gsub('"', '\\\\"', gsub("\\\\", "\\\\\\\\", x)), '"') }
pct <- function(x) ifelse(is.na(x), "-", sprintf("%.0f%%", 100 * x))
num <- function(x) ifelse(is.na(x), "-", format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE))
md_esc <- function(x) gsub("\\|", "\\\\|", ifelse(is.na(x), "", x))

has_decision <- function(path) {
  if (!file.exists(path)) return(FALSE)
  l <- readLines(path, n = 80, warn = FALSE)
  any(grepl("^  status: (?!pending)", l, perl = TRUE))
}

write_one <- function(p) {
  path <- file.path(pv_review, p$file)
  if (has_decision(path)) return("kept")
  vall <- var[.(p$TitleTxt7), nomatch = NULL]
  v <- vall[seq_len(min(nrow(vall), 20))]
  e <- ex[.(p$TitleTxt7), nomatch = NULL]
  s <- if (nrow(sugg)) sugg[TitleTxt7 == p$TitleTxt7] else data.table()
  nr <- if (p$crosswalk == "missing" && p$tier <= 3) near(p$TitleTxt7) else ""
  ct <- if (p$crosswalk == "missing" && p$tier <= 3) contained(p$TitleTxt7) else ""
  y <- c("---",
    paste0("id: ", p$id),
    paste0("title_v7: ", q(p$TitleTxt7)),
    paste0("queue: ", p$queue, "            # ADD (not in crosswalk) | INSPECT (automatic check raised) | CONFIRM"),
    paste0("tier: ", p$tier, "                 # 1 = 1,000+ rows, 2 = 100-999 rows, 3 = 5+ filings, 4 = tail"),
    paste0("crosswalk: ", p$crosswalk),
    paste0("current_standard: ", q(p$title_standard)),
    paste0("auto_checks: ", q(p$auto_checks)),
    paste0("n_rows: ", p$n_rows), paste0("n_filings: ", p$n_filings), paste0("n_orgs: ", p$n_orgs),
    paste0("years: ", p$first_year, "-", p$last_year),
    paste0("suggested_standard: ", q(if (nrow(s)) s$suggested[1] else "")),
    paste0("suggested_by: ", q(if (nrow(s)) s$source[1] else "")),
    "review:",
    "  status: pending           # pending | confirmed | remap | add | add_new_standard | fix_cleaning | not_a_title | ambiguous",
    "  standard: \"\"              # the title.standard this title should map to (remap / add / add_new_standard)",
    "  new_taxonomy: \"\"          # for add_new_standard: \"domain.category / domain.label / flags\", e.g. \"operations / finance / emp mgr\"",
    "  cleaning_issue: \"\"        # for fix_cleaning: which step mangles it (02 dates .. 07 c-suite) and how",
    "  reviewer: \"\"",
    "  date: \"\"",
    "  notes: \"\"",
    "---", "")
  b <- c(
    sprintf("# %s `%s`", p$id, if (p$TitleTxt7 == "") "(empty)" else p$TitleTxt7), "",
    if (p$crosswalk == "mapped")
      sprintf("**Crosswalk:** maps to **%s** -> %s / %s%s. Taxonomy flags: %s.", p$title_standard,
              ifelse(is.na(p$domain.category), "(no taxonomy row)", p$domain.category), ifelse(is.na(p$domain.label), "", p$domain.label),
              ifelse(is.na(p$soc.label) | p$soc.label == "", "", paste0(" (SOC: ", p$soc.label, ")")), ifelse(p$tax_flags == "", "none", p$tax_flags))
    else "**Crosswalk:** not in the crosswalk; `title.standard` is NA and the person gets no category.",
    "", sprintf("**Automatic checks:** %s", ifelse(p$auto_checks == "", "none", p$auto_checks)), "",
    "## Who holds this title", "",
    "| person-title rows | filings | orgs | years | 990-EZ share |", "|---|---|---|---|---|",
    sprintf("| %s | %s | %s | %s-%s | %s |", num(p$n_rows), num(p$n_filings), num(p$n_orgs), p$first_year, p$last_year, pct(p$pct_ez)), "",
    "Part VII checkboxes (990 filers only; the 990-EZ has none):", "",
    "| officer | trustee (indiv) | trustee (inst) | key employee | highest comp | former |", "|---|---|---|---|---|---|",
    sprintf("| %s | %s | %s | %s | %s | %s |", pct(p$pct_officer), pct(p$pct_trustee), pct(p$pct_trustee_inst), pct(p$pct_key_empl), pct(p$pct_high_comp), pct(p$pct_former_box)), "",
    "| paid (any comp) | median pay if paid | median hours/week | 35+ hours | second title in same entry |", "|---|---|---|---|---|",
    sprintf("| %s | $%s | %s | %s | %s |", pct(p$pct_paid), num(p$med_pay_paid), ifelse(is.na(p$med_hours), "-", format(p$med_hours)), pct(p$pct_fulltime), pct(p$pct_second_title)), "",
    sprintf("Status words removed in step 6: former %s, interim %s, founder %s, ex officio %s, partial year %s, co- %s. Step 7 rewrote the title in %s of rows.",
            pct(p$pct_former), pct(p$pct_interim), pct(p$pct_founder), pct(p$pct_exofficio), pct(p$pct_partial), pct(p$pct_co), pct(p$pct_step7_changed)), "",
    sprintf("Rows by year: %s", p$rows_by_year), "",
    if (nchar(nr) || nchar(ct) || nrow(s)) c("## Leads for the decision", "",
      if (nrow(s)) paste0("- Suggested: **", s$suggested, "** (", s$source, ")"),
      if (nchar(ct)) paste0("- Crosswalk variants contained in this title: ", ct),
      if (nchar(nr)) paste0("- Nearest crosswalk variants (Jaro-Winkler distance): ", nr), ""),
    sprintf("## Raw titles that become this title (top %d of %d)", nrow(v), nrow(vall)), "",
    "Each raw title is shown after step 3 (conjunctions), step 5 (spelling) and step 6 (status words removed); `#` is its position when the entry was split into several titles.", "",
    "| rows | raw title | step 3 | step 5 | step 6 | # |", "|---|---|---|---|---|---|",
    sprintf("| %s | %s | %s | %s | %s | %s |", num(v$n_rows), md_esc(v$TITLE_RAW), md_esc(v$TitleTxt3), md_esc(v$TitleTxt5), md_esc(v$TitleTxt6), v$Num.Titles), "",
    "## Example filings", "",
    "| year | form | org | raw title | hours | pay | off | trustee | key | HCE | former | XML |", "|---|---|---|---|---|---|---|---|---|---|---|---|",
    sprintf("| %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | [xml](%s) |", e$TAX_YEAR, e$RETURN_TYPE, md_esc(e$ORG_NAME_L1), md_esc(e$TITLE_RAW),
            e$TOT_HOURS, num(e$TOT_COMP), ifelse(is.na(e$OFFICER_X), "-", e$OFFICER_X), ifelse(is.na(e$TRUST_INDIV_X), "-", e$TRUST_INDIV_X),
            ifelse(is.na(e$KEY_EMPL_X), "-", e$KEY_EMPL_X), ifelse(is.na(e$HIGH_COMP_X), "-", e$HIGH_COMP_X), ifelse(is.na(e$FORMER_X), "-", e$FORMER_X), e$URL), "")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(enc2utf8(c(y, b)), path, useBytes = TRUE)
  "written"
}

todo_rows <- prof[tier <= max_tier]
message(sprintf("writing %d review files (tiers <= %d of %d titles)", nrow(todo_rows), max_tier, nrow(prof)))

# one task per block of 1,000 files, each carrying only its own variants and examples
var[, block := prof$block[match(TitleTxt7, prof$TitleTxt7)]]
ex[, block := prof$block[match(TitleTxt7, prof$TitleTxt7)]]
tasks <- lapply(split(todo_rows, todo_rows$block), function(p) list(
  p = p, var = var[block == p$block[1]], ex = ex[block == p$block[1]]))
var[, block := NULL]; ex[, block := NULL]
run_block <- function(t) {
  assign("var", setkey(t$var, TitleTxt7), envir = globalenv()); assign("ex", setkey(t$ex, TitleTxt7), envir = globalenv())
  vapply(seq_len(nrow(t$p)), function(i) write_one(t$p[i]), "")
}
workers <- as.integer(Sys.getenv("PV_WORKERS", "12"))
cl <- parallel::makeCluster(workers)
parallel::clusterExport(cl, c("write_one", "run_block", "has_decision", "near", "contained", "q", "pct", "num", "md_esc",
                              "sugg", "std_variants", "xw", "pv_review"))
invisible(parallel::clusterEvalQ(cl, suppressMessages(library(data.table))))
t0 <- Sys.time()
res <- unlist(parallel::clusterApplyLB(cl, tasks, run_block))
parallel::stopCluster(cl)
message(sprintf("written %d, kept (already reviewed) %d, in %s", sum(res == "written"), sum(res == "kept"), format(round(Sys.time() - t0))))

# index and queues
idx <- prof[, .(id, title_v7 = TitleTxt7, queue, tier, crosswalk, current_standard = title_standard, auto_checks,
                n_rows, n_filings, n_orgs, cum_pct_rows = round(cum_pct_rows, 5), pct_officer, pct_trustee, pct_paid, med_hours,
                file = ifelse(tier <= max_tier, file, ""))]
fwrite(idx, file.path(pv_review, "index.csv"))
for (qq in c("ADD", "INSPECT", "CONFIRM")) fwrite(idx[queue == qq], file.path(pv_review, sprintf("queue-%s.csv", tolower(qq))))
