# 14-panel-calibrate.R
# Calibrate step 09 on the 2019-2023 sample (13-panel-run.R output; FINDINGS.md
# F-026). All rates are per person (one row per person, role.primary) and
# weighted by the filing's design weight unless marked "unweighted".
#
# 1. Calibration by stratum (form x expense size x paid people):
#    - title coverage: the share of people whose title maps to a standard;
#    - board titles confirmed by the trustee box, and trustee boxes explained
#      by a board title;
#    - paid executive titles (CEO/OFFICER level) confirmed by the officer box;
#    - how often step 09 overrides the title-only default role;
#    - per filing: CEO coverage and the leadership type.
# 2. Cross-time consistency, for the panel organizations. People are linked
#    within an organization across consecutive years by synthid EMP_ID
#    (15-panel-synthid.R), or by normalized name if that has not been run. It
#    reports:
#    - whether each person's role stays the same;
#    - the most common flips;
#    - whether the CEO is the same person from one year to the next.
# 3. Silver labels: full-990 people for whom the title, the checkboxes and the
#    pay all agree with step 09's role. These go to
#    PARTVII_DIR/sample/silver-labels.rds, with a summary in the repository.
#
# Writes data-raw/partvii-validation/panel-results/*.csv.
#
#   Rscript data-raw/partvii-validation/14-panel-calibrate.R

suppressMessages(library(data.table))
PV  <- Sys.getenv("PARTVII_DIR", file.path(Sys.getenv("USERPROFILE", "~"), "Documents", "PARTVII"))
RES <- "data-raw/partvii-validation/panel-results"; dir.create(RES, showWarnings = FALSE)
d <- rbindlist(lapply(list.files(file.path(PV, "sample"), "^classified-\\d{4}\\.rds$", full.names = TRUE), readRDS), fill = TRUE)
p <- d[role.primary == TRUE]
num <- function(x) { x <- suppressWarnings(as.numeric(x)); x[is.na(x)] <- 0; x }
box <- function(x) suppressWarnings(as.numeric(x)) %in% 1
size_lv <- c("$0", "$1-150k", "$150k-1m", "$1m-10m", "$10m-100m", "$100m+", "missing")
p[, `:=`(
  size = factor(size, size_lv),
  paid = num(tot.comp) > 0,
  t_tru = box(dtk.indiv.trustee.x) | box(dtk.inst.trustee.x),
  t_off = box(dtk.officer.x),
  t_key = box(dtk.key.empl.x) | box(dtk.high.comp.x),
  mapped = !is.na(title.standard) & nzchar(title.standard),
  t_board = !is.na(board.role) & nzchar(board.role),
  t_exec = emp.level %in% c("CEO", "OFFICER"),
  t_level = emp.level %in% c("MANAGER", "PROFESSIONAL", "STAFF"))]
p[, default := fifelse(t_board, "BOARD", fifelse(nzchar(fcoalesce(emp.level, "")), emp.level, NA_character_))]
w <- function(x, wt) if (length(x)) round(100 * sum(wt[x], na.rm = TRUE) / sum(wt, na.rm = TRUE), 1) else NA_real_
wm <- function(cond, wt) { ok <- !is.na(cond); if (!any(ok)) NA_real_ else round(100 * sum(wt[ok & cond]) / sum(wt[ok]), 1) }

# ---- 1. calibration by stratum ---------------------------------------------------
cal <- p[, .(
  people = .N, filings = uniqueN(object.id),
  title_mapped = wm(mapped, weight),
  board_title_has_trustee_box = wm(ifelse(t_board, t_tru, NA), weight),
  trustee_box_has_board_title = wm(ifelse(t_tru, t_board, NA), weight),
  paid_exec_title_has_officer_box = wm(ifelse(t_exec & paid, t_off, NA), weight),
  step09_overrides_title = wm(ifelse(!is.na(default), role.final != default, NA), weight)),
  by = .(return_type, size)][order(return_type, size)]
fil <- d[, .(org.leadership = org.leadership[1], has_ceo = any(role.final == "CEO" & role.primary),
             paid_any = any(num(tot.comp) > 0), weight = weight[1], size = size[1], return_type = return_type[1],
             paid_bin = paid_bin[1]), by = object.id]
fil[, size := factor(size, size_lv)]
lead <- fil[, .(filings = .N,
                ceo_if_paid = wm(ifelse(paid_any, has_ceo, NA), weight),
                designated = wm(org.leadership == "designated", weight),
                imputed = wm(org.leadership == "imputed", weight),
                board_governed = wm(org.leadership == "board_governed", weight),
                no_paid = wm(org.leadership == "no_paid", weight)),
            by = .(return_type, size)][order(return_type, size)]
cal <- merge(cal, lead[, !"filings"], by = c("return_type", "size"))
by_paid <- p[, .(people = .N, title_mapped = wm(mapped, weight),
                 board_title_has_trustee_box = wm(ifelse(t_board, t_tru, NA), weight),
                 paid_exec_title_has_officer_box = wm(ifelse(t_exec & paid, t_off, NA), weight),
                 step09_overrides_title = wm(ifelse(!is.na(default), role.final != default, NA), weight)),
             by = .(return_type, paid_bin)][order(return_type, paid_bin)]
# 990-EZ returns have no checkboxes: the box measures do not apply
boxcols <- c("board_title_has_trustee_box", "trustee_box_has_board_title", "paid_exec_title_has_officer_box")
for (b in intersect(boxcols, names(cal)))     set(cal, which(cal$return_type == "990EZ"), b, NA_real_)
for (b in intersect(boxcols, names(by_paid))) set(by_paid, which(by_paid$return_type == "990EZ"), b, NA_real_)
fwrite(cal, file.path(RES, "calibration-by-size.csv")); fwrite(by_paid, file.path(RES, "calibration-by-paid.csv"))
roles <- dcast(p, return_type + size ~ role.final, value.var = "weight", fun.aggregate = sum)
role_cols <- setdiff(names(roles), c("return_type", "size"))
tot <- rowSums(roles[, role_cols, with = FALSE])
roles[, (role_cols) := lapply(.SD, function(x) round(100 * x / tot, 1)), .SDcols = role_cols]
fwrite(roles, file.path(RES, "roles-by-size.csv"))

# ---- 2. cross-time consistency (panel) ---------------------------------------------
# a person is followed by synthid's EMP_ID (15-panel-synthid.R) when the linked
# panel exists; otherwise by exact normalized name within the organization
lf <- file.path(PV, "sample", "panel-linked.rds")
if (file.exists(lf)) {
  lk <- readRDS(lf)
  pn <- lk[, .(EIN2 = ein, taxyr, nm = EMP_ID, role.final, size = factor(size, size_lv), return_type, weight)]
  link_method <- "synthid EMP_ID"
} else {
  pn <- p[design == "panel" & !is.na(dtk.name) & nzchar(dtk.name)]
  pn[, nm := gsub("\\s+", " ", trimws(gsub("[^A-Z ]", " ", toupper(dtk.name))))]
  pn[, nm := gsub("\\b(MR|MRS|MS|DR|JR|SR|II|III|IV|PHD|MD|ESQ|CPA|REV)\\b", "", nm)]
  pn[, nm := gsub("\\s+", " ", trimws(nm))]
  pn <- pn[nzchar(nm)]
  link_method <- "exact normalized name"
}
pn <- pn[!duplicated(pn[, .(EIN2, taxyr, nm)])]
pn[, yr := as.integer(taxyr)]
nxt <- merge(pn[, .(EIN2, nm, yr, role0 = role.final, size, return_type, weight)],
             pn[, .(EIN2, nm, yr = yr - 1L, role1 = role.final)], by = c("EIN2", "nm", "yr"))
stab <- nxt[, .(pairs = .N, same_role = round(100 * mean(role0 == role1), 1)), by = .(return_type, size)][order(return_type, size)]
stab_role <- nxt[, .(pairs = .N, same_role = round(100 * mean(role0 == role1), 1)), by = role0][order(-pairs)]
flips <- nxt[role0 != role1, .N, by = .(from = role0, to = role1)][order(-N)][, share_of_pairs := round(100 * N / nrow(nxt), 2)]
ceo <- pn[role.final == "CEO", .(ceo = paste(sort(unique(nm)), collapse = "|")), by = .(EIN2, yr, size, return_type)]
ceo2 <- merge(ceo, ceo[, .(EIN2, yr = yr - 1L, ceo1 = ceo)], by = c("EIN2", "yr"))
ceo2[, same := mapply(function(a, b) length(intersect(strsplit(a, "|", fixed = TRUE)[[1]], strsplit(b, "|", fixed = TRUE)[[1]])) > 0, ceo, ceo1)]
ceo_stab <- ceo2[, .(org_year_pairs = .N, same_ceo = round(100 * mean(same), 1)), by = .(return_type, size)][order(return_type, size)]
has_ceo <- pn[, .(has = any(role.final == "CEO")), by = .(EIN2, yr)]
ceo_gap <- merge(has_ceo, has_ceo[, .(EIN2, yr = yr - 1L, has1 = has)], by = c("EIN2", "yr"))
fwrite(stab, file.path(RES, "crosstime-role-stability.csv")); fwrite(stab_role, file.path(RES, "crosstime-by-role.csv"))
fwrite(flips, file.path(RES, "crosstime-flips.csv")); fwrite(ceo_stab, file.path(RES, "crosstime-ceo.csv"))

# ---- 3. silver labels --------------------------------------------------------------
f990 <- p$return_type == "990"
p[, silver := f990 & fcase(
  role.final == "BOARD",                   t_board & t_tru & !t_key,
  role.final %in% c("CEO", "OFFICER"),     t_exec & t_off & paid,
  role.final %in% c("MANAGER", "PROFESSIONAL", "STAFF"),
                                           t_level & emp.level == role.final & paid & !t_tru & !t_off,
  default = FALSE)]
sil <- p[silver == TRUE, .(object.id, person.id, EIN2, taxyr, return_type, size, paid_bin, stratum, weight,
                           title.raw, title.v7, title.standard, emp.level, board.role, tot.comp, tot.hours,
                           dtk.indiv.trustee.x, dtk.inst.trustee.x, dtk.officer.x, dtk.key.empl.x, dtk.high.comp.x,
                           role.final, role.board, role.ceo)]
saveRDS(sil, file.path(PV, "sample", "silver-labels.rds"))
sil_sum <- p[f990, .(people = .N, silver = sum(silver), silver_share = round(100 * mean(silver), 1)), by = .(size, role.final)][order(size, role.final)]
fwrite(sil_sum, file.path(RES, "silver-summary.csv"))

# ---- report ---------------------------------------------------------------------
cat(sprintf("people: %d (%d filings, %d organizations); panel people linked across years (%s): %d pairs\n\n",
            nrow(p), uniqueN(p$object.id), uniqueN(p$EIN2), link_method, nrow(nxt)))
cat("== calibration by size (weighted %) ==\n"); print(cal)
cat("\n== by paid people ==\n"); print(by_paid)
cat("\n== cross-time: same role next year, by size ==\n"); print(stab)
cat("\n== cross-time: by role ==\n"); print(stab_role)
cat("\n== most common flips ==\n"); print(head(flips, 12))
cat("\n== same CEO next year ==\n"); print(ceo_stab)
cat(sprintf("\nfilings with a CEO one year and none the next: %.1f%%; none then one: %.1f%%\n",
            100 * mean(ceo_gap$has & !ceo_gap$has1), 100 * mean(!ceo_gap$has & ceo_gap$has1)))
cat(sprintf("\n== silver labels: %d of %d full-990 people (%.1f%%) ==\n", sum(p$silver), sum(f990), 100 * mean(p$silver[f990])))
print(dcast(sil_sum, size ~ role.final, value.var = "silver"))
