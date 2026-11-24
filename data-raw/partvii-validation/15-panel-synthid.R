# 15-panel-synthid.R
# Link the panel organizations' people across 2019-2023 with synthid, so the
# cross-time checks in 14-panel-calibrate.R follow a person by a stable EMP_ID
# instead of an exact name match (FINDINGS.md F-026).
#
# Input: the step 09 output for the panel (13-panel-run.R), one row per person
# per year (role.primary). Names are parsed with peopleparser::parse_names(),
# as synthid's own preprocessing does (synthid/dev/01_preprocess_year.R), with
# add_nicknames = TRUE (peopleparser >= 0.1.0) so each record also carries its
# first-name variant columns: first_name_type, first_name_formal,
# first_name_nicknames, first_name_canonical and canonical_rule. link_panel()
# passes them through to its output. synthid already scores nickname pairs with
# the same variant lexicon, so these columns do not change the links.
# synthid::link_panel() then runs one organization at a time, in parallel
# across organizations, with its tuned weighted threshold. titleclassifier's
# output has no TABLE_ID, so the per-filing person key person.id stands in
# for it.
#
# synthid is loaded from its source checkout (SYNTHID_DIR, default ../synthid)
# with devtools::load_all(), as its batch runner does.
#
# Writes PARTVII_DIR/sample/panel-linked.rds: the person-year rows with EMP_ID,
# EMP_N_YEARS and the step 09 role columns.
#
#   Rscript data-raw/partvii-validation/15-panel-synthid.R [workers]

# Since peopleparser 0.2.0, parse_names() runs in the current session unless it
# is given workers = (or a future plan is set), so the call below passes
# workers = NW. Its workers load the installed peopleparser, so install it;
# devtools::load_all() of a peopleparser checkout makes parse_names() stop.
suppressMessages({ library(data.table); library(future); library(furrr); library(peopleparser) })
if (packageVersion("peopleparser") < "0.2.0")
  stop("needs peopleparser >= 0.2.0 (first-name variants, workers argument): ",
       "remotes::install_github(\"Nonprofit-Open-Data-Collective/peopleparser@v0.2.0\")")
NICK <- c("first_name_type", "first_name_formal", "first_name_nicknames", "first_name_canonical", "canonical_rule")
PV    <- Sys.getenv("PARTVII_DIR", file.path(Sys.getenv("USERPROFILE", "~"), "Documents", "PARTVII"))
SYN   <- normalizePath(Sys.getenv("SYNTHID_DIR", "../synthid"), winslash = "/")
args  <- commandArgs(trailingOnly = TRUE)
NW    <- if (length(args)) as.integer(args[1]) else max(1L, parallel::detectCores(logical = FALSE) - 1L)

d <- rbindlist(lapply(list.files(file.path(PV, "sample"), "^classified-\\d{4}\\.rds$", full.names = TRUE),
                      function(f) readRDS(f)[design == "panel" & role.primary == TRUE]), fill = TRUE)
d <- d[!is.na(dtk.name) & nzchar(trimws(dtk.name))]
message(sprintf("panel: %d person-years in %d organizations", nrow(d), uniqueN(d$EIN2)))

parsed <- peopleparser::parse_names(d$dtk.name, add_nicknames = TRUE, workers = NW)
pn <- data.table(
  ein = d$EIN2, org.name = d$org.name, taxyr = as.character(d$taxyr), name = parsed$name,
  OBJECTID = d$object.id, TABLE_ID = d$person.id,
  salutation = parsed$salutation, first_name = parsed$first_name, middle_name = parsed$middle_name,
  last_name = parsed$last_name, suffix = parsed$suffix, gender = parsed$gender,
  title.standard = d$title.standard,
  role.final = d$role.final, role.board = d$role.board, role.ceo = d$role.ceo,
  org.leadership = d$org.leadership, tot.comp = d$tot.comp, tot.hours = d$tot.hours,
  size = d$size, return_type = d$return_type, weight = d$weight,
  parsed[NICK])

# batches of whole organizations, linked in parallel
eins <- unique(pn$ein)
batches <- split(eins, ceiling(seq_along(eins) / 100))
plan(multisession, workers = NW)
link_batch <- function(b) {
  Sys.setenv(OMP_NUM_THREADS = "1"); data.table::setDTthreads(1L)
  suppressMessages(devtools::load_all(SYN, quiet = TRUE, export_all = FALSE))
  x <- as.data.frame(pn[ein %in% b])
  out <- synthid::link_panel(x)
  data.table::as.data.table(out)
}
t0 <- Sys.time()
linked <- rbindlist(future_map(batches, link_batch, .options = furrr_options(seed = TRUE,
                    globals = c("pn", "SYN"), packages = c("data.table", "devtools"))), fill = TRUE)
plan(sequential)
saveRDS(linked, file.path(PV, "sample", "panel-linked.rds"))
message(sprintf("linked %d person-years -> %d persons in %.1f min; persons seen in 2+ years: %d",
                nrow(linked), uniqueN(linked$EMP_ID), as.numeric(Sys.time() - t0, units = "mins"),
                uniqueN(linked$EMP_ID[linked$EMP_N_YEARS >= 2])))
