# 16-nobox-model.R
# P4 of the synthid plan: a model that resolves roles without the Part VII
# checkboxes, for 990-EZ filers (FINDINGS.md F-028).
#
# Teacher: step 09's role (role.final) for full-990 people in the 2019-2023
# sample (13-panel-run.R). On a full 990, step 09 uses the trustee, officer and
# key-employee boxes. The model sees only what a 990-EZ also has:
# - the title (keyword flags, crosswalk level, board role, domain, SOC group,
#   match type);
# - pay and hours;
# - the person's place in the filing (pay and hours rank and share);
# - the filing's make-up (people, paid people, other CEO-level or board
#   titles, people with the same title);
# - status flags (former, interim, co-, founder, ex officio) and expense size.
#
# Rows are people (role.primary). Training and test split by organization
# (EIN2), 80/20, so no organization is on both sides. The baseline is step 09
# itself run as if on a 990-EZ (boxes hidden): the rules the model has to beat.
#
# Model: xgboost, multi:softprob, 6 classes. Writes:
# - PARTVII_DIR/sample/nobox-model.json (the model);
# - panel-results/nobox-*.csv (the scores).
#
#   Rscript data-raw/partvii-validation/16-nobox-model.R

source("tests/regression/regression-helpers.R"); tc_load_package()
suppressMessages({ library(data.table); library(xgboost) })
PV  <- Sys.getenv("PARTVII_DIR", file.path(Sys.getenv("USERPROFILE", "~"), "Documents", "PARTVII"))
RES <- "data-raw/partvii-validation/panel-results"

d <- rbindlist(lapply(list.files(file.path(PV, "sample"), "^classified-\\d{4}\\.rds$", full.names = TRUE), readRDS), fill = TRUE)

# ---- features (nothing from the checkboxes) ---------------------------------
source("data-raw/partvii-validation/_nobox-features.R")

p <- features(d)[role.primary == TRUE]
full <- p[return_type == "990" & role.final %in% classes]
set.seed(2026)
orgs <- unique(full$EIN2); test_orgs <- sample(orgs, round(0.2 * length(orgs)))
tr <- full[!EIN2 %in% test_orgs]; te <- full[EIN2 %in% test_orgs]
y <- function(x) match(x$role.final, classes) - 1L

# ---- model ------------------------------------------------------------------
dtr <- xgb.DMatrix(mm(tr), label = y(tr)); dte <- xgb.DMatrix(mm(te), label = y(te))
params <- list(objective = "multi:softprob", num_class = length(classes), eta = 0.1, max_depth = 8,
               subsample = 0.8, colsample_bytree = 0.8, min_child_weight = 5, eval_metric = "mlogloss", nthread = 8)
t0 <- Sys.time()
fit <- xgb.train(params, dtr, nrounds = 400, evals = list(test = dte), early_stopping_rounds = 25, verbose = 0)
message(sprintf("trained in %.1f min, %d rounds", as.numeric(Sys.time() - t0, units = "mins"), xgb.attr(fit, "best_iteration")))
invisible(xgb.save(fit, file.path(PV, "sample", "nobox-model.json")))
# xgboost 3 returns a people x classes matrix; older versions a flat vector
probs <- function(dm) { pr <- predict(fit, dm); if (is.null(dim(pr))) pr <- matrix(pr, ncol = length(classes), byrow = TRUE); pr }
pred <- function(x, dm) classes[max.col(probs(dm), ties.method = "first")]
te$model <- pred(te, dte)

# ---- baseline: step 09 on the test filings with the boxes hidden ----------------
hid <- d[object.id %in% te$object.id]
for (b in c("dtk.indiv.trustee.x", "dtk.inst.trustee.x", "dtk.officer.x", "dtk.key.empl.x", "dtk.high.comp.x")) set(hid, j = b, value = 0)
hid[, formtype := "990EZ"]
hid <- as.data.table(resolve_roles(as.data.frame(hid[, !c("role.position", "role.final", "role.board", "role.ceo",
                                                          "role.source", "role.primary", "org.leadership")])))
te[hid[role.primary == TRUE], on = c("object.id", "person.id"), rules_nobox := i.role.final]

# ---- scores -------------------------------------------------------------------
acc <- function(a, b, w) round(100 * sum(w[a == b]) / sum(w), 1)
cat(sprintf("\ntest: %d people in %d filings (organizations held out)\n", nrow(te), uniqueN(te$object.id)))
cat(sprintf("agreement with step 09 using the boxes (weighted): model %.1f%% | step 09 without boxes %.1f%%\n",
            acc(te$model, te$role.final, te$weight), acc(te$rules_nobox, te$role.final, te$weight)))
nb <- te$role.final != "BOARD"
cat(sprintf("non-board people only:                             model %.1f%% | step 09 without boxes %.1f%%\n",
            acc(te$model[nb], te$role.final[nb], te$weight[nb]), acc(te$rules_nobox[nb], te$role.final[nb], te$weight[nb])))
by_size <- te[, .(people = .N, model = acc(model, role.final, weight), rules_nobox = acc(rules_nobox, role.final, weight)),
              by = size][order(match(size, c("$0", "$1-150k", "$150k-1m", "$1m-10m", "$10m-100m", "$100m+")))]
print(by_size)
pr <- function(cls, pr_col) { p <- te[[pr_col]] == cls; t <- te$role.final == cls
  c(precision = round(sum(p & t) / sum(p), 2), recall = round(sum(p & t) / sum(t), 2)) }
cmp <- t(sapply(classes, function(k) c(model = pr(k, "model"), rules = pr(k, "rules_nobox"))))
print(cmp)
cat("\nmodel confusion (rows = step 09 with boxes):\n"); print(table(step09 = te$role.final, model = te$model))
fwrite(by_size, file.path(RES, "nobox-by-size.csv"))
fwrite(as.data.table(cmp, keep.rownames = "role"), file.path(RES, "nobox-by-role.csv"))
imp <- xgb.importance(model = fit); fwrite(head(imp, 30), file.path(RES, "nobox-importance.csv"))
cat("\ntop features:\n"); print(head(imp[, .(Feature, Gain = round(Gain, 3))], 15))
