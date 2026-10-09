# 19-nobox-model-v2.R
# Retrain the no-checkbox model with real 990-EZ examples from organizations
# that switch forms (FINDINGS.md F-029), and compare it with the first model
# (16-nobox-model.R).
#
# - The switching organizations (18-switch-labels.R) split in half by
#   organization, seed 2026.
# - The training half's strict labels (same title in both years) join the
#   full-990 training data: these are 990-EZ-year people labeled with their
#   full-990-year role. They get weight 3, because 990-EZ is where the model is
#   used.
# - The other half is the test set. Both models are scored on it, and on the
#   990-EZ people in the labeled set (17-nobox-gold.R's matching).
#
# The better model is saved as PARTVII_DIR/sample/nobox-model-v2.ubj.
#
#   Rscript data-raw/partvii-validation/19-nobox-model-v2.R

source("tests/regression/regression-helpers.R"); tc_load_package()
suppressMessages({ library(data.table); library(xgboost); library(arrow) })
source("data-raw/partvii-validation/_nobox-features.R")
PV  <- Sys.getenv("PARTVII_DIR", file.path(Sys.getenv("USERPROFILE", "~"), "Documents", "PARTVII"))
RES <- "data-raw/partvii-validation/panel-results"

# full-990 teacher data, as in 16-nobox-model.R (same organization split)
d <- rbindlist(lapply(list.files(file.path(PV, "sample"), "^classified-\\d{4}\\.rds$", full.names = TRUE), readRDS), fill = TRUE)
full <- features(d)[role.primary == TRUE & return_type == "990" & role.final %in% classes]
set.seed(2026); test_orgs <- sample(unique(full$EIN2), round(0.2 * length(unique(full$EIN2))))
tr990 <- full[!EIN2 %in% test_orgs]

# switchers: features from the 990-EZ-year filings, labels from the full-990 year
lab <- readRDS(file.path(PV, "sample", "switch-labels.rds"))[same_title == TRUE & label %in% classes]
cl  <- readRDS(file.path(PV, "sample", "switch-classified.rds"))
fz  <- features(cl)[role.primary == TRUE]
ez  <- merge(fz, lab[, .(object.id, person.id, label)], by = c("object.id", "person.id"))
set.seed(2026); sw_train <- sample(unique(ez$EIN2), round(0.5 * uniqueN(ez$EIN2)))
ez_tr <- ez[EIN2 %in% sw_train]; ez_te <- ez[!EIN2 %in% sw_train]

y <- function(lbl) match(lbl, classes) - 1L
params <- list(objective = "multi:softprob", num_class = length(classes), eta = 0.1, max_depth = 8,
               subsample = 0.8, colsample_bytree = 0.8, min_child_weight = 5, eval_metric = "mlogloss", nthread = 8)
X <- rbind(mm(tr990), mm(ez_tr))
dtr <- xgb.DMatrix(X, label = c(y(tr990$role.final), y(ez_tr$label)),
                   weight = c(rep(1, nrow(tr990)), rep(3, nrow(ez_tr))))
fit2 <- xgb.train(params, dtr, nrounds = 151, verbose = 0)
fit1 <- xgb.load(file.path(PV, "sample", "nobox-model.json"))
pred <- function(fit, x) { pr <- predict(fit, xgb.DMatrix(mm(x))); if (is.null(dim(pr))) pr <- matrix(pr, ncol = length(classes), byrow = TRUE)
  classes[max.col(pr, ties.method = "first")] }
ez_te[, `:=`(m1 = pred(fit1, ez_te), m2 = pred(fit2, ez_te))]
sc <- function(x) x[, .(people = .N, step09 = round(100 * mean(role.final == label), 1),
                        model_v1 = round(100 * mean(m1 == label), 1), model_v2 = round(100 * mean(m2 == label), 1))]
res <- rbind(cbind(subset = "held-out switchers, strict", sc(ez_te)),
             cbind(subset = "  non-board label", sc(ez_te[label != "BOARD"])),
             cbind(subset = "  paid in 990-EZ year", sc(ez_te[paid == 1])))
cat(sprintf("training: %d full-990 people + %d 990-EZ switcher people (weight 3)\n", nrow(tr990), nrow(ez_tr)))
print(res)
rr <- function(col) t(sapply(classes, function(cl) { p <- ez_te[[col]] == cl; t <- ez_te$label == cl
  c(precision = round(sum(p & t) / max(sum(p), 1), 2), recall = round(sum(p & t) / max(sum(t), 1), 2)) }))
cat("\nby role, v1:\n"); print(rr("m1")); cat("by role, v2:\n"); print(rr("m2"))

# the labeled set's 990-EZ people (matching as in 17-nobox-gold.R)
g <- fread("data-raw/partvii-validation/panel-results/nobox-gold-people.csv", colClasses = "character", na.strings = NULL)
gf <- readRDS(file.path(PV, "sample", "gold-features.rds"))
gf[, `:=`(m1 = pred(fit1, gf), m2 = pred(fit2, gf))]
k <- function(o, t, c) paste(o, toupper(trimws(t)), round(as.numeric(c)))
gf[, .k := k(object.id, title.raw, tot.comp)]; g[, .k := k(OBJECTID, title.raw, comp)]
one <- function(col) vapply(g$.k, function(key) { r <- unique(gf[[col]][gf$.k == key]); if (length(r) == 1) r else NA_character_ }, "")
g[, `:=`(m1 = one("m1"), m2 = one("m2"))]
hit <- function(lab, pr) ifelse(lab == "DUAL", pr %in% c("CEO", "OFFICER", "BOARD"), lab == pr)
ge <- g[!role %in% c("UNSURE", "NONE") & !is.na(m1)]
gs <- ge[, .(people = .N, model_v1 = round(100 * mean(hit(role, m1)), 1), model_v2 = round(100 * mean(hit(role, m2)), 1)), by = formtype]
cat("\nlabeled set:\n"); print(gs)
fwrite(rbind(res, fill = TRUE), file.path(RES, "nobox-v2-switch.csv")); fwrite(gs, file.path(RES, "nobox-v2-gold.csv"))
xgb.save(fit2, file.path(PV, "sample", "nobox-model-v2.ubj"))
