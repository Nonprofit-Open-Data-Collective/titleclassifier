# 20-role-model-ship.R
# Train the role model that ships with the package (inst/extdata/role-model/),
# using the package's own role_model_features() so training and prediction
# share one definition (FINDINGS.md F-029).
#
# The package sees only Part VII, so the shipped model has no expense-size
# feature. First the cost of dropping size is measured, on the same splits as
# 19-nobox-model-v2.R:
# - training: the full-990 training organizations, plus half of the
#   form-switching organizations' 990-EZ people (weight 3);
# - scoring: the other half of the switchers, and the labeled set.
# Then the final model is trained on all full-990 people and all switchers.
#
#   Rscript data-raw/partvii-validation/20-role-model-ship.R

source("tests/regression/regression-helpers.R"); tc_load_package()
suppressMessages({ library(data.table); library(xgboost) })
PV  <- Sys.getenv("PARTVII_DIR", file.path(Sys.getenv("USERPROFILE", "~"), "Documents", "PARTVII"))
RES <- "data-raw/partvii-validation/panel-results"
OUT <- "inst/extdata/role-model"; dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
classes <- .role_classes

# full-990 people (teacher: step 09 with the boxes)
d <- rbindlist(lapply(list.files(file.path(PV, "sample"), "^classified-\\d{4}\\.rds$", full.names = TRUE), readRDS), fill = TRUE)
Xd <- role_model_features(d)
keep <- d$role.primary & d$return_type == "990" & d$role.final %in% classes
X990 <- Xd[keep, ]; y990 <- d$role.final[keep]; org990 <- d$EIN2[keep]

# switchers' 990-EZ people (label: full-990-year role, same title both years)
lab <- readRDS(file.path(PV, "sample", "switch-labels.rds"))[same_title == TRUE & label %in% classes]
cl  <- readRDS(file.path(PV, "sample", "switch-classified.rds"))
Xc  <- role_model_features(cl)
i   <- match(paste(lab$object.id, lab$person.id), paste(cl$object.id, cl$person.id))
i   <- i[!is.na(i)]; lab <- lab[match(paste(cl$object.id[i], cl$person.id[i]), paste(lab$object.id, lab$person.id))]
Xez <- Xc[i, ]; yez <- lab$label; orgez <- lab$EIN2; step09ez <- lab$step09

params <- list(objective = "multi:softprob", num_class = length(classes), eta = 0.1, max_depth = 8,
               subsample = 0.8, colsample_bytree = 0.8, min_child_weight = 5, eval_metric = "mlogloss", nthread = 8)
train <- function(X, y, w) xgb.train(params, xgb.DMatrix(X, label = match(y, classes) - 1L, weight = w), nrounds = 151, verbose = 0)
pred  <- function(fit, X) { pr <- predict(fit, xgb.DMatrix(X)); if (is.null(dim(pr))) pr <- matrix(pr, ncol = length(classes), byrow = TRUE)
  classes[max.col(pr, ties.method = "first")] }

# ---- the cost of dropping expense size (same splits as 19) ------------------------
set.seed(2026); test_orgs <- sample(unique(org990), round(0.2 * length(unique(org990))))
set.seed(2026); sw_train <- sample(unique(orgez), round(0.5 * length(unique(orgez))))
a <- !org990 %in% test_orgs; b <- orgez %in% sw_train
fit_eval <- train(rbind(X990[a, ], Xez[b, ]), c(y990[a], yez[b]), c(rep(1, sum(a)), rep(3, sum(b))))
te_pred <- pred(fit_eval, Xez[!b, ])
sw <- data.table(label = yez[!b], model = te_pred, step09 = step09ez[!b])
res <- rbind(sw[, .(subset = "held-out switchers, strict", people = .N,
                    step09 = round(100 * mean(step09 == label), 1), model_no_size = round(100 * mean(model == label), 1))],
             sw[label != "BOARD", .(subset = "  non-board label", people = .N,
                    step09 = round(100 * mean(step09 == label), 1), model_no_size = round(100 * mean(model == label), 1))])
cat("shipped model design (no expense size), evaluation fit:\n"); print(res)

gf <- readRDS(file.path(PV, "sample", "gold-features.rds"))
gf$m <- pred(fit_eval, role_model_features(gf)[ , colnames(X990)])
g  <- fread(file.path(RES, "nobox-gold-people.csv"), colClasses = "character", na.strings = NULL)
k  <- function(o, t, c) paste(o, toupper(trimws(t)), round(as.numeric(c)))
gf[, .k := k(object.id, title.raw, tot.comp)]; g[, .k := k(OBJECTID, title.raw, comp)]
g[, m := vapply(.k, function(key) { r <- unique(gf$m[gf$.k == key]); if (length(r) == 1) r else NA_character_ }, "")]
hit <- function(lab, pr) ifelse(lab == "DUAL", pr %in% c("CEO", "OFFICER", "BOARD"), lab == pr)
gs <- g[!role %in% c("UNSURE", "NONE") & !is.na(m), .(people = .N, model_no_size = round(100 * mean(hit(role, m)), 1)), by = formtype]
cat("\nlabeled set:\n"); print(gs)
fwrite(res, file.path(RES, "role-model-ship-switch.csv")); fwrite(gs, file.path(RES, "role-model-ship-gold.csv"))

# ---- the shipped model: all full-990 people and all switchers ----------------------
fit <- train(rbind(X990, Xez), c(y990, yez), c(rep(1, nrow(X990)), rep(3, nrow(Xez))))
xgb.save(fit, file.path(OUT, "role-model.ubj"))
writeLines(colnames(X990), file.path(OUT, "feature-names.txt"))
cat(sprintf("\nshipped: %s (%.1f MB), %d features, trained on %d full-990 + %d 990-EZ people\n",
            file.path(OUT, "role-model.ubj"), file.size(file.path(OUT, "role-model.ubj")) / 2^20,
            ncol(X990), nrow(X990), nrow(Xez)))
