# 11-gold-v2-assemble.R
# Assemble the v2 role-label set, gold-check/gold-v2.csv (FINDINGS.md F-024),
# from:
# - v2-existing-mapped.csv: the August labels, mapped to the v2 vocabulary
#   (10-gold-v2-build.R);
# - v2-labels-A.csv and v2-labels-B.csv: two blind labeling agents, for the
#   24 refresh cases (R..) and the 160 new people (N..);
# - v2-user-decisions.csv, if present: the user's call on the cases the agents
#   split, and any override.
#
# A label counts when both agents give the same role (and, for BOARD and DUAL,
# the same board_role). Cases they split, and that the user hasn't decided, go
# to v2-disagreements.csv and stay out of gold-v2.csv until decided.
#
#   Rscript data-raw/partvii-validation/11-gold-v2-assemble.R

D  <- "data-raw/partvii-validation/gold-check"
rd <- function(f) utils::read.csv(file.path(D, f), colClasses = "character", na.strings = character(0))
ex <- rd("v2-existing-mapped.csv"); rk <- rd("v2-refresh-key.csv"); nk <- rd("v2-new-key.csv")
A  <- rd("v2-labels-A.csv"); B <- rd("v2-labels-B.csv")
ud <- if (file.exists(file.path(D, "v2-user-decisions.csv"))) rd("v2-user-decisions.csv") else
        data.frame(case_id = character(0), role = character(0), board_role = character(0), note = character(0))

roles <- c("CEO", "OFFICER", "MANAGER", "PROFESSIONAL", "STAFF", "BOARD", "DUAL", "UNSURE", "NONE")
posts <- c("CHAIR", "VICE CHAIR", "SECRETARY", "TREASURER", "MEMBER")
cases <- c(rk$case_id, nk$case_id)
for (x in list(A = A, B = B)) {
  stopifnot(setequal(x$case_id, cases), !anyDuplicated(x$case_id), all(x$role %in% roles))
  stopifnot(all(x$board_role[x$role %in% c("BOARD", "DUAL")] %in% posts))
}

L <- merge(A, B, by = "case_id", suffixes = c(".A", ".B"))
br <- function(role, b) ifelse(role %in% c("BOARD", "DUAL"), b, "")
L$agree <- L$role.A == L$role.B & br(L$role.A, L$board_role.A) == br(L$role.B, L$board_role.B)
L$role <- ifelse(L$agree, L$role.A, ""); L$board_role <- ifelse(L$agree, br(L$role.A, L$board_role.A), "")
L$ceo_note <- ifelse(L$agree & L$ceo_note.A == L$ceo_note.B, L$ceo_note.A, "")
L$label_source <- ifelse(L$agree, "both agents", "")
ud_case <- ud[nzchar(ud$case_id), ]
i <- match(ud_case$case_id, L$case_id)
L$role[i] <- ud_case$role; L$board_role[i] <- ud_case$board_role
L$label_source[i] <- "decided at the user's request"

# ---- the evidence for each case -------------------------------------------------
ev <- c("OBJECTID", "org.name", "taxyr", "title.raw", "title.v7", "title.standard", "comp", "hrs",
        "cb_tru", "cb_off", "cb_key")
yn <- function(x) ifelse(x %in% c("TRUE", "Y"), "Y", "-")
mk <- function(case_id, set, stratum, formtype, d, role, board_role, ceo_note, src) {
  e <- d[, ev]; for (c in c("cb_tru", "cb_off", "cb_key")) e[[c]] <- yn(e[[c]])
  data.frame(case_id = case_id, set = set, stratum = stratum, formtype = formtype, e, role = role,
             board_role = board_role, ceo_note = ceo_note, label_source = src, check.names = FALSE)
}
old <- ex[ex$needs_label != "TRUE", ]
part_old <- mk("", "august", old$bucket, "990", old, old$role, old$board_role, old$ceo_note, "august label, mapped")
ref <- merge(rk, ex[ex$needs_label == "TRUE", ], by = c("OBJECTID", "title.raw", "comp"))
ref <- merge(ref[, setdiff(names(ref), c("role", "board_role", "ceo_note"))],
             L[, c("case_id", "role", "board_role", "ceo_note", "label_source")], by = "case_id")
part_ref <- mk(ref$case_id, "august", ref$bucket, "990", ref, ref$role, ref$board_role, ref$ceo_note, ref$label_source)
nw <- merge(nk, L[, c("case_id", "role", "board_role", "ceo_note", "label_source")], by = "case_id")
nw$cb_key <- ifelse(nw$cb_key == "Y" | nw$cb_high == "Y", "Y", "-")
part_new <- mk(nw$case_id, "new", nw$stratum, nw$formtype, nw, nw$role, nw$board_role, nw$ceo_note, nw$label_source)

gold <- rbind(part_old, part_ref, part_new)
# decisions that override an August label, keyed by filing + raw title + pay
ud_key <- ud[!nzchar(ud$case_id), ]
j <- match(paste(ud_key$OBJECTID, ud_key$title.raw, ud_key$comp), paste(gold$OBJECTID, gold$title.raw, gold$comp))
if (anyNA(j)) stop("user decision matches no labeled person: ", paste(ud_key$title.raw[is.na(j)], collapse = ", "))
gold$role[j] <- ud_key$role; gold$board_role[j] <- ud_key$board_role
gold$label_source[j] <- "decided at the user's request"
open <- gold[!nzchar(gold$role), ]
gold <- gold[nzchar(gold$role), ]
utils::write.csv(gold, file.path(D, "gold-v2.csv"), row.names = FALSE)

dis <- L[!L$agree & !L$case_id %in% ud$case_id,
         c("case_id", "role.A", "board_role.A", "confidence.A", "notes.A", "role.B", "board_role.B", "confidence.B", "notes.B")]
dis <- merge(dis, rbind(data.frame(case_id = rk$case_id, title.raw = rk$title.raw, comp = rk$comp, stratum = "august refresh"),
                        data.frame(case_id = nk$case_id, title.raw = nk$title.raw, comp = nk$comp, stratum = nk$stratum)),
             by = "case_id")
utils::write.csv(dis, file.path(D, "v2-disagreements.csv"), row.names = FALSE)

cat(sprintf("gold-v2: %d labeled people (%d august mapped, %d august refreshed, %d new); %d open for the user\n",
            nrow(gold), nrow(part_old), sum(gold$set == "august") - nrow(part_old), sum(gold$set == "new"), nrow(open)))
cat(sprintf("agents agreed on %d of %d cases (%.0f%%)\n", sum(L$agree), nrow(L), 100 * mean(L$agree)))
print(table(gold$role)); print(table(gold$formtype))
