# Step 09: organization types and the CEO thesaurus (FINDINGS.md F-033).

test_that("org_type_from_codes maps 501(c) subsections and NTEE codes", {
  expect_equal(org_type_from_codes(c(NA, NA, "J40", "K20", NA, NA, NA, NA),
                                   c(19, 8, 5, 5, 7, 12, 9, 13)),
               c("veterans", "fraternal", "union", NA, "club", "coop", "trust", "cemetery"))
  expect_equal(org_type_from_codes(c("B43", "B43", "M24", "A69", "X21", "UNDEFINED", "P20"), 3,
                                   c("STATE UNIVERSITY", "STATE UNIVERSITY FOUNDATION", "", "", "", "", "")),
               c("university", NA, "fire", "orchestra", "ministry", NA, NA))
})

test_that("org_type_from_name reads the name and the filing's titles", {
  expect_equal(org_type_from_name(c("AMERICAN LEGION POST 12", "BPO ELKS LODGE 99", "IBEW LOCAL 46",
                                    "XYZ CREDIT UNION", "OAK HILL COUNTRY CLUB", "SMITH COLLEGE",
                                    "COLLEGE OF PHYSICIANS", "FIRST BAPTIST CHURCH", "OAK GROVE CEMETERY")),
               c("veterans", "fraternal", "union", NA, "club", "university", NA, "ministry", "cemetery"))
  expect_equal(org_type_from_name("POST 7 INC", "COMMANDER | ADJUTANT"), "veterans")
  expect_equal(org_type_from_name("LOCAL 3", "PRESIDENT | BUSINESS MANAGER"), "union")
})

test_that("org_types prefers a supplied type, then codes, then the name", {
  d <- data.frame(object.id = c("A", "B", "C", "D", "E"),
                  org.name = c("SMITH COLLEGE", "UNION RESCUE MISSION", "FIRE CO", "SMITH COLLEGE", "PLAIN NAME"),
                  title.raw = "PRESIDENT",
                  org.type = c("trust", NA, NA, NA, NA),
                  ntee_code_clean = c(NA, "X20", "M24", NA, NA),
                  subsection_code = c(NA, 3, 3, 3, NA), stringsAsFactors = FALSE)
  o <- org_types(d)
  o <- o[match(d$object.id, o$object.id), ]
  expect_equal(o$org.type, c("trust", "ministry", "fire", "university", NA))
  expect_equal(o$org.type.source, c("supplied", "codes", "codes", "inferred", "none"))
  # with codes, the name does not add a membership type: a 501(c)(3) named
  # "... Union" is not a union
  u <- org_types(data.frame(object.id = "U", org.name = "STUDENTS UNION OF X", title.raw = "PRESIDENT",
                            subsection_code = 3, stringsAsFactors = FALSE))
  expect_true(is.na(u$org.type))
})

test_that("the CEO thesaurus is valid package data", {
  th <- get_ceo_thesaurus()
  expect_true(all(th$org_type %in% org_type_levels()))
  expect_true(all(th$effect %in% c("ceo", "not_ceo")))
  for (p in c(th$title_pattern, th$outranks[nzchar(th$outranks)])) expect_silent(grepl(p, "", perl = TRUE))
})

# hand-built filings, as in test-roles.R
tperson <- function(object.id, person.id, title.raw, title.standard = title.raw, emp.level = "",
                    board.role = "", comp = 0, hours = 0, trustee = 0, officer = 0, key = 0,
                    org.type = NA_character_, org.name = "")
  data.frame(object.id = object.id, person.id = person.id, title.raw = title.raw, title.v7 = title.raw,
             title.standard = title.standard, emp.level = emp.level, board.role = board.role,
             tot.comp = comp, tot.hours = hours, formtype = "990",
             dtk.indiv.trustee.x = trustee, dtk.inst.trustee.x = 0, dtk.officer.x = officer,
             dtk.key.empl.x = key, dtk.high.comp.x = 0, interim.x = 0,
             org.type = org.type, org.name = org.name, stringsAsFactors = FALSE)
pr_th <- function(d) { r <- resolve_roles(d, use_model = FALSE); r[r$role.primary, ] }
ceo_of <- function(p) p$person.id[p$role.final == "CEO" & !is.na(p$role.ceo)]

test_that("a club's general manager outranks the better-paid golf superintendent", {
  g <- rbind(tperson("G", "g1", "GOLF COURSE SUPERINTENDENT", "SUPERINTENDENT", "MANAGER", comp = 140000, hours = 50, officer = 1, org.type = "club"),
             tperson("G", "g2", "GENERAL MANAGER", "GENERAL MANAGER", "OFFICER", comp = 120000, hours = 50, officer = 1, org.type = "club"),
             tperson("G", "g3", "PRESIDENT", "BOARD PRESIDENT", board.role = "CHAIR", hours = 2, trustee = 1, officer = 1, org.type = "club"))
  p <- pr_th(g)
  expect_equal(ceo_of(p), "g2")
  expect_equal(p$role.ceo[p$person.id == "g2"], "thesaurus")
  expect_equal(unique(p$org.type), "club")
  # the same filing with no org type: the thesaurus does not apply
  g$org.type <- NA
  expect_false("thesaurus" %in% pr_th(g)$role.ceo)
})

test_that("a lodge secretary on a stipend is not made the CEO; a full-time one can be", {
  l <- rbind(tperson("L", "l1", "SECRETARY", "BOARD SECRETARY", board.role = "SECRETARY", comp = 18000, hours = 20, officer = 1, org.type = "fraternal"),
             tperson("L", "l2", "EXALTED RULER", "BOARD PRESIDENT", board.role = "CHAIR", hours = 5, trustee = 1, officer = 1, org.type = "fraternal"))
  expect_length(ceo_of(pr_th(l)), 0)
  l$tot.hours[1] <- 40; l$tot.comp[1] <- 32000
  expect_equal(ceo_of(pr_th(l)), "l1")
})

test_that("a post adjutant runs the department over a better-paid treasurer", {
  v <- rbind(tperson("V", "v1", "DEPARTMENT ADJUTANT", "ADJUTANT", "OFFICER", comp = 45000, hours = 40, officer = 1, org.type = "veterans"),
             tperson("V", "v2", "TREASURER", "BOARD TREASURER", board.role = "TREASURER", comp = 60000, hours = 40, officer = 1, org.type = "veterans"))
  expect_equal(ceo_of(pr_th(v)), "v1")
})

test_that("rule 7 imputes no one when several unpaid leaders tie for the most hours", {
  w <- rbind(tperson("W", "w1", "PRESIDENT", "BOARD PRESIDENT", board.role = "CHAIR", hours = 50, trustee = 1, officer = 1),
             tperson("W", "w2", "VICE PRESIDENT", "BOARD VICE PRESIDENT", board.role = "VICE CHAIR", hours = 50, trustee = 1, officer = 1))
  expect_length(ceo_of(pr_th(w)), 0)
})
