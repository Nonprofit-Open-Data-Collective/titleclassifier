# Step 09: resolve_roles() on small, hand-built filings with known answers.

person <- function(object.id, person.id, title.standard, emp.level = "", board.role = "",
                   comp = 0, hours = 0, trustee = 0, officer = 0, key = 0,
                   formtype = "990", title.raw = title.standard, interim = 0)
  data.frame(object.id = object.id, person.id = person.id, title.raw = title.raw,
             title.standard = title.standard, emp.level = emp.level, board.role = board.role,
             tot.comp = comp, tot.hours = hours, formtype = formtype,
             dtk.indiv.trustee.x = trustee, dtk.inst.trustee.x = 0, dtk.officer.x = officer,
             dtk.key.empl.x = key, dtk.high.comp.x = 0, interim.x = interim,
             stringsAsFactors = FALSE)

d <- rbind(
  # A: a paid executive director, and a volunteer board
  person("A", "a1", "EXECUTIVE DIRECTOR", "CEO", comp = 90000, hours = 40, officer = 1),
  person("A", "a2", "BOARD PRESIDENT", board.role = "CHAIR", hours = 2, trustee = 1, officer = 1),
  person("A", "a3", "VICE PRESIDENT", "OFFICER", hours = 1, trustee = 1, title.raw = "VICE PRESIDENT"),
  person("A", "a4", "BOARD MEMBER", board.role = "MEMBER", hours = 1, trustee = 1),
  # B: no CEO title; a paid full-time president with the officer box runs it
  person("B", "b1", "BOARD PRESIDENT", board.role = "CHAIR", comp = 120000, hours = 40, officer = 1,
         title.raw = "PRESIDENT"),
  person("B", "b2", "BOARD TREASURER", board.role = "TREASURER", hours = 1, trustee = 1),
  # C: one person, split into two titles: counted once
  person("C", "c1", "CEO", "CEO", comp = 150000, hours = 50, officer = 1, title.raw = "PRESIDENT/CEO"),
  person("C", "c1", "BOARD PRESIDENT", board.role = "CHAIR", comp = 150000, hours = 50, officer = 1,
         title.raw = "PRESIDENT/CEO"),
  person("C", "c2", "DOCTOR", "PROFESSIONAL", comp = 300000, hours = 40, key = 1),
  # D: only token pay to a board member: no CEO
  person("D", "d1", "BOARD SECRETARY", board.role = "SECRETARY", comp = 600, hours = 2, trustee = 1, officer = 1),
  person("D", "d2", "BOARD MEMBER", board.role = "MEMBER", hours = 1, trustee = 1),
  # E: a 990-EZ with an unpaid executive director and an unpaid vice president
  person("E", "e1", "EXECUTIVE DIRECTOR", "CEO", hours = 20, formtype = "990EZ"),
  person("E", "e2", "VICE PRESIDENT", "OFFICER", hours = 2, formtype = "990EZ")
)
r  <- resolve_roles(d)
pr <- r[r$role.primary, ]
role <- function(id) pr$role.final[pr$person.id == id]
f <- function(obj) unique(r$org.leadership[r$object.id == obj])

test_that("resolve_roles adds its columns and keeps every row in order", {
  expect_equal(nrow(r), nrow(d))
  expect_identical(r$title.standard, d$title.standard)
  for (v in c("role.position", "role.final", "role.board", "role.ceo",
              "role.source", "role.primary", "org.leadership"))
    expect_true(v %in% names(r), info = v)
})

test_that("a CEO title with an officer position is the designated CEO", {
  expect_equal(role("a1"), "CEO")
  expect_equal(pr$role.ceo[pr$person.id == "a1"], "designated")
  expect_equal(f("A"), "designated")
})

test_that("the volunteer board keeps its board roles, and VICE PRESIDENT is read as vice chair", {
  expect_equal(role("a2"), "BOARD"); expect_equal(pr$role.board[pr$person.id == "a2"], "CHAIR")
  expect_equal(role("a3"), "BOARD"); expect_equal(pr$role.board[pr$person.id == "a3"], "VICE CHAIR")
  expect_equal(role("a4"), "BOARD"); expect_equal(pr$role.board[pr$person.id == "a4"], "MEMBER")
})

test_that("a paid full-time president with no CEO title is the imputed CEO", {
  expect_equal(role("b1"), "CEO")
  expect_equal(pr$role.ceo[pr$person.id == "b1"], "imputed")
  expect_equal(f("B"), "imputed")
  expect_equal(role("b2"), "BOARD")
})

test_that("a person split into two titles is one person with one role", {
  expect_equal(sum(r$person.id == "c1"), 2)
  expect_equal(sum(r$role.primary[r$person.id == "c1"]), 1)
  expect_equal(unique(r$role.final[r$person.id == "c1"]), "CEO")
  expect_equal(role("c2"), "PROFESSIONAL")
})

test_that("token pay does not make a CEO: the filing is board-governed", {
  expect_false(any(pr$role.final[pr$object.id == "D"] == "CEO"))
  expect_equal(f("D"), "board_governed")
})

test_that("on a 990-EZ, an unpaid executive director is the CEO and an unpaid VP is board", {
  expect_equal(role("e1"), "CEO")
  expect_equal(role("e2"), "BOARD")
  expect_equal(f("E"), "designated")
})
