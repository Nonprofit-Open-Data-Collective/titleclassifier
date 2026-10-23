# CEO thesaurus by organization type: plan (2026-10-23)

Status: **built** (FINDINGS.md F-033): `data-raw/crosswalks/ceo-thesaurus.csv`,
`R/09-org-type.R` and the hook in `resolve_roles()`. The org type comes from
both routes of section 2 (user decision, 2026-10-23). The plan below is kept
as written. The evidence is in FINDINGS.md F-031 (the 150-org label set) and
F-032 (the step 09 rule fixes measured on it).

## Why

Step 09 names the chief executive from a CEO-level title, or failing that from
pay. In some organization types the executive carries a title the crosswalk
reads as something else. Examples:
- a club's GENERAL MANAGER (MANAGER level);
- a veterans department's ADJUTANT (a board secretary);
- a benefit trust's ADMINISTRATOR (MANAGER);
- a university's PRESIDENT (board chair).

When that title loses on pay to another paid person, step 09 picks the wrong
person or no one. Examples of the other person: a golf course superintendent,
a Legion treasurer, a chancellor.

After the F-032 rule fixes, 112 of 750 labeled filings still have the wrong
chief executive. About 30 of them (7 organizations) are this kind of error:

| Type | Labeled executive | Step 09 picks instead | Filings |
|---|---|---|---|
| University | PRESIDENT (paid) | CHANCELLOR; VP FOR ACADEMIC AFFAIRS | 8 |
| Club (c7) | GENERAL MANAGER | GOLF COURSE SUPERINTENDENT | 5 |
| Veterans department (c19) | DEPARTMENT ADJUTANT | TREASURER | 5 |
| Benefit trust (c9) | (FUND) ADMINISTRATOR | no one | 4 |
| Cemetery (c13) | TREASURER / SECRETARY who runs it | unpaid SUPERINTENDENT | 4 |
| Ministry | GENERAL PASTOR / PASTOR-PRESIDENT | no one | 3 |
| Union (c5) | BUSINESS MANAGER | former business manager | 1 |

A thesaurus should therefore move the CEO score from about 85% to about 89%
on this set. The rest of the remaining errors are rule questions (see the
end).

## Design

### 1. A table, not code

Add `data-raw/crosswalks/ceo-thesaurus.csv`, built into package data like the
other crosswalks. One row per (org type, title pattern):

| column | example | meaning |
|---|---|---|
| `org_type` | `c7 club` | the type the entry applies to (see 2) |
| `title_pattern` | `^(GENERAL\|CLUB) MANAGER\b` | regex on `title.v7` |
| `effect` | `ceo` / `not_ceo` | names the CEO, or rules a title out |
| `min_hours` | 20 | conditions on the holder |
| `min_pay` | 25000 | |
| `outranks` | `SUPERINTENDENT\|GOLF PRO\|CHEF` | titles it beats when both are paid |
| `evidence` | `F-031: O013 2019-23` | where the entry came from |

Negative entries matter as much as positive ones. In the label set the lodge
secretary or recorder, the fire company's secretary-treasurer and the Elks
officers on a stipend are **not** the CEO, but pay inference makes them one.

### 2. Where the org type comes from (needs a decision)

Part VII alone does not say what kind of organization filed. Options:

1. **An `org_type` column the user supplies.** For example, joined from the
   NCCS BMF by EIN: `panel990::bmf_retrieve()` gives the NTEE code and 501(c)
   subsection. This is precise, but it only works for users who join it.
2. **Inferred from the filing itself.** The org name (POST, LODGE, AERIE,
   COUNTRY CLUB, UNIVERSITY, CEMETERY, LOCAL ###) and the titles present
   (ADJUTANT, QUARTERMASTER, EXALTED RULER, WORSHIPFUL MASTER, BUSINESS
   AGENT) mark most of the types above. This always works, but it is fuzzier.
3. **Both (recommended).** Use `org_type` when given, otherwise infer it, and
   record the source in a new column (`org.type.source`).

The F-031 sample already carries NTEE and subsection for all 150
organizations, so both routes can be scored against each other.

### 3. Where it acts in step 09

Between the designated-CEO and pay-inference steps of `resolve_roles()`:
- a `ceo` entry makes its holder a designated CEO for the filing, if the
  conditions hold and no CEO-level title is present;
- an `outranks` entry wins over a higher-paid title it lists;
- a `not_ceo` entry removes its holder from pay inference.

`role.ceo` gets a new value, `thesaurus`, so its effect is visible and can be
counted.

## Entries to start with (from F-031 and the 2019-2023 cross-tab)

| Org type (subsection / NTEE) | CEO titles | Not CEO / outranked |
|---|---|---|
| c19 veterans posts and departments | ADJUTANT, DEPARTMENT ADJUTANT; paid full-time QUARTERMASTER | TREASURER, FINANCE OFFICER on a stipend; BAR or CANTEEN MANAGER |
| c8 / c10 lodges | (none: usually board-run) | SECRETARY, RECORDER, GRAND SECRETARY on a stipend |
| c7 clubs, c12 co-ops | GENERAL MANAGER, CLUB MANAGER, GM/COO | SUPERINTENDENT, GOLF PROFESSIONAL, CHEF, CONTROLLER |
| c9 benefit trusts, c17 | ADMINISTRATOR, FUND / PLAN ADMINISTRATOR | LEGAL / LDF ADMINISTRATOR |
| c13 cemeteries | SUPERINTENDENT if paid; else a paid full-time TREASURER/SECRETARY | unpaid SUPERINTENDENT |
| c5 unions | BUSINESS MANAGER; paid full-time PRESIDENT; SECRETARY-TREASURER | BUSINESS AGENT on part-time pay |
| c6 trade associations | EXECUTIVE VICE PRESIDENT; EXECUTIVE DIRECTOR | volunteer PRESIDENT |
| B41-B43 universities | PRESIDENT (paid) | CHANCELLOR when a paid president is listed; VPs, provost |
| X ministries | PASTOR, SENIOR / GENERAL PASTOR, BISHOP, OVERSEER | associate or second pastors |
| M24 fire departments | FIRE CHIEF, CHIEF (paid, 20+ h/wk) | volunteer chiefs on a stipend |
| A68-A69 orchestras | EXECUTIVE DIRECTOR, PRESIDENT/CEO | MUSIC DIRECTOR, CONDUCTOR (unless no executive) |
| E hospitals, B29 charter networks | the system CEO / superintendent | facility CEOs, principals (a multi-CEO rule: see below) |

Each entry needs at least two labeled organizations, or a clear pattern in the
2019-2023 cross-tab, before it ships.

## How it will be measured

1. **Labeled:** `27-ceo-rerun.R` + `26-ceo-score.R` on the F-031 set. The
   target is the chief-executive accuracy by stratum. Role and board-role
   accuracy must not fall.
2. **Older sets:** `09-gold-check.R` (labeled sets v1 and v2) must not fall.
3. **Unlabeled, at scale:** on the 2019-2023 sample, by subsection and NTEE:
   - the share of filings with a CEO named from the title (rather than from
     pay);
   - the two-or-more-CEO rate;
   - same-CEO-next-year stability.
4. **Held out:** the entries are written from the F-031 set, so the set
   overstates their effect. Before shipping, label a fresh 5-10 organizations
   per targeted type (same two-agent design) and score on those alone.

## Not covered by a thesaurus (rules, after F-032)

These are about 80 of the 112 remaining wrong filings:
- **Stipend-paid officers still named CEO** (about 25 filings): treasurers,
  recorders, secretaries paid $7-37k at 5+ h/wk. The `not_ceo` entries above
  cover the lodge and veterans cases; the rest need a pay or hours threshold
  by type.
- **Unpaid or barely paid presidents the labels make CEO** (about 25): many
  have no officer box, or report 0 hours, so rule 7 does not fire. They are
  close to the board-chair boundary, and the labelers split on some.
- **Former and interim CEOs** (about 15): the labels count an outgoing CEO
  who served part of the year, but not one paid only severance. That needs
  the tax period's dates, which step 09 does not see yet.
- **Multi-CEO systems**: a hospital system lists facility CEOs. A rule that
  keeps the top-paid CEO-level title as `designated` and makes the others
  `OFFICER` should be tested on its own.
