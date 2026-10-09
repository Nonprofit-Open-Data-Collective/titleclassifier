# Part VII title review: policy decisions and round 1 drafts

These are the policy decisions that apply to many titles at once. They are
decided before the one-by-one review so that each file does not have to settle
them again. Findings are in [FINDINGS.md](FINDINGS.md) and the process is in
[REVIEW-PROCESS.md](REVIEW-PROCESS.md).

## Policies (decided by J. Lecy, 2026-10-08)

| Id | Question | Decision | How it is applied |
|---|---|---|---|
| P1 | Chief-officer titles whose standard is `CHIEF _X_ OFFICER` (no taxonomy row; F-002) | One generic **CHIEF OFFICER** standard (operations / administration / emp c.level) | Tier 1 drafts map to CHIEF OFFICER. The other crosswalk variants still on `CHIEF _X_ OFFICER` / `CHEIF _X_ OFFICER` need the same change in bulk |
| P2 | Are officers of lodges, posts and clubs the board? | **Yes**, by role. Volunteer fire/EMS line officers are not | Presiding officers (commander, exalted ruler, grand knight, potentate, worshipful master) → BOARD PRESIDENT. Vice officers (vice/junior commanders, knights, wardens, vice/rear commodore, rabbans) → BOARD VICE PRESIDENT. Adjutant, registrar, recorder → BOARD SECRETARY. Quartermaster, finance officer → BOARD TREASURER. Ritual officers (tiler, guards, chaplain, conductor, marshal, historian, sergeant at arms…) → BOARD MEMBER. Union stewards → UNION REPRESENTATIVE. Fire/EMS: CHIEF → FIRE CHIEF; assistant/deputy chiefs, captains, lieutenants → new **FIRE OFFICER** (industry-specific / public safety / emp) |
| P3 | `CO-` prefix (F-009) | **Strip CO- in cleaning and keep the flag** | Code: `gen_status_codes()` strips `CO-` after setting `CO.X`; `split_titles()` keeps CO- with FOUNDER, so CO-FOUNDER no longer leaves a stray "CO" title. Regression baseline rebuilt (154 cells; unmatched rate in the demo 7.66% → 7.61%) |
| P4 | Empty, NA, NONE, "See Schedule O" and other boilerplate (F-007) | Map to an explicit **NO TITLE** standard (non-job title / missing) | Tier 1 drafts map these to NO TITLE. Fragments left by cleaning (THRU, UNTIL, PART YEAR, S, P…) stay `fix_cleaning`: the cleaning steps should drop them |
| F-016 | Long tail of board standards | **Accepted**: collapse to the five core board standards (BOARD MEMBER / PRESIDENT / VICE PRESIDENT / SECRETARY / TREASURER) and keep the role (`board.role`); committee, advisory and ex officio become flags | Tier 1 drafts no longer target BOARD CHAIR, VICE CHAIR, COMMITTEE CHAIR, ADVISORY BOARD, BOARD SECRETARY TREASURER and the like. The role is in each draft's notes as `F-016: board.role = …`. The bulk change for the other variants belongs to F-016's Phase 2 |

F-017 (context-dependent titles such as PRESIDENT, VICE PRESIDENT, DIRECTOR,
CHAIR and SECRETARY) is still open. The tier 1 drafts give these titles the
best crosswalk default from the holder profile (for example VICE PRESIDENT →
BOARD VICE PRESIDENT: 87% officer box, 12% paid), and note that step 09 should
override the default from the checkboxes, pay and hours.

## Round 1: tier 1 drafts (2026-10-08)

Claude drafted a decision for each of the 923 tier 1 titles (1,000+ person-title
rows, 92.9% of all rows), from the evidence in each review file. The drafts
are written in the files' `review:` blocks with `reviewer: "Claude (draft)"`
and collected in `review-decisions.csv`.

| Draft status | Titles |
|---|---|
| confirmed | 533 |
| remap | 199 |
| add | 98 |
| add_new_standard | 39 |
| fix_cleaning | 33 |
| ambiguous | 21 |

**A draft is not applied until a person confirms it** by replacing
`Claude (draft)` with their name, editing the decision first if needed.
`06-apply-decisions.R` skips every row whose reviewer contains "(draft)".

The 21 ambiguous titles have mixed holders that no single standard fits:
OWNER, AGENT, union building reps, athletic and foundation directors, and
others. They are candidates for step 09 rules.

New standards proposed by the drafts, each with a taxonomy row in
`taxonomy-additions.csv`:
- NO TITLE
- CHIEF OFFICER
- FIRE OFFICER
- PHARMACIST
- VETERINARIAN
- LINEMAN
- GOLF PROFESSIONAL
- GOLF COURSE SUPERINTENDENT
- ASSISTANT PRINCIPAL
- HEAD OF DIVISION
- SENIOR FELLOW
- SCIENTIST
- TRAINING COORDINATOR
- DISPATCHER
- FOREMAN
- BARTENDER

## Round 1: consensus vote and apply (2026-10-08)

So that crosswalk revisions are not held up until a person can review each
title, the tier 1 drafts were settled by a three-vote consensus
(`08-consensus-vote.R`) and applied. The owner asked for this approach.

- **Voters:**
  - Claude's draft
  - two independent Claude reviewers (A and B). Each saw the same evidence for
    every title, the list of valid standards, and the policies above, and voted
    agree or disagree. When they disagreed, they gave their own decision.
- **Rule:**
  - The draft stands if at least one reviewer agrees.
  - If both disagree with the same decision, the reviewers' decision wins.
  - Anything else is left `ambiguous` and not applied. This case did not occur.
- **Result:**

  | Outcome | Titles |
  |---|---|
  | Draft stands, both reviewers agree | 833 |
  | Draft stands, one reviewer agrees | 31 |
  | Reviewers' shared alternative replaces the draft | 59 |

- **Overrides:** most of the 59 send functional titles whose holders look like
  a board (officer or trustee box high, unpaid, a few hours a week) from staff
  standards to BOARD MEMBER (37) or BOARD VICE PRESIDENT (14). Examples are
  VP of membership, fundraising, publicity, events, liaison, and unpaid legal
  counsel on the board.
- **Records:**
  - Every vote and reason is in `consensus-votes-round1.csv`.
  - The vote files are kept in `~/Documents/PARTVII/review/votes/round1/`.
  - Each review file's `review:` block now has
    `reviewer: "Consensus vote (3 Claude reviewers)"` and a note starting
    `[... not yet reviewed by a person]`.
- **Applied with `06-apply-decisions.R`:**
  - 108 crosswalk variants added
  - 400 changed, including 129 variants moved from the `CHIEF _X_ OFFICER`
    placeholders to CHIEF OFFICER under P1
  - 16 new taxonomy rows, in the emp.level form of F-020

  The three union steward titles map to BOARD MEMBER, because UNION
  REPRESENTATIVE was collapsed under F-016.

**For later review by a person:** filter `review-decisions.csv` for reviewer
"Consensus vote (3 Claude reviewers)". Start with the 59 overrides and the 31
2-of-3 drafts (the `outcome` column in `consensus-votes-round1.csv`). To accept
a decision, put your name in `reviewer`. To change one, edit the review file
and rerun 05 and 06.

**Effect.** The classification was re-run with the consensus decisions, the
CO- fix (P3) and the same panel (61.3 million person-title rows). That run used
the snapshot from before F-020; F-020 renames roles but does not change which
titles are matched.

| | Round 0 | Round 1 |
|---|---|---|
| Standardized titles (`TitleTxt7`) | 618,164 | 611,042 |
| Rows with no `title.standard`, all | 6.35% | 5.81% |
| ... tier 1 | 0.61% | 0.07% |
| ... tier 2 | 56.9% | 57.1% |
| Tier 1 titles to ADD / INSPECT | 129 / 102 | 19 / 61 |

Tiers 2-4 are unchanged because round 1 covered tier 1 only. They are next,
through clusters and cleaning fixes, as the review process describes.

## Round 2: cleaning fixes (2026-10-08)

Before the tier 2 review, the cleaning steps were fixed for the leftovers that
round 1 marked `fix_cleaning` (F-007):

- **Fragments after a split.** `drop_fragment_titles()` handles them at the end
  of step 6:
  - A split piece that is only a status qualifier (THRU, THROUGH, UNTIL, PAST,
    FORMER, RESIGNED, TERM ENDED, PART YEAR, PARTIAL YEAR, NEW, ELECT...) or a
    single letter (S, P, M, V) sets the person's FORMER, PARTIAL or FUTURE flag.
  - The piece is dropped when the person has another title. When it is the
    only title, the title becomes empty, which maps to NO TITLE.
- **Dangling words and punctuation.** A leading AND, a trailing OF and trailing
  punctuation are removed. For example, PAST - PRESIDENT now gives PRESIDENT,
  VP-AT-LARGE gives VICE PRESIDENT, and DIRECTOR: THROUGH ... gives DIRECTOR.
- **Truncations:**
  - IMMEDIATE PA becomes IMMEDIATE PAST PRESIDENT. It was read as Pennsylvania.
  - PAST P becomes PAST PRESIDENT.
  - NON-VOTING M becomes NON-VOTING MEMBER.
- **ER PHYSICIAN** (and nurse, doctor, MD, director) becomes EMERGENCY ...,
  not EDITOR PHYSICIAN.

The equivalence check still holds. The regression reference was rebuilt with
`build-demo.R --reference-only`, which also pins the round 1 crosswalk. Two
demo rows are gone (fragments dropped): 4,137 → 4,135.
