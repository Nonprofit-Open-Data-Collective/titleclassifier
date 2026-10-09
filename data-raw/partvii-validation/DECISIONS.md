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
