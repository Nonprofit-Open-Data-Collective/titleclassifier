# Labeling guide: CEO and board sample (2019-2023)

This guide is for the CEO / board sample (FINDINGS.md F-031). It is the v2 role
guide (`../gold-check/LABELING-GUIDE-v2.md`) with two additions: a `ceo` column
that names each filing's chief executive, and a rule for following people
across years.

Each case is one organization with its Part VII for five tax years, 2019-2023.
Label **every row**: give each person's **true role in the organization in
that year**, judged from the whole filing and from the other years. Do not
judge from the title alone.

## Output

Write one CSV per batch with exactly these columns, one line per `row_id`, in
the order of the case file:

```
row_id,role,board_role,ceo,ceo_note,confidence,notes
```

| Column | Values | When |
|---|---|---|
| `role` | `CEO`, `OFFICER`, `MANAGER`, `PROFESSIONAL`, `STAFF`, `BOARD`, `DUAL`, `UNSURE`, `NONE` | always |
| `board_role` | `CHAIR`, `VICE CHAIR`, `SECRETARY`, `SECRETARY TREASURER`, `TREASURER`, `MEMBER` | when `role` is `BOARD` or `DUAL`; blank otherwise |
| `ceo` | `Y` or blank | `Y` for the organization's chief executive in that year (see below) |
| `ceo_note` | `co`, `interim`, `outgoing` | optional, for a chief executive who is one of two, acting, or leaving |
| `confidence` | `high`, `medium`, `low` | always |
| `notes` | a short phrase: the decisive evidence | required when confidence is not `high`, when `ceo` is `Y` on a title that is not CEO / executive director, and for every `DUAL`; optional otherwise |

Quote any field that contains a comma.

### The `ceo` column

Mark `Y` on the person who runs the organization day to day in that year: the
chief executive. That is usually the person with `role` = `CEO`, but it can be
a `DUAL` (a trustee who is also the paid executive director). Rules:

- **At most one `Y` per filing**, except co-chief executives (`ceo_note` =
  `co`) and a transition year with an outgoing and an incoming chief
  executive (`outgoing` on the former).
- **No `Y` at all** when no one runs the organization as its executive: a
  board-run organization whose paid people are part-time helpers, or an
  all-volunteer board. Rule 6 below decides it.
- **The head of a part of a system is not the chief executive** of the filing
  organization when someone else heads the whole (a facility administrator
  under a system CEO, a principal under a superintendent or network head).
  Label that person `OFFICER` or `MANAGER`.
- Whatever the title: a paid secretary, business manager, general manager,
  administrator, director or chief may be the chief executive if the evidence
  says they run the organization. A title alone is never enough either way.

### Roles

| Role | Meaning |
|---|---|
| `CEO` | The organization's chief executive. Either a titled CEO, executive director or president-as-executive, or the paid head who runs operations when no one has that title. |
| `OFFICER` | A paid senior leader below the CEO: CFO, COO, other chiefs, vice presidents of a function, deputies. |
| `MANAGER` | A department head or manager with authority, below senior leadership: director of a department, program director, manager. |
| `PROFESSIONAL` | A non-managerial job of the kind reported because of its pay: physician, lawyer, professor, scientist, performer, coach, pharmacist. |
| `STAFF` | Any other employee: counselor, teacher, chef, clerk, assistant, technician. |
| `BOARD` | A governing board seat, usually unpaid. Give `board_role`. |
| `DUAL` | A board seat and a paid executive or officer job held at once, e.g. a trustee who is also the paid executive director. Give the board seat in `board_role` and name the job in `notes`. |
| `UNSURE` | The evidence doesn't decide it. |
| `NONE` | Not a person or role (blank, boilerplate, "see Schedule O"). |

### Board roles
- `CHAIR`: president, chair or chairman of the board.
- `VICE CHAIR`: vice president or vice chair of the board.
- `SECRETARY`
- `TREASURER`
- `MEMBER`: every other trustee, director, council member or delegate, and an
  assistant secretary or assistant treasurer who sits on the board.

A combined secretary-treasurer is `SECRETARY TREASURER`. For any other combined
post (e.g. vice president and treasurer), give the first one named.

## Following people across years

The same person usually appears in several years. Give them the same label each
year unless the filing shows a change (a new title, a pay change from stipend
to salary, a different box). Use the other years as evidence: a person who is
the paid executive director in 2021 and has the same pay and hours in 2020
under a vaguer title is still the executive director in 2020.

## Independence

Work from the case file and this guide only. Do not open any other labels,
keys, scripts or classifier output in the repository.

## Evidence

- **Boxes.** Full 990 returns have six:
  - T = individual trustee or director
  - I = institutional trustee
  - O = officer
  - K = key employee
  - H = highly compensated employee
  - F = former

  990-EZ returns have no boxes, and every box shows `-`.
- **Pay** is reportable compensation from this organization in dollars;
  **related pay** is from related organizations.
- **h/wk** is average hours per week; **rel h/wk** is hours for related
  organizations.
- **Employees** is the filing's Part I count of W-2 employees, where reported.

## Rules

1. **Boxes come first on a full 990.** T leans board. O leans officer or
   executive. K or H leans paid staff. T and O together often mean a working
   board member; use DUAL if they also hold a paid executive job.
2. **Pay and hours cross-check the boxes.** Unpaid with few hours points to
   board. Paid with full-time hours points to staff, officer or executive.
3. **The title breaks ties.**
4. **A paid president is the CEO when no one else in the filing has a CEO-level
   title** (CEO, executive director, president/CEO). This holds even for modest
   pay, e.g. $7,000 for 20 hours a week in a small organization.
   - An unpaid president with the trustee box is the board CHAIR.
   - A president paid a small stipend (under about $25,000) for only a few
     hours (under about 10 a week) is also the board CHAIR: a titular
     president, not an executive.
5. **"Director" can mean the head.** A music, artistic or program director who
   is the organization's only or top paid staff member, in an organization
   with no executive director, runs it: `CEO`. "Director" with no other
   context, unpaid and with the T box, is a board `MEMBER`.
6. **A board-run organization: is the top paid person the CEO?**
   - **Part-time, low pay, no other staff, and a board president:** the board
     runs the organization and has hired help for projects. The paid person
     is `MANAGER` or `STAFF`, not `CEO`, and the filing has no `ceo` = `Y`.
   - **A full-time paid role running the operation, especially with other
     staff under it:** `CEO`.
   - **A manager of one specific function** (maintenance, kitchen, grounds,
     nursing) is a department head, `MANAGER`. This holds even when they are
     the best paid, if someone else runs the whole organization.
7. **Unpaid full-time leaders.** Members of a religious order and some
   founders work full-time without pay. An unpaid president or director who
   works full-time hours (35+ a week) with the officer box is the
   organization's `CEO`, even if paid staff earn more.
8. **Several organizations in one filing.** A title naming another
   organization (e.g. "CEO/parent system") or large pay with almost no hours
   may be paid by a related organization. Judge the person's role in **this**
   organization.
9. **Transitions.** A former and a current CEO in the same year: label both
   `CEO`, with `ceo_note` set to `outgoing` for the former one.
