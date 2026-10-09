# Role labeling guide, v2

This guide is for labeling people listed on IRS Form 990 / 990-EZ Part VII. It
uses the role standard the classifier uses since October 2026. It replaces the
August guide (`synthid/dev/GOLD-LABELING-GUIDE.md`).

For each target person, give their **true role in the organization**, judged from
the whole filing. Don't judge from the title alone.

## Columns to fill

| Column | Values | When |
|---|---|---|
| `role` | `CEO`, `OFFICER`, `MANAGER`, `PROFESSIONAL`, `STAFF`, `BOARD`, `DUAL`, `UNSURE`, `NONE` | always |
| `board_role` | `CHAIR`, `VICE CHAIR`, `SECRETARY`, `TREASURER`, `MEMBER` | when `role` is `BOARD` or `DUAL`; blank otherwise |
| `ceo_note` | `co`, `interim`, `outgoing` | optional, for a CEO who is one of two, acting, or leaving |
| `confidence` | `high`, `medium`, `low` | always |
| `notes` | one short sentence: the decisive evidence | always |

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
- `MEMBER`: every other trustee, director, council member or delegate.

For a combined post such as secretary-treasurer, give the first one named.

## Evidence

- **Boxes.** Full 990 returns have four:
  - T = individual trustee or director
  - O = officer
  - K = key employee
  - H = highly compensated employee

  990-EZ returns have no boxes, and every box shows `-`.
- **Pay** is total reported compensation in dollars.
- **h/wk** is average hours per week.

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
     is `MANAGER` or `STAFF`, not `CEO`.
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
