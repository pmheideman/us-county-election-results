#!/usr/bin/env python3
"""
Python preprocessing step (rest of this pipeline is R; Python used here because its regex/pandas
tooling is a better fit for wrangling noisy OCR'd text than base R would be).

Extracts U.S. House CONGRESSIONAL-DISTRICT-level vote totals (NOT county-level -- America Votes
does not report House results by county, only by district, confirmed by direct inspection) from
three "America Votes" reference-book PDFs (Internet Archive OCR scans):

  - America Votes 24 (covers election year 2000): each state's "CONGRESS" table gives one row per
    (district, year) for 1992, 1994, 1996, 1998, 2000 -- the full decade under 1992-drawn lines.
  - America Votes 29 (covers 2010): "HOUSE OF REPRESENTATIVES" table, one row per (district, year)
    for 2002, 2004, 2006, 2008, 2010 -- the full decade under 2002-drawn lines.
  - America Votes 31 (covers 2014): "HOUSE OF REPRESENTATIVES" table -- CONFIRMED UNUSABLE, not
    just lower-confidence. Manually checked against the raw OCR text (e.g. Alabama): the actual
    scanned table only survives intact for CD1's two rows: everything after that is missing from
    the extracted text entirely (an OCR/layout failure on this volume's table pages specifically,
    not a parsing bug). parse_v31_table()'s row-order-inference approach papered over this by
    pulling in unrelated text (footnotes, primary-election narrative) as if it were table rows,
    producing rows with swapped party labels, duplicated vote totals, and merged candidate names
    that pass the total-vote sanity check by coincidence. Its output is EXCLUDED from the final
    RDS (filtered by source_volume != 31) and saved separately, unused, in
    R/output/house_district_america_votes_2012_2014_UNRELIABLE.rds -- do not merge it back in
    without a fundamentally different extraction approach (e.g. re-OCR at higher resolution, or a
    different scan of this volume). 2012 and 2014 U.S. House data remains an open gap.

Because of this retrospective-decade format, volumes 21 (1994) and 30 (2012) -- not in the
downloaded set -- are NOT needed for most of 1992-2014: 24+29+31 already cover it. Exception: any
state where mid-decade court-ordered redistricting made the earlier years within a decade
incomparable -- America Votes itself flags these with a footnote ("Results for those years may be
found in America Votes NN"), logged below rather than guessed at.

At-large states (single House seat, code "AL" not a number) need no district-to-county crosswalk
at all, since the "district" is the whole state.

Output: R/output/house_district_america_votes.rds (columns: state, cd, year, total_vote, rep_vote,
rep_candidate, dem_vote, dem_candidate, other_vote, plurality_vote, plurality_party, source_volume,
confidence). District-to-county allocation is explicitly OUT OF SCOPE here -- separate follow-up.
"""
import re
import subprocess
from pathlib import Path

import pandas as pd

BASE = Path("/home/paul/Stats/substack_projects/immigration")
AV_DIR = BASE / "R" / "data" / "america_votes"
TXT_DIR = AV_DIR / "text_layout"
TXT_DIR.mkdir(parents=True, exist_ok=True)
OUT_RDS = BASE / "R" / "output" / "house_district_america_votes.rds"
LOG_PATH = BASE / "R" / "data" / "america_votes" / "extraction_footnotes_log.txt"

STATES = [
    "ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT", "DELAWARE",
    "FLORIDA", "GEORGIA", "HAWAII", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY",
    "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI",
    "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO",
    "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA",
    "RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT",
    "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING",
]
STATE_SET = set(STATES)

VOLUMES = [
    {"num": 24, "pdf": AV_DIR / "america_votes24.pdf", "txt": TXT_DIR / "av24_layout.txt", "years": [1992, 1994, 1996, 1998, 2000]},
    {"num": 29, "pdf": AV_DIR / "america_votes29.pdf", "txt": TXT_DIR / "av29_layout.txt", "years": [2002, 2004, 2006, 2008, 2010]},
    {"num": 31, "pdf": AV_DIR / "america_votes31.pdf", "txt": TXT_DIR / "av31_layout.txt", "years": [2012, 2014]},
]

NUM_RE = r"[\d][\d,]{1,}"


def ensure_text(vol):
    if not vol["txt"].exists():
        subprocess.run(["pdftotext", "-layout", str(vol["pdf"]), str(vol["txt"])], check=True)
    return vol["txt"].read_text(errors="replace").splitlines()


def clean_num(tok):
    """Strip obvious OCR junk from a vote-count token; return int or None if unrecoverable."""
    t = tok.strip()
    t = re.sub(r"[^\d,]", "", t)
    t = t.replace(",", "")
    if not t or not t.isdigit():
        return None
    return int(t)


def find_state_line_indices(lines):
    """For each state, the set of line indices where the stripped line equals exactly that state name."""
    idx = {s: [] for s in STATES}
    for i, line in enumerate(lines):
        s = line.strip()
        if s in STATE_SET:
            idx[s].append(i)
    return idx


def nearest_state_before(state_idx, pos):
    best_state, best_i = None, -1
    for s, idxs in state_idx.items():
        for i in idxs:
            if i <= pos and i > best_i:
                best_i, best_state = i, s
    return best_state


HEADER_RE = re.compile(r"^[^A-Za-z]{0,6}(CONGRESS|HOUSE OF REPRESENTATIVES)\s*$")


def find_table_headers(lines):
    """Return list of (line_index) for confirmed CONGRESS / HOUSE OF REPRESENTATIVES vote tables.
    Tolerant of a stray leading OCR character (e.g. "i   CONGRESS") before the heading word."""
    hits = []
    for i, line in enumerate(lines):
        s = line.strip()
        if HEADER_RE.match(s):
            window = " ".join(lines[i : i + 8])
            if "Republican" in window and "Democratic" in window and ("Year" in window or "Total Vote" in window):
                hits.append(i)
    return hits


def find_table_end(lines, start):
    for j in range(start + 1, min(start + 400, len(lines))):
        if "GENERAL AND PRIMARY ELECTIONS" in lines[j]:
            return j
        s = lines[j].strip()
        if s in STATE_SET and s != lines[nearest_prior_state_line(lines, start)] if False else False:
            pass
    # fallback: stop at next standalone state name after start+5 (skip the header's own state echoes)
    for j in range(start + 5, min(start + 400, len(lines))):
        s = lines[j].strip()
        if s in STATE_SET:
            return j
    return min(start + 400, len(lines))


def nearest_prior_state_line(lines, pos):
    for j in range(pos, -1, -1):
        if lines[j].strip() in STATE_SET:
            return lines[j].strip()
    return None


def parse_v24_v29_table(lines, start, end, state, vol_num, expected_years):
    """Parse the clean-format tables (v24, v29): explicit CD (sometimes missing -> inferred) and
    explicit year on most rows."""
    rows = []
    current_cd = 0
    saw_any_cd_token = False
    for j in range(start, end):
        raw = lines[j]
        line = raw.strip()
        if not line or line.startswith("Note") or line.startswith("**"):
            continue
        year_m = re.search(r"\b(19[5-9]\d|20[0-2]\d)\b", line[:20])
        if not year_m:
            continue
        year = int(year_m.group(1))
        # CD token: whatever precedes the year match, on the same line, trimmed
        prefix = line[: year_m.start()].strip()
        cd = None
        if prefix:
            if prefix.upper() in ("AL", "A L", "AT-LARGE", "AT LARGE"):
                cd = "AL"
            else:
                digits = re.sub(r"[^\d]", "", prefix)
                if digits and int(digits) <= 60:
                    cd = int(digits)
        if cd is not None:
            current_cd = cd
            saw_any_cd_token = True
        elif current_cd == 0:
            current_cd = 1  # first block in a state's table with no explicit CD token yet -> CD 1
        cd_out = current_cd

        rest = line[year_m.end() :].strip()
        plurality_m = re.search(r"([\d,]{4,})\s*([RD])\b", rest)
        plurality_vote = clean_num(plurality_m.group(1)) if plurality_m else None
        plurality_party = plurality_m.group(2) if plurality_m else None
        # Drop the plurality clause from `rest` before tokenizing so it isn't double-counted as
        # another vote number.
        body = rest[: plurality_m.start()] if plurality_m else rest

        nums_clean = [clean_num(n) for n in re.findall(NUM_RE, body)]
        name_chunks = re.split(NUM_RE, body)
        names = [re.sub(r"[^A-Za-z.,\"'\* ]", "", c).strip(" ,") for c in name_chunks]
        names = [n for n in names if len(n) >= 2 and re.search(r"[A-Za-z]{2,}", n)]

        total_vote = nums_clean[0] if len(nums_clean) > 0 else None
        remaining_nums = nums_clean[1:]  # everything after total: [main_vote, (other_vote?)] when
        # only one candidate ran; [rep_vote, dem_vote, (other_vote?)] when both did.

        rep_vote = dem_vote = other_vote = None
        rep_candidate = dem_candidate = None
        if len(names) >= 2:
            # Two candidates -> convention confirmed from inspection: first name/vote listed is
            # Republican, second is Democratic, regardless of who won.
            rep_candidate, dem_candidate = names[0], names[1]
            rep_vote = remaining_nums[0] if len(remaining_nums) > 0 else None
            dem_vote = remaining_nums[1] if len(remaining_nums) > 1 else None
            other_vote = remaining_nums[2] if len(remaining_nums) > 2 else None
        elif len(names) == 1:
            # Unopposed race: only one party fielded a candidate. Which column (Rep or Dem) that
            # single vote number belongs to can't be read off position alone (a Dem-unopposed row
            # leaves the Rep column blank, shifting everything left) -- use plurality_party, which
            # always names the party of whoever ran, to disambiguate.
            main_vote = remaining_nums[0] if len(remaining_nums) > 0 else None
            other_vote = remaining_nums[1] if len(remaining_nums) > 1 else None
            if plurality_party == "R":
                rep_candidate, rep_vote = names[0], main_vote
            elif plurality_party == "D":
                dem_candidate, dem_vote = names[0], main_vote
            # else: can't disambiguate without a plurality-party anchor; leave both NA (rare).

        rows.append(
            dict(
                state=state, cd=cd_out, year=year, total_vote=total_vote, rep_vote=rep_vote,
                rep_candidate=rep_candidate, dem_vote=dem_vote, dem_candidate=dem_candidate,
                other_vote=other_vote, plurality_vote=plurality_vote, plurality_party=plurality_party,
                source_volume=vol_num, confidence="high" if saw_any_cd_token or cd_out == 1 else "medium",
            )
        )
    return rows


def parse_v31_table(lines, start, end, state, vol_num):
    """Volume 31's table lost per-row CD and year tokens to OCR. Recover via: rows come in
    alternating pairs (year_a, year_b) per district in ascending CD order; the two years are read
    off the trailing 'TOTAL <year>' aggregate lines. Lower confidence than v24/v29."""
    total_year_lines = [
        int(m.group(1)) for j in range(start, end) if (m := re.search(r"^\s*TOTAL\s+(\d{4})", lines[j]))
    ]
    if len(total_year_lines) != 2:
        return []  # can't safely recover year assignment without exactly 2 aggregate anchors
    year_a, year_b = total_year_lines[0], total_year_lines[1]

    data_rows = []
    for j in range(start, end):
        line = lines[j].strip()
        if not line or line.startswith("TOTAL") or line.startswith("Note") or "%" not in line:
            continue
        nums = re.findall(NUM_RE, line)
        nums_clean = [clean_num(n) for n in nums]
        if len(nums_clean) < 2 or nums_clean[0] is None:
            continue
        name_chunks = re.split(NUM_RE, line)
        names = [re.sub(r"[^A-Za-z.,\"'\* ]", "", c).strip(" ,") for c in name_chunks]
        names = [n for n in names if len(n) >= 2 and re.search(r"[A-Za-z]{2,}", n)]
        plurality_m = re.search(r"([\d,]{4,})\s*([RD])\b", line)
        data_rows.append(
            dict(
                total_vote=nums_clean[0], rep_vote=nums_clean[1] if len(nums_clean) > 1 else None,
                dem_vote=nums_clean[2] if len(nums_clean) > 2 else None,
                rep_candidate=names[0] if names else None, dem_candidate=names[1] if len(names) > 1 else None,
                plurality_vote=clean_num(plurality_m.group(1)) if plurality_m else None,
                plurality_party=plurality_m.group(2) if plurality_m else None,
            )
        )

    rows = []
    for k, d in enumerate(data_rows):
        cd = k // 2 + 1
        year = year_a if k % 2 == 0 else year_b
        rows.append(dict(state=state, cd=cd, year=year, other_vote=None, source_volume=vol_num, confidence="low", **d))
    return rows


def main():
    all_rows = []
    footnotes = []
    for vol in VOLUMES:
        lines = ensure_text(vol)
        state_idx = find_state_line_indices(lines)
        headers = find_table_headers(lines)
        seen_states_this_vol = set()
        for h in headers:
            state = nearest_state_before(state_idx, h)
            if state is None or state in seen_states_this_vol:
                continue
            end = find_table_end(lines, h)
            body_start = h + 4  # skip header/subheader lines
            if vol["num"] == 31:
                rows = parse_v31_table(lines, body_start, end, state, vol["num"])
            else:
                rows = parse_v24_v29_table(lines, body_start, end, state, vol["num"], vol["years"])
            # A header attributed to `state` that yields zero usable rows is very likely a
            # mis-attribution (nearest_state_before latched onto a stray mention of `state`'s name
            # inside a PRECEDING state's chapter, rather than that state's own real chapter) --
            # confirmed happening for New Mexico in v24. Don't mark the state "seen" on an empty
            # parse; keep scanning for its real table later in the document.
            if not rows:
                continue
            seen_states_this_vol.add(state)
            all_rows.extend(rows)

            for j in range(h, end):
                if "may be found in America Votes" in lines[j] or "may be found in" in lines[j]:
                    footnotes.append(f"[vol {vol['num']}] {state}: {lines[j].strip()} {lines[j+1].strip() if j+1<len(lines) else ''}")

        missing = [s for s in STATES if s not in seen_states_this_vol]
        if missing:
            footnotes.append(f"[vol {vol['num']}] no CONGRESS/HOUSE table found for: {', '.join(missing)}")

    df = pd.DataFrame(all_rows)
    df = df.drop_duplicates(subset=["state", "cd", "year", "source_volume"])

    # Volume 31 (2012/2014) is confirmed unusable -- see module docstring. Route it to a
    # separate, clearly-labeled file instead of the main output so it can never be silently
    # merged back into the real dataset by a future run of this script.
    unreliable = df[df["source_volume"] == 31].copy()
    df = df[df["source_volume"] != 31].copy()

    # Validation: total_vote vs rep+dem+other when both present
    def check(r):
        if pd.isna(r.get("rep_vote")) or pd.isna(r.get("dem_vote")) or pd.isna(r.get("total_vote")):
            return None
        parts = (r.get("rep_vote") or 0) + (r.get("dem_vote") or 0) + (r.get("other_vote") or 0)
        if r["total_vote"] == 0:
            return None
        return abs(parts - r["total_vote"]) / r["total_vote"]

    df["validation_pct_diff"] = df.apply(check, axis=1)

    BASE.joinpath("R", "output").mkdir(parents=True, exist_ok=True)
    csv_path = OUT_RDS.with_suffix(".csv")
    df.to_csv(csv_path, index=False)
    unreliable_csv = OUT_RDS.parent / "house_district_america_votes_2012_2014_UNRELIABLE.csv"
    unreliable.to_csv(unreliable_csv, index=False)
    # Hand off to R for the actual .rds (pandas has no native RDS writer).
    subprocess.run(
        ["Rscript", "-e",
         f'saveRDS(read.csv("{csv_path}", stringsAsFactors=FALSE), "{OUT_RDS}"); '
         f'saveRDS(read.csv("{unreliable_csv}", stringsAsFactors=FALSE), '
         f'"{unreliable_csv.with_suffix(".rds")}")'],
        check=True,
    )

    LOG_PATH.write_text("\n".join(footnotes))

    print(f"TOTAL ROWS: {len(df)}")
    print(df.groupby("year").size().to_string())
    print("\nBy source_volume x confidence:")
    print(df.groupby(["source_volume", "confidence"]).size().to_string())
    bad = df[df["validation_pct_diff"] > 0.01]
    print(f"\nVALIDATION FAILURES (>1% mismatch): {len(bad)}")
    print(bad[["state", "cd", "year", "source_volume", "total_vote", "rep_vote", "dem_vote", "other_vote", "validation_pct_diff"]].head(60).to_string())
    print(f"\nFootnotes logged: {len(footnotes)} -> {LOG_PATH}")

    states_present = set(df["state"].unique())
    missing_states = [s for s in STATES if s not in states_present]
    print(f"\nStates with ZERO extracted rows across all volumes: {missing_states}")


if __name__ == "__main__":
    main()
