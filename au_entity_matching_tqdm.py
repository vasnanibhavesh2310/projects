
import re
import math
import itertools
from dataclasses import dataclass
from typing import Dict, List, Tuple, Optional
from datetime import datetime

import numpy as np
import pandas as pd
from difflib import SequenceMatcher

# Attempt to import tqdm; fall back gracefully if unavailable
try:
    from tqdm.auto import tqdm as _tqdm
    _TQDM_AVAILABLE = True
except Exception:  # pragma: no cover
    _TQDM_AVAILABLE = False

    class _DummyTQDM:
        def __init__(self, iterable=None, **kwargs):
            self.iterable = iterable
        def __iter__(self):
            return iter(self.iterable) if self.iterable is not None else iter([])
        def update(self, n=1): 
            pass
        def close(self): 
            pass
        def set_postfix(self, *a, **k): 
            pass
        def set_description(self, *a, **k): 
            pass
        def __enter__(self):
            return self
        def __exit__(self, exc_type, exc, tb):
            pass

    def _tqdm(iterable=None, **kwargs):  # pragma: no cover
        return _DummyTQDM(iterable)

def _pbar(iterable, progress: bool, **kwargs):
    """
    Progress-enabled iterator. If progress is False, returns the iterable as-is.
    If progress is True, wraps with tqdm (or a no-op fallback if tqdm unavailable).
    """
    if not progress:
        return iterable
    return _tqdm(iterable, **kwargs)

# =====================================================
# AU Entity Matching (Dependency-lite, Enterprise-grade)
# =====================================================
# Draws heavily from the provided reference implementation (structure, ideas,
# AU-specific normalisers, scoring, blocking) but removes third-party
# dependencies like rapidfuzz/jellyfish/dateutil by supplying light-weight
# pure-Python alternatives for string similarity, phonetics, and date parsing.
#
# Standardised field names expected post-schema-mapping:
#   first_name, last_name, dob, email, phone, address, suburb, state, postcode
#
# Primary entry point:
#   match_entities(df_left, df_right, schema_left, schema_right,
#                  auto_threshold=1.20, review_threshold=0.80, review_path=None)
#
# Returns:
#   (auto_match_index_pairs, review_df)
# where auto_match_index_pairs is a list of (left_index, right_index) tuples
# referring to the ORIGINAL indices of df_left and df_right, and review_df
# is a DataFrame of pairs needing human review (with feature contributions).
# If review_path is provided, review_df is also written to CSV.
#
# Notes:
# - Pandas/Numpy are assumed available, as inputs are DataFrames.
# - Phone/state/address/email normalisation is AU-aware.
# - Blocking keys are robust but conservative to bound candidate pairs.
# - Scores are explainable via feature contributions.
# - Clustering step is available (not required for index-pair output).
# =====================================================

# -------------------------
# AU-Normalisers & Helpers
# -------------------------

STATE_MAP = {
    "nsw": "NSW", "new south wales": "NSW",
    "vic": "VIC", "victoria": "VIC",
    "qld": "QLD", "queensland": "QLD",
    "sa": "SA", "south australia": "SA",
    "wa": "WA", "western australia": "WA",
    "tas": "TAS", "tasmania": "TAS",
    "nt": "NT", "northern territory": "NT",
    "act": "ACT", "australian capital territory": "ACT",
}

STREET_TYPES = {
    "st": "street", "street": "street",
    "rd": "road", "road": "road",
    "ave": "avenue", "av": "avenue", "avenue": "avenue",
    "blvd": "boulevard", "boulevard": "boulevard",
    "ln": "lane", "lane": "lane",
    "dr": "drive", "drive": "drive",
    "ct": "court", "court": "court",
    "pl": "place", "place": "place",
    "hwy": "highway", "highway": "highway",
    "cir": "circuit", "cct": "circuit", "circuit": "circuit",
    "cres": "crescent", "crescent": "crescent",
    "mt": "mount", "mount": "mount"
}

NICKNAMES = {
    "liz":"elizabeth","beth":"elizabeth","lizzy":"elizabeth","liza":"elizabeth",
    "bill":"william","will":"william","billy":"william","liam":"william",
    "rob":"robert","bob":"robert","bobby":"robert","robbie":"robert",
    "jen":"jennifer","jenny":"jennifer",
    "alex":"alexander","sasha":"alexander",
    "chris":"christopher",
    "matt":"matthew","maddy":"madeline",
    "sam":"samantha",
    "jack":"john",
    "gaz":"gary","gazza":"gary",
}
COMMON_SURNAMES = set(\"\"\"
smith jones williams brown taylor wilson thompson white martin anderson lee clark
walker harris lewis robinson young king wright scott green baker adams hall
\"\"\".split())

EMAIL_DOMAIN_EQUIV = {
    "googlemail.com":"gmail.com",
    "me.com":"icloud.com"
}

def norm_text(x: str) -> str:
    return re.sub(r"\\s+", " ", (x or "")).strip().lower()

def normalise_first_name(x: str) -> str:
    x = norm_text(x)
    return NICKNAMES.get(x, x)

def normalise_last_name(x: str) -> str:
    return norm_text(x)

# ---- Pure-Python phonetic key (Soundex) ----
# Metaphone is stronger, but Soundex is dependency-free and sufficient for blocking.
def soundex(s: str) -> str:
    s = norm_text(s)
    if not s:
        return ""
    first_letter = s[0].upper()
    mapping = {**{k: '1' for k in "bfpv"},
               **{k: '2' for k in "cgjkqsxz"},
               **{k: '3' for k in "dt"},
               **{k: '4' for k in "l"},
               **{k: '5' for k in "mn"},
               **{k: '6' for k in "r"}}
    digits = []
    prev = ''
    for ch in s[1:]:
        if ch in "aeiouyhw":
            code = ''
        else:
            code = mapping.get(ch, '')
        if code and code != prev:
            digits.append(code)
        prev = code
    code = first_letter + ("".join(digits) + "000")[:3]
    return code

def normalise_email(email: str) -> str:
    email = (email or "").strip().lower()
    if not email or "@" not in email:
        return ""
    local, domain = email.split("@", 1)
    domain = EMAIL_DOMAIN_EQUIV.get(domain, domain)
    # Gmail-style normalisation
    local = local.split("+", 1)[0]
    if domain in ("gmail.com", "googlemail.com"):
        local = local.replace(".", "")
    return f"{local}@{domain}"

def email_parts(email: str) -> Tuple[str, str]:
    if "@" not in email:
        return email, ""
    lp, dom = email.split("@", 1)
    return lp, dom

def normalise_phone_au(phone: str) -> str:
    """Canonicalise to E.164 (+61...). Handles mobiles 04.. and landlines with area codes."""
    digits = re.sub(r"\\D+", "", phone or "")
    if not digits:
        return ""
    digits = re.sub(r"^0*61", "61", digits)
    if digits.startswith("61"):
        rest = digits[2:]
        if rest.startswith("4") and len(rest) == 9:  # mobile
            return "+61" + rest
        if len(rest) in (8, 9):
            return "+61" + rest
        return "+61" + rest
    if digits.startswith("04") and len(digits) == 10:
        return "+61" + digits[1:]
    if digits.startswith(("02","03","07","08")) and len(digits) == 10:
        return "+61" + digits[1:]
    if 8 <= len(digits) <= 10:
        if digits[0] == "0":
            return "+61" + digits[1:]
        return "+61" + digits
    return "+" + digits

# ---- Date parsing without dateutil ----
_KNOWN_DATE_FORMATS = [
    "%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%Y/%m/%d",
    "%d %b %Y", "%d %B %Y",
    "%d/%m/%y", "%d-%m-%y",
]

def _parse_date_try(s: str) -> Optional[pd.Timestamp]:
    for fmt in _KNOWN_DATE_FORMATS:
        try:
            dt = datetime.strptime(s, fmt)
            # normalise two-digit years to 1900/2000 range (assume 1950-2049 window)
            if "%y" in fmt and "%Y" not in fmt:
                year = dt.year
                if year < 1950:
                    year += 2000
                elif year < 100:  # unlikely due to window, but safe-guard
                    year += 1900
                dt = dt.replace(year=year)
            return pd.Timestamp(dt.date())
        except Exception:
            continue
    return None

def parse_dob(dob) -> pd.Timestamp:
    if dob is None or (isinstance(dob, float) and math.isnan(dob)) or dob == "":
        return pd.NaT
    s = str(dob).strip()
    # 1) Prefer AU DD/MM/YYYY styles first
    out = _parse_date_try(s)
    if out is not None:
        return out
    # 2) Fallback: try swapping day/month if looks ambiguous (MM/DD/YYYY -> DD/MM/YYYY)
    m = re.match(r"^(\\d{1,2})[\\-/](\\d{1,2})[\\-/](\\d{2,4})$", s)
    if m:
        d1, d2, y = m.groups()
        swapped = f"{d2}/{d1}/{y}"
        out = _parse_date_try(swapped)
        if out is not None:
            return out
    return pd.NaT

def normalise_state(x: str) -> str:
    return STATE_MAP.get(norm_text(x), norm_text(x).upper())

def normalise_postcode(x) -> str:
    s = re.sub(r"\\D+", "", str(x or ""))
    return s[:4] if s else ""

def normalise_address(addr: str) -> str:
    a = norm_text(addr)
    if not a:
        return ""
    tokens = []
    for t in re.split(r"[,\s/]+", a):
        tokens.append(STREET_TYPES.get(t, t))
    a = " ".join(tokens)
    a = re.sub(r"\\b(unit|apt|apartment|flat|level|lvl|suite|lot)\\b", "", a)
    return re.sub(r"\\s+", " ", a).strip()

def postcode_from_address(addr: str, fallback: str) -> str:
    m = re.search(r"\\b(\\d{4})\\b", addr or "")
    return m.group(1) if m else normalise_postcode(fallback)

# -------------------------
# Pure-Python Similarities
# -------------------------

def jaro_winkler(a: str, b: str, p: float = 0.1) -> float:
    a, b = norm_text(a), norm_text(b)
    if a == b:
        return 1.0 if a else 1.0  # both empty -> 1.0
    if not a or not b:
        return 0.0

    # Jaro
    la, lb = len(a), len(b)
    match_dist = max(0, max(la, lb) // 2 - 1)
    a_matches = [False] * la
    b_matches = [False] * lb
    matches = 0

    for i in range(la):
        start = max(0, i - match_dist)
        end = min(i + match_dist + 1, lb)
        for j in range(start, end):
            if b_matches[j]:
                continue
            if a[i] == b[j]:
                a_matches[i] = True
                b_matches[j] = True
                matches += 1
                break

    if matches == 0:
        return 0.0

    # Transpositions
    a_m = [a[i] for i in range(la) if a_matches[i]]
    b_m = [b[j] for j in range(lb) if b_matches[j]]
    transpositions = sum(ch1 != ch2 for ch1, ch2 in zip(a_m, b_m)) / 2

    jaro = (matches / la + matches / lb + (matches - transpositions) / matches) / 3.0

    # Winkler prefix scale
    prefix = 0
    for ch1, ch2 in zip(a, b):
        if ch1 == ch2:
            prefix += 1
        else:
            break
        if prefix == 4:
            break
    return jaro + prefix * p * (1 - jaro)

def token_jaccard(a: str, b: str) -> float:
    a, b = norm_text(a), norm_text(b)
    if not a and not b:
        return 1.0
    if not a or not b:
        return 0.0
    sa = set(re.split(r"\\s+", a))
    sb = set(re.split(r"\\s+", b))
    if not sa and not sb:
        return 1.0
    inter = len(sa & sb)
    uni = len(sa | sb)
    return inter / uni if uni else 0.0

def soft_token_ratio(a: str, b: str) -> float:
    """A light-weight alternative to RapidFuzz's token_set_ratio.
    Combines token Jaccard with SequenceMatcher to give a smoother score.
    """
    a, b = norm_text(a), norm_text(b)
    if not a and not b:
        return 1.0
    if not a or not b:
        return 0.0
    j = token_jaccard(a, b)
    s = SequenceMatcher(None, " ".join(sorted(set(a.split()))),
                             " ".join(sorted(set(b.split())))).ratio()
    # Blend; weight Jaccard higher for robustness to token order/noise
    return 0.7 * j + 0.3 * s

# -------------------------
# Feature Engineering
# -------------------------

def jw_sim(a: str, b: str) -> float:
    return jaro_winkler(a, b)

def token_similarity(a: str, b: str) -> float:
    return soft_token_ratio(a, b)  # 0..1

def surname_common_penalty(last_name: str) -> float:
    return -0.15 if (last_name or "").lower() in COMMON_SURNAMES else 0.0

def birth_year(ts):
    if pd.isna(ts):
        return ""
    return int(pd.to_datetime(ts).year)

# -------------------------
# Blocking Keys
# -------------------------

def blocking_keys(row: pd.Series) -> List[str]:
    keys = []
    # 1) surname_soundex + postcode
    keys.append(f"k1:{soundex(row['last_name'])}|{row['postcode']}")
    # 2) email_domain + first_initial + birth_year
    lp, dom = email_parts(row['email'])
    first_initial = (row['first_name'][:1] if row['first_name'] else "")
    keys.append(f"k2:{dom}|{first_initial}|{birth_year(row['dob'])}")
    # 3) phone last 7
    phone = (row['phone'] or "").replace("+", "")
    keys.append(f"k3:{phone[-7:]}")
    # 4) state + first3 surname
    keys.append(f"k4:{row['state']}|{(row['last_name'] or '')[:3].lower()}")
    # 5) postcode only (broad)
    keys.append(f"k5:{row['postcode']}")
    return list(set(keys))

# -------------------------
# Pairwise Feature Set & Scoring
# -------------------------


def pair_features(a: pd.Series, b: pd.Series) -> Dict[str, float]:
    # Identifier-only features (as requested)
    same_email = 1.0 if a['email'] and a['email'] == b['email'] else 0.0
    same_phone = 1.0 if a['phone'] and a['phone'] == b['phone'] else 0.0
    dob_exact = 1.0 if (not pd.isna(a['dob']) and a['dob'] == b['dob']) else 0.0
    dob_year_match = 1.0 if (not pd.isna(a['dob']) and not pd.isna(b['dob']) and a['dob'].year == b['dob'].year) else 0.0

    fn_sim = jw_sim(a['first_name'], b['first_name'])
    ln_sim = jw_sim(a['last_name'], b['last_name'])

    same_postcode = 1.0 if a['postcode'] and a['postcode'] == b['postcode'] else 0.0

    feats = {
        "same_email": same_email,
        "same_phone": same_phone,
        "dob_exact": dob_exact,
        "dob_year_match": dob_year_match,
        "fn_sim": fn_sim,
        "ln_sim": ln_sim,
        "same_postcode": same_postcode,
    }
    return feats

WEIGHTS = {
    "same_email": 2.50,
    "same_phone": 2.00,
    "dob_exact": 0.80,
    "dob_year_match": 0.30,
    "fn_sim": 0.30,
    "ln_sim": 0.40,
    "same_postcode": 0.20,
}

AUTO_THR = 3.8
REVIEW_THR = 3.5

def weighted_score(feats: Dict[str, float]) -> Tuple[float, Dict[str, float]]:
    contribs = {}
    score = 0.0
    for k, w in WEIGHTS.items():
        v = feats.get(k, 0.0)
        c = w * v
        contribs[k] = round(c, 4)
        score += c
    return round(score, 4), contribs

# -------------------------
# Union-Find for Clustering
# -------------------------

class UnionFind:
    def __init__(self):
        self.parent = {}
        self.rank = {}
    def find(self, x):
        if x not in self.parent:
            self.parent[x] = x
            self.rank[x] = 0
            return x
        if self.parent[x] != x:
            self.parent[x] = self.find(self.parent[x])
        return self.parent[x]
    def union(self, a, b):
        ra, rb = self.find(a), self.find(b)
        if ra == rb:
            return
        if self.rank[ra] < self.rank[rb]:
            self.parent[ra] = rb
        elif self.rank[ra] > self.rank[rb]:
            self.parent[rb] = ra
        else:
            self.parent[rb] = ra
            self.rank[ra] += 1

# -------------------------
# Schema handling
# -------------------------

STANDARD_FIELDS = ["first_name","last_name","dob","email","phone","address","suburb","state","postcode"]

def apply_schema(df: pd.DataFrame, schema: Dict[str, str]) -> pd.DataFrame:
    """Map user-provided schema to standard field names.
    schema: dict of {standard_field: actual_column_name}
    Missing fields are created with empty strings (or NaT for dob).
    """
    df = df.copy()
    # Preserve original index for reporting
    df["__orig_index"] = df.index
    # Rename if present
    rename_map = {schema.get(f): f for f in STANDARD_FIELDS if schema.get(f) in df.columns}
    df = df.rename(columns=rename_map)
    # Ensure all required columns exist
    for col in STANDARD_FIELDS:
        if col not in df.columns:
            df[col] = ""
    return df

# -------------------------
# Main Pipeline
# -------------------------

def normalise_table(df: pd.DataFrame, table_name: str) -> pd.DataFrame:
    df = df.copy()
    df["__table"] = table_name

    df["first_name"] = df["first_name"].apply(normalise_first_name)
    df["last_name"]  = df["last_name"].apply(normalise_last_name)
    df["email"]      = df["email"].apply(normalise_email)
    df["phone"]      = df["phone"].apply(normalise_phone_au)
    df["dob"]        = df["dob"].apply(parse_dob)

    df["state"] = df["state"].apply(normalise_state)
    df["postcode"] = df.apply(lambda r: normalise_postcode(r.get("postcode", "")), axis=1)

    base_addr = df["address"].apply(normalise_address)
    addr_extended = base_addr + " " + df["suburb"].apply(norm_text) + " " + df["state"].fillna("")
    df["address"] = addr_extended.str.strip()
    df["postcode"] = df.apply(lambda r: postcode_from_address(r["address"], r["postcode"]), axis=1)

    if "__id" not in df.columns:
        # embed original index in id to recover left/right indices later
        df["__id"] = [f"{table_name}:{idx}" for idx in df["__orig_index"]]
    return df

def make_blocks(df: pd.DataFrame, progress: bool = False) -> Dict[str, List[int]]:
    blocks = {}
    for idx, row in _pbar(df.iterrows(), progress, total=len(df), desc="Indexing blocks"):
        for k in blocking_keys(row):
            blocks.setdefault(k, []).append(idx)
    return blocks

def candidate_pairs(df: pd.DataFrame, blocks: Dict[str, List[int]], progress: bool = False) -> List[Tuple[int, int]]:
    seen = set()
    pairs = set()
    for k, idxs in _pbar(blocks.items(), progress, total=len(blocks), desc="Generating candidates"):
        if len(idxs) < 2:
            continue
        by_tbl = {}
        for i in idxs:
            by_tbl.setdefault(df.at[i, "__table"], []).append(i)
        tbls = list(by_tbl.keys())
        if len(tbls) < 2:
            continue
        for t1, t2 in itertools.combinations(tbls, 2):
            for i in by_tbl[t1]:
                for j in by_tbl[t2]:
                    a, b = (i, j) if i < j else (j, i)
                    key = (a, b)
                    if key in seen:
                        continue
                    seen.add(key)
                    pairs.add(key)
    return list(pairs)

@dataclass
class MatchDecision:
    left_id: str
    right_id: str
    score: float
    decision: str  # 'auto' | 'review' | 'reject'
    features: Dict[str, float]
    contributions: Dict[str, float]

def score_pairs(df: pd.DataFrame, pairs: List[Tuple[int,int]], auto_thr: float, review_thr: float, progress: bool = False) -> List[MatchDecision]:
    decisions = []
    for i, j in _pbar(pairs, progress, total=len(pairs), desc="Scoring pairs"):
        a, b = df.loc[i], df.loc[j]
        feats = pair_features(a, b)
        score, contribs = weighted_score(feats)
        decision = "reject"
        if score >= auto_thr:
            decision = "auto"
        elif score >= review_thr:
            decision = "review"
        decisions.append(MatchDecision(
            left_id=a["__id"], right_id=b["__id"], score=score,
            decision=decision, features=feats, contributions=contribs
        ))
    return decisions

def cluster_entities(df: pd.DataFrame, decisions: List[MatchDecision], progress: bool = False) -> pd.DataFrame:
    uf = UnionFind()
    for d in _pbar(decisions, progress, total=len(decisions), desc="Clustering autos"):
        if d.decision == "auto":
            uf.union(d.left_id, d.right_id)

    entity_id = {}
    for rid in _pbar(df["__id"], progress, total=len(df), desc="Assigning entity IDs"):
        entity_id[rid] = uf.find(rid)

    roots = sorted(set(entity_id.values()))
    root_to_seq = {r: f"E{n+1}" for n, r in enumerate(roots)}

    df_out = df.copy()
    df_out["entity_id"] = df_out["__id"].map(lambda x: root_to_seq[entity_id[x]])
    return df_out, root_to_seq

def explain_table(decisions: List[MatchDecision], progress: bool = False) -> pd.DataFrame:
    rows = []
    for d in _pbar(decisions, progress, total=len(decisions), desc="Building explain table"):
        row = {
            "left_id": d.left_id, "right_id": d.right_id,
            "score": d.score, "decision": d.decision
        }
        for k, v in d.contributions.items():
            row[f"c_{k}"] = v
        rows.append(row)
    return pd.DataFrame(rows).sort_values(["decision","score"], ascending=[True, False])

# -------------------------
# Public API
# -------------------------

def link_entities(tables: Dict[str, pd.DataFrame], auto_thr: float = AUTO_THR, review_thr: float = REVIEW_THR, show_progress: bool = False):
    # Stage progress (coarse-grained)
    stage = _tqdm(total=4, desc="Linking pipeline", disable=not show_progress)
    try:
        frames = []
        for name, df in _pbar(tables.items(), show_progress, total=len(tables), desc="Normalising tables"):
            frames.append(normalise_table(df, name))
        stage.update(1)

        df_all = pd.concat(frames, ignore_index=True)

        blocks = make_blocks(df_all, progress=show_progress)
        stage.update(1)

        pairs = candidate_pairs(df_all, blocks, progress=show_progress)
        stage.update(1)

        decisions = score_pairs(df_all, pairs, auto_thr, review_thr, progress=show_progress)
        stage.update(1)
    finally:
        stage.close()

    clustered, entity_map = cluster_entities(df_all, decisions, progress=show_progress)

    review = [d for d in decisions if d.decision == "review"]
    reject = [d for d in decisions if d.decision == "reject"]

    return {
        "records": clustered,
        "decisions_df": explain_table(decisions, progress=show_progress),
        "review_pairs": pd.DataFrame([d.__dict__ for d in review]),
        "reject_pairs": pd.DataFrame([d.__dict__ for d in reject]),
        "entity_map": entity_map
    }

def _split_id(s: str) -> Tuple[str, int]:
    # format table_name:orig_index
    t, i = s.split(":", 1)
    return t, int(i)

def match_entities(
    df_left: pd.DataFrame,
    df_right: pd.DataFrame,
    schema_left: Dict[str, str],
    schema_right: Dict[str, str],
    auto_threshold: float = AUTO_THR,
    review_threshold: float = REVIEW_THR,
    review_path: Optional[str] = None,
    matches_path: Optional[str] = None,
    show_progress: bool = False,
) -> Tuple[List[Tuple[int, int]], pd.DataFrame]:
    """Top-level helper tailored for two dataframes and explicit schemas.

    Parameters
    ----------
    df_left, df_right : DataFrame
        Input tables.
    schema_left, schema_right : dict
        Mapping from standard field names to actual column names in each DF.
        Example: {"first_name": "FirstName", "last_name": "Surname", ...}
    auto_threshold, review_threshold : float
        Score thresholds for auto-match and manual review.
    review_path : str, optional
        If provided, write review pairs with contributions to this CSV path.
    matches_path : str, optional
        If provided, write auto-matched index pairs (left_index,right_index,score) to this CSV path.
    show_progress : bool, optional
        If True, display tqdm progress bars across the pipeline (requires tqdm).

    Returns
    -------
    auto_pairs : List[(left_index, right_index)]
        Index tuples referencing original df_left/df_right indices.
    review_df : DataFrame
        Pairs requiring review with scores & contributions.
    """
    left_std = apply_schema(df_left, schema_left)
    right_std = apply_schema(df_right, schema_right)

    out = link_entities({"left": left_std, "right": right_std},
                        auto_thr=auto_threshold, review_thr=review_threshold, show_progress=show_progress)

    # Extract auto matches only
    decisions_df = out["decisions_df"]
    auto_df = decisions_df[decisions_df["decision"] == "auto"]
    auto_pairs: List[Tuple[int, int]] = []
    auto_rows = []
    for _, row in auto_df.iterrows():
        tL, idxL = _split_id(row["left_id"])   # left:<orig_index>
        tR, idxR = _split_id(row["right_id"])  # right:<orig_index>
        if tL == "left" and tR == "right" :
            auto_pairs.append((idxL, idxR))
            auto_rows.append({"left_index": idxL, "right_index": idxR, "score": row.get("score", None)})
        elif tL == "right" and tR == "left":
            auto_pairs.append((idxR, idxL))
            auto_rows.append({"left_index": idxR, "right_index": idxL, "score": row.get("score", None)})
        else:
            # Safety: ignore if not cross-table (shouldn't happen due to blocking)
            continue

    if matches_path:
        pd.DataFrame(auto_rows).to_csv(matches_path, index=False)

    review_df = out["decisions_df"][out["decisions_df"]["decision"] == "review"].copy()
    if review_path:
        review_df.to_csv(review_path, index=False)

    return auto_pairs, review_df

# -------------------------
# Example (synthetic)
# -------------------------
if __name__ == "__main__":
    t1 = pd.DataFrame([
        {"FirstName":"Liz","Surname":"Smith","DOB":"14/03/1991","Email":"liz.smith+promo@gmail.com","Phone":"0412 345 678","Address":"Unit 3/15 King St","Suburb":"Newtown","State":"NSW","Postcode":"2042"},
        {"FirstName":"Robert","Surname":"Brown","DOB":"1990-01-05","Email":"rob.brown@outlook.com","Phone":"(02) 9123 4567","Address":"10 Mount Rd","Suburb":"Chatswood","State":"NSW","Postcode":"2067"},
        {"FirstName":"Samantha","Surname":"Nguyen","DOB":"05/05/1988","Email":"sam.nguyen@uni.sydney.edu.au","Phone":"0499 111 222","Address":"8 Crescent Ave","Suburb":"Carlton","State":"VIC","Postcode":"3053"},
    ]).set_index(pd.Index([101,102,103]))  # custom original indices

    t2 = pd.DataFrame([
        {"first_name":"Elizabeth","last_name":"Smith","dob":"1991-03-14","email":"lizsmith@gmail.com","phone":"+61 412 345 678","address":"3/15 King Street","suburb":"Newtown","state":"New South Wales","postcode":""},
        {"first_name":"Bob","last_name":"Brown","dob":"05/01/1990","email":"bobby.brown@outlook.com","phone":"+61291234567","address":"10 Mt Road","suburb":"Chatswood","state":"NSW","postcode":"2067"},
        {"first_name":"Sam","last_name":"Nguyen","dob":"1988/05/05","email":"sam.nguyen@USYD.edu.au","phone":"+61-499-111-222","address":"8 CRES AVE","suburb":"Carlton","state":"VIC","postcode":""},
    ]).set_index(pd.Index([555,556,557]))

    schema_left = {
        "first_name": "FirstName",
        "last_name": "Surname",
        "dob": "DOB",
        "email": "Email",
        "phone": "Phone",
        "address": "Address",
        "suburb": "Suburb",
        "state": "State",
        "postcode": "Postcode",
    }
    schema_right = {
        "first_name": "first_name",
        "last_name": "last_name",
        "dob": "dob",
        "email": "email",
        "phone": "phone",
        "address": "address",
        "suburb": "suburb",
        "state": "state",
        "postcode": "postcode",
    }

    pairs, review = match_entities(t1, t2, schema_left, schema_right, review_path=None, show_progress=True)
    print("Auto-matched index pairs:", pairs)
    print("\\nReview head:\\n", review.head())
