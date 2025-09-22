
import re, sys
from datetime import datetime
from typing import Dict, List, Tuple, Optional

import pandas as pd
from difflib import SequenceMatcher

try:
    from tqdm.auto import tqdm
except Exception:
    def tqdm(iterable=None, total=None, desc=None, unit=None, position=0, leave=True, miniters=1):
        return iterable if iterable is not None else []

# -------------------- Normalisers --------------------
def _norm_email(x: str) -> str:
    x = (x or "").strip().lower()
    if not x or "@" not in x:
        return ""
    lp, dom = x.split("@", 1)
    lp = lp.split("+", 1)[0]
    if dom in ("gmail.com", "googlemail.com"):
        lp = lp.replace(".", "")
    return f"{lp}@{dom}"

def _norm_phone_au(x: str) -> str:
    s = re.sub(r"\D+", "", (x or ""))
    if not s:
        return ""
    s = s.lstrip("0")
    if s.startswith("61"):
        core = s[2:]
    else:
        core = s
    return "+61" + core if core else ""

def _norm_postcode(x: str) -> str:
    s = re.sub(r"\D+", "", (x or ""))
    if not s:
        return ""
    return s[:4] if s[0] == "2" else s

def _parse_dmy_any(x: str) -> str:
    if x is None or (isinstance(x, float) and pd.isna(x)):
        return ""
    s = str(x).strip()
    if not s:
        return ""
    fmts = ["%Y-%m-%d", "%d/%m/%Y", "%Y/%m/%d", "%d-%m-%Y", "%m/%d/%Y", "%Y.%m.%d"]
    for f in fmts:
        try:
            return datetime.strptime(s, f).strftime("%Y-%m-%d")
        except Exception:
            pass
    return ""

# -------------------- Schema apply (tolerant) --------------------
def _schema_apply(df: pd.DataFrame, schema: Dict[str, str]) -> pd.DataFrame:
    out = pd.DataFrame(index=df.index.copy())

    def col_or_blank(key: str) -> pd.Series:
        col = schema.get(key, None)
        if col is not None and col in df.columns:
            try:
                return df[col].astype(str)
            except Exception:
                return df[col].map(lambda v: "" if pd.isna(v) else str(v))
        return pd.Series([""] * len(df), index=df.index, dtype="string")

    out["first_name"] = col_or_blank("first_name")
    out["last_name"]  = col_or_blank("last_name")
    out["dob"]        = col_or_blank("dob")
    out["email"]      = col_or_blank("email")
    out["phone"]      = col_or_blank("phone")
    out["address"]    = col_or_blank("address")
    out["suburb"]     = col_or_blank("suburb")
    out["state"]      = col_or_blank("state")
    out["postcode"]   = col_or_blank("postcode")

    out["email_n"]    = out["email"].map(_norm_email)
    out["phone_n"]    = out["phone"].map(_norm_phone_au)
    out["postcode_n"] = out["postcode"].map(_norm_postcode)
    out["dob_n"]      = out["dob"].map(_parse_dmy_any)
    return out

# -------------------- Weights & thresholds (v2-inspired) --------------------
WEIGHTS = {
    "same_email": 2.50,
    "same_phone": 2.00,
    "dob_exact": 0.80,
    "dob_year_match": 0.30,
    "fn_sim": 0.30,   # multiplied by similarity 0..1
    "ln_sim": 0.40,   # multiplied by similarity 0..1
    "same_postcode": 0.20,
}

AUTO_THR = 3.8
REVIEW_THR = 3.5

def _score_pair(L: pd.Series, R: pd.Series) -> Tuple[float, Dict[str, float]]:
    contrib: Dict[str, float] = {}

    # Booleans / exact
    if L["email_n"] and L["email_n"] == R["email_n"]:
        contrib["same_email"] = WEIGHTS["same_email"]
    if L["phone_n"] and L["phone_n"] == R["phone_n"]:
        contrib["same_phone"] = WEIGHTS["same_phone"]
    if L["postcode_n"] and L["postcode_n"] == R["postcode_n"]:
        contrib["same_postcode"] = WEIGHTS["same_postcode"]

    # DOB
    if L["dob_n"] and R["dob_n"]:
        if L["dob_n"] == R["dob_n"]:
            contrib["dob_exact"] = WEIGHTS["dob_exact"]
        else:
            try:
                yL = int(str(L["dob_n"])[:4]); yR = int(str(R["dob_n"])[:4])
                if yL == yR:
                    contrib["dob_year_match"] = WEIGHTS["dob_year_match"]
            except Exception:
                pass

    # Name similarity (split)
    fnL = str(L.get("first_name", "")).lower()
    fnR = str(R.get("first_name", "")).lower()
    lnL = str(L.get("last_name", "")).lower()
    lnR = str(R.get("last_name", "")).lower()

    if fnL or fnR:
        try:
            sim = SequenceMatcher(None, fnL, fnR).ratio()
            if sim > 0:
                contrib["fn_sim"] = WEIGHTS["fn_sim"] * float(sim)
        except Exception:
            pass
    if lnL or lnR:
        try:
            sim = SequenceMatcher(None, lnL, lnR).ratio()
            if sim > 0:
                contrib["ln_sim"] = WEIGHTS["ln_sim"] * float(sim)
        except Exception:
            pass

    score = float(sum(contrib.values()))
    return score, contrib

# -------------------- Main API --------------------
def match_entities(
    df_left: pd.DataFrame,
    df_right: pd.DataFrame,
    schema_left: Dict[str, str],
    schema_right: Dict[str, str],
    auto_threshold: float = AUTO_THR,
    review_threshold: float = REVIEW_THR,
    review_path: Optional[str] = None,
    matches_path: Optional[str] = None,
):
    L = _schema_apply(df_left, schema_left)
    R = _schema_apply(df_right, schema_right)

    left_idx  = list(L.index)
    right_idx = list(R.index)
    total = len(left_idx) * len(right_idx)

    bar_all  = tqdm(total=total, desc="All pairs", unit="pair", position=0, leave=True, miniters=1)

    
    bar_left = tqdm(total=len(left_idx), desc='Left rows', unit='row', position=1, leave=False, miniters=1)auto_pairs: List[Tuple[int,int]] = []
    review_rows: List[Dict[str, object]] = []
    match_rows: List[Dict[str, object]] = []

    try:
        for i in left_idx:
            bar_left.update(1)
            for j in right_idx:
                bar_all.update(1)
                score, contrib = _score_pair(L.loc[i], R.loc[j])

                if score >= auto_threshold:
                    auto_pairs.append((i, j))
                    if matches_path:
                        match_rows.append({"left_index": i, "right_index": j, "score": score, **{f"w_{k}": v for k, v in contrib.items()}})
                elif score >= review_threshold:
                    review_rows.append({"left_index": i, "right_index": j, "score": score, **{f"w_{k}": v for k, v in contrib.items()}})
    finally:
        try: bar_all.close()
        except Exception: pass
        try: bar_left.close()
        except Exception: pass

    review_df = pd.DataFrame(review_rows)
    if review_path:
        try: review_df.to_csv(review_path, index=False)
        except Exception: pass

    if matches_path and match_rows:
        try: pd.DataFrame(match_rows).to_csv(matches_path, index=False)
        except Exception: pass

    return auto_pairs, review_df
