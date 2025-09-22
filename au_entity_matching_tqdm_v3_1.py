
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
    digits = re.sub(r"\D+", "", (x or ""))
    if not digits:
        return ""
    digits = digits.lstrip("0")
    if digits.startswith("61"):
        core = digits[2:]
    elif digits.startswith("4") and len(digits) == 9:
        core = digits
        return "+61" + core
    else:
        core = digits
    if len(core) in (8, 9):
        return "+61" + core
    return "+61" + core

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

def _name_key(fn: str, ln: str) -> str:
    import re as _re
    s = f"{(fn or '').strip().lower()} {(ln or '').strip().lower()}"
    s = _re.sub(r"[^a-z]+", " ", s).strip()
    return s

def _similar(a: str, b: str) -> float:
    return SequenceMatcher(None, a or "", b or "").ratio()

def _schema_apply(df: pd.DataFrame, schema: Dict[str, str]) -> pd.DataFrame:
    out = pd.DataFrame(index=df.index.copy())
    out["first_name"] = df[schema["first_name"]].astype(str)
    out["last_name"]  = df[schema["last_name"]].astype(str)
    out["dob"]        = df[schema["dob"]].astype(str)
    out["email"]      = df[schema["email"]].astype(str)
    out["phone"]      = df[schema["phone"]].astype(str)
    out["address"]    = df[schema["address"]].astype(str)
    out["suburb"]     = df[schema["suburb"]].astype(str)
    out["state"]      = df[schema["state"]].astype(str)
    out["postcode"]   = df[schema["postcode"]].astype(str)
    out["email_n"]    = out["email"].map(_norm_email)
    out["phone_n"]    = out["phone"].map(_norm_phone_au)
    out["dob_n"]      = out["dob"].map(_parse_dmy_any)
    out["name_key"]   = (out["first_name"] + " " + out["last_name"]).map(lambda s: re.sub(r"[^a-z]+"," ", s.lower()).strip())
    return out

def match_entities(
    df_left: pd.DataFrame,
    df_right: pd.DataFrame,
    schema_left: Dict[str, str],
    schema_right: Dict[str, str],
    auto_threshold: float = 0.92,
    review_threshold: float = 0.75,
    review_path: Optional[str] = None,
    matches_path: Optional[str] = None,
):
    L = _schema_apply(df_left, schema_left)
    R = _schema_apply(df_right, schema_right)

    left_idx  = list(L.index)
    right_idx = list(R.index)
    total = len(left_idx) * len(right_idx)

    bar_all  = tqdm(total=total, desc="All pairs", unit="pair", position=0, leave=True, miniters=1)
    bar_left = tqdm(total=len(left_idx), desc="Left rows", unit="row", position=1, leave=False, miniters=1)

    auto_pairs: List[Tuple[int,int]] = []
    review_rows = []
    matches_rows = []  # for writing all auto matches to CSV if matches_path is provided
    match_rows = []  # for CSV export via matches_path

    try:
        for _li, i in enumerate(left_idx):
            bar_left.update(1)
            for j in right_idx:
                bar_all.update(1)

                if L.at[i,"email_n"] and L.at[i,"email_n"] == R.at[j,"email_n"]:
                    auto_pairs.append((i, j))
                    match_rows.append({"left_index": i, "right_index": j, "reason": "email exact", "score": 1.0})
                    match_rows.append({"left_index": i, "right_index": j, "reason": "email exact", "score": 1.0})
                    continue
                if L.at[i,"phone_n"] and L.at[i,"phone_n"] == R.at[j,"phone_n"]:
                    auto_pairs.append((i, j))
                    match_rows.append({"left_index": i, "right_index": j, "reason": "phone exact", "score": 1.0})
                    continue

                if L.at[i,"dob_n"] and L.at[i,"dob_n"] == R.at[j,"dob_n"]:
                    sim = _similar(L.at[i,"name_key"], R.at[j,"name_key"])
                    if sim >= auto_threshold:
                        auto_pairs.append((i, j))
                        match_rows.append({"left_index": i, "right_index": j, "reason": "name+dob strong", "score": float(sim)})
                    elif sim >= review_threshold:
                        review_rows.append({"left_index": i, "right_index": j, "score": sim, "reason": "name+dob close"})

    finally:
        try: bar_all.close()
        except Exception: pass
        try: bar_left.close()
        except Exception: pass

    review_df = pd.DataFrame(review_rows)
    if review_path:
        try: review_df.to_csv(review_path, index=False)
        except Exception: pass

    # Optional CSV dump of all auto matches
    if matches_path:
        try:
            import pandas as _pd
            _pd.DataFrame(match_rows).to_csv(matches_path, index=False)
        except Exception:
            pass

    return auto_pairs, review_df