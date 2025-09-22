

import re
import pandas as pd
from datetime import datetime
from typing import Dict, List, Tuple, Optional
from difflib import SequenceMatcher
from joblib import Parallel, delayed
from tqdm.auto import tqdm
import time

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
    """Canonicalise to E.164 (+61...). Handles mobiles 04.. and landlines with area codes."""
    digits = re.sub(r"\D+", "", x or "")
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

# -------------------- Schema apply --------------------
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
    out["postcode"]   = col_or_blank("postcode")

    out["email_n"]    = out["email"].map(_norm_email)
    out["phone_n"]    = out["phone"].map(_norm_phone_au)
    out["postcode_n"] = out["postcode"].map(_norm_postcode)
    out["dob_n"]      = out["dob"].map(_parse_dmy_any)
    return out

# -------------------- Weights & thresholds --------------------
WEIGHTS = {
    "same_email": 2.00,
    "same_phone": 2.00,
    "dob_exact": 0.80,
    "dob_year_match": 0.30,
    "fn_sim": 0.30,
    "ln_sim": 0.40,
    "same_postcode": 0.20,
}

AUTO_THR = 3.8
REVIEW_THR = 3.5

def _build_blocks(df: pd.DataFrame) -> Dict[str, Dict[str, List[int]]]:
    email_index, phone_index, pc_index = {}, {}, {}
    for idx, row in df.iterrows():
        if row["email_n"]:
            email_index.setdefault(row["email_n"], []).append(idx)
        if row["phone_n"]:
            phone_index.setdefault(row["phone_n"], []).append(idx)
        if row["postcode_n"]:
            pc_index.setdefault(row["postcode_n"], []).append(idx)
    return {"email": email_index, "phone": phone_index, "pc": pc_index}

def _score_pair_fast(L, R):
    contrib = {}
    score = 0.0

    # High weight fields first
    if L["email_n"] and L["email_n"] == R["email_n"]:
        contrib["same_email"] = WEIGHTS["same_email"]
        score += contrib["same_email"]
    if L["phone_n"] and L["phone_n"] == R["phone_n"]:
        contrib["same_phone"] = WEIGHTS["same_phone"]
        score += contrib["same_phone"]
    if score >= AUTO_THR:
        return score, contrib, True

    if L["postcode_n"] and L["postcode_n"] == R["postcode_n"]:
        contrib["same_postcode"] = WEIGHTS["same_postcode"]
        score += contrib["same_postcode"]

    if L["dob_n"] and R["dob_n"]:
        if L["dob_n"] == R["dob_n"]:
            contrib["dob_exact"] = WEIGHTS["dob_exact"]
            score += contrib["dob_exact"]
        else:
            try:
                if str(L["dob_n"])[:4] == str(R["dob_n"])[:4]:
                    contrib["dob_year_match"] = WEIGHTS["dob_year_match"]
                    score += contrib["dob_year_match"]
            except Exception:
                pass

    if score >= (REVIEW_THR - 0.5):
        fnL, fnR = L.get("first_name", "").lower(), R.get("first_name", "").lower()
        lnL, lnR = L.get("last_name", "").lower(), R.get("last_name", "").lower()
        if fnL and fnR:
            sim_fn = SequenceMatcher(None, fnL, fnR).ratio()
            if sim_fn > 0:
                contrib["fn_sim"] = WEIGHTS["fn_sim"] * sim_fn
                score += contrib["fn_sim"]
        if lnL and lnR:
            sim_ln = SequenceMatcher(None, lnL, lnR).ratio()
            if sim_ln > 0:
                contrib["ln_sim"] = WEIGHTS["ln_sim"] * sim_ln
                score += contrib["ln_sim"]
    return score, contrib, False

def match_entities(
    df_left: pd.DataFrame,
    df_right: pd.DataFrame,
    schema_left: Dict[str, str],
    schema_right: Dict[str, str],
    auto_threshold: float = AUTO_THR,
    review_threshold: float = REVIEW_THR,
    review_path: Optional[str] = None,
    matches_path: Optional[str] = None,
    n_jobs: int = -1
):
    start_time = time.perf_counter()
    L = _schema_apply(df_left, schema_left)
    R = _schema_apply(df_right, schema_right)

    blocks = _build_blocks(R)
    left_idx = list(L.index)

    match_rows, review_rows = [], []

    bar_left = tqdm(total=len(left_idx), desc="Left rows", unit="row", position=1, leave=False)

    def process_left(i):
        local_matches, local_reviews = [], []
        candidates = set()
        rowL = L.loc[i]
        if rowL["email_n"] and rowL["email_n"] in blocks["email"]:
            candidates.update(blocks["email"][rowL["email_n"]])
        elif rowL["phone_n"] and rowL["phone_n"] in blocks["phone"]:
            candidates.update(blocks["phone"][rowL["phone_n"]])
        elif rowL["postcode_n"] and rowL["postcode_n"] in blocks["pc"]:
            candidates.update(blocks["pc"][rowL["postcode_n"]])
        else:
            candidates.update(R.index)

        for j in candidates:
            score, contrib, _ = _score_pair_fast(rowL, R.loc[j])
            if score >= auto_threshold:
                local_matches.append({"left_index": i, "right_index": j, "score": score, **{f"w_{k}": v for k, v in contrib.items()}})
            elif score >= review_threshold:
                local_reviews.append({"left_index": i, "right_index": j, "score": score, **{f"w_{k}": v for k, v in contrib.items()}})
        bar_left.update(1)
        return local_matches, local_reviews, len(candidates)

    results = Parallel(n_jobs=n_jobs, prefer="threads")(
        delayed(process_left)(i) for i in tqdm(left_idx, desc="All pairs (blocked)", position=0, unit="row")
    )

    total_candidates = 0
    for m, r, c in results:
        match_rows.extend(m)
        review_rows.extend(r)
        total_candidates += c

    bar_left.close()

    review_df = pd.DataFrame(review_rows)
    if review_path:
        review_df.to_csv(review_path, index=False)
    if matches_path and match_rows:
        pd.DataFrame(match_rows).to_csv(matches_path, index=False)

    elapsed = time.perf_counter() - start_time
    avg_candidates = total_candidates / len(left_idx) if left_idx else 0
    print(f"Execution completed in {elapsed:.2f}s | Avg candidates per left row: {avg_candidates:.2f}")

    return [(m["left_index"], m["right_index"]) for m in match_rows], review_df
