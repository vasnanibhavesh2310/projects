import re
import math
import itertools
import hashlib
from dataclasses import dataclass
from typing import Dict, List, Tuple

import numpy as np
import pandas as pd
from rapidfuzz import fuzz, process
import jellyfish
from dateutil import parser as dtparser

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
    "jack":"john", # older records
    "gaz":"gary","gazza":"gary",
}
COMMON_SURNAMES = set("""
smith jones williams brown taylor wilson thompson white martin anderson lee clark
walker harris lewis robinson young king wright scott green baker adams hall
""".split())

EMAIL_DOMAIN_EQUIV = {
    "googlemail.com":"gmail.com",
    "me.com":"icloud.com"
}

def norm_text(x: str) -> str:
    return re.sub(r"\s+", " ", (x or "")).strip().lower()

def normalise_first_name(x: str) -> str:
    x = norm_text(x)
    return NICKNAMES.get(x, x)

def normalise_last_name(x: str) -> str:
    return norm_text(x)

def phonetic(s: str) -> str:
    s = norm_text(s)
    if not s: return ""
    # Use Metaphone; could extend to double-metaphone
    return jellyfish.metaphone(s)

def normalise_email(email: str) -> str:
    email = (email or "").strip().lower()
    if not email or "@" not in email:
        return ""
    local, domain = email.split("@", 1)
    domain = EMAIL_DOMAIN_EQUIV.get(domain, domain)
    # Gmail-style normalisation
    if domain in ("gmail.com", "googlemail.com"):
        local = local.split("+", 1)[0]
        local = local.replace(".", "")
    else:
        local = local.split("+", 1)[0]  # conservative
    return f"{local}@{domain}"

def email_parts(email: str) -> Tuple[str, str]:
    if "@" not in email: return email, ""
    lp, dom = email.split("@", 1)
    return lp, dom

def normalise_phone_au(phone: str) -> str:
    """Heuristic canonicalisation to E.164 (+61...). Handles mobiles 04.. and landlines with area codes."""
    digits = re.sub(r"\D+", "", phone or "")
    if not digits:
        return ""
    # Remove leading 61 if present in local form like 0614...
    digits = re.sub(r"^0*61", "61", digits)
    # Mobile: 04XXXXXXXX or +614XXXXXXXX
    if digits.startswith("61"):
        rest = digits[2:]
        if rest.startswith("4") and len(rest) == 9:  # mobile
            return "+61" + rest
        # Landline: expect 1 leading area digit + 8 digits total
        if len(rest) == 9 or len(rest) == 8:
            return "+61" + rest
        # fallback
        return "+61" + rest
    if digits.startswith("04") and len(digits) == 10:
        return "+61" + digits[1:]
    # Landline with leading 0 (02/03/07/08)
    if digits.startswith(("02","03","07","08")) and len(digits) == 10:
        return "+61" + digits[1:]
    # Fallback: if it looks like 9–10 digits, assume local and add +61
    if 8 <= len(digits) <= 10:
        if digits[0] == "0":
            return "+61" + digits[1:]
        return "+61" + digits
    return "+" + digits  # unknown, preserve country code if present

def parse_dob(dob: str):
    if not dob or pd.isna(dob):
        return pd.NaT
    s = str(dob).strip()
    # Try AU-first (DD/MM/YYYY), then generic
    for dayfirst in (True, False):
        try:
            dt = dtparser.parse(s, dayfirst=dayfirst, yearfirst=False, fuzzy=True)
            return pd.Timestamp(dt.date())
        except Exception:
            continue
    return pd.NaT

def normalise_state(x: str) -> str:
    return STATE_MAP.get(norm_text(x), norm_text(x).upper())

def normalise_postcode(x) -> str:
    s = re.sub(r"\D+", "", str(x or ""))
    return s[:4] if s else ""

def normalise_address(addr: str) -> str:
    """Very light normalisation; for production, use G-NAF or an API."""
    a = norm_text(addr)
    if not a: return ""
    # standardise street types
    tokens = []
    for t in re.split(r"[,\s/]+", a):
        tokens.append(STREET_TYPES.get(t, t))
    a = " ".join(tokens)
    # remove "unit/apt/level" tokens but keep numbers
    a = re.sub(r"\b(unit|apt|apartment|flat|level|lvl|suite|lot)\b", "", a)
    return re.sub(r"\s+", " ", a).strip()

def postcode_from_address(addr: str, fallback: str) -> str:
    m = re.search(r"\b(\d{4})\b", addr or "")
    return m.group(1) if m else normalise_postcode(fallback)

# -------------------------
# Feature Engineering
# -------------------------

def jw_sim(a: str, b: str) -> float:
    a, b = norm_text(a), norm_text(b)
    if not a and not b: return 1.0
    if not a or not b: return 0.0
    return jellyfish.jaro_winkler_similarity(a, b)

def token_similarity(a: str, b: str) -> float:
    a, b = norm_text(a), norm_text(b)
    if not a and not b: return 1.0
    if not a or not b: return 0.0
    return fuzz.token_set_ratio(a, b) / 100.0

def surname_common_penalty(last_name: str) -> float:
    return -0.15 if last_name.lower() in COMMON_SURNAMES else 0.0

def birth_year(ts):
    if pd.isna(ts): return ""
    return int(pd.to_datetime(ts).year)

# -------------------------
# Blocking Keys
# -------------------------

def blocking_keys(row: pd.Series) -> List[str]:
    keys = []

    # 1) surname_metaphone + postcode
    keys.append(f"k1:{phonetic(row['last_name'])}|{row['postcode']}")

    # 2) email_domain + first_initial + birth_year
    lp, dom = email_parts(row['email'])
    first_initial = (row['first_name'][:1] if row['first_name'] else "")
    keys.append(f"k2:{dom}|{first_initial}|{birth_year(row['dob'])}")

    # 3) phone last 7
    phone = row['phone'].replace("+", "")
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
    # Exact-ish matches
    same_email = 1.0 if a['email'] and a['email'] == b['email'] else 0.0
    same_phone = 1.0 if a['phone'] and a['phone'] == b['phone'] else 0.0
    dob_exact = 1.0 if (not pd.isna(a['dob']) and a['dob'] == b['dob']) else 0.0
    dob_year_match = 1.0 if (not pd.isna(a['dob']) and not pd.isna(b['dob']) and a['dob'].year == b['dob'].year) else 0.0

    # Names (nicknames handled in normalisation)
    fn_sim = jw_sim(a['first_name'], b['first_name'])
    ln_sim = jw_sim(a['last_name'], b['last_name'])
    ln_phonetic = 1.0 if phonetic(a['last_name']) and phonetic(a['last_name']) == phonetic(b['last_name']) else 0.0

    # Email parts
    alp, adom = email_parts(a['email'])
    blp, bdom = email_parts(b['email'])
    email_local_sim = jw_sim(alp, blp) if adom and bdom and adom == bdom and not same_email else 0.0

    # Address
    addr_sim = token_similarity(a['address'], b['address'])
    same_postcode = 1.0 if a['postcode'] and a['postcode'] == b['postcode'] else 0.0
    same_state = 1.0 if a['state'] and a['state'] == b['state'] else 0.0

    penalty = surname_common_penalty(a['last_name']) if a['last_name'] == b['last_name'] else 0.0

    feats = {
        "same_email": same_email,
        "same_phone": same_phone,
        "dob_exact": dob_exact,
        "dob_year_match": dob_year_match,
        "fn_sim": fn_sim,
        "ln_sim": ln_sim,
        "ln_phonetic": ln_phonetic,
        "email_local_sim": email_local_sim,
        "addr_sim": addr_sim,
        "same_postcode": same_postcode,
        "same_state": same_state,
        "common_surname_penalty": penalty,
    }
    return feats

WEIGHTS = {
    "same_email": 1.00,
    "same_phone": 0.90,
    "dob_exact": 0.50,
    "dob_year_match": 0.20,
    "fn_sim": 0.20,
    "ln_sim": 0.25,
    "ln_phonetic": 0.25,
    "email_local_sim": 0.20,
    "addr_sim": 0.40,
    "same_postcode": 0.20,
    "same_state": 0.10,
    "common_surname_penalty": 1.00,  # multiplier on penalty value (-0.15)
}

AUTO_THR = 1.20
REVIEW_THR = 0.80

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
        if ra == rb: return
        if self.rank[ra] < self.rank[rb]:
            self.parent[ra] = rb
        elif self.rank[ra] > self.rank[rb]:
            self.parent[rb] = ra
        else:
            self.parent[rb] = ra
            self.rank[ra] += 1

# -------------------------
# Main Pipeline
# -------------------------

def normalise_table(df: pd.DataFrame, table_name: str) -> pd.DataFrame:
    df = df.copy()
    df["__table"] = table_name
    # Required columns presence
    for col in ["first_name","last_name","dob","email","phone","address","suburb","state","postcode"]:
        if col not in df.columns:
            df[col] = ""

    df["first_name"] = df["first_name"].apply(normalise_first_name)
    df["last_name"]  = df["last_name"].apply(normalise_last_name)
    df["email"]      = df["email"].apply(normalise_email)
    df["phone"]      = df["phone"].apply(normalise_phone_au)
    df["dob"]        = df["dob"].apply(parse_dob)

    # State/Postcode
    df["state"] = df["state"].apply(normalise_state)
    df["postcode"] = df.apply(lambda r: normalise_postcode(r.get("postcode", "")), axis=1)

    # Address (expand + include suburb/state for similarity)
    base_addr = df["address"].apply(normalise_address)
    addr_extended = base_addr + " " + df["suburb"].apply(norm_text) + " " + df["state"].fillna("")
    df["address"] = addr_extended.str.strip()
    df["postcode"] = df.apply(lambda r: postcode_from_address(r["address"], r["postcode"]), axis=1)

    # Record id
    if "__id" not in df.columns:
        df["__id"] = [f"{table_name}:{i}" for i in range(len(df))]
    return df

def make_blocks(df: pd.DataFrame) -> Dict[str, List[int]]:
    blocks = {}
    for idx, row in df.iterrows():
        for k in blocking_keys(row):
            blocks.setdefault(k, []).append(idx)
    return blocks

def candidate_pairs(df: pd.DataFrame, blocks: Dict[str, List[int]]) -> List[Tuple[int, int]]:
    seen = set()
    pairs = set()
    for k, idxs in blocks.items():
        if len(idxs) < 2: 
            continue
        # Ensure cross-table only
        group = df.loc[idxs, ["__table","__id"]]
        # Partition indices by table
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

def score_pairs(df: pd.DataFrame, pairs: List[Tuple[int,int]]) -> List[MatchDecision]:
    decisions = []
    for i, j in pairs:
        a, b = df.loc[i], df.loc[j]
        feats = pair_features(a, b)
        score, contribs = weighted_score(feats)
        decision = "reject"
        if score >= AUTO_THR:
            decision = "auto"
        elif score >= REVIEW_THR:
            decision = "review"
        decisions.append(MatchDecision(
            left_id=a["__id"], right_id=b["__id"], score=score,
            decision=decision, features=feats, contributions=contribs
        ))
    return decisions

def cluster_entities(df: pd.DataFrame, decisions: List[MatchDecision]) -> pd.DataFrame:
    uf = UnionFind()
    # Auto edges only for clustering
    for d in decisions:
        if d.decision == "auto":
            uf.union(d.left_id, d.right_id)
    # Assign entity ids
    entity_id = {}
    for rid in df["__id"]:
        entity_id[rid] = uf.find(rid)

    # Canonicalise entity id (smallest string)
    # Remap to sequential ids for cleanliness
    roots = sorted(set(entity_id.values()))
    root_to_seq = {r: f"E{n+1}" for n, r in enumerate(roots)}
    df_out = df.copy()
    df_out["entity_id"] = df_out["__id"].map(lambda x: root_to_seq[entity_id[x]])
    return df_out, root_to_seq

def explain_table(decisions: List[MatchDecision]) -> pd.DataFrame:
    rows = []
    for d in decisions:
        row = {
            "left_id": d.left_id, "right_id": d.right_id,
            "score": d.score, "decision": d.decision
        }
        # Flatten key contributions for readability
        for k, v in d.contributions.items():
            row[f"c_{k}"] = v
        rows.append(row)
    return pd.DataFrame(rows).sort_values(["decision","score"], ascending=[True, False])

# -------------------------
# Driver
# -------------------------

def link_entities(tables: Dict[str, pd.DataFrame]):
    # 1) Normalise
    frames = []
    for name, df in tables.items():
        frames.append(normalise_table(df, name))
    df_all = pd.concat(frames, ignore_index=True)

    # 2) Blocking & candidates
    blocks = make_blocks(df_all)
    pairs = candidate_pairs(df_all, blocks)

    # 3) Score & decide
    decisions = score_pairs(df_all, pairs)

    # 4) Cluster on auto edges
    clustered, entity_map = cluster_entities(df_all, decisions)

    # 5) Review queue
    review = [d for d in decisions if d.decision == "review"]
    reject = [d for d in decisions if d.decision == "reject"]

    return {
        "records": clustered,
        "decisions_df": explain_table(decisions),
        "review_pairs": pd.DataFrame([d.__dict__ for d in review]),
        "reject_pairs": pd.DataFrame([d.__dict__ for d in reject]),
        "entity_map": entity_map
    }

# -------------------------
# Example usage (synthetic)
# -------------------------
if __name__ == "__main__":
    t1 = pd.DataFrame([
        {"first_name":"Liz","last_name":"Smith","dob":"14/03/1991","email":"liz.smith+promo@gmail.com","phone":"0412 345 678","address":"Unit 3/15 King St","suburb":"Newtown","state":"NSW","postcode":"2042"},
        {"first_name":"Robert","last_name":"Brown","dob":"1990-01-05","email":"rob.brown@outlook.com","phone":"(02) 9123 4567","address":"10 Mount Rd","suburb":"Chatswood","state":"NSW","postcode":"2067"},
        {"first_name":"Samantha","last_name":"Nguyen","dob":"05/05/1988","email":"sam.nguyen@uni.sydney.edu.au","phone":"0499 111 222","address":"8 Crescent Ave","suburb":"Carlton","state":"VIC","postcode":"3053"},
    ])
    t2 = pd.DataFrame([
        {"first_name":"Elizabeth","last_name":"Smith","dob":"1991-03-14","email":"lizsmith@gmail.com","phone":"+61 412 345 678","address":"3/15 King Street","suburb":"Newtown","state":"New South Wales","postcode":""},
        {"first_name":"Bob","last_name":"Brown","dob":"05/01/1990","email":"bobby.brown@outlook.com","phone":"+61291234567","address":"10 Mt Road","suburb":"Chatswood","state":"NSW","postcode":"2067"},
        {"first_name":"Sam","last_name":"Nguyen","dob":"1988/05/05","email":"sam.nguyen@USYD.edu.au","phone":"+61-499-111-222","address":"8 CRES AVE","suburb":"Carlton","state":"VIC","postcode":""},
    ])

    out = link_entities({"crm": t1, "orders": t2})
    print("\n=== Clustered Records ===")
    print(out["records"][["__table","__id","first_name","last_name","dob","email","phone","address","postcode","state","entity_id"]])

    print("\n=== Decisions (top) ===")
    print(out["decisions_df"].head(20))

    print("\n=== Review Queue ===")
    print(out["review_pairs"][["left_id","right_id","score"]].head())

    print("\n=== Entity Map Roots ===")
    print(out["entity_map"])
