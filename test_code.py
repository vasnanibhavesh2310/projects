import pandas as pd
from au_entity_matching import match_entities

# df_left / df_right: your two input DataFrames
# Map your columns to the standard fields the matcher expects
schema_left = {
  "first_name":"FirstName","last_name":"Surname","dob":"DOB","email":"Email",
  "phone":"Phone","address":"Address","suburb":"Suburb","state":"State","postcode":"Postcode",
}
schema_right = {
  "first_name":"first_name","last_name":"last_name","dob":"dob","email":"email",
  "phone":"phone","address":"address","suburb":"suburb","state":"state","postcode":"postcode",
}

auto_pairs, review_df = match_entities(
    df_left, df_right, schema_left, schema_right,
    auto_threshold=1.20, review_threshold=0.80,
    review_path="to_review.csv"   # optional CSV for human QA
)

print(auto_pairs[:10])     # -> [(left_idx, right_idx), ...] using original indices
print(review_df.head())    # explainable scores + contributions per pair
