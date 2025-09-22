import time
import pandas as pd
from au_entity_matching_tqdm_v3_2 import match_entities
from utility import convert_all_columns_to_string

df_left = pd.read_csv("All Donors.csv")
df_right = pd.read_csv("Accomodation_Data_clean.csv")

# Convert every column to string to ensure downstream code treats values as text
df_left = convert_all_columns_to_string(df_left)
df_right = convert_all_columns_to_string(df_right)
# Map your columns to the standard fields the matcher expects
schema_left = {
  "first_name":"First Name","last_name":"Last Name","dob":"Birthdate","email":"Email",
  "phone":"Mobile","suburb":"Suburb","state":"Mailing State","postcode":"Mailing Postal Code",
}
schema_right = {
  "first_name":"First Name","last_name":"Last Name","dob":"Birthdate","email":"Email",
  "phone":"Phone","suburb":"Suburb","state":"Mailing State","postcode":"Mailing Postal Code",
}
start = time.perf_counter()
auto_pairs, review_df = match_entities(
    df_left, df_right, schema_left, schema_right,
    auto_threshold=2.0, review_threshold=1.7,
    matches_path='auto_matches.csv',  # optional CSV output of auto-matched pairs
    review_path="to_review.csv"   # optional CSV for human QA
)
end = time.perf_counter()

# Calculate metrics
elapsed = end - start
n_rows = len(df_left)
avg_time_per_row = elapsed / n_rows if n_rows else float('nan')

print(f"\nTotal time spent: {elapsed:.2f} seconds")
print(f"Average time per row: {avg_time_per_row:.6f} seconds")

print(auto_pairs[:10])     # -> [(left_idx, right_idx), ...] using original indices
print(review_df.head())    # explainable scores + contributions per pair
