import pandas as pd
from au_entity_matching_tqdm_v3_1 import match_entities

df_left = pd.DataFrame([
        {"FirstName":"Liz","Surname":"Smith","DOB":"14/03/1991","Email":"liz.smith+promo@gmail.com","Phone":"0412 345 678","Address":"Unit 3/15 King St","Suburb":"Newtown","State":"NSW","Postcode":"2042"},
        {"FirstName":"Robert","Surname":"Brown","DOB":"1990-01-05","Email":"rob.brown@outlook.com","Phone":"(02) 9123 4567","Address":"10 Mount Rd","Suburb":"Chatswood","State":"NSW","Postcode":"2067"},
        {"FirstName":"Samantha","Surname":"Nguyen","DOB":"05/05/1988","Email":"sam.nguyen@uni.sydney.edu.au","Phone":"0499 111 222","Address":"8 Crescent Ave","Suburb":"Carlton","State":"VIC","Postcode":"3053"},
    ]).set_index(pd.Index([101,102,103]))  # custom original indices
df_right = pd.DataFrame([
        {"first_name":"Elizabeth","last_name":"Smith","dob":"1991-03-14","email":"lizsmith@gmail.com","phone":"+61 412 345 678","address":"3/15 King Street","suburb":"Newtown","state":"New South Wales","postcode":""},
        {"first_name":"Bob","last_name":"Brown","dob":"05/01/1990","email":"bobby.brown@outlook.com","phone":"+61291234567","address":"10 Mt Road","suburb":"Chatswood","state":"NSW","postcode":"2067"},
        {"first_name":"Sam","last_name":"Nguyen","dob":"1988/05/05","email":"sam.nguyen@USYD.edu.au","phone":"+61-499-111-222","address":"8 CRES AVE","suburb":"Carlton","state":"VIC","postcode":""},
    ]).set_index(pd.Index([555,556,557]))
# Map your columns to the standard fields the matcher expects
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
auto_pairs, review_df = match_entities(
    df_left, df_right, schema_left, schema_right,
    auto_threshold=1.20, review_threshold=0.80,
    review_path="to_review.csv",   # optional CSV for human QA
    matches_path= "matches.csv"
)

print(auto_pairs[:10])     # -> [(left_idx, right_idx), ...] using original indices
print(review_df.head())    # explainable scores + contributions per pair
