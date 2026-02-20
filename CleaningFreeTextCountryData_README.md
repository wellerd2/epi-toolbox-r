Travel Destination Mapping

This script standardizes messy travel destination text and maps it to WHO and GBD regions.

What it does:
1. Takes a dataset with a free-text column named traveldest.
2. Standardizes country names using a manual alias table and a country dictionary.
3. Maps each destination to WHO subregion, WHO region, and GBD region.
4. Handles multiple countries in one field.
5. Flags Mexico and Caribbean where applicable.
5. Reprocesses unmapped rows using a more flexible tokenizer.

Required objects in your R session:
1. travellers: a data.frame or tibble containing a column named traveldest.
2. country_alias_map: a tibble with columns from and to.
3. country_dictionary: a data.frame with columns country_key, country_canonical, aliases, exactmatch, exclusions, who_subregion, who_region, gbd_region.

How matching works (briefly):
1. Text is lowercased and trimmed.
2. Manual aliases are applied first.
3. Tokens are matched exactly to country_key.
4. If no match is found, full-field exactmatch is attempted.
5. Remaining unmapped rows are re-tokenized more flexibly and matched again.

Output:
1. travellers3 (and travellers4 after second pass).
2. Added columns: who_subregion_all; who_subregion_single; who_region_all; who_region_single; gbd_region_all; gbd_region_single; unmapped_tokens; Mexico; Caribbean.

How to run:
1. Load travellers; country_alias_map; country_dictionary into your R session.
2. Run the R Markdown document or source the script.
3. Inspect travellers3 or travellers4.

Notes:
1. Most improvements should be made in country_alias_map or country_dictionary, not by changing the code.
2. If many rows are Unmapped, add aliases or exactmatch terms.