library(tidyverse)

setwd("~/Desktop/BA 888/888/")

sellers <- read_csv("olist_sellers_dataset.csv")
items <- read_csv("olist_order_items_dataset.csv")
cd <- read_csv("olist_closed_deals_dataset.csv")
mql <- read_csv("olist_marketing_qualified_leads_dataset.csv")
trsanlatedReview <- read_csv("Translated_reviews - order_review_translated.csv")

mf <- merge(cd, mql, by="mql_id")
BR <- merge(sellers, items, by="seller_id")
combined <- merge(mf, BR, by="seller_id")

final <- combined %>% 
  select(-has_company,-has_gtin,-average_stock,-declared_product_catalog_size)

translated_review_clean <- trsanlatedReview %>% 
  select(order_id,review_score,review_comments)

final_withreview <- merge(final,translated_review_clean, by="order_id")