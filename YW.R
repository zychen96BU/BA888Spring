install.packages(zipcode)
library(tidyverse)
library(ggplot2)

sellers <- read_csv("olist_sellers_dataset.csv")
items <- read_csv("olist_order_items_dataset.csv")
cd <- read_csv("olist_closed_deals_dataset.csv")
mql <- read_csv("olist_marketing_qualified_leads_dataset.csv")

mf <- merge(cd, mql, by="mql_id")
mf
BR <- merge(sellers, items, by="seller_id")
BR
combined <- merge(mf, BR, by="seller_id")

final <- combined %>% 
  select(-has_company,-has_gtin,-average_stock,-declared_product_catalog_size)

reviews <- read_csv("Translated_reviews - order_review_translated.csv")
View(reviews)
join <- merge(reviews, final, by="order_id")
View(join)

colnames(join)
join %>% group_by(seller_zip_code_prefix) %>% summarise(avg_score=mean(review_score)) 

