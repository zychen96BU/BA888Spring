library(tidyverse)

sellers <- read_csv("Desktop/BA888/brazilian-ecommerce/olist_sellers_dataset.csv")
items <- read_csv("Desktop/BA888/brazilian-ecommerce/olist_order_items_dataset.csv")
cd <- read_csv("Desktop/BA888/marketing-funnel-olist/olist_closed_deals_dataset.csv")
mql <- read_csv("Desktop/BA888/marketing-funnel-olist/olist_marketing_qualified_leads_dataset.csv")

mf <- merge(cd, mql, by="mql_id")
BR <- merge(sellers, items, by="seller_id")
final <- merge(mf, BR, by="seller_id")

