# load olist datasets
customer <- read.csv('olist_customers_dataset.csv')
order <- read.csv('olist_orders_dataset.csv')
order_item <- read.csv('olist_order_items_dataset.csv')
order_payment <- read.csv('olist_order_payments_dataset.csv')
order_reviews <- read.csv('olist_order_reviews_dataset.csv')
order_review_translated <- read.csv('Translated_reviews - order_review_translated.csv')

product <- read.csv('olist_products_dataset.csv')
location <- read.csv('olist_geolocation_dataset.csv')
sellers <- read.csv('olist_sellers_dataset.csv')

# load marketing funnel dataset
cd <- read_csv("olist_closed_deals_dataset.csv")
mql <- read_csv("olist_marketing_qualified_leads_dataset.csv")
marketing_funnel<- merge(cd, mql, by="mql_id")

location1 = location %>% group_by(geolocation_zip_code_prefix) %>% 
  summarize(mean_lat = mean(geolocation_lat),
            mean_long = mean(geolocation_lng))

### join all the dataset. 
p<- left_join(left_join(left_join(customer, order),order_item),product)
transaction <- left_join(p,order_payment)
transaction1 <- left_join(transaction,location1,
                          by = c("customer_zip_code_prefix"="geolocation_zip_code_prefix"))
transaction2 <- na.omit(left_join(transaction1,sellers))

transaction <- transaction2 %>%
  mutate(major_state = if_else(customer_state == c('SP','RJ','MG','BA','PA','PE'), 1, 0))

transaction$order_estimated_delivery_date <- as.POSIXct(transaction$order_estimated_delivery_date, format="%Y-%m-%d %H:%M:%S")
transaction$order_approved_at <- as.POSIXct(transaction$order_approved_at, format="%Y-%m-%d %H:%M:%S")
transaction$order_delivered_customer_date <- as.POSIXct(transaction$order_delivered_customer_date, format="%Y-%m-%d %H:%M:%S")
transaction$deliverd_difftime <- as.numeric(difftime(transaction$order_delivered_customer_date ,transaction$order_estimated_delivery_date)/3600/24)

transaction <- na.omit(transaction)

final <- merge(mf, transaction, by="seller_id")
translated_review_clean <- order_review_translated %>% 
  select(order_id,review_score,review_comments)

final_withreview <- merge(final,translated_review_clean, by="order_id")

final_withreview <- left_join(final,translated_review_clean, by="order_id")
View(final_withreview)

