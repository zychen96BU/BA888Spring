install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)

setwd("~/BA888Spring")

sellers <- read_csv("olist_sellers_dataset.csv")
View(sellers)
items <- read_csv("olist_order_items_dataset.csv")
View(items)
cd <- read_csv("olist_closed_deals_dataset.csv")
View(cd)
mql <- read_csv("olist_marketing_qualified_leads_dataset.csv")
View(mql)
mf <- merge(cd, mql, by="mql_id")
View(mf)
BR <- merge(sellers, items, by="seller_id")
View(BR)
combined <- merge(mf, BR, by="seller_id")
View(combined)

final <- combined %>% 
  select(-has_company,-has_gtin,-average_stock,-declared_product_catalog_size)
View(final)

translated_review_clean <- trsanlatedReview %>% 
  select(order_id,review_score,review_comments)

final_withreview <- left_join(final,translated_review_clean, by="order_id")

#histogram for price 
hist(final$price, 
     xlim = c(0,800), 
     ylim = c(0,5000), 
     main = "Histogram for price",
     xlab="Price",
     col = "orange")
max(final$price)
min(final$price)

final <- as.data.frame(final)
ggplot(data = final,aes(final$price)) + 
  geom_histogram(binwidth = 600,
                 xlim =c(0,1000)) 

# histogram for price below 250
qplot(final$price,
      geom = "histogram",
      binwidth = 10,
      xlim = c(0,250),
      main = "Histogram for price below 250",
      xlab = "price",
      fill = I("#F26633"),
      col = I("black"))

# histogram for price (0,1000)
qplot(final$price,
      geom = "histogram",
      binwidth = 30,
      main = "Histogram for price",
      xlim = c(0,1000),
      xlab = "price",
      fill = I("#F26633"))
