library(tidyverse)
library(ggplot2)
install.packages('maptools')
install.packages('rgeos') 
install.packages('RColorBrewer') 
install.packages('classInt')
library(dplyr)
library(plyr)
library(maptools)

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
X <- join %>% group_by(seller_state) %>% summarise(avg_score=mean(review_score)) 
Y <- join %>% group_by(seller_state ) %>% count()
Z <- merge(X,Y, by= "seller_state")

ggplot(Z,aes(x=seller_state, y=avg_score)) + geom_bar(stat="identity") +geom_text(aes(label=avg_score), vjust=-0.3, size=3.5)+
  theme_minimal()

ggplot(Z,aes(x=seller_state, y=avg_score,group=1)) +geom_line() +geom_text(aes(label= n ), vjust=-0.3, size=3.5)+
  theme_minimal()

x <- join %>% group_by(seller_zip_code_prefix) %>% summarise(avg_score=mean(review_score)) 
y <- join %>% group_by(seller_zip_code_prefix ) %>% select(seller_zip_code_prefix) %>% count() 
sort(y,decreasing = T)
head(y)
                                                                                                            

