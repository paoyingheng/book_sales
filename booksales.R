
# Load required libraries
library(tidyverse)
library(knitr)

# Load the dataset
data <- read.csv("sales2019.csv")

# Print a summary of the data
glimpse(data)


# Drop rows with missing reviews
data_clean <- data %>% filter(!is.na(user_submitted_review))  

# Calculate average total_purchased by book title and replace missing values
complete_sale_by_book <- data_clean %>% 
  group_by(title) %>%  
  mutate(avg_purchased = mean(total_purchased, na.rm = TRUE),
         total_purchased = ifelse(is.na(total_purchased), round(avg_purchased), total_purchased)) %>%  
  ungroup()  

# identify unique entries
unique(complete_sale_by_book$user_submitted_review)

#list positive sentiment keywords
words_pos <- c("okay", "Awesome", "better book", "OK", "learned") 

# Create a new column 'review' based on sentiment keywords
complete_sale_by_book <- complete_sale_by_book %>% 
  mutate(review = case_when(
    str_detect(user_submitted_review, paste(words_pos, collapse = "|")) ~ "Positive",
    TRUE ~ "Negative"
  ))

kable(head(complete_sale_by_book))


# Create a new column to separate sales into before and after the program
data_pre_post <- complete_sale_by_book %>% mutate(pre_or_post = if_else(mdy(date) < ymd("2019/07/01"), "Pre", "Post"))

# Compare book sales before and after the program
data_sales <- data_pre_post %>% 
  group_by(pre_or_post) %>%
  summarize(total_books_purchased = sum(total_purchased)) 

# Output a table with summary
kable(data_sales, caption = "Total books purchased before and after new sales program")


sales_by_customer <- data_pre_post %>% 
  group_by(pre_or_post, customer_type) %>%
  summarize(total_books_purchased = sum(total_purchased)) 

# Output a table with summary
kable(sales_by_customer, caption="Book Sales Before and After the Program by Customer Type")


pos_rv_pre_post <- data_pre_post %>%
  group_by(pre_or_post) %>% 
  summarize(number_reviews = sum(str_count(review,"Positive")))
kable(pos_rv_pre_post, caption="Positive reviews pre- vs post-new program")


neg_rv_pre_post <- data_pre_post %>%
  group_by(pre_or_post) %>% 
  summarize(number_reviews = sum(str_count(review,"Negative")))
kable(neg_rv_pre_post, caption="Negative reviews pre- vs post-new program")
