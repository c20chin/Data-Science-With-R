setwd("../R_working_directory")
loan=read.csv("./data/loanData.csv",stringsAsFactors = TRUE)
library(tidyverse)
library(ggplot2)
head(loan)
tibble(loan)

# data description
str(loan)


ggplot(data = loan, aes(x = reorder(region, -loan_amount), y = loan_amount)) +
  geom_bar(stat = "identity")


ggplot(data = loan) +
  geom_histogram(mapping = aes(x = loan_amount), binwidth = 0.5)


ggplot(data = loan, mapping = aes(x = loan_amount)) + 
  geom_freqpoly(mapping = aes(colour = grade), binwidth = 500)

ggplot(loan) +
  geom_bar(mapping = aes(x = application_type))

#standardised
ggplot(data = bfData, mapping = aes(x = Purchase, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = Age), binwidth = 500)


ggplot(data = bfData, mapping = aes(x = Gender, y = Purchase)) +
  geom_boxplot()

ggplot(data = bfData, mapping = aes(x = City_Category, y = Purchase)) +
  geom_boxplot()

loan$income_category <- ordered(loan$income_category, levels = c('High', 'Medium', 'Low'))
loan %>%
  count(income_category, loan_condition_cat) %>%
  ggplot(mapping = aes(x = income_category, y = loan_condition_cat)) +
  geom_tile(mapping = aes(fill = n))

#region vs loan amount
ggplot(data = loan, mapping = aes(x = loan_amount)) +
  geom_freqpoly(mapping = aes(colour = region), binwidth = 500)

ggplot(data = loan, mapping = aes(x = loan_amount, y = ..density..)) +
  geom_freqpoly(mapping = aes(colour = region), binwidth = 500)

loan%>% 
  count(income_category, loan_condition)

bfData %>%
  count(Product_Category_1, City_Category) %>%
  ggplot(mapping = aes(x = Product_Category_1, y = City_Category)) +
  geom_tile(mapping = aes(fill = n))


ggplot(data = loan) +
  geom_point(mapping = aes(x= grade_cat, y = loan_amount))

# annual income, loan amount
ggplot(data = loan) +
  geom_point(mapping = aes(x= annual_inc, y = loan_amount))

loan_inc <- loan %>%
  group_by(loan_amount) %>%
  summarize(Income_Mean = mean(annual_inc))

view(loan_inc)

ggplot(loan_inc, aes(x = loan_amount)) +
  geom_point(aes(y=Income_Mean))
  
