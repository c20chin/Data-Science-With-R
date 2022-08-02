setwd("./R_working_directory")
bfData=read.csv("./data/BlackFriday.csv",stringsAsFactors = TRUE)
library(tidyverse)
library(ggplot2)
head(dataset)
tibble(dataset)

# data description
str(bfData)


ggplot(data = bfData) +
  geom_bar(mapping = aes(x= Occupation))

bfData %>%
  count(Occupation)

#Purchase
ggplot(data = bfData) +
  geom_histogram(mapping = aes(x = Purchase), binwidth = 0.5) +
  ggtitle("Count of Purchase")

# summary on purchase
summary(bfData$Purchase)

ggplot(data = bfData, mapping = aes(x = Purchase)) + 
  geom_freqpoly(mapping = aes(colour = Age), binwidth = 500)

ggplot(bfData) +
  geom_bar(mapping = aes(x = Age))

#standardized
ggplot(data = bfData, mapping = aes(x = Purchase, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = Age), binwidth = 500) +
  ggtitle("Purchase of Each Age Bin - Standardized")

ggplot(data = bfData, mapping = aes(x = Purchase)) + 
  geom_freqpoly(mapping = aes(colour = Age), binwidth = 500) +
  ggtitle("Purchase of Each Age Bin")



ggplot(data = bfData, mapping = aes(x = Gender, y = Purchase)) +
  geom_boxplot(aes(fill = Gender))+
  ggtitle("Gender in Purchase")

ggplot(data = bfData, mapping = aes(x = City_Category, y = Purchase)) +
  geom_boxplot(aes(fill = City_Category)) +
  ggtitle("Cities in Purchase") +
  xlab("City Category")  


  
ggplot(bfData, aes(x = reorder(Product_Category_1, -Count), y = Count)) +
  geom_bar(stat = "identity")

bfData %>%
  count(Product_Category_1, Age) %>%
  ggplot(mapping = aes(x = Product_Category_1, y = Age)) +
  geom_tile(mapping = aes(fill = n))

bfData %>%
  count(Product_Category_1, City_Category) %>%
  ggplot(mapping = aes(x = Product_Category_1, y = City_Category)) +
  geom_tile(mapping = aes(fill = n))


