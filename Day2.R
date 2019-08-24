getwd()
library(tidyverse)
library(ggplot2)
library(dplyr)
library(magrittr)
library(corrplot)
library(gridExtra)
library(plotly)
train0 <- read.csv("train.csv", stringsAsFactors = FALSE)
test0 <- read.csv("test.csv", stringsAsFactors = FALSE)
# Names 欄位名稱 which 是一個test sapply為向量輸出
#把train0是數目的以向量輸出，且只輸出名子
num_features <- names(which(sapply(train0, is.numeric)))
#把train0是文字的以向量輸出，且只輸出名子
cat_features <- names(which(sapply(train0, is.character)))
#不知道下面是什麼意思
train_numeric <- train0[, names(train0) %in% num_features]
train_categoric <- train0[, names(train0) %in% cat_features]
print(num_features)
print(cat_features)
#37個數字;43個類別
#把評分用的數字轉成categroical類別因子
train0$OverallCond <- as.factor(train0$OverallCond)
train0$OverallQual <- as.factor(train0$OverallQual)
train0$MSSubClass <- as.factor(train0$MSSubClass)
#查看資料缺失並刪除NA
missing_values <- sapply(train0, function(x) sum(is.na(x)))
null_count <- data.frame(Count = missing_values, Proportion = missing_values/nrow(train0))
null_count_gteZero <- null_count[null_count$Count > 0, ]
train_non_null <- train0 %>% 
  select(-c(rownames(null_count_gteZero), OverallCond, OverallQual, MSSubClass))
null_count_gteZero[order(-null_count_gteZero$Count),]
#刪除所有出現NA的欄位
train_non_null<-train0%>%select(-c(rownames(null_count_gteZero), OverallCond, OverallQual, MSSubClass))
#網路上發現可以抓na的數量公式
sapply(train_non_null,function(x)sum(is.na(x)))
#練習畫圖
#paste0(..., collapse = NULL)黏貼聯接的意思
data.frame(num_features)
match_num_features <- paste(num_features, collapse = "|")
train_non_null_df <- select(train_non_null, matches(match_num_features))
#
theme_set(theme_bw())  # pre-set the bw theme.

# 篩選SF(面積)的欄位 
train_SF <- select(train_non_null, matches("SF|SalePrice"))

# 各種面積與SalePrice的關係
#gather()搭配facet_wrap()他可以將欄位分成 key 跟 value 兩種。重新組合成一個 長 的格式，可以用 key 跟 value 選擇要保留下來的欄位。
train_SF %>%
  # keep(is.numeric) %>% 
  gather(-SalePrice, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = SalePrice)) +   # Plot the values
  facet_wrap(~ var, scales = "free") +   # In separate panels
  geom_point() 

