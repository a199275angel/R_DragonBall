getwd()
setwd()
install.packages(c("tidyverse", "ploty", "zoo", "lubridate", "rmarkdown","data.table", "DT", "kableExtra"), dependencies = TRUE)
library(DT)
library(zoo)
library(plotly)
library(lubridate)
library(rmarkdown)
library(data.table)
library(tidyverse)
library(kableExtra)
options(dplyr.print_max=1e9)
train1<-read.csv("train.csv")
train2<-fread("train.csv")
#檢示read.csv跟fread所讀到的差別
str(train1[,1:10])
str(train2[,1:10])
#觀察到read.csv讀到的categorical variable是Factor在fread是chr
#新設定train0，並設定不把數質讀成Factor
train0<-read.csv("train.csv", stringsAsFactors = FALSE)
test0<-read.csv("test.csv", stringsAsFactors = FALSE)
dim(train0)
#使用dim觀察資料的範圍，幾列幾行
dim(test0)
#可以發現差一列一行，想要知道差哪列就使用邏輯判斷式
# %in%是表示前者是否在包含在後者中
#下方程式的意思為colnames欄名 資料train0 !不%in%包含在 test0中
colnames(train0)[!colnames(train0)%in%colnames(test0)]
# %>% 管線與管線的套件 select()針對欄位 (Column)做子集，使用方式為select(資料名稱,欄位條件1,欄位條件2,...)
#從train0中選出欄位10~15
dat<-train0%>%select(10:15)
head(dat)
#欄位符合某種pattern
dat<-train0%>%select(matches("Yr|Year|year|yr")) 
head(dat)
#視覺化
plot_ly(train0, x=~SalePrice, type="histogram")
#範圍函數 $好像是截取的意思
range(train0$SalePrice)
#截取saletype最多交易方式的計數
table(train0$SaleType)
#使用filter
dat<-train0%>%filter(SaleType=="WD")
table(dat$SaleType) 
#售出年份與SaleType的數量 類似excel篩選
table(train0$YrSold, train0$SaleType)
#下面的跑不出來，不知道為什麼
dat<-train0%>%
  group_by(Neighborhood)%>%
  summarise(low=min(SalePrice),
            high=max(SalePrice),
            average=mean(SalePrice),
            sd=sd(SalePrice))%>%
  arrange(-average)
#看到了，要按envirment的表格
dat<-train0%>%
  group_by(YearBuilt, MasVnrType)%>%
  summarise(average=mean(SalePrice))%>%
  arrange(-average)

plot_ly(dat, x = ~YearBuilt, y = ~average, text = ~MasVnrType, type = 'scatter', mode = 'markers', size = ~average, color = ~MasVnrType,
        #Choosing the range of the bubbles' sizes:
        sizes = c(10, 80),
        marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(title = 'Estate Sale Price by Neighborhood',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = TRUE)
#改欄位名稱rename
dat<-train0%>%   
  select(BsmtQual)%>%
  rename(BsmtHght=BsmtQual)   
head(dat)
#
dcr0<-read.delim("data_description.txt", header = FALSE, stringsAsFactors = FALSE)

datatable(dcr0)
#
dat<-train0%>%select(MSZoning, YearBuilt, SalePrice)
head(dat)
