
##########################
## 資料輸入與離群值處理 ##
##########################

# Loading required packages

library(tidyverse)
library(readr)
library(readxl)

# Read file : cultural_data.csv

cultural_data <- read_csv("/Users/WayneChiang/Desktop/R_final/cultural_data.csv")

# Creating new column

## region：北部/中部/南部/東部/離島

region <- rep(c("northern","central","southern","northern",
                "central","southern","eastern","offshore",
                "northern","southern","offshore"), 
              c(3,1,2,2,4,2,2,1,2,1,2))

## status：直轄市/一般市/一般縣
status <- rep(c("municipality","county","city","county"), 
              c(6,11,3,2))

# Adding 2 new columns to cultura_data

cultural_data <- cultural_data %>%
  transmute(year, admin, region = rep(region, 6), 
            status = rep(status, 6), ptcp_rate, 
            cspt_ntd, ctgp_per1wp, training_hr,
            pct_budget, pctc_per1wp)

###################################
## Deleting & Replacing Outliers ##
###################################

# 先用pairs()看scatterplot matrix
ssm_original <- pairs(~ptcp_rate+cspt_ntd+ctgp_per1wp+training_hr+pct_budget+pctc_per1wp,
                      data = cultural_data, main = "Simple Scatterplot Matrix_Original Data")

# Finding & Replacing Outliers
# 從原始數據中找出極端值，將那些極端值轉為NA或5/95百分位數
# 用 boxplot.stats()可找出 > Q3+1.5IQR or < Q1-1.5IQR 的outliers 
# e.g. outliers.ptcp_rate <- boxplot.stats(cultural_data$ptcp_rate)$out

## Method 1: Remove outliers with 5%/95% ile
## 自訂函數 cap_outliers()
## 分別用數據中的 5% 和 95% 的百分位數
## 取代< Q1 - 1.5IQR 以及 > Q3 + 1.5IQR的離群值

cap_outliers <- function(x) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T) # qnt 為 Q1 & Q3
  caps <- quantile(x, probs=c(.05, .95), na.rm = T) # caps 為 5%ile & 95%ile
  H <- 1.5 * IQR(x, na.rm = T) # H = 1.5IQR
  x[x < (qnt[1] - H)] <- caps[1] # 當 x < Q1 - 1.5IQR，以NA取代之
  x[x > (qnt[2] + H)] <- caps[2] # 當 x > Q3 + 1.5IQR，以NA取代之
  return(x)
}

cultural_data_test1 <- cultural_data

cultural_data_test1$ptcp_rate <- cap_outliers(cultural_data$ptcp_rate)
cultural_data_test1$cspt_ntd <- cap_outliers(cultural_data$cspt_ntd)
cultural_data_test1$ctgp_per1wp <- cap_outliers(cultural_data$ctgp_per1wp)
cultural_data_test1$training_hr <- cap_outliers(cultural_data$training_hr)
cultural_data_test1$pct_budget <- cap_outliers(cultural_data$pct_budget)
cultural_data_test1$pctc_per1wp <- cap_outliers(cultural_data$pctc_per1wp)

pairs(~ptcp_rate+cspt_ntd+ctgp_per1wp+training_hr+pct_budget+pctc_per1wp,
      data = cultural_data_test1, main = "Simple Scatterplot Matrix_Replace Outliers with 5%/95%ile")

### 還是有許多極端值分佈於散佈圖邊緣...試試看用NA取代outliers

## Method 2: Remove outliers with NA
## 自訂函數 remove_outliers()
## 以 NA 取代< Q1 - 1.5IQR 以及 > Q3 + 1.5IQR的離群值

remove_outliers <- function(x) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T) # qnt 為 Q1 & Q3
  H <- 1.5 * IQR(x, na.rm = T) # H = 1.5IQR
  x[x < (qnt[1] - H)] <- NA # 當 x < Q1 - 1.5IQR，以NA取代之
  x[x > (qnt[2] + H)] <- NA # 當 x > Q3 + 1.5IQR，以NA取代之
  return(x)
}

cultural_data_test2 <- cultural_data

cultural_data_test2$ptcp_rate <- remove_outliers(cultural_data$ptcp_rate)
cultural_data_test2$cspt_ntd <- remove_outliers(cultural_data$cspt_ntd)
cultural_data_test2$ctgp_per1wp <- remove_outliers(cultural_data$ctgp_per1wp)
cultural_data_test2$training_hr <- remove_outliers(cultural_data$training_hr)
cultural_data_test2$pct_budget <- remove_outliers(cultural_data$pct_budget)
cultural_data_test2$pctc_per1wp <- remove_outliers(cultural_data$pctc_per1wp)

pairs(~ptcp_rate+cspt_ntd+ctgp_per1wp+training_hr+pct_budget+pctc_per1wp,
      data = cultural_data_test2, main = "Simple Scatterplot Matrix_Replace Outliers with NA")

### 結果看起來比用 5%/95%ile 取代來得好一些
### 以NA取代outliers更能看出資料點的分佈狀況
### 因此決定以NA取代< Q1 - 1.5IQR 以及 > Q3 + 1.5IQR的離群值

cultural_data_withNA <- cultural_data_test2

sum(is.na(cultural_data_withNA) == TRUE)

### 總共有59個極端值被以NA取代

sum(is.na(cultural_data_withNA) == TRUE)/(132*6)

### NA約佔全部資料的7.45% 

# 把去除outliers後的檔案存入cultural_data_withNA.csv

write.csv(cultural_data_withNA, file = "/Users/WayneChiang/Desktop/R_final/Final code/cultural_data_withNA.csv")

