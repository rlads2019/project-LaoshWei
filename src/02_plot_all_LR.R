
###################################################
## Plotting all linear regression for each group ##
###################################################

# Loading required packages

library(tidyverse)
library(readr)
library(readxl)
library(gridExtra)
library(Laurae)
library(ggpmisc)

# Read cultural_data_withNA.csv

read_csv("/Users/WayneChiang/Desktop/R_final/cultural_data_withNA.csv")


## 民眾參與指標
### ptcp_rate (pt)= 文化活動參與次數/千人
### cspt_ntd (cs) = 文化相關產業營業額/總人口
### ctgp_per1wp (ct) = 民間文化組織數/萬人

## 政府投入指標
### training_hr (tr) = 文化培訓總時數
### pct_budget (pc) = 文化預算佔總預算之比例
### pctc_per1wp (pp) = 文化展演場地數/萬人

##################################################
# 以region & status分類縣市，找出R^2 > 0.3的組別 #
##################################################

# 分別以region & status分類縣市
# 從中尋找出R^2 > 0.3的組別

## 圖2A、以region劃分之九宮格

### 第一列：以文化培訓時數為x軸

plot_trpt <- ggplot(cultural_data_withNA, aes(x= training_hr, y= ptcp_rate, col = region))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = formula_1, se = F) +
  stat_poly_eq(aes(label = ..rr.label..), size = 3,
               geom = "label", alpha = 0.2,
               formula = formula_1, parse = TRUE)

plot_trcs <- ggplot(cultural_data_withNA, aes(x= training_hr, y= cspt_ntd, col = region)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = formula_1, se = F) +
  stat_poly_eq(aes(label = ..rr.label..), size = 3,
               geom = "label", alpha = 0.2,
               formula = formula_1, parse = TRUE)

plot_trct <- ggplot(cultural_data_withNA, aes(x= training_hr, y= ctgp_per1wp, col = region)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = formula_1, se = F) +
  stat_poly_eq(aes(label = ..rr.label..), size = 3,
               geom = "label", alpha = 0.2,
               formula = formula_1, parse = TRUE)

### 第二列：以文化預算比例為x軸

plot_pcpt <- ggplot(cultural_data_withNA, aes(x= pct_budget, y= ptcp_rate, col = region))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = formula_1, se = F) +
  stat_poly_eq(aes(label = ..rr.label..), size = 3,
               geom = "label", alpha = 0.2,
               formula = formula_1, parse = TRUE)

plot_pccs <- ggplot(cultural_data_withNA, aes(x= pct_budget, y= cspt_ntd, col = region))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = formula_1, se = F) +
  stat_poly_eq(aes(label = ..rr.label..), size = 3,
               geom = "label", alpha = 0.2,
               formula = formula_1, parse = TRUE)

plot_pcct <- ggplot(cultural_data_withNA, aes(x= pct_budget, y= ctgp_per1wp, col = region))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = formula_1, se = F) +
  stat_poly_eq(aes(label = ..rr.label..), size = 3,
               geom = "label", alpha = 0.2,
               formula = formula_1, parse = TRUE)

### 第三列：以每萬人文化場地數為x軸

plot_pppt <- ggplot(cultural_data_withNA, aes(x= pctc_per1wp, y= ptcp_rate, col = region))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = formula_1, se = F) +
  stat_poly_eq(aes(label = ..rr.label..), size = 3,
               geom = "label", alpha = 0.2,
               formula = formula_1, parse = TRUE)

plot_ppcs <- ggplot(cultural_data_withNA, aes(x= pctc_per1wp, y= cspt_ntd, col = region))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = formula_1, se = F) +
  stat_poly_eq(aes(label = ..rr.label..), size = 3,
               geom = "label", alpha = 0.2,
               formula = formula_1, parse = TRUE)

plot_ppct <- ggplot(cultural_data_withNA, aes(x= pctc_per1wp, y= ctgp_per1wp, col = region))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = formula_1, se = F) +
  stat_poly_eq(aes(label = ..rr.label..), size = 3,
               geom = "label", alpha = 0.2,
               formula = formula_1, parse = TRUE)

### 儲存圖檔

plot_trpt
plot_trcs
plot_trct
plot_pcpt
plot_pccs
plot_pcct
plot_pppt
plot_ppcs
plot_ppct

# 記得clean the environment

## 圖2B、以status劃分之九宮格

plot_trpt <- ggplot(cultural_data_withNA, aes(x= training_hr, y= ptcp_rate, col = status))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = formula_1, se = F) +
  stat_poly_eq(aes(label = ..rr.label..), size = 3,
               geom = "label", alpha = 0.2,
               formula = formula_1, parse = TRUE)

plot_trcs <- ggplot(cultural_data_withNA, aes(x= training_hr, y= cspt_ntd, col = status)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = formula_1, se = F) +
  stat_poly_eq(aes(label = ..rr.label..), size = 3,
               geom = "label", alpha = 0.2,
               formula = formula_1, parse = TRUE)

plot_trct <- ggplot(cultural_data_withNA, aes(x= training_hr, y= ctgp_per1wp, col = status)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = formula_1, se = F) +
  stat_poly_eq(aes(label = ..rr.label..), size = 3,
               geom = "label", alpha = 0.2,
               formula = formula_1, parse = TRUE)

plot_pcpt <- ggplot(cultural_data_withNA, aes(x= pct_budget, y= ptcp_rate, col = status))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = formula_1, se = F) +
  stat_poly_eq(aes(label = ..rr.label..), size = 3,
               geom = "label", alpha = 0.2,
               formula = formula_1, parse = TRUE)

plot_pccs <- ggplot(cultural_data_withNA, aes(x= pct_budget, y= cspt_ntd, col = status))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = formula_1, se = F) +
  stat_poly_eq(aes(label = ..rr.label..), size = 3,
               geom = "label", alpha = 0.2,
               formula = formula_1, parse = TRUE)

plot_pcct <- ggplot(cultural_data_withNA, aes(x= pct_budget, y= ctgp_per1wp, col = status))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = formula_1, se = F) +
  stat_poly_eq(aes(label = ..rr.label..), size = 3,
               geom = "label", alpha = 0.2,
               formula = formula_1, parse = TRUE)

plot_pppt <- ggplot(cultural_data_withNA, aes(x= pctc_per1wp, y= ptcp_rate, col = status))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = formula_1, se = F) +
  stat_poly_eq(aes(label = ..rr.label..), size = 3,
               geom = "label", alpha = 0.2,
               formula = formula_1, parse = TRUE)

plot_ppcs <- ggplot(cultural_data_withNA, aes(x= pctc_per1wp, y= cspt_ntd, col = status))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = formula_1, se = F) +
  stat_poly_eq(aes(label = ..rr.label..), size = 3,
               geom = "label", alpha = 0.2,
               formula = formula_1, parse = TRUE)

plot_ppct <- ggplot(cultural_data_withNA, aes(x= pctc_per1wp, y= ctgp_per1wp, col = status))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = formula_1, se = F) +
  stat_poly_eq(aes(label = ..rr.label..), size = 3,
               geom = "label", alpha = 0.2,
               formula = formula_1, parse = TRUE)

### 儲存圖檔

plot_trpt
plot_trcs
plot_trct
plot_pcpt
plot_pccs
plot_pcct
plot_pppt
plot_ppcs
plot_ppct
