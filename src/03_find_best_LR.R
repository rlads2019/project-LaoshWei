
####################################
## Region組：尋找最適合之回歸模型 ##
####################################

formula_1 <- y ~ poly(x, 1, raw = TRUE)
formula_2 <- y ~ poly(x, 2, raw = TRUE)
formula_3 <- y ~ poly(x, 3, raw = TRUE)

cultural_data_northern <- cultural_data_withNA %>% 
  filter(region == "northern")

cultural_data_central <- cultural_data_withNA %>% 
  filter(region == "central")

cultural_data_southern <- cultural_data_withNA %>% 
  filter(region == "southern")

cultural_data_eastern <- cultural_data_withNA %>% 
  filter(region == "eastern")

cultural_data_offshore <- cultural_data_withNA %>% 
  filter(region == "offshore")

################################
## 圖6A:trpt_southern ##########
################################

### R^2 = 0.52, 0.56, 0.59

### 檢驗回歸適配性

attach(cultural_data_southern)

trpt_ln = lm(ptcp_rate~poly(training_hr, degree=1,raw=T)) 
summary(trpt_ln) # x^1能解釋y值之顯著差異
trpt_qd = lm(ptcp_rate~poly(training_hr, degree=2,raw=T)) 
summary(trpt_qd) # x^2無法解釋y值之顯著差異
trpt_cb = lm(ptcp_rate~poly(training_hr, degree=3,raw=T)) 
summary(trpt_cb) # x^3也無法解釋y值之顯著差異

anova(trpt_ln, trpt_qd)[2,6] # 二次沒比一次好
anova(trpt_qd, trpt_cb)[2,6] # 三次沒比二次好
anova(trpt_ln, trpt_cb)[2,6] # 三次也沒比一次好

detach(cultural_data_southern)

# 只有x^1能解釋y值之顯著差異 故使用一次線性回歸

################################
## 圖6B:trcs_southern ##########
################################

### R^2 = 0.42, 0.45, 0.46

### 檢驗回歸適配性 & partial F test

attach(cultural_data_southern)

trcs_ln = lm(cspt_ntd~poly(training_hr, degree=1,raw=T)) 
summary(trcs_ln) # x^1能解釋y值之顯著差異
trcs_qd = lm(cspt_ntd~poly(training_hr, degree=2,raw=T)) 
summary(trcs_qd) # x^2無法解釋y值之顯著差異
trcs_cb = lm(cspt_ntd~poly(training_hr, degree=3,raw=T)) 
summary(trcs_cb) # x^3無法解釋y值之顯著差異

anova(trcs_ln, trcs_qd)[2,6] # 1>2
anova(trcs_qd, trcs_cb)[2,6] # 2>3
anova(trcs_ln, trcs_cb)[2,6] # 1>2>3

detach(cultural_data_southern)

# 1>2>3 故使用線性回歸

###############################
## 圖X:pcpt_eastern (此圖不用)
###############################

### R^2 = 0.32, 0.48, 0.64

### 檢驗回歸適配性 & partial F test

attach(cultural_data_eastern)

pcpt_ln = lm(ptcp_rate~poly(pct_budget, degree=1,raw=T)) 
summary(pcpt_ln) # x^1無法解釋y值之顯著差異(p = 0.0686)
pcpt_qd = lm(ptcp_rate~poly(pct_budget, degree=2,raw=T)) 
summary(pcpt_qd) # x^2無法解釋y值之顯著差異
pcpt_cb = lm(ptcp_rate~poly(pct_budget, degree=3,raw=T)) 
summary(pcpt_cb) # x^3無法解釋y值之顯著差異

anova(pcpt_ln, pcpt_qd)[2,6] # 1>2
anova(pcpt_qd, pcpt_cb)[2,6] # 2>3
anova(pcpt_ln, pcpt_cb)[2,6] # 1>2>3

detach(cultural_data_eastern)

# x^1, x^2, x^3 都無法解釋y值之顯著差異
# 故不放此張圖

###############################
## 圖6C:pcct_central ##########
###############################

### R^2 = 0.42, 0.42, 0.44

### 檢驗回歸適配性

attach(cultural_data_central)

pcct_ln = lm(ctgp_per1wp~poly(pct_budget, degree=1,raw=T)) 
summary(pcct_ln) # x^1可以解釋y值之顯著差異
pcct_qd = lm(ctgp_per1wp~poly(pct_budget, degree=2,raw=T)) 
summary(pcct_qd) # x^2無法解釋y值之顯著差異
pcct_cb = lm(ctgp_per1wp~poly(pct_budget, degree=3,raw=T)) 
summary(pcct_cb) # x^3無法解釋y值之顯著差異

anova(pcct_ln, pcct_qd)[2,6] # 1>2
anova(pcct_qd, pcct_cb)[2,6] # 2>3
anova(pcct_ln, pcct_cb)[2,6] # 1>2>3

detach(cultural_data_central)

# 只有x^1可解釋y值之顯著差異
# x^3 x^2 都沒有比x^1好
# 故選用一次回歸

################################
## 圖6D:pcct_southern ##########
################################

### R^2 = 0.38, 0.38, 0.47

### 檢驗回歸適配性 & Partial F test

attach(cultural_data_southern)

pcct_ln = lm(ctgp_per1wp~poly(pct_budget, degree=1,raw=T)) 
summary(pcct_ln) # x^1可以解釋y值之顯著差異
pcct_qd = lm(ctgp_per1wp~poly(pct_budget, degree=2,raw=T)) 
summary(pcct_qd) # x^2無法解釋y值之顯著差異
pcct_cb = lm(ctgp_per1wp~poly(pct_budget, degree=3,raw=T)) 
summary(pcct_cb) # x^3無法解釋y值之顯著差異

anova(pcct_ln, pcct_qd)[2,6] # 二次沒比一次好
anova(pcct_qd, pcct_cb)[2,6] # 三次沒比二次好
anova(pcct_ln, pcct_cb)[2,6] # 三次沒比一次好

detach(cultural_data_southern)

# 只有x^1可解釋y值之顯著差異
# 故使用一次線性回歸

#####################################
## 圖X:pcct_offshore(xxx) ##########
#####################################

### R^2 = 0.74, 0.80, 0.84

### 檢驗回歸適配性

attach(cultural_data_offshore)

pcct_ln = lm(ctgp_per1wp~poly(pct_budget, degree=1,raw=T)) 
summary(pcct_ln) # x^1無法解釋y值之顯著差異(p = 0.06)
pcct_qd = lm(ctgp_per1wp~poly(pct_budget, degree=2,raw=T)) 
summary(pcct_qd) # x^2無法解釋y值之顯著差異
pcct_cb = lm(ctgp_per1wp~poly(pct_budget, degree=3,raw=T)) 
summary(pcct_cb) # x^3無法解釋y值之顯著差異

anova(pcct_ln, pcct_qd)[2,6] # 1>2
anova(pcct_qd, pcct_cb)[2,6] # 2>3
anova(pcct_ln, pcct_cb)[2,6] # 1>2>3

detach(cultural_data_offshore)

# x^1, x^2, x^3 都無法解釋y值之顯著差異
# 資料點太少故不放

################################
## 圖6E:pppt_northern ##########
################################

### R^2 = 0.52, 0.52, 0.57

### 檢驗回歸適配性 & Partial F test

attach(cultural_data_northern)

pppt_ln = lm(ptcp_rate~poly(pctc_per1wp, degree=1,raw=T)) 
summary(pppt_ln) # x^1可以解釋y值之顯著差異
pppt_qd = lm(ptcp_rate~poly(pctc_per1wp, degree=2,raw=T)) 
summary(pppt_qd) # x^2無法解釋y值之顯著差異
pppt_cb = lm(ptcp_rate~poly(pctc_per1wp, degree=3,raw=T)) 
summary(pppt_cb) # x^3可以解釋y值之顯著差異

# Partial F test 檢驗x^3是否比x^1更能解釋y值之顯著差異
anova(pppt_ln, pppt_qd)[2,6] # 1>2
anova(pppt_qd, pppt_cb)[2,6] # 3>2
anova(pppt_ln, pppt_cb)[2,6] # 1>3>2

detach(cultural_data_northern)

# x^1 & x^3 兩者皆可解釋y值之顯著差異
# 但Partial F test 檢驗發現x^1比x^3好
# 故使用一次線性回歸

##############################
## 圖6F:ppct_central##########
##############################

### R^2 = 0.48, 0.54, 0.68

### 檢驗回歸適配性 & Partial F test

attach(cultural_data_central)

ppct_ln = lm(ctgp_per1wp~poly(pctc_per1wp, degree=1,raw=T)) 
summary(ppct_ln) # x^1可以解釋y值之顯著差異
ppct_qd = lm(ctgp_per1wp~poly(pctc_per1wp, degree=2,raw=T)) 
summary(ppct_qd) # x^2沒有比x^1更能y值之顯著差異
ppct_cb = lm(ctgp_per1wp~poly(pctc_per1wp, degree=3,raw=T)) 
summary(ppct_cb) # x^3比x^1和x^2更能解釋y值之顯著差異

# Partial F test 檢驗x^3是否比x^1更能解釋y值之顯著差異
anova(ppct_ln, ppct_qd)[2,6] # 二次沒有比一次好
anova(ppct_qd, ppct_cb)[2,6] # 三次比二次好
anova(ppct_ln, ppct_cb)[2,6] # 三次比一次好 故 3>1>2

detach(cultural_data_central)

# 綜合上述結果，決定選擇三次回歸作圖

####################################
## Status組：尋找最適合之回歸模型 ##
####################################

cultural_data_muni <- cultural_data_withNA %>% 
  filter(status == "municipality")

cultural_data_county <- cultural_data_withNA %>% 
  filter(status == "county")

cultural_data_city <- cultural_data_withNA %>% 
  filter(status == "city")

##############################
## 圖7A:pcpt_muni ############
##############################

### R^2 = 0.39, 0.39, 0.40

### 檢驗回歸適配性

attach(cultural_data_muni)

pcpt_ln = lm(ptcp_rate~poly(pct_budget, degree=1,raw=T)) 
summary(pcpt_ln) # x^1可以解釋y值之顯著差異
pcpt_qd = lm(ptcp_rate~poly(pct_budget, degree=2,raw=T)) 
summary(pcpt_qd) # x^2無法解釋y值之顯著差異
pcpt_cb = lm(ptcp_rate~poly(pct_budget, degree=3,raw=T)) 
summary(pcpt_cb) # x^3無法解釋y值之顯著差異

anova(pcpt_ln, pcpt_qd)[2,6] # 2沒有比1好
anova(pcpt_qd, pcpt_cb)[2,6] # 3沒有比2好
anova(pcpt_ln, pcpt_cb)[2,6] # 3沒有比1好

detach(cultural_data_muni)

# 只有x^1可解釋y之顯著差異 故選用一次線性回歸

############################
## 圖7B:pcct_city ##########
############################

### R^2 = 0.38, 0.38, 0.39

### 檢驗回歸適配性

attach(cultural_data_city)

pcct_ln = lm(ctgp_per1wp~poly(pct_budget, degree=1,raw=T)) 
summary(pcct_ln) # x^1無法解釋y值之顯著差異(p = 0.06)
pcct_qd = lm(ctgp_per1wp~poly(pct_budget, degree=2,raw=T)) 
summary(pcct_qd) # x^2無法解釋y值之顯著差異
pcct_cb = lm(ctgp_per1wp~poly(pct_budget, degree=3,raw=T)) 
summary(pcct_cb) # x^3無法解釋y值之顯著差異

anova(pcct_ln, pcct_qd)[2,6] # 2沒有比1好
anova(pcct_qd, pcct_cb)[2,6] # 3沒有比2好
anova(pcct_ln, pcct_cb)[2,6] # 3沒有比1好

detach(cultural_data_city)

# 選用一次線性回歸

##############################
## 圖7C:pppt_muni ############
##############################

### R^2 = 0.69, 0.69, 0.71

### 檢驗回歸適配性

attach(cultural_data_muni)

pppt_ln = lm(ptcp_rate~poly(pctc_per1wp, degree=1,raw=T)) 
summary(pppt_ln) # x^1可以解釋y值之顯著差異
pppt_qd = lm(ptcp_rate~poly(pctc_per1wp, degree=2,raw=T)) 
summary(pppt_qd) # x^2無法解釋y值之顯著差異
pppt_cb = lm(ptcp_rate~poly(pctc_per1wp, degree=3,raw=T)) 
summary(pppt_cb) # x^3無法解釋y值之顯著差異

anova(pppt_ln, pppt_qd)[2,6] # 2沒有比1好
anova(pppt_qd, pppt_cb)[2,6] # 3沒有比2好
anova(pppt_ln, pppt_cb)[2,6] # 3沒有比1好

detach(cultural_data_muni)

# 選用一次線性回歸

##############################
## 圖7D:ppct_county ##########
##############################

### R^2 = 0.51, 0.57, 0.61

### 檢驗回歸適配性

attach(cultural_data_county)

ppct_ln = lm(ctgp_per1wp~poly(pctc_per1wp, degree=1,raw=T)) 
summary(ppct_ln) # x^1可以解釋y值之顯著差異
ppct_qd = lm(ctgp_per1wp~poly(pctc_per1wp, degree=2,raw=T)) 
summary(ppct_qd) # x^2可以y值之顯著差異
ppct_cb = lm(ctgp_per1wp~poly(pctc_per1wp, degree=3,raw=T)) 
summary(ppct_cb) # x^3也可以解釋y值之顯著差異

# Partial F test 檢驗x^3是否比x^1更能解釋y值之顯著差異
anova(ppct_ln, ppct_qd)[2,6] # 二次比一次好
anova(ppct_qd, ppct_cb)[2,6] # 三次比二次好
anova(ppct_ln, ppct_cb)[2,6] # 三次比一次好 故 3>2>1

detach(cultural_data_county)

# 綜合適配性檢驗與partial F test結果，選用三次回歸模型