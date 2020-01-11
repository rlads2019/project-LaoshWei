########################
##去除極端值後讀入資料##
########################

#loading required packages

library(readr)
library(tidyverse)
library(ggplot2)

##########################
##列出各指標各縣市的排名##
##########################

#讀入政府投入資料並去除極大值
gov_participate <- read_csv("政府投入指標.csv", locale = locale(encoding = stringi::stri_enc_get()))

#極大值去除 小時總數 新竹縣 2012年 237132小時
#極大值去除 小時總數 嘉義縣 2014年 17681小時

##政府投入指標
### a_hour = 教育課程總時數
### a_ratio = 文化局占地方政府總預算比率
### a_place = 每萬人均場地數

##讀取各年度各縣市教育課程總時數

a_hour <- gov_participate %>%
  select(年度, 項目, 小時總數)

#非直轄市取歷年平均進行比較

a_hour_ranking <- a_hour %>%
  filter(項目 %in% c("新北巿","臺北市","桃園巿","臺中市","高雄市","臺南市") == F) %>%
  group_by(項目) %>%
  summarise("小時總數" = mean(小時總數, na.rm=T)) %>%
  arrange(desc(小時總數)) %>%
  mutate("排名" = 1:16) 

#直轄市取歷年平均進行比較

muni_a_hour_ranking <- a_hour %>%
  filter(項目 %in% c("新北巿","臺北市","桃園巿","臺中市","高雄市","臺南市") == T) %>%
  group_by(項目) %>%
  summarise("小時總數" = mean(小時總數, na.rm=T)) %>%
  arrange(desc(小時總數)) %>%
  mutate("排名" = 1:6)

##讀取各年度各縣市文化局占地方政府總預算比率

a_ratio <- gov_participate %>%
  select(年度, 項目, 文化局占地方政府總預算比率)

#非直轄市取歷年平均進行比較

a_ratio_ranking <- a_ratio %>%
  filter(項目 %in% c("新北巿","臺北市","桃園巿","臺中市","高雄市","臺南市") == F) %>%
  group_by(項目) %>%
  summarise("文化局占地方政府總預算比率" = mean(文化局占地方政府總預算比率))%>%
  arrange(desc(文化局占地方政府總預算比率)) %>%
  mutate("排名" = 1:16)

#直轄市取歷年平均進行比較

muni_a_ratio_ranking <- a_ratio %>%
  filter(項目 %in% c("新北巿","臺北市","桃園巿","臺中市","高雄市","臺南市") == T) %>%
  group_by(項目) %>%
  summarise("文化局占地方政府總預算比率" = mean(文化局占地方政府總預算比率))%>%
  arrange(desc(文化局占地方政府總預算比率)) %>%
  mutate("排名" = 1:6)

##讀取各年度各縣市每萬人均場地數

a_place <- gov_participate %>%
  select(年度, 項目, 每萬人均場地數)

#非直轄市取歷年平均進行比較

a_place_ranking <- a_place %>%
  filter(項目 %in% c("新北巿","臺北市","桃園巿","臺中市","高雄市","臺南市") == F) %>%
  group_by(項目) %>%
  summarise("每萬人均場地數" = mean(每萬人均場地數)) %>%
  arrange(desc(每萬人均場地數)) %>%
  mutate("排名" = 1:16)

#直轄市取歷年平均進行比較

muni_a_place_ranking <- a_place %>%
  filter(項目 %in% c("新北巿","臺北市","桃園巿","臺中市","高雄市","臺南市") == T) %>%
  group_by(項目) %>%
  summarise("每萬人均場地數" = mean(每萬人均場地數)) %>%
  arrange(desc(每萬人均場地數)) %>%
  mutate("排名" = 1:6)

#計算非直轄市之"政府投入指標"排名

a_hour_ranking_2 <- a_hour_ranking %>%
  select("項目","排名")
a_ratio_rakning_2 <- a_ratio_ranking %>%
  select("項目","排名")
a_place_rakning_2 <- a_place_ranking %>%
  select("項目","排名")
gov_participate_ranking <- rbind(a_hour_ranking_2, a_ratio_rakning_2, a_place_rakning_2) %>%
  group_by(項目) %>%
  summarise("排名總和" = sum(排名)) %>%
  arrange(desc(排名總和)); gov_participate_ranking

#計算直轄市之"政府投入指標"排名
muni_a_hour_ranking_2 <- muni_a_hour_ranking %>%
  select("項目","排名")
muni_a_ratio_rakning_2 <- muni_a_ratio_ranking %>%
  select("項目","排名")
muni_a_place_rakning_2 <- muni_a_place_ranking %>%
  select("項目","排名")
gov_participate_muni_ranking <- rbind(muni_a_hour_ranking_2, muni_a_ratio_rakning_2, muni_a_place_rakning_2) %>%
  group_by(項目) %>%
  summarise("排名總和" = sum(排名)) %>%
  arrange(desc(排名總和)); gov_participate_muni_ranking

#讀入民眾參與資料並去除極大值

people_participate <- read_csv("人民參與度指標.csv", locale = locale(encoding = stringi::stri_enc_get()))

##民眾參與指標
### a_act_parti = 每千人均藝文活動參與次數
### a_business = 人均文創產業營業額
### a_group = 每萬人均民間文化組織

##讀取各年度各縣市每千人均藝文活動參與次數

a_act_parti <- people_participate %>%
  select(年度, 項目, 每千人均藝文活動參與次數)

#非直轄市取歷年平均進行比較

a_act_parti_ranking <- a_act_parti %>%
  filter(項目 %in% c("新北市","台北市","桃園市","台中市","高雄市","台南市") == F) %>%
  group_by(項目) %>%
  summarise("每千人均藝文活動參與次數" = mean(每千人均藝文活動參與次數)) %>%
  arrange(desc(每千人均藝文活動參與次數)) %>%
  mutate("排名" = 1:16)

#直轄市取歷年平均進行比較
muni_a_act_parti_ranking <- a_act_parti %>%
  filter(項目 %in% c("新北市","台北市","桃園市","台中市","高雄市","台南市") == T) %>%
  group_by(項目) %>%
  summarise("每千人均藝文活動參與次數" = mean(每千人均藝文活動參與次數)) %>%
  arrange(desc(每千人均藝文活動參與次數)) %>%
  mutate("排名" = 1:6)

##讀取各年度各縣市人均文創產業營業額

a_business <- people_participate %>%
  select(年度, 項目, 人均文創產業營業額)

#非直轄市取歷年平均進行比較
a_business_ranking <- a_business %>%
  filter(項目 %in% c("新北市","台北市","桃園市","台中市","高雄市","台南市") == F) %>%
  group_by(項目) %>%
  summarise("人均文創產業營業額" = mean(人均文創產業營業額)) %>%
  arrange(desc(人均文創產業營業額)) %>%
  mutate("排名" = 1:16)

#直轄市取歷年平均進行比較
muni_a_business_ranking <- a_business %>%
  filter(項目 %in% c("新北市","台北市","桃園市","台中市","高雄市","台南市") == T) %>%
  group_by(項目) %>%
  summarise("人均文創產業營業額" = mean(人均文創產業營業額)) %>%
  arrange(desc(人均文創產業營業額)) %>%
  mutate("排名" = 1:6)

##讀取各年度各縣市每萬人均民間文化組織

a_group <- people_participate %>%
  select(年度, 項目, 每萬人均民間文化組織)

#非直轄市取歷年平均進行比較
a_group_ranking <- a_group %>%
  filter(項目 %in% c("新北市","台北市","桃園市","台中市","高雄市","台南市") == F) %>%
  group_by(項目) %>%
  summarise("每萬人均民間文化組織" = mean(每萬人均民間文化組織)) %>%
  arrange(desc(每萬人均民間文化組織)) %>%
  mutate("排名" = 1:16)

#直轄市取歷年平均進行比較
muni_a_group_ranking <- a_group %>%
  filter(項目 %in% c("新北市","台北市","桃園市","台中市","高雄市","台南市") == T) %>%
  group_by(項目) %>%
  summarise("每萬人均民間文化組織" = mean(每萬人均民間文化組織)) %>%
  arrange(desc(每萬人均民間文化組織)) %>%
  mutate("排名" = 1:6)

#計算非直轄市之"人民參與度指標"排名
a_act_parti_ranking_2 <- a_act_parti_ranking %>%
  select("項目","排名")
a_business_rakning_2 <- a_business_ranking %>%
  select("項目","排名")
a_group_rakning_2 <- a_group_ranking %>%
  select("項目","排名")
people_participate_ranking <-rbind(a_act_parti_ranking_2, a_business_rakning_2, a_group_rakning_2) %>%
  group_by(項目) %>%
  summarise("排名總和" = sum(排名)) %>%
  arrange(desc(排名總和)); people_participate_ranking

#計算直轄市之"人民參與度指標"排名
muni_a_act_parti_ranking_2 <- muni_a_act_parti_ranking %>%
  select("項目","排名")
muni_a_business_rakning_2 <- muni_a_business_ranking %>%
  select("項目","排名")
muni_a_group_rakning_2 <- muni_a_group_ranking %>%
  select("項目","排名")
people_participate_muni_ranking <- rbind(muni_a_act_parti_ranking_2, muni_a_business_rakning_2, muni_a_group_rakning_2) %>%
  group_by(項目) %>%
  summarise("排名總和" = sum(排名)) %>%
  arrange(desc(排名總和)); people_participate_muni_ranking

##合併指標排名成文化程度總排名

#整合文字格式
people_participate_ranking$項目[people_participate_ranking$項目 == "台東縣"] <- "臺東縣"
people_participate_muni_ranking$項目[people_participate_muni_ranking$項目 == "台中市"] <- "臺中市"
people_participate_muni_ranking$項目[people_participate_muni_ranking$項目 == "台南市"] <- "臺南市"
people_participate_muni_ranking$項目[people_participate_muni_ranking$項目 == "台北市"] <- "臺北市"
gov_participate_muni_ranking$項目[gov_participate_muni_ranking$項目 == "桃園巿"] <- "桃園市"
gov_participate_muni_ranking$項目[gov_participate_muni_ranking$項目 == "新北巿"] <- "新北市"

#計算非直轄市之文化總排名
rbind(gov_participate_ranking, people_participate_ranking) %>%
  group_by(項目) %>%
  summarise("排名總和" = sum(排名總和)) %>%
  arrange(desc(排名總和))

#計算直轄市之文化總排名
rbind(gov_participate_muni_ranking, people_participate_muni_ranking) %>%
  group_by(項目) %>%
  summarise("排名總和" = sum(排名總和)) %>%
  arrange(desc(排名總和))