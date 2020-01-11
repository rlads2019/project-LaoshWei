
####################################
##特定縣市歷年資料觀察各指標之趨勢##
####################################

#載入需要之套件：

library(tidyverse)
library(ggplot2)

##非直轄市文化排名倒數三名以及第一名(依序)：金門縣、屏東縣、嘉義縣、新竹市

#讀入金門縣歷年資料各指標

Kinmen_county_gov <- gov_participate %>%
  filter(項目 == "金門縣") 
Kinmen_county_people <- people_participate %>%
  filter(項目 == "金門縣")
Kinmen_county <- cbind(Kinmen_county_gov, Kinmen_county_people[, 3:5])

#繪出並儲存圖檔

png("金門縣歷年文化指標.png", width = 638, height = 600)
ggplot(Kinmen_county) +
  geom_bar(mapping = aes(x = 年度, y = 小時總數/100, fill =  "小時總數"), stat = "identity") +
  geom_line(mapping = aes(x = 年度, y = 文化局占地方政府總預算比率, colour = "文化局占地方政府總預算比率")) +
  geom_line(mapping = aes(x = 年度, y = 每萬人均場地數, colour =  "每萬人均場地數")) +
  geom_line(mapping = aes(x = 年度, y = 每千人均藝文活動參與次數, colour = "每千人均藝文活動參與次數")) +
  geom_line(mapping = aes(x = 年度, y = 人均文創產業營業額, colour = "人均文創產業營業額")) +
  geom_line(mapping = aes(x = 年度, y = 每萬人均民間文化組織, colour = "每萬人均民間文化組織")) +
  ylab("") +
  scale_y_continuous(name = "",limits = c(0, 7) , breaks = seq(1,6), sec.axis = sec_axis(~. *100, name = "小時總數", breaks = seq(100,435,100))) +
  scale_fill_manual("", values = c("小時總數"="yellow")) +
  scale_colour_manual("", values = c("文化局占地方政府總預算比率"="red", "每萬人均場地數"="orange", "每千人均藝文活動參與次數"="blue", "人均文創產業營業額"="skyblue", "每萬人均民間文化組織"="navy")) +
  theme(legend.text = element_text(size = 12))
dev.off()

#註：教育課程小時總數因數值普遍較其他指標大，故用長條圖及另一y軸表示

#讀入屏東歷年資料各指標

Pingtung_county_gov <- gov_participate %>%
  filter(項目 == "屏東縣") 
Pingtung_county_people <- people_participate %>%
  filter(項目 == "屏東縣")
Pingtung_county <- cbind(Pingtung_county_gov, Pingtung_county_people[, 3:5])

#繪出並儲存圖檔

png("屏東縣歷年文化指標.png", width = 638, height = 600)
ggplot(Pingtung_county) +
  geom_bar(mapping = aes(x = 年度, y = 小時總數/1900, fill =  "小時總數"), stat = "identity") +
  geom_line(mapping = aes(x = 年度, y = 文化局占地方政府總預算比率, colour = "文化局占地方政府總預算比率")) +
  geom_line(mapping = aes(x = 年度, y = 每萬人均場地數, colour =  "每萬人均場地數")) +
  geom_line(mapping = aes(x = 年度, y = 每千人均藝文活動參與次數, colour = "每千人均藝文活動參與次數")) +
  geom_line(mapping = aes(x = 年度, y = 人均文創產業營業額, colour = "人均文創產業營業額")) +
  geom_line(mapping = aes(x = 年度, y = 每萬人均民間文化組織, colour = "每萬人均民間文化組織")) +
  ylab("") +
  scale_y_continuous(name = "",limits = c(0, 6) , breaks = seq(1,6), sec.axis = sec_axis(~. *1900, name = "小時總數", breaks = seq(1000,11000,2000))) +
  scale_fill_manual("", values = c("小時總數"="yellow")) +
  scale_colour_manual("", values = c("文化局占地方政府總預算比率"="red", "每萬人均場地數"="orange", "每千人均藝文活動參與次數"="blue", "人均文創產業營業額"="skyblue", "每萬人均民間文化組織"="navy")) +
  theme(legend.text = element_text(size = 12))
dev.off()

#讀入嘉義縣歷年資料各指標

Chiayi_county_gov <- gov_participate %>%
  filter(項目 == "嘉義縣") 
Chiayi_county_people <- people_participate %>%
  filter(項目 == "嘉義縣")
Chiayi_county <- cbind(Chiayi_county_gov, Chiayi_county_people[, 3:5])

#繪出並儲存圖檔

png("嘉義縣歷年文化指標.png", width = 638, height = 600)
ggplot(Chiayi_county) +
  geom_bar(mapping = aes(x = 年度, y = 小時總數/76, fill =  "小時總數"), stat = "identity") +
  geom_line(mapping = aes(x = 年度, y = 文化局占地方政府總預算比率, colour = "文化局占地方政府總預算比率")) +
  geom_line(mapping = aes(x = 年度, y = 每萬人均場地數, colour =  "每萬人均場地數")) +
  geom_line(mapping = aes(x = 年度, y = 每千人均藝文活動參與次數, colour = "每千人均藝文活動參與次數")) +
  geom_line(mapping = aes(x = 年度, y = 人均文創產業營業額, colour = "人均文創產業營業額")) +
  geom_line(mapping = aes(x = 年度, y = 每萬人均民間文化組織, colour = "每萬人均民間文化組織")) +
  ylab("") +
  scale_y_continuous(name = "",limits = c(0, 9) , breaks = seq(1,9), sec.axis = sec_axis(~. *76, name = "小時總數", breaks = seq(100,700,100))) +
  scale_fill_manual("", values = c("小時總數"="yellow")) +
  scale_colour_manual("", values = c("文化局占地方政府總預算比率"="red", "每萬人均場地數"="orange", "每千人均藝文活動參與次數"="blue", "人均文創產業營業額"="skyblue", "每萬人均民間文化組織"="navy")) +
  theme(legend.text = element_text(size = 12))
dev.off()

#讀入新竹市歷年資料各指標

Hsinchu_county_gov <- gov_participate %>%
  filter(項目 == "新竹市") 
Hsinchu_county_people <- people_participate %>%
  filter(項目 == "新竹市")
Hsinchu_county <- cbind(Hsinchu_county_gov, Hsinchu_county_people[, 3:5])

#繪出並儲存圖檔

png("新竹市歷年文化指標.png", width = 638, height = 600)
ggplot(Hsinchu_county) +
  geom_bar(mapping = aes(x = 年度, y = 小時總數/85, fill =  "小時總數"), stat = "identity") +
  geom_line(mapping = aes(x = 年度, y = 文化局占地方政府總預算比率, colour = "文化局占地方政府總預算比率")) +
  geom_line(mapping = aes(x = 年度, y = 每萬人均場地數, colour =  "每萬人均場地數")) +
  geom_line(mapping = aes(x = 年度, y = 每千人均藝文活動參與次數, colour = "每千人均藝文活動參與次數")) +
  geom_line(mapping = aes(x = 年度, y = 人均文創產業營業額, colour = "人均文創產業營業額")) +
  geom_line(mapping = aes(x = 年度, y = 每萬人均民間文化組織, colour = "每萬人均民間文化組織")) +
  ylab("") +
  scale_y_continuous(name = "",limits = c(0, 32) , breaks = seq(0,32,4), sec.axis = sec_axis(~. *85, name = "小時總數", breaks = seq(500,2500,500))) +
  scale_fill_manual("", values = c("小時總數"="yellow")) +
  scale_colour_manual("", values = c("文化局占地方政府總預算比率"="red", "每萬人均場地數"="orange", "每千人均藝文活動參與次數"="blue", "人均文創產業營業額"="skyblue", "每萬人均民間文化組織"="navy")) +
  theme(legend.text = element_text(size = 12))
dev.off()

####################################################

##直轄市文化排名倒數三名以及第一名(依序)：臺中市、桃園市、新北市、高雄市

#讀入臺中市歷年資料各指標

Taichung_county_gov <- gov_participate %>%
  filter(項目 == "臺中市") 
Taichung_county_people <- people_participate %>%
  filter(項目 == "台中市")
Taichung_county <- cbind(Taichung_county_gov, Taichung_county_people[, 3:5])

#繪出並儲存圖檔

png("臺中市歷年文化指標.png", width = 638, height = 600)
ggplot(Taichung_county) +
  geom_bar(mapping = aes(x = 年度, y = 小時總數/152, fill =  "小時總數"), stat = "identity") +
  geom_line(mapping = aes(x = 年度, y = 文化局占地方政府總預算比率, colour = "文化局占地方政府總預算比率")) +
  geom_line(mapping = aes(x = 年度, y = 每萬人均場地數, colour =  "每萬人均場地數")) +
  geom_line(mapping = aes(x = 年度, y = 每千人均藝文活動參與次數, colour = "每千人均藝文活動參與次數")) +
  geom_line(mapping = aes(x = 年度, y = 人均文創產業營業額, colour = "人均文創產業營業額")) +
  geom_line(mapping = aes(x = 年度, y = 每萬人均民間文化組織, colour = "每萬人均民間文化組織")) +
  ylab("") +
  scale_y_continuous(name = "",limits = c(0, 22) , breaks = seq(0,22,2), sec.axis = sec_axis(~. *152, name = "小時總數", breaks = seq(500,3500,500))) +
  scale_fill_manual("", values = c("小時總數"="yellow")) +
  scale_colour_manual("", values = c("文化局占地方政府總預算比率"="red", "每萬人均場地數"="orange", "每千人均藝文活動參與次數"="blue", "人均文創產業營業額"="skyblue", "每萬人均民間文化組織"="navy")) +
  theme(legend.text = element_text(size = 12))
dev.off()

#讀入桃園市歷年資料各指標

Taoyuan_county_gov <- gov_participate %>%
  filter(項目 == "桃園巿") 
Taoyuan_county_people <- people_participate %>%
  filter(項目 == "桃園市")
Taoyuan_county <- cbind(Taoyuan_county_gov, Taoyuan_county_people[, 3:5])

#繪出並儲存圖檔

png("桃園市歷年文化指標.png", width = 638, height = 600)
ggplot(Taoyuan_county) +
  geom_bar(mapping = aes(x = 年度, y = 小時總數/140, fill =  "小時總數"), stat = "identity") +
  geom_line(mapping = aes(x = 年度, y = 文化局占地方政府總預算比率, colour = "文化局占地方政府總預算比率")) +
  geom_line(mapping = aes(x = 年度, y = 每萬人均場地數, colour =  "每萬人均場地數")) +
  geom_line(mapping = aes(x = 年度, y = 每千人均藝文活動參與次數, colour = "每千人均藝文活動參與次數")) +
  geom_line(mapping = aes(x = 年度, y = 人均文創產業營業額, colour = "人均文創產業營業額")) +
  geom_line(mapping = aes(x = 年度, y = 每萬人均民間文化組織, colour = "每萬人均民間文化組織")) +
  ylab("") +
  scale_y_continuous(name = "",limits = c(0, 22) , breaks = seq(0,22,2), sec.axis = sec_axis(~. *140, name = "小時總數", breaks = seq(400,3200,400))) +
  scale_fill_manual("", values = c("小時總數"="yellow")) +
  scale_colour_manual("", values = c("文化局占地方政府總預算比率"="red", "每萬人均場地數"="orange", "每千人均藝文活動參與次數"="blue", "人均文創產業營業額"="skyblue", "每萬人均民間文化組織"="navy")) +
  theme(legend.text = element_text(size = 12))
dev.off()

#讀入新北市歷年資料各指標

NewTaipei_county_gov <- gov_participate %>%
  filter(項目 == "新北巿") 
NewTaipei_county_people <- people_participate %>%
  filter(項目 == "新北市")
NewTaipei_county <- cbind(NewTaipei_county_gov, NewTaipei_county_people[, 3:5])

#繪出並儲存圖檔

png("新北市歷年文化指標.png", width = 638, height = 600)
ggplot(NewTaipei_county) +
  geom_bar(mapping = aes(x = 年度, y = 小時總數/324, fill =  "小時總數"), stat = "identity") +
  geom_line(mapping = aes(x = 年度, y = 文化局占地方政府總預算比率, colour = "文化局占地方政府總預算比率")) +
  geom_line(mapping = aes(x = 年度, y = 每萬人均場地數, colour =  "每萬人均場地數")) +
  geom_line(mapping = aes(x = 年度, y = 每千人均藝文活動參與次數, colour = "每千人均藝文活動參與次數")) +
  geom_line(mapping = aes(x = 年度, y = 人均文創產業營業額, colour = "人均文創產業營業額")) +
  geom_line(mapping = aes(x = 年度, y = 每萬人均民間文化組織, colour = "每萬人均民間文化組織")) +
  ylab("") +
  scale_y_continuous(name = "",limits = c(0, 32) , breaks = seq(0,32,4), sec.axis = sec_axis(~. *324, name = "小時總數", breaks = seq(2000,12000,2000))) +
  scale_fill_manual("", values = c("小時總數"="yellow")) +
  scale_colour_manual("", values = c("文化局占地方政府總預算比率"="red", "每萬人均場地數"="orange", "每千人均藝文活動參與次數"="blue", "人均文創產業營業額"="skyblue", "每萬人均民間文化組織"="navy")) +
  theme(legend.text = element_text(size = 12))
dev.off()

#讀入高雄市歷年資料各指標

Kaohsiung_county_gov <- gov_participate %>%
  filter(項目 == "高雄市") 
Kaohsiung_county_people <- people_participate %>%
  filter(項目 == "高雄市")
Kaohsiung_county <- cbind(Kaohsiung_county_gov, Kaohsiung_county_people[, 3:5])

#繪出並儲存圖檔

png("高雄市歷年文化指標.png", width = 638, height = 600)
ggplot(Kaohsiung_county) +
  geom_bar(mapping = aes(x = 年度, y = 小時總數/88, fill =  "小時總數"), stat = "identity") +
  geom_line(mapping = aes(x = 年度, y = 文化局占地方政府總預算比率, colour = "文化局占地方政府總預算比率")) +
  geom_line(mapping = aes(x = 年度, y = 每萬人均場地數, colour =  "每萬人均場地數")) +
  geom_line(mapping = aes(x = 年度, y = 每千人均藝文活動參與次數, colour = "每千人均藝文活動參與次數")) +
  geom_line(mapping = aes(x = 年度, y = 人均文創產業營業額, colour = "人均文創產業營業額")) +
  geom_line(mapping = aes(x = 年度, y = 每萬人均民間文化組織, colour = "每萬人均民間文化組織")) +
  ylab("") +
  scale_y_continuous(name = "",limits = c(0, 86) , breaks = seq(0,90,10), sec.axis = sec_axis(~. *88, name = "小時總數", breaks = seq(500,7500,1000))) +
  scale_fill_manual("", values = c("小時總數"="yellow")) +
  scale_colour_manual("", values = c("文化局占地方政府總預算比率"="red", "每萬人均場地數"="orange", "每千人均藝文活動參與次數"="blue", "人均文創產業營業額"="skyblue", "每萬人均民間文化組織"="navy")) +
  theme(legend.text = element_text(size = 12))
dev.off()