
##############################################
## 完成回歸適配性檢測與選取後之圖6與圖7作圖 ##
##############################################

formula_1 <- y ~ poly(x, 1, raw = TRUE)
formula_2 <- y ~ poly(x, 2, raw = TRUE)
formula_3 <- y ~ poly(x, 3, raw = TRUE)


# 以縣市地理分區劃分

# Fig.6A 南部地區文化活動參與率與培訓總時數之關係

(trpt_southern_ln <- 
  ggplot(cultural_data_southern,
         aes(x= training_hr, y= ptcp_rate))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "orange", 
              formula = formula_1, se = F) +
  stat_poly_eq(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
             formula = formula_1, parse = TRUE) +
  labs(title = "Southern TW"))

# Fig.6B 南部地區人均文化消費與培訓總時數之關係

(trcs_southern_ln <- 
  ggplot(cultural_data_southern, 
         aes(x= training_hr, y= cspt_ntd)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "orange",
              formula = formula_1, se = F) +
  stat_poly_eq(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
               formula = formula_1, parse = TRUE) +
  labs(title = "Southern TW"))
  
# Fig.6C 中部地區文化團體數與文化預算占比之關係

(pcct_central_ln <- 
  ggplot(cultural_data_central, 
         aes(x= pct_budget, y= ctgp_per1wp))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "orange",
              formula = formula_1, se = F) +
  stat_poly_eq(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
               formula = formula_1, parse = TRUE) +
  labs(title = "Central TW"))
  
# Fig.6D 南部地區文化團體數與文化預算占比之關係
  
(pcct_southern_ln <- 
  ggplot(cultural_data_southern,
         aes(x= pct_budget, y= ctgp_per1wp))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "orange", 
              formula = formula_1, se = F) +
  stat_poly_eq(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
               formula = formula_1, parse = TRUE) +
  labs(title = "Southern TW"))  
  
# Fig.6E 北部地區文化參與率與人均文化場地數之關係
  
(pppt_northern_ln <- 
  ggplot(cultural_data_northern,
         aes(x= pctc_per1wp, y= ptcp_rate))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "orange", 
              formula = formula_1, se = F) +
  stat_poly_eq(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
               formula = formula_1, parse = TRUE) +
  labs(title = "Northern TW"))

# Fig.6F 中部地區文化團體數與人均文化場地數之關係

(ppct_central_cb <- 
  ggplot(cultural_data_central, 
         aes(x= pctc_per1wp, y= ctgp_per1wp))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "palevioletred1",
              formula = formula_3, se = F) +
  stat_poly_eq(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
               formula = formula_3, parse = TRUE) +
  labs(title = "Central TW"))

###################################################### 

# 以縣市行政層級區分

# Fig.7A 直轄市之文化活動參與率與文化預算占比之關係
  
(pcpt_muni_ln <- 
  ggplot(cultural_data_muni,
         aes(x= pct_budget, y= ptcp_rate))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "orange", 
              formula = formula_1, se = F) +
  stat_poly_eq(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
              formula = formula_1, parse = TRUE) +
  labs(title = "Municipalities in TW"))  
      
# Fig.7B 一般市之人均文化團體數與文化預算占比之關係
  
(pcct_city_ln <- 
  ggplot(cultural_data_city,
         aes(x= pct_budget, y= ctgp_per1wp))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "orange", 
              formula = formula_1, se = F) +
  stat_poly_eq(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
               formula = formula_1, parse = TRUE) +
  labs(title = "Cities in TW")) 
  
# Fig.7C 直轄市之文化活動參與率與人均文化場地數之關係

(pppt_muni_ln <- 
  ggplot(cultural_data_muni,
         aes(x= pctc_per1wp, y= ptcp_rate))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "orange", 
              formula = formula_1, se = F) +
  stat_poly_eq(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
               formula = formula_1, parse = TRUE) +
  labs(title = "Municipalities in TW"))  

# Fig.7D 一般縣之人均文化團體數與人均文化場地數之關係

(ppct_county_cb <- 
  ggplot(cultural_data_county,
         aes(x= pctc_per1wp, y= ctgp_per1wp))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "palevioletred1", 
              formula = formula_3, se = F) +
  stat_poly_eq(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
               formula = formula_3, parse = TRUE) +
  labs(title = "Counties in TW")) 

  