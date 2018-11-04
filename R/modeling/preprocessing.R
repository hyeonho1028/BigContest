library(dplyr)
library(boot)
library(caret)
library(leaps)
library(glmnet)


dat = read.csv('C:/workspace/Github/big_contest/data/final/dat.csv', stringsAsFactors = F)
predict_dat = read.csv('C:/workspace/Github/big_contest/data/final/predict.csv', stringsAsFactors = F)


dat = dat[dat$genre != '서부극(웨스턴)' & dat$genre != '공연' & dat$genre != '뮤지컬' & dat$genre != '다큐멘터리',]
dat$country[dat$country != '미국' & dat$country != '한국'] = '제3국'

dat$genre[dat$genre ==  '가족' | dat$genre ==  '공포(호러)' | dat$genre ==  '애니메이션'] = '장르1'
dat$genre[dat$genre ==  'SF' | dat$genre ==  '드라마' | dat$genre ==  '멜로/로맨스' | dat$genre == '미스터리' |
            dat$genre == '스릴러' | dat$genre == '코미디'] = '장르2'
dat$genre[dat$genre ==  '범죄' | dat$genre ==  '사극' | dat$genre ==  '액션' | dat$genre ==  '어드벤처' |
            dat$genre ==  '전쟁' | dat$genre ==  '판타지'] = '장르3'

dat = dat %>% group_by(director) %>% mutate(director_score = mean(audience))
dat = dat %>% transform(director_score_rev = ifelse(director_score >= 1000000, 1, 0))
dat = dat %>% group_by(producer) %>% mutate(producer_score = mean(audience))
dat = dat %>% transform(producer_score_rev = ifelse(producer_score >= 2500000, 1, 0))
dat = dat %>% group_by(Distributor) %>% mutate(Distributor_score = mean(audience))
dat = dat %>% transform(Distributor_score_rev = ifelse(Distributor_score >= 1500000, 1, 0))
dat = dat %>% transform(Importer_score = ifelse(ifelse(dat$Importer!='',1,0),0,1))
dat = dat %>% group_by(Distributor) %>% mutate(Distributor_score = mean(audience))
dat = dat %>% group_by(actor1) %>% mutate(actor1_score = mean(audience))
dat = dat %>% group_by(actor2) %>% mutate(actor2_score = mean(audience))
dat = dat %>% group_by(actor3) %>% mutate(actor3_score = mean(audience))
dat = dat %>% transform(star1 = ifelse(star >= 8, 1, 0),
                        star2 = ifelse(star < 8 & star >=6, 1, 0),
                        star3 = ifelse(star < 6, 1, 0))
dat = dat %>% transform(season1 = ifelse(month %in% c(1,7), 1, 0),
                        season2 = ifelse(month %in% c(2,3,8,10,11,12), 1, 0),
                        season3 = ifelse(month %in% c(4,5,6,9), 1, 0))

dat = dat %>% select(-c(name, director, producer, Importer, Distributor, date, actor1, actor2, actor3,
                        star, director_score, producer_score, Distributor_score))
dat$year <- as.factor(dat$year)
dat$country <- as.factor(dat$country)
dat$genre <- as.factor(dat$genre)
dat$limit <- as.factor(dat$limit)


predict_dat = predict_dat[predict_dat$genre != '서부극(웨스턴)' & predict_dat$genre != '공연' & 
                            predict_dat$genre != '뮤지컬' & predict_dat$genre != '다큐멘터리',]
predict_dat$country[predict_dat$country != '미국' & predict_dat$country != '한국'] = '제3국'

predict_dat$genre[predict_dat$genre ==  '가족' | predict_dat$genre ==  '공포(호러)' | 
                    predict_dat$genre ==  '애니메이션'] = '장르1'
predict_dat$genre[predict_dat$genre ==  'SF' | predict_dat$genre ==  '드라마' | 
                    predict_dat$genre ==  '멜로/로맨스' | predict_dat$genre == '미스터리' |
                    predict_dat$genre == '스릴러' | predict_dat$genre == '코미디'] = '장르2'
predict_dat$genre[predict_dat$genre ==  '범죄' | predict_dat$genre ==  '사극' | predict_dat$genre ==  '액션' | 
                    predict_dat$genre ==  '어드벤처' | predict_dat$genre ==  '전쟁' |
                    predict_dat$genre ==  '판타지'] = '장르3'
predict_dat = predict_dat %>% transform(season1 = ifelse(month %in% c(1,7), 1, 0),
                                        season2 = ifelse(month %in% c(2,3,8,10,11,12), 1, 0),
                                        season3 = ifelse(month %in% c(4,5,6,9), 1, 0))

predict_dat = predict_dat %>% select(-c(name, director, producer, Importer, Distributor, 
                                        date, actor1, actor2, actor3, star))
predict_dat$year <- as.factor(predict_dat$year)
predict_dat$country <- as.factor(predict_dat$country)
predict_dat$genre <- as.factor(predict_dat$genre)
predict_dat$limit <- as.factor(predict_dat$limit)