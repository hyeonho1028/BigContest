dat = read.csv('C:/workspace/Github/big_contest/data/dat.csv', stringsAsFactors = FALSE)


dat$전국스크린수 = as.integer(dat$전국스크린수)
dat$전국관객수 = gsub(',', '', dat$전국관객수)
dat$전국관객수 = as.integer(dat$전국관객수)

dat = dat %>% filter(국적 == '한국' & 전국스크린수 >= 201) %>% 
  select(-c('순번', '전국매출액', '서울매출액', '서울관객수', '국적')) %>% group_by(감독) %>% 
  mutate(감독점수 = mean(전국관객수)) #%>% select(-감독)



par(mfrow = c(3,5))
hist(dat_score$전국관객수[dat_score$장르 == '판타지'], breaks = 10, prob = TRUE)
hist(dat_score$전국관객수[dat_score$장르 == '사극'], breaks = 10, prob = TRUE)
hist(dat_score$전국관객수[dat_score$장르 == '액션'], breaks = 10, prob = TRUE)
hist(dat_score$전국관객수[dat_score$장르 == '전쟁'], breaks = 10, prob = TRUE)
hist(dat_score$전국관객수[dat_score$장르 == 'SF'], breaks = 10, prob = TRUE)
hist(dat_score$전국관객수[dat_score$장르 == '범죄'], breaks = 10, prob = TRUE)
hist(dat_score$전국관객수[dat_score$장르 == '드라마'], breaks = 10, prob = TRUE)
hist(dat_score$전국관객수[dat_score$장르 == '코미디'], breaks = 10, prob = TRUE)
hist(dat_score$전국관객수[dat_score$장르 == '미스터리'], breaks = 10, prob = TRUE)
hist(dat_score$전국관객수[dat_score$장르 == '스릴러'], breaks = 10, prob = TRUE)
hist(dat_score$전국관객수[dat_score$장르 == '공포(호러)'], breaks = 10, prob = TRUE)
hist(dat_score$전국관객수[dat_score$장르 == '멜로/로맨스'], breaks = 10, prob = TRUE)
hist(dat_score$전국관객수[dat_score$장르 == '다큐멘터리'], breaks = 10, prob = TRUE)
hist(dat_score$전국관객수[dat_score$장르 == '가족'], breaks = 10, prob = TRUE)
hist(dat_score$전국관객수[dat_score$장르 == '애니메이션'], breaks = 10, prob = TRUE)



setwd('C:/workspace/Github/big_contest/data')
getwd()
