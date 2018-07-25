#Time Series Analysis

library(here)
library(tidyverse)
library(tidytext)
library(lubridate)

dataset<-read_csv(here::here("data", "Building_Violations.csv"))

dataset<-dataset %>% mutate(V_Date = lubridate::mdy(`VIOLATION DATE`))
dataset<-dataset %>% mutate(V_Year = lubridate::year(V_Date))
dataset<-dataset %>% mutate(V_Month = lubridate::month(V_Date))
dataset<-dataset %>% mutate(V_Month_Date = paste(V_Month, V_Year, sep="-"))

pests <- dataset %>% filter(`VIOLATION CODE` == "CN134016" |
                              `VIOLATION CODE` == "CN135016"|
                              `VIOLATION CODE` == "CN136016"|
                              `VIOLATION CODE` == "CN136026")

pests<-pests %>% mutate(Violation = recode(`VIOLATION CODE`, "CN134016" = "Rats", "CN135016" = "Mice and Rodents", "CN136016" = "Roaches", "CN136026" = "Insects"))

pests_month<-pests %>% group_by(V_Year, V_Month, Violation) %>% summarise(count=n())
pests_month<-pests_month %>% mutate(V_Month_Year = as_date(paste(V_Year, V_Month, "01", sep="-")))

tidy_insects_base<-dataset %>%  filter(`VIOLATION CODE` == "CN136026") %>% select(ID, `VIOLATION INSPECTOR COMMENTS`) %>% unnest_tokens(bigram, `VIOLATION INSPECTOR COMMENTS`, token = "ngrams", n=2)
tidy_insects_1_separated <- tidy_insects_base %>%
  separate(bigram, c("word1", "word2"), sep = " ")
tidy_insects_1_separated$match1<- ifelse(tidy_insects_1_separated$word1 == "bed" & tidy_insects_1_separated$word2 == "bug", 1, 0)
tidy_insects_1_separated$match2<- ifelse(tidy_insects_1_separated$word1 == "bed" & tidy_insects_1_separated$word2 == "bugs", 1, 0)
tidy_insects_1_separated$match3<- ifelse(tidy_insects_1_separated$word1 == "bedbug", 1, 0)
tidy_insects_1_separated$match4<- ifelse(tidy_insects_1_separated$word1 == "bedbugs", 1, 0)
tidy_insects_1_separated$finalmatch<- ifelse(tidy_insects_1_separated$match1 == 1 | tidy_insects_1_separated$match2 == 1 | tidy_insects_1_separated$match3 == 1 | tidy_insects_1_separated$match4 == 1, 1, 0)
tidy_insects_test<-tidy_insects_1_separated %>% group_by(ID) %>% summarise(count = sum(finalmatch))
tidy_insects_test$bb<-ifelse(tidy_insects_test$count > 1, 1, 0)
sum(tidy_insects_test$bb, na.rm=TRUE)
pests<-left_join(pests, tidy_insects_test, by="ID")
sum(pests$bb, na.rm=TRUE)

insects <- pests %>% filter(`VIOLATION CODE` == "CN136026")
insects$bb[is.na(insects$bb)]<-0

insects_month<-insects %>% group_by(V_Year, V_Month, bb) %>% summarise(count=n())
insects_month<-insects_month %>% mutate(V_Month_Year = as_date(paste(V_Year, V_Month, "01", sep="-")))
bb_month<-insects_month %>% ungroup() %>% filter(bb == 1, V_Year != 2008, V_Year != 2009) %>% select(V_Month_Year, count)
bb_month<-bb_month %>% complete(V_Month_Year = seq.Date(min(V_Month_Year), max(V_Month_Year), by="month"), fill=list(count = 0))
bb_month$V_Month_Year<-as.character(bb_month$V_Month_Year)
#bb_month %>% filter(V_Year != 2008, V_Year != 2009) %>% group_by(V_Year) %>% summarise(violations = sum(count))

# library(xts)
# bb_ts <- xts(x = bb_month, order.by = bb_month$V_Month_Year)

min(bb_month$V_Month_Year)
max(bb_month$V_Month_Year)
bb_month<-bb_month %>% select(count)
bb_ts = ts(bb_month, start=c(2010, 3), end=c(2018, 7), frequency = 12)
bb_ts
decompose_bb<- decompose(bb_ts, "additive")

plot(as.ts(decompose_bb$seasonal))
plot(as.ts(decompose_bb$trend))
plot(as.ts(decompose_bb$random))
plot(decompose_bb)

decompose_bb_m<- decompose(bb_ts, "multiplicative")

plot(as.ts(decompose_bb_m$seasonal))
plot(as.ts(decompose_bb_m$trend))
plot(as.ts(decompose_bb_m$random))
plot(decompose_bb_m)
