git config --global user.name "Andrew Greenlee"
git config --global user.email "agreen4@illinois.edu"

git remote add origin https://github.com/agreen4/Chicago.git
git push -u origin master

install.packages("here", "tidyverse", "stringdist", "tidytext", "lubridate", "sp", "rgdal", "tigris")
library(here)
library(tidyverse)
#install.packages("stringdist")
library(stringdist)
library(tidytext)
library(lubridate)
library(sp)
library(rgdal)
library(tigris)
dataset<-read_csv(here::here("data", "Building_Violations.csv"))
#codes<-dataset %>% select(`VIOLATION CODE`, `VIOLATION DESCRIPTION`) %>% unique()
#codes %>% filter(`VIOLATION DESCRIPTION` == "ROACHES")
# test<-dataset %>% group_by(`VIOLATION CODE`, `VIOLATION DESCRIPTION`) %>% summarise(count = n())
# write_csv(test, here("output", "violation_types.csv"))

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


ggplot(pests_month, aes(x=V_Month_Year, y=count, group = Violation, colour = Violation))+
  geom_line()+
  scale_x_date(name = "Year", limits = c(as.Date("2010-01-01"), NA), date_labels = "%Y")+
  stat_smooth()+facet_grid(Violation ~ .)+theme_classic()+scale_y_continuous(name = "Violation Count", limits = c(0, 80))+
  theme(strip.background =element_blank(), legend.position = "none")

ggsave("F2.Pests.png", plot = last_plot(), device = "png", path = here::here("output"), scale = 1, units = "in", dpi = 300)

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
sum(tidy_insects_test$bb)
pests<-left_join(pests, tidy_insects_test, by="ID")
sum(pests$bb, na.rm=TRUE)

insects <- pests %>% filter(`VIOLATION CODE` == "CN136026")
insects$bb[is.na(insects$bb)]<-0

insects_month<-insects %>% group_by(V_Year, V_Month, bb) %>% summarise(count=n())
insects_month<-insects_month %>% mutate(V_Month_Year = as_date(paste(V_Year, V_Month, "01", sep="-")))

ggplot(insects_month, aes(x=V_Month_Year, y=count, group = bb, colour = bb))+
  geom_line()+
  scale_x_date(name = "Year", limits = c(as.Date("2010-01-01"), NA), date_labels = "%Y")+
  stat_smooth()+
  facet_grid(bb ~ ., labeller = labeller(bb = c("0" = "Not Bed Bugs", "1" = "Bed Bugs")))+
  theme_classic()+scale_y_continuous(name = "Violation Count", limits = c(0, 50))+
  theme(strip.background =element_blank(), legend.position = "none")

ggsave("F2.Insects.png", plot = last_plot(), device = "png", path = here::here("output"), scale = 1, units = "in", dpi = 300)


pests1<-pests %>% filter(V_Year == 2010 | V_Year == 2011 | V_Year == 2012 | V_Year == 2013 | V_Year == 2014 | V_Year == 2015 | V_Year == 2016 | V_Year == 2017 | V_Year==2018)

chicago_shp<-readOGR(here::here("data/Boundary", "chicago.shp"), layer = "chicago")

chicago<-fortify(chicago_shp, region = "objectid")

ggplot()+geom_polygon(data = chicago, aes(x=long, y=lat, group = group), fill = NA, color = "grey70")+
  geom_point(data=pests1, aes(x=LONGITUDE, y=LATITUDE, colour = Violation, alpha = .1), size = .075)+
  theme_classic()+
  facet_grid(Violation ~ V_Year)+
  coord_fixed(1.3)+
  theme(strip.background =element_blank(), legend.position = "none")+
  scale_x_continuous(name = NULL, labels = NULL, breaks = NULL)+
  scale_y_continuous(name = NULL, labels = NULL, breaks = NULL)

ggsave("F3.Pest_Maps.png", plot = last_plot(), device = "png", path = here::here("output"), scale = 1, units = "in", dpi = 300)

ggplot(data = pests1, aes(x=LONGITUDE, y=LATITUDE))+ stat_density_2d(aes(fill = ..level.., alpha = ..level..), geom = "polygon")+
  scale_fill_distiller(palette = "Spectral", na.value = "white")+geom_polygon(data = chicago, aes(x=long, y=lat, group = group), fill = NA, color = "grey70")+
  facet_grid(Violation ~ V_Year)+theme_classic()+
  coord_fixed(1.3)+
  theme(strip.background =element_blank(), legend.position = "none")+
  scale_x_continuous(name = NULL, labels = NULL, breaks = NULL)+
  scale_y_continuous(name = NULL, labels = NULL, breaks = NULL)
ggsave("F4.Pest_Maps_Head.png", plot = last_plot(), device = "png", path = here::here("output"), scale = 1, units = "in", dpi = 300)

#Map out Insect Listings - BB / NOT BB
insects1<-insects %>% filter(V_Year == 2010 | V_Year == 2011 | V_Year == 2012 | V_Year == 2013 | V_Year == 2014 | V_Year == 2015 | V_Year == 2016 | V_Year == 2017 | V_Year == 2018)
ggplot()+geom_polygon(data = chicago, aes(x=long, y=lat, group = group), fill = NA, color = "grey70")+
  geom_point(data=insects1, aes(x=LONGITUDE, y=LATITUDE, colour = bb, alpha = .1), size = .075)+
  theme_classic()+
  facet_grid(bb ~ V_Year, labeller = labeller(bb = c("0" = "Not Bed Bugs", "1" = "Bed Bugs")))+
  coord_fixed(1.3)+
  theme(strip.background =element_blank(), legend.position = "none")+
  scale_x_continuous(name = NULL, labels = NULL, breaks = NULL)+
  scale_y_continuous(name = NULL, labels = NULL, breaks = NULL)

ggsave("F5.Insect_Maps.png", plot = last_plot(), device = "png", path = here::here("output"), scale = 1, units = "in", dpi = 300)

ggplot(data = insects1, aes(x=LONGITUDE, y=LATITUDE))+ stat_density_2d(aes(fill = ..level.., alpha = ..level..), geom = "polygon")+
  scale_fill_distiller(palette = "Spectral", na.value = "white")+geom_polygon(data = chicago, aes(x=long, y=lat, group = group), fill = NA, color = "grey70")+
  facet_grid(bb ~ V_Year, labeller = labeller(bb = c("0" = "Not Bed Bugs", "1" = "Bed Bugs")))+theme_classic()+
  coord_fixed(1.3)+
  theme(strip.background =element_blank(), legend.position = "none")+
  scale_x_continuous(name = NULL, labels = NULL, breaks = NULL)+
  scale_y_continuous(name = NULL, labels = NULL, breaks = NULL)
ggsave("F5.Insect_Maps_Head.png", plot = last_plot(), device = "png", path = here::here("output"), scale = 1, units = "in", dpi = 300)


# #Get Tracts and perform spatial overlay ----
# IL_TRT<-tracts("17", "031")
# proj4string(IL_TRT)
# #Create Spatial Points Data Frame from Pests and Insects
# pests2<-pests1 %>% filter(!is.na(LONGITUDE), !is.na(LATITUDE))
# coordinates(pests2) <- ~LONGITUDE+LATITUDE
# proj4string(pests2) = CRS(proj4string(IL_TRT))
# 
# insects2<-insects1 %>% filter(!is.na(LONGITUDE), !is.na(LATITUDE))
# coordinates(insects2) <- ~LONGITUDE+LATITUDE
# proj4string(insects2) = CRS(proj4string(IL_TRT))
# 
# pests_GID<-over(pests2, IL_TRT)
# pests_GID<-cbind(pests2@data, pests_GID)
# 
# insects_GID<-over(insects2, IL_TRT)
# insects_GID<-cbind(insects2@data, insects_GID)
# 
# pests_year<-pests_GID %>% 
#   group_by(GEOID, V_Year) %>%
#   summarise(count = n(), Insects = sum(Violation == "Insects"), Mice_Rodents = sum(Violation == "Mice and Rodents"), Rats = sum(Violation == "Rats"), Roaches = sum(Violation == "Roaches"))
# 
# pests_year<-left_join(pests_year, insects_year, by=c("GEOID", "V_Year"))
# 
# pests_year$Bedbugs[is.na(pests_year$Bedbugs)]<-0
# 
# # pests_trt <- as.data.frame(unique(pests_year$GEOID))
# # names(pests_trt)[1]<-"GEOID"
# # years<-c("2010", "2011", "2012", "2013", "2014", "2015", "2016")
# # pests_trt<-expand.grid(GEOID = factor(pests_trt), V_Year = factor(years))
# # 
# # pests_trt<-full_join(pests_trt, pests_year, by="GEOID")
# 
# #Import and Join ACS Covariats ----
# acs_data<-read_csv(here::here("data", "acs_data.csv"))
# acs_data$X1<-NULL
# acs_data$GEOID<-as.character(acs_data$GEOID)
# library(rgeos)
# chi_tract_filter<-gIntersects(chicago_shp, IL_TRT, byid = TRUE)
# chi_tract<-IL_TRT[as.vector(chi_tract_filter),]
# 
# 
# 
# pests_trt<-as.data.frame(chi_tract@data$GEOID)
# names(pests_trt)[1]<-"GEOID"
# pests_trt_10<-pests_trt
# pests_trt_10$V_Year<-2010
# pests_trt_11<-pests_trt
# pests_trt_11$V_Year<-2011
# pests_trt_12<-pests_trt
# pests_trt_12$V_Year<-2012
# pests_trt_13<-pests_trt
# pests_trt_13$V_Year<-2013
# pests_trt_14<-pests_trt
# pests_trt_14$V_Year<-2014
# pests_trt_15<-pests_trt
# pests_trt_15$V_Year<-2015
# pests_trt_16<-pests_trt
# pests_trt_16$V_Year<-2016
# pests_trt<-rbind(pests_trt_10, pests_trt_11,pests_trt_12,pests_trt_13,pests_trt_14,pests_trt_15,pests_trt_16)
# rm(pests_trt_10, pests_trt_11,pests_trt_12,pests_trt_13,pests_trt_14,pests_trt_15,pests_trt_16)
# 
# pests_trt1<-left_join(pests_trt, pests_year, by=c("GEOID", "V_Year"))
# pests_trt1$count[is.na(pests_trt1$count)]<-0
# pests_trt1$Insects[is.na(pests_trt1$Insects)]<-0  
# pests_trt1$Mice_Rodents[is.na(pests_trt1$Mice_Rodents)]<-0
# pests_trt1$Rats[is.na(pests_trt1$Rats)]<-0
# pests_trt1$Roaches[is.na(pests_trt1$Roaches)]<-0
# pests_trt1$Bedbugs[is.na(pests_trt1$Bedbugs)]<-0
# 
# 
# pests_trt_ct<-pests_trt1 %>% group_by(V_Year) %>% summarise(Violation = (sum(count > 0)/869)*100, Insects = (sum(Insects > 0)/869)*100, Mice_Rodents = (sum(Mice_Rodents > 0)/869)*100, Rats = (sum(Rats > 0)/869)*100, Roaches = (sum(Roaches > 0)/869)*100, Bedbugs = (sum(Bedbugs > 0)/869)*100)
# 
# write_csv(pests_trt1, here::here("output", "pests_trt.csv"))
# write_csv(acs_data, here::here("output", "acs_data.csv"))
# write_csv(bedbugs_trt1, here::here("output", "bedbugs_closed.csv"))
# 
# plot(chi_tract)
# writeOGR(chi_tract, here::here("output", "chicago_trt.shp"), driver = "ESRI Shapefile", layer = "chicago_trt")
# 
# bedbugs<-insects_GID %>% filter(bb == 1)
# bedbugs_year<-bedbugs %>% group_by(GEOID, V_Year) %>% summarise(count = n(), closed = sum(`INSPECTION STATUS` == "CLOSED"))
# bedbugs_trt1<-left_join(pests_trt, bedbugs_year, by=c("GEOID", "V_Year"))
# bedbugs_trt1$count[is.na(bedbugs_trt1$count)]<-0
# bedbugs_trt1$closed[is.na(bedbugs_trt1$closed)]<-0
# sum(bedbugs_trt1$count)
# 
# test<-insects %>% filter(bb == 1) %>% group_by(LOCATION) %>% summarise(count=n())
# 
# tapply(pests_trt1$Bedbugs,list(pests_trt1$GEOID,pests_trt1$V_Year),sum)
# apply(tapply(pests_trt1$Bedbugs,list(pests_trt1$GEOID,pests_trt1$V_Year),sum)>0,2,mean)
# table(test$count)
# points(apply(tapply(pests_trt1$Insects,list(pests_trt1$GEOID,pests_trt1$V_Year),sum)>0,2,mean),ylim=c(0,1),col=3)
# points(apply(tapply(pests_trt1$Mice_Rodents,list(pests_trt1$GEOID,pests_trt1$V_Year),sum)>0,2,mean),ylim=c(0,1),col=4)
# points(apply(tapply(pests_trt1$Roaches,list(pests_trt1$GEOID,pests_trt1$V_Year),sum)>0,2,mean),ylim=c(0,1),col=5)
# points(apply(tapply(pests_trt1$Rats,list(pests_trt1$GEOID,pests_trt1$V_Year),sum)>0,2,mean),ylim=c(0,1),col=6)
# plot(apply(tapply(pests_trt1$Bedbugs,list(pests_trt1$GEOID,pests_trt1$V_Year),sum)>0,2,mean),ylim=c(0,1))
# points(apply(tapply(pests_trt1$Insects,list(pests_trt1$GEOID,pests_trt1$V_Year),sum)>0,2,mean),ylim=c(0,1),col=3)
# 
# table(bedbugs$`INSPECTOR ID`)
# table(bedbugs$`INSPECTOR ID`, bedbugs$`INSPECTION STATUS`)
# plot(bedbugs$LATITUDE~bedbugs$LONGITUDE,col=as.numeric(bedbugs$`INSPECTOR ID`),asp=1)
# table(bedbugs$`INSPECTOR ID`, bedbugs$`V_Year`)
# image(t(table(bedbugs$`INSPECTOR ID`, bedbugs$`V_Year`)),col=terrain.colors(100))
# table(pests_GID$`INSPECTION STATUS`, pests_GID$`VIOLATION STATUS`)
# 
# 
# bedbugs_ID <- bedbugs %>% select(ID, bb)
# pests_join<-left_join(pests_GID, bedbugs_ID, by="ID")
# pests_join$bb[is.na(pests_join$bb)]<-0
# write_csv(pests_join, here::here("output", "pests_join.csv"))
# 
# 
# 
# library(reshape2)
# bedbugs_trt1 %>% dcast(GEOID ~ V_Year, value.var = "count")
# bedbugs_trt1 %>% dcast(GEOID ~ V_Year, value.var = "closed")
# 
# bedbugs$Violation_Year<-lubridate::mdy(bedbugs$`VIOLATION DATE`)
# bedbugs$Update_Year<-lubridate::mdy(bedbugs$`VIOLATION STATUS DATE`)
# 
# bedbugs_complied<-bedbugs %>% filter(`VIOLATION STATUS` == "COMPLIED") %>% select(ID, GEOID, Violation_Year, Update_Year)
# 
# bedbugs_seq<-bedbugs_complied %>% 
#   rowwise() %>% 
#   do(data.frame(ID = .$ID, Year = year(seq(.$Violation_Year, .$Update_Year, by=1))))
# 
# bedbugs_seq<- bedbugs_seq %>% group_by(ID, Year) %>% summarise(count=n())
# bedbugs_seq_1<-bedbugs_seq %>% select(ID, Year)%>% dcast(ID~Year, fill = 0)
# bedbugs_seq_1$`2010`[bedbugs_seq_1$`2010` != 0]<-1
# bedbugs_seq_1$`2011`[bedbugs_seq_1$`2011` != 0]<-1
# bedbugs_seq_1$`2012`[bedbugs_seq_1$`2012` != 0]<-1
# bedbugs_seq_1$`2013`[bedbugs_seq_1$`2013` != 0]<-1
# bedbugs_seq_1$`2014`[bedbugs_seq_1$`2014` != 0]<-1
# bedbugs_seq_1$`2015`[bedbugs_seq_1$`2015` != 0]<-1
# bedbugs_seq_1$`2016`[bedbugs_seq_1$`2016` != 0]<-1
# bedbugs_seq_1$`2017`[bedbugs_seq_1$`2017` != 0]<-1
# 
# bedbugs_id<-bedbugs %>% select(ID, GEOID) %>% unique()
# bedbugs_seq_2<-left_join(bedbugs_seq_1, bedbugs_id, by="ID")
# bedbugs_seq_3<-bedbugs_seq_2 %>% group_by(GEOID) %>% 
#   summarise(Y2010 = sum(`2010`), Y2011 = sum(`2011`), Y2012 = sum(`2012`), Y2013 = sum(`2013`), Y2014 = sum(`2014`), Y2015 = sum(`2015`), Y2016 = sum(`2016`), Y2017 = sum(`2017`))
# 
# bedbugs_total<-bedbugs %>% group_by(GEOID, V_Year) %>% summarise(count = n())
# bedbugs_total_1<-bedbugs_total %>% dcast(GEOID ~ V_Year, value.var = "count", fill = 0)
