install.packages("RSocrata")
library(RSocrata)
setwd("/nfs/bedbugs-data/Summer_2018/Chicago")
chicago_data <- read.socrata("https://data.cityofchicago.org/resource/ucdv-yd74.json")
write_csv(as.data.frame(chicago_data), "data/Chicago_Violations.csv")
library(sf)

chicago_data<-chicago_data %>% filter(!is.na(latitude), !is.na(longitude))
chicago_data<-chicago_data %>% mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude))
chicago_data <-st_as_sf(chicago_data, coords = c("longitude", "latitude"))
#plot(chicago_data_sp$geometry)
chicago_data<-chicago_data %>% mutate(V_Date = violation_date)
chicago_data<-chicago_data %>% mutate(V_Year = lubridate::year(V_Date))
chicago_data<-chicago_data %>% mutate(V_Month = lubridate::month(V_Date))
chicago_data<-chicago_data %>% mutate(V_Month_Date = paste(V_Month, V_Year, sep="-"))

pests <- chicago_data %>% filter(violation_code == "CN134016" |
                                   violation_code == "CN135016"|
                                   violation_code == "CN136016"|
                                   violation_code == "CN136026")

pests<-pests %>% mutate(Violation = recode(violation_code, "CN134016" = "Rats", "CN135016" = "Mice and Rodents", "CN136016" = "Roaches", "CN136026" = "Insects"))

pests_month<-pests %>% group_by(V_Year, V_Month, Violation) %>% summarise(count=n())
pests_month<-pests_month %>% mutate(V_Month_Year = as_date(paste(V_Year, V_Month, "01", sep="-")))

tidy_insects_base<-chicago_data %>%  filter(violation_code == "CN136026") %>% select(id, violation_inspector_comments) %>% unnest_tokens(bigram, violation_inspector_comments, token = "ngrams", n=2)
tidy_insects_1_separated <- tidy_insects_base %>%
  separate(bigram, c("word1", "word2"), sep = " ")
tidy_insects_1_separated$match1<- ifelse(tidy_insects_1_separated$word1 == "bed" & tidy_insects_1_separated$word2 == "bug", 1, 0)
tidy_insects_1_separated$match2<- ifelse(tidy_insects_1_separated$word1 == "bed" & tidy_insects_1_separated$word2 == "bugs", 1, 0)
tidy_insects_1_separated$match3<- ifelse(tidy_insects_1_separated$word1 == "bedbug", 1, 0)
tidy_insects_1_separated$match4<- ifelse(tidy_insects_1_separated$word1 == "bedbugs", 1, 0)
tidy_insects_1_separated$finalmatch<- ifelse(tidy_insects_1_separated$match1 == 1 | tidy_insects_1_separated$match2 == 1 | tidy_insects_1_separated$match3 == 1 | tidy_insects_1_separated$match4 == 1, 1, 0)
tidy_insects_test<-tidy_insects_1_separated %>% group_by(id) %>% summarise(count = sum(finalmatch))
tidy_insects_test$bb<-ifelse(tidy_insects_test$count > 1, 1, 0)
sum(tidy_insects_test$bb, na.rm=TRUE)
pests<-left_join(pests, tidy_insects_test, by="id")
sum(pests$bb, na.rm=TRUE)
pests$bb[is.na(pests$bb)]<-0

rm(tidy_insects_1_separated, tidy_insects_base, tidy_insects_test)
insects <- pests %>% filter(violation_code == "CN136026")
insects$bb[is.na(insects$bb)]<-0

library(leaflet)
leaflet() %>% addTiles()%>% setView(lng = -87.623177, lat = 41.881832, zoom = 10)

insects_sp<-insects %>% filter(!is.na(latitude), !is.na(longitude))
insects_sp<-insects_sp %>% mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude))
insects_sp <-st_as_sf(insects_sp, coords = c("longitude", "latitude"))
plot(insects_sp$geometry)

leaflet(insects_sp) %>% addTiles()%>% setView(lng = -87.623177, lat = 41.881832, zoom = 10) %>% addCircleMarkers(radius = .5)
leaflet(insects_sp[insects_sp$bb == 1,]) %>% addTiles()%>% setView(lng = -87.623177, lat = 41.881832, zoom = 10) %>% addCircleMarkers(radius = .5)




#Prepare Data for Time Series Analysis ----
insects_month<-insects %>% group_by(V_Year, V_Month, bb) %>% summarise(count=n())
insects_month<-insects_month %>% mutate(V_Month_Year = as_date(paste(V_Year, V_Month, "01", sep="-")))
insects_month<-insects_month %>% filter(V_Year >= 2010)
insects_month<-insects_month %>% complete(V_Month_Year = seq.Date(min(V_Month_Year), max(V_Month_Year), by="month"), fill=list(count = 0))

bb_month<-insects_month %>% ungroup() %>% filter(bb == 1) %>% select(V_Month_Year, count)
bb_month<-bb_month %>% complete(V_Month_Year = seq.Date(min(V_Month_Year), max(V_Month_Year), by="month"), fill=list(count = 0))

bb_month$V_Month_Year<-as.character(bb_month$V_Month_Year)



#Misc - not for production ----
# setwd("/nfs/bedbugs-data/Summer_2018/NYC/data")
# nyc_complaints<-read.socrata("https://data.cityofnewyork.us/resource/gih3-4epm.json")
# curl::curl_download("https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_18v1.zip", "pluto.zip")
# unzip("pluto.zip")
# 
# import_settings<-c(Block = "character", Lot = "character", CD = "character", CT2010 = "character", 
#                    CB2010 = "character", Council = "character", SchoolDist = "character", ZipCode = "character", 
#                    PolicePrct = "character", YearAlter1 = "date", YearAlter2 = "date", BBL = "character", 
#                    Tract2010 = "character", XCoord = "numeric", YCoord="numeric", APPBBL = "character", 
#                    AssessLand = "numeric", AssessTot = "numeric", ExemptLand = "numeric",
#                    ExemptTot = "numeric")
# pluto_bk<-data.table::fread("PLUTO_for_WEB/BK_18v1.csv", colClasses = import_settings)
# pluto_bx<-data.table::fread("PLUTO_for_WEB/BX_18v1.csv", colClasses = import_settings)
# pluto_mn<-data.table::fread("PLUTO_for_WEB/MN_18v1.csv", colClasses = import_settings)
# pluto_qn<-data.table::fread("PLUTO_for_WEB/QN_18v1.csv", colClasses = import_settings)
# pluto_si<-data.table::fread("PLUTO_for_WEB/SI_18v1.csv", colClasses = import_settings)
# 
# nyc_pluto<-bind_rows(pluto_bk, pluto_bx, pluto_mn, pluto_qn, pluto_si)
# rm(pluto_bk, pluto_bx, pluto_mn, pluto_qn, pluto_si)
# 
# nyc_complaints<-
# setwd("/nfs/bedbugs-data/Summer_2018/Chicago")