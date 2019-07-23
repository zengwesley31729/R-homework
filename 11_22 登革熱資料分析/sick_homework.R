install.packages("dplyr")
install.packages("ggmap")
library(ggmap)
library(mapproj)
library(ggplot2) 
library(dplyr)
install.packages("readxl")
library(readxl)
sick = read_excel("D:/UserDATA/Desktop/姜老師R程式碼/data/sick.xlsx")

#流水號、調查日期、縣市、鄉鎮市區、村里、調查地區分類、 
#調查機關或代表地點、緯度、經度、調查戶數、陽性戶數、陽性戶數(有埃及斑蚊幼蟲)、
#調查人員種類、積水容器數量(戶內)、積水容器數量(戶外)、陽性容器數量(戶內)、
#陽性容器數量(戶外) 、採獲埃及斑蚊雌蟲數(戶內)、採獲埃及斑蚊雌蟲數(戶內)、
#採獲白線斑蚊雌蟲數(戶內)、採獲白線斑蚊雌蟲數(戶內)、孳生埃及斑蚊幼蟲數、
#孳生白線斑蚊幼蟲數、孳生斑蚊幼蟲數(未分類)、孳生斑紋蛹數、布氏指數、布氏級數、
#成蟲指數(埃及斑蚊)、成蟲指數(白線斑蚊)、住宅指數、
#住宅級數 住宅指數(有白線斑蚊)、住宅級數(有白線斑蚊)、容器指數、容器級數、
#幼蟲指數、幼蟲級數、蛹指數

#The serial number, the date of the survey, the county, the town, the town, the village, the survey area,
#Survey agency or representative location, latitude, longitude, number of surveyed households, number of positive households, number of positive households (with larvae of Aedes aegypti),
#Type of investigator, number of water storage containers (indoor), number of water storage containers (outdoor), number of positive containers (indoor),
#Number of positive containers (outdoors), number of females collecting Egyptian mosquitoes (indoors), number of females collecting Egyptian mosquitoes (indoors),
#Number of females caught in white-spotted mosquitoes (indoors), number of females caught in white-spotted mosquitoes (indoors), number of larvae of Aedes aegypti,
#The number of larvae of white-spotted mosquitoes, the number of larvae of aphid mosquitoes (unclassified), the number of plaques of axillary plaques, the Brinell index, the Brinell series,
#Adult index (Aedes aegypti), adult index (white-spotted mosquito), residential index,
#Residential grade residential index (with white line mosquitoes), residential grades (with white line mosquitoes), container index, container level,
#Larval index, larval series, sputum index

#rename column as english
names(sick) <- c("serial_number", "investigating_date","county", "township","village", "classification_of_survey_area", "investigation_agency_or_representative_location",
                  "latitude", "longitude", "number_of_households_surveyed", "positive_households", "positive_households_with_Egyptian_larvae", "kinds_of_the_investigating_people",
                  "number_of_water_containers_of_indoors", "number_of_water_containers_of_outdoors", "number_of_positive_containers_of_indoors", "number_of_positive_containers_of_outdoors",
                  "number_of_females_collecting_Egyptian_mosquitoes_of_indoors", "number_of_females_collecting_Egyptian_mosquitoes_of_outdoors",
                  "number_of_females_collecting_white_spotted_mosquitoes_of_indoors", "number_of_females_collecting_white_spotted_mosquitoes_of_outdoors", "number_of_aphid_larvae",
                  "number_of_white_spotted_mosquitoes", "unclassified_number_of_larvae_of_aphid_mosquitoes", "number_of_aphididae", "brinell_index", "brinell_series", "Adult_index_of_Aedes_aegypti",
                  "Adult_index_of_white_spotted_mosquitoes", "residential_index", "residential_series", "residential_index_with_white_spotted_mosquitoes", "residential_series_with_white_spotted_mosquitoes",
                  "container_index", "container_series", "larval_index", "larval_series", "sputum_index")

#google地圖無法讀取
#google API沒有認證不合法?
#我選擇死亡，因為我不想付錢買google api
#Error in download.file(url, destfile = tmp, quiet = !messaging, mode = "wb") : 
#cannot open URL 'http://maps.googleapis.com/maps/api/staticmap?center=Taiwan&zoom=7&size=640x640&scale=2&maptype=terrain&language=zh-TW&sensor=false'
#In addition: Warning message:
#In download.file(url, destfile = tmp, quiet = !messaging, mode = "wb") :
# cannot open URL 'http://maps.googleapis.com/maps/api/staticmap?center=Taiwan&zoom=7&size=640x640&scale=2&maptype=terrain&language=zh-TW&sensor=false'
#: HTTP status was '403 Forbidden' 

map7 = get_map(location = "Tainan", zoom = 7, language = "zh-TW", maptype = "roadmap")

ggmap(map7, darken = c(0.5, "white")) +
  geom_point(aes(x = 經度, y= 緯度),color = "red", data = "sick")

map = get_map(location = "Taiwan", zoom = 7, language = "zh-TW")

summary(sick)
attach(sick)
str(sick)
sick1 = select(sick, -investigation_agency_or_representative_location, -c(number_of_females_collecting_Egyptian_mosquitoes_of_indoors:number_of_females_collecting_white_spotted_mosquitoes_of_outdoors),
               -number_of_white_spotted_mosquitoes, -unclassified_number_of_larvae_of_aphid_mosquitoes, -number_of_aphididae,
               -sputum_index)
  
#填補NA值
sick1$positive_households[is.na(sick1$positive_households)] = 0

#colume names
#colume names
#1 serial number流水號
#2 investigating date調查日期 
#3 county、縣市
#4 township、鄉鎮市區
#5 village、村里
#6 classification of survey area、調查地區分類、
#7 investigation agency or representative location#調查機關或代表地點
#8 latitude、緯度
#9 longitude、經度
#10 number of households surveyed、調查戶數
#11 positive households、陽性戶數
#12 positive households with Egyptian larvae、陽性戶數(有埃及斑蚊幼蟲)、
#13 kinds of the investigating people#調查人員種類
#14 number of water containers of indoors積水容器數量(戶內)
#15number of water containers of outdoors、、積水容器數量(戶外)
#16 number of positive containers of indoors陽性容器數量(戶內)、
#17 number of positive containers of outdoors、陽性容器數量(戶外)
#18 number of females collecting Egyptian mosquitoes of indoors採獲埃及斑蚊雌蟲數(戶內)
#19 number of females collecting Egyptian mosquitoes of outdoors採獲埃及斑蚊雌蟲數(戶內)、
#20 number of females collecting white-spotted mosquitoes of indoors#採獲白線斑蚊雌蟲數(戶內)
#21 number of females collecting white-spotted mosquitoes of outdoors採獲白線斑蚊雌蟲數(戶外)
#22 number of aphid larvae、、孳生埃及斑蚊幼蟲數、
#23 number of white-spotted mosquitoes#孳生白線斑蚊幼蟲數
#24 unclassified number of larvae of aphid mosquitoes、孳生斑蚊幼蟲數(未分類)
#25 number of aphididae、孳生斑紋蛹數、
#26 brinell index布氏指數 
#(陽性容器數/調查之戶數) × 100 
#27 brinell series、布氏級數
#28 Adult index of Aedes aegypti、#成蟲指數(埃及斑蚊)
#每一戶住宅平均登革熱病媒蚊雌性成蟲數
#29 Adult index of white-spotted mosquitoes、成蟲指數(白線斑蚊)
#30 residential index、住宅指數、
#調查100戶住宅，發現登革熱病媒蚊幼蟲孳生戶數之百分比
#31 residential series#住宅級數 
#32 residential index with white-spotted mosquitoes住宅指數(有白線斑蚊)
#33 residential series with white-spotted mosquitoes、住宅級數(有白線斑蚊)
#34 container index、容器指數 
#調查100個容器，發現登革熱病媒蚊幼蟲孳生容器之百分比
#35 container series、容器級數、
#36 larval index#幼蟲指數、
#37 larval series幼蟲級數
#38 sputum index、蛹指數


#探索性資料
#ggplot2 視覺化
#histogram of number of households surveyed
ggplot(sick1, aes(`number_of_households_surveyed`)) + geom_histogram()
#histogram of number of positive_households
ggplot(sick1, aes(`positive_households`)) + geom_histogram()
#point of number_of_households_surveyed and  positive_households
ggplot(data = sick1,
       aes( x = number_of_households_surveyed,
            y = positive_households))+
  geom_point(color = 'red',
             alpha = 0.5) + theme_bw()
#point of number_of_households_surveyed and  latitude
ggplot(data = sick1,
       aes( x = latitude,
            y = positive_households))+
  geom_point(color = 'red',
             alpha = 0.5) + theme_bw()
#point of number_of_households_surveyed and  longitude
ggplot(data = sick1,
       aes( x = longitude,
            y = positive_households))+
  geom_point(color = 'red',
             alpha = 0.5) + theme_bw()

#boxplot of number_of_positive_households by village
ggplot(sick1)+geom_boxplot( aes( x = village,
                                         y = positive_households,
                                         colour = village))+
  labs( x = 'village',
        title = 'number_of_positive_households by village') + theme_bw()

#一張一張畫好麻煩，全部用迴圈畫好了
str(sick1)
#把字串類的column移除
sick2 = select(sick1, -serial_number, -investigating_date, -county,
               -township, -village, -classification_of_survey_area,
             -kinds_of_the_investigating_people)
               
               

plots <- list()
for(nm in names(sick2)) {
  png(file=paste0(nm, ".png"), width=600, height=600)
  plots[[nm]] <- ggplot(data=sick2) + geom_density(aes_string(x=nm)) + theme_bw()
  print(plots[[nm]])
  dev.off()
}

#真是佩服我自己
#完美
#可以睡覺了


