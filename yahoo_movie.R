library("XML")
library("httr")
library("stringr")
library("dplyr")

cn_name<-NULL
en_name<-NULL

#載入網址
html <- content(GET("https://tw.movies.yahoo.com/chart.html?cate=taipei"),as = 'parsed')

#先抓第1名
first<-xpathApply(html,"//div[@class='item clearfix']/div[@class='text']/a",xmlValue)

cn_name<-c(cn_name,unlist(first[1]))
en_name<-c(en_name,unlist(first[2]))

#抓2-20
others_list<-xpathApply(html,"//td[@class='c3']/a",xmlValue)

others_length<-length(others_list)/2

for (x in 1:others_length) {
  cn_name<-c(cn_name,unlist(others_list[1+(x-1)*2]))
  en_name<-c(en_name,unlist(others_list[2+(x-1)*2]))
}

#結合
all_name<-cbind(cn_name,en_name)
View(all_name)

#存檔
all_path<-paste("D:/000/yahoo_movie.CSV",sep = "")
write.csv(all_name, file = all_path)
