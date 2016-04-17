
##尋找目標
#html_find <- content(GET("https://www.ptt.cc/bbs/movie/index3607.html"),as = 'parsed')
#head(xpathApply(html_find,"//div[@class='date']",xmlValue),1)

###############################set###############################
board<-"movie"
start_page<-3607
final_page<-3912
###############################set###############################


######################################################################
ptm <- proc.time()

library("XML")
library("httr")
library("stringr")
library("dplyr")

######################################################################

#擷取文章網址

data_html <- NULL
index_count<-0
if (index_count<=(final_page-start_page)) {
  for( i in start_page:final_page){
    index_count<-index_count+1
    Sys.sleep(1)
    url_1 <- paste('bbs/',board,'/index', i, '.html', sep='')
    html_1 <- content(GET("https://www.ptt.cc/", path = url_1),as = 'parsed')
    
    ###################################################
    ##有密碼的板                                     ##
    ##GET(url, config=set_cookies("over18"="1"),...) ##
    ###################################################
    
    url.list <- xpathSApply(html_1, "//div[@class='title']/a[@href]", xmlAttrs)
    for (a in 1:length(url.list)) {
      data_html <- rbind(data_html, url.list[[a]])
    }
  }
  Sys.sleep(5)
}
######################################################################

count_text_all<-0
count_text<-0
text_html_all<-NULL
poster_all<-NULL
title_all<-NULL
date_all<-NULL
full_text_all<-NULL
all_puller<-NULL
all_puller_num_div3_all<-NULL
all_puller_tag_good_num<-NULL
all_puller_tag_bad_num<-NULL
all_puller_tag_arrow_num<-NULL

#擷取各分頁內容
if (count_text_all<=length(data_html)) {
  start_num<-count_text_all+1
  for (b_count in start_num:length(data_html)) {
    count_text_all<-b_count

    ####################################    
    Sys.sleep(1)
    ####################################
    
    html <-content(GET('https://www.ptt.cc',path =data_html[[b_count]]), as = 'parsed')
 
    ######################################################################

    #推文*3,推噓箭頭,人,內容
    ##############################################################
    all_puller<-xpathApply(html,"//div[@class='push']/span[position()<4]/text()",xmlValue)

    #回文數>1才做
    if (length(all_puller)>3) {
      
      ####################################    
      Sys.sleep(1)
      ####################################    

      #計數
      count_text<-count_text+1
      
      #文章網址
      text_html<-paste('https://www.ptt.cc',data_html[[b_count]],sep = "")
      text_html_all<-c(text_html_all,text_html)
      
      #標題套餐*3
      titleforthree <- xpathApply(html,"//div[@class='article-metaline']/span[@class                                ='article-meta-value']"  ,xmlValue)

        #作者,清除暱稱,向量
        poster <- titleforthree[[1]]
        poster_clear <- str_split_fixed(poster, "\\(",2)[,1]   
        poster_all<-c(poster_all,poster_clear)
        
        #標題,向量
        title <- titleforthree[[2]]
        title_all<-c(title_all,title)
        
        #日期,向量
        date <- titleforthree[[3]]
        date_all<-c(date_all,date)
      
      ##內文,向量
      full_text <- xpathApply(html,"//div[@class='bbs-screen bbs-content']/text()"                                  ,xmlValue)
      full_text_length<-length(full_text)
      if(full_text_length>1){
        full_text_max<-1
        full_text_max_number<-0
        for (z in 1:full_text_length) {
          if (full_text_max<nchar(full_text[z])) {
            full_text_max<-nchar(full_text[z])
            full_text_max_number<-z
          }
        }
        full_text<-full_text[full_text_max_number]
      }
      
      full_text_all<-c(full_text_all,full_text)

      ##############################################################
     # html <-content(GET('https://www.ptt.cc/bbs/Stock/M.1459404002.A.8C4.html'), as = 'parsed')
      #all_puller<-xpathApply(html,"//div[@class='push']/span[position()<4]/text()",xmlValue)

      ##處理多餘推文內容    
      count_puller<-0
      repeat{
        if(length(all_puller)<=count_puller){
          break
        }
          for (k in count_puller:length(all_puller)) {
            count_puller<-count_puller+1
            if(k%%3==1){
              x<-c("推 ","噓 ","→ ")
              if(sum(all_puller[k]!=x)==3){
                all_puller<-all_puller[-(k-1)]
                break
              }
            }
          }
      }
      #推文數向量
      all_puller_num_div3<-length(all_puller)/3
      all_puller_num_div3_all<-c(all_puller_num_div3_all,all_puller_num_div3)

      ##############################################################      
      all_puller_tag<-NULL
      all_puller_id<-NULL
      all_puller_content<-NULL
      all_puller_tag_good<-0
      all_puller_tag_bad<-0
      all_puller_tag_arrow<-0
      ##############################################################
      
      #推文向量
      for (all_puller_num in 0:(all_puller_num_div3-1)) {
        all_puller_tag<-c(all_puller_tag,all_puller[1+all_puller_num*3])
        all_puller_id<-c(all_puller_id,all_puller[2+all_puller_num*3])
        all_puller_content<-c(all_puller_content,all_puller[3+all_puller_num*3])
      }
      all_puller_clean<-cbind(all_puller_tag,all_puller_id,all_puller_content)
      
      html_split<-unlist(strsplit(data_html[[b_count]],split="/",fixed=T))
      
      #推文內容存檔
      
      new_path<-paste("D:/000/",html_split[4],".CSV",sep = "")
      write.csv(all_puller_clean, file = new_path)

        #辨識推文
        for (j in 1:length(all_puller_tag)) {
          if(all_puller_tag[j]=="推 "){
            all_puller_tag_good<-all_puller_tag_good+1
          }
          if(all_puller_tag[j]=="噓 "){
            all_puller_tag_bad<-all_puller_tag_bad+1
          }
          if(all_puller_tag[j]=="→ "){
            all_puller_tag_arrow<-all_puller_tag_arrow+1
          }
        }
      
        #各式推文向量
        all_puller_tag_good_num<-c(all_puller_tag_good_num,all_puller_tag_good)
        all_puller_tag_bad_num<-c(all_puller_tag_bad_num,all_puller_tag_bad)
        all_puller_tag_arrow_num<-c(all_puller_tag_arrow_num,all_puller_tag_arrow)
    }
  }
  ####################################
  Sys.sleep(5)
  ####################################
}

#處理內文取代\n
full_text_all_by_all<-NULL
for (n in 1:length(full_text_all)) {
  full_text_all_by<-gsub("\n",replacement="",full_text_all[n])
  full_text_all_by_all<-c(full_text_all_by_all,full_text_all_by)
}

#結合成matrix
all_data<-cbind(title_all,poster_all,date_all,all_puller_num_div3_all,
           all_puller_tag_good_num,all_puller_tag_bad_num,
           all_puller_tag_arrow_num,text_html_all,full_text_all_by_all)

colnames(all_data)<-c('title','poster','date','puller_num','puller_good','puller_bad'                        ,'puller_arrow','html','full_text')

#展示,檢查
View(all_data)
total_ALL<-paste('總共',length(data_html),'篇文章',sep=" ")
catch_ALL<-paste('擷取',count_text,'篇文章',sep=" ")
check<-cbind(total_ALL,catch_ALL)


#計算執行時間
runtime<-proc.time() - ptm

runtime
check

###############################set###############################
#存檔
all_path<-paste("D:/000/",start_page,"_",final_page,".CSV",sep = "")
write.csv(all_data, file = all_path)
###############################set###############################
