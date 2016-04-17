#載入資料
{
  x1_path<-paste("D:/000/yahoo_movie.csv")
  yahoo_movie<-read.csv(x1_path,header = TRUE,stringsAsFactors = FALSE)
  
  x2_path<-paste("D:/000/3607_3912.csv")
  ptt_movie<-read.csv(x2_path,header = TRUE,stringsAsFactors = FALSE)
}

#找雷文
{
  class_ptt_title<-NULL
  for (x in 1:nrow(ptt_movie)) {
   
    #先找有沒有"]"
    ptt_title_check<-unlist(strsplit(ptt_movie$title[x],split="",fixed=T))
    if (sum(ptt_title_check == "]")>0) {
  
      #切割找"雷"
      ptt_title_split<-unlist(strsplit(ptt_movie$title[x],split="]",fixed=T))
      ptt_title_one_split<-unlist(strsplit(ptt_title_split[1],split="",fixed=T))
      if (sum(ptt_title_one_split == "雷")>0) {
        class_ptt_title<-c(class_ptt_title,x)
      }
    }
  }
}

#雷文彙總
{
  ptt_movie_ray<-NULL
  for (y in 1:length(class_ptt_title)) {
    ptt_movie_ray<-rbind(ptt_movie_ray,ptt_movie[class_ptt_title[y],])
  }
}

#依排名分類
{
  #超人自己做
  {
  w<-1
  yahoo_movie_title_split<-unlist(strsplit(yahoo_movie$cn_name[w],split="",fixed=T))
  yahoo_movie_title_split_en<-c("b","B","v","V","s","S")
  
    #電影名稱長度>3使用
    if (nchar(yahoo_movie$cn_name[w])>3) {
      ptt_by_yahoo<-NULL
      for (z in 1:nrow(ptt_movie_ray)) {
        ptt_title_ray_split_first<-unlist(strsplit(ptt_movie_ray$title[z],split="]",fixed=T))
        ptt_title_ray_split<-unlist(strsplit(ptt_title_ray_split_first[2],split="",fixed=T))
        
        #比較字元yahoo:ptt標題
        b<-0 
        c<-1
        d<-0
       
        #中文比對
        for (a in 1:length(yahoo_movie_title_split)) {
          if (sum(grepl(yahoo_movie_title_split[a],ptt_title_ray_split))>0) {
            b<-b+c
          }
        }
        
        #英文比對(額外)
        for (a in 1:length(yahoo_movie_title_split_en)) {
          if (sum(grepl(yahoo_movie_title_split_en[a],ptt_title_ray_split))>0) {
            d<-d+c
          }
        }
        
        #字元比對中文相同數>2,英文>2
        if (b>2 | d>2) {
          ptt_by_yahoo<-rbind(ptt_by_yahoo,ptt_movie_ray[z,])
        }
      }
      new_path<-paste("D:/111/",w,".CSV",sep = "")
      write.csv(ptt_by_yahoo, file = new_path)
    }
  }
  #2-20名
  for (w in 2:nrow(yahoo_movie)) {
    
    #拆字
    yahoo_movie_title_split<-unlist(strsplit(yahoo_movie$cn_name[w],split="",fixed=T))
    
    #電影名稱長度<=3使用
    if (nchar(yahoo_movie$cn_name[w])<=3) {
      ptt_by_yahoo<-NULL
      for (z in 1:nrow(ptt_movie_ray)) {
        ptt_title_ray_split_first<-unlist(strsplit(ptt_movie_ray$title[z],split="]",fixed=T))
        ptt_title_ray_split<-unlist(strsplit(ptt_title_ray_split_first[2],split="",fixed=T))
        
        #比較字元yahoo:ptt標題
        b<-0
        for (a in 1:length(yahoo_movie_title_split)) {
          if (sum(grepl(yahoo_movie_title_split[a],ptt_title_ray_split))>0) {
            c<-1
            b<-b+c
          }
        }
        
        #字元數相同>2
        if (b>1) {
          ptt_by_yahoo<-rbind(ptt_by_yahoo,ptt_movie_ray[z,])
        }
      }
    }
    
    #電影名稱長度>3使用
    if (nchar(yahoo_movie$cn_name[w])>3) {
      ptt_by_yahoo<-NULL
      for (z in 1:nrow(ptt_movie_ray)) {
        ptt_title_ray_split_first<-unlist(strsplit(ptt_movie_ray$title[z],split="]",fixed=T))
        ptt_title_ray_split<-unlist(strsplit(ptt_title_ray_split_first[2],split="",fixed=T))
   
        #比較字元yahoo:ptt標題     
        b<-0
        for (a in 1:length(yahoo_movie_title_split)) {
          if (sum(grepl(yahoo_movie_title_split[a],ptt_title_ray_split))>0) {
            c<-1
            b<-b+c
          }
        }
        
        #字元數相同>2
        if (b>2) {
          ptt_by_yahoo<-rbind(ptt_by_yahoo,ptt_movie_ray[z,])
        }
      }
    }
    
    new_path<-paste("D:/111/",w,".CSV",sep = "")
    write.csv(ptt_by_yahoo, file = new_path)
  }
}
