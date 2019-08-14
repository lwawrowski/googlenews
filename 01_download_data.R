library(tidyverse)
library(jsonlite)
library(lubridate)

api_key <- ""

get_news <- function(api_key){
  
  category <- c("general", "business", "entertainment", "health", "science", "sports", "technology")
  
  d <- data.frame()
  
  for(i in 1:length(category)){
    
    di <- fromJSON(paste0("https://newsapi.org/v2/top-headlines?country=pl&pageSize=100&category=",category[i],"&apiKey=",api_key))
    
    di_df <- cbind(di$articles$source,di$articles[,2:ncol(di$articles)]) %>% mutate(category=category[i])
    
    d <- rbind(d, di_df)
    
  }
  
  return(d)

}

news <- get_news(api_key)

start_time <- "14-08-2019 16:00"
end_time <- "29-08-2019 16:00"

int <- dmy_hm(start_time) %--% dmy_hm(end_time)
time<- int@.Data/60

n <- round(as.numeric(time) / 60)

news_all <- data.frame()

for(t in 1:n){
  
  cat(paste0("News ", t, " z ", n, "\n"))
  
  news <- get_news(api_key)
  
  news_all <- rbind(news_all, news)
  
  if(t %% 30 == 0){
    save(news_all, file = paste0("data/dane_",format(Sys.time(),"%d_%m_%Y_%H_%M"),".RData"))
    
    news_all <- data.frame()
  }
  
  if(t != n){
    Sys.sleep(3540)  
  }
  
}