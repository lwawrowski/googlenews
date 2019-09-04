library(tidyverse)
library(jsonlite)
library(lubridate)

api_key <- "e1b299f31e2142b3b05793f58d2f368d"

get_news <- function(api_key){
  
  category <- c("general", "business", "entertainment", "health", "science", "sports", "technology")
  
  d <- data.frame()
  
  for(i in 1:length(category)){

    out <- tryCatch({
      
      url <- paste0("https://newsapi.org/v2/top-headlines?country=pl&pageSize=100&category=",category[i],"&apiKey=",api_key)
      
      di <- fromJSON(url)
      
      di_df <- cbind(di$articles$source,di$articles[,2:ncol(di$articles)]) %>% mutate(category=category[i])
      
      d <- rbind(d, di_df)
      
    }, error = function(e){
      
      message(e)
      
      return(d)
      
    })
    
  }
  
  return(out)

}

a <- Sys.time()
news <- get_news(api_key)
Sys.time() - a

start_time <- "18-08-2019 23:00"
end_time <- "2-09-2019 3:00"

int <- dmy_hm(start_time) %--% dmy_hm(end_time)
time<- int@.Data/60

n <- round(as.numeric(time) / 60)

news_all <- data.frame()

for(t in 1:n){
  
  cat(paste0("News ", t, " z ", n, "\n"))
  
  news <- get_news(api_key)
  
  news_all <- rbind(news_all, news)
  
  if(t %% 34 == 0){
    save(news_all, file = paste0("data/dane_",format(Sys.time(),"%d_%m_%Y_%H_%M"),".RData"))
    
    news_all <- data.frame()
  }
  
  if(t != n){
    Sys.sleep(3590)  
  }
  
}