## -----------------------------------------
## load packages
## -----------------------------------------

library(here)
library(rvest)
library(tidyverse)

## -----------------------------------------
## set URL(s)
## ----------------------------------------- 

url1 <- "https://ecoforum.strangeloopgames.com/category/23/general"
url2 <- "https://ecoforum.strangeloopgames.com/category/22/game-servers"
url3 <- "https://ecoforum.strangeloopgames.com/category/18/game-stories"
url4 <- "https://ecoforum.strangeloopgames.com/category/24/ideas-feedback"
url5 <- "https://ecoforum.strangeloopgames.com/category/19/eco-as-education"
url6 <- "https://ecoforum.strangeloopgames.com/category/20/modding"
url7 <- "https://ecoforum.strangeloopgames.com/category/8/community-help"
url8 <- "https://ecoforum.strangeloopgames.com/category/9/community-management"
url9 <- "https://ecoforum.strangeloopgames.com/category/21/bug-reports"


urlList <- c(url1, url2, url3, url4, url5, url6, url7, url8, url9)

urlList

## -----------------------------------------
## do initial scraping
## -----------------------------------------

topicGrabber <- function(url){
  categoryName <- paste0(substr(url, max(gregexpr("/", url)[[1]]) + 1, nchar(url)),"_frame")
  
  firstPage <- read_html(paste0(url,"?page=1"))
  print(paste0("processing page 1 of ",categoryName))
  firstPageLinks <- firstPage %>% html_nodes('.title') %>% html_nodes(xpath = "a")

  assign(categoryName, bind_rows(lapply(xml_attrs(firstPageLinks), function(x) data.frame(as.list(x), stringsAsFactors=FALSE))), pos = .GlobalEnv)
  
  loopLive <- TRUE
  i <- 2
  
  while(loopLive){
    tempFrame <- get(categoryName)
    nextPage <- NULL
    try(nextPage <- read_html(paste0(url,"?page=",i)))
    if(is.null(nextPage)){
      print("WHOA, ERROR")
      next
    }
    nextPageLinks <- nextPage %>% 
      html_nodes('.title') %>% 
      html_nodes(xpath = "a")
    if(length(nextPageLinks) == 0){
      loopLive <- FALSE
      break
    }
    if(length(nextPageLinks) > 0){
      tempFrame <- rbind(tempFrame, bind_rows(lapply(xml_attrs(nextPageLinks), function(x) data.frame(as.list(x), stringsAsFactors=FALSE))))
      print(paste0("processing page ", i, " of ",categoryName))
      i <- i + 1
      assign(categoryName, tempFrame, pos = .GlobalEnv)
    }
  }
  write_csv(get(categoryName), paste0(here::here(), "/data/topics/",categoryName,".csv"))
}

lapply(urlList, topicGrabber)

## -----------------------------------------
## grab individual posts
## -----------------------------------------

postGrabber <- function(frameList){
  for(i in 1:length(frameList)){
    forumName <- substr(frameList[i],0,regexpr("_",frameList[i]) - 1)
    print(paste0("beginning analysis of ",forumName))
    tempForum <- read_csv(paste0(here::here(),"/data/topics/",frameList[i]))
    threadList <- tempForum$href
    
    temp_frame <- data.frame(forum=character(),
                             thread=character(),
                             threadLink=character(),
                             views=character(), 
                             threadPosts=integer(),
                             postNumber=integer(),
                             screenname=character(),
                             post=character(),
                             stringsAsFactors=FALSE)
  
    for(j in 1:length(threadList)){
      threadLink <- paste0("https://ecoforum.strangeloopgames.com",threadList[j])
      threadPage <- NULL
      try(threadPage <- read_html(threadLink))
      if(is.null(threadPage)){
        next
      }
      threadTitle <- xml_text(threadPage %>% xml_nodes(xpath = "head/title"))
      print(paste0("beginning analysis of ",threadTitle," thread ",j," of ",length(threadList)))
      viewCount <- as.integer(xml_text(threadPage %>% xml_nodes(xpath = "//div/span[@class='human-readable-number']"))[2])
      totalPostCount <- as.integer(xml_text(threadPage %>% xml_nodes(xpath = "//div/span[@class='human-readable-number']"))[1])
      print(paste0("this thread has ",totalPostCount, " posts"))
      postList <- threadPage %>% xml_nodes('.content')
      usernameList <- xml_text(threadPage %>% xml_nodes(xpath = "//a/@data-username"))
      
      for(k in 1:length(postList)){
        post <- ""
        try(post <- paste(xml_text(postList[k] %>% xml_nodes(xpath = "p")), collapse = " "))
        temp_frame[nrow(temp_frame)+1,] <- cbind(forumName, threadTitle, threadLink, viewCount, totalPostCount, k, usernameList[k], post)
        print(paste0("scraped post ",k," of ",totalPostCount))
      }
      
      if(totalPostCount > 20){
        print("reading page 2")
        threadPage <- read_html(paste0(threadLink,"?page=2"))
        postList <- threadPage %>% xml_nodes('.content')
        usernameList <- xml_text(threadPage %>% xml_nodes(xpath = "//a/@data-username"))
        for(l in 1:length(postList)){
          post <- ""
          try(post <- paste(xml_text(postList[l] %>% xml_nodes(xpath = "p")), collapse = " "))
          temp_frame[nrow(temp_frame)+1,] <- cbind(forumName, threadTitle, threadLink, viewCount, totalPostCount, k + l, usernameList[l], post)
          print(paste0("scraped post ",k + l," of ",totalPostCount))  
        }
      }
      
      if(totalPostCount > 40){
        print("reading page 3")
        threadPage <- read_html(paste0(threadLink,"?page=3"))
        postList <- threadPage %>% xml_nodes('.content')
        usernameList <- xml_text(threadPage %>% xml_nodes(xpath = "//a/@data-username"))
        for(m in 1:length(postList)){
          post <- ""
          try(post <- paste(xml_text(postList[m] %>% xml_nodes(xpath = "p")), collapse = " "))
          temp_frame[nrow(temp_frame)+1,] <- cbind(forumName, threadTitle, threadLink, viewCount, totalPostCount, k + l + m, usernameList[m], post)
          print(paste0("scraped post ",k + l + m," of ",totalPostCount))
        }
      }
      if(totalPostCount > 60){
        print("reading page 4")
        threadPage <- read_html(paste0(threadLink,"?page=4"))
        postList <- threadPage %>% xml_nodes('.content')
        usernameList <- xml_text(threadPage %>% xml_nodes(xpath = "//a/@data-username"))
        for(n in 1:length(postList)){
          post <- ""
          try(post <- paste(xml_text(postList[n] %>% xml_nodes(xpath = "p")), collapse = " "))
          temp_frame[nrow(temp_frame)+1,] <- cbind(forumName, threadTitle, threadLink, viewCount, totalPostCount, k + l + m + n, usernameList[n], post)
          print(paste0("scraped post ",k + l + m + n," of ",totalPostCount))
        }
      }
      if(totalPostCount > 80){
        print("reading page 5")
        threadPage <- read_html(paste0(threadLink,"?page=5"))
        postList <- threadPage %>% xml_nodes('.content')
        usernameList <- xml_text(threadPage %>% xml_nodes(xpath = "//a/@data-username"))
        for(o in 1:length(postList)){
          post <- ""
          try(post <- paste(xml_text(postList[o] %>% xml_nodes(xpath = "p")), collapse = " "))
          temp_frame[nrow(temp_frame)+1,] <- cbind(forumName, threadTitle, threadLink, viewCount, totalPostCount, k + l + m + n + o, usernameList[o], post)
          print(paste0("scraped post ",k + l + m + n + o," of ",totalPostCount))
        }
      }
      if(totalPostCount > 100){
        print("reading page 6")
        threadPage <- read_html(paste0(threadLink,"?page=6"))
        postList <- threadPage %>% xml_nodes('.content')
        usernameList <- xml_text(threadPage %>% xml_nodes(xpath = "//a/@data-username"))
        for(p in 1:length(postList)){
          post <- ""
          try(post <- paste(xml_text(postList[p] %>% xml_nodes(xpath = "p")), collapse = " "))
          temp_frame[nrow(temp_frame)+1,] <- cbind(forumName, threadTitle, threadLink, viewCount, totalPostCount, k + l + m + n + o + p, usernameList[p], post)
          print(paste0("scraped post ",k + l + m + n + o + p," of ",totalPostCount))
        }
      }
      
    # end of post loop
    }
    # end of thread loop
    write_csv(temp_frame,paste0(here::here(), "/data/posts/",forumName,".csv")) 
  }
  # end of forum loop
}







# this will be input into the function 
frameList <- list.files(paste0(here::here(),"/data/topics"))

frameList

postGrabber(frameList)


