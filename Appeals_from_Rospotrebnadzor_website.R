####### Packages

library(RCurl)
library(httr)
library(xml2)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(tidyverse) 
library(rvest)
library(stringi)

########## Go using Ajax

## For the first page
url <- 'http://zpp.rospotrebnadzor.ru/Forum/Appeals/AjaxindexList'
request <- GET(url = url,
              query = list(
                page = 1, ## for the first page
                searchtext ="",
                categories ="[]")
)

wpage<-read_html(request)
questions <- html_nodes(wpage, '.appeal-title-link')
questions <- html_text(questions)  #appeal title is useless. I will delete it later

# Extract category
category <- html_nodes(wpage,'.appeal-cat-title')
category <- html_text(category)

# Extract region
region <- html_nodes(wpage,'.appeal-element-bottom~ .appeal-element-bottom+ .appeal-element-bottom')
region <- html_text(region)

# Extract ID with as /Forum/Appeal... Delete it later
appeal_id <- html_nodes(wpage, '.appeal-title-link')
appeal_id <- appeal_id  %>% html_attr("href")
dfdf <- data.frame(Appeal_ID = appeal_id, Appeal= questions, Category = category, 
                  Region= region, stringsAsFactors = FALSE)


#### There is a loop which sends page numbers to query. Starting from the 2nd page. Overall number of pages 2095 (last version: 29.11.2019)
ff <- for(i in 2:2095) {
  request <- GET(url = url,
                 query = list(
                   page = i,
                   searchtext ="",
                   categories ="[]")
  )
  wpage<-read_html(request)
  questions <- html_nodes(wpage, '.appeal-title-link')
  questions <- html_text(questions) #appeal title is useless. I will delete it later
  
  # Category
  category <- html_nodes(wpage,'.appeal-cat-title')
  category <- html_text(category)
  
  #Region
  region <- html_nodes(wpage,'.appeal-element-bottom~ .appeal-element-bottom+ .appeal-element-bottom')
  region <- html_text(region)
  
  #Appeal_ID
  appeal_id <- html_nodes(wpage, '.appeal-title-link')
  appeal_id <- appeal_id  %>% html_attr("href")
  
  df6 <- data.frame(Appeal_ID = appeal_id, Appeal= questions, Category = category, 
                    Region= region, stringsAsFactors = FALSE)
  dfdf <- rbind(dfdf, df6)
  
}


##### Scrape the text of appeals and responses

# URL of the main page 
url1 <- "http://zpp.rospotrebnadzor.ru" 

list_of_pages <- str_c(url1, dfdf$Appeal_ID)

url2 <- lapply(list_of_pages, read_html)

# Function for the appeal text extraction
appeal_text_function <- function(html){
  html %>% 
    html_nodes('.appeal-details-message') %>%      
    html_text() 
  
}    

appeal_text<- lapply(url2, appeal_text_function)
full_df <- cbind(dfdf, as.data.frame(unlist(appeal_text)))


# Function for the response text extraction
response_text_function <- function(html){
  html %>% 
    html_nodes('.appeal-comments-message') %>%      
    html_text() 
}


response_text<- lapply(url2, response_text_function)

# Command below helps to solve a problem when some pages have double response texts
response_text <- as.data.frame(t(stri_list2matrix(response_text)))
response_text <- response_text['V1']
full_df <- cbind(full_df, response_text)


# Function for the extraction of the appeal date
appeal_date_function <- function(html){
  html %>% 
    html_nodes('.appeal-element-bottom') %>%      
    html_text() 
  
  
}

appeal_date<- lapply(url2, appeal_date_function)
full_df<- cbind(full_df, as.data.frame(unlist(appeal_date)))


# Function for the extraction of the response date
response_date_function <- function(html){
  html %>% 
    html_nodes('.appeal-comments-date') %>%      
    html_text() 
  
}


response_date<- lapply(url2, response_date_function)
response_date <- as.data.frame(t(stri_list2matrix(response_date))) # here is the same problem as in case of response texts. Delete replicating responses
response_date <- response_date['V1']
full_df <- cbind(full_df, response_date)


# Names as in task
names(full_df) <- c("appeal_ID", "appeal", "category", "region",
                    "appeal_text", "appeal_date", "response_text", "response_date")

# Order of columns as in task
full_df <- full_df[, c(1,6,8,4,3,5,7,2)]

# Delete Useless title of the appeal 
full_df<-full_df[, -8]


##### Cleaning columns
# Create new dataframe in order to return full_df without any problem
full_df_1 <- mutate(full_df, region = gsub("Регион:", "", region),
                   appeal_ID = gsub("/Forum/Appeals/Details/", "", appeal_ID), 
                   appeal_date = gsub("Опубликовано:", "", appeal_date),
                   appeal_date = gsub("в", "", appeal_date),
                   response_date = gsub("в", "", response_date))
str(full_df_1)

full_df_1$region <- factor(full_df_1$region)
full_df_1$category <- factor(full_df_1$category)

write.csv(full_df_1, file = "Rospotrebnadzor_29_11_19.csv", fileEncoding = "UTF-8")


####### Сross tabulation

# Sum of categories
categories_sum <- table(full_df_1$category)
categories_sum <- categories_sum[order(categories_sum, decreasing = TRUE)]  

# Sum by regions
regions_sum <- table(full_df_1$region)
regions_sum <- regions_sum[order(regions_sum, decreasing = TRUE)]  


write.table(categories_sum, file = "categories_sum.txt", sep = ",", quote = FALSE, row.names = F)
write.table(regions_sum, file = "region.txt", sep = ",", quote = FALSE, row.names = F)


# Categories and regions. Not so good for the publication in the Word
categories_by_region <- xtabs(~ category + region, data = full_df_1)

#######################################
#######################################
#######################################
