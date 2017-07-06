## ps_scraper is a function to scrape the text of articles on Project Syndicate website
## http://www.project-syndicate.org/

setwd("C:/Users/Stephen/Desktop/R/Project-Syndicate-webscraper")

## the ps_scraper function takes a two-digit month in quotes as its argument
## for example, for August, enter: ps_scraper("08")

## ps_scraper will then scrape the articles written that month (if any) by selected authors

## note that ps_scraper is designed to scrape articles for the current or last month 
## for example, to scrape articles published in August, ps_scraper should be run in August or September
## otherwise, ps_scraper will not consistently scrape articles from the specified month 

ps_scraper <- function(month) {
        
        ## first set working directory
        
        ## load XML library
        library(XML)
        library(stringr)
        library(rvest)
        
        ## create empty list to which all articles will be appended
        master_text <- c()
        
        ## load websites for selected authors 
        ## selected author's websites are listed in stand-alone .csv file
        ## this file has one column, and in each row is the url for the author's website
        ## for instance, for Michael Boskin's website, the url reads: http://www.project-syndicate.org/columnist/michael-boskin
        ## note that the list of selected authors can be modified to scrape articles from different authors
        
        websites_list <- read.csv("PSwebsites.csv", header = TRUE)
        websites <- apply(websites_list, 1, function(x) as.character(x))
        num_websites <- length(websites)
        article_num <- 0
        month_list <- c("January", "February", "March", "April", "May", "June", "July", "August", "September",
                           "October", "Novermber", "December")
        
        ## extract url for author's latest article
        
        for(i in 1:num_websites) {
                
                ## extract author's name for appending to beginning of article in masterText file
                rough_name <- str_replace(websites[i], "http://www.project-syndicate.org/columnist/", "")
                author_name <- str_replace_all(rough_name, "-", " ") 
                # print(author_name)
                print(str_c(author_name, " is ", i, " of ", num_websites))
                
                ## get url for most recent article from author's website
                url_author <- websites[i]
                html <- read_html(url_author)
                article_subdomain <- html %>% html_nodes("[id = tab-commentaries]") %>% 
                        html_nodes("article") %>% .[[1]] %>%
                        html_nodes("a") %>% 
                        html_attr("href") %>% .[[1]]       
                url_article <- paste("http://www.project-syndicate.org", toString(article_subdomain), sep = "")

                ## check to see if article is written in current month.  if not, get next article
                if(str_sub(url_article, -2, -1) != month){
                        print("getting previous article")
                        article_subdomain <- html %>% html_nodes("[id = tab-commentaries]") %>% 
                                html_nodes("article") %>% .[[2]] %>%
                                html_nodes("a") %>%
                                html_attr("href") %>% .[[1]]               
                        url_article <- paste("http://www.project-syndicate.org", toString(article_subdomain), sep = "")

                        ## print error if second article month does not match current month
                        if(str_sub(url_article, -2, -1) != month){
                                error <- paste(author_name, "did not write an article for this month.", sep = " ")
                                print(error)
                                next
                        }
                }
                
                ## get article text
                html_article <- read_html(url_article)
                title <- html_article %>% html_nodes("header") %>%
                        html_nodes("[itemprop = headline]") %>%
                        html_text()
                article <- html_article %>% html_nodes("[itemprop = articleBody]") %>% 
                        html_nodes("p") %>%
                        html_text()
                
                # remove random advertisements in article text
                article <- article[!grepl("PS On Point: Your review",
                                           article)]
                article <- str_replace(article, "Project Syndicate needs your help to provide readers everywhere equal access to the ideas and debates shaping their lives.", "")
                
                article <- paste(article, collapse = " ")
                article_num <- article_num + 1
                article_month <- month_list[as.numeric(as.character(month))]
                article <- paste("Project Syndicate - ", article_month, " - ", "Article ", article_num, " of ", 
                                 "zztotalarticleszz", ".", author_name, ". ", title, ". ", article, "{{split}}", sep = " ")
                
                ## append article to master_text
                master_text <- append(master_text, article)
        }
        
        print("out of author loop, now cleaning and writing file")
        
        # str_replace to input final article count
        master_text <- str_replace(master_text, "zztotalarticleszz", as.character(article_num))
        
        ## create output text file to working diretory
        writeLines(master_text, "ps.txt")
        print("ps.txt file created")
}
