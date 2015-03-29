## ps_scraper is a function to scrape the text of articles on Project Syndicate website
## http://www.project-syndicate.org/

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
        
        ## extract url for author's latest article
        
        for(i in 1:num_websites) {
                
                ## extract author's name for appending to beginning of article in masterText file
                rough_name <- str_replace(websites[i], "http://www.project-syndicate.org/columnist/", "")
                author_name <- str_replace_all(rough_name, "-", " ") 
                print(author_name)
                
                ## get url for most recent article from author's website
                url_author <- websites[i]
                xml_author <- htmlTreeParse(url_author, useInternalNodes = TRUE)
                article_node <- getNodeSet(xml_author, "//div[@id = \"tab-commentaries\"]/article[1]/a/@href")
                url_article <- paste("http://www.project-syndicate.org", toString(article_node[1]), sep = "")
                
                ## check to see if article is written in current month.  if not, get next article
                if(str_sub(url_article, -2, -1) != month){
                        article_node <- getNodeSet(xml_author, "//div[@id = \"tab-commentaries\"]/article[2]/a/@href")
                        url_article <- paste("http://www.project-syndicate.org", toString(article_node[1]), sep = "")
                        ## print error if second article month does not match current month
                        if(str_sub(url_article, -2, -1) != month){
                                error <- paste(author_name, "did not write an article for this month.", sep = " ")
                                print(error)
                        }
                }
                
                ## get article text
                xml_article <- htmlTreeParse(url_article, useInternalNodes = TRUE)
                title <- xpathSApply(xml_article, "//header/h1", xmlValue)
                article <- xpathSApply(xml_article, "//div[@itemprop = \"articleBody\"]/child::p", xmlValue)
                article <- paste(article, collapse = " ")
                article <- paste(author_name, "-", title, article, "{{split}}", sep = " ")
                
                ## append article to master_text
                master_text <- append(master_text, article)
        }
        
        ## create output text file to working diretory
        writeLines(master_text, "ps.txt")
        print("ps.txt file created")
}
