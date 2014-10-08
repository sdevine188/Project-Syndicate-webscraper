## PSscraper is a function to scrape the text of articles on Project Syndicate website
## http://www.project-syndicate.org/

## the PSscraper function takes a two-digit month in quotes as its argument
## for example, for August, enter: PSscraper("08")

## PSscraper will then scrape the articles written that month (if any) by selected authors

## note that PSscraper is designed to scrape articles for the current or last month 
## for example, to scrape articles published in August, PSscraper should be run in August or September
## otherwise, PSscraper will not consistently scrape articles from the specified month 

PSscraper <- function(month) {
        
        ## first set working directory
                
        ## load XML library
        library(XML)
        
        ## create empty list to which all articles will be appended
        masterText <- c()
        
        ## load websites for selected authors 
        ## selected author's websites are listed in stand-alone .csv file
        ## this file has one column, and in each row is the url for the author's website
        ## for instance, for Michael Boskin's website, the url reads: http://www.project-syndicate.org/columnist/michael-boskin
        ## note that the list of selected authors can be modified to scrape articles from different authors
        
        websitesList <- read.csv("PSwebsites.csv", header = TRUE)
        websites <- unlist(websitesList)
        numWebsites <- length(websites)
        
        ## extract url for author's latest article
        
        for(i in 1:numWebsites) {
                
                ## scrape HTML from author's website
                fileUrl <- websites[i]
                doc <- htmlTreeParse(fileUrl, useInternal = TRUE)
                saveXML(doc, "authorpage.txt")
                authorpage <- file("authorpage.txt")
                text <- readLines(authorpage)
                
                ## extract and clean url for most recent article 
                urlList <- grep("/commentary", text, value = TRUE)
                url <- urlList[1]
                cleaningUrl1 <- sub("<article class.+<a href=\"", "", url)
                cleaningUrl2 <- sub("\">", "", cleaningUrl1)
                cleanUrl <- paste("http://www.project-syndicate.org", cleaningUrl2, sep = "")
                
                ## extract author's name for appending to beginning of article in masterText file
                roughNames <- gsub("http://www.project-syndicate.org/columnist/", "", websites)
                roughNames2 <- gsub("--", " ", roughnames)
                authorName <- gsub("-", " ", roughnames2)
                print(authorName)
                
                ## navigate to latest article website
                ## check to see if latest article is from current (specified) month
                               
                fileUrl <- cleanUrl
                doc <- htmlTreeParse(fileUrl, useInternal = TRUE)
                saveXML(doc, "article.txt")
                article <- file("article.txt")
                xmltext <- readLines(article)
                articleDateList <- grep("datetime", xmltext, value = TRUE)
                articleDate <- articleDateList[1]
                cleaningArticleDate1 <- sub("<time pubdate=\"pubdate\" itemprop=\"datePublished\" datetime=\"", "", articleDate)
                cleaningArticleDate2 <- strsplit(cleaningArticleDate1, "-")
                cleaningArticleDate3 <- unlist(cleaningArticleDate2)
                articleMonth <- cleaningArticleDate3[2]
                
                ## extract article text if latest article is from current (specified) month
                if(articleMonth == month) {
                        text <- xpathSApply(doc, "//div[@class='body']", xmlValue)
                        text2 <- paste(text, "{{split}}")
                        text3 <- paste(authorName, text2)
                        masterText <- append(masterText, text3)
                } 
                
                ## if latest article is not from current (specified) month
                ## return to list of article urls from author's website
                ## use the same code from above to select the second most recent article
                
                ## for instance, if you scraped the website in mid-September
                ## and were looking for articles from August
                ## some authors might have published a September article already
                ## which would then be selected by the code above as the most recent article
                ## in this case, selecting the second most recent article would find their August article
                
                if(articleMonth != month) {
                        url2 <- urlList[5]
                        cleaningUrl3 <- sub("</article><article class.+<a href=\"", "", url2)
                        cleaningUrl4 <- sub("\">", "", cleaningUrl3)
                        cleanUrl <- paste("http://www.project-syndicate.org", cleaningUrl4, sep = "")
                        fileUrl <- cleanUrl
                        doc <- htmlTreeParse(fileUrl, useInternal = TRUE)
                        saveXML(doc, "article.txt")
                        article <- file("article.txt")
                        xmltext <- readLines(article)
                        articleDateList <- grep("datetime", xmltext, value = TRUE)
                        articleDate <- articleDateList[1]
                        cleaningArticleDate1 <- sub("<time pubdate=\"pubdate\" itemprop=\"datePublished\" datetime=\"", "", articleDate)
                        cleaningArticleDate2 <- strsplit(cleaningArticleDate1, "-")
                        cleaningArticleDate3 <- unlist(cleaningArticleDate2)
                        articleMonth <- cleaningArticleDate3[2]
                        if(articleMonth == month) {
                                text <- xpathSApply(doc, "//div[@class='body']", xmlValue)
                                text2 <- paste(text, "{{split}}")
                                text3 <- paste(authorName, text2)
                                masterText <- append(masterText, text3)
                        } 
                        
                        ## print error if the second most recent article is not from the current (specified) month either
                        
                        if(articleMonth != month) {
                                error <- paste(authorName, "did not write an article for this month")
                                print(error)
                        }
                }
        }
        
        ## create output text file 
        masterTextFile <- file("PS.txt")
        writeLines(masterText, masterTextFile)
        close(masterTextFile)
        print("PS file created")
}

