library(tm)
library(XML)
library(RCurl)

#test to see which nodes we need in our list
blsurl = "http://www.bls.gov/oes/current/oes172112.htm"

blsdoc = htmlParse(blsurl)

titleNode = getNodeSet(blsdoc, "//title")
titleNode

title1<-xpathSApply(blsdoc,"//title",xmlValue)

tableNodes = getNodeSet(blsdoc, "//table")

tableNodes[[2]] #Employment estimates

tableNodes[[3]]


table1 <- readHTMLTable(tableNodes[[2]], header=TRUE, trim=TRUE, colClasses=c("Currency","Percent","Currency","Currency","Percent"))

table2 <- readHTMLTable(tableNodes[[3]], header=TRUE, trim = TRUE, colClasses=c("character",rep("Currency",5)))



#### functions for the above tests


oes.readJob = 
  function(u, doc = htmlParse(u))
  {
    lis =  list(Title = oes.getJobTitle(doc),
                Employment = oes.getEmploymentTable(doc),
                Salary = oes.getSalaryTable(doc))
    ##Added a return to get this going
    return(lis)
  }

oes.getEmploymentTable =
  function(doc)
  {
    tableNodes = getNodeSet(doc, "//table")
    table1 <- readHTMLTable(tableNodes[[2]], header=TRUE, trim=TRUE, colClasses=c("Currency","Percent","Currency","Currency","Percent"))
  }


oes.getSalaryTable = 
  function(doc)
  { tableNodes = getNodeSet(doc, "//table")
  table2 <- readHTMLTable(tableNodes[[3]], header=TRUE, trim = TRUE, colClasses=c("character",rep("Currency",5)))
  }

oes.getJobTitle =
  function(doc)
  {
    title1<-xpathSApply(doc,"//title",xmlValue)
  }

###see if it works

u="http://www.bls.gov/oes/oes110000.htm"
oes.readJob(u)

####test to get links from main page
u = "http://www.bls.gov/oes/current/oes_stru.htm#17-0000"

oesdoc = htmlParse(u)
links = getNodeSet(oesdoc, "//td[@class = 'main-content']//li//a/@href")
links = as.character(links)
#clean link data
newlinks<-gsub("(f|ht)(tp)(s?)(://)(.*)[.|/][a-z]+(/)[oes]+(/)[#|0-9]", "", links)
newlinks1<-regexpr("^oes[0-9]+[.][a-z]+",newlinks)
links=substring(newlinks,newlinks1,newlinks1+attr(newlinks1,"match.length")-1) 
link<-links[links != ""]

links = getRelativeURL(link,baseURL = "http://www.bls.gov/oes/current/")
links





oes.getJobLinks =
  function(doc, baseURL = "http://www.bls.gov/oes/current/")
  {
    if(is.character(doc))
      doc = htmlParse(doc)
    
    links = getNodeSet(doc, "//td[@class = 'main-content']//li//a/@href")
    links = as.character(links)
    
    newlinks<-gsub("(f|ht)(tp)(s?)(://)(.*)[.|/][a-z]+(/)[oes]+(/)[#|0-9]", "", links)
    newlinks1<-regexpr("^oes[0-9]+[.][a-z]+",newlinks)
    links=substring(newlinks,newlinks1,newlinks1+attr(newlinks1,"match.length")-1) 
    link<-links[links != ""]
    
    links = getRelativeURL(link,baseURL = "http://www.bls.gov/oes/current/")
    ## Had to add this line to make sure the /oes/current showed up.  For some reason it wasn't showing
    ## with getRealtiveURL, weird but it worked
    links=gsub("\\/oes\\/","/oes/current/",links)
    ## Added a return to get it to work
    return(links)
  }

links=oes.getJobLinks(oesdoc)
links


###stops working here

## Just checked the first 10, still getting NA's by coercion.  That's probably from the read in.
posts = lapply(links[1:10], oes.readJob)
posts[[1]]



oes.readPage =
  function(u, doc = htmlParse(u), links = oes.getJobLinks(doc, baseURL),
           baseURL = "http://www.bls.gov/oes/current/")
  {
    if(is.character(doc))
      doc = htmlParse(doc)
    lapply(links, oes.readJob)
  }


Posts<-oes.readPage(u)



