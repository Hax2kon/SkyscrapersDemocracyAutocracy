
Sky_Scrape <- function(url){
  library(rvest);library(zoo)
  
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  
  building <- read_html(url) %>%
   html_nodes(xpath='//*[@id="figures-section"]') %>%
   html_text()

  building <- gsub("Â","",building)
  building <- trim(unlist(strsplit(gsub("Å","",gsub("#","Number",gsub("[\t]", "", building))),"[\r\n]")))
  building <- building[which(nchar(building)>0)]
  building <- data.frame(cbind(trim(gsub(" ft$","", gsub("[[:punct:]]","", gsub(" m /","", gsub('[[:digit:]]', '',building))))),
                               trim(gsub(":","",gsub('[[:alpha:]]', '',building)))),stringsAsFactors = FALSE)
  building[,1] <- ifelse(building[,1]=="",NA,building[,1])
  
  names(building) <- c("variable","value")
  building[,1] <- na.locf(building[,1],na.rm=FALSE)
  

  if(class(try(read_html(url) %>%
               html_nodes(xpath='//*[@id="table-figures"]') %>% html_table()))!="try-error"){
    
    buildingtab <- read_html(url) %>%
      html_nodes(xpath='//*[@id="table-figures"]') %>% html_table()
    
    
    if(length(buildingtab==1)){
      buildingtab <- as.data.frame(buildingtab[[1]])
      buildingtab[,1] <- gsub("#","Number",buildingtab[,1])
      buildingtab <- data.frame(cbind(trim(gsub(" ft$","", gsub("[[:punct:]]","", gsub(" m /","", gsub('[[:digit:]]', '',buildingtab[,1]))))),
                                      trim(gsub(":","",gsub('[[:alpha:]]', '',buildingtab[,1])))),stringsAsFactors = FALSE)
      names(buildingtab) <- c("variable","value")
      building <- rbind(building,buildingtab)
    }}
  
  
  building$value <- trim(building$value)
  building <- building[which(building$value!=""),]
  building <- building[which(nchar(building$variable)<90),]

  #Extract facts
  facts <- read_html(url) %>%
    html_nodes(xpath='/html/body/div[2]/div[3]/div[1]/table') %>%
    html_table()
  if(length(facts)==0){
    facts <- read_html(url) %>%
    html_nodes(xpath='/html/body/div[2]/div[4]/div[1]/table') %>%
    html_table()
  }
  if(length(facts)==0){
    facts <- data.frame(cbind(variable="facts",value="no data"),stringsAsFactors = FALSE)
    } else {
    facts <- facts[[1]]
    names(facts) <- c("variable","value")
    facts$variable <- gsub("[\t]","", gsub("[\r\n]", "",facts$variable))
    facts$value <- gsub("[\t]","", gsub("[\r\n]", "",facts$value))
    
  }
  
  #Extract companies involved
  specs <- try(read_html(url) %>%
                 html_nodes(xpath='/html/body/div[2]/div[3]/div[2]/table') %>%
                 html_table())
  
  if(class(specs)=="try-error"){
    specs <- data.frame(cbind(variable="specs",value="no data"),stringsAsFactors = FALSE)
  } else { if(length(specs)==0){
    specs <- data.frame(cbind(variable="specs",value="no data"),stringsAsFactors = FALSE)
  } else{ 
    specs <- specs[[1]]
    names(specs) <- c("variable","value")
    specs$variable <- gsub("â€¢","",gsub("[\t]","", gsub("[\r\n]", "",specs$variable)))
    specs$value <- gsub("â€¢","",gsub("[\t]","", gsub("[\r\n]", "",specs$value)))
  }
  }
  
  specs$temp     <- grepl("•",specs$variable)
  specs$variable <- trim(gsub("•","",specs$variable))
  specs$temp2    <- ifelse(specs$temp==FALSE,specs$variable,NA)
  specs$temp2    <- na.locf(specs$temp2,na.rm=FALSE)
  specs$variable <- ifelse(specs$temp==TRUE,paste0(specs$temp2,"_subcat_",specs$variable),specs$variable)
  specs[,c("temp","temp2")] <- NULL
  
  #Extract the text
  text <- read_html(url) %>%
    html_nodes(xpath='//*[@id="about-text"]') %>%
    html_text()
  
  if(length(text)==0){
    text <- cbind(variable="text",value="Nada")
    } else {
      text <- cbind(variable="text",value=text)
    }
  
  #Finalize data
  data <- data.frame(t(rbind(building,facts,specs,text)),stringsAsFactors=FALSE)
  colnames(data) <- as.character(data[1,])
  data <- data[2,]
  data$url <- gsub(".*building/","",url)
  
  return(data)
}
