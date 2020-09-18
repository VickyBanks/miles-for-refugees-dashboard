if(getwd() !='/Users/banksv03/Documents/Projects/miles-for-refugees-dashboard'){setwd("/data/users/VickyBanks/miles-for-refugees-dashboard")}

library(rvest)
library(dplyr)
library(stringr)
source("/data/functions/write_to_redshift.R") #this allows you to write to an S3 bucket and pull into Redshift


#/html/body/div[3]/div[1]/div[2]/div[1]/div/div/h3   lizzie = div[1]/div/div/h3
#//div[contains(@id,'activity-feed')]
#[...document.querySelectorAll('#activity-feed .donation div.article--header h3')].map(node => node.innerText)

# 1. Create a df with the user and their slug
heroPages<- data.frame(
  "name" = c("Vicky", "Bethany","Emily",
             "Helen","Beth", "Laura",
             "Charlotte", "Sohail", "Izzy"),
  "slug" = c('vicky-8', 'bethany-11','emily-70',
             'helen-39', 'beth-24', 'laura-92',
             'charlotte-52', 'sohail','izzy-2')
)

# heroPages<- data.frame(
#   "name" = c("Vicky"),
#   "slug" = c('vicky-8')
# )
# 2. Loop through the users, get their donors and put into a df
donorDF<-data.frame()
for(name in 1:nrow(heroPages)) {
  print(heroPages$name[name])
  heroPageUrl <-
    paste0(
      "https://miles-for-refugees-2020.everydayhero.com/uk/",
      heroPages$slug[name],
      "/activity-feed"
    )
  
  htmlData <- read_html(heroPageUrl)
  
  #Get donar names
  donorNames <<- htmlData %>%
    rvest::html_nodes('body') %>%
    xml2::xml_find_all(
      "//div[contains(@id,'activity-feed')]
    //div[contains(@class, 'donation')]
    /div[contains(@class, 'article--content')]
    /div[contains(@class, 'article--header')]
    /h3"
    ) %>%
    rvest::html_text()
  
  #Get donation values
  donationInfo <- htmlData %>%
    rvest::html_nodes('body') %>%
    xml2::xml_find_all(
      "//div[contains(@id,'activity-feed')]
    //div[contains(@class, 'donation')]
    /div[contains(@class, 'article--content')]
    /div[contains(@class, 'article--header')]
    /small[contains(@class, 'donation')] 
    "
    ) %>%
    rvest::html_text()
  donationInfo
  
  donation<-data.frame()
  for(row in 1:length(donorNames)){
    donationInfoSplit<-data.frame(str_split(donationInfo[row],"\n"))
    donationInfoSplit
    
    donationValue<-data.frame("value" = str_split(donationInfoSplit[3,]," "))
    donationValue<-donationValue[10,]
    donationValue
    
    donationGiftAid<-data.frame("gift_aid" = str_split(donationInfoSplit[5,]," "))
    donationGiftAid<-if(is.na(donationGiftAid[12,])== TRUE){'£0.00'} else{donationGiftAid[12,]}
    donationGiftAid
    
    donation<- donation %>% rbind(cbind(donationValue, donationGiftAid))
    donation
  }
  
  
  if(length(donorNames)>0){
    print(length(donorNames))
    donorDF <- donorDF%>%
      rbind(
        cbind(data.frame("name" = heroPages$name[name], donorNames),
              donation)
        )
    }
  else("no donors")

}         
knitr::kable(donorDF%>%head(10))

getwd()
if(getwd() =='/Users/banksv03/Documents/Projects/miles-for-refugees-dashboard'){
  write.csv(donorDF, file = "donorDF.csv", row.names = FALSE)
} else {
  write_to_redshift(df = donorDF, s3_folder = "vicky_banks", redshift_location = "dataforce_sandbox.vb_miles_refugees_donors")
}



