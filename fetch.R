# Idea 1. Fetch the page for me as a user, and then use regex or somehting else on the file that gets returned
library(httr)
library(dplyr)
source("/data/functions/write_to_redshift.R") #this allows you to write to an S3 bucket and pull into Redshift

# Use the API.
# # by inspecting the page we've found my user id
# everydayHeroVicky<- GET("https://everydayhero.com/api/v2/pages/edc1e64c-0003-4000-8000-00000037bb45")
# everydayHeroVicky
# str(content(everydayHeroVicky))
# test<-content(everydayHeroVicky)

# get a list of the uuid of each team member
#go to their page, right click and say "view source" and look for "window.edh.trackable.page_id"
teamUuids<- data.frame(
  "name" = c("Vicky", "Bethany"),
  "uuid" = c("edc1e64c-0003-4000-8000-00000037bb45", "edc1e64c-0003-4000-8000-00000037bb39")
)


############    getting data ############ 
#1. find user's uuid and put them all into df
#2. for each user, 
#2.a GET their page and select the useful information
#2.b Write this into a DF
#3. write df to csv


#Fucntion to get the individual user's data
getUserInfo<- function(uuid){
  webURL<- paste0("https://everydayhero.com/api/v2/pages/",uuid)
  heroInfo<- content(GET(webURL))
  fitnessGoal<- heroInfo$page$fitness_goal
  userInfo<-data.frame("fitnessGoal" = (fitnessGoal))
  return(userInfo)
}

#function that gets all the teams data using getUserInfo for the individual
getTeamInfo <- function(uuids) {
  teamInfo <- data.frame()
  
  for (user in 1:nrow(uuids)) {
    userInfo <- uuids[user, ] %>%
      cbind(getUserInfo(uuids$uuid[user]))
    
    teamInfo <- teamInfo %>% rbind(userInfo)
  }
  return(teamInfo)
}
teamInfo <- getTeamInfo(teamUuids)

#write to csv
write.csv2(teamInfo, file = "teamInfo.csv", row.names = FALSE)


#Write to redshift
write_to_redshift(df = teamInfo, s3_folder = "vicky_banks", redshift_location = "dataforce_sandbox.vb_miles_refugees_team_info")
