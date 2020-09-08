library(httr)
library(dplyr)
library(purrr)
source("/data/functions/write_to_redshift.R") #this allows you to write to an S3 bucket and pull into Redshift

# Use the API.
# # by inspecting the page we've found my user id
#Get function pulls the list of lists from this page. Basically a summary of me the user.
everydayHeroVicky<- GET("https://everydayhero.com/api/v2/pages/edc1e64c-0003-4000-8000-00000037bb45")
test<-content(everydayHeroVicky)


############    getting data ############ 
#1. find user's uuid and put them all into df
#2. for each user, 
#2.a GET their page and select the useful information
#2.b Write this into a DF
#3. write df to csv


# 1. get a list of the uuid of each team member
#go to their page, right click and say "view source" and look for "window.edh.trackable.page_id"
teamUuids<- data.frame(
  "name" = c("Vicky", "Bethany"),
  "uuid" = c("edc1e64c-0003-4000-8000-00000037bb45", "edc1e64c-0003-4000-8000-00000037bb39")
)

#2. 
#Function to process each activity and return the data needed
processActivity<- function(activity) {
  processedActivity<- data.frame(
    "type" = activity$type,
    "startedAt" = activity$started_at,
    "coordinate" = paste0("(",activity$coordinate$lat,",",activity$coordinate$lon,")"),
    "distanceMetres" = activity$distance_in_meters,
    "durationSec" = activity$duration_in_seconds
    
  )

  return(processedActivity)
}

#Fucntion to get the individual user's data
getUserInfo<- function(uuid) {
  #goes to the user summary page and gets summary data
  summaryWebURL<- paste0("https://everydayhero.com/api/v2/pages/",uuid)
  heroInfo<<- content(GET(summaryWebURL))
  pageID<- heroInfo$page$id
  
  #This pulls in the summary info from the user page
  userInfo<- data.frame(
    "name" = heroInfo$page$name,
    "fitnessGoal" = heroInfo$page$fitness_goal,
    "amount" = round(heroInfo$page$amount$cents/100,2),
    "image" = heroInfo$page$image$facebook_xl_image_url
  )
  
  #get the activity data using the pageID found earlier
  activityWebURL<-paste0("https://everydayhero.com/api/v2/search/fitness_activities?page_id=",pageID)
  activityInfo<<- content(GET(activityWebURL))
  
  userActivities<- data.frame()
  for(row in 1:length(activityInfo$fitness_activities)){
    
    activity<-processActivity(activityInfo$fitness_activities[[row]])
    userActivities<- userActivities %>% rbind(activity)
  }
  return(list(
    "userInfo" = userInfo,
    "userActivity" = userActivities
  ))
  
}

teamInfo<-data.frame()
userActivities<-data.frame()

for (user in 1:nrow(teamUuids)) {
  uuid<- teamUuids$uuid[user]
  output<<-getUserInfo(teamUuids$uuid[user])
  
  teamInfo<- teamInfo %>% rbind(output[[1]])
  userActivities<- userActivities %>% rbind(cbind(uuid,output[[2]]))
}
userActivities
teamInfo


getwd()
if(getwd() =='/Users/banksv03/Documents/Projects/miles-for-refugees-dashboard'){
  write.csv2(teamInfo, file = "teamInfo.csv", row.names = FALSE)
} else {write_to_redshift(df = teamInfo, s3_folder = "vicky_banks", redshift_location = "dataforce_sandbox.vb_miles_refugees_team_info")}

