if(getwd() !='/Users/banksv03/Documents/Projects/miles-for-refugees-dashboard'){setwd("/data/users/VickyBanks/miles-for-refugees-dashboard")}

library(httr)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)
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
  "name" = c("Vicky", "Bethany","Emily", 
             "Helen","Beth", "Laura",
             "Charlotte", "Sohail", "Izzy"),
  "uuid" = c("edc1e64c-0003-4000-8000-00000037bb45", "edc1e64c-0003-4000-8000-00000037bb39", "edc1e64c-0003-4000-8000-00000037d5fb",
             "edc1e64c-0003-4000-8000-00000037bb67", "edc1e64c-0003-4000-8000-00000037d41c", "edc1e64c-0003-4000-8000-00000037d28e",
             "edc1e64c-0003-4000-8000-00000037d35c", "edc1e64c-0003-4000-8000-00000037d320", "edc1e64c-0003-4000-8000-00000037d1d3")
)

# teamUuids<- data.frame(
#   "name" = c("Emily"),
#   "uuid" = c("edc1e64c-0003-4000-8000-00000037d5fb")
# )

#Team donanations so far
#moneyRaised<- read_csv("donation_totals_pre_2020-09-11.csv")
moneyRaised<- read_csv("moneyRaised.csv")
moneyRaised$date<- ymd(as.character(moneyRaised$date))


#2 get the user's info. 
#Function to process each activity and return the data needed
processActivity<- function(activity) {
  processedActivity<- data.frame(
    "type" = activity$type,
    "startedAt" = activity$started_at,
    "coordinate_lat" = if(is.null(activity$coordinate)){0}else{activity$coordinate$lat},
    "coordinate_long" = if(is.null(activity$coordinate)){0}else{activity$coordinate$lon},
    "distanceMetres" = activity$distance_in_meters,
    "durationSec" = activity$duration_in_seconds
  )
  return(processedActivity)
}

#function to take in each activity and make a df of the user's summary of that activity.
#if statement for if they haven't done that actitvity
#eval(parse(text = paste0( parts allows the cols to be named with the acvitity
activitySummaryFunction<- function(activityType, activitySummary){
  
  #Check if they've done a walk and make the DF accordingly
  if (length(activitySummary) > 0) {
    activitySummary<- eval(parse(text = paste0(
      "data.frame(",
      activityType,"_duration_total=", activitySummary$duration_in_seconds,",",
      activityType,"_calories_total =", activitySummary$calories,",",
      activityType,"_distance_total =", activitySummary$distance_in_meters,")")))
  }
  else{
    activitySummary<- eval(parse(text = paste0(
      "data.frame(",
      activityType,"_duration_total=0",",",
      activityType,"_calories_total =0,",
      activityType,"_distance_total =0)")))
  }
  return(activitySummary)
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
    "fitnessGoal" = round(heroInfo$page$fitness_goal*1609.34,2),
    "amount" = round(heroInfo$page$amount$cents/100,2),
    "targetAmmount" = round(heroInfo$page$target_cents/100,2),
    "image" = heroInfo$page$image$facebook_xl_image_url)
 
  #find the summary for each actitivity the user may have done
  userInfo<-userInfo%>%
    cbind(activitySummaryFunction("walk",heroInfo$page$fitness_activity_overview$walk)) %>%
    cbind(activitySummaryFunction("run",heroInfo$page$fitness_activity_overview$run)) %>%
    cbind(activitySummaryFunction("bike",heroInfo$page$fitness_activity_overview$bike))
  
  #get the money raised
  dailyDontionTotal<<-data.frame(
    "date" = Sys.Date(),
    "name" = heroInfo$page$name,
    "totalRaised" = heroInfo$page$amount$cents
  )
  
  #get the activity data using the pageID found earlier
  activityWebURL<-paste0("https://everydayhero.com/api/v2/search/fitness_activities?page_id=",pageID)
  activityInfo<<- content(GET(activityWebURL))
  
  userActivities <- data.frame()
  if (length(activityInfo$fitness_activities) > 0) {
    for (row in 1:length(activityInfo$fitness_activities)) {
      activity <- processActivity(activityInfo$fitness_activities[[row]])
      userActivities <- userActivities %>% rbind(activity)
    }
  }
    else{}

  return(list(
    "userInfo" = userInfo,
    "userActivity" = userActivities,
    "dailyDonationTotal" = dailyDontionTotal
  ))
  
}

teamInfo<-data.frame()
allUserActivities<-data.frame()

for (user in 1:nrow(teamUuids)) {
  uuid<- teamUuids$uuid[user]
  name<- teamUuids$name[user]
  print(teamUuids$name[user])
  
  output<<-getUserInfo(teamUuids$uuid[user])
  
  teamInfo<- teamInfo %>% rbind(cbind(uuid,output[[1]]))
  
  if(length(output[[2]])>0){
  allUserActivities<- allUserActivities %>% rbind(cbind(name,output[[2]]))
  }
  
  moneyRaised <- moneyRaised %>% rbind(output[[3]])
}

#convert to char otherwise it won't push to redshift
moneyRaised$date<- as.character(moneyRaised$date)
getwd()
write.csv(moneyRaised, file = "moneyRaised.csv", row.names = FALSE)
if(getwd() =='/Users/banksv03/Documents/Projects/miles-for-refugees-dashboard'){
  write.csv(teamInfo, file = "teamInfo.csv", row.names = FALSE)
  write.csv(allUserActivities, file = "allUserActivities.csv", row.names = FALSE)
} else {
  write_to_redshift(df = teamInfo, s3_folder = "vicky_banks", redshift_location = "dataforce_sandbox.vb_miles_refugees_team_info")
  write_to_redshift(df = allUserActivities, s3_folder = "vicky_banks", redshift_location = "dataforce_sandbox.vb_miles_refugees_user_activities")
  write_to_redshift(df = moneyRaised, s3_folder = "vicky_banks", redshift_location = "dataforce_sandbox.vb_miles_refugees_money_raised")
  }






