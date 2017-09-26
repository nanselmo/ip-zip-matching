#change the directory to where the data/scripts live
setwd("/Users/nicoleanselmo/Desktop/LLK")
#remove prior objects from env
#rm(list=ls())
#remove prior plots
#dev.off()
#libraries
##to clean zipcodes
library('zipcode')
##dplyr package (select, group_by, count etc)
library('dplyr')
##graphical package
library('ggplot2')
library(devtools)
#install_github('arilamstein/choroplethrZip@v1.3.0')
library(choroplethrZip)

#only load the data if the dfs don't already exist
if (!exists("teacher_data_raw")) {
  teacher_data_raw <- read.csv("full_educator_ip_with_demographics.csv")
  teacher_data<-teacher_data_raw%>%rename(predicted_zip=zip)
  num_user_by_ip <- read.csv("num_of_users_by_ip.csv")
  
  #convert ip_mod and datetime_active column to POSIXct
  teacher_data$ip_mod<-as.POSIXct(strptime(teacher_data$ip_mod, format='%Y-%m-%d %H:%M:%S'))
  teacher_data$datetime_active<-as.POSIXct(strptime(teacher_data$datetime_active, format='%Y-%m-%d %H:%M:%S'))
  #convert predicted zip from int to character and add leading zero
  teacher_data$predicted_zip <- sprintf("%05d", teacher_data$predicted_zip)
  #convert address_zip to character too
  teacher_data$address_zip <- as.character(teacher_data$address_zip)
}




##############################################################################################
#                                 General Data Cleaning                                      #
##############################################################################################
#take out zip codes that are invalid (according to zipcode library)
teacher_data$clean_zip <- clean.zipcodes(teacher_data$address_zip)
teacher_data_clean_zips <- teacher_data[!is.na(teacher_data$clean_zip),]

#take out ip address starting with 18.85
teacher_data_clean_zips <- teacher_data_clean_zips[ grep("18.85", teacher_data_clean_zips$ip, invert = TRUE) , ]
#take out rows with mit.edu
teacher_data_clean_zips <- teacher_data_clean_zips[ grep("mit.edu", teacher_data_clean_zips$organization_url, invert = TRUE) , ]
#take out MIT zip codes
teacher_data_clean_zips <- teacher_data_clean_zips[ grep("2139", teacher_data_clean_zips$predicted_zip, invert = TRUE) , ]
#take out the early testing dates
teacher_data_clean_zips <- teacher_data_clean_zips%>%filter(teacher_data_clean_zips$datetime_active>as.POSIXct("2016-07-12"))

##############################################################################################
#                                 Teacher Conferences                                        #
##############################################################################################

#find teachers who were accessing at a conference away from their home school
# predicted zip != address zip 
## && more than 25 users from that predicted zip also used scratch in that hour
#21 instances
away_conferences<-teacher_data_clean_zips %>%
  filter(address_zip!=predicted_zip) %>%
  group_by(recent_login_date_hour = format(ip_mod, "%Y-%m-%d %H:00"), predicted_zip) %>%
  summarize(num_users = n()) %>% filter(num_users>=25)

#example of an "away" conference group 
##more than 25 teachers using in the same hour
##with a different predicted zip than their registered address)
suffolk_virginia_group <- teacher_data_clean_zips%>%filter(format(ip_mod, '%Y-%m-%d')=="2017-08-28", predicted_zip=="23851")


#teachers who were accessing from the same ip, at the same hour as 40 other teachers
#9 instances
conferences <- teacher_data_clean_zips %>% 
  filter(address_zip!=predicted_zip) %>%
  group_by(recent_login_date_hour = format(ip_mod, "%Y-%m-%d %H:00"), ip) %>%
  summarize(num_users = n()) %>% filter(num_users>=40)


##############################################################################################
#                                 1 IP address per Teacher                                   #
##############################################################################################
#teacher ips (first vs last vs most frequent) 

#function to get one ip per user 
#(maybe it should be the most frequent IP?)
single_ip <- function (user_df, method){
  slim_user_df<- user_df%>%select('user_id', 'ip', 'ip_mod', 'address_zip', 'predicted_zip')%>%
    rename(ip_net_address = ip)
  if (method == "most_frequent_ip"){
  user_df_frequent_ip <- merge(slim_user_df, 
                               num_user_by_ip, by="ip_net_address")
  users <- user_df_frequent_ip %>% group_by(user_id)%>%
    arrange(desc(ip_mod))
  #find the max, if there isn't a max, find the max ip_mod
  max_users<- users%>%filter(rank(-num_users, ties.method="first")==1)
  users_single_ip <- max_users
  }
  else if (method == "least_recent"){
    first_ip_login<- slim_user_df %>% group_by(user_id)%>% 
      top_n(-1, ip_mod)
    users_single_ip <- first_ip_login
  }
  else if (method == "most_recent"){
    last_ip_login<- slim_user_df %>% group_by(user_id)%>% 
      top_n(1, ip_mod)
    users_single_ip <- last_ip_login
    
  }
  else {
    stop("Please choose a valid method to group user ips")
  }
  return(users_single_ip)
}

#example to test the single_ip function
grouped_zip_example <- teacher_data_clean_zips%>%filter(format(ip_mod, '%Y-%m-%d')=="2017-08-08", clean_zip=="29072")
new_grouped_zip_example <- single_ip(grouped_zip_example, "most_frequent_ip")

##############################################################################################
#                                 Use Conferences & IP to clean data                         #
##############################################################################################
#remove conference instances
#add hour to teacher_data_clean_zips
teacher_data_clean_zips$recent_login_date_hour = format(teacher_data_clean_zips$ip_mod,
                                                        "%Y-%m-%d %H:00")
#away_conference_data<-merge(teacher_data_clean_zips, 
                       #away_conferences,
                       #by=c("recent_login_date_hour", "predicted_zip"),
                       #all = TRUE)

conference_data<-merge(teacher_data_clean_zips, 
                            conferences,
                            by=c("recent_login_date_hour", "ip"),
                            all = TRUE)

teacher_data_no_conferences<-conference_data%>%filter(is.na(num_users))

#make single ips using all three methods
clean_teacher_data_frequent_ip<-single_ip(teacher_data_no_conferences, "most_frequent_ip")
clean_teacher_data_most_recent_ip<-single_ip(teacher_data_no_conferences, "most_recent")
clean_teacher_data_oldest_ip<-single_ip(teacher_data_no_conferences, "least_recent")

###########################################################
#compare zip code accuracy
###########################################################

equalZips <- function(row){
  if(row[["address_zip"]]==row[["predicted_zip"]])
    return(TRUE)
  else
    return(FALSE)
}


#function to generate accuracy tables
get_ip_accuracy <- function(clean_teacher_ip_df){
  clean_teacher_ip_df$zipMatchBool<- apply(clean_teacher_ip_df,1, equalZips)
  ip_accuracy<-prop.table(table(clean_teacher_ip_df$zipMatchBool))
  return (ip_accuracy)
}

ip_accuracy_unclean_data<-get_ip_accuracy(teacher_data_clean_zips)
#36962 total observations (multiple ips per user)
#14.3 accuracy

ip_accuracy_frequent_ip<-get_ip_accuracy(clean_teacher_data_frequent_ip)
#15123 total observations
#we lose some ips because this method only uses ip address 
#that have been access by more than one user
#2709 true, #12414 false, 17.9% accuracy
#17.6% accuracy for normal conferences (>40 users as same ip in same hour)

ip_accuracy_most_recent_ip <- get_ip_accuracy(clean_teacher_data_most_recent_ip)
#16771 total observation
#17.0% accurate
#16.7% accurate for normal conferences (>40 users as same ip in same hour)

ip_accuracy_oldest_ip <- get_ip_accuracy(clean_teacher_data_oldest_ip)
#16771 total observations
#18.2 % accuracy
#18% accurate for normal conferences (>40 users as same ip in same hour)





##############################################################################################
#                                 Map Viz                                                    #
##############################################################################################
##zip code data exploration
#frequentZips <- teacher_data_clean_zips %>% count(clean_zip)
#hist(as.Date(visitor_zips$ip_mod), breaks="months")
#zips_for_choropleth<-frequentZips%>%rename(region = clean_zip, value=n)
#missing regions are just regions that don't have any data
#7629 zip codes from teacher data set
#3:3800 error
#3800:7620 error too
#state_zoom = "new york" works (and illinois, massachusetts)
#test_map<-zip_choropleth(zips_for_choropleth, 
               #state_zoom = "massachusetts",         
               #title      = "Scratch Teacher Account Zip Codes",
               #legend     = "# of Teachers per Zip") + coord_map()    
#print(test_map)

