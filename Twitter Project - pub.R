#Date: Originally Written in 05-14-2017
#Please excuse the messy code and lack of commenting. This codebase was strictly intended to be side project and really didn't consider that others might be interested in it 01-05-2019


library(twitteR)
#----------------------------------------------------------------------
#Start with the Twitter Authorization Process

consumerkey <- ""
consumerSecret <- ""
accesstoken <- ''
accesssecret <- ''

reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

options(RCurlOptions = list(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl')))
options(httr_oauth_cache=T) #This will enable the use of a local file to cache OAuth access credentials between R sessions

twitCred <- OAuthFactory$new(consumerKey=consumerkey,
                             consumerSecret=consumerSecret,
                             requestURL=reqURL,
                             accessURL=accessURL,
                             authURL=authURL)
twitCred$handshake()
#registerTwitterOAuth(twitCred) # Deprecated in the new version
setup_twitter_oauth(consumerkey, consumerSecret, accesstoken, accesssecret)
# Connected

---------------------------------------------------------------------------

#Get the first portion of the needed training data from my account and from dunmy account with 100% Bot fellowship
  
original_user <- getUser('josephshaheen')

getCurRateLimitInfo() # Check for rate limiting by Twitter

#registerTwitterOAuth(twitCred) #Sometimes We need to re-register
#tweetm <- updateStatus('testing') #Testing Writing
#followers_attr <- lookupUsers(friends) #This doesnt work
#followers_attr
training_followers_1 <- original_user$getFollowers() # Get all the friends of the first portion of training data. 
#length(training_followers_1) # Checking if we got them all
write.csv(twListToDF(training_followers_1),"joes_friends.csv") #twListToDF(class) takes the TwitteR class object and converts it to a DF
OU_twitter_followers_DF <- twListToDF(training_followers_1) #saved the twitter class object as a dataframe for use in the Bayesian package

#Let's convert that data DF into one which will be used in the Bayesian classification

Description_length_t1 <- nchar(OU_twitter_followers_DF$description) #vector with description length
url_existence_t1 <- !is.na(OU_twitter_followers_DF$url) 
name_length_t1 <- nchar(OU_twitter_followers_DF$name)
screen_name_length_t1 <- nchar(OU_twitter_followers_DF$screenName)
location_length_t1 <- nchar(OU_twitter_followers_DF$location)

#Construct the new data frame to be ready for the Bayes Analysis below/ Took Multiple lines to enhance readabilty

training_df_1 <- data.frame(Screen_name = OU_twitter_followers_DF$screenName, Bot = FALSE, Description_length = Description_length_t1, Status_count = OU_twitter_followers_DF$statusesCount)
training_df_1$followers_count <- OU_twitter_followers_DF$followersCount #followers count
training_df_1$favorites_count <- OU_twitter_followers_DF$favoritesCount #favorites count
training_df_1$friends_count <- OU_twitter_followers_DF$friendsCount #friends count
training_df_1$url_existence <- url_existence_t1 #url existence
training_df_1$name_length <- name_length_t1 #name length
training_df_1$creation_date <- OU_twitter_followers_DF$created #account creation date
training_df_1$protected_status <- OU_twitter_followers_DF$protected #protected status
training_df_1$verified_status <- OU_twitter_followers_DF$verified #verified status
training_df_1$screen_length <- screen_name_length_t1 #screen name length
training_df_1$location_length <- location_length_t1 #location length (existence) 
training_df_1$listed_count <- OU_twitter_followers_DF$listedCount #listed_count

#-------------------------------------------------------------------------------

#Now we would like to download the training data (part2) of the data set where we have a sample made up entiurely of known Bots


Bot_twitter_account <- getUser('TalentNewsMedia')

getCurRateLimitInfo() # Check for rate limiting by Twitter

#registerTwitterOAuth(twitCred) #Sometimes We need to re-register
#tweetm <- updateStatus('testing') #Testing Writing
#followers_attr <- lookupUsers(friends) #This doesnt work
#followers_attr
training_followers_2 <- Bot_twitter_account$getFollowers() # Get all the friends of the first portion of training data. 
#length(training_followers_2) # Checking if we got them all
write.csv(twListToDF(training_followers_2),"Bot_friends.csv") #twListToDF(class) takes the TwitteR class object and converts it to a DF
Bot_twitter_followers_DF <- twListToDF(training_followers_2) #saved the twitter class object as a dataframe for use in the Bayesian package

#Let's convert that data DF into one which will be used in the Bayesian classification

Description_length_t2 <- nchar(Bot_twitter_followers_DF$description) #vector with description length
url_existence_t2 <- !is.na(Bot_twitter_followers_DF$url) 
name_length_t2 <- nchar(Bot_twitter_followers_DF$name)
screen_name_length_t2 <- nchar(Bot_twitter_followers_DF$screenName)
location_length_t2 <- nchar(Bot_twitter_followers_DF$location)

#Construct the new data frame to be ready for the Bayes Analysis below/ Took Multiple lines to enhance readabilty

training_df_2 <- data.frame(Screen_name = Bot_twitter_followers_DF$screenName, Bot = TRUE, Description_length = Description_length_t2, Status_count = Bot_twitter_followers_DF$statusesCount)
training_df_2$followers_count <- Bot_twitter_followers_DF$followersCount #followers count
training_df_2$favorites_count <- Bot_twitter_followers_DF$favoritesCount #favorites count
training_df_2$friends_count <- Bot_twitter_followers_DF$friendsCount #friends count
training_df_2$url_existence <- url_existence_t2 #url existence
training_df_2$name_length <- name_length_t2 #name length
training_df_2$creation_date <- Bot_twitter_followers_DF$created #account creation date
training_df_2$protected_status <- Bot_twitter_followers_DF$protected #protected status
training_df_2$verified_status <- Bot_twitter_followers_DF$verified #verified status
training_df_2$screen_length <- screen_name_length_t2 #screen name length
training_df_2$location_length <- location_length_t2 #location length (existence) 
training_df_2$listed_count <- Bot_twitter_followers_DF$listedCount #listed_count

#Now we can combine both data sets into one training data set

training_set <- rbind(training_df_1, training_df_2)


#--------------------------------------------------------------------------
#Let's download our test data targeting the school of engineering
test_data_col_eng <- getUser("VolgenauSchool")
test_data_friends <- test_data_col_eng$getFollowers()
test_data_DF <- twListToDF(test_data_friends)
write.csv(test_data_DF,"test_engineering_friends.csv")

#attributes(test_data_DF) < -- lets me see the attributes of the DF
#str(DF) shows the structure

Description_length <- nchar(test_data_DF$description) #vector with description length
url_existence <- !is.na(test_data_DF$url) 
name_length <- nchar(test_data_DF$name)
screen_name_length <- nchar(test_data_DF$screenName)
location_length <- nchar(test_data_DF$location)

#Construct the new data frame to be ready for the Bayes Analysis below/ Took Multiple lines to enhance readabilty

test_df_new <- data.frame(Screen_name = test_data_DF$screenName, Bot = FALSE, Description_length, Status_count = test_data_DF$statusesCount)
test_df_new$followers_count <- test_data_DF$followersCount #followers count
test_df_new$favorites_count <- test_data_DF$favoritesCount #favorites count
test_df_new$friends_count <- test_data_DF$friendsCount #friends count
test_df_new$url_existence <- url_existence #url existence
test_df_new$name_length <- name_length #name length
test_df_new$creation_date <- test_data_DF$created #account creation date
test_df_new$protected_status <- test_data_DF$protected #protected status
test_df_new$verified_status <- test_data_DF$verified #verified status
test_df_new$screen_length <- screen_name_length #screen name length
test_df_new$location_length <- location_length #location length (existence) 
test_df_new$listed_count <- test_data_DF$listedCount #listed_count
test_df_new$Bot[1] <- "TRUE" #In order to make sure that there are 2 levels I have to make sure there is one acct. labelled as True

#----------------------------------------------------------------------------
#The Test data from the GMU school of Engineering is now ready for a naive bayes classification in terms of format

#We now have to choose an R Package that will allow us to learn from this data, then infer the status of accounts (bor or not on other accounts)
#There are several options http://people.math.aau.dk/~sorenh/software/gR/:
#gRain | Excellent Package
#bnlearn | Excellent Package
#gRim | Excellent Package
#gRc | Excellent Package
#e1071 | Excellent Package | Native naive Bayes implementation methods <-- use this one - faster implementation
#klar
#----------------------------------------------------------------------------
library(e1071)
library(caret)
library(bnlearn)

#seems like the date field is not natively being read by the model so removing it.
training_set$creation_date <- NULL
training_set_final <- training_set
training_set_final$Screen_name <- NULL
training_set_final$Bot <- as.factor(training_set_final$Bot)
training_set_final$url_existence <- as.factor(training_set_final$url_existence)
training_set_final$protected_status <- as.factor(training_set_final$protected_status)
training_set_final$verified_status <- as.factor(training_set_final$verified_status)
training_set_final$Description_length <- as.numeric(training_set_final$Description_length)
training_set_final$name_length <- as.numeric(training_set_final$name_length)
training_set_final$screen_length <- as.numeric(training_set_final$screen_length)
training_set_final$location_length <- as.numeric(training_set_final$location_length)
summary(training_set_final)
str(training_set_final)

disc_training <- discretize(training_set_final, method = 'interval', breaks = c(2,8,10,10,10,10,2,5,2,2,5,5,10), ordered = TRUE )
#------------------------------------------------------------------------
#Let's also re-format and dioscretize the test data to the same format as the training data

test_df_new$creation_date <- NULL
test_df_new_final <- test_df_new
test_df_new_final$Screen_name <- NULL
test_df_new_final$Bot <- as.factor(test_df_new_final$Bot)
test_df_new_final$url_existence <- as.factor(test_df_new_final$url_existence)
test_df_new_final$protected_status <- as.factor(test_df_new_final$protected_status)
test_df_new_final$verified_status <- as.factor(test_df_new_final$verified_status)
test_df_new_final$Description_length <- as.numeric(test_df_new_final$Description_length)
test_df_new_final$name_length <- as.numeric(test_df_new_final$name_length)
test_df_new_final$screen_length <- as.numeric(test_df_new_final$screen_length)
test_df_new_final$location_length <- as.numeric(test_df_new_final$location_length)
summary(test_df_new_final)
str(test_df_new_final)


disc_testing <- discretize(test_df_new_final, method = 'interval', breaks = c(2,8,10,10,10,10,2,5,2,2,5,5,10), ordered = TRUE )

#------------------------------------------------------------------------------------------------
#Now let's create some models based on training data

bnlearn_model.gs <- gs(disc_training)
bnlearn.hc_model <- hc(disc_training, score = "aic")
bnlearn.hc_model

graphviz.plot(bnlearn_model.gs,layout = 'neato')
graphviz.plot(bnlearn.hc_model,layout = 'neato')

compare(bnlearn_model.gs,bnlearn.hc_model)

#let's fit the parmeters based on the structure

fit_HC_model <- bn.fit(bnlearn.hc_model, disc_training)

disc_testing_wout_bot <- disc_testing
disc_testing_wout_bot$Bot <- NULL
HC_prediction_model <- predict(fit_HC_model, data = disc_testing_wout_bot , node = 'Bot')
HC_prediction_model

#--------------------------------------------------------------------
#Now let's trey the e1071 package and do a classic naive bayes which we can use for prediction

bot_model <- naiveBayes(Bot~.,data = disc_training) 
bot_model 
write.csv(as.table(bot_model), "naive_bayes.csv")  
str(bot_model) 
prediction_model_2 <- predict(bot_model, disc_testing) 
prediction_model_2
# Let's count how many Bots it identified

bot_count <- table(prediction_model_2)
bot_count #684 False and 510 True: Seems a little high!
#----------------------------------------------------------------------------------

twitter_url <- sub("^", "http://www.twitter.com/", test_df_new$Screen_name)
predict_data_set <- cbind(prediction_model_2 , test_df_new_final$Bot , twitter_url, test_df_new)
predict_data_set
str(prediction_model_2)
summary(prediction_model_2)
write.csv(predict_data_set, "prediction_model.csv")

  
  
  
  