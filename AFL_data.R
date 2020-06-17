library(fitzRoy)
library(dplyr)
library(data.table)
library(PlayerRatings)
library(ggplot2)
library(plotly)
library(lubridate)
library(reshape2)
# Get Football Draw
fixture<-get_fixture(2020)

##########----- Gather Data from fitZroy package -----########## 
# player stats
dat <- update_footywire_stats()
## betting data
betting_odds<-get_footywire_betting_odds(
  start_season = "2010",
  end_season = lubridate::year(Sys.Date()))
## Get match results
results<-get_match_results()

##########----- Clean and merge results with stats -----########## 

  # Create an index of the rows you want with duplications
res_idx <- rep(1:nrow(results), 2)
  # Use that index to genderate your new data frame
results_df <- results[res_idx,]
  # Add variables for joining
res <- results_df%>%
  group_by(Game)%>%
  filter(Season > 2009)%>%
  mutate(num = row_number())%>%
  mutate(Status = ifelse(num == 1, "Home", "Away"))%>%
  mutate(Team = ifelse(num == 1, Home.Team, Away.Team))%>%
  mutate(Opposition = ifelse(Team == Home.Team, Away.Team, Home.Team))%>%
  mutate(goals = ifelse(Team == Home.Team, Home.Goals, Away.Goals))%>%
  mutate(behinds = ifelse(Team == Home.Team, Home.Behinds, Away.Behinds))%>%
  mutate(points = ifelse(Team == Home.Team, Home.Points, Away.Points))%>%
  mutate(opp_goals = ifelse(Team == Home.Team, Away.Goals, Home.Goals))%>%
  mutate(opp_behinds = ifelse(Team == Home.Team, Away.Behinds, Home.Behinds))%>%
  mutate(opp_points = ifelse(Team == Home.Team, Away.Points, Home.Points))%>%
  mutate(Margin = points - opp_points) %>% 
  ungroup()%>%
  select(Date, Season, Team, goals, behinds, points, opp_goals, opp_behinds, opp_points, Margin)
library(tidyverse)
res$Team<-str_replace(res$Team, "Footscray", "Western Bulldogs")
res$Team<-str_replace(res$Team, "Brisbane Lions", "Brisbane")
detach("package:tidyverse", unload = TRUE) # detach Tidyverse because it conflicts with lag function

# get team summarized data for merging
match<-dat %>%
  group_by(Date, Season, Round,Venue, Team, Opposition, Status, Match_id)%>%
  summarize_if(is.numeric, sum, na.rm=TRUE)
# bind res with match
match<-merge(match, res, by=c("Date","Season", "Team"))

#differential scores
match<-match %>% 
  group_by(Match_id) %>% 
  mutate(tackel_diff = (T*2) - sum(T)) %>%
  mutate(SC_diff = (SC*2)- sum(SC)) %>%
  mutate(score_acc = G/(G+B)) %>% 
  ungroup()

#turn score difference into an integer D = 2, W = 1, L = 0
match$results <- ifelse(match$Margin < 0, 0, ifelse(match$Margin > 0, 1, 2))
# determine how many wins had for the year
match <- match%>%
  group_by(Season, Team) %>% 
  arrange(Date)%>%
  mutate(wins_this_season = cumsum(ifelse(results == 2, 0.5, results))) %>% 
  ungroup()

##########----- Make Glicko Ratings -----########## 
ratings <- match %>%
  filter(Status == 'Home') %>%
  select(Date, Team, Opposition, results)%>%
  mutate(results = ifelse(results == 2, 0.5, results))

ratings$date <- as.integer(format(ratings$Date, "%Y%m%d"))
ratings$match <- rank(ratings$date)

ratings <- ratings %>%
  select(match, Team, Opposition, results)
ratings$Match_id<-NULL

glicko_rate<-glicko2(ratings, history = T)
# plot ratings
plot(glicko_rate, players = glicko_rate$ratings$player)
print(glicko_rate) #print latest ratings

#make dataframe with history ratings
glicko <- as.data.frame(glicko_rate$history)
setDT(glicko, keep.rownames = TRUE)[]
glicko <- melt(glicko)
glicko$variable <- as.character(glicko$variable)
var <-data.frame(do.call('rbind', strsplit(as.character(glicko$variable),'.',fixed=TRUE)))
glicko<-cbind(glicko, var)
names(glicko)[1] <- "Team"
names(glicko)[4] <- "match"
names(glicko)[5] <- "var"
glicko <- glicko %>%
  filter(var == "Rating")
#rate$match_num <- with(rate, match(match, unique(Date)))

## See Glicko Prediction.R for ratings predictions ##

#prepare data for merging with player stats
glicko <- glicko %>% 
  group_by(Team) %>%
  mutate(rate_change = (value) - lag(value)) %>%
  mutate(rate_change = ifelse(is.na(rate_change), 2200 - value, rate_change)) %>% 
  ungroup()

glicko_clean<-glicko[apply(glicko!=0, 1, all),]
glicko_clean <- glicko_clean %>% filter(var == "Rating")
glicko_clean$match <- as.integer(glicko_clean$match)

glicko_clean<-glicko_clean%>%
  group_by(Team)%>%
  mutate(match_num = order(order(match, decreasing=F)))%>%
  select(Team, match_num, value, rate_change) %>% 
  ungroup()
# create interactive plot with plotly
plotly_build(glicko%>%
  filter(var == "Rating")%>%
  mutate(match = as.numeric(match))%>%
  ggplot(aes(x = match, y = value, color = Team)) +
  geom_point()+
  geom_line()+
  annotate(geom="text", x=3, y=2800, label="2010", color="black")+ 
  annotate(geom="text", x=880, y=2800, label="2020",color="black")+
  ggtitle("AFL: Glicko 2 Ratings"))

#join with match dataset
match$date <- as.integer(format(match$Date, "%Y%m%d"))
match<- match %>%
  group_by(Team) %>%
  mutate(match_num = order(order(date, decreasing=F)))

match <- merge(match, glicko_clean, by=c("Team","match_num"))

##########----- Clean and merge betting statistics -----########## 

# Create an index of the rows you want with duplication
idx <- rep(1:nrow(betting_odds), 2)
# Use that index to genderate your new data frame
betting <- betting_odds[idx,]
# Add variables for joining
bet <- betting%>%
  group_by(Date, Home.Team)%>%
  mutate(num = seq(1,2))%>%
  mutate(Status = ifelse(num == 1, "Home", "Away"))%>%
  mutate(Team = ifelse(num == 1, Home.Team, Away.Team))%>%
  mutate(Opposition = ifelse(Team == Home.Team, Away.Team, Home.Team))%>%
  mutate(Odds = ifelse(Team == Home.Team, Home.Win.Odds, Away.Win.Odds))%>%
  mutate(Opp_Odds = ifelse(Opposition == Home.Team, Home.Win.Odds, Away.Win.Odds))%>%
  mutate(line_Odds = ifelse(Team == Home.Team, Home.Line.Odds, Away.Line.Odds))%>%
  mutate(Opp_lineOdds = ifelse(Opposition == Home.Team, Home.Line.Odds, Away.Line.Odds))%>%
  select(Date,Status, Home.Team, Team, Odds, Opp_Odds, line_Odds, Opp_lineOdds)
library(tidyverse)
bet$Team<-str_replace(bet$Team, "Footscray", "Western Bulldogs")
bet$Team<-str_replace(bet$Team, "Brisbane Lions", "Brisbane")
detach("package:tidyverse", unload = TRUE) # detach Tidyverse because it conflicts with lag function
#merge with match stats
match <- merge(match, bet, by=c("Date","Status", "Team"))

##########----- Add next round fixture to dataframe -----########## 

# add new fixture to dataframe for prediction
round <- read.csv('fixture.csv', stringsAsFactors = F)
round$Date<- as.Date(round$Date, "%Y-%m-%d %H:%M:%S")
match<-as.data.frame(match)

library(plyr) #remove plyr from library after this:
new<-rbind.fill(match, round)
detach("package:plyr", unload = TRUE)

#change team names & home and away status to integer values
new$team <- as.numeric(ordered(new$Team, levels = c("Adelaide","Brisbane","Carlton","Collingwood","Essendon","Fremantle",       
                                                        "Geelong","Gold Coast","GWS" ,"Hawthorn","Melbourne","North Melbourne", 
                                                        "Port Adelaide","Richmond","St Kilda","Sydney","West Coast","Western Bulldogs")))

new$opposition <- as.numeric(ordered(new$Opposition, levels = c("Adelaide","Brisbane","Carlton","Collingwood","Essendon","Fremantle",       
                                                                    "Geelong","Gold Coast","GWS" ,"Hawthorn","Melbourne","North Melbourne", 
                                                                      "Port Adelaide","Richmond","St Kilda","Sydney","West Coast","Western Bulldogs")))
new$status <- as.numeric(ordered(new$Status, levels = c("Home", "Away")))

new<-new %>%
  group_by(Team) %>%
  mutate(last_scoreDiff = lag(Margin, order_by=Date)) %>%
  mutate(last_result = lag(results, order_by=Date)) %>%
  mutate(last_SC = lag(SC_diff, order_by=Date)) %>%
  mutate(last_score_acc = lag(score_acc, order_by=Date)) %>%
  mutate(last_disposals = lag(D, order_by=Date)) %>%
  mutate(last_I50 = lag(I50, order_by=Date)) %>%
  mutate(last_One.Percenters = lag(One.Percenters, order_by=Date)) %>%
  mutate(pre_rate = lag(value, order_by=Date))%>%
  mutate(last_tackelDiff = lag(tackel_diff, order_by=Date)) %>%
  mutate(matches_won = lag(wins_this_season, order_by = Date)) %>% 
  ungroup()

new<-new %>% 
  group_by(Match_id) %>% 
  mutate(rate_diff = (pre_rate*2)-sum(pre_rate)) %>%
  mutate(opp_rating = (sum(pre_rate)-pre_rate)) %>% 
  ungroup()

new<-new %>% 
  group_by(Team, Opposition) %>% 
  mutate(last_encounter_margin = lag(Margin, order_by = Date)) %>% 
  mutate(last_encounter_SC = lag(SC, order_by = Date)) %>% 
  mutate(last_encounter_score_acc = lag(score_acc, order_by=Date)) %>%
  mutate(last_encounter_disposals = lag(D, order_by=Date)) %>%
  mutate(last_encounter_line_Odds = lag(line_Odds, order_by = Date)) %>% 
  ungroup()

# use above metrics to create a couple of more
new<-new%>%
  group_by(Team) %>%
  mutate(last_rateDiff = lag(rate_diff, order_by=Date))%>%
  mutate(pre_oppRate = lag(opp_rating, order_by=Date))%>%
  mutate(last_opp = lag(opposition, order_by=Date)) %>%
  mutate(last_oppRate = lag(pre_oppRate, order_by=Date))%>%
  mutate(last_Odds = lag(Odds, order_by = Date)) %>%
  mutate(last_LineOdds = lag(line_Odds, order_by = Date))%>%
  mutate(last_CP = lag(CP, order_by = Date))%>%
  mutate(last_CM = lag(CM, order_by = Date))%>%
  mutate(last_MI5 = lag(MI5, order_by = Date))%>%
  mutate(last_AF = lag(AF, order_by = Date))%>%
  ungroup()
# Select metrics to include in training the model; I've left out a lot of metrics because they seem to make the model perform better after trial and error.
future_data_lean <- new %>%
  select(results, Season, team, opposition, status,
         pre_rate,pre_oppRate,Odds, Opp_Odds,line_Odds,Opp_lineOdds, 
         matches_won, last_encounter_margin, last_encounter_SC,last_encounter_score_acc, 
         last_encounter_line_Odds)

future_data_lean<-future_data_lean[complete.cases(future_data_lean), ] #remove NAs from data frame
future_data_lean<-future_data_lean%>%
  filter(results == 0 | results == 1 | results == 999) #remove draws ensure that the loss function is "binary_crossentropy", if you want to keep Draws change to "categorical_crossentropy"

#Create data frame for margin predictions used in DeepLearning_Margin.R
score_data_lean <- new %>%
  select(Margin, Season, team, opposition, status, last_scoreDiff, 
         pre_rate,pre_oppRate,Odds, Opp_Odds,line_Odds,Opp_lineOdds,last_score_acc, 
         matches_won, last_encounter_margin, last_rateDiff,last_Odds,
         last_LineOdds, last_encounter_SC,last_encounter_score_acc, 
         last_encounter_disposals,last_encounter_line_Odds
         )
score_data_lean<-score_data_lean[complete.cases(score_data_lean), ] #remove NAs from data frame

