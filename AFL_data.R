library(fitzRoy)
library(dplyr)
library(data.table)
library(PlayerRatings)
library(ggplot2)
library(plotly)
library(lubridate)
library(reshape2)
## Update footywire data player box score statistics
dat <- update_footywire_stats()
#get betting odds as well
betting_odds<-get_footywire_betting_odds(
  start_season = "2010",
  end_season = lubridate::year(Sys.Date())
)

# get team summarized data
match<-dat %>%
  group_by(Date, Season, Round,Venue, Team, Opposition, Status, Match_id)%>%
  summarize_if(is.numeric, sum, na.rm=TRUE)

#differential scores
match<-match %>% 
  group_by(Match_id) %>% 
  mutate(score = (G*6)+(B*1)) %>%
  mutate(score_diff = score*2 - sum(score))%>%
  mutate(tackel_diff = (T*2) - sum(T)) %>%
  mutate(SC_diff = (SC*2)- sum(SC)) %>%
  mutate(score_acc = G/(G+B)) 

#turn score difference into an integer D = 2, W = 1, L = 0
match$results <- ifelse(match$score_diff < 0, 0, ifelse(match$score_diff > 0, 1, 2))

#Make Glicko Ratings
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

#prepare data for merging with player stats
glicko <- glicko %>% 
  group_by(Team) %>%
  mutate(rate_change = (value) - lag(value)) %>%
  mutate(rate_change = ifelse(is.na(rate_change), 2200 - value, rate_change))

glicko_clean<-glicko[apply(glicko!=0, 1, all),]
glicko_clean <- glicko_clean %>% filter(var == "Rating")
glicko_clean$match <- as.integer(glicko_clean$match)

glicko_clean<-glicko_clean%>%
  group_by(Team)%>%
  mutate(match_num = order(order(match, decreasing=F)))%>%
  select(Team, match_num, value, rate_change)
# create interactive plot with plotly
p<- glicko%>%
  filter(var == "Rating")%>%
  mutate(match = as.numeric(match))%>%
  ggplot(aes(x = match, y = value, color = Team)) +
  geom_point()+
  geom_line()+
  annotate(geom="text", x=3, y=2800, label="2010", color="black")+ 
  annotate(geom="text", x=880, y=2800, label="2020",color="black")+
  ggtitle("AFL: Glicko 2 Ratings")

plotly_build(p)

#join with match dataset
match$date <- as.integer(format(match$Date, "%Y%m%d"))
match<- match %>%
  group_by(Team) %>%
  mutate(match_num = order(order(date, decreasing=F)))

match <- merge(match, glicko_clean, by=c("Team","match_num"))

# Reorganize betting data to merge with match stats
# Create an index of the rows you want with duplications
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

# lag values from previous matches
match<-match %>%
  group_by(Team) %>%
  mutate(last_scoreDiff = lag(score_diff, order_by=Date)) %>%
  mutate(last_result = lag(results, order_by=Date)) %>%
  mutate(last_SC = lag(SC_diff, order_by=Date)) %>%
  mutate(last_score_acc = lag(score_acc, order_by=Date)) %>%
  mutate(last_disposals = lag(D, order_by=Date)) %>%
  mutate(last_I50 = lag(I50, order_by=Date)) %>%
  mutate(last_One.Percenters = lag(One.Percenters, order_by=Date)) %>%
  mutate(pre_rate = lag(value, order_by=Date))%>%
  mutate(last_tackelDiff = lag(tackel_diff, order_by=Date))%>%
  ungroup()
# Add some more differential metrics
match<-match %>% 
  group_by(Match_id) %>% 
  mutate(rate_diff = (pre_rate*2)-sum(pre_rate)) %>%
  mutate(opp_rating = (sum(pre_rate)-pre_rate)) 
# select variable to model
#match team and opposition integer values
match$team <- as.numeric(ordered(match$Team, levels = c("Adelaide","Brisbane","Carlton","Collingwood","Essendon","Fremantle",       
                                                        "Geelong","Gold Coast","GWS" ,"Hawthorn","Melbourne","North Melbourne", 
                                                        "Port Adelaide","Richmond","St Kilda","Sydney","West Coast","Western Bulldogs")))

match$opposition <- as.numeric(ordered(match$Opposition, levels = c("Adelaide","Brisbane","Carlton","Collingwood","Essendon","Fremantle",       
                                                                    "Geelong","Gold Coast","GWS" ,"Hawthorn","Melbourne","North Melbourne", 
                                                                    "Port Adelaide","Richmond","St Kilda","Sydney","West Coast","Western Bulldogs")))
match$status <- as.numeric(ordered(match$Status, levels = c("Home", "Away")))
#final metrics to add
match<-match%>%
  group_by(Team) %>%
  mutate(last_rateDiff = lag(rate_diff, order_by=Date))%>%
  mutate(pre_oppRate = lag(opp_rating, order_by=Date))%>%
  mutate(last_opp = lag(opposition, order_by=Date)) %>%
  mutate(last_oppRate = lag(pre_oppRate, order_by=Date))%>%
  ungroup()


# select variables
model_data <- match %>%
  select(results, Season, team, opposition, status, last_scoreDiff, last_result, 
         last_SC, last_score_acc, last_disposals, last_I50, rate_diff,
         last_One.Percenters,pre_rate, last_opp, last_oppRate, last_rateDiff, last_tackelDiff)

model_data<-model_data[complete.cases(model_data), ] #remove NAs from dataframe

# add new fixture to dataframe for prediction
new_season<-get_fixture(season = lubridate::year(Sys.Date()), convert_date = FALSE)
new_season <- new_season%>%
  filter(Round == 2)
round2 <- read.csv('fixture.csv', stringsAsFactors = F)
round2$Date<- as.Date(round2$Date, "%Y-%m-%d %H:%M:%S")
#round2$results <- 999
match<-as.data.frame(match)

library(plyr) #remove plyr from library after this:
new<-rbind.fill(match, round2)
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
mutate(last_scoreDiff = lag(score_diff, order_by=Date)) %>%
mutate(last_result = lag(results, order_by=Date)) %>%
mutate(last_SC = lag(SC_diff, order_by=Date)) %>%
mutate(last_score_acc = lag(score_acc, order_by=Date)) %>%
mutate(last_disposals = lag(D, order_by=Date)) %>%
mutate(last_I50 = lag(I50, order_by=Date)) %>%
mutate(last_One.Percenters = lag(One.Percenters, order_by=Date)) %>%
mutate(pre_rate = lag(value, order_by=Date))%>%
mutate(last_tackelDiff = lag(tackel_diff, order_by=Date))

new<-new %>% 
  group_by(Match_id) %>% 
  mutate(rate_diff = (pre_rate*2)-sum(pre_rate)) %>%
  mutate(opp_rating = (sum(pre_rate)-pre_rate)) 

  #final metrics to add
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
  ungroup()

future_data <- new %>%
  select(results, Season, team, opposition, status, last_scoreDiff, last_result, 
         last_SC, last_score_acc, last_disposals, last_I50, rate_diff,
         last_One.Percenters,pre_rate, last_opp, last_oppRate, last_rateDiff, 
         last_tackelDiff, Odds, Opp_Odds,line_Odds,Opp_lineOdds,
         last_Odds,last_LineOdds,last_CP, last_CM, last_MI5)

future_data<-future_data[complete.cases(future_data), ] #remove NAs from dataframe
