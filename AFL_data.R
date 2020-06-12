library(fitzRoy)
library(dplyr)
library(data.table)
library(PlayerRatings)
library(ggplot2)
library(lubridate)
## Update footywire data player box score statistics
dat <- update_footywire_stats()

# get team summarized data
match<-dat %>%
  group_by(Date, Season, Round,Venue, Team, Opposition, Status, Match_id)%>%
  summarize_if(is.numeric, sum, na.rm=TRUE)

#differential scores
match<-match %>% 
  group_by(Match_id) %>% 
  mutate(score_diff = (T*2) - sum(T)) %>%
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

glicko_rate<-glicko2(ratings, history = T)
plot(glicko_rate, players = glicko_rate$ratings$player)

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
rate <- glicko %>%
  filter(var == "Rating")
rate$match_num <- with(rate, match(match, unique(Date)))

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

#join with match dataset
match$date <- as.integer(format(match$Date, "%Y%m%d"))
match<- match %>%
  group_by(Team) %>%
  mutate(match_num = order(order(date, decreasing=F)))

match <- merge(match, glicko_clean, by=c("Team","match_num"))
# Add some more differential metrics
match<-match %>% 
  group_by(Match_id) %>% 
  mutate(rate_diff = (value*2)-sum(value)) %>%
  mutate(opp_rating = (sum(value)-value))

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
  mutate(last_rate = lag(value, order_by=Date)) %>%
  mutate(last_rateDiff = lag(rate_diff, order_by=Date))%>%
  mutate(last_oppRate = lag(opp_rating, order_by=Date))%>%
  mutate(last_opp = lag(opposition, order_by=Date))

# select variable to model
  #match team and opposition integer values
match$team <- as.numeric(ordered(match$Team, levels = c("Adelaide","Brisbane","Carlton","Collingwood","Essendon","Fremantle",       
                                                      "Geelong","Gold Coast","GWS" ,"Hawthorn","Melbourne","North Melbourne", 
                                                      "Port Adelaide","Richmond","St Kilda","Sydney","West Coast","Western Bulldogs")))

match$opposition <- as.numeric(ordered(match$Opposition, levels = c("Adelaide","Brisbane","Carlton","Collingwood","Essendon","Fremantle",       
                                                        "Geelong","Gold Coast","GWS" ,"Hawthorn","Melbourne","North Melbourne", 
                                                        "Port Adelaide","Richmond","St Kilda","Sydney","West Coast","Western Bulldogs")))
match$status <- as.numeric(ordered(match$Status, levels = c("Home", "Away")))
# select variables
model_data <- match %>%
  select(results, Season, team, opposition, status, last_scoreDiff, last_result, 
         last_SC, last_score_acc, last_disposals, last_I50, rate_diff,
         last_One.Percenters,last_rate, last_opp, last_oppRate, last_rateDiff)

model_data$Team<-NULL #remove grouping value
model_data<-model_data[complete.cases(model_data), ] #remove NAs from dataframe

# add new fixture to dataframe for prediction
new_season<-get_fixture(season = lubridate::year(Sys.Date()), convert_date = FALSE)
new_season <- new_season%>%
  filter(Round == 2)
round2 <- read.csv('round2_fixture.csv', stringsAsFactors = F)
round2$Date<- as.Date(round2$Date, "%Y-%m-%d %H:%M:%S")
match<-as.data.frame(match)
library(plyr) #remove plyr from library after this:
new<-rbind.fill(match, round2)


new<-new%>%
  arrange(Date)%>%
  group_by(Team) %>%
  mutate(last_scoreDiff = dplyr::lag(score_diff, n = 1, default = NA)) %>%
  mutate(last_result = dplyr::lag(results, n = 1, default = NA)) %>%
  mutate(last_SC = dplyr::lag(SC_diff, n = 1, default = NA)) %>%
  mutate(last_score_acc = dplyr::lag(score_acc, n = 1, default = NA)) %>%
  mutate(last_disposals = dplyr::lag(D, n = 1, default = NA)) %>%
  mutate(last_I50 = dplyr::lag(I50, n = 1, default = NA)) %>%
  mutate(last_One.Percenters = dplyr::lag(One.Percenters, n = 1, default = NA)) %>%
  mutate(last_rate = dplyr::lag(value, n = 1, default = NA)) %>%
  mutate(last_rateDiff = dplyr::lag(rate_diff, n = 1))%>%
  mutate(last_oppRate = dplyr::lag(opp_rating, n = 1))%>%
  mutate(last_opp = dplyr::lag(opposition, n = 1, default = NA))

new<-new%>%
  group_by(team) %>%
  arrange(Date)%>%
  mutate(last_scoreDiff = dplyr::lag(score_diff, n = 1, default = NA))

new$team <- as.numeric(ordered(new$Team, levels = c("Adelaide","Brisbane","Carlton","Collingwood","Essendon","Fremantle",       
                                                        "Geelong","Gold Coast","GWS" ,"Hawthorn","Melbourne","North Melbourne", 
                                                        "Port Adelaide","Richmond","St Kilda","Sydney","West Coast","Western Bulldogs")))

new$opposition <- as.numeric(ordered(new$Opposition, levels = c("Adelaide","Brisbane","Carlton","Collingwood","Essendon","Fremantle",       
                                                                    "Geelong","Gold Coast","GWS" ,"Hawthorn","Melbourne","North Melbourne", 
                                                                      "Port Adelaide","Richmond","St Kilda","Sydney","West Coast","Western Bulldogs")))
new$status <- as.numeric(ordered(new$Status, levels = c("Home", "Away")))
