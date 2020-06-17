# AFL
Playing with AFL repository fitzRoy

This repository makes use of the fitzRoy package in R https://cran.r-project.org/web/packages/fitzRoy/index.html

## What the code is doing
AFL_data.R file is to pull data from footy_wire and reorganises it so it may go into a the models in other r files:
Glicko Prediction.R is using Glicko2 ratings to predict matches
prediction_model.R is used to run a keras classification model to predict outcome of matches
DeepLearning_margin.R is used to predict the margin of the matches.
Store_predictions.R is used to tabulate the next match predictions.

## First
AFL_data.R must be run first. It pulls and organises several datasources including betting data, team performance data and a ranking data built using Glicko2 in PlayerRatings package.

## Second
After running the AFL_data.R file you can choose any of Glicko Prediction.R, prediction_model.R or DeepLearning_margin.R next to play with. 

## Finally
After running the predictions you can use Store_predictions.R to tabulate and store predictions in a .csv the  I've kept track of the performance of the model in fixture_res.csv file.

###credits
To build the models I started by looking at Dr. Bharatendra Rai's channel on YouTube https://www.youtube.com/channel/UCuWECsa_za4gm7B3TLgeV_A and then adapted the models from there.
