# AFL
Playing with AFL repository fitzRoy

This repository makes use of the fitzRoy package in R https://cran.r-project.org/web/packages/fitzRoy/index.html

## What the code is doing
AFL_data.R file is to pull data from footy_wire and reorganises it for models in other r files:
Glicko Prediction.R is using Glicko2 ratings to predict matches note: glicko2 gets merged in AFL_data.R to be used as an input into other models but this file allows you to use Glicko2 model independently.
prediction_model.R is used to run a keras classification model to predict outcome of matches
margin_estimate.R is used to predict the margin of the matches.
Store_predictions.R is used to tabulate the next match predictions.

## First
AFL_data.R must be run first. It pulls and organises several datasources including betting data, team performance data and a ranking data built using Glicko2 in PlayerRatings package.

## Second
After running the AFL_data.R file you can choose Glicko Prediction.R or prediction_model.R next to play with, these two are independent prediction models and results will be different. After you run prediction_model.R and obtain the prediction probabilities you can run the margin_estimate.R to estimate the margin.

## Finally
After running the predictions you can use Store_predictions.R to tabulate and store predictions in a .csv the  I've kept track of the performance of the model in fixture_res.csv file.

###credits
To build the prediction models I started by looking at Dr. Bharatendra Rai's channel on YouTube https://www.youtube.com/channel/UCuWECsa_za4gm7B3TLgeV_A and then adapted the model architecture from there.
