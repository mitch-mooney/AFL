# AFL
Playing with AFL repository fitzRoy

This repository makes use of the fitzRoy package in R https://cran.r-project.org/web/packages/fitzRoy/index.html

## What the code is doing
AFL_data.R file is to pull data from footy_wire and reorganises it so it may go into a deep learning netural network run from predict_model.R

## First
AFL_data.R must be run first, it pulls the data then creates a data rating based on the Glicko2 rating system from PlayerRatings package. These are then merged so we can incorporate the ratings into the match statistics for modeling.

The code then creates metrics that are known before a match such as previous match score difference etc.

There is a .csv file called fixture which allow you to make predictions about an upcoming round based on the model. You just need to fill in the cells with the next rounds matches. You only need to do this if you want to test to see how your model goes in prediciting future matches.

## Second
Run the deep learning model through Keras package using the prediction_model.R file. Play with the model perameters and plot the results. For tutorials on how to build your own models I recommend Dr. Bharatendra Rai's channel on YouTube https://www.youtube.com/channel/UCuWECsa_za4gm7B3TLgeV_A

## Finally
After running the predictions I've kept track of the performance of the model in round2_fixture_res.csv file.
