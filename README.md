# AFL
Playing with AFL repository fitzRoy

This repository makes use of the fitzRoy package in R https://cran.r-project.org/web/packages/fitzRoy/index.html

## What the code is doing
AFL_data.R file is to pull data from footy_wire and reorganise it so it may go into a deep learning netural network in predict_model.R

## First
AFL_data.R must be run first, it first pulls the data then creates a data rating based on the Glicko2 rating system from PlayerRatings package.

Once this is done creating metrics that are known before a match are created. You can create your own if you like.

There is a .csv file called fixture which allow you to make predictions about an upcoming round based on the model. You just need to fill in the cells with the next rounds matches.

## Second
Run the deep learning model through Keras package using the prediction_model.R file. For tutorials on how to build your own I recommend Dr. Bharatendra Rai's channel on YouTube https://www.youtube.com/channel/UCuWECsa_za4gm7B3TLgeV_A

## Finally
After running the predictions I've kept track of the performance of the model in round2_fixture_res.csv file.
