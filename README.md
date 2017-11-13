# NFL Play Predictor
Predict what's coming next in the NFL!

In all levels of football, on-field trends are typically discerned exclusively through voluminous film study of opponent history, and decisions are made using anecdotal evidence and gut instinct. These methods in isolation are highly inefficient and prone to human error.

Enter – the play predictor. This tool aims to enhance in-game NFL decision making with a tool capable of predicting the type of play the opposing team will run at high accuracy in real-time. On average this tool is able to predict pass or run at 73.6% accuracy, with varying performance dictated by teams playing and mostly game situation.

This tool is able to predict in close to real-time, meaning you can follow along during a game and generate accurate predictions of what type of play will be run next, where it will go, how that decision compares to all other teams in the NFL, and what the likelihood of success is (as defined by scoring a touchdown or getting a first down).

Click the link below to try it out!

http://wespasplaypredictor.com/

The app was built in Shiny, and the website hosted using the AWS stack (an ec2 instance with a custom domain using Route 53). Check out the blog posts below for more details, I'll be publishing a series of posts on methods for this project. The first post goes into the predictor itself, while the second focuses on how to host a Shiny Application using Amazon EC2, and how to set up a custom domain name for that application.

https://wesleypasfield.wordpress.com/2017/09/17/predicting-nfl-plays-with-the-xgboost-decision-tree-algorithm-wespasplaypredictor-com/

https://wesleypasfield.wordpress.com/2017/11/13/hosting-an-r-shiny-application-on-amazon-ec2/

