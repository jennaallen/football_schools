# Football Fans: A Data-Driven Approach to College Selection
**Abstract**

Using a dimensional model, data warehouse, and reporting I explored data from the College Scorecard and NCAA Division I FBS football games to create a data-driven approach to school selection for college football fans. 

**Overview**

**Data Warehouse Opportunity and Objectives**

For many students in the United States, NCAA Division I football is an important part of their student life and college experience. It is also my biased opinion that college football is fun to watch, especially when you have an emotional investment in one of the teams playing. Thus, I saw an opportunity to create a data-driven approach to school selection for college football fans. 

The data-driven approach included several objectives:

* Retrieve data from the College Scorecard using the available API
* Scrape NCAA Division I college football game scores from the web 
* Cleanup and transform the data
* Create a dimensional data model  
* Load the data into a MySQL data warehouse running on AWS
* Create an interactive dashboard that allows users to input certain criteria regarding school location, size, graduation rate, total cost, etc. and get back a filtered list of schools showing a map of the school location, cost per win, point differential per game, points per game, etc.

An R Notebook was used to annotate my code. Please view/download the football_schools_data_warehouse.nb.html file for a comprehensiive walk-through of this project. 
