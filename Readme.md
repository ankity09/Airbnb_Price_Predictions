# Predicting Airbnb Prices - New York

### For R Version 3.6 (Base R)

## Dataset

Dataset comprises of two files for New York City, March 2019:
1. listings.csv - Contains detailed listing data for hosts endorsing houses on Airbnb.
2. reviews.csv - Contains reviews for houses listed on Airbnb.

Source: http://insideairbnb.com/get-the-data.html

## Description

**There exists 1 Master Script that:**
- Reads Dataset.
- Installs and loads required libraries.
- Installs user defined functions.
- Performs data cleaning.
- Performs text mining.
- Joins listings and review sentiments.
- Writes the new cleaned and joined files to csv (redundant backups).

**There are separate R Scripts for each model built.**

**Guidelines:**
1. Master Script is to be run once per session.
2. Master Script will take atleast an hour to run (sentiment_by function call on 38000 reviews.)
3. Each prediction model resides in a separate script.
4. To check any model, the entire script is to be run.
5. Visualizations and Exploratory Data Analysis resides in a separate script.

**Instructions for running R Scripts:**
1. Place all data files and r scripts in one folder.
2. Open Master.R in RStudio.
3. Set the Working Directory to Source File.
4. Run Master.R script. *(Text Mining is a time-intensive process.
Scanning sentiments of ~38000 reviews took about an hour on our machines.)*
5. Open a script for the regression model desired.
6. Library loading commands are redundant if you are in the same session.
7. Run script.
8. Once run, view plots generated in the plot window.
	  
**To view model summaries, view the dataframe "modelkpi".**
