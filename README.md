# Exploring-Data-Final-Project
This repository contains the files for the final project in the Exploring Data Course on Coursera. In this project we examined EPA data to determine the trend in pollution emissions for a pollutant called pm25. We examined this trend across the United States for emissions from Coal sources and motor vehicle sources. In addition, we examined the local trends in Baltimore and Los Angeles. We culminate the project in a comparison plot between the two local areas. 

Files in repo:
- PlotX.png - these are the plots that respond the questions in the prompt.
- Final Project R file - this is the file that creates and saves all of the displayed plots.
- Source Classification Code - this is an epa provided file provides brief explanations for the source codes in the data frame.
- summarySC_PM25 - this is the data frame that contains the epa pollution emission data for pm25.

Here is the project prompt:

The overall goal of this assignment is to explore the National Emissions Inventory database and see what it say about fine particulate matter pollution in the United states over the 10-year period 1999–2008. You may use any R package you want to support your analysis.
Questions

You must address the following questions and tasks in your exploratory analysis. For each question/task you will need to make a single plot. Unless specified, you can use any plotting system in R to make your plot.

- Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
- Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.
- Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.
- Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
- How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
- Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

For each plot you should:

- Construct the plot and save it to a PNG file.
- Create a separate R code file (plot1.R, plot2.R, etc.) that constructs the corresponding plot, i.e. code in plot1.R constructs the plot1.png plot. Your code file should include code for reading the data so that the plot can be fully reproduced. You must also include the code that creates the PNG file. Only include the code for a single plot (i.e. plot1.R should only include code for producing plot1.png)
- Upload the PNG file on the Assignment submission page
- Copy and paste the R code from the corresponding R file into the text box at the appropriate point in the peer assessment.
