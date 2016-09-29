## Exploratory Data Analysis Final Project
## September 2016

# Load the necessary libraries
library(ggplot2)

## First read in the data at specified
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Question 1: Did the total emissions of pm25 per year decrease?
# To answer this question we must first sum the total emissions from all sources for each year
totalEmissions = aggregate(NEI$Emissions, list(NEI$year), sum)

#Now we want to plot the results - to make the plot slightly more readable we divide the total
# emissions by 1000 and note this in the y label
plot(totalEmissions$x/1000,xlab="Year",ylab="Total Emission [tons x1000]",main='Total Tons of pm25 Emitted \n per Year',col="red",lwd=3,type='l')

# Now save the results in a PNG file for submission
dev.copy(png,'Plot1.png')
dev.off()

# Question 2: Have the total emissions of pm25 decreased in Baltimore between 1999 and 2008?
# First subset the data so that only the Baltimore portion of the data is extracted from the data frame
BaltimoreData = NEI[which(NEI$fips == 24510),]

# Now again sum the total emission for a given year from all sources
totalBaltimoreEmissions = aggregate(BaltimoreData$Emissions, list(BaltimoreData$year), sum)

#Now we want to plot the results
plot(totalBaltimoreEmissions$x,xlab="Year",ylab="Total Emission [tons]",main='Total Tons of pm25 Emitted per Year \n in Baltimore',col="red",lwd=3,type='l')

# Now save the results in a PNG file for submission
dev.copy(png,'Plot2.png')
dev.off()

# Question 3: Of the 4 types of pollution, which has seen an increase and which has seen a decrease in pollution?
# First subset the data so that only the Baltimore portion of the data is extracted from the data frame
BaltimoreData = NEI[which(NEI$fips == 24510),]

# Now aggregate the data both by year and by type
totalBaltimoreEmissionsByType = aggregate(BaltimoreData$Emissions, list(BaltimoreData$year,BaltimoreData$type), sum)

# Now plot the data using ggplot
ggplot(data=totalBaltimoreEmissionsByType, aes(x=Group.1, y=x))+geom_line(aes(col=Group.2,group=Group.2),lwd=3)+xlab("year")+ylab("Pollution [tons]")+labs(title="Pollution of pm25 by Type")

# Now save the results in a PNG file for submission
dev.copy(png,'Plot3.png')
dev.off()

# Question 4: Across the United States, how have emissions from coal related sources changed?
# First lets identify the rows of the label table that have the identifier numbers for sources that contain coal
selectedRows <- SCC[grep(" Coal ", SCC$Short.Name), ]

# Now extract the relevant codes
codes = selectedRows$SCC

# Now take the NEI data that corresponds only to the coal sources
subsetData = NEI[NEI$SCC %in% codes,]

# Lastly, aggregate this data, i.e. sum the total pollution by year
subsetDataYear = aggregate(subsetData$Emissions, list(subsetData$year), sum)


# Plot the results
plot(subsetDataYear$x,xlab="Year",ylab="Total Emission [tons]",main='Total Tons of pm25 Emitted From Coal \n Sources in US per Year',col="red",lwd=3,type='l')

# Now save the results in a PNG file for submission
dev.copy(png,'Plot4.png')
dev.off()

# Question 5: How have the emissions of pm25 changed from motor vehicles in the city of Baltimore?
# First find all emissions from motor vehicles by searching for type on-road
selectedRows <- NEI[grep("ON-ROAD", NEI$type), ]

# Now select the rows that correspond to Baltimore
subsetData = selectedRows[which(selectedRows$fips == 24510),]

# Lastly, aggregate this data, i.e. sum the total pollution by year
BaltimoreDataMV = aggregate(subsetData$Emissions, list(subsetData$year), sum)

# Plot the results
plot(BaltimoreDataMV$x,xlab="Year",ylab="Total Emission [tons]",main='Total Tons of pm25 Emitted \n From Motor Vehicles in Baltimore per Year',col="red",lwd=3,type='l')

# Now save the results in a PNG file for submission
dev.copy(png,'Plot5.png')
dev.off()

# Question 6: Compare the emissions from motor vehicles in Baltimore and LA. Which has experienced a greater emissions reduction?
selectedRows <- NEI[grep("ON-ROAD", NEI$type), ]

# Now select the rows that correspond to Baltimore
comparisonData = selectedRows[which(selectedRows$fips == 24510 | selectedRows$fips == '06037'),]

# Lastly, aggregate this data, i.e. sum the total pollution by year
comparisonDataAgg = aggregate(comparisonData$Emissions, list(comparisonData$year,comparisonData$fips), sum)

# Plot the results
# Now plot the data using ggplot
ggplot(data=comparisonDataAgg, aes(x=Group.1, y=x))+geom_line(aes(col=Group.2,group=Group.2),lwd=3)+xlab("year")+ylab("Pollution [tons]")+labs(title="Comparing the Emissions in Baltimore and LA")

# Now save the results in a PNG file for submission
dev.copy(png,'Plot6.png')
dev.off()
