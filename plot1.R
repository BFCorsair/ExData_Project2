plot1 <- function() {

	# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
	# Using the base plotting system, make a plot showing the total PM2.5 emission from 
	# all sources for each of the years 1999, 2002, 2005, and 2008.


	# Read the data files
	NEI <- readRDS("summarySCC_PM25.rds")
	# SCC <- readRDS("Source_Classification_Code.rds")

	# Aggregate all emissions by year
	Emissions <- aggregate(Emissions ~ year, data=NEI, sum)
	# Scale the units to Millions of Tons
	Emissions <- transform(Emissions, Emissions = 0.000001*Emissions)
	generatePlot1(Emissions)
	Emissions
}

generatePlot1 <- function(df) {

	# Create the plot
	png(filename="plot1.png", width=480,height = 480)


	with (df, {
		# Plot 1
		barplot(df$Emissions, col="red", names.arg = df$year)
		title(ylab="PM2.5 Emissions (Millions of Tons)", main="PM2.5 Emissions by Year")
	})

	# Done - save the file
	dev.off()
}
