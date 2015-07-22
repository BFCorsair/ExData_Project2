plot2 <- function() {

	# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
	#(fips == "24510") from 1999 to 2008?
	# Use the base plotting system to make a plot answering this question


	# Read the data files
	NEI <- readRDS("summarySCC_PM25.rds")
	# SCC <- readRDS("Source_Classification_Code.rds")

	# Extract the Baltimore data
	Balt <- subset(NEI, fips == 24510)

	# Aggregate all emissions by year
	Emissions <- aggregate(Emissions ~ year, data=Balt, sum)
	# Scale the units to Millions of Tons
	Emissions <- transform(Emissions, Emissions = 0.001*Emissions)
	generatePlot2(Emissions)
	Emissions
}

generatePlot2 <- function(df) {

	# Create the plot
	png(filename="plot2.png", width=480,height = 480)


	with (df, {
		# Plot 1
		barplot(df$Emissions, col="blue", names.arg = df$year)
		title(ylab="PM2.5 Emissions (Thousands of Tons)", main="Baltimore - PM2.5 Emissions by Year")
	})

	# Done - save the file
	dev.off()
}
