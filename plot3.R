plot3 <- function() {

	# Of the four types of sources indicated by the type
	# (point, nonpoint, onroad, nonroad) variable,
	# which of these four sources have seen decreases in emissions
	# from 1999–2008 for Baltimore City?
	# Which have seen increases in emissions from 1999–2008?
	# Use the ggplot2 plotting system to make a plot answer this question


	# Read the data files
	NEI <- readRDS("summarySCC_PM25.rds")
	# SCC <- readRDS("Source_Classification_Code.rds")

	# Extract the Baltimore data
	Balt <- subset(NEI, fips == 24510)
	# Make type a factor
	Balt <- transform(Balt, type = factor(type))
	# Aggregate all emissions by year and type of emissions
	Emissions <- aggregate(Emissions ~ year+type, data=Balt, sum)
	generatePlot3(Emissions)
	Emissions
}

generatePlot3 <- function(df) {

	# Create the plot
	qplot(year, Emissions, data=df, color= type, geom="line", 
		xlab="Year", ylab="PM2.5 Emissions", main="Baltimore - PM2.5 Emissions by Year")

	# Done - save the file
	ggsave("plot3.png")
}
