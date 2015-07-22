plot4 <- function() {

	# Across the United States, how have emissions from coal combustion-related
	# sources changed from 1999–2008?


	# Read the data files
	NEI <- readRDS("summarySCC_PM25.rds")
	SCC <- readRDS("Source_Classification_Code.rds")

	# Find the SCC codes related to Coal - Use EI.Sector
	# indices of EI.Sector that match coal
	coal_idx <- grep("coal", SCC$EI.Sector, ignore.case = TRUE)
	# Get the SCC values
	scc_coal <- SCC[coal_idx,"SCC"]

	# Extract the data for coal 
	Coal <- subset(NEI, SCC %in% scc_coal)

	# Aggregate all emissions by year
	Emissions <- aggregate(Emissions ~ year, data=Coal, sum)
	generatePlot4(Emissions)
	Emissions
}

generatePlot4 <- function(df) {

	# Create the plot
	png(filename="plot4.png", width=480,height = 480)


	with (df, {
		# Plot 4
		barplot(df$Emissions, col="green", names.arg = df$year)
		title(ylab="PM2.5 Emissions", main="US - PM2.5 Emissions by Year From Coal Combustion")
	})

	# Done - save the file
	dev.off()
}
