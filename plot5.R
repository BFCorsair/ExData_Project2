plot5 <- function() {

	# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?


	# Read the data files
	NEI <- readRDS("summarySCC_PM25.rds")
	SCC <- readRDS("Source_Classification_Code.rds")

	# Find the SCC codes related to Coal - Use EI.Sector
	# indices of EI.Sector that match motor
	motor_idx <- grep("Mobile - On-Road", SCC$EI.Sector, ignore.case = TRUE)
	# Get the SCC values
	scc_motor <- SCC[motor_idx,"SCC"]

	# Extract the Baltimore data
	Balt <- subset(NEI, fips == 24510)

	# Extract the  data for motor vehicles
	Motor <- subset(Balt, SCC %in% scc_motor)

	# Aggregate all emissions by year
	Emissions <- aggregate(Emissions ~ year, data=Motor, sum)
	generatePlot5(Emissions)
	Emissions
}

generatePlot5 <- function(df) {

	# Create the plot
	png(filename="plot5.png", width=480,height = 480)


	with (df, {
		# Plot 4
		barplot(df$Emissions, col="orange", names.arg = df$year)
		title(ylab="PM2.5 Emissions", main="Baltimore - PM2.5 Emissions by Year From Motor Vehicles")
	})

	# Done - save the file
	dev.off()
}
