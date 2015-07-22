plot6 <- function() {

	# Compare emissions from motor vehicle sources in Baltimore City  (fips == "24510") with emissions
	# from motor vehicle sources in Los Angeles County, California (fips == "06037").
	# Which city has seen greater changes over time in motor vehicle emissions?




	# Read the data files
	NEI <- readRDS("summarySCC_PM25.rds")
	SCC <- readRDS("Source_Classification_Code.rds")

	# Find the SCC codes related to Coal - Use EI.Sector
	# indices of EI.Sector that match motor
	motor_idx <- grep("Mobile - On-Road", SCC$EI.Sector, ignore.case = TRUE)
	# Get the SCC values
	scc_motor <- SCC[motor_idx,"SCC"]

	# Extract the Baltimore data
	BaltLA <- subset(NEI, fips %in% c("06037","24510"))

	# Extract the  data for motor vehicles
	Motor <- subset(BaltLA, SCC %in% scc_motor)
	Motor <- transform(Motor, fips = factor(fips, levels=c("24510", "06037"), labels=c("Baltimore", "Los Angeles")))

	# Aggregate all emissions by year
	Emissions <- aggregate(Emissions ~ year+fips, data=Motor, sum)
	# Set 1999 year - as baseline of 100 for each city
	# Subset by city and normalize
	EmB <- subset(Emissions, fips=="Baltimore")
	EmLA <- subset(Emissions, fips=="Los Angeles")
	em99 <- Emissions[Emissions$year==1999, "Emissions"]
	EmB <- mutate(EmB, Emissions=Emissions*100.0/em99[1])
	EmLA <- mutate(EmLA, Emissions=Emissions*100.0/em99[2])
	EmFinal <- rbind(EmB,EmLA)

	generatePlot6(EmFinal)
	EmFinal
}

generatePlot6 <- function(df) {

	# Rename fips to City
	colnames(df)[2] <- "City"
	# Create the plot
	qplot(year, Emissions, data=df, color= City, geom="line", size=I(2),
		xlab="Year", ylab="PM2.5 Emissions Relative to 1999", main="Yearly PM2.5 Emissions From Motor Vehicles Relative to 1999\nBaltimore & Los Angeles")

	# Done - save the file
	ggsave("plot6.png")
}
