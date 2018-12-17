# Read data file, which is expected to be in the current directory.
# Select data for just the two dates of interest.
# Replace '?' with NA everywhere in selected data.
# Create a new datetime column from the date and time columns.
# Convert the appropriate columns to numeric.
# return the resulting data frame
#
readit <- function() {
    df <-read.csv("household_power_consumption.txt", sep=";", colClasses='character')
    df2 <- df[(df$Date == '1/2/2007') | (df$Date == '2/2/2007'),]
    df2[df2 == '?'] <- NA
    df2$datetime <- paste(df2$Date, df2$Time)
    df2$datetime <- strptime(df2$datetime, '%d/%m/%Y %H:%M:%S')
    df2$Global_active_power <- as.numeric(df2$Global_active_power)
    df2$Global_reactive_power <- as.numeric(df2$Global_reactive_power)
    df2$Voltage <- as.numeric(df2$Voltage)
    df2$Sub_metering_1 <- as.numeric(df2$Sub_metering_1)
    df2$Sub_metering_2 <- as.numeric(df2$Sub_metering_2)
    df2$Sub_metering_3 <- as.numeric(df2$Sub_metering_3)
    df2
}

# Produce the Global Active Power plot from the given data frame.
# Assume the caller set the device. 
#
plot2 <- function(df2, ylabstr="Global Active Power") {
    plot(df2$datetime, df2$Global_active_power, xlab="", ylab=ylabstr, type="n")
    lines(df2$datetime, df2$Global_active_power, type="l", col="black")
}

# Produce the Energy sub metering plot from the given data frame.
# Assume the caller set the device. 
#
plot3 <- function(df2) {
    plot(df2$datetime, df2$Sub_metering_1, type="n", xlab="", ylab="Energy sub metering")
    lines(df2$datetime, df2$Sub_metering_1, type="l")
    lines(df2$datetime, df2$Sub_metering_2, type="l", col="red")
    lines(df2$datetime, df2$Sub_metering_3, type="l", col="blue")
    legend("topright",legend=c('Sub_metering_1','Sub_metering_2','Sub_metering_3'), col=c('black','red', 'blue'), lty=c(1,1,1))
}

# Produce the Voltage plot from the given data frame.
# Assume the caller set the device. 
#
plot4 <- function(df2) {
    with(df2, plot(datetime, Voltage, type="n"))
    with(df2, lines(datetime, Voltage, type="l", col="black"))
}

# Produce the Global reactive power plot from the given data frame.
# Assume the caller set the device. 
#
plot5 <- function(df2) {
    with(df2, plot(datetime, Global_reactive_power, type="n"))
    with(df2, lines(datetime, Global_reactive_power, type="l", col="black"))
}


#
# Call readit() to create data frame from data file.
# Open output png file.
# Set up plotting row and column count.
# Call plotting functions in the right order to produce the four plots.
# Close output file.
#
df <- readit()
png("plot4.png", height=480, width=480)
par(mfrow=c(2,2))
plot2(df)
plot4(df)
plot3(df)
plot5(df)
dev.off()
