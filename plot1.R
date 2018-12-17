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

# Produce the first plot from the given data frame.
# Assume the caller set the device.
#
plot1 <- function(df2) {
    hist(df2$Global_active_power, xlab="Global Active Power (kilowatts)", 
         main="Global Active Power", col="red")
}

png("plot1.png", height=480, width=480)
plot1(readit())
dev.off()
