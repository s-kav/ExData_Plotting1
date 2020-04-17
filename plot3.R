library(data.table)

Url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
## fixing a link to a variable
download.file(Url, destfile = "./epc.zip", method = "curl")
## downloading needed file

unzip(zipfile = "./epc.zip", exdir = "./data")
## unpack loaded file

hpc <- data.table::fread("./data/household_power_consumption.txt",  header = TRUE, na.strings = "?", sep = ";", dec = ".", data.table = FALSE)
## read a file to work dataframe correspodence to conditions: gap cells were indicated by sign "?"; separator between columns is ";"; with header

object.size(hpc)
## check a volume of memory for this dataframe
##
hpc$Date <- as.Date(hpc$Date, "%d/%m/%Y")
## conversation the first column as Date by the appropriate format

hpc_new <- subset(hpc, Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02"))
## cleaning our dataframe based on a range of dates

rm(hpc)
## deleting initial (generic) dataframe

hpc_new <- hpc_new[complete.cases(hpc_new), ]
## checking on losses values

DaT <- paste(hpc_new$Date, hpc_new$Time, sep = " ")
## form new vector from our work dataframe (from two columns)

DaT <- strftime(DaT, format = "%d/%m/%Y %H:%M:%S")
## reformatting new dataframe in accordance with needed Date & Time formats

DaT <- as.POSIXct(DaT, tryFormats = "%d/%m/%Y %H:%M:%S")
## conversation new vector

hpc_new <- hpc_new[ ,!(names(hpc_new) %in% c("Date","Time"))]
## deleting two excess cloumns from our work dataframe

hpc_new <- cbind("Date_Time" = DaT, hpc_new)
## obtaining new work dataframe with modified type of the first column and preparing to the following plots

par(mfrow = c(1, 1), mar = c(2, 4, 2, 1))
with(hpc_new, {
        plot(Sub_metering_1 ~ Date_Time, type = "l",                              ylab = "Energy sub metering", xlab = "")
        lines(Sub_metering_2 ~ Date_Time, col = "Red")
        lines(Sub_metering_3 ~ Date_Time, col = "Blue")
})
legend("topright", col = c("black", "red", "blue"), lwd = c(1, 1, 1),
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
dev.copy(png,"./figure/plot3.png", width = 480, height = 480)
dev.off()
## plotting in accordance with needed conditions (Plot 3); write as PNG-file with specified resolutions (480 x 480 pxs); close a connection with a device
