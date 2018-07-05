plot4 <- function(){
        
        # Define file name and file path
        file <- "household_power_consumption.txt"
        filepath <- "./dataset"
        fileurl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        
        # Download and extract the file in case it is not existing in the dataset folder
        if (!file.exists(paste(filepath, "/", file,sep =""))){
                filezip = "household_power_consumption.zip"
                download.file(fileurl,filezip)
                dir.create(filepath)
                unzip(filezip, exdir = filepath ) 
                if (file.exists(paste(filepath, "/", file,sep =""))){
                        file.remove(filezip)
                }
                else
                        stop("error when unzipping")
        }
        
        # Create the dataset consumption by creating a POSIXct class column
        consumption <- (read.table(paste(filepath, "/", file,sep =""), 
                                   sep=";", 
                                   header = TRUE,
                                   na.strings = "?") 
                        %>% mutate(date_time = as.POSIXct(strptime(paste(Date, Time), "%d/%m/%Y %H:%M:%S")))
                        %>% select(-c("Date", "Time"))
                        %>% filter(date(date_time) %in% date(c("2007-02-01", "2007-02-02")))
        )
        
        
        # Define the path and the file name of the plot
        plot_filepath = "./figures"
        dir.create(plot_filepath)
        png(paste(plot_filepath, "/", "plot4.png", sep=""))
        
        # Creation of the set of 4 plots 
        par(mfrow = c(2, 2))
        plot(consumption$date_time, 
             consumption$Global_active_power, 
             type ="l", 
             ylab = "Global Active Power (kilowatts)")
        
        plot(consumption$date_time, 
             consumption$Voltage, 
             type ="l", 
             xlab = "datetime",
             ylab = "Voltage")
        
        plot(consumption$date_time, consumption$Sub_metering_1, type ="l", col = "black", ylab = "Energy sub metering", xlab = "")
        lines(consumption$date_time, consumption$Sub_metering_2, type ="l", col = "red")
        lines(consumption$date_time, consumption$Sub_metering_3, type ="l", col = "blue")
        legend("topright", col=c("black", "red","blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1)
        
        plot(consumption$date_time, 
             consumption$Global_reactive_power, 
             type ="l", 
             xlab = "datetime",
             ylab = "Global_reactive_power")
        
        dev.off()
}