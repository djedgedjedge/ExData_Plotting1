plot2 <- function(){
        
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
        png(paste(plot_filepath, "/", "plot2.png", sep=""))
        
        # Creation of the plot to give the Global Active Power by week days
        plot(consumption$date_time, 
             consumption$Global_active_power, 
             type ="l", 
             ylab = "Global Active Power (kilowatts)")
        dev.off()
}