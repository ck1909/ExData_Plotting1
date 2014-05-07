#We will only be using data from the dates 2007-02-01 and 2007-02-02
#So read the data from just those dates to optimize memory usage
read_file <- function(f) {
    con <- file(f, "r")
    #store column headers
    col_names <- strsplit(readLines(con, 1), ";")
    list <- list()
    #read file line by line, store filtered lines in list
    while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {        
        line <- strsplit(line, ";")
        #add to list only for dates 1/2/2007 and 2/2/2007
        if((line[[1]][1] == "1/2/2007") || (line[[1]][1] == "2/2/2007")) {
            list <- c(list, line)
        }
    }
    #turn list into data frame
    d <- do.call(rbind, list)
    colnames(d) <- col_names[[1]]
    close(con)
    d <- as.data.frame(d)
    #return tidy data
    return(d)
}

#core funtion to generate plot
get_plot4 <- function(d) {
    #create 4 graphs in a single 2x2 layout
    Sys.setlocale("LC_TIME", "English")
    #pick subset of relvant data
    m <- data.frame(paste(d$Date, d$Time))
    m <- cbind(m, d$Global_active_power, d$Global_reactive_power, d$Voltage, d$Sub_metering_1, d$Sub_metering_2, d$Sub_metering_3)
    colnames(m)<-c("DateTime", "Global_active_power", "Global_reactive_power", "Voltage", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
    m$DateTime <- strptime(as.character(m$DateTime), format="%d/%m/%Y %H:%M:%S")
    for(i in 2:7) m[,i] <- as.numeric(m[,i])
    
    #specify 2x2 layout
    par(mfrow = c(2,2))
    
    #plot 1
    plot(m$DateTime, m$Global_active_power, type="n", xlab="", ylab="Global Active Power")
    lines(m$DateTime, m$Global_active_power, lty=1)
    
    #plot 2
    plot(m$DateTime, m$Voltage, type="n", xlab="datetime", ylab="Voltage", ylim=c(234,246))
    lines(m$DateTime, m$Voltage, lty=1)
    
    #plot 3
    plot(m$DateTime, m$Sub_metering_1, type="n", xlab="", ylab="Energy sub metering")
    lines(m$DateTime, m$Sub_metering_1, lty=1, col="black")
    lines(m$DateTime, m$Sub_metering_2, lty=1, col="red")
    lines(m$DateTime, m$Sub_metering_3, lty=1, col="blue")
    legend("topright",
           legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
           lty=c(1,1,1), bty= "n", #box without border
           col=c("black","red", "blue"),
           #text.width=0.1,
           cex=.3)
    
    #plot 4
    plot(m$DateTime, m$Global_reactive_power, type="n", xlab="datetime", ylab="Global_reactive_power", ylim=c(0.0,0.5))
    lines(m$DateTime, m$Global_reactive_power, lty=1)
}

#generic function to save plot to a PNG file
save_plot <- function(data, plot_function, outfile_name) {
    png(paste0(outfile_name,".png"), width=480, height=480, units="px")
    plot_function(data)
    dev.off()
}

#main function to go from raw data to output PNG
main <- function(data_file_path) {
    data <- read_file(data_file_path)
    save_plot(data, get_plot4, "plot4.png")
}
