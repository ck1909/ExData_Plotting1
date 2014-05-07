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
get_plot3 <- function(d) {
    #create multple line graph with legend
    Sys.setlocale("LC_TIME", "English")
    m <- data.frame(paste(d$Date, d$Time))
    m <- cbind(m, d$Sub_metering_1, d$Sub_metering_2, d$Sub_metering_3)
    colnames(m)<-c("DateTime", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
    m$DateTime <- strptime(as.character(m$DateTime), format="%d/%m/%Y %H:%M:%S")
    for(i in 2:4) m[,i] <- as.numeric(m[,i])
    plot(m$DateTime, m$Sub_metering_1, type="n", xlab="", ylab="Energy sub metering")
    lines(m$DateTime, m$Sub_metering_1, lty=1, col="black")
    lines(m$DateTime, m$Sub_metering_2, lty=1, col="red")
    lines(m$DateTime, m$Sub_metering_3, lty=1, col="blue")
    legend("topright",
           legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
           lty=c(1,1,1),
           col=c("black","red", "blue"),
           cex=.5, pt.cex=2, xjust=1)
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
    save_plot(data, get_plot3, "plot3.png")
}
