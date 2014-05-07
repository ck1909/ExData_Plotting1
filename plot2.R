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
get_plot2 <- function(d) {
    #create line graph
    Sys.setlocale("LC_TIME", "English") #so weekdays are outputted in English
    #take required subset of data
    m <- data.frame(paste(d$Date, d$Time))
    m <- cbind(m, d$Global_active_power)
    colnames(m)<-c("DateTime", "Global_active_power")
    #set date format
    m$DateTime <- strptime(as.character(m$DateTime), format="%d/%m/%Y %H:%M:%S")
    m[,2] <- as.numeric(m[,2])
    plot(m$DateTime, m$Global_active_power, type="n", xlab="", ylab="Global Active Power (kilowatts)")
    lines(m$DateTime, m$Global_active_power, lty=1)
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
    save_plot(data, get_plot2, "plot2.png")
}
