plot3 <- function() {

  cols <- c("date","time","global_active_power","global_reactive_power",
           "voltage","global_intensity","sub_metering_1","sub_metering_2","sub_metering_3")  
  
  # Making some assumptions here, given 2075259 rows for 47 months.
  # So there are nearly 50000 rows for each month.
  # We are interested in Feb 2007 which is the 3rd month,
  # so skip the first 50000 rows and from there read the next 100000.
  # Also assumed is that the date and time are in sequence.
  # The log file indicates that this is so.
  # We first narrow down the entries nearer to Feb 2007.
  tmp <- read.table("household_power_consumption.txt", sep=";", col.names=cols, 
            skip=50000, nrows=100000, stringsAsFactors=FALSE)
  dt <- paste(tmp$date,tmp$time,sep="")
  dt = strptime(dt, "%d/%m/%Y %H:%M:%S")
  tmp$date <- as.Date(dt, "%Y-%m-%d %H:%M:%S")
  tmp <- na.omit(tmp)
  
  # Now we retrieve the data for the 2 days in Feb 2007 only.
  dates_to_view <- as.Date(c("2007-02-01","2007-02-02"))
  in_dates_to_view <- tmp$date %in% dates_to_view
  dat <- subset(tmp, in_dates_to_view)
  
  # We then convert the classes for the columns so that we can plot.
  dat$date <- as.POSIXct(paste(as.character(dat$date), dat$time, sep=" "))
  dat$sub_metering_1 <- as.numeric(dat$sub_metering_1)
  dat$sub_metering_2 <- as.numeric(dat$sub_metering_2)
  dat$sub_metering_3 <- as.numeric(dat$sub_metering_3)
  
  png("plot3.png")
  
  plot(dat$date, dat$sub_metering_1, xlab="", ylab="Energy sub metering", type="n")
  line_colours <- c("black","red","blue")
  lines(dat$date, dat$sub_metering_1, col=line_colours[1])
  lines(dat$date, dat$sub_metering_2, col=line_colours[2])
  lines(dat$date, dat$sub_metering_3, col=line_colours[3])
  legend("topright", col=line_colours, , lty=c("solid"), 
         legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
  
  dev.off()
            	
}