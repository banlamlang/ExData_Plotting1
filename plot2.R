plot2 <- function() {

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
  dat <- subset(tmp, in_dates_to_view, select=date:global_active_power)
  
  # We then convert the class for the columns so that we can plot.
  dat$date <- as.POSIXct(paste(as.character(dat$date), dat$time, sep=" "))
  dat$global_active_power <- as.numeric(dat$global_active_power)
  
  png("plot2.png")
  
  plot(dat$date, dat$global_active_power, xlab="", ylab="Global Active Power (kilowatts)", type="n")
  lines(dat$date, dat$global_active_power)
  
  dev.off()
            	
}