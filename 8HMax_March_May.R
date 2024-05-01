# Make a blank csv with name RESULT_FILE

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
 

myPath <- 'PATH_TO_DATA_FOLDER'
myFiles <- list.files(myPath)


result_file <- 'PATH_TO_RESULT_FILE'

shuru <- "03-01 00:00:00"
khatm <- "05-31 23:59:59"

my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
my.mean <- function(x) ifelse( !all(is.na(x)), mean(x, na.rm=T), NA)

analyze <- function(f) {
	# Reading CSV raw data into R data frame
	dfile <- paste(myPath,'/',f,sep='')
	myData <- read_csv(dfile, col_select=c('Timestamp', 'Ozone (µg/m³)'), show_col_types = FALSE)
	
	# Timestamp, Datetime processing
	myData$Samay <- with(myData,strptime(Timestamp, "%Y-%m-%d %H:%M:%S"))
	myData$Date <- with(myData,as.Date(Timestamp))
	mySaal <- substr(myData$Timestamp[1],1,4)
	
	myStart <- paste(mySaal, shuru, sep="-")
	myST <- myStart
	myStart <- strptime(myStart, "%Y-%m-%d %H:%M:%S")

	myEnd <- paste(mySaal, khatm, sep="-")
	myET <- myEnd
	myEnd <- strptime(myEnd, "%Y-%m-%d %H:%M:%S")
	
	myData$Mahina <- with(myData,myData$Samay$mon)
	myData$Ghanta <- with(myData,myData$Samay$hour)
	
	# Whole year data
	#max_mean_8h <- myData %>% group_by(Date) %>% summarise(across(.cols = 'Ozone (µg/m³)', .f = list(max,mean)))
	
	# Daily max & mean for Mar-May
	# Use which?
	max_mean_period <- filter(myData, Date >= as.Date(myStart) & Date <= as.Date(myEnd))
	max_mean_summer <- max_mean_period %>% group_by(Date) %>% summarise(across(.cols = 'Ozone (µg/m³)', .f = list(max,mean)))
	max_summer <- max_mean_summer[,1:2]
	
	return(max_summer)
}

# analyze(myFiles[15])

for (f in myFiles) {	
	f_result <- analyze(f)
	#fname <- strsplit(f,"_")[[1]][3:6]
	#fname <- paste(unlist(fname), collapse='_')	
	fname <- strsplit(f,"_")
	
	f_site_no <- fname[[1]][6]
	f_result$Site_No <- with(f_result, rep(f_site_no, length(f_result$Date)))
	
	f_year <- fname[[1]][4]
	f_result$Year <- with(f_result, rep(f_year, length(f_result$Date)))
	
#	f_hourly <- fname[[1]][3]
#	f_result$Hourly	<- with(f_result, f_hourly)
	
	
#	f_site_name <- toString(fname[[1]][7:10])
#	f_result$Site_Name <- with(f_result, f_site_name)
	
#	f_site_board <- sapply(fname, tail, 3)[2]
#	f_result$Site_Board <- with(f_result, f_site_board)
	
	write.table(f_result, file = result_file, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
	
	
	
	print(paste("Done analyzing Site No", f_site_no, sep=' '))
	
	
}

print("Done printing all to RESULTS-Summer-All-Sites")


#data=data.frame(variety, treatment ,  note)
 
## grouped boxplot
#ggplot(data, aes(x=variety, y=note, fill=treatment)) + 
#    geom_boxplot()

c_names <- c("Date","Oz_Max","Site","Year")

boxed <- read_csv('PATH_TO_RESULT_FILE', col_names = c_names, col_types="Ddcc")

# https://r-graph-gallery.com/265-grouped-boxplot-with-ggplot2.html
# https://stackoverflow.com/questions/46075788/ggplotgeom-boxplot-how-to-change-the-width-of-one-box-group-in-r#51251923

p0 <- ggplot(boxed, aes(x=Site, y=Oz_Max, fill=Year)) + 
    geom_boxplot(position = position_dodge2(preserve = "single"))

p1 <- ggplot(boxed, aes(x=Site, y=Oz_Max, fill=Year)) + 
    geom_boxplot(position = position_dodge2(preserve = "single")) +
    facet_wrap(~Year)
# one box per variety
p2 <- ggplot(boxed, aes(x=Site, y=Oz_Max, fill=Year)) + 
    geom_boxplot(position = position_dodge2(preserve = "single")) +
    facet_wrap(~Site, scale="free")

