# Writes following to RESULTS-8hMax
# "MDA-8","MAD-8","From_Timestamp","To_Timestamp","Six_M_Timestamp","Total_Obs","NA_Ozone_Obs","Ste_No","Year","Hourly","Site_Name","Site_Board"
# MDA-8 : No of days in whole year when Max_Daily_Ozone > 100
# MAD-8 : Average of Max_07:00_to_19:00_Ozone during 6 months Jan-June
# Total_Obs : Total days in year
# NA_Ozone_Obs: Days where Ozone reading NA

library(dplyr)
library(tidyr)
library(readr)

myPath <- 'YOUR PATH TO DATA'
myFiles <- list.files(myPath)

setwd('YOUR PATH TO RESULT')
result_file <- 'Results-8HMax'

shuru <- "01-01 07:00:00"
six_m <- "06-30 18:59:59"
khatm <- "12-31 18:59:59"

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
	
	mySixM <- paste(mySaal, six_m, sep="-")
	mySX <- mySixM
	mySixM <- strptime(mySixM, "%Y-%m-%d %H:%M:%S")
	
	
	myEnd <- paste(mySaal, khatm, sep="-")
	myET <- myEnd
	myEnd <- strptime(myEnd, "%Y-%m-%d %H:%M:%S")
	
	myData$Mahina <- with(myData,myData$Samay$mon)
	myData$Ghanta <- with(myData,myData$Samay$hour)
	
	# Daily max & mean for whole year
	max_mean_8h <- myData %>% group_by(Date) %>% summarise(across(.cols = 'Ozone (µg/m³)', .f = list(max,mean)))
	
	# Daily max & mean for Jan-June 07:00-19:00
	# Use which?
	max_mean_8h_6m <- filter(myData, Date >= as.Date(myStart) & Date <= as.Date(mySixM), Ghanta > 6 & Ghanta < 20)
	max_mean_8h_6m <- max_mean_8h_6m %>% group_by(Date) %>% summarise(across(.cols = 'Ozone (µg/m³)', .f = list(max,mean)))

	rez <- data.frame(
	# No of days in whole year when daily O_max > 100
	MDA_8 = length(which(max_mean_8h$'Ozone (µg/m³)_1' > 100)),
	# Average of daily O_max during 6 months Jan-June (07:00 to 19:00)
	MAD_8 = my.mean(max_mean_8h_6m$'Ozone (µg/m³)_1'),
	Start_Date = c(myST),
	Six_M_Date = c(mySX),
	End_Date = c(myET),
	#Total_Obs = length(myData$Timestamp),
	Total_Obs = length(max_mean_8h$Date),
	#NA_Ozone_Obs = sum(is.na(myData$'Ozone (µg/m³)'))
	NA_Ozone_Obs = sum(is.na(max_mean_8h$'Ozone (µg/m³)_1'))
	)
	return(rez)
}

# analyze(myFiles[15])

for (f in myFiles) {	
	f_result <- analyze(f)
	#fname <- strsplit(f,"_")[[1]][3:6]
	#fname <- paste(unlist(fname), collapse='_')	
	fname <- strsplit(f,"_")
	
	f_site_no <- fname[[1]][6]
	f_result$Site_No <- with(f_result, f_site_no)
	
	f_year <- fname[[1]][4]
	f_result$Year <- with(f_result, f_year)
	
	f_hourly <- fname[[1]][3]
	f_result$Hourly	<- with(f_result, f_hourly)
	
	
	f_site_name <- toString(fname[[1]][7:10])
	f_result$Site_Name <- with(f_result, f_site_name)
	
	f_site_board <- sapply(fname, tail, 3)[2]
	f_result$Site_Board <- with(f_result, f_site_board)
	
	write.table(f_result, file = result_file, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
	
	
	
	print(paste("Done analyzing Site No", f_site_no, sep=' '))
	
	
}

print("Done printing all to RESULTS-8hMax")
