library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

myPath <- 'PATH_TO_DATA'
myFiles <- list.files(myPath)

result_file <- 'PATH_TO_RESULT'

myCols <- c('Timestamp', 'PM2.5 (µg/m³)', 'PM10 (µg/m³)', 
			'NO (µg/m³)', 'NO2 (µg/m³)', 'NOx (ppb)', 
			'CO (mg/m³)', #'NH3 (µg/m³)', 'SO2 (µg/m³)', 
			'Ozone (µg/m³)', 
			'AT (°C)', 'RH (%)', 'SR (W/mt2)', 'VWS (m/s)'
			 )

my.sd <- function(x) ifelse( !all(is.na(x)), sd(x, na.rm=T), NA)
my.mean <- function(x) ifelse( !all(is.na(x)), mean(x, na.rm=T), NA)
Total_Obs <- function(x) ifelse( !all(is.na(x)), length(x), NA)
Non_NA_Obs <- function(x) ifelse( !all(is.na(x)), sum(!is.na(x)) * 100/length(x), NA)


analyze <- function(f) {
	# Reading CSV raw data into R data frame
	dfile <- paste(myPath,'/',f,sep='')
	myData <- read_csv(dfile, col_select = myCols, show_col_types = FALSE)
	#myData <- read_csv(dfile, show_col_types = FALSE)
	
	urData <- myData %>%
	summarise(across(
		.cols = is.numeric, 
		.fns = list(N = Total_Obs, Non_NA = Non_NA_Obs, Mean = my.mean, SD = my.mean), 
		.names = "{.col}_{.fn}"
		))
		
	
	
	# Timestamp, Datetime processing
	#myData$Samay <- with(myData,strptime(Timestamp, "%Y-%m-%d %H:%M:%S"))
	#myData$myDate <- with(myData,as.Date(Timestamp))
	#urData$Saal <- substr(myData$Timestamp[1],1,4)
	#myData$Mahina <- with(myData,myData$Samay$mon)

	return(urData)
}


for (f in myFiles) {	
	f_result <- analyze(f)
	
	#fname <- strsplit(f,"_")[[1]][3:6]
	#fname <- paste(unlist(fname), collapse='_')	
	
	
	
	fname <- strsplit(f,"_")
	
	
	f_site_no <- fname[[1]][6]
	f_result$Site_No <- with(f_result, f_site_no)
	
	f_year <- fname[[1]][4]
	f_result$Year <- with(f_result, f_year)
	
	#f_hourly <- fname[[1]][3]
	#f_result$Hourly	<- with(f_result, f_hourly)
	
	
	#f_site_name <- toString(fname[[1]][7:10])
	#f_result$Site_Name <- with(f_result, f_site_name)
	
	f_site_board <- sapply(fname, tail, 3)[2]
	f_result$Site_Board <- with(f_result, f_site_board)
	
	f_result <- f_result %>% relocate(Site_No, Year, Site_Board)
	write.table(f_result, file = result_file, append = TRUE, row.names = FALSE, col.names = TRUE, sep = ",")
	
	
	
	#print(paste(f_result, f_site_no, f_year,  sep=' '))
	#print(paste(f_result))#, f_site_no, f_year,  sep=' '))
		
			
	
}
