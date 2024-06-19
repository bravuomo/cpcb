library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

myPath <- 'DATA_PATH'
myFiles <- list.files(myPath)

result_file <- 'DESC.csv_RESULT_PATH'

myCols <- c('Timestamp', 'PM2.5 (µg/m³)', 'PM10 (µg/m³)', 
			'NO (µg/m³)', 'NO2 (µg/m³)', 'NOx (ppb)', 
			'CO (mg/m³)', #'NH3 (µg/m³)', 'SO2 (µg/m³)', 
			'Ozone (µg/m³)', 
			'AT (°C)', 'RH (%)', 'WS (m/s)', 'SR (W/mt2)'
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
		.cols = is.numeric | is.logical, 
		.fns = list(N = Total_Obs, Valid_N = Non_NA_Obs, Mean = my.mean, SD = my.sd), 
		.names = "{.col}_{.fn}"
		))

	return(urData)
}

#tst <- analyze(myFiles[5])

list_of_dfs = lapply(myFiles, 
    function(x) {
		f_result <- analyze(x)		
		fname <- strsplit(x,"_")
		
		f_site_no <- fname[[1]][6]
		f_result$Site_No = f_site_no
		
		f_year <- fname[[1]][4]
		f_result$Year <- f_year
		
		f_site_board <- sapply(fname, tail, 3)[2]
		f_result$Site_Board <- f_site_board
		
		f_o_period <- fname[[1]][3]
		f_result$Obs_Period <- f_o_period  
		
		f_result <- f_result %>% relocate(Site_No, Year, Site_Board, Obs_Period)   
        return(f_result)
    })
    
merged_summary = list_of_dfs %>% bind_rows()
write.table(merged_summary, file = result_file, append = FALSE, row.names = FALSE, col.names = TRUE, sep = ",")

#for (f in myFiles) {	
#	f_result <- analyze(f)	
	
#	fname <- strsplit(f,"_")
	
	
#	f_site_no <- fname[[1]][6]
#	f_result$Site_No <- with(f_result, f_site_no)
	
#	f_year <- fname[[1]][4]
#	f_result$Year <- with(f_result, f_year)
	
#	#f_hourly <- fname[[1]][3]
#	#f_result$Hourly	<- with(f_result, f_hourly)
	
	
#	#f_site_name <- toString(fname[[1]][7:10])
#	#f_result$Site_Name <- with(f_result, f_site_name)
	
#	f_site_board <- sapply(fname, tail, 3)[2]
#	f_result$Site_Board <- with(f_result, f_site_board)
	
#	f_result <- f_result %>% relocate(Site_No, Year, Site_Board)
#	write.table(f_result, file = result_file, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
	
	
	
#	#print(paste(f_result, f_site_no, f_year,  sep=' '))
#	#print(paste(f_result))#, f_site_no, f_year,  sep=' '))
		
			
	
#}


#result_header <- c("Site_No","Year","Site_Board","PM2.5 (µg/m³)_N","PM2.5 (µg/m³)_Non_NA","PM2.5 (µg/m³)_Mean","PM2.5 (µg/m³)_SD",
#	"PM10 (µg/m³)_N","PM10 (µg/m³)_Non_NA","PM10 (µg/m³)_Mean","PM10 (µg/m³)_SD",
#	"NO (µg/m³)_N","NO (µg/m³)_Non_NA","NO (µg/m³)_Mean","NO (µg/m³)_SD",
#	"NO2 (µg/m³)_N","NO2 (µg/m³)_Non_NA","NO2 (µg/m³)_Mean","NO2 (µg/m³)_SD",
#	"NOx (ppb)_N","NOx (ppb)_Non_NA","NOx (ppb)_Mean","NOx (ppb)_SD",
#	"CO (mg/m³)_N","CO (mg/m³)_Non_NA","CO (mg/m³)_Mean","CO (mg/m³)_SD",
#	"Ozone (µg/m³)_N","Ozone (µg/m³)_Non_NA","Ozone (µg/m³)_Mean","Ozone (µg/m³)_SD",
#	"RH (%)_N","RH (%)_Non_NA","RH (%)_Mean","RH (%)_SD",
#	"SR (W/mt2)_N","SR (W/mt2)_Non_NA","SR (W/mt2)_Mean","SR (W/mt2)_SD",
#	"AT (°C)_N","AT (°C)_Non_NA","AT (°C)_Mean","AT (°C)_SD",
#	"VWS (m/s)_N","VWS (m/s)_Non_NA","VWS (m/s)_Mean","VWS (m/s)_SD")
