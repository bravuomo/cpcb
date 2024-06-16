library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

myPath <- 'DATA_Path'
myFiles <- list.files(myPath)

myCols <- c('Timestamp', 'PM2.5 (µg/m³)', 'PM10 (µg/m³)', 
			'NO (µg/m³)', 'NO2 (µg/m³)', 'NOx (ppb)', 
			'NH3 (µg/m³)', 'SO2 (µg/m³)', 'CO (mg/m³)', 
			'Ozone (µg/m³)', 
			'AT (°C)', 'RH (%)', 'BP (mmHg)', 'VWS (m/s)'
			 )

my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
my.mean <- function(x) ifelse( !all(is.na(x)), mean(x, na.rm=T), NA)

analyze <- function(f) {
	# Reading CSV raw data into R data frame
	dfile <- paste(myPath,'/',f,sep='')
	myData <- read_csv(dfile, col_select = myCols, show_col_types = FALSE)
	#myData <- read_csv(dfile, show_col_types = FALSE)
	
	#myData %>% select(any_of(myCols))
	
	# Timestamp, Datetime processing
	#myData$Samay <- with(myData,strptime(Timestamp, "%Y-%m-%d %H:%M:%S"))
	myData$myDate <- with(myData,as.Date(Timestamp))
	#myData$Saal <- substr(myData$Timestamp[1],1,4)
	#myData$Mahina <- with(myData,myData$Samay$mon)

	return(myData)
}

#tst <- analyze(myFiles[1])

for (f in myFiles) {	
	f_result <- analyze(f)
	
	#fname <- strsplit(f,"_")[[1]][3:6]
	#fname <- paste(unlist(fname), collapse='_')	
	
	
	
	fname <- strsplit(f,"_")
	
	
	f_site_no <- fname[[1]][6]
	#f_result$Site_No <- with(f_result, f_site_no)
	
	f_year <- fname[[1]][4]
	#f_result$Year <- with(f_result, f_year)
	
	f_hourly <- fname[[1]][3]
	#f_result$Hourly	<- with(f_result, f_hourly)
	
	
	f_site_name <- toString(fname[[1]][7:10])
	#f_result$Site_Name <- with(f_result, f_site_name)
	
	f_site_board <- sapply(fname, tail, 3)[2]
	#f_result$Site_Board <- with(f_result, f_site_board)
	
	#write.table(f_result, file = result_file, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
	
	
	
	#print(paste(f_result, f_site_no, f_year, f_hourly, sep=' '))
	
	for (colm in colnames(f_result)) {
		
		colm_name <- strsplit(colm," ")
		colm_name <- colm_name[[1]][1]
		len_colm <- sum(!is.na(f_result[[colm]]))
		Total_Obs <- length(f_result$myDate)
		avl_colm <- len_colm * 100 / Total_Obs
		
		site_desc <- paste(f_site_no, f_year, sep = '_')
		
		if (avl_colm > 79) {
			
			p_box <- ggplot(f_result, aes(y = f_result[[colm]])) +	geom_boxplot(fill="slateblue", alpha=0.2) + ylab(colm)
			ggsave(file = paste(site_desc, colm_name, '.jpg', sep = '_'), width = 2.75, height = 2, device='jpeg', dpi=300)
			print(paste(f_site_no, f_year, colm_name, round(avl_colm, 3), sep = ' '))
		} else {
			print(paste(f_site_no, f_year, colm_name, '< 80%', sep = ' '))
		}
	}
			
	
}
