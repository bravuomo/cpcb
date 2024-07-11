library(dplyr)
library(tidyr)
library(readr)
#library(ggplot2)

myPath <- 'DATA_PATH'
myFiles <- list.files(myPath)

result_file <- 'RESULTS_PATH'


my_Months <- c(2, 3, 4)
myCols_8 <- c('Timestamp', 'Ozone (µg/m³)')
myCols_1 <- c('Timestamp', 'AT (°C)', 'NO2 (µg/m³)')
my_Metrics <- c('Ozone (µg/m³)', 'AT (°C)', 'NO2 (µg/m³)')

#Total_Obs <- function(x) ifelse( !all(is.na(x)), length(x), NA)
#Non_NA_Obs <- function(x) ifelse( !all(is.na(x)), sum(!is.na(x)) * 100/length(x), NA)
my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)

analyze <- function(f) {
	# Reading CSV raw data into R data frame
	dfile <- paste(myPath, '/', f, sep = '')
	myData <- read_csv(dfile, col_types = list(Timestamp ="T"), show_col_types = FALSE)
	
	ifelse(grepl("8Hrs", f, fixed = TRUE),
			myData <- myData %>% select(any_of(myCols_8)),
			myData <- myData %>% select(any_of(myCols_1))
			)
	myData$Date <- with(myData, as.Date(Timestamp))
	myData$Samay <- with(myData, strptime(Timestamp, "%Y-%m-%d %H:%M:%S"))
	myData$Month <- with(myData, myData$Samay$mon)
	
	urData <- myData %>% filter(Month %in% my_Months)
	
	urData <- urData %>% group_by(Date) %>%
	summarise(across(
		.cols = any_of(my_Metrics),
		#.cols = where(is.numeric) | where(is.logical),
		#.fns = list(N = Total_Obs, Valid_N = Non_NA_Obs, Max = my.max),
		.fns = list(Max = my.max),
		.names = "{.col}_{.fn}"
		))

	return(urData)
}

#tst <- analyze(myFiles[5])

my_DFs <- lapply(myFiles,
	function(x) {
		f_result <- analyze(x)
		fname <- strsplit(x, "_")
		
		f_result$Site <- fname[[1]][6]
		#f_result$Obs_Period <- fname[[1]][3]
		
		f_result <- f_result %>% relocate(Site)
		
		return(f_result)
	})

zxz <- Reduce(full_join, my_DFs)
write.table(zxz, file = result_file, append = FALSE, row.names = FALSE, col.names = TRUE, sep = ",")
