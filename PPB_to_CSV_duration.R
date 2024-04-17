library(dplyr)
#library(knitr)
myPath <- 'YOUR myPath'
myFiles <- list.files(myPath)

result_file <- 'Your myPath/Results-PPB40'


shuru <- "01-15 00:00:00"
khatm <- "04-15 23:59:59"



analyze <- function(f) {
	dfile <- paste(myPath,'/',f,sep='')
	df <- read.csv(dfile)
	myData <- df[c("Timestamp", "Ozone..µg.m..")]	
	myData$Samay <- with(myData,strptime(Timestamp, "%Y-%m-%d %H:%M:%S"))
	
	mySaal <- substr(myData$Timestamp[1],1,4)
	
	myStart <- paste(mySaal, shuru, sep="-")
	myST <- myStart
	myStart <- strptime(myStart, "%Y-%m-%d %H:%M:%S")
	
	myEnd <- paste(mySaal, khatm, sep="-")
	myET <- myEnd
	myEnd <- strptime(myEnd, "%Y-%m-%d %H:%M:%S")
	
		
	#myData$Mahina <- with(myData,myData$Samay$mon)
	#myData$Ghanta <- with(myData,myData$Samay$hour)
	myData$ppb_40 <- with(myData, (((myData$Ozone..µg.m..)/2.0)-40.0))
	#df_ppb_40 <- filter(myData, Mahina > 2 & Mahina < 6, Ghanta > 6 & Ghanta < 20, ppb_40 > 0)
	df_ppb_40 <- filter(myData, Samay > myStart & Samay < myEnd, ppb_40 > 0)
	#return(paste(myStart, myEnd, sum(df_ppb_40$ppb_40), sep=' '))
	#return(paste(sum(df_ppb_40$ppb_40), 'from', myStart, 'to', myEnd, sep='  '))
	#rez <- c(sum(df_ppb_40$ppb_40), myST, myET)
	rez <- data.frame(
	ppb40 = c(sum(df_ppb_40$ppb_40)),
	Start_Date = c(myST),
	End_Date = c(myET)
	)
	return(rez)
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
	
	f_hourly <- fname[[1]][3]
	f_result$Hourly	<- with(f_result, f_hourly)
	
	
	f_site_name <- toString(fname[[1]][7:10])
	f_result$Site_Name <- with(f_result, f_site_name)
	
	f_site_board <- sapply(fname, tail, 3)[2]
	f_result$Site_Board <- with(f_result, f_site_board)
	
	write.table(f_result, file = result_file, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
	
	
	
	print(paste("Done analyzing Site No", f_site_no, sep=' '))
	
	
}

print("Done printing all to RESULTS_PPB40")
