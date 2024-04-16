library(dplyr)
myPath <- 'TYPE_PATH'
myFiles <- list.files(myPath)

shuru <- "01-15 00:00:00"
khatm <- "04-15 23:59:59"

analyze <- function(f) {
	dfile <- paste(myPath,'/',f,sep='')
	df <- read.csv(dfile)
	myData <- df[c("Timestamp", "Ozone..µg.m..")]	
	myData$Samay <- with(myData,strptime(Timestamp, "%Y-%m-%d %H:%M:%S"))
	
	mySaal <- substr(myData$Timestamp[1],1,4)
	myStart <- paste(mySaal, shuru, sep="-")
	myStart <- strptime(myStart, "%Y-%m-%d %H:%M:%S")
	myEnd <- paste(mySaal, khatm, sep="-")
	myEnd <- strptime(myEnd, "%Y-%m-%d %H:%M:%S")
	
		
	#myData$Mahina <- with(myData,myData$Samay$mon)
	#myData$Ghanta <- with(myData,myData$Samay$hour)
	myData$ppb_40 <- with(myData, (((myData$Ozone..µg.m..)/2.0)-40.0))
	#df_ppb_40 <- filter(myData, Mahina > 2 & Mahina < 6, Ghanta > 6 & Ghanta < 20, ppb_40 > 0)
	df_ppb_40 <- filter(myData, Samay > myStart & Samay < myEnd, ppb_40 > 0)
	#return(paste(myStart, myEnd, sum(df_ppb_40$ppb_40), sep=' '))
	return(paste(sum(df_ppb_40$ppb_40), 'from', myStart, 'to', myEnd, sep='  '))
}


for (f in myFiles) {
	fname <- strsplit(f,"_")[[1]][3:6]
	fname <- paste(unlist(fname), collapse='_')
	print(paste(analyze(f), fname, sep='  '))
	
}

