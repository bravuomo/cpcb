library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

myPath <- 'DATA_PATH'
myFiles <- list.files(myPath)

myCols <- c('Timestamp', 'PM2.5 (µg/m³)', 'PM10 (µg/m³)', 
			#'NO (µg/m³)', 'NO2 (µg/m³)', 'NOx (ppb)', 
			#'NH3 (µg/m³)', 'SO2 (µg/m³)', 'CO (mg/m³)', 
			#'Ozone (µg/m³)', 
			'Ozone (µg/m³)'#, 
			#'AT (°C)', 'RH (%)', 'SR (W/mt2)', 'VWS (m/s)'
			 )

#list_of_dfs = lapply(list.files(myPath, pattern = '*csv'), 
list_of_dfs = lapply(myFiles, 
    function(x) {
		fname <- strsplit(x,"_")
		f_site_no <- fname[[1]][6]
		f_year <- fname[[1]][4]
		f_hourly <- fname[[1]][3]
        dat = read_csv(paste(myPath,'/',x,sep=''), col_select = myCols, show_col_types = FALSE)
        dat$Site_No = f_site_no
        dat$Saal <- f_year
        dat$Samay <- with(dat,strptime(Timestamp, "%Y-%m-%d %H:%M:%S"))
        dat$Year <- with(dat,dat$Samay$year)
        dat$DoH <- f_hourly       
        return(dat)
    })
    
merged_df = list_of_dfs %>% bind_rows()

oz_polyfit <- lm(merged_df$`Ozone (µg/m³)` ~ poly(merged_df$`Timestamp`, 2, raw=TRUE))
oz_lin_fit <- lm(merged_df$`Ozone (µg/m³)` ~ merged_df$`Timestamp`)
# write.csv(summary(oz_lin_fit)['coefficients'],file='Ozone_LM_result.csv')

# zzz <- merged_df %>% group_by(Site_No) %>% group_by(Saal) %>% filter(merged_df, sum(!is.na(`PM2.5 (µg/m³)`)) * 100/length(Saal) >= 80)
merged_df %>% ggplot(aes(x = Saal, y = `PM2.5 (µg/m³)`, group = Saal)) + geom_boxplot(fill="slateblue", alpha=0.2) + xlab('Year') + ylab('PM2.5 (µg/m³)') + facet_wrap(~ Site_No)
ggsave(file = "PM2.5_Box.jpg", device = "jpeg")
merged_df %>% ggplot(aes(x = Saal, y = `PM10 (µg/m³)`, group = Saal)) + geom_boxplot(fill="slateblue", alpha=0.2) + xlab('Year') + ylab('PM10 (µg/m³)') + facet_wrap(~ Site_No)
ggsave(file = "PM10_Box.jpg", device = "jpeg")
merged_df %>% ggplot(aes(x = Saal, y = `Ozone (µg/m³)`, group = Saal)) + geom_boxplot(fill="slateblue", alpha=0.2) + xlab('Year') + ylab('Ozone (µg/m³)') + facet_wrap(~ Site_No)
ggsave(file = "Ozone_Box.jpg", device = "jpeg")


merged_df %>% ggplot(aes(x = Timestamp, y = `Ozone (µg/m³)`)) + geom_point(color = 'gray') + ylab('Ozone (µg/m³)') + 
	geom_smooth( 
		#aes(color = "y ~ x", fill = "y ~ x"),
		#aes(fill = "red"),
		level = 0.90, 
		method = "lm", 
		formula = y~x) +
		#se = TRUE, # Plot the line only (without confidence bands)
		#fullrange = FALSE) + # The fit spans the full range of the horizontal axis)
	#stat_cor(p.accuracy = 0.001,#label.y = 300, 
		#aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
	#stat_regline_equation(label.y.npc = 0.9) +
    theme_light() + 
    facet_wrap(~ Site_No)

ggsave(file = "Ozone_Trend_LM.jpg", device = "jpeg")

merged_df %>% ggplot(aes(x = Timestamp, y = `Ozone (µg/m³)`)) + geom_point(color = 'gray') + ylab('Ozone (µg/m³)') + 
	geom_smooth( 
		#aes(color = "y ~ x", fill = "y ~ x"),
		#aes(fill = "red"),
		level = 0.90, 
		method = "gam", 
		formula = y ~ s(x, bs = "cs")) +
		#se = TRUE, # Plot the line only (without confidence bands)
		#fullrange = FALSE) + # The fit spans the full range of the horizontal axis)
	#stat_cor(p.accuracy = 0.001,#label.y = 300, 
		#aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
	#stat_regline_equation(label.y.npc = 0.9) +
    theme_light() + 
    facet_wrap(~ Site_No)

ggsave(file = "Ozone_Trend_GAM.jpg", device = "jpeg")

merged_df %>% ggplot(aes(x = Timestamp, y = `Ozone (µg/m³)`)) + geom_point(color = 'gray') + ylab('Ozone (µg/m³)') + 
	geom_smooth( 
		#aes(color = "y ~ x", fill = "y ~ x"),
		#aes(fill = "red"),
		level = 0.90, 
		method = "lm", 
		formula = y ~ poly(x,2, raw=TRUE)) +
		#se = TRUE, # Plot the line only (without confidence bands)
		#fullrange = FALSE) + # The fit spans the full range of the horizontal axis)
	#stat_cor(p.accuracy = 0.001,#label.y = 300, 
		#aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
	#stat_regline_equation(label.y.npc = 0.9) +
    theme_light() + 
    facet_wrap(~ Site_No)

ggsave(file = "Ozone_Trend_POLY.jpg", device = "jpeg")

#???? formula = y ~ x + I(x^2)

# https://stats.oarc.ucla.edu/r/faq/how-can-i-explore-different-smooths-in-ggplot2/
# https://m-clark.github.io/generalized-additive-models/application.html

#merged_df %>% ggplot(aes(x = Timestamp, y = `Ozone (µg/m³)`)) + geom_point(color = 'gray') + ylab('Ozone (µg/m³)') + 
#	geom_smooth( 
#		#aes(color = "y ~ x", fill = "y ~ x"),
#		#aes(fill = "red"),
#		level = 0.90, 
#		method = "lm") + 
#		#formula = y~x, 
#		#se = TRUE, # Plot the line only (without confidence bands)
#		#fullrange = FALSE) + # The fit spans the full range of the horizontal axis)
#    theme_light() + 
#    facet_wrap(~ Site_No)

#merged_df %>% ggplot(aes(x = Timestamp, y = `Ozone (µg/m³)`)) + geom_point(color = 'gray') + ylab('Ozone (µg/m³)') + 
#	geom_smooth(aes(fill = "red")) +
#		#aes(color = "y ~ x", fill = "y ~ x"),
#		#aes(fill = "red"),
#		#level = 0.90, 
#		#method = "lm") +
#		#formula = y~x, 
#		#se = TRUE, # Plot the line only (without confidence bands)
#		#fullrange = FALSE) + # The fit spans the full range of the horizontal axis)
#	stat_cor(p.accuracy = 0.001,#label.y = 300, 
#           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
#	stat_regline_equation(label.y.npc = 0.9) +

#    theme_light() + 
#    facet_wrap(~ Site_No)



p.list = lapply(sort(unique(merged_df$Site_No)), function(i) {
  i <- ggplot(merged_df[merged_df$Site_No==i,], aes(x = Saal, y = `Ozone (µg/m³)`, group = Saal)) +
    geom_boxplot(fill="slateblue", alpha=0.2) +
    xlab('Year') + 
    ylab('Ozone (µg/m³)') +
    facet_wrap(~ Site_No)
})


for (p in 1:length(p.list)) {
    file_name = paste(p, ".jpg", sep="")
    jpeg(file_name)
    print(p.list[[p]])
    dev.off()
}
