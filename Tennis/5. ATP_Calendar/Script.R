

# Packages ----

library(ggplot2)
library(tidyverse)
library(rvest)
library(lubridate)
library(ggtext)
library(cowplot)
library(ragg)



# Directory 

setwd("C:/Users/maxim/Documents/Site_Web/Git/R-Dataviz/Tennis/5. ATP_Calendar")



# Data ----



# ATP calendar data is from https://www.perfect-tennis.com/atp-calendar/

df <- read_delim("data.txt", delim = ";", escape_double = FALSE, col_names = F, trim_ws = TRUE)

df <- df %>% select(1,2,4,5,6)
colnames(df) <- c('Name','Category','Start','End','Surface')




# Function to have 2023 calendar (https://github.com/tashapiro/peloton-stats/blob/main/code/peloton-active-days-calendar.R)

get_calendar <- function(start_date, end_date) {

n_days     <- interval(start_date,end_date)/days(1)
date       <- start_date + days(0:n_days)
month_name <- format(date,"%B")
month_num  <- format(date,"%m")
year       <- format(date,"%Y")
day_num    <- format(date,'%d')
day        <- wday(date, label=TRUE) %>% as.character()
week_num   <- strftime(date, format = "%V")
cal        <- data.frame(date, year, month_name, month_num, day_num, day, week_num) 
cal[cal$week_num>=52 & cal$month_num=="01","week_num"] = 00
  
week_month <- cal %>% group_by(year,month_name, week_num) %>% dplyr::summarise() %>% mutate(week_month_num = row_number())
  
cal <- merge(cal, week_month, by = c("month_name" = "month_name", "week_num" = "week_num", "year" = "year"))
  
  
  
# Don't know why the output is in french

cal <- cal %>% mutate(month_name = case_when(month_name == "janvier"   ~ "January",
  																					 month_name == "février"   ~ "February",
  																					 month_name == "mars"      ~ "March",
  																					 month_name == "avril"     ~ "April",
  																					 month_name == "mai"       ~ "May",
  																					 month_name == "juin"      ~ "June",
  																					 month_name == "juillet"   ~ "July",
  																					 month_name == "août"      ~ "August",
  																					 month_name == "septembre" ~ "September",
  																					 month_name == "octobre"   ~ "October",
  																					 month_name == "novembre"  ~ "November")
  										)
  
  
cal <- cal %>% mutate(day = case_when(day == "lun\\."   ~ "Mon",
  																		day == "mar\\."   ~ "Tue",
  																		day == "mer\\."   ~ "Wed",
  																		day == "jeu\\."   ~ "Thu",
  																		day == "ven\\."   ~ "Fri",
  																		day == "sam\\."   ~ "Sat",
  																		day == "dim\\."   ~ "Sun")
  										)
cal$month_name <- factor(cal$month_name, levels = c("January","February","March","April","May","June","July","August","September","October","November","December"))
cal$day        <- factor(cal$day,        levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
  
cal <- cal %>% mutate(day_week_num = case_when(day == "Mon" ~ 1,
  																						 day == "Tue" ~ 2,
  																						 day == "Wed" ~ 3,
  																						 day == "Thu" ~ 4,
  																						 day == "Fri" ~ 5,
  																						 day == "Sat" ~ 6,
  																						 day == "Sun" ~ 7
  																						 )
  											)
  
  
cal <- cal %>% arrange(date)
  
return(cal)
  
}


# Create date range

start_date <- as.Date('2023-01-01')
end_date   <- as.Date('2023-11-30')


# Create calendar

cal <- get_calendar(start_date, end_date)




# Data processing ----


# Transform the date columns

df$Start <- lubridate::dmy(df$Start)
df$End   <- lubridate::dmy(df$End)


# For after :

df_end_date <- df %>% select(Name,End)



# Need to have only 2023 days so the first date need to be 2023-01-01 and not 2022-12-29

df$Start[1] <- as.Date("2023-01-01")


# Count the number of days in each tournament

df <- df %>% mutate(n_days = interval(Start,End)/days(1))


# Create n duplicated lines for each tournament

n_rows <- nrow(df)

for(i in 1:n_rows){
	
j_start <- 1
while(j_start <= df$n_days[i]){
	
df[nrow(df)+1,]    <- df[i,]
df$Start[nrow(df)] <- df$Start[nrow(df)] + days(j_start)
df$End[nrow(df)]   <- NA
j_start            <- j_start + 1

}

}


df              <- df %>% select(-c(n_days,End)) %>% arrange(Start,Name)
colnames(df)[3] <- 'date'
df              <- left_join(cal[,c('date','month_num','day_num','week_month_num','day_week_num')], df[,c('date','Category','Name')])


# Create a first version of placement for the new variables 

df <- df %>% group_by(month_num) %>% mutate(xmin = day_week_num - 1,
																					  xmax = day_week_num,
																					  ymin = max(week_month_num) - week_month_num,
																					  ymax = max(week_month_num) - week_month_num + 1,
																						max_week_month = max(week_month_num)
																					  )

# Several months are 6 weeks and others 5. So there will be 2 different height of calendar month viz. Need to have the same

for(i in 1:nrow(df)){
	
	if(max(df %>% ungroup() %>% filter(month_num == df$month_num[i]) %>% select(week_month_num) %>% pull()) <= 5){
		
		df$ymin[i] = df$ymin[i] * 1.2
		df$ymax[i] = df$ymax[i] * 1.2
		
	}
	
}



																					  

# Number of tournament by date

df <- df %>% group_by(date) %>% mutate(nbr_tournaments = n())




# Now I need to change the y placement values for the line who have mode than 1 tournament by date
# The principle is to divide one "line" to have several color by tournament. Because for now is surperposate, so we see nothing
# Also for a better visualisation, I need no transform the Name variable in factor. And to know the order, I will use df_end_date

#View(left_join(df[,c('date','Name','Category','nbr_tournaments')],df_end_date) %>% group_by(Name) %>% slice(1))


df$Name <- factor(df$Name, 
									levels = c("United Cup","Adelaide International 1","Tata Open Maharashtra","ASB Classic","Adelaide International 2","Australian Open","Cordoba Open","Dallas Open","Open Sud de France – Montpellier","ABN AMRO Open","Argentina Open","Delray Beach Open","Open 13 Provence","Qatar ExxonMobil Open","Rio Open","Abierto Mexicano Telcel","Chile Open","Dubai","Davis Cup Qualifier","BNP Paribas Open","Miami Open","Fayez Sarofim & Co. U.S. Men's Clay Court Championship","Grand Prix Hassan II","Millennium Estoril Open","Rolex Monte-Carlo Masters","BMW Open by American Express","Banja Luka Open","Barcelona Open","Mutua Madrid Open","Internazionali BNL d'Italia","Gonet Geneva Open","Open Parc Auvergne-Rhone-Alpes Lyon","Roland Garros","BOSS OPEN","Libema Open","Cinch Championships","Terra Wortmann Open","Mallorca Championships","Rothesay International","Wimbledon","EFG Swiss Open Gstaad","Infosys Hall of Fame Open","Nordea Open","Hamburg European Open","Open Umag","Truist Atlanta Open presented by Fiserv","Abierto de Tenis Mifel","Citi Open","Generali Open","National Bank Open","Western & Southern Open","Winston-Salem Open","US Open","Chengdu Open","Zhuhai Championships","Laver Cup","Astana Open","China Open","Rolex Shanghai Masters","European Open","Japan Open","Erste Bank Open","Swiss Indoors Basel","Rolex Paris Masters","Sofia Open","Moselle Open","Stockholm Open","Next Gen ATP Finals","Nitto ATP Finals")
									)

df <- df %>% arrange(date, Name)


# The algo for have the new coordinates on y

i <- 1
while(i <= nrow(df)){
	
	if(df$nbr_tournaments[i] == 1){i <- i + 1}
	
	if(i > nrow(df)){break}
	
	if(df$nbr_tournaments[i] == 2 & df$max_week_month[i] > 5){
		df$ymin[i]   <- df$ymin[i] + (1/2)
		df$ymax[i+1] <- df$ymax[i+1] - (1/2)
		i            <- i + 2
	}

	if(df$nbr_tournaments[i] == 2 & df$max_week_month[i] <= 5){
		df$ymin[i]   <- df$ymin[i] + (1.2/2)
		df$ymax[i+1] <- df$ymax[i+1] - (1.2/2)
		i            <- i + 2
	}	
	
	
	if(i > nrow(df)){break}
	
	if(df$nbr_tournaments[i] == 3  & df$max_week_month[i] > 5){
		df$ymin[i]   <- df$ymax[i] - (1/3)
		df$ymin[i+1] <- df$ymin[i+1] + (1/3)
		df$ymax[i+1] <- df$ymax[i+1] - (1/3)
		df$ymax[i+2] <- df$ymin[i+2] + (1/3)
		i            <- i + 3
	}
	
	if(df$nbr_tournaments[i] == 3  & df$max_week_month[i] <= 5){
		df$ymin[i]   <- df$ymax[i] - (1.2/3)
		df$ymin[i+1] <- df$ymin[i+1] + (1.2/3)
		df$ymax[i+1] <- df$ymax[i+1] - (1.2/3)
		df$ymax[i+2] <- df$ymin[i+2] + (1.2/3)
		i            <- i + 3
	}
	
	if(i > nrow(df)){break}
	
	if(df$nbr_tournaments[i] == 4  & df$max_week_month[i] > 5){
		df$ymin[i]   <- df$ymax[i] - (1/4)
		df$ymin[i+1] <- df$ymin[i+1] + (1/2)
		df$ymax[i+1] <- df$ymax[i+1] - (1/4)
		df$ymin[i+2] <- df$ymin[i+2] + (1/4)
		df$ymax[i+2] <- df$ymin[i+2] + (1/2)
		df$ymax[i+3] <- df$ymin[i+3] + (1/4)
		i            <- i + 4
	}
	
	if(df$nbr_tournaments[i] == 4  & df$max_week_month[i] <= 5){
		df$ymin[i]   <- df$ymax[i] - (1.2/4)
		df$ymin[i+1] <- df$ymax[i] - 2*(1.2/4)
		df$ymax[i+1] <- df$ymax[i] - (1.2/4)
		df$ymin[i+2] <- df$ymax[i] - 3*(1.2/4)
		df$ymax[i+2] <- df$ymax[i] - 2*(1.2/4)
		df$ymax[i+3] <- df$ymax[i] - 3*(1.2/4)
		i            <- i + 4
	}
	
	if(i > nrow(df)){break}
	
print(i)

}





# Color by category of tournament

df = df %>% mutate(color_rect_category = case_when(Category == "ATP 250"                     ~ "#ff595e",
																									 Category == "ATP 500"                     ~ "#ff924c",
																									 Category == "Masters 1000"                ~ "#ffca3a",
																									 Category == "Grand Slam"                  ~ "#8ac926",
																									 Category %in% c("Davis Cup","Team Event") ~ "#1982c4",
																									 Category == "Season End"                  ~ "#6a4c93",
																									 T                                         ~ "#C4C4C4")
									 )



# Day number coordinates

df <- df %>% group_by(date) %>% mutate(x_day_number = (xmax + xmin)/2 - 0.05,
																			 y_day_number = min(ymin) + 0.85
																			 )

# These coordinates are ok for month with 6 weeks, but not for 5 weeks
# Also, need to consider the nchar of day number to hjust the label

for(i in 1:nrow(df)){
	
	if(df$max_week_month[i] <= 5){df$y_day_number[i] <- df$y_day_number[i] + 0.15}
	if(nchar(as.numeric(df$day_num[i])) >= 2){df$x_day_number[i] <- df$x_day_number[i] - 0.05}
	
}


# Month number in numeric

df$month_num <- as.numeric(df$month_num) 





# Plots ----


# Loop for all plots

for(i in 1:11){

df_plot              <- df %>% filter(month_num == i) 
max_horizontal_lines <- unique(df_plot$max_week_month)
vector_label_month   <- c('January','February','March','April','May','June','July','August','September','October','November')
	


if(i >= 8 & max_horizontal_lines > 5){
	
plot <- ggplot(df_plot) +
						scale_x_continuous(limits = c(0,7)) + 
						scale_y_continuous(limits = c(-0.5,7)) + 
						theme(panel.grid.major =  element_blank(), 
									panel.grid.minor = element_blank(),
									panel.background = element_rect(fill  = "#F8F8F8", 
																									color = "#F8F8F8"
																									),
									plot.background  = element_rect(fill  = "#F8F8F8", 
																		  					  color = "#F8F8F8"
																									),
									axis.text.x      = element_blank(),
									axis.ticks.x     = element_blank(),
									axis.text.y      = element_blank(),
									axis.ticks.y     = element_blank(), 
									axis.ticks       = element_blank(), 
									axis.title       = element_blank()
									) + 
						# Days labels
						geom_text(data = df_plot, 
												 aes(x     = x_day_number, 
												 		 y     = y_day_number, 
												 		 label = paste(as.numeric(day_num))
												 		),
												 hjust = 0,
												 size  = 2.25,
												 col   = "#777474"
											) +
						# Month label
						annotate('text',
										 x     = 3.4,
										 y     = 6.75,
										 label = vector_label_month[i],
										 size  = 4.75,
										 col   = "#898484"
										 ) + 
						# Rectangles
						annotate('rect',
										 xmin  = df_plot$xmin,
										 xmax  = df_plot$xmax,
										 ymin  = df_plot$ymin,
										 ymax  = df_plot$ymax , 
										 fill  = df_plot$color_rect_category,
										 color = "white",
										 alpha = 0.45
										 ) +
						# X axis text
						annotate('text', 
										 label = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),
										 x     = seq(0.5,6.5,1),
										 y     = rep(-0.4,7), 
										 color = "#6D6767",
										 size  = 2.5
										 ) +
						# Vertical main lines
						annotate('segment',
										 x     =  c(0,7),
										 xend  = c(0,7), 
										 y     = c(0,0), 
										 yend  = c(max_horizontal_lines,max_horizontal_lines),
										 color = "#4E4A4A"
										 ) + 
						# Horizontal main lines
						annotate('segment',
										 x     = rep(0,max_horizontal_lines + 1), 
										 xend  = rep(7,max_horizontal_lines + 1), 
										 y     = seq(0,max_horizontal_lines,1), 
										 yend  = seq(0,max_horizontal_lines,1),
										 color = "#4E4A4A"
										 )


assign(paste("Plot_Month_",i, sep = ""),plot)


}


if(i >= 8 & max_horizontal_lines <= 5){

plot <- ggplot(df_plot) +
						scale_x_continuous(limits = c(0,7)) + 
						scale_y_continuous(limits = c(-0.5,7)) + 
						theme(panel.grid.major =  element_blank(), 
									panel.grid.minor = element_blank(),
									panel.background = element_rect(fill = "#F8F8F8", 
																									color = "#F8F8F8"
																									),
									plot.background  = element_rect(fill = "#F8F8F8", 
																									color = "#F8F8F8"
																									),
									axis.text.x      = element_blank(),
									axis.ticks.x     = element_blank(),
									axis.text.y      = element_blank(),
									axis.ticks.y     = element_blank(), 
									axis.ticks       = element_blank(), 
									axis.title       = element_blank()
									) + 
						# Day labels
						geom_text(data = df_plot, 
												 aes(x     = x_day_number, 
												 		 y     = y_day_number, 
												 		 label = paste(as.numeric(day_num))
												 		),
												 hjust = 0,
												 size  = 2.25,
												 col   = "#777474"
											) + 
						# Month label
						annotate('text',
										 x     = 3.4,
										 y     = 6.75,
										 label = vector_label_month[i],
										 size  = 4.75,
										 col   = "#898484"
										 ) + 
						# Rectangles
						annotate('rect',
										 xmin  = df_plot$xmin,
										 xmax  = df_plot$xmax,
										 ymin  = df_plot$ymin,
										 ymax  = df_plot$ymax , 
										 fill  = df_plot$color_rect_category,
										 color = "white",
										 alpha = 0.45
										 ) +
						# X axis text
						annotate('text', 
										 label = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),
										 x     = seq(0.5,6.5,1),
										 y     = rep(-0.4,7), 
										 color = "#6D6767",
										 size  = 2.5
										 ) +
						# Horizontal lines	
						annotate('segment',
										 x     = rep(0,6), 
										 xend  = rep(7,6), 
										 y     = seq(0,max_horizontal_lines+1,1.2), 
										 yend  = seq(0,max_horizontal_lines+1,1.2),
										 color = "#4E4A4A"
										 )+
						# Vertical lines
						annotate('segment',
										 x     =  c(0,7),
										 xend  = c(0,7), 
										 y     = c(0,0), 
										 yend  = c(max_horizontal_lines+1,max_horizontal_lines+1),
										 color = "#4E4A4A"
										 )

assign(paste("Plot_Month_",i, sep = ""),plot)

}
	
	


if(i < 8 & max_horizontal_lines <= 5){

plot <- ggplot(df_plot) +
						scale_x_continuous(limits = c(0,7)) + 
						scale_y_continuous(limits = c(-0.5,7)) + 
						theme(panel.grid.major =  element_blank(), 
									panel.grid.minor = element_blank(),
									panel.background = element_rect(fill = "#F8F8F8", 
																									color = "#F8F8F8"
																									),
									plot.background  = element_rect(fill = "#F8F8F8", 
																									color = "#F8F8F8"
																									),
									axis.text.x      = element_blank(),
									axis.ticks.x     = element_blank(),
									axis.text.y      = element_blank(),
									axis.ticks.y     = element_blank(), 
									axis.ticks       = element_blank(), 
									axis.title       = element_blank()
									) +  
						# Day labels
						geom_text(data = df_plot, 
												 aes(x     = x_day_number, 
												 		 y     = y_day_number, 
												 		 label = paste(as.numeric(day_num))
												 		),
												 hjust = 0,
												 size  = 2.25,
												 col   = "#777474"
											) +
						# Month label
						annotate('text',
										 x     = 3.4,
										 y     = 6.75,
										 label = vector_label_month[i],
										 size  = 4.75,
										 col   = "#898484"
										 ) +  
						# Rectangles
						annotate('rect',
										 xmin  = df_plot$xmin,
										 xmax  = df_plot$xmax,
										 ymin  = df_plot$ymin,
										 ymax  = df_plot$ymax , 
										 fill  = df_plot$color_rect_category,
										 color = "white",
										 alpha = 0.45
										 ) +
						# Horizontal lines	
						annotate('segment',
										 x     = rep(0,6), 
										 xend  = rep(7,6), 
										 y     = seq(0,max_horizontal_lines+1,1.2), 
										 yend  = seq(0,max_horizontal_lines+1,1.2),
										 color = "#4E4A4A"
										 )+
						# Vertical lines
						annotate('segment',
										 x     =  c(0,7),
										 xend  = c(0,7), 
										 y     = c(0,0), 
										 yend  = c(max_horizontal_lines+1,max_horizontal_lines+1),
										 color = "#4E4A4A"
										 )

assign(paste("Plot_Month_",i, sep = ""),plot)

}



if(i < 8 & max_horizontal_lines > 5){
	
plot <- ggplot(df_plot) +
						scale_x_continuous(limits = c(0,7)) + 
						scale_y_continuous(limits = c(-0.5,7)) + 
						theme(panel.grid.major =  element_blank(), 
									panel.grid.minor = element_blank(),
									panel.background = element_rect(fill = "#F8F8F8", 
																									color = "#F8F8F8"
																									),
									plot.background  = element_rect(fill = "#F8F8F8", 
																									color = "#F8F8F8"
																									),
									axis.text.x      = element_blank(),
									axis.ticks.x     = element_blank(),
									axis.text.y      = element_blank(),
									axis.ticks.y     = element_blank(), 
									axis.ticks       = element_blank(), 
									axis.title       = element_blank()
									) + 
						# Day labels
						geom_text(data = df_plot, 
												 aes(x = x_day_number, 
												 		 y = y_day_number, 
												 		 label = paste(as.numeric(day_num))
												 		),
												 hjust = 0,
												 size  = 2.25,
												 col   = "#777474"
											) + 
						# Month label
						annotate('text',
										 x     = 3.4,
										 y     = 6.75,
										 label = vector_label_month[i],
										 size  = 4.75,
										 col   = "#898484"
										 ) + 
						# Rectangles
						annotate('rect',
										 xmin  = df_plot$xmin,
										 xmax  = df_plot$xmax,
										 ymin  = df_plot$ymin,
										 ymax  = df_plot$ymax , 
										 fill  = df_plot$color_rect_category,
										 color = "white",
										 alpha = 0.45
										 ) +
						# Vertical main lines
						annotate('segment',
										 x     =  c(0,7),
										 xend  = c(0,7), 
										 y     = c(0,0), 
										 yend  = c(max_horizontal_lines,max_horizontal_lines),
										 color = "#4E4A4A"
										 ) + 
						# Horizontal main lines
						annotate('segment',
										 x     = rep(0,max_horizontal_lines + 1), 
										 xend  = rep(7,max_horizontal_lines + 1), 
										 y     = seq(0,max_horizontal_lines,1), 
										 yend  = seq(0,max_horizontal_lines,1),
										 color = "#4E4A4A"
										 )


assign(paste("Plot_Month_",i, sep = ""),plot)


}




}
	
	
Plot_Month_12 <- ggplot(df_plot) +
										scale_x_continuous(limits = c(0,7)) + 
										scale_y_continuous(limits = c(-0.5,7)) + 
										theme(panel.grid.major =  element_blank(), 
													panel.grid.minor = element_blank(),
													panel.background = element_rect(fill = "#F8F8F8", 
																													color = "#F8F8F8"
																													),
													plot.background  = element_rect(fill = "#F8F8F8", 
																													color = "#F8F8F8"
																													),
													axis.text.x      = element_blank(),
													axis.ticks.x     = element_blank(),
													axis.text.y      = element_blank(),
													axis.ticks.y     = element_blank(), 
													axis.ticks       = element_blank(), 
													axis.title       = element_blank()
													)  
	

	
	
fill_values <- c("ATP 250"       = "#ff595e",
								 "ATP 500"       = "#ff924c",
								 "Masters 1000"  = "#ffca3a",
								 "Grand Slam"    = "#8ac926",
								 "Team Event"    = "#1982c4",
								 "Masters Final" = "#6a4c93",
								 "Nothing"       = "#C4C4C4"
								 )

df_title = tibble(x     = c(0.2,0.4,0.6,0.8,1,1.2,1.4), 
							    y     = rep(1,7), 
							    label = c("ATP 250","ATP 500","Masters 1000","Grand Slam","Team Event","Masters Final","Nothing")
							  ) 

plot_title = ggplot(df_title,aes(x = x, y = y)) +
									geom_point(aes(color = label), 
														 size  = 7, 
														 alpha = 0.45
														 ) +
									annotate('text',
													 x     = df_title$x ,
													 y     = df_title$y + 0.03,
													 label = df_title$label,
													 size  = 3,
													 col   = "black"
													 ) + 
									  theme_void() +
								    scale_x_continuous(expand = c(0, 0), 
								    									 limits = c(0, 1.6)
								    									 ) +
									  scale_y_continuous(limits = c(0.99, 1.1)) +
									  scale_color_manual(values = fill_values) +
									  guides(color = 'none')+
								    labs(title = 'ATP calendar 2023') +
								    theme(plot.margin      = margin(c(0,0,25,0)),
											  	panel.background = element_rect(fill  = "#F8F8F8", 
											  																	color = "#F8F8F8"
											  																	),
												  plot.background  = element_rect(fill  = "#F8F8F8", 
												  																color = "#F8F8F8"
												  																),
											  	plot.title       = element_text(size   = 20,
											  																	face   = "bold",
											  																	hjust  = 0.5,
											  																	margin = margin(c(10,0,-2,0))
											  																	)
											  	) 


caption_plot = ggplot(tibble(x = 1, y = 0), aes(x = x, y = y)) +
									  theme_void() +
								    scale_x_continuous(expand = c(0, 0), 
								    									 limits = c(0, 1.1)
								    									 ) +
									  scale_y_continuous(limits = c(0, 1.1)) +
								    labs(title = 'Visualisation by DENIAUX Maxime | Data : perfect-tennis.com | Inspiration : SHAPIRO Tania & ROYÉ Dominic') +
								    theme(plot.margin      = margin(c(10,0,10,0)),
											  	panel.background = element_rect(fill  = "#F8F8F8", 
											  																	color = "#F8F8F8"
											  																	),
												  plot.background  = element_rect(fill  = "#F8F8F8", 
												  																color = "#F8F8F8"
												  																),
											  	plot.title       = element_text(size   = 11,
											  																	hjust  = 0.5,
											  																	margin = margin(c(0,0,5,0)), 
											  																	color = "#8E8B8B"
											  																	)
											  	) 




# Save plot ----



months_plot <- plot_grid(Plot_Month_1,Plot_Month_2,Plot_Month_3,Plot_Month_4,
												 Plot_Month_5,Plot_Month_6,Plot_Month_7,Plot_Month_8,
												 Plot_Month_9,Plot_Month_10,Plot_Month_11,Plot_Month_12,
												 align = "h",
												 nrow  = 3)

Final_Plot <- plot_grid(plot_title,
											  months_plot,
											  caption_plot,
											  nrow        = 3,
											  rel_heights = c(0.17,0.77,0.06))


ggsave(plot     = Final_Plot,
			 filename = "final_plot.png", 
			 device   = agg_png(),
			 width    = 13,
			 height   = 10,
			 dpi      = 300)







