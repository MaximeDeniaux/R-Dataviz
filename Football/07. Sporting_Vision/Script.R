
# Note: I could and should have automated the 4 graphs with a loop and only had one "code" for each plot.


# Packages ----

library(ggplot2)
library(tidyverse)
library(rvest)
library(lubridate)
library(ggimage)
library(ggtext)
library(cowplot)


# Directory

setwd("C:/Users/maxim/Documents/Site_Web/Git/R-Dataviz/Football/7. Sporting_Vision")



# Data ----


# Evolution of players value, mannualy because impossible top scrap

# Vitinha, OM

Vitinha_Value <- data.frame(Date            = c('31/01/2023','31/03/2023','03/06/2023','21/10/2023'), 
														Category_Value  = c(17,22,18,15), 
														Player          = rep('Vitinha',4), 
														Team            = rep('Marseille',4)
														)


# Wahi, LENS

Wahi_Value <- data.frame(Date            = c('19/08/2023','20/10/2023'), 
												 Category_Value  = c(37,40), 
												 Player          = rep('Wahi',2), 
												 Team            = rep('Lens',2)
												 )


# Moffi, NICE

Moffi_Value <- data.frame(Date           = c('31/01/2023','19/03/2023','03/06/2023','21/10/2023'), 
												  Category_Value = c(18,20,20,25), 
												  Player         = rep('Moffi',4), 
												  Team           = rep('Nice',4)
												  )


# Openda, LENS

Openda_Value <- data.frame(Date            = c('06/08/2022','18/09/2022','18/03/2023','03/06/2023'), 
												   Category_Value  = c(7.5,12,20,35), 
												   Player          = rep('Openda',4), 
												   Team            = c(rep('Lens',4))
												   )

# Kalimuendo, RENNES

Kalimuendo_Value <- data.frame(Date            = c('12/08/2022','18/09/2022','05/11/2023','03/06/2023'), 
															 Category_Value  = c(18,22,25,22), 
															 Player          = rep('Kalimuendo',4), 
															 Team            = rep('Rennes',4)
															 )

# David, LILLE

David_Value <- data.frame(Date            = c('11/08/2020','18/10/2020','23/05/2021','18/12/2021','21/05/2022','18/03/2023'), 
												  Category_Value  = c(25,30,35,50,45,60), 
												  Player          = rep('David',6), 
												  Team            = rep('Lille',6)
												  )


# Bind each dataframe

Players_Values      <- bind_rows(Vitinha_Value,Wahi_Value,Moffi_Value,Openda_Value,Kalimuendo_Value,David_Value)
Players_Values$Date <- lubridate::dmy(Players_Values$Date)


rm(list = c("Vitinha_Value","Wahi_Value","Moffi_Value","Openda_Value","Kalimuendo_Value","David_Value"))



# Transfer value

Transfer_Values <- data.frame(Player   = c('Vitinha','Wahi','Moffi','Openda','Kalimuendo','David'),
															Transfer = c(32,30,25,15.4,20,27),
															Date     = c('31/01/2023','20/08/2023','31/01/2023','06/08/2022','11/08/2022','11/08/2020')
															)



# Scrap performance data (game starting or not, goals, assists)

url <- c('https://www.transfermarkt.fr/vitinha/leistungsdatendetails/spieler/586853/saison//verein/244/liga/0/wettbewerb//pos/0/trainer_id/0/plus/1',
				 'https://www.transfermarkt.fr/elye-wahi/leistungsdatendetails/spieler/659542/saison//verein/826/liga/0/wettbewerb//pos/0/trainer_id/0/plus/1',
				 'https://www.transfermarkt.fr/terem-moffi/leistungsdatendetails/spieler/538874/saison//verein/417/liga/0/wettbewerb//pos/0/trainer_id/0/plus/1',
				 'https://www.transfermarkt.fr/lois-openda/leistungsdatendetails/spieler/368887/saison//verein/826/liga/0/wettbewerb//pos/0/trainer_id/0/plus/1',
				 'https://www.transfermarkt.fr/arnaud-kalimuendo/leistungsdatendetails/spieler/585959/saison//verein/273/liga/0/wettbewerb//pos/0/trainer_id/0/plus/1',
				 'https://www.transfermarkt.fr/jonathan-david/leistungsdatendetails/spieler/533738/saison//verein/1082/liga/0/wettbewerb//pos/0/trainer_id/0/plus/1'
				 )

players <- c('Vitinha','Wahi','Moffi','Openda','Kalimuendo','David')


for(j in 1:length(url)){

print(paste("j : ",j*100/length(url), ' %',sep = ""))
nbr_tables <- length(read_html(url[j]) %>% html_nodes('table'))

for(i in 3:nbr_tables){

print(paste("i : ",i*100/nbr_tables,' %', sep = ""))
	
temp_table           <- (read_html(url[j]) %>% html_nodes('table') %>% .[i] %>% html_table(fill = T))[[1]] %>% as.data.frame() 
temp_table           <- temp_table[-(nrow(temp_table)),c(2,8,9,10,15,16,17)]
colnames(temp_table) <- c('Date','Starting','Goal','Assist','Entrance','Replaced','Time_Played')
temp_table           <- temp_table %>% mutate(Player = players[j])

if(i == 3 & j == 1){table = temp_table}
if((i != 3 & j == 1) | (j != 1) ){table = bind_rows(table,temp_table)}

}

}

rm(temp_table)



# Data Processing ----

# Custom date

table$Date <- gsub("jan.",  "01",table$Date)
table$Date <- gsub("févr.", "02",table$Date)
table$Date <- gsub("mars",  "03",table$Date)
table$Date <- gsub("avr.",  "04",table$Date)
table$Date <- gsub("mai",   "05",table$Date)
table$Date <- gsub("juin",  "06",table$Date)
table$Date <- gsub("août",  "08",table$Date)
table$Date <- gsub("sept.", "09",table$Date)
table$Date <- gsub("oct.",  "10",table$Date)
table$Date <- gsub("nov.",  "11",table$Date)
table$Date <- gsub("déc.",  "12",table$Date)

table$Date <- gsub(" ","/",table$Date)

table$Date <- lubridate::dmy(table$Date)


# Remove blank lines

table <- table[-which(is.na(table$Date)),]



# Custom goal column

table <- table %>% mutate(Goal = case_when(!Goal %in% c("1","2","3")  ~ "",
																					 Goal %in% c("1")           ~ "1",
																					 Goal %in% c("2")           ~ "2",
																					 Goal %in% c("3")           ~ "3"
																					)
													)


# Custom assist column

table <- table %>% mutate(Assist = case_when(!Assist %in% c("1","2")  ~ "",
																					   Assist %in% c("1")       ~ "1",
																					   Assist %in% c("2")       ~ "2"
																				  	)
													)


# Custom Time Played column

table <- table %>% mutate(Time_Played = case_when(Time_Played %in% c("Blessure à la main",
																																		 "Déchirure du ligament externe de la cheville",
																																		 "Blessure à l'ischio",
																																		 "Blessure à la cuisse",
																																		 "Déchirure musculaire",
																																		 "Information indisponible",
																																		 "Suspension pour deux avertissements",
																																		 "Supension pour accumulation de cartons jaunes",
																																		 "Pas dans l'effectif",
																																		 "Sur le banc")  ~ "",
																									T ~ Time_Played
																				  	      )
													)


# Custom Replaced column

table <- table %>% mutate(Replaced = case_when(Replaced %in% c("Blessure à la main",
																															 "Déchirure du ligament externe de la cheville",
																														 	 "Blessure à l'ischio",
																															 "Blessure à la cuisse",
																															 "Déchirure musculaire",
																															 "Information indisponible",
																															 "Suspension pour deux avertissements",
																															 "Supension pour accumulation de cartons jaunes",
																															 "Pas dans l'effectif",
																															 "Sur le banc")  ~ "",
																									T ~ Replaced
																				  	      )
													)


# Custom Entrance column

table <- table %>% mutate(Entrance = case_when(Entrance %in% c("Blessure à la main",
																															 "Déchirure du ligament externe de la cheville",
																														 	 "Blessure à l'ischio",
																															 "Blessure à la cuisse",
																															 "Déchirure musculaire",
																															 "Information indisponible",
																															 "Suspension pour deux avertissements",
																															 "Supension pour accumulation de cartons jaunes",
																															 "Pas dans l'effectif",
																															 "Sur le banc")  ~ "",
																									T ~ Entrance
																				  	      )
													)




# First custom Starting match column (because some values are confusing)

table <- table %>% mutate(Starting = case_when(Entrance != ""   ~ "",
																							 T ~ Starting)
													)


# Second custom Starting match column (because some values are confusing)

table <- table %>% mutate(Starting = case_when(Starting %in% c("","Sur le banc")   ~ "No",
																							 Starting %in% c("AC","2S","AiD","MO","AiG") ~ "Yes",
																							 Starting %in% c("Pas dans l'effectif",
																							 								 "Supension pour accumulation de cartons jaunes",
																							 								 "Suspension pour deux avertissements",
																							 								 "Information indisponible",
																							 								 "Déchirure musculaire",
																							 								 "Blessure à la cuisse",
																							 								 "Blessure à l'ischio",
																							 								 "Déchirure du ligament externe de la cheville",
																							 								 "Blessure à la main") ~ "Absent"
																							 )
													)






# Need one variable for Goal, Assist, Starting

table <- table %>% pivot_longer(cols      = c("Starting","Goal","Assist","Time_Played"),
															  names_to  = "Category",
																values_to = "Category_Value"
																)


table <- table %>% mutate(Category_num = case_when(Category == "Assist" ~ 5,
																									 Category == "Goal" ~ 10,
																									 Category == "Starting" ~ 15,
																									 Category == "Time_Played" ~ 20
																									 )
													)


for(i in 1:nrow(table)){
	
if(table$Category[i] == "Assist" & table$Category_Value[i] != ""){table$Category_Value[i] <-  paste("Assist_",table$Category_Value[i], sep = "")}
if(table$Category[i] == "Goal" & table$Category_Value[i] != ""){table$Category_Value[i]   <-  paste("Goal_",table$Category_Value[i], sep = "")}

}


table$Category_Value[table$Category == "Goal" & table$Category_Value == ""]   <-  NA_character_
table$Category_Value[table$Category == "Assist" & table$Category_Value == ""] <-  NA_character_


# Match number

table <- table %>% group_by(Player, Category) %>% arrange(Date) %>%  mutate(n_match = 1:n())


# After testing, for better visibility, I only keep 1 year of matches for each player

table <- table %>% filter( (Player == "David"      & Date <= as.Date("2021-08-08")) | 
													 (Player == "Kalimuendo" & Date <= as.Date("2023-08-13")) | 
													 (Player == "Openda"     & Date <= as.Date("2023-08-07")) | 
													 (Player == "Moffi"      & Date <= as.Date("2024-02-01")) | 
													 (Player == "Wahi"       & Date <= as.Date("2024-08-20")) | 
													 (Player == "Vitinha"    & Date <= as.Date("2023-11-30"))
												 )



# "Normalize" between 25 and 35 for each player

Players_Values <- Players_Values %>% filter( (Player == "David"      & Date <= as.Date("2021-08-08")) | 
																						 (Player == "Kalimuendo" & Date <= as.Date("2023-08-13")) | 
																						 (Player == "Openda"     & Date <= as.Date("2023-08-07")) | 
																						 (Player == "Moffi"      & Date <= as.Date("2024-02-01")) | 
																						 (Player == "Wahi"       & Date <= as.Date("2024-08-20")) | 
																						 (Player == "Vitinha"    & Date <= as.Date("2023-11-30"))
																					 )

Players_Values <- Players_Values %>% group_by(Player) %>% mutate(Category_num = 30 - 5 * ( (((max(Category_Value) + min(Category_Value)) / 2) - Category_Value) / (((max(Category_Value) + min(Category_Value)) / 2) - min(Category_Value) ) ) )

Players_Values <- left_join(Players_Values,table[table$Category == "Starting",c("Player","Date","n_match")],by = c("Player","Date"))

Players_Values$n_match[is.na(Players_Values$n_match)] <-  0



# Plot ----




# Take only the first and last date for visibility

labels                        <-  paste(sort(unique(c(Players_Values$Date[Players_Values$Player == "Vitinha"], table$Date[table$Player == "Vitinha"]))),sep = "")
labels[setdiff(1:40,c(1,40))] <-  ""

Vitinha_Plot <-  ggplot() + 
													# Starting 
													geom_image(data = table %>% filter(Player == "Vitinha", Category == "Starting"), 
																		aes(x     = n_match, 
																				y     = Category_num, 
																				image = paste(Category_Value,
																											".png", 
																											sep = ""
																											)
																				),
																		size = 0.04
																		) + 
	                        # Player img 
													geom_image(data = data.frame(x = 1.25, y = 46.5, img = "Vitinha.png"), 
																		aes(x, 
																				y, 
																				image = img
																				),
																		size = 0.22
																		) + 
	                        # Team img 
													geom_image(data = data.frame(x = 38, y = 46.5, img = "Marseille.png"), 
																		aes(x,
																				y, 
																				image = img),
																		size = 0.25
																		) + 
													# Time Played
													geom_point(data = table %>% filter(Player == "Vitinha", Category == "Time_Played"), 
													  				 aes(x    = n_match,
													  				 		 y    = Category_num, 
													  				 		 fill = extract_numeric(Category_Value)), 
																		 size  = 5, 
																		 color = "white", 
																		 shape = 22
																		 ) +
													scale_fill_gradient(low = "yellow", high = "red", na.value = "gray90") +
													guides(fill = 'none') + 
													# Goal 
													geom_image(data = table %>% filter(Player == "Vitinha", Category == "Goal"), 
																		aes(x     = n_match,
																				y     = Category_num, 
																				image = paste(Category_Value,
																											".png", 
																											sep = ""
																											)
																				),
																		size = 0.08
																		) + 
													# Assist
													geom_image(data = table %>% filter(Player == "Vitinha", Category == "Assist"), 
																		aes(x     = n_match, 
																				y     = Category_num, 
																				image = paste(Category_Value,
																											".png", 
																											sep = ""
																											)
																				),
																		size = 0.08
																		) + 
													# Value player
													geom_line(data = Players_Values %>% filter(Player == "Vitinha"), 
																		aes(x     = n_match, 
																				y     = Category_num, 
																				group = Player),
																		color     = "#D5904B", 
																		linewidth = 1.25
																		)+
													geom_point(data = Players_Values %>% filter(Player == "Vitinha"), 
																		aes(x     = n_match, 
																				y     = Category_num),
																		size  = 3.5, 
																		shape = 21, 
																		color = "white",
																	  fill  = "#9E5F21"
																		)+
													geom_text(data = Players_Values %>% filter(Player == "Vitinha"), 
																		aes(x     = n_match, 
																				y     = Category_num + 2, 
																				label = paste(Category_Value,
																											"M€")
																				),
																		nudge_x = 0.25, 
																		color   = "#FFA66D", 
																		size    = 6.5
																		) +
													# Arrow new club 
													annotate("segment", 
																	 x      = 0, 
																	 xend   = 0,
																	 y      = 0, 
																	 yend   = 27,
												           colour = "#E791D1", 
																	 size   = 0.8, 
																	 arrow  = arrow()
																	 ) +
													# Text new club
													annotate("text", 
																	 x      = 4.5, 
																	 y      = 1, 
																	 label  = "Joined Marseille",
												           colour = "#E791D1", 
																	 size   = 8
																	 ) +
													# Labs
													labs(x        = "", 
															 y        = "", 
															 title    = "Vitor Vitinha",
															 subtitle = paste("Purchase price : ",
															 								 Transfer_Values$Transfer[Transfer_Values$Player == "Vitinha"],
															 								 " M€", 
															 								 sep = ""
															 								 )
															 ) + 
													# Scale
													scale_y_continuous(breaks = c(5,10,15,20,30),
																						 labels = c("Assist","Goal","Starting","Time played","Value"),
																						 limits = c(0,50), 
																						 expand = c(0,0)) +
													scale_x_continuous(breaks = seq(0,39,1), 
																						 labels = labels, 
																						 limits = c(-0.25,42)
																						 ) +
													coord_cartesian(clip = 'off', ylim = c(0,38.5)) +
													# Theme
													theme(axis.text.x        = element_text(size   = 20, 
																																	margin = margin(t = 15),
																																	vjust  = 1, 
																																	hjust  = 0.5, 
																																	color  = "#D9DDE3"
																																	),
																axis.text.y        = element_text(face   = "bold", 
																																	color  = "#D9DDE3", 
																																	size   = 20, 
																																	margin = margin(r = 10),
																																	hjust  = 0), 
																axis.ticks         = element_blank(),
																panel.grid.major.y = element_blank(),
																panel.grid.minor.y = element_blank(),
																panel.grid.minor.x = element_blank(),
																panel.grid.major.x = element_line(linetype  = 'dashed',
																																	linewidth = 0.2,
																																	color     = "#606060"
																																	),
																panel.background   = element_rect(fill  = "#15171A", 
																																	color = "#15171A"
																																	),
																plot.background    = element_rect(fill  = "#15171A", 
																																	color = "#15171A"
																																	),
																plot.title         = element_text(color  = "white",
																																	size   = 35,
																																	margin = margin(t = 30, b = 20), 
																																	hjust  = 0.45),
																plot.subtitle      = element_text(color  = "#E791D1",
																																	size   = 25,
																																	margin = margin(b = 40), 
																																	hjust  = 0.45
																																	),
																plot.margin        = margin(b = 65)
																)




labels                        <-  paste(sort(unique(c(Players_Values$Date[Players_Values$Player == "Moffi"], table$Date[table$Player == "Moffi"]))),sep = "")
labels[setdiff(1:37,c(1,37))] <-  ""
labels                        <- labels[1:37]

Moffi_Plot <-  ggplot() + 
													# Starting 
													geom_image(data = table %>% filter(Player == "Moffi", Category == "Starting"), 
																		aes(x     = n_match, 
																				y     = Category_num, 
																				image = paste(Category_Value,
																											".png", 
																											sep = ""
																											)
																				),
																		size = 0.04
																		) + 
	                        # Player img 
													geom_image(data = data.frame(x = 1, y = 46.5, img = "Moffi.png"), 
																		aes(x, 
																				y,
																				image = img
																				),
																		size = 0.22
																		) + 
	                        # Team img 
													geom_image(data = data.frame(x = 35, y = 46.5, img = "Nice.png"), 
																		aes(x,
																				y, 
																				image = img
																				),
																		size = 0.22
																		) + 
													# Time Played 
													geom_point(data = table %>% filter(Player == "Moffi", Category == "Time_Played"), 
													  				 aes(x    = n_match, 
													  				 		 y    = Category_num, 
													  				 		 fill = extract_numeric(Category_Value)
													  				 		), 
																		 size  = 5,
																		 color = "white", 
																		 shape = 22
																		 ) +
													scale_fill_gradient(low = "yellow", high = "red", na.value = "gray90") +
													guides(fill = 'none') + 
													# Goal 
													geom_image(data = table %>% filter(Player == "Moffi", Category == "Goal"), 
																		aes(x     = n_match,
																				y     = Category_num, 
																				image = paste(Category_Value, 
																											".png", 
																											sep = ""
																											)
																				),
																		size = 0.08
																		) + 
													# Assist
													geom_image(data = table %>% filter(Player == "Moffi", Category == "Assist"), 
																		aes(x     = n_match, 
																				y     = Category_num, 
																				image = paste(Category_Value,
																											".png", 
																											sep = ""
																											)
																				),
																		size = 0.08
																		) + 
													# Value player
													geom_line(data = Players_Values %>% filter(Player == "Moffi"), 
																		aes(x     = n_match, 
																				y     = Category_num,
																				group = Player
																				),
																		color     = "#D5904B", 
																		linewidth = 1.25
																		)+
													geom_point(data = Players_Values %>% filter(Player == "Moffi"), 
																		aes(x = n_match, 
																				y = Category_num
																				),
																		size  = 3.5,
																		shape = 21, 
																		color = "white",
																		fill  = "#9E5F21"
																		)+
													geom_text(data = Players_Values %>% filter(Player == "Moffi"), 
																		aes(x     = n_match, 
																				y     = Category_num + 2, 
																				label = paste(Category_Value,
																											"M€"
																											)
																				),
																		nudge_x = 0.25, 
																		color   = "#FFA66D", 
																		size    = 6.5
																		) +
													# Arrow new club 
													annotate("segment", 
																	 x      = 0, 
																	 xend   = 0, 
																	 y      = 0,
																	 yend   = 24,
												           colour = "#E791D1", 
																	 size   = 0.8, 
																	 arrow  = arrow()
																	 ) +
													# Text new club
													annotate("text", 
																	 x      = 3.25, 
																	 y      = 1, 
																	 label  = "Joined Nice",
												           colour = "#E791D1",
																	 size   = 8) +
													# Labs
													labs(x        = "", 
															 y        = "", 
															 title    = "Terem Moffi", 
															 subtitle = paste("Purchase price : ",
															 								 Transfer_Values$Transfer[Transfer_Values$Player == "Moffi"],
															 								 " M€", 
															 								 sep = ""
															 								 )
															 ) + 
													# Scale
													scale_y_continuous(breaks = c(5,10,15,20,30),
																						 labels = c("Assist","Goal","Starting","Time played","Value"),
																						 limits = c(0,50), 
																						 expand = c(0,0)) +
													scale_x_continuous(breaks = seq(0,36,1), 
																						 labels = labels, 
																						 limits = c(-0.25,39)
																						 ) +
													coord_cartesian(clip = 'off',ylim = c(0,38.5))+
													# Theme
													theme(axis.text.x        = element_text(size   = 20, 
																																	margin = margin(t = 15),
																																	vjust  = 1, 
																																	hjust  = 0.5, 
																																	color  = "#D9DDE3"
																																	),
																axis.text.y        = element_text(face   = "bold", 
																																	color  = "#15171A",
																																	size   = 20, 
																																	margin = margin(r = 10),
																																	hjust  = 0
																																	), 
																axis.ticks         = element_blank(),
																panel.grid.major.y = element_blank(),
																panel.grid.minor.y = element_blank(),
																panel.grid.minor.x = element_blank(),
																panel.grid.major.x = element_line(linetype  = 'dashed', 
																																	linewidth = 0.2,
																																	color     = "#606060"
																																	),
																panel.background   = element_rect(fill  = "#15171A", 
																																	color = "#15171A"
																																	),
																plot.background    = element_rect(fill  = "#15171A", 
																																	color = "#15171A"
																																	),
																plot.title         = element_text(color   = "white",
																																	size    = 35,
																																	margin  = margin(t = 30, b = 20), 
																																	hjust   = 0.45
																																	),
																plot.subtitle      = element_text(color  = "#E791D1",
																																	size   = 25,
																																	margin = margin(b = 40), 
																																	hjust  = 0.45
																																	),
																plot.margin        = margin(b = 65)
																)


labels                        <-  paste(sort(unique(c(Players_Values$Date[Players_Values$Player == "Openda"],table$Date[table$Player == "Openda"]))),sep = "")
labels[setdiff(1:43,c(1,43))] <-  ""

Openda_Plot <-   ggplot() + 
													# Starting 
													geom_image(data = table %>% filter(Player == "Openda", Category == "Starting"), 
																		aes(x     = n_match,
																				y     = Category_num, 
																				image = paste(Category_Value,
																											".png",
																											sep = ""
																											)
																				),
																		size = 0.04
																		) + 
	                        # Player img 
													geom_image(data = data.frame(x = 1.5, y = 46.5, img = "Openda.png"), 
																		aes(x,
																				y, 
																				image = img
																				),
																		size = 0.22
																		) + 
	                        # Team img 
													geom_image(data = data.frame(x = 41, y = 46.5, img = "Lens.png"), 
																		aes(x, 
																				y, 
																				image = img
																				),
																		size = 0.22
																		) + 
													# Time Played 
													geom_point(data = table %>% filter(Player == "Openda", Category == "Time_Played"), 
													  				 aes(x    = n_match, 
													  				 	 	 y    = Category_num, 
													  				 		 fill = extract_numeric(Category_Value)
													  				 		 ), 
																		 size  = 5, 
																		 color = "white", 
																		 shape = 22
																		 ) +
													scale_fill_gradient(low = "yellow", high = "red", na.value = "gray90") +
													guides(fill = 'none')+ 
													# Goal 
													geom_image(data = table %>% filter(Player == "Openda", Category == "Goal"), 
																		aes(x     = n_match, 
																				y     = Category_num, 
																				image = paste(Category_Value,
																											".png",
																											sep = ""
																											)
																				),
																		size = 0.08
																		) + 
													# Assist
													geom_image(data = table %>% filter(Player == "Openda", Category == "Assist"), 
																		aes(x     = n_match, 
																				y     = Category_num, 
																				image = paste(Category_Value,
																											".png", 
																											sep = ""
																											)
																				),
																		size = 0.08
																		) + 
													# Value player
													geom_line(data = Players_Values %>% filter(Player == "Openda"), 
																		aes(x     = n_match, 
																				y     = Category_num, 
																				group = Player),
																		color     = "#D5904B", 
																		linewidth = 1.25
																		)+
													geom_point(data = Players_Values %>% filter(Player == "Openda"), 
																		aes(x = n_match, 
																				y = Category_num
																				),
																		size  = 3.5, 
																		shape = 21, 
																		color = "white",
																		fill  = "#9E5F21"
																		)+
													geom_text(data = Players_Values %>% filter(Player == "Openda"), 
																		aes(x     = n_match, 
																				y     = Category_num + 2, 
																				label = paste(Category_Value,
																											"M€"
																											)
																				),
																		nudge_x = 0.25, 
																		color   = "#FFA66D", 
																		size    = 6.5
																		) +
													# Arrow new club 
													annotate("segment", 
																	 x      = 0,
																	 xend   = 0, 
																	 y      = 0,
																	 yend   = 24,
												           colour = "#E791D1",
																	 size   = 0.8, 
																	 arrow  = arrow()
																	 ) +
													# Text new club
													annotate("text", 
																	 x      = 3.75, 
																	 y      = 1,
																	 label  = "Joined Lens",
												           colour = "#E791D1",
																	 size   = 8
																	 ) +
													# Labs
													labs(x        = "", 
															 y        = "", 
															 title    = "Lois Openda", 
															 subtitle = paste("Purchase price : ",
															 								 Transfer_Values$Transfer[Transfer_Values$Player == "Openda"],
															 								 " M€", 
															 								 sep = ""
															 								 )
															 ) +  
													# Scale
													scale_y_continuous(breaks = c(5,10,15,20,30),
																						 labels = c("Assist","Goal","Starting","Time played","Value"),
																						 limits = c(0,50), 
																						 expand = c(0,0)) +
													scale_x_continuous(breaks = seq(0,42,1), 
																						 labels = labels, 
																						 limits = c(-0.25,45)
																						 ) +
													coord_cartesian(clip = 'off',ylim = c(0,38.5))+
													# Theme
													theme(axis.text.x        = element_text(size   = 20,
																																	margin = margin(t = 15),
																																	vjust  = 1,
																																	hjust  = 0.5, 
																																	color  = "#D9DDE3"
																																	),
																axis.text.y        = element_text(face   = "bold",
																																	color  = "#D9DDE3", 
																																	size   = 20, 
																																	margin = margin(r = 10),
																																	hjust  = 0
																																	), 
																axis.ticks         = element_blank(),
																panel.grid.major.y =  element_blank(),
																panel.grid.minor.y =  element_blank(),
																panel.grid.minor.x =  element_blank(),
																panel.grid.major.x = element_line(linetype  = 'dashed',
																																	linewidth = 0.2,
																																	color     = "#606060"
																																	),
																panel.background   = element_rect(fill  = "#15171A", 
																																	color = "#15171A"
																																	),
																plot.background    = element_rect(fill  = "#15171A", 
																																	color = "#15171A"
																																	),
																plot.title         = element_text(color  = "white",
																																	size   = 35,
																																	margin = margin(t = 30, b = 20), 
																																	hjust  = 0.45),
																plot.subtitle      = element_text(color  = "#E791D1",
																																	size   = 25,
																																	margin = margin(b = 40), 
																																	hjust  = 0.45),
																plot.margin        = margin(b = 65)
																)





labels                        <-  paste(sort(unique(c(Players_Values$Date[Players_Values$Player == "David"],table$Date[table$Player == "David"]))),sep = "")
labels[setdiff(1:52,c(1,52))] <-  ""

David_Plot <-  ggplot() + 
													# Starting 
													geom_image(data = table %>% filter(Player == "David", Category == "Starting"), 
																		aes(x     = n_match, 
																				y     = Category_num, 
																				image = paste(Category_Value,
																											".png", 
																											sep = ""
																											)
																				),
																		size = 0.04
																		) + 
	                        # Player img 
													geom_image(data = data.frame(x = 1.5, y = 46.5, img = "David.png"), 
																		aes(x, 
																				y, 
																				image = img
																				),
																		size = 0.22
																		) + 
	                        # Team img 
													geom_image(data = data.frame(x = 50, y = 46.5, img = "Lille.png"), 
																		aes(x, 
																				y, 
																				image = img
																				),
																		size = 0.22
																		) + 
													# Time Played 
													geom_point(data = table %>% filter(Player == "David", Category == "Time_Played"), 
													  				 aes(x    = n_match, 
													  				 		 y    = Category_num, 
													  				 		 fill = extract_numeric(Category_Value)
													  				 		), 
																		 size  = 5, 
																		 color = "white", 
																		 shape = 22
																		 ) +
													scale_fill_gradient(low = "yellow", high = "red", na.value = "gray90") +
													guides(fill = 'none')+ 
													# Goal 
													geom_image(data = table %>% filter(Player == "David", Category == "Goal"), 
																		aes(x     = n_match, 
																				y     = Category_num, 
																				image = paste(Category_Value,
																											".png", 
																											sep = ""
																											)
																				),
																		size = 0.08
																		) + 
													# Assist
													geom_image(data = table %>% filter(Player == "David", Category == "Assist"), 
																		aes(x     = n_match,
																				y     = Category_num, 
																				image = paste(Category_Value,
																											".png", 
																											sep = ""
																											)
																				),
																		size = 0.08
																		) + 
													# Value player
													geom_line(data = Players_Values %>% filter(Player == "David"), 
																		aes(x     = n_match,
																				y     = Category_num, 
																				group = Player
																				),
																		color     = "#D5904B", 
																		linewidth = 1.25
																		)+
													geom_point(data = Players_Values %>% filter(Player == "David"), 
																		aes(x = n_match, 
																				y = Category_num
																				),
																		size  = 3.5, 
																		shape = 21, 
																		color = "white",
																		fill  = "#9E5F21"
																		)+
													geom_text(data = Players_Values %>% filter(Player == "David"), 
																		aes(x     = n_match,
																				y     = Category_num + 2,
																				label = paste(Category_Value,
																											"M€"
																											)
																				),
																		nudge_x = 0.25, 
																		color   = "#FFA66D", 
																		size    = 6.5
																		) +
													# Arrow new club 
													annotate("segment", 
																	 x      = 0, 
																	 xend   = 0, 
																	 y      = 0, 
																	 yend   = 24,
												           colour = "#E791D1", 
																	 size   = 0.8, 
																	 arrow  = arrow()
																	 ) +
													# Text new club
													annotate("text", 
																	 x      = 4.25, 
																	 y      = 0.2, 
																	 label  = "Joined Lille",
												           colour = "#E791D1", 
																	 size   = 8
																	 ) +
													# Labs
													labs(x        = "", 
															 y        = "", 
															 title    = "Jonathan David", 
															 subtitle = paste("Purchase price : ",
															 								 Transfer_Values$Transfer[Transfer_Values$Player == "David"],
															 								 " M€",
															 								 sep = ""
															 								 )
															 ) + 
													# Scale
													scale_y_continuous(breaks = c(5,10,15,20,30),
																						 labels = c("Assist","Goal","Starting","Time played","Value"),
																						 limits = c(0,50), 
																						 expand = c(0,0)) +
													scale_x_continuous(breaks = seq(0,51,1), 
																						 labels = labels, 
																						 limits = c(-0.25,54)
																						 ) +
													coord_cartesian(clip = "off",ylim = c(0, 38.5)) +
													# Theme
													theme(axis.text.x        = element_text(size   = 20, 
																																	margin = margin(t = 15), 
																																	vjust  = 1, 
																																	hjust  = 0.5, 
																																	color  = "#D9DDE3"
																																	),
																axis.text.y        = element_text(face   = "bold", 
																																	color  = "#15171A", 
																																	size   = 20, 
																																	margin = margin(r = 10),
																																	hjust  = 0
																																	), 
																axis.ticks         = element_blank(),
																panel.grid.major.y = element_blank(),
																panel.grid.minor.y = element_blank(),
																panel.grid.minor.x = element_blank(),
																panel.grid.major.x = element_line(linetype   = 'dashed', 
																																	linewidth  = 0.2,
																																	color      = "#606060"
																																	),
																panel.background   = element_rect(fill  = "#15171A", 
																																	color = "#15171A"
																																	),
																plot.background    = element_rect(fill  = "#15171A", 
																																	color = "#15171A"
																																	),
																plot.title         = element_text(color  = "white",
																																	size   = 35,
																																	margin = margin(t = 30, b = 20),
																																	hjust  = 0.45
																																	),
																plot.subtitle      = element_text(color  = "#E791D1",
																																	size   = 25,
																																	margin = margin(b = 40), 
																																	hjust  = 0.45
																																	),
																plot.margin        = margin(b = 65)
																)



# Legend

Legend <- ggplot() + 
									geom_point(data = table %>% filter(Player == "David", Category == "Time_Played", Date <= as.Date("2021-08-11")), 
													  				 aes(x    = n_match, 
													  				 		 y    = Category_num, 
													  				 		 fill = extract_numeric(Category_Value)
													  				 		), 
														 size  = 5,
														 color = "white", 
														 shape = 21
														 ) +
									scale_fill_gradient(low = "yellow", high = "red", na.value = "gray90", labels = NULL) +
									geom_image(data = data.frame(x   = c(0.6,0.65,0.7),
																							 y   = c(0.45,0.45,0.45),
																							 img = c("Absent.png","No.png","Yes.png")
																							 ),
														 aes(x,
														 		 y,
														 		 image = img
														 		), 
														 size = 0.095
														 )+
									# Titles
									geom_text(data = data.frame(x     = c(0.389,0.65),
																							y     = c(0.7,0.7),
																							label = c("Time Played","Start the game ?")
																							),
														aes(x,
																y,
																label = label
																), 
														size  = 10, 
														color = "white"
														)+
									# Labels
									geom_text(data = data.frame(x     = c(0.595,0.65,0.70,0.335,0.39,0.44),
																							y     = rep(0.33,6),
																							label = c("Absent","No","Yes","0","45","90")
																							),
														aes(x,
																y,
																label = label
																), 
														size  = 9, 
														color = "gray90"
														)+
	                labs(fill     = "", 
	                		 title    = "A sporting vision absent in Marseille ?", 
	                		 subtitle = "The sporting vision of Ligue 1 clubs aiming for a place in the European Cup for the central striker position seems uniform : betting on <br>      a young player with good potential with the possibility of resale at a high price if his performances are good. However, the continuity <br> and confidence given to this young player differs depending on the club.")+
									scale_y_continuous(limits = c(0,1)) +
									scale_x_continuous(limits = c(0,1)) +
									theme_void() +
	                theme(legend.direction = "horizontal",
	                			legend.position  = c(0.4,0.5),
	                			legend.title     = element_text(color = "white", 
	                																			hjust = 0.5, 
	                																			size  = 15,
	                																			vjust = 1,
	                																			face  = "bold"
	                																			),
	                			panel.background = element_rect(fill  = "#15171A", 
	                																			color = "#15171A"
	                																			),
												plot.background  = element_rect(fill  = "#15171A", 
																												color = "#15171A"
																												), 
												plot.margin      = margin(b = 50),
												plot.title       = element_markdown(size   = 45,
																														face   = "bold",
																														hjust  = 0.5,
																														margin = margin(t = 35, b = 20), 
																														color  = "white"
																														),
												plot.subtitle    = element_markdown(size       = 30,
																														hjust      = 0.5, 
																														halign     = 0.5,
																														lineheight = 1.5, 
																														margin     = margin(t = 20, b = 10),
																														color      = "gray85"
																														)
												) +
	                guides(fill = guide_colourbar(title.position = 'top',
	                														  label.theme    = element_text(color = "white", 
	                														  															size  = 30
	                														  															),
	                															barwidth       = 15 )
	                			 )


caption <-  ggplot(data.frame(x = 1, y = 1)) + 
									theme_void() +
									labs(title = "Visualisation by DENIAUX Maxime | Data : Transfermarkt | Icons & Images : Flaticon, Fotmob, Transfermarkt") +
									theme(panel.background = element_rect(fill  = "#15171A", 
																												color = "#15171A"
																												),
												plot.background  = element_rect(fill  = "#15171A",
																												color = "#15171A"
																												),
												plot.title       = element_markdown(hjust  = 0.5, 
																														color  = "gray85", 
																														size   = 20, 
																														margin = margin(0,0,0,0)
																														),
												plot.margin      = margin(t = 30, b = 30)
												)


# Save Plot ----

mid <- plot_grid(Vitinha_Plot,
								 Moffi_Plot,
								 Openda_Plot,
								 David_Plot,
								 nrow = 2
								 )

ggsave(plot =  plot_grid(Legend,
												 mid,
												 caption,
												 ncol        = 1,
												 rel_heights = c(0.3,0.68,0.02)
												 ),
			 filename = "Final_Plot.png",
			 width    = 30,
			 height   = 25,
			 device   = 'png'
			 )















