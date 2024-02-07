

# Packages ----

library(ggplot2)
library(dplyr)
library(cowplot)


# Directory

setwd("C:/Users/maxim/Documents/Site_Web/Git/R-Dataviz/Tennis/7. Seed_3rd_Round_In_GC")


# Data ----


#____Men results ----

for(i in 2000:2023){
	
download.file(url      = paste("https://github.com/JeffSackmann/tennis_atp/raw/master/atp_matches_",i,".csv",
															 sep = ""),
							destfile = paste("ATP_Matches_",i,".csv", sep = ""))
	
	
assign(paste("ATP_", i, sep = ""), 
			 read.csv(paste("ATP_Matches_",i,".csv", sep = ""))
			 )
	
}




#____Women results ----

for(i in 2000:2023){
	
download.file(url      = paste("https://github.com/JeffSackmann/tennis_wta/raw/master/wta_matches_",i,".csv",
															 sep = ""),
							destfile = paste("WTA_Matches_",i,".csv", sep = ""))
	
	
assign(paste("WTA_", i, sep = ""), 
			 read.csv(paste("WTA_Matches_",i,".csv", sep = ""))
			 )
	
}



# Data processing ----



#____Men data ----



# Variables selection

for(i in 2000:2023){
	
assign(paste("ATP_", i, sep = ""),get(paste("ATP_", i, sep = "")) %>% 
			 select(tourney_name, tourney_level, surface, tourney_date, winner_seed,loser_seed, round) %>% 
			 filter(tourney_level == "G")
			 )
	
}



# Bind rows to work on a single dataframe (I could have done it before)

ATP_df <- bind_rows(mget(paste("ATP_",2000:2023, sep="")))
rm(list = paste("ATP_",2000:2023,sep = ""))



# Date

ATP_df$Season <- substr(ATP_df$tourney_date,1,4) 
ATP_df$Season <- as.numeric(ATP_df$Season)
ATP_df        <- ATP_df %>% select(-tourney_date)


# Tourney_name values curation

ATP_df$tourney_name[ATP_df$tourney_name == "Us Open"] <- 'US Open'


# Levels of 2 variables

ATP_df$tourney_name <- factor(x      = ATP_df$tourney_name,
															levels = c('Australian Open',
																				 'Roland Garros',
																				 'Wimbledon',
																				 'US Open'
																				 )
															)

ATP_df$Season <- factor(x = ATP_df$Season, levels = as.character(2000:2023))



# Keep only the 3rd round (the last 32 players remaining in the draw)

ATP_df <- ATP_df %>% filter(round == "R32")


# New variable to know if there is a top seed

ATP_df <- ATP_df %>% mutate(Top_Seed_Nbr = ifelse(is.na(ATP_df$winner_seed) & is.na(ATP_df$loser_seed),
																									0,
																									ifelse((is.na(ATP_df$winner_seed) & !is.na(ATP_df$loser_seed)) | (!is.na(ATP_df$winner_seed) & is.na(ATP_df$loser_seed)),
																												 1,
																												 2)
																									),
														Type = "M"
														)







#____Women data ----



# Variables selection

for(i in 2000:2023){
	
assign(paste("WTA_", i, sep = ""),get(paste("WTA_", i, sep = "")) %>% 
			 select(tourney_name, tourney_level, surface, tourney_date, winner_seed,loser_seed, round) %>% 
			 filter(tourney_level == "G")
			 )
	
}


# Some different class variable between tables

WTA_2022$winner_seed <- as.integer(WTA_2022$winner_seed)
WTA_2022$loser_seed  <- as.integer(WTA_2022$loser_seed)

WTA_2020$winner_seed <- as.integer(WTA_2020$winner_seed)
WTA_2020$loser_seed  <- as.integer(WTA_2020$loser_seed)

WTA_2016$winner_seed <- as.integer(WTA_2016$winner_seed)



# Bind rows to work on a single dataframe (I could have done it before)

WTA_df <- bind_rows(mget(paste("WTA_",2000:2023, sep="")))
rm(list = paste("WTA_",2000:2023,sep = ""))



# Date

WTA_df$Season <- substr(WTA_df$tourney_date,1,4) 
WTA_df$Season <- as.numeric(WTA_df$Season)
WTA_df        <- WTA_df %>% select(-tourney_date)


# Tourney_name values curation

WTA_df$tourney_name[WTA_df$tourney_name == "Us Open"] <- 'US Open'


# Levels of 2 variables

WTA_df$tourney_name <- factor(x      = WTA_df$tourney_name,
															levels = c('Australian Open',
																				 'Roland Garros',
																				 'Wimbledon',
																				 'US Open'
																				 )
															)


WTA_df$Season <- factor(x = WTA_df$Season,levels = as.character(2000:2023))




# Keep only the 3rd round (the last 32 players remaining in the draw)

WTA_df <- WTA_df %>% filter(round == "R32")


# New variable to know if there is a top seed

WTA_df <- WTA_df %>% mutate(Top_Seed_Nbr = ifelse(is.na(WTA_df$winner_seed) & is.na(WTA_df$loser_seed),
																									0,
																									ifelse((is.na(WTA_df$winner_seed) & !is.na(WTA_df$loser_seed)) | (!is.na(WTA_df$winner_seed) & is.na(WTA_df$loser_seed)),
																												 1,
																												 2)
																									),
														Type = "W"
														)






#____Combined ----


Summary <- bind_rows(ATP_df,WTA_df) %>% arrange(Season,tourney_name)

Summary_Top_Plot <- Summary %>% filter(Season %in% as.character(2000:2020)) %>% group_by(Season, tourney_name, Type) %>% summarise(Ratio = round(sum(Top_Seed_Nbr) * 100 / 32, digits = 2),
																																																																	 Nbr = sum(Top_Seed_Nbr)
																																																																	 ) 

Summary_Top_Plot_Mean <- Summary_Top_Plot %>% group_by(tourney_name, Type) %>% summarise(Mean_pct = round(mean(Ratio), digits = 2),
																																												 Mean_Nbr = round(mean(Nbr),digits = 2),
																																												 sd_pct   = sd(Ratio),
																																												 sd       = sd(Nbr)
																																												 ) 


Summary_First_Decade_Plot <-  Summary  %>% filter(Season %in% as.character(2000:2010)) %>% group_by(Season, tourney_name, Type) %>% summarise(Ratio = round(sum(Top_Seed_Nbr) * 100 / 32, digits = 2),
																																																																							Nbr = sum(Top_Seed_Nbr)
																																																																							) 


Summary_First_Decade_Plot_Mean <- Summary_First_Decade_Plot %>% group_by(tourney_name, Type) %>% summarise(Mean_pct = round(mean(Ratio), digits = 2),
																																																					 Mean_Nbr = round(mean(Nbr),digits = 2),
																																																					 sd_pct   = sd(Ratio),
																																																					 sd       = sd(Nbr)
																																																					 ) 


Summary_Second_Decade_Plot <-  Summary  %>% filter(Season %in% as.character(2011:2020)) %>% group_by(Season, tourney_name, Type) %>% summarise(Ratio = round(sum(Top_Seed_Nbr) * 100 / 32, digits = 2),
																																																																							 Nbr = sum(Top_Seed_Nbr)
																																																																							 ) 


Summary_Second_Decade_Plot_Mean <- Summary_Second_Decade_Plot %>% group_by(tourney_name, Type) %>% summarise(Mean_pct = round(mean(Ratio), digits = 2),
																																																						 Mean_Nbr = round(mean(Nbr),digits = 2),
																																																						 sd_pct   = sd(Ratio),
																																																						 sd       = sd(Nbr)
																																																						 ) 


rm(ATP_df)
rm(WTA_df)


# Plot ----

Top_Plot <- ggplot() + 
	                # Horizontal line sd
	                geom_segment(data      = Summary_Top_Plot_Mean %>% filter(Type == "M"), 
		                					 mapping   = aes(x    = 1, 
		                						 							 y    = Mean_pct - sd_pct, 
		                						 							 xend = 1, 
		                						 					     yend = Mean_pct + sd_pct
		                					 								), 
	                						 color     = '#AAA699',
	                						 linewidth = 1) +
	                geom_segment(data = Summary_Top_Plot_Mean %>% filter(Type == "W"), 
	                						 mapping   = aes(x    = 0.5, 
	                						 							   y    = Mean_pct - sd_pct, 
	                						 								 xend = 0.5,
	                						 								 yend = Mean_pct + sd_pct), 
	                						 color     = '#494845',
	                						 linewidth = 1) +
	                # Cross mean
	                geom_segment(data      = Summary_Top_Plot_Mean %>% filter(Type == "M"), 
	                						 mapping   = aes(x    = 0.80, 
	                						 							   y    = Mean_pct - 1, 
	                						 							   xend = 1.20, 
	                						 							   yend = Mean_pct + 1
	                						 								), 
	                						 color     = '#AAA699',
	                						 linewidth = 1) +
	                geom_segment(data      = Summary_Top_Plot_Mean %>% filter(Type == "M"), 
	                						 mapping   = aes(x    = 0.80, 
	                						 							   y    = Mean_pct + 1, 
	                						 							   xend = 1.20, 
	                						 							   yend = Mean_pct - 1
	                						 							   ), 
	                						 color     = '#AAA699',
	                						 linewidth = 1) +
	                geom_segment(data      = Summary_Top_Plot_Mean %>% filter(Type == "W"), 
	                						 mapping   = aes(x    = 0.30, 
	                						 							   y    = Mean_pct - 1, 
	                						 							   xend = 0.70, 
	                						 							   yend = Mean_pct + 1
	                						 								), 
	                						 color     = '#494845',
	                						 linewidth = 1) +
	                geom_segment(data      = Summary_Top_Plot_Mean %>% filter(Type == "W"), 
	                						 mapping   = aes(x    = 0.30, 
	                						 							   y    = Mean_pct + 1, 
	                						 							   xend = 0.70, 
	                						 							   yend = Mean_pct - 1
	                						 								), 
	                						 color     = '#494845',
	                						 linewidth = 1
	                						 ) +
	                # Line end interval sd
	                geom_segment(data      = Summary_Top_Plot_Mean %>% filter(Type == "M"),
	                						 mapping   = aes(x    = 0.85, 
	                						 							   y    = Mean_pct - sd_pct, 
	                						 							   xend = 1.15, 
	                						 							   yend = Mean_pct - sd_pct
	                						 								 ),
	                						 color     = '#AAA699',
	                						 linewidth = 1) +
	                geom_segment(data      = Summary_Top_Plot_Mean %>% filter(Type == "M"),
	                						 mapping   = aes(x    = 0.85, 
	                						 							   y    = Mean_pct + sd_pct, 
	                						 							   xend = 1.15, 
	                						 							   yend = Mean_pct + sd_pct
	                						 								),
	                						 color     = '#AAA699',
	                						 linewidth = 1) +
	                geom_segment(data      = Summary_Top_Plot_Mean %>% filter(Type == "W"),
	                						 mapping   = aes(x    = 0.35,
	                						 							   y    = Mean_pct + sd_pct, 
	                						 							   xend = 0.65, 
	                						 							   yend = Mean_pct + sd_pct
	                						 								),
	                						 color     = '#494845',
	                						 linewidth = 1) +
	                geom_segment(data      = Summary_Top_Plot_Mean %>% filter(Type == "W"),
	                						 mapping   = aes(x    = 0.35, 
	                						 								 y    = Mean_pct - sd_pct, 
	                						 								 xend = 0.65, 
	                						 								 yend = Mean_pct - sd_pct
	                						 								),
	                						 color     = '#494845',
	                						 linewidth = 1) +
							    geom_bar(data     = Summary_Top_Plot, aes(x=Season, y=Ratio, fill=Type),
							    				 position = position_dodge(width = 0.5), 
							    				 stat     = "identity", 
							    				 width    = 0.5
							    				 ) + 
	                # Scales
								  scale_x_discrete(expand = c(0.05,0),
								  								 limits = as.character(1999:2020), 
								  								 labels = c("",as.character(2000:2020))
								  								 ) +
								  scale_y_continuous(breaks = c(0,20,40,60,80,100),
								  									 limits = c(0,100),
								  									 labels = c("0 %","20 %","40 %","60 %","80 %",""),
								  									 expand = c(0,0)
								  									 ) +
								  scale_fill_manual(values = c('#AAA699','#494845'),
								  									labels = c("Men", "Women")
								  									) +
	                # Facets
								  facet_wrap(~tourney_name,nrow = 1) +
	                # Theme
								  theme(panel.spacing      = unit(0, "lines"),
								  			panel.background   = element_rect(fill      = "white", 
								  																			  color     = NA,
								  																			  linewidth = 0.75
								  																				),
								  			plot.background    = element_rect(fill = "white"), 
								  			axis.ticks         = element_blank(), 
								  			panel.grid.major.x = element_line(color     = "#DAC878", 
								  																				linetype  = 'dashed',
								  																				linewidth = 0.75
								  																				),
								  			panel.grid.major.y = element_blank(), 
								  			panel.grid.minor.y = element_blank(),
								  			legend.position    = "none",
								  			strip.background   = element_rect(color     = "black", 
								  																			  fill      = "#FDF0B4", 
								  																			  linewidth = 0.5, 
								  																			  linetype  = "solid"
								  																				), 
								  			plot.margin        = margin(c(0,20,5,20))
								  			) +
								  coord_flip() +
								  labs(x = "", y = "") 

Top_Plot



First_Decade_Plot <- ggplot() + 
	                # Horizontal line sd
	                geom_segment(data      = Summary_First_Decade_Plot_Mean %>% filter(Type == "M"), 
	                						 mapping   = aes(x    = 1, 
	                						 								 y    = Mean_pct - sd_pct, 
	                						 								 xend = 1, 
	                						 								 yend = Mean_pct + sd_pct
	                						 								), 
	                						 color     = '#AAA699',
	                						 linewidth = 1
	                						 ) +
	                geom_segment(data      = Summary_First_Decade_Plot_Mean %>% filter(Type == "W"), 
	                						 mapping   = aes(x    = 0.5, 
	                						 								 y    = Mean_pct - sd_pct,
	                						 								 xend = 0.5, 
	                						 								 yend = Mean_pct + sd_pct
	                						 								), 
	                						 color     = '#494845',
	                						 linewidth = 1
	                						 ) +
	                # Cross mean
	                geom_segment(data      = Summary_First_Decade_Plot_Mean %>% filter(Type == "M"), 
	                						 mapping   = aes(x    = 0.90, 
	                						 								 y    = Mean_pct - 2, 
	                						 								 xend = 1.10, 
	                						 								 yend = Mean_pct + 2
	                						 								), 
	                						 color     = '#AAA699',
	                						 linewidth = 1
	                						 ) +
	                geom_segment(data      = Summary_First_Decade_Plot_Mean %>% filter(Type == "M"), 
	                						 mapping   = aes(x    = 0.90, 
	                						 								 y    = Mean_pct + 2,
	                						 								 xend = 1.10, 
	                						 								 yend = Mean_pct - 2
	                						 								), 
	                						 color     = '#AAA699',
	                						 linewidth = 1
	                						 ) +
	                geom_segment(data      = Summary_First_Decade_Plot_Mean %>% filter(Type == "W"), 
	                						 mapping   = aes(x    = 0.40, 
	                						 								 y    = Mean_pct - 2, 
	                						 								 xend = 0.6, 
	                						 								 yend = Mean_pct + 2
	                						 								), 
	                						 color     = '#494845',
	                						 linewidth = 1
	                						 ) +
	                geom_segment(data      = Summary_First_Decade_Plot_Mean %>% filter(Type == "W"), 
	                						 mapping   = aes(x    = 0.40, 
	                						 								 y    = Mean_pct + 2,
	                						 								 xend = 0.6, 
	                						 								 yend = Mean_pct - 2
	                						 								), 
	                						 color     = '#494845',
	                						 linewidth = 1) +
	                # Line end interval sd
	                geom_segment(data      = Summary_First_Decade_Plot_Mean %>% filter(Type == "M"),
	                						 mapping   = aes(x    = 0.90, 
	                						 								 y    = Mean_pct - sd_pct, 
	                						 								 xend = 1.10, 
	                						 								 yend = Mean_pct - sd_pct
	                						 								),
	                						 color     = '#AAA699',
	                						 linewidth = 1
	                						 ) +
	                geom_segment(data      = Summary_First_Decade_Plot_Mean %>% filter(Type == "M"),
	                						 mapping   = aes(x    = 0.90, 
	                						 								 y    = Mean_pct + sd_pct, 
	                						 								 xend = 1.10, 
	                						 								 yend = Mean_pct + sd_pct
	                						 								),
	                						 color     = '#AAA699',
	                						 linewidth = 1
	                						 ) +
	                geom_segment(data      = Summary_First_Decade_Plot_Mean %>% filter(Type == "W"),
	                						 mapping   = aes(x    = 0.40, 
	                						 								 y    = Mean_pct + sd_pct,
	                						 								 xend = 0.60, 
	                						 								 yend = Mean_pct + sd_pct
	                						 								),
	                						 color     = '#494845',
	                						 linewidth = 1
	                						 ) +
	                geom_segment(data      = Summary_First_Decade_Plot_Mean %>% filter(Type == "W"),
	                						 mapping   = aes(x    = 0.40, 
	                						 								 y    = Mean_pct - sd_pct, 
	                						 								 xend = 0.60, 
	                						 								 yend = Mean_pct - sd_pct
	                						 								),
	                						 color     = '#494845',
	                						 linewidth = 1
	                						 ) +
							    geom_bar(data     = Summary_First_Decade_Plot, aes(x=Season, y=Ratio, fill=Type),
							    				 position = position_dodge(width = 0.5), 
							    				 stat     = "identity", 
							    				 width    = 0.5
							    				 ) + 
	                # Scales
								  scale_x_discrete(expand = c(0.05,0),
								  								 limits = as.character(1999:2010),
								  								 labels = c("",as.character(2000:2010))
								  								 ) +
								  scale_y_continuous(breaks = c(0,20,40,60,80,100),
								  									 limits = c(0,100),
								  									 labels = c("0 %","20 %","40 %","60 %","80 %",""),
								  									 expand = c(0,0)) +
								  scale_fill_manual(values = c('#AAA699','#494845'),
								  									labels = c("Men", "Women")
								  									) +
	                # Facets
								  facet_wrap(~tourney_name,nrow = 1) +
	                # Theme
								  theme(panel.spacing      = unit(0, "lines"),
								  			panel.background   = element_rect(fill      = "white", 
									  																			color     = NA,
									  																			linewidth = 0.75
								  																				),
								  			plot.background    = element_rect(fill = "white"), 
								  			axis.ticks         = element_blank(), 
								  			panel.grid.major.x = element_line(color     = "#DAC878", 
								  																				linetype  = 'dashed',
								  																				linewidth = 0.75
								  																				),
								  			panel.grid.major.y = element_blank(), 
								  			panel.grid.minor.y = element_blank(),
								  			legend.position    = "none",
								  			strip.background   = element_rect(color     = "black", 
									  																			fill      = "#FDF0B4", 
									  																			linewidth = 0.5, 
									  																			linetype  = "solid"
								  																				), 
								  			plot.margin        = margin(c(0,20,0,20))
								  			) +
								  coord_flip() +
								  labs(x = "", y = "")

First_Decade_Plot





Second_Decade_Plot <- ggplot() + 
	                # Horizontal line sd
	                geom_segment(data      = Summary_Second_Decade_Plot_Mean %>% filter(Type == "M"), 
	                						 mapping   = aes(x    = 1, 
	                						 								 y    = Mean_pct - sd_pct, 
	                						 								 xend = 1, 
	                						 								 yend = Mean_pct + sd_pct
	                						 								), 
	                						 color     = '#AAA699',
	                						 linewidth = 1
	                						 ) +
	                geom_segment(data      = Summary_Second_Decade_Plot_Mean %>% filter(Type == "W"), 
	                						 mapping   = aes(x    = 0.5, 
	                						 								 y    = Mean_pct - sd_pct, 
	                						 								 xend = 0.5, 
	                						 								 yend = Mean_pct + sd_pct
	                						 								), 
	                						 color     = '#494845',
	                						 linewidth = 1
	                						 ) +
	                # Cross mean
	                geom_segment(data      = Summary_Second_Decade_Plot_Mean %>% filter(Type == "M"), 
	                						 mapping   = aes(x    = 0.90, 
		                						 							 y    = Mean_pct - 2, 
		                						 							 xend = 1.10, 
		                						 							 yend = Mean_pct + 2
		                						 							), 
	                						 color     = '#AAA699',
	                						 linewidth = 1
	                						 ) +
	                geom_segment(data      = Summary_Second_Decade_Plot_Mean %>% filter(Type == "M"), 
	                						 mapping   = aes(x    = 0.90, 
		                						 							 y    = Mean_pct + 2, 
		                						 							 xend = 1.10, 
		                						 							 yend = Mean_pct - 2
		                						 							), 
	                						 color     = '#AAA699',
	                						 linewidth = 1
	                						 ) +
	                geom_segment(data      = Summary_Second_Decade_Plot_Mean %>% filter(Type == "W"), 
	                						 mapping   = aes(x    = 0.40,
		                						 							 y    = Mean_pct - 2,
		                						 							 xend = 0.60, 
		                						 							 yend = Mean_pct + 2
		                						 							), 
	                						 color     = '#494845',
	                						 linewidth = 1
	                						 ) +
	                geom_segment(data      = Summary_Second_Decade_Plot_Mean %>% filter(Type == "W"), 
	                						 mapping   = aes(x    = 0.40, 
	                						 							   y    = Mean_pct + 2,
	                						 							   xend = 0.60, 
	                						 							   yend = Mean_pct - 2
	                						 							), 
	                						 color     = '#494845',
	                						 linewidth = 1
	                						 ) +
	                # Line end interval sd
	                geom_segment(data      = Summary_Second_Decade_Plot_Mean %>% filter(Type == "M"),
	                						 mapping   = aes(x    = 0.90, 
		                						 							 y    = Mean_pct - sd_pct,
		                						 							 xend = 1.10, 
		                						 							 yend = Mean_pct - sd_pct
	                						 								),
	                						 color     = '#AAA699',
	                						 linewidth = 1
	                						 ) +
	                geom_segment(data      = Summary_Second_Decade_Plot_Mean %>% filter(Type == "M"),
	                						 mapping   = aes(x    = 0.90, 
	                						 								 y    = Mean_pct + sd_pct, 
	                						 								 xend = 1.10, 
	                						 								 yend = Mean_pct + sd_pct
	                						 								),
	                						 color     = '#AAA699',
	                						 linewidth = 1
	                						 ) +
	                geom_segment(data      = Summary_Second_Decade_Plot_Mean %>% filter(Type == "W"),
	                						 mapping   = aes(x    = 0.40, 
	                						 								 y    = Mean_pct + sd_pct, 
	                						 								 xend = 0.60, 
	                						 								 yend = Mean_pct + sd_pct
	                						 								),
	                						 color     = '#494845',
	                						 linewidth = 1
	                						 ) +
	                geom_segment(data      = Summary_Second_Decade_Plot_Mean %>% filter(Type == "W"),
	                						 mapping   = aes(x    = 0.40, 
	                						 							   y    = Mean_pct - sd_pct, 
	                						 								 xend = 0.60, 
	                						 								 yend = Mean_pct - sd_pct
	                						 								),
	                						 color     = '#494845',
	                						 linewidth = 1
	                						 ) +
							    geom_bar(data     = Summary_Second_Decade_Plot,aes(x=Season, y=Ratio, fill=Type),
							    				 position = position_dodge(width = 0.5), 
							    				 stat     = "identity", 
							    				 width    = 0.5
							    				 ) + 
	                # Scales
								  scale_x_discrete(expand = c(0.05,0),
								  								 limits = as.character(2010:2020), 
								  								 labels = c("",as.character(2011:2020))
								  								 ) +
								  scale_y_continuous(breaks = c(0,20,40,60,80,100),
								  									 limits = c(0,100),
								  									 labels = c("0 %","20 %","40 %","60 %","80 %",""),
								  									 expand = c(0,0)
								  									 ) +
								  scale_fill_manual(values = c('#AAA699','#494845'),
								  									labels = c("Men", "Women")
								  									) +
	                # Facets
								  facet_wrap(~tourney_name,nrow = 1) +
	                # Theme
								  theme(panel.spacing      = unit(0, "lines"),
								  			panel.background   = element_rect(fill      = "white", 
								  																			  color     = NA,
								  																			  linewidth = 0.75),
								  			plot.background    = element_rect(fill = "white"), 
								  			axis.ticks         = element_blank(), 
								  			panel.grid.major.x = element_line(color     = "#DAC878", 
								  																				linetype  = 'dashed',
								  																				linewidth = 0.75),
								  			panel.grid.major.y = element_blank(), 
								  			panel.grid.minor.y = element_blank(),
								  			legend.position    = "none",
								  			strip.background   = element_rect(color     = "black", 
								  																			  fill      = "#FDF0B4", 
								  																			  linewidth = 0.5, 
								  																			  linetype  = "solid"), 
								  			axis.text.y        = element_text(size = 9),
								  			plot.margin        = margin(c(0,20,0,20))
								  			) +
								  coord_flip() +
								  labs(x = "", y = "")

Second_Decade_Plot


Legend <-  ggplot(data.frame(x = 1, y = 1)) + 
								# Men Women rect
								annotate('rect', xmin = 0.15, xmax = 0.20, ymin = 0.48, ymax = 0.53, fill = '#AAA699') + 
								annotate('rect', xmin = 0.35, xmax = 0.40, ymin = 0.48 ,ymax = 0.53, fill = '#494845') + 
								# Mean + sd rect
								annotate('rect', xmin = 0.55, xmax = 0.60, ymin = 0.48, ymax = 0.53, fill = '#DAA6EA') + 
								# Mean + sd cross
								annotate('segment', x = 0.572, xend = 0.578, y = 0.4, yend = 0.6, color = '#DAA6EA', linewidth = 1) + 
								annotate('segment', x = 0.578, xend = 0.572, y = 0.4, yend = 0.6, color = '#DAA6EA', linewidth = 1) + 
								# Mean + sd interval line end
								annotate('segment', x = 0.55, xend = 0.55, y = 0.4, yend = 0.6, color = '#DAA6EA', linewidth = 1) + 
								annotate('segment', x = 0.60, xend = 0.6,  y = 0.4, yend = 0.6, color = '#DAA6EA', linewidth = 1) + 
								# Men Women text
								annotate('text', label = 'Men',   x = 0.175, y = 0.85, color = 'black', size = 3.75) +
								annotate('text', label = 'Women', x = 0.375, y = 0.85, color = 'black', size = 3.75) + 
								annotate('text', label = 'µ ± σ', x = 0.575, y = 0.85, color = 'black', size = 3.75) +
								# Scales
								scale_x_continuous(limits = c(0,0.75))+
								scale_y_continuous(limits = c(0,1)) +
								# Theme custom
								theme_void() +
								theme(plot.margin = margin(t = 0, b = 0.25))


Caption <-  ggplot(data.frame(x = 1, y = 1)) + 
									theme_void() +
									labs(title = "Visualisation by DENIAUX Maxime | Data : SACKMANN Jeff") +
									theme(plot.margin   = margin(t = 20, b = 20), 
												plot.title    = element_text(size  = 10,
																										 hjust = 0.5, 
																										 color = "gray55"
																										 )
												)


Title_First_Plot <- ggplot(data.frame(x = 1, y = 1)) + 
											theme_void() +
											labs(title    = "Do the top seeds in the grand slam tournaments hold their own in the present century ?",
													 subtitle = "32 at the start of each of the 4 tournaments for 128 players in total, the proportion of top seeds qualifying for the 3rd round (32 players remaining) is represented below."
													 ) +
											theme(plot.margin    = margin(t = 20, b = 5), 
														plot.title     = element_text(size   = 20,
																													hjust  = 0.5, 
																													color  = "black", 
																													margin = margin(b = 5)
																													), 
														plot.subtitle  = element_text(size  = 10,
																													hjust = 0.5, 
																													color = "gray40")
														)



Title_Second_Plot <- ggplot(data.frame(x = 1, y = 1)) + 
												theme_void() +
												labs(title = "A last decade with top seeds having more regular performances, but not necessarily better on average.") +
												theme(plot.margin   = margin(t = 30, b = 0), 
															plot.title    = element_text(size  = 15,
																													 hjust = 0.5, 
																													 color = "black"
																													 )
															)




# Save Plot ----

Final_First_Plot <- plot_grid(Title_First_Plot,Legend,Top_Plot,Caption, 
															nrow        = 4,
															rel_heights = c(0.15,0.05,0.74,0.06)
															)

Final_First_Plot_Without_Caption <- plot_grid(Title_First_Plot,Legend,Top_Plot, 
																						  nrow = 3,
																						  rel_heights = c(0.15,0.08,0.77)
																						  )

Final_Second_Plot <- plot_grid(Title_Second_Plot,
															 plot_grid(First_Decade_Plot,Second_Decade_Plot,nrow = 1),
															 Caption,
											         nrow = 3,
								               rel_heights = c(0.15,0.75,0.10)
											)



ggsave(plot     =  Final_First_Plot,
 			 filename = "First_Plot.png",
			 width    = 15,
			 height   = 10,
			 device   = 'png',
			 bg       = "white")

ggsave(plot     =  Final_Second_Plot,
 			 filename = "Second_Plot.png",
			 width    = 20,
			 height   = 8,
			 device   = 'png',
			 bg       = "white")

ggsave(plot =  plot_grid(Final_First_Plot_Without_Caption,
												 Final_Second_Plot, 
												 nrow = 2),
 			 filename = "Final_Plot.png",
			 width    = 17,
			 height   = 15,
			 device   = 'png',
			 bg       = "white")











