

# Packages ----

library(ggplot2)
library(tidyverse)
library(cowplot)



# Directory

setwd("C:/Users/maxim/Documents/Site_Web/Git/R-Dataviz/Tennis/3. Comparaison_ATP_WTA")


# Data ----

atp_matches_2021 <- read_csv("atp_matches_2021.csv") %>% select(surface,winner_rank,loser_rank)
wta_matches_2021 <- read_csv("wta_matches_2021.csv") %>% select(surface,winner_rank,loser_rank)

atp_matches_2020 <- read_csv("atp_matches_2020.csv") %>% select(surface,winner_rank,loser_rank)
wta_matches_2020 <- read_csv("wta_matches_2020.csv") %>% select(surface,winner_rank,loser_rank)

atp_matches_2019 <- read_csv("atp_matches_2019.csv") %>% select(surface,winner_rank,loser_rank)
wta_matches_2019 <- read_csv("wta_matches_2019.csv") %>% select(surface,winner_rank,loser_rank)

atp_matches_2018 <- read_csv("atp_matches_2018.csv") %>% select(surface,winner_rank,loser_rank)
wta_matches_2018 <- read_csv("wta_matches_2018.csv") %>% select(surface,winner_rank,loser_rank)

atp_matches_2017 <- read_csv("atp_matches_2017.csv") %>% select(surface,winner_rank,loser_rank)
wta_matches_2017 <- read_csv("wta_matches_2017.csv") %>% select(surface,winner_rank,loser_rank)

atp_matches_2016 <- read_csv("atp_matches_2016.csv") %>% select(surface,winner_rank,loser_rank)
wta_matches_2016 <- read_csv("wta_matches_2016.csv") %>% select(surface,winner_rank,loser_rank)

atp_matches_2015 <- read_csv("atp_matches_2015.csv") %>% select(surface,winner_rank,loser_rank)
wta_matches_2015 <- read_csv("wta_matches_2015.csv") %>% select(surface,winner_rank,loser_rank)

atp_matches_2014 <- read_csv("atp_matches_2014.csv") %>% select(surface,winner_rank,loser_rank)
wta_matches_2014 <- read_csv("wta_matches_2014.csv") %>% select(surface,winner_rank,loser_rank)

atp_matches_2013 <- read_csv("atp_matches_2013.csv") %>% select(surface,winner_rank,loser_rank)
wta_matches_2013 <- read_csv("wta_matches_2013.csv") %>% select(surface,winner_rank,loser_rank)

atp_matches_2012 <- read_csv("atp_matches_2012.csv") %>% select(surface,winner_rank,loser_rank)
wta_matches_2012 <- read_csv("wta_matches_2012.csv") %>% select(surface,winner_rank,loser_rank)

atp_matches_2011 <- read_csv("atp_matches_2011.csv") %>% select(surface,winner_rank,loser_rank)
wta_matches_2011 <- read_csv("wta_matches_2011.csv") %>% select(surface,winner_rank,loser_rank)

atp_matches_2010 <- read_csv("atp_matches_2010.csv") %>% select(surface,winner_rank,loser_rank)
wta_matches_2010 <- read_csv("wta_matches_2010.csv") %>% select(surface,winner_rank,loser_rank)



# Data Processing ----


# Bind rows

WTA_Matches <-  bind_rows(wta_matches_2010,wta_matches_2011,wta_matches_2012,wta_matches_2013,wta_matches_2014,
												wta_matches_2015,wta_matches_2016,wta_matches_2017,wta_matches_2018,
												wta_matches_2019,wta_matches_2020,wta_matches_2021)


rm(wta_matches_2010,wta_matches_2011,wta_matches_2012,wta_matches_2013,wta_matches_2014,
	 wta_matches_2015,wta_matches_2016,wta_matches_2017,wta_matches_2018,
	 wta_matches_2019,wta_matches_2020,wta_matches_2021)



ATP_Matches <-  bind_rows(atp_matches_2010,atp_matches_2011,atp_matches_2012,atp_matches_2013,atp_matches_2014,
												atp_matches_2015,atp_matches_2016,atp_matches_2017,atp_matches_2018,
												atp_matches_2019,atp_matches_2020,atp_matches_2021)


rm(atp_matches_2010,atp_matches_2011,atp_matches_2012,atp_matches_2013,atp_matches_2014,
	 atp_matches_2015,atp_matches_2016,atp_matches_2017,atp_matches_2018,
	 atp_matches_2019,atp_matches_2020,atp_matches_2021)



# Favorite (by rank) win ?

ATP_Matches <-  ATP_Matches %>% mutate(Favorite_By_Rank_Win = ifelse(winner_rank < loser_rank, 1, 0) )
WTA_Matches <-  WTA_Matches %>% mutate(Favorite_By_Rank_Win = ifelse(winner_rank < loser_rank, 1, 0) )


# Best rank 

ATP_Matches <-  ATP_Matches %>% mutate(Best_Rank = ifelse(winner_rank < loser_rank,winner_rank,loser_rank))
WTA_Matches <-  WTA_Matches %>% mutate(Best_Rank = ifelse(winner_rank < loser_rank,winner_rank,loser_rank))


# 2nd best rank

ATP_Matches <-  ATP_Matches %>% mutate(Second_Best_Rank = ifelse(winner_rank > loser_rank,winner_rank,loser_rank))
WTA_Matches <-  WTA_Matches %>% mutate(Second_Best_Rank = ifelse(winner_rank > loser_rank,winner_rank,loser_rank))



# Remove lines with NA

ATP_Matches <-  ATP_Matches[-which(is.na(ATP_Matches$Favorite_By_Rank_Win)),]
WTA_Matches <-  WTA_Matches[-which(is.na(WTA_Matches$Favorite_By_Rank_Win)),]


# Remove surface 'carpet'

ATP_Matches <-  ATP_Matches %>% filter(surface != 'Carpet')
WTA_Matches <-  WTA_Matches %>% filter(surface != 'Carpet')



# Individuals plots

Data_Plot_WTA <-  WTA_Matches %>% filter(Best_Rank <= 50) %>%
	group_by(Best_Rank,Favorite_By_Rank_Win, surface) %>%
	summarise(Count = n()) %>%
	group_by(Best_Rank, surface) %>%
	mutate(pct = Count / sum(Count))

Data_Plot_WTA$Favorite_By_Rank_Win <-  factor(Data_Plot_WTA$Favorite_By_Rank_Win,levels = c(1,0))
	
	


Plot_WTA <-  ggplot(Data_Plot_WTA, 
																 aes(x    = as.factor(Best_Rank), 
																 		y     = pct,  
																 		fill  = surface, 
																 		alpha = Favorite_By_Rank_Win)) +
													    geom_bar(position = "fill", 
													    				 stat     = "identity", 
													    				 width    = 1,
													    				 color    = "#F4F1F1") +
													   	theme_minimal_hgrid() +
													    facet_wrap(~ surface,
													    					 ncol = 3) +
														  scale_alpha_manual(values = c(0.5,1),
														  									 labels = c('No','Yes'))  +
														  scale_y_continuous(labels = scales::percent) +
														  scale_x_discrete(breaks = c(1,seq(0,100,10))) +
														  labs(y     = "", 
														  		 x     = "\n Player rank", 
														  		 title = "\n WTA \n") + 
													  	theme(strip.text.x        = element_blank(),
													  				panel.spacing       = unit(2, "lines"),
													  				plot.margin         = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
													  				plot.title          = element_text(hjust = 0,
													  																					 color = "#A9A9A9"
													  																					 ),
													  				axis.text.x         = element_text(color = '#817777'),
													  				axis.ticks.length.x = unit(0.25, "cm"),
													  				panel.grid.major.y  = element_line(linewidth = 0.75,
													  																					 colour    = "#BBB3B3"),
													  				axis.ticks.y        = element_blank()
													  				) + 
														  guides(fill  = 'none',
														  			 alpha = 'none') 
  







Data_Plot_ATP <-  ATP_Matches %>% filter(Best_Rank <= 50) %>%
	group_by(Best_Rank,Favorite_By_Rank_Win, surface) %>%
	summarise(Count = n()) %>%
	group_by(Best_Rank, surface) %>%
	mutate(pct = Count / sum(Count))

Data_Plot_ATP$Favorite_By_Rank_Win <-  factor(Data_Plot_ATP$Favorite_By_Rank_Win,levels = c(1,0))
	
	


Plot_ATP <-  ggplot(Data_Plot_ATP, 
																 aes(x    = as.factor(Best_Rank), 
																 		y     = pct,  
																 		fill  = surface, 
																 		alpha = Favorite_By_Rank_Win)) +
													    geom_bar(position = "fill",  
													    				 stat     = "identity", 
													    				 width = 1,
													    				 color = "#F4F1F1") +
													   	theme_minimal_hgrid() +
													    facet_wrap(~ surface, 
													    					 ncol = 3) +
														  scale_alpha_manual(values = c(0.5,1),
														  									 labels = c('No','Yes'))  +
														  scale_y_continuous(labels = scales::percent) +
														  scale_x_discrete(breaks = c(1,seq(0,100,10))) +
														  labs(y     = "", 
														  		 x     = "", 
														  		 title = "\n ATP \n") + 
													  	theme(strip.text.x        = element_blank(),
													  				panel.spacing       = unit(2, "lines"),
													  				plot.margin         = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
													  				plot.title          =  element_text(hjust = 0,
													  																					 color = "#A9A9A9"
													  																					 ),
													  				axis.text.x         = element_blank(),
													  				axis.ticks.length.x = unit(0.25, "cm"),
													  				panel.grid.major.y  = element_line(linewidth = 0.75,
													  																					 colour    = "#BBB3B3"),
													  				axis.ticks.y        = element_blank()
													  				) + 
														  guides(fill  = 'none',
														  			 alpha = 'none')






# Legend plot

Legend <-  ggplot() + 
					# First part
					annotate(geom  = 'rect',
									 xmin  = 11, xmax = 12.5 ,
									 ymin  = 20.5, ymax = 21,
									 fill  = "#F8766D",
									 color = "black") +
					annotate(geom  = 'rect',
									 xmin  = 12.75, xmax = 14.25 ,
									 ymin  = 20.5, ymax = 21,
									 fill  = "#00BA38",
									 color = "black"
									 ) +
					annotate(geom  = 'rect',
									 xmin  = 14.5, xmax = 16,
									 ymin  = 20.5, ymax = 21,
									 fill  = "#619CFF",
									 color = "black"
									 ) +
					annotate('text',
									 label = "Court surface",
									 size  = 4,
									 x     = 13.45,
									 y     = 22.1,
									 color = "black") +
						annotate('text',
									 label = "Clay",
									 size  = 4.25,
									 x     = 11.75,
									 y     = 20.05,
									 color = "#F8766D") +
						annotate('text',
									 label = "Grass",
									 size  = 4.25,
									 x     = 13.5,
									 y     = 20.05,
									 color = "#00BA38") +
						annotate('text',
									 label = "Hard",
									 size  = 4.25,
									 x     = 15.25,
									 y     = 20.05,
									 color = "#619CFF") +
					
					# 2nd part
					annotate(geom  = 'rect',
									 xmin  = 11, xmax = 13.25,
									 ymin  = 14.5, ymax = 15,
									 fill  = "#3E3D3D",
									 color = "black") +
					annotate(geom  = 'rect',
									 xmin  = 13.75, xmax = 16 ,
									 ymin  = 14.5, ymax = 15,
									 fill  = "#E2E0E0",
									 color = "black"
									 ) +
					annotate('text',
									 label = "Lost to lower ranked ?",
									 size  = 4,
									 x     = 13.5,
									 y     = 16.1,
									 color = "black") +
						annotate('text',
									 label = "Yes",
									 size  = 4,
									 x     = 12.125,
									 y     = 14.05,
									 color = "black") +
						annotate('text',
									 label = "No",
									 size  = 4,
									 x     = 14.875,
									 y     = 14.05,
									 color = "black") +
					scale_x_continuous(limits = c(10,18)) + 
					scale_y_continuous(limits = c(5,30)) +
					theme_nothing()
	
										


Title = ggplot(df_title,aes(x = x, y = y)) +
									  theme_void() +
								    labs(title = 'Comparison of top 50 performances between WTA and ATP (2010-2021)') +
								    theme(plot.margin      = margin(c(20,0,0,0)),
											  	panel.background = element_rect(fill  = "white", 
											  																	color = "white"
											  																	),
												  plot.background  = element_rect(fill  = "white", 
												  																color = "white"
												  																),
											  	plot.title       = element_text(size   = 18,
											  																	face   = "bold",
											  																	hjust  = 0.5,
											  																	margin = margin(c(0,0,0,0)),
											  																	color  = "#2A2A2A"
											  																	)
											  	) 


Caption = ggplot(tibble(x = 1, y = 0), aes(x = x, y = y)) +
									  theme_void() +
								    labs(title = 'Visualisation by DENIAUX Maxime | Data : SACKMANN Jeff') +
								    theme(plot.margin      = margin(c(10,0,10,0)),
											  	panel.background = element_rect(fill  = "white", 
											  																	color = "white"
											  																	),
												  plot.background  = element_rect(fill  = "white", 
												  																color = "white"
												  																),
											  	plot.title       = element_text(size   = 11,
											  																	hjust  = 0.45,
											  																	margin = margin(c(0,0,5,0)), 
											  																	color = "#8E8B8B"
											  																	)
											  	) 



# Combine plots


Left_Part <-  plot_grid(Plot_ATP, 
	                      Plot_WTA, 
	                      nrow = 2, 
	                      rel_heights = c(0.5,0.5)
	                      )

Plot_mid      <-  plot_grid(Left_Part, 
											      Legend, 
											      rel_widths = c(0.8, 0.2)
														)

Final_Plot    <- plot_grid(Title, 
		                       Plot_mid, 
													 Caption,
		                       nrow = 3, 
		                       rel_heights = c(0.05,0.90,0.05)
		                       )





ggsave(plot     = Final_Plot,
			 filename = "ATP_WTA_Comparison.png", 
			 width    = 15,
			 height   = 12.5,
			 bg       = "white")




