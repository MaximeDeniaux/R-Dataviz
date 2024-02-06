

# Packages ----

library(rvest)
library(tidyverse)
library(cowplot)
library(ggtext)



# Directory ----

setwd("C:/Users/maxim/Documents/Site_Web/Git/R-Dataviz/Football/10. Tactical_Systems")




# Data ----

Data_Final <- read_csv("Data.txt")



# Data processing ----


data_barplot1 <-  Data_Final %>% group_by(League,Classification) %>% summarise(n = n())%>% mutate(freq = n / sum(n)) %>% select(-n)
data_barplot2 <-  data_barplot1 %>% group_by(League) %>% summarise(total_3_5_d  = freq[Classification == "3_5d_1s"] + freq[Classification == "3_5d_2s"],
																																   total_4_d    = 1 - (freq[Classification == "3_5d_1s"] + freq[Classification == "3_5d_2s"])
																																   ) %>% pivot_longer(cols      = c("total_3_5_d","total_4_d"),
																																 									    names_to  = "Classification",
																																 									    values_to = "freq"
																																   									 )

data_barplot                <-  bind_rows(data_barplot1,data_barplot2)
data_barplot$Classification <-  factor(x      = data_barplot$Classification,
																		   levels = c("3_5d_1s","3_5d_2s","total_3_5_d",
																		 					    "4d_1s","4d_2s","total_4_d"
																		   					 )
																		   )


rm(data_barplot1)
rm(data_barplot2)

# Plot ----


	
Plot <- data_barplot %>%
	         ggplot() +
											geom_bar(aes(x    = Classification, 
																	 y    = freq, 
																	 fill = League),
															 position = 'dodge',
															 stat     = "identity",
															 alpha    = 0.85, 
															 color    = "white"
															 ) +
	                    # Fill colors
											scale_fill_manual(values = c("#264653","#2a9d8f", "#e9c46a", "#f4a261", "#e76f51")) +
	                    # Titles
											labs(title    = "The most used types of playing systems in 2023 in the 5 major leagues",
													 subtitle = "How to read ?",
													 x        = "", 
													 y        = ""
													 ) + 
	                    # Scales
											scale_y_continuous(breaks = seq(0,1,0.2),
																				 labels = paste(seq(0,100,20)," %",sep = ""),
																				 limits = c(0,1.20)
																				 ) + 
											scale_x_discrete(labels = c('3_5d & 1s','3_5d & 2s','3_5d','4d & 1s','4d & 2s','4d')) +
	                    # Legend annotation
											annotate('text', x =  4.35,  y = 1.1825, label = "Playing systems typology",  size = 3.8, color = "#7F4A39") +
											annotate('text', x =  5.45,  y = 1.1825, label = "3_5d  :  3 or 5 defenders", size = 3.4, color = "#56565B") +
											annotate('text', x =  6.2, y = 1.1825, label = "2s  :  2 strikers",        size = 3.4, color = "#56565B") +  
	                    # Arrow annotation data
	                    annotate('curve',
													     x = 0.75, 
													     y = 0.5,
													     xend = 1,
													     yend = 0.3,
													     linewidth = 0.5,
													     curvature = -0.25,
													     color = "gray30",
													     arrow = arrow(length = unit(0.2, 'cm'))
													     ) +
	                    # Text annotation data
	                    geom_richtext(aes(x     = 0.55, 
	                    									y     = 0.55, 
	                    									label = "Around 27% of the playing <br> systems used in Ligue 1 had <br> 3 or 5 defenders and 1 striker."), 
	                    							color = "black", 
	                    							fill  = "white", 
	                    							hjust = 0,
	                    							vjust = 0.5, 
	                    							size = 2.8
	                    							) +
											theme(panel.background   = element_rect(fill = "white"),
														axis.ticks         = element_blank(),
														panel.grid.major.y = element_line(color     = "gray80",
																															linewidth = 0.5, 
																															linetype  = "dashed"
																															), 
														legend.position    = c(0.21,0.95),
														legend.spacing.x   = unit(0.2, 'cm'), 
														plot.title         = element_text(margin = margin(b = 40), 
																															size   = 20, 
																															hjust  = 0.5, 
																															color  = "#353539"
																															),
														plot.subtitle      = element_text(margin = margin(t = 10, b = 20), 
																															size   = 15, 
																															hjust  = 0.5,
																															color  = "#7F4A39"
																															), 
														plot.margin        = margin(20,20,20,20)
														
														) + 
											guides(fill = guide_legend(keywidth       = unit(2.15,"cm"),
																								 keyheight      = unit(0.35,"cm"), 
																								 direction      = "horizontal",
																								 label.position = "top",
																								 label.theme    = element_text(color = "#56565B", 
																								 						 							     size  = 8.25
																								 															 ),
																								 title.theme = element_text(vjust  = 0.15,
																								 													  hjust  = -1.5, 
																								 													  color  = "#7F4A39",
																								 													  size   = 11,
																								 													  margin = margin(r = 10)
																								 													 ),
																								 )
														 )


# Caption 

Caption <-  ggplot(data.frame(x = 1, y = 1)) + 
							theme_void() +
							labs(title = "Visualisation by DENIAUX Maxime | Data : Transfermarkt") +
							theme(panel.background      = element_rect(fill = "white",color = "white"),
										plot.background       = element_rect(fill = "white",color = "white"), 
										plot.title            = element_text(hjust  = 0.5, 
																												 color  = "gray30", 
																												 size   = 7, 
																												 margin = margin(0,0,0,0)
																												 ),
										plot.margin           = margin(t = 10, b = 20)
										)


# Save ----





ggsave(plot     =  plot_grid(Plot,Caption,
														 nrow        = 2,
														 rel_heights = c(0.95,0.05)
														 ),
			 filename = "Plot.png",
			 width    = 13,
			 height   = 8
			 )

