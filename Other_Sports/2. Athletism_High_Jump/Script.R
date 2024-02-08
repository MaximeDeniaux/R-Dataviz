



# Packages ----

library(tidyverse)
library(ggplot2)
library(ggimage)
library(ggtext)
library(rvest)
library(cowplot)
library(ggforce)



# Directory

setwd("C:/Users/maxim/Documents/Site_Web/Git/R-Dataviz/Other_Sports/2. Athletism_High_Jump")


# Data ----



# Historic best performance by year since 1970

Men_Historic_Best_Perf   <- (read_html('https://en.wikipedia.org/wiki/High_jump') %>% html_nodes('table') %>% .[17] %>% html_table(fill = T))[[1]]
Women_Historic_Best_Perf <- (read_html('https://en.wikipedia.org/wiki/High_jump') %>% html_nodes('table') %>% .[18] %>% html_table(fill = T))[[1]]





# Data processing ----



Reduced_Table_Men <- Men_Historic_Best_Perf %>% group_by(Year) %>% summarise(Mark         = Mark,
																																						 Athlete_s    = ifelse(length(unique(Athlete)) == 1,
																																						 								 	     Athlete[1],
																																						 									     paste(Athlete, collapse = " - ")
																																						 									     ),
																																						 Mark_numeric = as.numeric(substr(Mark,1,4))
																																						 ) %>% distinct(Year,.keep_all = T)

Reduced_Table_Women <- Women_Historic_Best_Perf %>% group_by(Year) %>% summarise(Mark = Mark,
																																						     Athlete_s = ifelse(length(unique(Athlete)) == 1,
																																						     									  Athlete[1],
																																						     									  paste(Athlete, collapse = " - ")
																																						     									 ),
																																								 Mark_numeric = as.numeric(substr(Mark,1,4))
																																						     ) %>% distinct(Year,.keep_all = T)


# Poles coordinates

df_poles <- data.frame(x          = c(10,10,35,35), 
											 y          = c(0,15,5,20),
											 which_pole = c('one','one','two','two')
											 )


# Bodies coordinates

df_img_M <- data.frame(x     = c(16.2,18.75,21.5,29,37),
										   y     = c(1.5, 8.25,15,11.75,5),
										   img_M = c("J_0_man.png","J_1_man.png","J_2_man.png","J_3_man.png","Man.png")
										   )

df_img_W <- data.frame(x     = c(16.2,18.75,21.5,29,37),
										   y     = c(1.5, 8.25,13.25,10,5),
										   img_W = c("J_0_woman.png","J_1_woman.png","J_2_woman.png","J_3_woman.png","Woman.png")
										   )


# Steps coordinates

df_steps_W <- data.frame(x     = c(13.5,12,7.25,6,1.5,0,-3.5,-3.25,-5,-3),
										     y     = c(-1.75,-5.5,-7.25,-11,-12.5,-17.5,-21,-25,-30,-30),
										     img   = rep(c("Left_Foot_woman.png","Right_Foot_woman.png"),5),
											   angle = c(65,45,60,40,55,35,45,12,15,-5)
										     )


df_steps_M <- data.frame(x     = c(13.5,12,7.25,6,1.5,0,-3.5,-3.25,-5,-3),
										     y     = c(-1.75,-5.5,-7.25,-11,-12.5,-17.5,-21,-25,-30,-30),
										     img   =  rep(c("Right_Foot_man.png","Left_Foot_man.png"),5),
											   angle = c(65,45,60,40,55,35,45,12,15,-5)
										     )



# Plots ----

# For body transparency

transparent <- function(img) {magick::image_fx(img, expression = "0.50*a", channel = "alpha")}



# Plot mid/top, left  

# There are several 47 - ... Because I started with the right plot, and then I wanted to do a trick to create the mirror effect.
# Also, it was possible to use dataframes to avoid using certain functions several times. 
# But let's say that it was easier to adjust by doing it like that.

plot_mid_top_left = ggplot() +  
														# Steps
														geom_image(data = df_steps_M,
																			 mapping = aes(47-x,
																										 y,
																										 image = img,
																										 angle = -angle),
																			 image_fun = transparent,
																			 size      = 0.025
																			 ) +
														# Poles
														geom_line(data = df_poles, 
																			mapping = aes(x     = 47-x, 
																										y     = y, 
																										group = which_pole),
																			color     = "#113109", 
																			linewidth = 1,
																			linetype  = "solid"
																			) +
														# Rod
														geom_line(mapping =  aes(x = c(12,37), 
																										 y = c(17.5,12.5)),
																			color     = "#654E65", 
																			linewidth = 1,
																			linetype  = "solid"
																			) +
														# Body jump
														geom_image(data = df_img_M %>% filter(img_M != "Man.png"), 
																			 mapping = aes(47-x,
																			 							 y,
																			 							 image = img_M
																			 							),
																			 image_fun = transparent,
																			 size      = 0.15
																			 ) +
														# Body side pole
														geom_image(data = df_img_M %>% filter(img_M == "Man.png"), 
																			 mapping = aes(44.5,
																			 							 4.88,
																			 							 image = img_M
																			 							 ),
																			 image_fun = transparent,
																			 size      = 0.072
																			 ) +
														# Name annotation
														geom_richtext(mapping = aes(x             = 7.2,
																												y             = 0,
																												label         = "<span style='color:white'>.</span>**Javier SOTOMAYOR**<span style='color:white'>........</span>"),
																												angle         = 90,
																												size          = 12,
																												label.padding = unit(c(0.7, 0.2, 0.2, 0.2), "lines"),
																												label.margin  = unit(c(0.7, 0.1, 0.1, 0.1), "lines")
																					) +
	                          # Flag
	                          geom_image(mapping = aes(x     = 7.4,
	                          												 y     = 14,
	                          												 image = "Cuba.png"
	                          												 ),
	                          					 size  = 0.09,
	                          					 angle = 270
	                          					 ) +
														# Arrow height rod
	                          geom_segment(aes(x = 38, y = 0,
	                          						     xend = 38, yend = 12.5
	                          								 ),
	                          						 arrow = arrow(length = unit(0.01, "npc"),
	                          						 							 ends   = "both",
	                          						 							 type   = "closed"
	                          						 							),
	                          						 color     = "#654E65",
	                          						 linewidth = 0.75
	                          						 ) +
														# Annotation height rod
														annotate('text',
																		 x     = 40.75 ,
																		 y     = 12.35 ,
																		 label = "2.45m",
																		 size  = 10, 
																		 color = "#654E65") +
														# Arrow height body side pole
	                          geom_segment(aes(x = 47, y = 0,
	                          						     xend = 47, yend = 9.62
	                          								 ),
	                          						 arrow = arrow(length = unit(0.01, "npc"),
	                          						 							 ends   = "both",
	                          						 							 type   = "closed"
	                          						 							),
	                          						 color     = "#a887c0",
	                          						 linewidth = 0.75
	                          						 ) +
														# Annotation height body
														annotate('text',
																		 x     = 49.5 ,
																		 y     = 9.5 ,
																		 label = "1.93m",
																		 size  = 10, 
																		 color = "#886D9B") +
														# Circle annotation factor jump
													  geom_mark_circle(aes(x = 45, 
													  										 y = 20
													  										 ), 
													  								 radius = unit(20, "mm"), 
													  								 color  = "#6FA672", 
													  								 size   = 1.5
													  								 ) +
														# Annotation factor jump
													  annotate('text',
																		 x     = 45 ,
																		 y     = 20 ,
																		 label = "Factor\nx1.27",
																		 size  = 8.5, 
																		 color = "black") +
														# Arrow for the 2 heights
	                          geom_segment(aes(x = c(43.5,46.5), y = c(16.1,16.1),
	                          						     xend = c(39.5,48), yend = c(13.5,10.5)
	                          								 ),
	                          						 color     = "#6FA672",
	                          						 linewidth = 0.5
	                          						 ) +
														# Scales
														scale_x_continuous(limits = c(7,54)) +
														scale_y_continuous(limits = c(-30,25)) +
														# Theme
														theme(plot.background  = element_rect(fill = "white"),
																	panel.background = element_rect(fill = "white"),
																	axis.text        = element_blank(),
																	axis.ticks       = element_blank(),
																	plot.margin      = margin(0,0,0,0)
																	) +
														# Labs
														labs(x = NULL,
																 y = NULL)



# Plot mid/top, right  

plot_mid_top_right = ggplot() +  
														# Steps
														geom_image(data = df_steps_W,
																			 mapping = aes(x,
																										 y,
																										 image = img,
																										 angle = angle),
																			 image_fun = transparent,
																			 size      = 0.025
																			 ) +
														# Poles
														geom_line(data = df_poles, 
																			mapping = aes(x     = x, 
																										y     = y, 
																										group = which_pole),
																			color     = "#113109", 
																			linewidth = 1,
																			linetype  = "solid"
																			) +
														# Rod
														geom_line(mapping =  aes(x = c(10,35),
																										 y = c(10.66,15.66)
																										 ),
																			color     = "#654E65", 
																			linewidth = 1,
																			linetype  = "solid"
																			) +
														# Body jump
														geom_image(data = df_img_W %>% filter(img_W != "Woman.png"), 
																			 mapping = aes(x,
																			 							 y,
																			 							 image = img_W
																			 							 ),
																			 image_fun = transparent,
																			 size      = 0.15
																			 ) +
														# Body side pole
														geom_image(data = df_img_W %>% filter(img_W == "Woman.png"), 
																			 mapping = aes(2.5,
																			 							 4.71,
																			 							 image = img_W
																			 							 ),
																			 image_fun = transparent,
																			 size      = 0.066
																			 ) +
														# Name annotation
														geom_richtext(mapping = aes(x     = 39.6,
																												y     = 0,
																												label = "<span style='color:white'>.</span>**Stefka KOSTADINOVA**<span style='color:white'>........</span>"),
																					angle         = 270,
																					size          = 12,
																					label.padding = unit(c(0.7, 0.2, 0.2, 0.2), "lines"),
																					label.margin  = unit(c(0.7, 0.1, 0.1, 0.1), "lines")
																					) +
	                          # Flag
	                          geom_image(mapping = aes(x     = 39.4,
	                          												 y     = -16,
	                          												 image = "Bulgaria.png"
	                          												 ),
	                          					 size  = 0.09,
	                          					 angle = 90
	                          					 ) +
														# Arrow height rod
	                          geom_segment(aes(x = 9, y = 0,
	                          						     xend = 9, yend = 10.66
	                          								 ),
	                          						 arrow = arrow(length = unit(0.01, "npc"),
	                          						 							 ends = "both",
	                          						 							 type="closed"
	                          						 							),
	                          						 color     = "#654E65",
	                          						 linewidth = 0.75
	                          						 ) +
														# Annotation height rod
														annotate('text',
																		 x     = 6.5 ,
																		 y     = 10.5 ,
																		 label = "2.09m",
																		 size  = 10, 
																		 color = "#654E65") +
														# Arrow height body side pole
	                          geom_segment(aes(x = 0, y = 0, 
	                          						     xend = 0, yend = 9.18
	                          								 ),
	                          						 arrow = arrow(length = unit(0.01, "npc"), 
	                          						 							 ends = "both",
	                          						 							 type="closed"
	                          						 							),
	                          						 color     = "#e99a64",
	                          						 linewidth = 0.75
	                          						 ) +
														# Annotation height body
														annotate('text',
																		 x     = -2.5,
																		 y     = 9.1 ,
																		 label = "1.80m",
																		 size  = 10, 
																		 color = "#CB8658"
																		 ) +
														# Circle annotation factor jump
													  geom_mark_circle(aes(x = 4, 
													  										 y = 20
													  										 ), 
													  								 radius = unit(20, "mm"), 
													  								 color  = "#6FA672", 
													  								 size   = 1.5
													  								 ) +
														# Annotation factor jump
													  annotate('text',
																		 x     = 4 ,
																		 y     = 20 ,
																		 label = "Factor\nx1.16",
																		 size  = 8.5, 
																		 color = "black"
																		 ) +
														# Arrow for the 2 heights
	                          geom_segment(aes(x = c(2.5,5.5), y = c(16,16),
	                          						     xend = c(-0.5,7), yend = c(10.5,11.5)),
	                          						 color     = "#6FA672",
	                          						 linewidth = 0.5
	                          						 ) +
														# Scales
														scale_x_continuous(limits = c(-7,40)) +
														scale_y_continuous(limits = c(-30,25)) +
														# Theme
														theme(plot.background  = element_rect(fill = "white"),
																	panel.background = element_rect(fill = "white"),
																	axis.text        = element_blank(),
																	axis.ticks       = element_blank(),
																	plot.margin      = margin(0,0,0,0)
																	) +
														# Labs
														labs(x = NULL,
																 y = NULL)


# Remove useless objects

rm(df_img)
rm(df_img_M)
rm(df_img_W)
rm(df_poles)
rm(df_rod)
rm(df_steps_M)
rm(df_steps_W)
rm(transparent)






# Bottom plot, left

# Labels for x axis

label_axis_x <- paste(seq(1970,2023,1))
label_axis_x[seq(2,54,2)] = ""

plot_bottom_left <- ggplot(Reduced_Table_Men, aes(Year, Mark_numeric)) +
	                      # Area
											  geom_ribbon(aes(x = Year, ymin = 2.24, ymax = Mark_numeric),
											  						fill  = '#a887c0', 
											  						alpha = 0.35
											  						) +
	                      # Main line
												geom_line(linewidth = 0.8, 
																	linetype  = "solid",
																	color     = "#2A2A23"
																	) +
												# Annotation new century
												geom_segment(aes(x = 2000, xend = 2000, y = 2.24, yend = 2.4),
																		 color     = "#45314D", 
																		 linewidth = 0.35
																		 ) +
												annotate('text',x = 1999.25 ,y = 2.30,
																 label = "New century", 
																 angle = 90, 
																 size  = 6.75) + 
												# Annotation name
												geom_curve(aes(x = 1998, y = 2.44, xend = 1993.25, yend = 2.45),
																	 arrow = arrow(length = unit(0.015, "npc"),
											  							type   = "closed"),
																	 curvature = 0.25,
																	 color     = "#45314D"
											             ) +
												geom_textbox(aes(x     = 1995.5, 
																				 y     = 2.44, 
																				 label = "**Javier SOTOMAYOR**"), 
																		 hjust = 0,
																		 vjust = 0.3,
																		 size  = 6,
																		 col   = "#514F4F",
																		 fill  = "#FFFADF",
																		 width = unit(2.5, "inch")
																		 ) +
												# Scales
												scale_x_continuous(limits = c(1970,2023), 
																					 breaks = seq(1970,2023,1),
																					 labels = label_axis_x,
																					 expand = c(0,0)
																					 ) +
	                      scale_y_continuous(limits = c(2.24,2.46),
	                      									 breaks = seq(2.24,2.46,0.02),
	                      									 labels = paste(sprintf("%.2f", round(seq(2.24,2.46,0.02), digits = 2)),"m", sep = ""),
	                      									 expand = c(0,0)
	                      									 ) +
												# Theme
												theme(plot.background  = element_rect(fill = "white"),
															panel.background = element_rect(fill  = "white",
																															color = "black"),
															plot.margin      = margin(0,15,0,30),
															axis.ticks       = element_blank(),
															axis.text.x      = element_text(size   = 15,
																														  face   = "bold",
																														  color  = "#393232",
																														  angle  = 90,
																														  vjust  = 0.5,
																														  hjust  = 0,
																														  margin = margin(t = 10, b = 5)
																												 ),
															axis.text.y      = element_text(size   = 15,
																														  face   = "bold",
																														  color  = "#393232",
																														  hjust  = 0,
																														  margin = margin(r = 13)
																															),
															panel.grid.major = element_line(linetype = "dashed", 
																															color    = "gray"
																															)
														  ) +
															# Labs
															labs(x = NULL,y = NULL)



# Bottom plot, right


plot_bottom_right <- ggplot(Reduced_Table_Women, aes(Year, Mark_numeric)) +
												# Area
											  geom_ribbon(aes(x = Year, ymin = 1.86, ymax = Mark_numeric),
											  						fill  ='#e99a64', 
											  						alpha = 0.35) +
												# Main line
												geom_line(linewidth = 0.8, 
																	linetype  = "solid",
																	color     = "#2A2A23"
																	) +
												# Annotation new century
												geom_segment(aes(x = 2000, xend = 2000, y = 1.86, yend = 2.02),
																		 color     = "#e99a64", 
																		 linewidth = 0.35) +
												annotate('text',x = 1999.25 ,y = 1.92,
																 label = "New century", 
																 angle = 90, 
																 size  = 6.75) +
												# Annotation name
												geom_curve(aes(x = 1993, y = 2.08, xend = 1987.25, yend = 2.09),
																	 arrow     = arrow(length = unit(0.015, "npc"),
																	 									 type   = "closed"),
																	 curvature = 0.25,
																	 color     = "#e99a64"
																	 ) +
												geom_textbox(aes(x     = 1992, 
																				 y     = 2.08, 
																				 label = "**Stefka KOSTADINOVA**"), 
																		 vjust = 0.3, 
																		 hjust = 0,
																		 size  = 6,
																		 col   = "#514F4F",
																		 fill  = "#FFFADF",
																		 width = unit(2.65, "inch")
																		 ) +
												# Scales
												scale_x_continuous(limits = c(1970,2023), 
																					 breaks = seq(1970,2023,1),
																					 labels = label_axis_x,
																					 expand = c(0,0)
																					 ) +
												scale_y_continuous(limits = c(1.86,2.10),
																					 breaks = seq(1.86,2.10,0.02),
																					 labels = paste(sprintf("%.2f", round(seq(1.86,2.10,0.02),digits = 2)),"m", sep = ""),
																					 expand = c(0,0),
																					 position = 'right'
																					 ) +
												# Theme
												theme(plot.background  = element_rect(fill = "white"),
															panel.background = element_rect(fill  = "white",
																															color = "black"),
															plot.margin      = margin(0,30,0,15),
															axis.ticks       = element_blank(),
															axis.text.x      = element_text(size   = 15,
																														  face   = "bold",
																														  color  = "#393232",
																														  angle  = 90,
																														  vjust  = 0.5,
																														  hjust  = 0,
																														  margin = margin(t = 10, b = 5)
																														  ),
															axis.text.y.right = element_text(size   = 15,
																															 face   = "bold",
																															 color  = "#393232",
																															 hjust  = 0,
																															 margin = margin(l = 13)
																															 ),
															panel.grid.major  = element_line(linetype = "dashed", 
																															 color    = "gray")
															) +
															# Labs
															labs(x = NULL,y = NULL)



# Combine plots

plot_bottom <-	plot_grid(plot_bottom_left,
											    plot_bottom_right,
											    nrow = 1)

plot_mid <-	plot_grid(plot_mid_top_left,
											plot_mid_top_right,
											nrow = 1)

title_bottom <- ggplot(tibble(x = 1, y = 1),
											 aes(x = x, y = y)
										   ) +
									  theme_void() +
								    labs(title = 'Best performance by year since 1970 (indoor and outdoor combined)') +
								    theme(plot.margin      = margin(c(10,0,0,0)),
											  	panel.background = element_rect(fill  = "white", 
											  																	color = "white"
											  																	),
												  plot.background  = element_rect(fill  = "white", 
												  																color = "white"
												  																),
											  	plot.title       = element_text(size   = 26,
		 									  																	face   = "bold",
											  																	hjust  = 0.5,
											  																	color  = "#000000",
											  																	margin = margin(c(40,0,15,0))
											  																	)
											  	) 


title_top <- ggplot(tibble(x = 1, y = 1),
										aes(x = x, y = y)
										) +
									  theme_void() + 
								    labs(title    = 'High Jump, a discipline where the previous century still reigns',
								    		 subtitle = "<span style='color:#000000;'>World records (</span><span style='color:#a887c0;'>men</span><span style='color:#000000;'> & </span><span style='color:#e99a64;'>women</span><span style='color:#000000;'>)</span>") +
								    theme(plot.margin      = margin(c(10,0,0,0)),
											  	panel.background = element_rect(fill  = "white", 
											  																	color = "white"
											  																	),
												  plot.background  = element_rect(fill  = "white", 
												  																color = "white"
												  																),
											  	plot.title       = element_text(size   = 45,
											  																	face   = "bold",
											  																	hjust  = 0.5,
											  																	color  = "#150606",
											  																	margin = margin(c(25,0,10,0))
											  																	),
											  	plot.subtitle    = element_markdown(size   = 26,
													  																	face   = "bold",
													  																	hjust  = 0.5,
													  																	margin = margin(c(25,0,5,0))
													  																	)
											  	) 


caption <- ggplot(tibble(x = 1, y = 1), 
								 aes(x = x, y = y)) +
								 theme_void() +
								 labs(title = 'Visualisation by DENIAUX Maxime | Data : Wikipedia | Icons : Flaticon') +
								 theme(plot.margin         = margin(c(50,0,0,0)),
											 panel.background    = element_rect(fill  = "white", 
											  															    color = "white"
											  										 							),
											 plot.background     = element_rect(fill  = "white", 
												  																color = "white"
												  																),
									  	 plot.title          = element_text(size   = 20,
											  																	hjust  = 0.5,
											  																	margin = margin(c(5,0,5,0)), 
											  																	color  = "#635E5E",
									  																	    face   = "bold"
											  																	)
											 ) 



# Save plot ----


ggsave(plot =  plot_grid(title_top,
												 plot_mid,
												 title_bottom,
												 plot_bottom,
												 caption,
												 nrow = 5,
												 rel_heights = c(0.1,0.4,0.05,0.4,0.05)
												 ),
			 filename = 'Final_Plot.png',
			 device   = "png" , 
			 width    = 30,
			 height   = 25,
			 dpi      = 300)

