

# Packages ----

library("tidyverse")
library('cowplot')
library("jsonlite")
library("magick")
library("grid")
library("gridExtra")
library("ggpubr")


# Directory 

setwd("C:/Users/maxim/Documents/Site_Web/Git/R-Dataviz/Football/2. Six_Touchmaps")



# Data ----

JSON_Data <-   jsonlite::fromJSON("https://github.com/sonofacorner/soc-viz-of-the-week/raw/main/09052022/data/brighton_leicester_09052022.json")
													 



# Data processing ----


# Infos players

Players <- data.frame(Player_ID   = names(unlist(JSON_Data$playerIdNameDictionary)), 
		                  Player_Name = as.vector(unlist(JSON_Data$playerIdNameDictionary))
											)


# Infos touchmap

df_touchmap          <-  JSON_Data[["events"]] %>% select(playerId, x, y, isTouch)

df_touchmap          <-  df_touchmap %>% filter(isTouch == 'TRUE') %>% 
                                         select(-isTouch)

df_touchmap$playerId <-  as.character(df_touchmap$playerId)


# Join for players names

df_touchmap <-  left_join(df_touchmap,
													Players,
													by = c("playerId"  = "Player_ID")
													)




# Pitch processing

create_Pitch <- function(background_colour){
  
theme_blankPitch = function() { 
												       cowplot::theme_nothing() +
												       theme(panel.background = element_rect(fill = background_colour,
													     colour= NA)
													     )
												       }
 
return(ggplot() + theme_blankPitch())
  
}


# https://github.com/statsbomb/open-data/blob/master/doc/StatsBomb%20Open%20Data%20Specification%20v1.1.pdf

# Defining features along the length

boxEdgeDef  <-   18 
boxEdgeOff  <-  102 
halfwayline <-   60 
sixYardDef  <-    6 
sixYardOff  <-  114 
penSpotDef  <-   12 
penSpotOff  <-  108 
  
# Defining features along the width

boxEdgeLeft   <-  18 
boxEdgeRight  <-  62 
sixYardLeft   <-  30 
sixYardRight  <-  50 
goalPostLeft  <-  36 
goalPostRight <-  44 
CentreSpot    <-  40 




# Defining the circle function

circleFun <- function(center = c(60,40),diameter =1, 	npoints = 100){
								
							    r  = diameter / 2
							    tt = seq(0,
							    	     2 * pi,
							             length.out = npoints
							             )
							
							    x = center[1] + r * cos(tt)
							    y = center[2] + r * sin(tt)
							    
							    return(data.frame(x = x, y = y))
							    
							  }



# Creating the center circle 

Center_Circle <- circleFun(c(60,40),13,npoints = 100)

ymin <-    0 # minimum width
ymax <-   80 # maximum width 
xmin <-    0 # minimum length
xmax <-  120 # maximum length  


 	
# Plots ----
 

## Pitch  ----


png(file   = "Pitch.png",
		type   = "cairo",
		width  = 300,
		height = 300 * 1.5,
		res    = 150
		)

create_Pitch("#53535c") + 
	                coord_flip()  + 
	                scale_x_continuous(expand=c(0,0),limits = c(0, 120)) +
			scale_y_continuous(expand=c(0,0),limits = c(0, 80)) +
			labs(x = NULL, y = NULL) +
	   
	    geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
	    					colour = "#faf4f4",
	    					fill   = NA
	    					) +
			
			# 18 box defensive
			geom_rect(aes(xmin=xmin, xmax=boxEdgeDef, ymin=boxEdgeLeft, ymax=boxEdgeRight), 
			          colour = "white",
				        fill   = NA
								) +
			
                        # 18 box offensive
			geom_rect(aes(xmin=boxEdgeOff, xmax=xmax, ymin=boxEdgeLeft, ymax=boxEdgeRight), 
							  colour = "white",
							  fill   = NA
								) +
								  
			# halway line
			geom_segment(aes(x = halfwayline, y = ymin, xend = halfwayline, yend = ymax),
				           colour = "white"
									 ) +
								  
			# 6 box Defensive
			geom_rect(aes(xmin=xmin, xmax=sixYardDef, ymin=sixYardLeft, ymax=sixYardRight),  
							  colour = "white",
							  fill   = NA
								)  +
								  
			# 6 box offensive
			geom_rect(aes(xmin=sixYardOff, xmax=xmax, ymin=sixYardLeft, ymax=sixYardRight),  
							  colour = "white",
							  fill   = NA
								) +
								  
			# centre circle
		        geom_path(data=Center_Circle, aes(x=x,y=y), 
				              colour = "white"
		        					) +
								   
			# penalty spot left
			geom_point(aes(x = penSpotDef , y = CentreSpot), 
			           colour = "white", 
				         size   = 0.75
								 ) +
								  
			# penalty spot right
			geom_point(aes(x = penSpotOff , y = CentreSpot), 
				         colour = "white", 
			           size   = 0.75
								 ) +
								 
			# centre spot
			geom_point(aes(x = halfwayline , y = CentreSpot), 
							   colour = "white", 
							   size   = 0.75
								 ) +
								  
			# arcs
			annotate("path",
							 x   = 12 + 10 * cos(seq(-0.3*pi, 0.3*pi, length.out = 30)),
							 y   = 40 + 10 * sin(seq(-0.3*pi, 0.3*pi, length.out = 30)),
							 col = "white"
							 ) +
			
			annotate("path",
							 x   = (120-12) - 10 * cos(seq(-0.3*pi, 0.3*pi, length.out = 30)),
							 y   = 40 + 10 * sin(seq(-0.3*pi, 0.3*pi, length.out = 30)),
							 col = "white"
							 ) +
								  
			# goal defensive
			geom_segment(aes(x = xmin, y = goalPostLeft, xend = xmin, yend = goalPostRight),
				     colour = "#f5e107", 
				     size   = 2.25
				     ) +
								  
			# goal offensive
			geom_segment(aes(x = xmax, y = goalPostLeft, xend = xmax, yend = goalPostRight),
				     colour = "#f5e107", 
				     size   = 2.25
				     )


dev.off()




## One player on pitch  ----


# Player selection 


Solly_March_Data <-  df_touchmap %>% filter(Player_Name == "Solly March")


# Color palette object to insert in the ggplot :

colfunc <-  colorRampPalette(c("#53535c","#515161","#61617a","#6f7c9e","#65959e","#61ba99","#5ab878","#81b83e","#97c43b","#b7d437","#cfb92b","#d18726","#d16532"))



# Plot

png(file   = "MarchSolly.png",
		type   = "cairo",
		width  = 450,
		height = 450*1.5,
		res    = 200)


					  
create_Pitch("#53535c") + 
			geom_density_2d_filled(data = Solly_March_Data, aes(x = x *(120/100), y = 80 - (y*80/100)),
											       alpha       = 0.75,
											       contour_var = "ndensity", 
											       breaks      = seq(0.1, 
																	             1, 
																	             length.out = 250)
											       ) + 
	                coord_flip()  + 
	                scale_x_continuous(expand=c(0,0),limits = c(0, 120)) +
			scale_y_continuous(expand=c(0,0),limits = c(0, 80)) +
			labs(x = NULL, y = NULL) +
	   
	    geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
	    					colour = "#faf4f4",
	    					fill   = NA
	    					) +
			
			# 18 box defensive
			geom_rect(aes(xmin=xmin, xmax=boxEdgeDef, ymin=boxEdgeLeft, ymax=boxEdgeRight), 
			          colour = "white",
				        fill   = NA
								) +
			
                        # 18 box offensive
			geom_rect(aes(xmin=boxEdgeOff, xmax=xmax, ymin=boxEdgeLeft, ymax=boxEdgeRight), 
							  colour = "white",
							  fill   = NA
								) +
								  
			# halway line
			geom_segment(aes(x = halfwayline, y = ymin, xend = halfwayline, yend = ymax),
				           colour = "white"
									 ) +
								  
			# 6 box Defensive
			geom_rect(aes(xmin=xmin, xmax=sixYardDef, ymin=sixYardLeft, ymax=sixYardRight),  
							  colour = "white",
							  fill   = NA
								)  +
								  
			# 6 box offensive
			geom_rect(aes(xmin=sixYardOff, xmax=xmax, ymin=sixYardLeft, ymax=sixYardRight),  
							  colour = "white",
							  fill   = NA
								) +
								  
			# centre circle
		        geom_path(data=Center_Circle, aes(x=x,y=y), 
				              colour = "white"
		        					) +
								   
			# penalty spot left
			geom_point(aes(x = penSpotDef , y = CentreSpot), 
			           colour = "white", 
				         size   = 0.75
				        ) +
					  			  
			# penalty spot right
			geom_point(aes(x = penSpotOff , y = CentreSpot), 
				         colour = "white", 
			           size   = 0.75
								 ) +
								 
			# centre spot
			geom_point(aes(x = halfwayline , y = CentreSpot), 
							   colour = "white", 
							   size   = 0.75
							   ) +
								  
			# arcs
			annotate("path",
							 x   = 12 + 10 * cos(seq(-0.3*pi, 0.3*pi, length.out = 30)),
							 y   = 40 + 10 * sin(seq(-0.3*pi, 0.3*pi, length.out = 30)),
							 col = "white"
							 ) +
			
			annotate("path",
							 x   = (120-12) - 10 * cos(seq(-0.3*pi, 0.3*pi, length.out = 30)),
							 y   = 40 + 10 * sin(seq(-0.3*pi, 0.3*pi, length.out = 30)),
							 col = "white"
							 ) +
								  
			# goal defensive
			geom_segment(aes(x = xmin, y = goalPostLeft, xend = xmin, yend = goalPostRight),
							     colour = "#f5e107", 
							     size   = 2.25) +
								  
			# goal offensive
			geom_segment(aes(x = xmax, y = goalPostLeft, xend = xmax, yend = goalPostRight),
							     colour = "#f5e107", 
							     size   = 2.25
							     ) +

			# attacking direction text + arrow
			annotate("segment", 
							 x      = 21, 
							 xend   = 48, 
							 y      = 12.5,
							 yend   = 12.5,
							 colour = "white", 
							 size   = 0.2, 
							 arrow  = arrow(length = unit(0.5,"cm"))
							 ) +
			annotate("text",
							 x     = 35, 
							 y     = 4.5, 
							 label = "Attacking direction", 
							 angle = 90, 
							 size  = 2.65, 
							 color = "white"
							 ) 
	                


dev.off()







## Combine plots adding flags  ----



# Data 

Solly_March_Data         <-  df_touchmap %>% filter(Player_Name == "Solly March")
Pascal_Groß_Data         <-  df_touchmap %>% filter(Player_Name == "Pascal Groß")
Alexis_Mac_Allister_Data <-  df_touchmap %>% filter(Player_Name == "Alexis Mac Allister")
Moises_Caicedo_Data      <-  df_touchmap %>% filter(Player_Name == "Moisés Caicedo")
Leandro_Trossard_Data    <-  df_touchmap %>% filter(Player_Name == "Leandro Trossard")
Enock_Mwepu_Data         <-  df_touchmap %>% filter(Player_Name == "Enock Mwepu")


# Flags

download.file("https://images.fotmob.com/image_resources/logo/teamlogo/eng.png",'England_Flag.png', mode = 'wb')

download.file("https://images.fotmob.com/image_resources/logo/teamlogo/ger.png",'Germany_Flag.png', mode = 'wb')

download.file("https://images.fotmob.com/image_resources/logo/teamlogo/arg.png",'Argentina_Flag.png', mode = 'wb')

download.file("https://images.fotmob.com/image_resources/logo/teamlogo/ecu.png",'Ecuador_Flag.png', mode = 'wb')

download.file("https://images.fotmob.com/image_resources/logo/teamlogo/bel.png",'Belgium_Flag.png', mode = 'wb')

download.file("https://images.fotmob.com/image_resources/logo/teamlogo/zam.png",'Zambia_Flag.png', mode = 'wb')



# Plot

Solly_March_Plot = create_Pitch("#51515f") + 
																						geom_density_2d_filled(data = Solly_March_Data, aes(x = x *(120/100), y = 80 - (y*80/100)),
																																	 alpha       = 0.75,
																					                         contour_var = "ndensity", 
																																	 breaks      = seq(0.1, 
																																	 									1, 
																																	 									length.out = 250)
																																	 ) + 								  
																						# geom_point(data = Solly_March_Data, aes(x = x, y = 100 - y), colour = "red", size = 2) 
														                coord_flip()  + 
																						scale_fill_manual(values = colfunc(250), aesthetics = c("fill","color")) +
														                scale_x_continuous(expand=c(0,0),limits = c(0, 120)) +
																			      scale_y_continuous(expand=c(0,0),limits = c(0, 80)) +
																			      labs(x = NULL, y = NULL) +
														                geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
														                					colour = "white",
														                					fill   = NA
														                					) +
																					  # add the 18 yard box defensive
																					  geom_rect(aes(xmin=xmin, xmax=boxEdgeDef, ymin=boxEdgeLeft, ymax=boxEdgeRight), 
																					  					colour = "white",
																					  					fill   = NA
																					  					) +
																					  # add the 18 yard box offensive
																					  geom_rect(aes(xmin=boxEdgeOff, xmax=xmax, ymin=boxEdgeLeft, ymax=boxEdgeRight), 
																					  					colour = "white",
																					  					fill   = NA
																					  					) +
																					  # add halway line
																					  geom_segment(aes(x = halfwayline, y = ymin, xend = halfwayline, yend = ymax),
																					  						 colour = "white"
																					  						 ) +
																					  # add the six yard box Defensive
																					  geom_rect(aes(xmin=xmin, xmax=sixYardDef, ymin=sixYardLeft, ymax=sixYardRight),  
																					  					colour = "white",
																					  					fill   = NA
																					  					)  +
																					  # add the six yard box offensive
																					  geom_rect(aes(xmin=sixYardOff, xmax=xmax, ymin=sixYardLeft, ymax=sixYardRight),  
																					  					colour = "white",
																					  					fill   = NA
																					  					) +
																					  # add centre circle
																					  geom_path(data=Center_Circle, aes(x=x,y=y), 
																					  					colour = "white"
																					  					) +
																					    # add penalty spot left
																					  geom_point(aes(x = penSpotDef , y = CentreSpot), 
																					  					 colour = "white", 
																					  					 size   = 0.75
																					  					 ) +
																					  # add penalty spot right
																					  geom_point(aes(x = penSpotOff , y = CentreSpot), 
																					  					 colour = "white", 
																					  					 size   = 0.75
																					  					 ) +
																					  # add centre spot
																					  geom_point(aes(x = halfwayline , y = CentreSpot), 
																					  					 colour = "white", 
																					  					 size   = 0.75
																					  					 ) +
																					  # add the arcs
																					  annotate("path",
																					           x = 12 + 10 * cos(seq(-0.3*pi, 0.3*pi, length.out = 30)),
																					           y = 40 + 10 * sin(seq(-0.3*pi, 0.3*pi, length.out = 30)),
																					           col = "white") +
																					  annotate("path",
																					           x = (120-12) - 10 * cos(seq(-0.3*pi, 0.3*pi, length.out = 30)),
																					           y = 40 + 10 * sin(seq(-0.3*pi, 0.3*pi, length.out = 30)),
																					           col = "white"
																					  				 ) +
																					  # add the goal Defensive
																					  geom_segment(aes(x = xmin, y = goalPostLeft, xend = xmin, yend = goalPostRight),
																					  						 colour = "#f5e107", 
																					  						 size   = 2.25
																					  						 ) +
																					  # add the goal offensive
																					  geom_segment(aes(x = xmax, y = goalPostLeft, xend = xmax, yend = goalPostRight),
																					  						 colour = "#f5e107",
																					  						 size   = 2.25
																					  						 ) + 
																						# attacking direction text + arrow
																						annotate("segment", x = 5, xend = 40, y = 12.5, yend = 12.5,
																					           colour = "white", 
																										 size   = 0.2, 
																										 arrow  = arrow(length = unit(0.3,"cm"))
																										 ) +
																						annotate("text", 
																										 x     = 25, 
																										 y     = 4.5, 
																										 label = "Attacking direction", 
																										 angle = 90, 
																										 size  = 2.65, 
																										 color = "white"
																										 )  +
																						theme(plot.background  = element_rect(colour = "#F9F6F5", 
																																									fill   = "#F9F6F5"
																																									),
																							    plot.margin      = unit(c(1.5, 1, 1, 1), "cm")
																									)


Pascal_Groß_Plot = create_Pitch("#51515f") + 
																						geom_density_2d_filled(data = Pascal_Groß_Data, aes(x = x *(120/100), y = 80 - (y*80/100)),
																																	 alpha       = 0.75,
																					                         contour_var = "ndensity", 
																																	 breaks      = seq(0.1, 
																																	 						 1, 
																																	 						 length.out = 250)
																																	 ) + 								  
														                coord_flip()  + 
																						scale_fill_manual(values = colfunc(250), aesthetics = c("fill","color")) +
														                scale_x_continuous(expand=c(0,0),limits = c(0, 120)) +
																			      scale_y_continuous(expand=c(0,0),limits = c(0, 80)) +
																			      labs(x = NULL, y = NULL) +
														                geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
														                					colour = "white",
														                					fill   = NA
														                					) +
																					  # add the 18 yard box defensive
																					  geom_rect(aes(xmin=xmin, xmax=boxEdgeDef, ymin=boxEdgeLeft, ymax=boxEdgeRight), 
																					  					colour = "white",
																					  					fill   = NA
																					  					) +
																					  # add the 18 yard box offensive
																					  geom_rect(aes(xmin=boxEdgeOff, xmax=xmax, ymin=boxEdgeLeft, ymax=boxEdgeRight), 
																					  					colour = "white",
																					  					fill   = NA
																					  					) +
																					  # add halway line
																					  geom_segment(aes(x = halfwayline, y = ymin, xend = halfwayline, yend = ymax),
																					  						 colour = "white"
																					  						 ) +
																					  # add the six yard box Defensive
																					  geom_rect(aes(xmin=xmin, xmax=sixYardDef, ymin=sixYardLeft, ymax=sixYardRight),  
																					  					colour = "white",
																					  					fill   = NA
																					  					)  +
																					  # add the six yard box offensive
																					  geom_rect(aes(xmin=sixYardOff, xmax=xmax, ymin=sixYardLeft, ymax=sixYardRight),  
																					  					colour = "white",
																					  					fill   = NA
																					  					) +
																					  # add centre circle
																					  geom_path(data=Center_Circle, aes(x=x,y=y), 
																					  					colour = "white"
																					  					) +
																					    # add penalty spot left
																					  geom_point(aes(x = penSpotDef , y = CentreSpot), 
																					  					 colour = "white", 
																					  					 size   = 0.75
																					  					 ) +
																					  # add penalty spot right
																					  geom_point(aes(x = penSpotOff , y = CentreSpot), 
																					  					 colour = "white", 
																					  					 size   = 0.75
																					  					 ) +
																					  # add centre spot
																					  geom_point(aes(x = halfwayline , y = CentreSpot), 
																					  					 colour = "white", 
																					  					 size   = 0.75
																					  					 ) +
																					  # add the arcs
																					  annotate("path",
																					           x   = 12 + 10 * cos(seq(-0.3*pi, 0.3*pi, length.out = 30)),
																					           y   = 40 + 10 * sin(seq(-0.3*pi, 0.3*pi, length.out = 30)),
																					           col = "white"
																					  				 ) +
																					  annotate("path",
																					           x   = (120-12) - 10 * cos(seq(-0.3*pi, 0.3*pi, length.out = 30)),
																					           y   = 40 + 10 * sin(seq(-0.3*pi, 0.3*pi, length.out = 30)),
																					           col = "white"
																					  				 ) +
																					  # add the goal Defensive
																					  geom_segment(aes(x = xmin, y = goalPostLeft, xend = xmin, yend = goalPostRight),
																					  						 colour = "#f5e107", 
																					  						 size   = 2.25
																					  						 ) +
																					  # add the goal offensive
																					  geom_segment(aes(x = xmax, y = goalPostLeft, xend = xmax, yend = goalPostRight),
																					  						 colour = "#f5e107", 
																					  						 size = 2.25
																					  						 ) +
																						# margin plot
																						theme(plot.background  = element_rect(colour = "#F9F6F5", fill = "#F9F6F5"),
																							    plot.margin      = unit(c(1.5, 1, 1, 1), "cm")
																									)




Alexis_Mac_Allister_Plot = create_Pitch("#51515f") + 
																						geom_density_2d_filled(data = Alexis_Mac_Allister_Data, aes(x = x *(120/100), y = 80 - (y*80/100)),
																																	 alpha       = 0.75,
																					                         contour_var = "ndensity", 
																																	 breaks      = seq(0.1, 
																																	 						       1, 
																																	 						       length.out = 250)
																																	 ) + 								  
														                coord_flip()  + 
																						scale_fill_manual(values = colfunc(250), aesthetics = c("fill","color")) +
														                scale_x_continuous(expand=c(0,0),limits = c(0, 120)) +
																			      scale_y_continuous(expand=c(0,0),limits = c(0, 80)) +
																			      labs(x = NULL, y = NULL) +
														                geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
														                					colour = "white",
														                					fill   = NA) +
																					  # add the 18 yard box defensive
																					  geom_rect(aes(xmin=xmin, xmax=boxEdgeDef, ymin=boxEdgeLeft, ymax=boxEdgeRight), 
																					  					colour = "white",
																					  					fill   = NA
																					  					) +
																					  # add the 18 yard box offensive
																					  geom_rect(aes(xmin=boxEdgeOff, xmax=xmax, ymin=boxEdgeLeft, ymax=boxEdgeRight), 
																					  					colour = "white",
																					  					fill   = NA
																					  					) +
																					  # add halway line
																					  geom_segment(aes(x = halfwayline, y = ymin, xend = halfwayline, yend = ymax),
																					  						 colour = "white"
																					  						 ) +
																					  # add the six yard box Defensive
																					  geom_rect(aes(xmin=xmin, xmax=sixYardDef, ymin=sixYardLeft, ymax=sixYardRight),  
																					  					colour = "white",
																					  					fill   = NA
																					  					)  +
																					  # add the six yard box offensive
																					  geom_rect(aes(xmin=sixYardOff, xmax=xmax, ymin=sixYardLeft, ymax=sixYardRight),  
																					  					colour = "white",
																					  					fill   = NA
																					  					) +
																					  # add centre circle
																					  geom_path(data=Center_Circle, aes(x=x,y=y), 
																					  					colour = "white"
																					  					) +
																					    # add penalty spot left
																					  geom_point(aes(x = penSpotDef , y = CentreSpot), 
																					  					 colour = "white", 
																					  					 size   = 0.75
																					  					 ) +
																					  # add penalty spot right
																					  geom_point(aes(x = penSpotOff , y = CentreSpot), 
																					  					 colour = "white", 
																					  					 size   = 0.75
																					  					 ) +
																					  # add centre spot
																					  geom_point(aes(x = halfwayline , y = CentreSpot), 
																					  					 colour = "white", 
																					  					 size   = 0.75
																					  					 ) +
																					  # add the arcs
																					  annotate("path",
																					           x   = 12 + 10 * cos(seq(-0.3*pi, 0.3*pi, length.out = 30)),
																					           y   = 40 + 10 * sin(seq(-0.3*pi, 0.3*pi, length.out = 30)),
																					           col = "white"
																					  				 ) +
																					  annotate("path",
																					           x   = (120-12) - 10 * cos(seq(-0.3*pi, 0.3*pi, length.out = 30)),
																					           y   = 40 + 10 * sin(seq(-0.3*pi, 0.3*pi, length.out = 30)),
																					           col = "white"
																					  				 ) +
																					  # add the goal Defensive
																					  geom_segment(aes(x = xmin, y = goalPostLeft, xend = xmin, yend = goalPostRight),
																					  						 colour = "#f5e107", 
																					  						 size   = 2.25
																					  						 ) +
																					  # add the goal offensive
																					  geom_segment(aes(x = xmax, y = goalPostLeft, xend = xmax, yend = goalPostRight),
																					  						 colour = "#f5e107", 
																					  						 size   = 2.25
																					  						 ) +
																						# margin plot
																						theme(plot.background  = element_rect(colour = "#F9F6F5", fill = "#F9F6F5"),
																							    plot.margin      = unit(c(1.5, 1, 1, 1), "cm")
																									)




Moises_Caicedo_Plot = create_Pitch("#51515f") + 
																						geom_density_2d_filled(data = Moises_Caicedo_Data, aes(x = x *(120/100), y = 80 - (y*80/100)),
																																	 alpha       = 0.75,
																					                         contour_var = "ndensity", 
																																	 breaks      = seq(0.1, 
																																	 						       1, 
																																	 						       length.out = 250)
																																	 ) + 							
														                coord_flip()  + 
																						scale_fill_manual(values = colfunc(250), aesthetics = c("fill","color")) +
														                scale_x_continuous(expand=c(0,0),limits = c(0, 120)) +
																			      scale_y_continuous(expand=c(0,0),limits = c(0, 80)) +
																			      labs(x = NULL, y = NULL) +
														                geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
														                					colour = "white",
														                					fill   = NA
														                					) +
																					  # add the 18 yard box defensive
																					  geom_rect(aes(xmin=xmin, xmax=boxEdgeDef, ymin=boxEdgeLeft, ymax=boxEdgeRight), 
																					  					colour = "white",
																					  					fill   = NA
																					  					) +
																					  # add the 18 yard box offensive
																					  geom_rect(aes(xmin=boxEdgeOff, xmax=xmax, ymin=boxEdgeLeft, ymax=boxEdgeRight), 
																					  					colour = "white",
																					  					fill   = NA
																					  					) +
																					  # add halway line
																					  geom_segment(aes(x = halfwayline, y = ymin, xend = halfwayline, yend = ymax),
																					  						 colour = "white"
																					  						 ) +
																					  # add the six yard box Defensive
																					  geom_rect(aes(xmin=xmin, xmax=sixYardDef, ymin=sixYardLeft, ymax=sixYardRight),  
																					  					colour = "white",
																					  					fill   = NA
																					  					)  +
																					  # add the six yard box offensive
																					  geom_rect(aes(xmin=sixYardOff, xmax=xmax, ymin=sixYardLeft, ymax=sixYardRight),  
																					  					colour = "white",
																					  					fill   = NA
																					  					) +
																					  # add centre circle
																					  geom_path(data = Center_Circle, aes(x=x,y=y), 
																					  					colour = "white"
																					  					) +
																					    # add penalty spot left
																					  geom_point(aes(x = penSpotDef , y = CentreSpot), 
																					  					 colour = "white", 
																					  					 size   = 0.75
																					  					 ) +
																					  # add penalty spot right
																					  geom_point(aes(x = penSpotOff , y = CentreSpot), 
																					  					 colour = "white", 
																					  					 size   = 0.75
																					  					 ) +
																					  # add centre spot
																					  geom_point(aes(x = halfwayline , y = CentreSpot), 
																					  					 colour = "white", 
																					  					 size   = 0.75
																					  					 ) +
																					  # add the arcs
																					  annotate("path",
																					           x   = 12 + 10 * cos(seq(-0.3*pi, 0.3*pi, length.out = 30)),
																					           y   = 40 + 10 * sin(seq(-0.3*pi, 0.3*pi, length.out = 30)),
																					           col = "white"
																					  				 ) +
																					  annotate("path",
																					           x   = (120-12) - 10 * cos(seq(-0.3*pi, 0.3*pi, length.out = 30)),
																					           y   = 40 + 10 * sin(seq(-0.3*pi, 0.3*pi, length.out = 30)),
																					           col = "white"
																					  				 ) +
																					  # add the goal Defensive
																					  geom_segment(aes(x = xmin, y = goalPostLeft, xend = xmin, yend = goalPostRight),
																					  						 colour = "#f5e107", 
																					  						 size   = 2.25
																					  						 ) +
																					  # add the goal offensive
																					  geom_segment(aes(x = xmax, y = goalPostLeft, xend = xmax, yend = goalPostRight),
																					  						 colour = "#f5e107", 
																					  						 size   = 2.25
																					  						 ) + 
	                                          # margin plot
																						theme(plot.background  = element_rect(colour = "#F9F6F5", fill = "#F9F6F5"),
																							    plot.margin      = unit(c(1.5, 1, 1, 1), "cm")
																									)




Leandro_Trossard_Plot = create_Pitch("#51515f") + 
																						geom_density_2d_filled(data = Leandro_Trossard_Data, aes(x = x *(120/100), y = 80 - (y*80/100)),
																																	 alpha       = 0.75,
																					                         contour_var = "ndensity", 
																																	 breaks      = seq(0.1, 
																																	 						       1, 
																																	 						       length.out = 250)
																																	 ) + 								  
														                coord_flip()  + 
																						scale_fill_manual(values = colfunc(250), aesthetics = c("fill","color")) +
														                scale_x_continuous(expand=c(0,0),limits = c(0, 120)) +
																			      scale_y_continuous(expand=c(0,0),limits = c(0, 80)) +
																			      labs(x = NULL, y = NULL) +
														                geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
														                					colour = "white",
														                					fill   = NA 
														                					) +
																					  # add the 18 yard box defensive
																					  geom_rect(aes(xmin=xmin, xmax=boxEdgeDef, ymin=boxEdgeLeft, ymax=boxEdgeRight), 
																					  					colour = "white",
																					  					fill   = NA
																					  					) +
																					  # add the 18 yard box offensive
																					  geom_rect(aes(xmin=boxEdgeOff, xmax=xmax, ymin=boxEdgeLeft, ymax=boxEdgeRight), 
																					  					colour = "white",
																					  					fill   = NA
																					  					) +
																					  # add halway line
																					  geom_segment(aes(x = halfwayline, y = ymin, xend = halfwayline, yend = ymax),
																					  						 colour = "white"
																					  						 ) +
																					  # add the six yard box Defensive
																					  geom_rect(aes(xmin=xmin, xmax=sixYardDef, ymin=sixYardLeft, ymax=sixYardRight),  
																					  					colour = "white",
																					  					fill   = NA
																					  					)  +
																					  # add the six yard box offensive
																					  geom_rect(aes(xmin=sixYardOff, xmax=xmax, ymin=sixYardLeft, ymax=sixYardRight),  
																					  					colour = "white",
																					  					fill   = NA
																					  					) +
																					  # add centre circle
																					  geom_path(data=Center_Circle, aes(x=x,y=y), 
																					  					colour = "white"
																					  					) +
																					    # add penalty spot left
																					  geom_point(aes(x = penSpotDef , y = CentreSpot), 
																					  					 colour = "white", 
																					  					 size   = 0.75
																					  					 ) +
																					  # add penalty spot right
																					  geom_point(aes(x = penSpotOff , y = CentreSpot), 
																					  					 colour = "white", 
																					  					 size   = 0.75
																					  					 ) +
																					  # add centre spot
																					  geom_point(aes(x = halfwayline , y = CentreSpot), 
																					  					 colour = "white", 
																					  					 size   = 0.75
																					  					 ) +
																					  # add the arcs
																					  annotate("path",
																					           x   = 12 + 10 * cos(seq(-0.3*pi, 0.3*pi, length.out = 30)),
																					           y   = 40 + 10 * sin(seq(-0.3*pi, 0.3*pi, length.out = 30)),
																					           col = "white"
																					  				 ) +
																					  annotate("path",
																					           x   = (120-12) - 10 * cos(seq(-0.3*pi, 0.3*pi, length.out = 30)),
																					           y   = 40 + 10 * sin(seq(-0.3*pi, 0.3*pi, length.out = 30)),
																					           col = "white"
																					  				 ) +
																					  # add the goal Defensive
																					  geom_segment(aes(x = xmin, y = goalPostLeft, xend = xmin, yend = goalPostRight),
																					  						 colour = "#f5e107", 
																					  						 size   = 2.25
																					  						 ) +
																					  # add the goal offensive
																					  geom_segment(aes(x = xmax, y = goalPostLeft, xend = xmax, yend = goalPostRight),
																					  						 colour = "#f5e107", 
																					  						 size   = 2.25
																					  						 ) +  
	                                          # margin plot
																						theme(plot.background  = element_rect(colour = "#F9F6F5", fill = "#F9F6F5"),
																							    plot.margin      = unit(c(1.5, 1, 1, 1), "cm")
																									)





Enock_Mwepu_Plot = create_Pitch("#51515f") + 
																						geom_density_2d_filled(data = Enock_Mwepu_Data, aes(x = x *(120/100), y = 80 - (y*80/100)),
																																	 alpha       = 0.75,
																					                         contour_var = "ndensity", 
																																	 breaks      = seq(0.1, 
																																	 						       1, 
																																	 						       length.out = 250)
																																	 ) + 								   
														                coord_flip()  + 
																						scale_fill_manual(values = colfunc(250), aesthetics = c("fill","color")) +
														                scale_x_continuous(expand=c(0,0),limits = c(0, 120)) +
																			      scale_y_continuous(expand=c(0,0),limits = c(0, 80)) +
																			      labs(x = NULL, y = NULL) +
														                geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
														                					colour = "white",
														                					fill   = NA
														                					) +
																					  # add the 18 yard box defensive
																					  geom_rect(aes(xmin=xmin, xmax=boxEdgeDef, ymin=boxEdgeLeft, ymax=boxEdgeRight), 
																					  					colour = "white",
																					  					fill   = NA
																					  					) +
																					  # add the 18 yard box offensive
																					  geom_rect(aes(xmin=boxEdgeOff, xmax=xmax, ymin=boxEdgeLeft, ymax=boxEdgeRight), 
																					  					colour = "white",
																					  					fill   = NA
																					  					) +
																					  # add halway line
																					  geom_segment(aes(x = halfwayline, y = ymin, xend = halfwayline, yend = ymax),
																					  						 colour = "white"
																					  						 ) +
																					  # add the six yard box Defensive
																					  geom_rect(aes(xmin=xmin, xmax=sixYardDef, ymin=sixYardLeft, ymax=sixYardRight),  
																					  					colour = "white",
																					  					fill   = NA
																					  					)  +
																					  # add the six yard box offensive
																					  geom_rect(aes(xmin=sixYardOff, xmax=xmax, ymin=sixYardLeft, ymax=sixYardRight),  
																					  					colour = "white",
																					  					fill   = NA
																					  					) +
																					  # add centre circle
																					  geom_path(data=Center_Circle, aes(x=x,y=y), 
																					  					colour = "white"
																					  					) +
																					    # add penalty spot left
																					  geom_point(aes(x = penSpotDef , y = CentreSpot), 
																					  					 colour = "white", 
																					  					 size   = 0.75
																					  					 ) +
																					  # add penalty spot right
																					  geom_point(aes(x = penSpotOff , y = CentreSpot), 
																					  					 colour = "white", 
																					  					 size   = 0.75
																					  					 ) +
																					  # add centre spot
																					  geom_point(aes(x = halfwayline , y = CentreSpot), 
																					  					 colour = "white", 
																					  					 size   = 0.75
																					  					 ) +
																					  # add the arcs
																					  annotate("path",
																					           x   = 12 + 10 * cos(seq(-0.3*pi, 0.3*pi, length.out = 30)),
																					           y   = 40 + 10 * sin(seq(-0.3*pi, 0.3*pi, length.out = 30)),
																					           col = "white"
																					  				 ) +
																					  annotate("path",
																					           x   = (120-12) - 10 * cos(seq(-0.3*pi, 0.3*pi, length.out = 30)),
																					           y   = 40 + 10 * sin(seq(-0.3*pi, 0.3*pi, length.out = 30)),
																					           col = "white"
																					  				 ) +
																					  # add the goal Defensive
																					  geom_segment(aes(x = xmin, y = goalPostLeft, xend = xmin, yend = goalPostRight),
																					  						 colour = "#f5e107", 
																					  						 size   = 2.25
																					  						 ) +
																					  # add the goal offensive
																					  geom_segment(aes(x = xmax, y = goalPostLeft, xend = xmax, yend = goalPostRight),
																					  						 colour = "#f5e107", 
																					  						 size   = 2.25
																					  						 ) + 
	                                          # margin plot
																						theme(plot.background  = element_rect(colour = "#F9F6F5", fill = "#F9F6F5"),
																							    plot.margin      = unit(c(1.5, 1, 1, 1), "cm")
																									)


# title and subtitle plot

tg     <- textGrob("Touchmap for Brighton's Midfielders vs. Leicester", hjust = 0.57, gp = gpar(fontsize = 18, fontface = 'bold'))
sg     <- textGrob("Touches per player in the fixture against Leicester City. Largely inspired by @sonofacorner viz \n\n", hjust = 0.505, gp = gpar(fontsize = 12,col="gray52"))
margin <- unit(1, "line")



# Combinate plots

grided <-  gridExtra::grid.arrange(Solly_March_Plot,
																	 Pascal_Groß_Plot,
																	 Alexis_Mac_Allister_Plot,
																	 Moises_Caicedo_Plot,
																	 Leandro_Trossard_Plot,
																	 Enock_Mwepu_Plot,
																	 ncol = 3
																	 )

png(file   = "Plots_6.png", 
		type   = "cairo",
    width  = 1500, 
		height = 1650,
		bg     = "#F9F6F5",
    res    = 200)

gridExtra::grid.arrange(tg, sg, grided,
                        heights = unit.c(grobHeight(tg) + 1.2*margin,
                                         grobHeight(sg) + margin,
                                         unit(1,"null")
                        								 )
												)


dev.off()




# Add flags and title/subtile on top of each pitch


img = magick::image_read("Plots_6.png") %>%
	# flags position
	magick::image_composite(image_scale(magick::image_read("England_Flag.png"),  "x35"),  offset = "+380+270") %>%
	magick::image_composite(image_scale(magick::image_read("Germany_Flag.png"),  "x35"),  offset = "+880+270") %>%
	magick::image_composite(image_scale(magick::image_read("Argentina_Flag.png"),"x35"),offset = "+1380+270") %>%
	magick::image_composite(image_scale(magick::image_read("Ecuador_Flag.png"),  "x35"),  offset = "+380+975") %>%
	magick::image_composite(image_scale(magick::image_read("Belgium_Flag.png"),  "x35"),  offset = "+880+975") %>%
	magick::image_composite(image_scale(magick::image_read("Zambia_Flag.png"),   "x35"),   offset = "+1380+975") %>%
	# titles 
	image_annotate("Solly March", 
								 size     = 30,
								 weight   = 700, 
								 color    = "black",
								 font     = "sans",
								 location = "+80+270"
								 ) %>%
	image_annotate("Pascal Groß", 
								 size     = 30,
								 weight   = 700, 
								 color    = "black",
								 font     = "sans",
								 location = "+580+270"
								 ) %>%
	image_annotate("Alexis Mac Allister", 
								 size     = 30,
								 weight   = 700, 
								 color    = "black",
								 font     = "sans",
								 location = "+1080+270"
								 ) %>%
	image_annotate("Moisés Caicedo", 
								 size     = 30,
								 weight   = 700, 
								 color    = "black",
								 font     = "sans",
								 location = "+80+975"
								 ) %>%
	image_annotate("Leandro Trossard", 
								 size     = 30,
								 weight   = 700, 
								 color    = "black",
								 font     = "sans",
								 location = "+580+975"
								 ) %>%
	image_annotate("Enock Mwepu", 
								 size     = 30,
								 weight   = 700, 
								 color    = "black",
								 font     = "sans",
								 location = "+1080+975"
								 ) %>%
	#subtitles
	image_annotate("touches 55 | final third : 73%", 
								 size     = 23,
								 weight   = 500, 
								 color    = "#6f6767",
								 font     = "sans",
								 location = "+80+310"
								 ) %>%
	image_annotate("touches 71 | final third : 61%", 
								 size     = 23,
								 weight   = 500, 
								 color    = "#6f6767",
								 font     = "sans",
								 location = "+580+310"
								 ) %>%
	image_annotate("touches 58 | final third : 24%",  
								 size     = 23,
								 weight   = 500,
								 color    = "#6f6767",
								 font     = "sans",
								 location = "+1080+310"
								 ) %>%
	image_annotate("touches 53 | final third : 47%",  
								 size     = 23,
								 weight   = 500,
								 color    = "#6f6767",
								 font     = "sans",
								 location = "+80+1015"
								 ) %>%
	image_annotate("touches 69 | final third : 51%",  
								 size     = 23,
								 weight   = 500,
								 color    = "#6f6767",
								 font     = "sans",
								 location = "+580+1015"
								 ) %>%
	image_annotate("touches 39 | final third : 62%",  
								 size     = 23,
								 weight   = 500,
								 color    = "#6f6767",
								 font     = "sans",
								 location = "+1080+1015"
								 ) 
	

# Save

magick::image_write(img, "Final_Plot.png")


