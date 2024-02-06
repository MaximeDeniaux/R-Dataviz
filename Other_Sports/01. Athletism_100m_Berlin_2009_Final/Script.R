



# Packages ----

library(tidyverse)
library(gganimate)
library(ggplot2)
library(ggimage)
library(ggtext)
library(readr)
library(gifski)
library(cropcircles)



# Directory

setwd("C:/Users/maxim/Documents/Site_Web/Git/R-Dataviz/Other_Sports/1. Athletism_100m_Berlin_2009_Final")



# Data ----

# For my part I have a 14th column of NA which is created (I don't know why) so I remove it

Data <- read_delim("~/Projets_Perso_R_HTML/Site_Web/Posts/Other_Themes/1_Athletism_100m_Berlin_2009_Final/Data.txt", 
									 delim         = ";", 
									 escape_double = FALSE, 
									 trim_ws       = TRUE, 
									 col_select    = 1:13
									 )




# Data processing ----

Data <- Data %>% pivot_longer(cols      = 3:13, 
															names_to  = "Step_meters",
															values_to = "Chrono")

Data <- Data %>%  mutate(Step_meters = case_when(Step_meters == "chrono_0_m"   ~ 0,
																								 Step_meters == "chrono_10_m"  ~ 10,
																								 Step_meters == "chrono_20_m"  ~ 20,
																								 Step_meters == "chrono_30_m"  ~ 30,
																								 Step_meters == "chrono_40_m"  ~ 40,
																								 Step_meters == "chrono_50_m"  ~ 50,
																								 Step_meters == "chrono_60_m"  ~ 60,
																								 Step_meters == "chrono_70_m"  ~ 70,
																								 Step_meters == "chrono_80_m"  ~ 80,
																								 Step_meters == "chrono_90_m"  ~ 90,
																								 Step_meters == "chrono_100_m" ~ 100)
												 )


Data_plot <- data.frame(Name = c(rep(unique(Data$Name)[1],12),
																 rep(unique(Data$Name)[2],12),
																 rep(unique(Data$Name)[3],12),
															   rep(unique(Data$Name)[4],12),
																 rep(unique(Data$Name)[5],12),
																 rep(unique(Data$Name)[6],12),
																 rep(unique(Data$Name)[7],12),
																 rep(unique(Data$Name)[8],12)
															   ),
												
												Country = c(rep(unique(Data$Country)[1],12),
																		rep(unique(Data$Country)[2],12),
																		rep(unique(Data$Country)[3],12),
																		rep(unique(Data$Country)[4],12),
																		rep(unique(Data$Country)[5],12),
																		rep(unique(Data$Country)[4],12),
																		rep(unique(Data$Country)[5],12),
																		rep(unique(Data$Country)[2],12)
																		),
												
											  Time = rep(seq(0,11,1),8)
												)


Data_plot$Time[Data_plot$Time == 11] <- 10.34 
Data_plot$Meters                     <- NA



# The goal here is to know how many meters were covered in each passing time
# Because in the animated graphic, it will be animated through the Time variable.

j <- 1
for(i in 1:nrow(Data_plot)){
	
	if(Data_plot$Time[i] == 0 & Data_plot$Name[i] != "Dwain CHAMBERS"){j = j - 1}	
	if(Data_plot$Time[i] == 0){Data_plot$Meters[i] = 0}
	if(Data_plot$Time[i] == 10 & Data_plot$Name[i] %in% c("Dwain CHAMBERS","Marc BURNS")){Data_plot$Meters[i] = 100}
	if(Data_plot$Time[i] == 10 & !Data_plot$Name[i] %in% c("Dwain CHAMBERS","Marc BURNS","Darvis PATTON")){Data_plot$Meters[i] = 105}
	if(Data_plot$Time[i] == 10.34 & Data_plot$Name[i] != "Darvis PATTON"){Data_plot$Meters[i] = 105}
	if(Data_plot$Time[i] == 10.34 & Data_plot$Name[i] == "Darvis PATTON"){Data_plot$Meters[i] = 100}
	if(Data_plot$Time[i] == 10 & Data_plot$Name[i] == "Darvis PATTON"){Data_plot$Meters[i] = Data$Step_meters[j-1] + ( (Data_plot$Time[i]-Data$Chrono[j-1]) * 10 / (Data$Chrono[j] - Data$Chrono[j-1]) )}
	if(Data_plot$Time[i] %in% c(1:9)){Data_plot$Meters[i] = Data$Step_meters[j-1] + ( (Data_plot$Time[i]-Data$Chrono[j-1]) * 10 / (Data$Chrono[j] - Data$Chrono[j-1]) )}
  j = j + 1
  i = i + 1
  
}	

# End position for everybody (alternative at end_pause in animate function at the end of the script)

Data_plot[97:104,]   <- Data_plot[seq(12,96,12),]
Data_plot[97:104,3]  <- rep(11,8)
Data_plot[97:104,4]  <- rep(105,8)
Data_plot[105:112,]  <- Data_plot[seq(12,96,12),]
Data_plot[105:112,3] <- rep(12,8)
Data_plot[105:112,4] <- rep(105,8)
Data_plot[113:120,]  <- Data_plot[seq(12,96,12),]
Data_plot[113:120,3] <- rep(13,8)
Data_plot[113:120,4] <- rep(105,8) 


# Add images to the dataframe

Data_plot <- Data_plot %>% mutate(Country_flag = case_when(Country == "GBR" ~ "GBR.png",
																													 Country == "TTO" ~ "TTO.png",
																													 Country == "ANT" ~ "ANT.png",
																													 Country == "USA" ~ "USA.png",
																													 Country == "JAM" ~ "JAM.png"),
																	
																	Name_img    = case_when(Name == "Dwain CHAMBERS"   ~ circle_crop("Dwain CHAMBERS.png",   border_size = 3.75, border_colour = "#252525"),
																													Name == "Marc BURNS"       ~ circle_crop("Marc BURNS.png",       border_size = 3.75, border_colour = "#252525"),
																													Name == "Daniel BAILEY"    ~ circle_crop("Daniel BAILEY.png",    border_size = 3.75, border_colour = "#252525"),
																													Name == "Usain BOLT"       ~ circle_crop("Usain BOLT.png",       border_size = 3.75, border_colour = "#252525"),
																													Name == "Tyson GAY"        ~ circle_crop("Tyson GAY.png",        border_size = 3.75, border_colour = "#252525"),
																													Name == "Asafa POWELL"     ~ circle_crop("Asafa POWELL.png",     border_size = 3.75, border_colour = "#252525"),
																													Name == "Darvis PATTON"    ~ circle_crop("Darvis PATTON.png",    border_size = 3.75, border_colour = "#252525"),
																													Name == "Richard THOMPSON" ~ circle_crop("Richard THOMPSON.png", border_size = 3.75, border_colour = "#252525")
																													),
																	
																	Body_img    = case_when(Time == 0                            ~ "Start_Pos.png",
																													Meters == 105 & Name == "Usain BOLT" ~ "Winner_Pos.png",
																													Time == 10 & Meters == 100           ~ "Run_Pos_Mid.png",
																													Time >= 10 & Meters > 100            ~ "Final_Pos.png",
																													Time %in% seq(1,9,2)                 ~ "Run_Pos_Ext.png",
																													T                                    ~ "Run_Pos_Mid.png"
																													)
																	)


Data_plot$Name <- factor(Data_plot$Name,
												 levels = c('Richard THOMPSON','Darvis PATTON','Asafa POWELL','Tyson GAY','Usain BOLT','Daniel BAILEY','Marc BURNS','Dwain CHAMBERS'))

Data_plot <- Data_plot %>% mutate(Name_num = case_when(Name == "Dwain CHAMBERS"   ~ 8,
																											 Name == "Marc BURNS"       ~ 7,
																											 Name == "Daniel BAILEY"    ~ 6,
																											 Name == "Usain BOLT"       ~ 5,
																											 Name == "Tyson GAY"        ~ 4,
																											 Name == "Asafa POWELL"     ~ 3,
																											 Name == "Darvis PATTON"    ~ 2,
																											 Name == "Richard THOMPSON" ~ 1),
																	
																	Name_img_pos_x =  ifelse(Body_img == "Start_Pos.png",Name_num - 0.15,Name_num + 1),
																	Name_img_pos_y =  ifelse(Body_img == "Start_Pos.png",Meters + 2,
																													 ifelse(Meters >= 100, Meters + 0.2,Meters + 0.8)
																													 ) 
																	)





# Trick on data position to have more space between sprinters

for(i in 1:nrow(Data_plot)){
	
	if(Data_plot$Name_num[i] == 8){
		Data_plot$Name_num[i] = 29
		Data_plot$Name_img_pos_x[i] = Data_plot$Name_img_pos_x[i] + 21}
		
	if(Data_plot$Name_num[i] == 7){
		Data_plot$Name_num[i] = 25
		Data_plot$Name_img_pos_x[i] = Data_plot$Name_img_pos_x[i] + 18}
		
	if(Data_plot$Name_num[i] == 6){
		Data_plot$Name_num[i] = 21
		Data_plot$Name_img_pos_x[i] = Data_plot$Name_img_pos_x[i] + 15}
	
	if(Data_plot$Name_num[i] == 5){
		Data_plot$Name_num[i] = 17
		Data_plot$Name_img_pos_x[i] = Data_plot$Name_img_pos_x[i] + 12}
		
	if(Data_plot$Name_num[i] == 4){
		Data_plot$Name_num[i] = 13
		Data_plot$Name_img_pos_x[i] = Data_plot$Name_img_pos_x[i] + 9}
		
	if(Data_plot$Name_num[i] == 3){
		Data_plot$Name_num[i] = 9
		Data_plot$Name_img_pos_x[i] = Data_plot$Name_img_pos_x[i] + 6}
	
  if(Data_plot$Name_num[i] == 2){
		Data_plot$Name_num[i] = 5
		Data_plot$Name_img_pos_x[i] = Data_plot$Name_img_pos_x[i] + 3}
}


# Add new data for a better gif (start and end)

Data_plot[121:128,]   <- Data_plot[seq(1,85,12),]
Data_plot[121:128,3]  <- rep(-2,8)
Data_plot[121:128,4]  <- rep(-9,8)
Data_plot[121:128,6]  <- Data_plot[113:120,6]
Data_plot[121:128,7]  <- rep("Final_Pos.png",8)
Data_plot[121:128,9]  <- rev(seq(2,30,4))
Data_plot[121:128,10] <- rep(-9,8)

Data_plot[129:136,]   <- Data_plot[seq(1,85,12),]
Data_plot[129:136,3]  <- rep(-1,8)
Data_plot[129:136,4]  <- rep(-2.2,8)
Data_plot[129:136,6]  <- Data_plot[113:120,6]
Data_plot[129:136,7]  <- rep("Start_Pos.png",8)
Data_plot[129:136,9]  <- Data_plot[129:136,9] + 0.35
Data_plot[129:136,10] <- rep(0.5,8)


Start <-
  tibble(
    Time     = c(-2, -1, 0, 0.9),
    Meters   = c(5,  -2.5,-3,-3),
    label    = c("On your marks", "Set","Pan !","Pan !"),
    Body_img = c("Referee.png", "Referee.png", "Referee.png","Referee.png")
  )

Data_plot <- bind_rows(Data_plot,Start) %>% mutate(label = if_else(is.na(label), "", label))


for(i in 1:(nrow(Data_plot)-4)){
	if(Data_plot$Time[i] == 0){Data_plot[i,c(4,8:10)] = Data_plot[which(Data_plot$Name == Data_plot$Name[i] & Data_plot$Time == -1),c(4,8:10)] }
}




# Triangle lanes

df_triangles <- data.frame(x = seq(1,29,4), y = rep(-15,8),img = rep("triangle.png",8), label = rev(1:8))


# Starting blocks lanes

df_starting_blocks <- data.frame(x = seq(1,29,4)-1.25, y = rep(-4.9,8),img = rep("Starting_Block.png",8))
df_starting_blocks <- df_starting_blocks %>% bind_rows(data.frame(x = seq(1,29,4)-1.3, y = rep(-2.45,8),img = rep("Starting_Block.png",8)))


# Initially, I wanted to add in each lane, the time for each sprinter arrival. 
# But, for doing this, I had to use {(frame_along)}, but it can only be used in elements included in labs(). 
# So title, subtitle, caption, tag, x, y.
# It is therefore impossible for these elements to have a position in each corridor. Tag yes... but one. I need 8....




# Plot  ----




plot_anim <- ggplot() +
											# Main vertical white lines
											  annotate('segment',
											  				 x         = rep(-5,11),
											  				 y         = seq(0,100,10),
											  				 xend      = rep(31.2,11), 
											  				 yend      = seq(0,100,10),
											  				 color     = "white", 
											  				 linetype  = 'dashed',
											  				 linewidth = 0.75
											  				 ) +
	
											# Main horizontal white lines
											  annotate('segment',
											  				 x         = seq(-1,31,4),
											  				 y         = rep(-20,9),
											  				 xend      = seq(-1,31,4), 
											  				 yend      = rep(125,9),
											  				 color     = "white",
											  				 linewidth = 1.4
											  				 ) +
	
										 	# Triangles lane
											  geom_image(data = df_triangles, aes(x     = x,
											  																		y     = y,
											  																		image = img
											  																		),
											  					 size = 0.04
											  					 ) +
	
											# Starting blocks
											  geom_image(data = df_starting_blocks, aes(x     = x,
											  																					y     = y,
											  																					image = img
											  																					),
											  					 size = 0.035
											  					 ) +
	
										 	# Referee
											  geom_image(data = Data_plot %>% filter(label != ""), aes(x     = 33.5,
											  																												 y     = -10.5,
											  																												 image = Body_img
											  																												 ),
											  					 size = 0.10
											  					 ) +
	
										  # Starting words
										    geom_text(data = Data_plot, aes(34.5, 
										    																Meters, 
										    																label = label
										    																),
										              color    = "black",
										              size     = 8.5,
										              fontface = "bold"
										    					) +
	
										  # Lanes num
										    geom_text(data = df_triangles,aes(x     = x - 0.1,
										    																	y     = y + 0.1, 
										    																	label = label
										    																	),
										              color    = "black",
										              hjust    = 0.5,
										              size     = 4.25,
										              fontface = "bold"
										    					) +
	
										  # Images body
										    geom_image(data = Data_plot,aes(Name_num + 0.20, 
										    																Meters, 
										    																image = Body_img,
										    																group = Name_num
										    																), 
										    					 size = 0.11
										    					 ) +
	
										  # Images face
										 	  geom_image(data = Data_plot,aes(Name_img_pos_x + 0.60, 
										 	  																Name_img_pos_y, 
										 	  																image = Name_img,
										 	  																group = Name_num
										 	  																), 
										 	  					 size = 0.053
										 	  					 ) +
	
											# Scales
										    scale_x_continuous(expand = c(0.01, 0.01),
										    									 limits = c(-5, 35),
										    									 breaks = seq(1, 29, 4),
										    									 labels = c("Richard THOMPSON <img src='C:/Users/maxim/Documents/Projets_Perso_R_HTML/Site_Web/Posts/Other_Themes/1_Athletism_100m_Berlin_2009_Final/TTO.png' width='18'/>",
										    									 					  "Darvis PATTON <img src='C:/Users/maxim/Documents/Projets_Perso_R_HTML/Site_Web/Posts/Other_Themes/1_Athletism_100m_Berlin_2009_Final/USA.png' width='18'/>",
										    									 					  "Asafa POWELL <img src='C:/Users/maxim/Documents/Projets_Perso_R_HTML/Site_Web/Posts/Other_Themes/1_Athletism_100m_Berlin_2009_Final/JAM.png' width='18'/>",
										    									 					  "Tyson GAY <img src='C:/Users/maxim/Documents/Projets_Perso_R_HTML/Site_Web/Posts/Other_Themes/1_Athletism_100m_Berlin_2009_Final/USA.png' width='18'/>",
										    									 					  "Usain BOLT <img src='C:/Users/maxim/Documents/Projets_Perso_R_HTML/Site_Web/Posts/Other_Themes/1_Athletism_100m_Berlin_2009_Final/JAM.png' width='18'/>",
										    									 					  "Daniel BAILEY <img src='C:/Users/maxim/Documents/Projets_Perso_R_HTML/Site_Web/Posts/Other_Themes/1_Athletism_100m_Berlin_2009_Final/ANT.png' width='18'/>",
										    									 					  "Marc BURNS <img src='C:/Users/maxim/Documents/Projets_Perso_R_HTML/Site_Web/Posts/Other_Themes/1_Athletism_100m_Berlin_2009_Final/TTO.png' width='18'/>",
										    									 					  "Dwain CHAMBERS <img src='C:/Users/maxim/Documents/Projets_Perso_R_HTML/Site_Web/Posts/Other_Themes/1_Athletism_100m_Berlin_2009_Final/GBR.png' width='18'/>"
										    									 					 )
										    									 ) +
	
										    scale_y_continuous(limits = c(-20, 125),
										                       expand = c(0.001, 0.001),
										                       breaks = seq(0, 100, by = 10),
										                       labels = paste0(as.character(seq(0, 100, by = 10))," m")
										    									 ) +
	
											# Orientation
										    coord_flip() + 
	
	                    # Theme
										    theme(axis.ticks.x      = element_blank(),
										          axis.text.x       = element_markdown(size  = 15, 
										          																	   color = "#36373B",
										          																	   vjust = 0
										          																	   ),
										          axis.ticks.y      = element_blank(),
										          axis.text.y       = element_markdown(size   = 15, 
										          																	   color  = "#36373B",
										          																	   margin = margin(0,5,0,0)
										          																	   ),
										          plot.title        = element_text(size   = 26, 
												          													   color  = "#E58C33",
												          													   hjust  = 0.4, 
												          													   margin = margin(10,0,10,0)
												          													   ),
										          plot.subtitle     = element_markdown(size       = 17,
													                                         lineheight = 1.2,
													          													 	   hjust      = 0.4, 
													          												   	   margin     = margin(10,0,60,0)
													          													 	   ),
										          plot.caption      = element_text(size   = 14.5,
											                                         color  = "#74757A",
											          														   hjust  = 0.4,
											          														   margin = margin(30,0,0,0)
										          															 ),
										    			plot.tag.position = c(0.93,0.83),
										    			plot.tag          = element_text(hjust = 0,
										    																			 color = "white",
										    																			 face  = "bold",
										    																			 size  = 18),
										          panel.border      = element_blank(),
										          panel.grid.major  = element_blank(),
										    			panel.grid.minor  = element_blank(),
										          plot.margin       = margin(40, 40, 30, 40),
										    			plot.background   = element_rect(color = "white", 
										    																			 fill  = "white"),
										    			panel.background  = element_rect(color = "#313F7F", 
										    																			 fill  = "#313F7F")
										    			) +
	
	                        # Labs
										      labs(x        = NULL, 
										      		 y        = NULL,
										      		 tag      = '{ifelse(between(round(frame_along,2),0,9.58),round(frame_along,2),
										      		                     ifelse(round(frame_along,2) > 9.58,"9.58 WR","0.00"))}',
										           title    = "2009 World Championships in Athletics – Men's 100 meters final",
										           subtitle = "<img src='C:/Users/maxim/Documents/Projets_Perso_R_HTML/Site_Web/Posts/Other_Themes/1_Athletism_100m_Berlin_2009_Final/GER.png' width='18'/>  BERLIN (OLYMPIASTADION), GERMANY 16 AUG 2009 - 21:35 , Wind +0.9",
										           caption  = "\n\nVisualization by DENIAUX Maxime  |  Data : worldathletics & me  |  Icons and images : Flaticon & worldathletics"
										      		 ) +
	
	                        # Transition / animate
										      transition_reveal(Time)




# Save plot ----


animate(plot_anim, 
        width         = 1300, 
				height        = 950, 
				duration      = 16, 
				nframes       = 500,
				fps           = 15,
        start_pause   = 55,
        renderer      = gifski_renderer("2019_Berlin_100m_Final.gif")
				)










