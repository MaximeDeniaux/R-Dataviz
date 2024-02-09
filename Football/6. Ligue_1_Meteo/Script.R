

# Packages ----

library("geofacet")
library("rvest")
library("lubridate")
library("ggplot2")
library("tidyverse")



# Directory

setwd("C:/Users/maxim/Documents/Site_Web/Git/R-Dataviz/Football/6. Ligue_1_Meteo")





# Data ----



# Matchs 

# Initially I only took season 22/23 but upon reflection, that turned out to be too little. 
# Because this had the consequence of only taking 2 matches per month in general per team which 
# is too small a sample of data to have an optimal representation. So I took the last 10 years, 
# that is to say from 2013-2014 to 2022-2023

# The cities selected subsequently will be the most present (I think at least present 5 times = half the time)



# Template for the future dataframe

df_matches           <-  data.frame(matrix(nrow = 1, ncol = 8))
colnames(df_matches) <-  c(colnames((read_html("https://www.worldfootball.net/schedule/fra-ligue-1-2013-2014-spieltag/1/") %>% 
																			html_nodes('table') %>% 
																			.[2] %>% 
																			html_table(fill = T)
																 )[[1]]),"Season")

for(j in 2013:2022){
	
if(j != 2019){
for(i in 1:38){
	
to_add <- (read_html(paste("https://www.worldfootball.net/schedule/fra-ligue-1-",j,"-",j+1,"-spieltag/",i,"/",sep = "")) %>% 
																			html_nodes('table') %>% 
																			.[2] %>% 
																			html_table(fill = T)
																	 )[[1]]

to_add$Season <-  j

df_matches <-  bind_rows(df_matches,to_add)

print(i)

}

}
	
if(j == 2019){
for(i in 1:27){ # Covid interruption
	
to_add <-  (read_html(paste("https://www.worldfootball.net/schedule/fra-ligue-1-",j,"-",j+1,"-spieltag/",i,"/",sep = "")) %>% 
																			html_nodes('table') %>% 
																			.[2] %>% 
																			html_table(fill = T)
																	 )[[1]]

to_add$Season <-  j

df_matches <-  bind_rows(df_matches,to_add)

print(i)
}
	

}

print(j)
}






# Processing data ----


# Remove the first line (blank line)

df_matches <-  df_matches[-1,]



# I only need columns 1 to 3 in order to know the location of the match and its schedule 
# to get the weather information at this time

df_matches <-  df_matches[,c(1,2,3,8)]



# I have to select cities that appear in at least 5 seasons to have a sufficient number of data.

Cities_to_take <-  df_matches %>% 
											group_by(X3,Season) %>% 
											slice(1) %>% 
											ungroup() %>% 
											select(X3) %>% 
											group_by(X3) %>%
											count() %>% 
											filter(n >= 5) %>% 
											pull(X3)


df_matches <-  df_matches %>% filter(X3 %in% Cities_to_take)


# Now, I need to change teams name to cities name used on the meteo website

df_matches <-  df_matches %>% mutate(City = case_when(X3 == "Olympique Lyon"      ~ "Lyon",
																											X3 == "RC Strasbourg"       ~ "Strasbourg",
																											X3 == "Toulouse FC"         ~ "Toulouse",
																											X3 == "Montpellier HSC"     ~ "Montpellier",
																											X3 == "Lille OSC"           ~ "Lille",
																											X3 == "Angers SCO"          ~ "Angers",
																											X3 == "Stade Rennais"       ~ "Rennes",
																											X3 == "Olympique Marseille" ~ "Marseille",
																											X3 == "FC Nantes"           ~ "Nantes",
																											X3 == "AS Monaco"           ~ "Monaco",
																											X3 == "Paris Saint-Germain" ~ "Paris",
																											X3 == "Stade Reims"         ~ "Reims",
																											X3 == "OGC Nice"            ~ "Nice",
																											X3 == "FC Lorient"          ~ "Lorient",
																											X3 == "AS Saint-Étienne"    ~ "Saint-Étienne",
																											X3 == "Dijon FCO"           ~ "Dijon",
																											X3 == "EA Guingamp"         ~ "Guingamp",
																											X3 == "Girondins Bordeaux"  ~ "Bordeaux",
																											X3 == "SM Caen"             ~ "Caen",
																											X3 == "FC Metz"             ~ "Metz"),
																	 
																	 url_meteo =  case_when(X3 == "Olympique Lyon"      ~ "https://www.meteociel.fr/temps-reel/obs_villes.php?code2=7480",
																													X3 == "RC Strasbourg"       ~ "https://www.meteociel.fr/temps-reel/obs_villes.php?code2=7190",
																													X3 == "Clermont Foot 63"    ~ "https://www.meteociel.fr/temps-reel/obs_villes.php?code2=7460",
																													X3 == "Toulouse FC"         ~ "https://www.meteociel.fr/temps-reel/obs_villes.php?code2=7630",
																													X3 == "Montpellier HSC"     ~ "https://www.meteociel.fr/temps-reel/obs_villes.php?code2=7643",
																													X3 == "Lille OSC"           ~ "https://www.meteociel.fr/temps-reel/obs_villes.php?code2=7015",
																													X3 == "RC Lens"             ~ "https://www.meteociel.fr/temps-reel/obs_villes.php?code2=62873001", # Arras because Lens not available
																													X3 == "Angers SCO"          ~ "https://www.meteociel.fr/temps-reel/obs_villes.php?code2=7230",
																													X3 == "Stade Rennais"       ~ "https://www.meteociel.fr/temps-reel/obs_villes.php?code2=7130",
																													X3 == "Olympique Marseille" ~ "https://www.meteociel.fr/temps-reel/obs_villes.php?code2=7650",
																													X3 == "FC Nantes"           ~ "https://www.meteociel.fr/temps-reel/obs_villes.php?code2=7222",
																													X3 == "AS Monaco"           ~ "https://www.meteociel.fr/temps-reel/obs_villes.php?code2=6083005", # Menton because Monaco not available
																													X3 == "Paris Saint-Germain" ~ "https://www.meteociel.fr/temps-reel/obs_villes.php?code2=7156",
																													X3 == "AC Ajaccio"          ~ "https://www.meteociel.fr/temps-reel/obs_villes.php?code2=7761",
																													X3 == "AJ Auxerre"          ~ "https://www.meteociel.fr/temps-reel/obs_villes.php?code2=7266",
																													X3 == "ESTAC Troyes"        ~ "https://www.meteociel.fr/temps-reel/obs_villes.php?code2=7168",
																													X3 == "Stade Reims"         ~ "https://www.meteociel.fr/temps-reel/obs_villes.php?code2=7072",
																													X3 == "OGC Nice"            ~ "https://www.meteociel.fr/temps-reel/obs_villes.php?code2=7072",
																													X3 == "Stade Brestois"      ~ "https://www.meteociel.fr/temps-reel/obs_villes.php?code2=7110",
																													X3 == "FC Lorient"          ~ "https://www.meteociel.fr/temps-reel/obs_villes.php?code2=7205",
																													X3 == "AS Saint-Étienne"    ~ "https://www.meteociel.fr/temps-reel/obs_villes.php?code2=7475",
																													X3 == "Dijon FCO"           ~ "https://www.meteociel.fr/temps-reel/obs_villes.php?code2=7280",
																													X3 == "EA Guingamp"         ~ "https://www.meteociel.fr/temps-reel/obs_villes.php?code2=7120",# Saint-Brieuc because...
																													X3 == "Girondins Bordeaux"  ~ "https://www.meteociel.fr/temps-reel/obs_villes.php?code2=7510",
																													X3 == "SM Caen"             ~ "https://www.meteociel.fr/temps-reel/obs_villes.php?code2=7027",
																													X3 == "FC Metz"             ~ "https://www.meteociel.fr/temps-reel/obs_villes.php?code2=7093") 
																	)



# Now, I need to fill in the blanks in the first column
# When there is a blank, it means it is the same value as the line above

for(i in 1:nrow(df_matches)){
	
	if(df_matches$X1[i] == ""){df_matches$X1[i] = df_matches$X1[i-1]}
	
}


# Date decomposition

df_matches$Day   <-  ifelse(substr(df_matches$X1,1,1) == "0",substr(df_matches$X1,2,2),substr(df_matches$X1,1,2))
df_matches$Month <-  ifelse(substr(df_matches$X1,4,4) == "0",substr(df_matches$X1,5,5),substr(df_matches$X1,4,5))
df_matches$Year  <-  substr(df_matches$X1,7,10)



# I will later use the site https://www.meteociel.fr/ which has a particular URL coding 
# so I still have to make a modification to the month variable.
# There is an offset of 1 in negative. For example the month January is has a value of 0 and not 1.

df_matches$Month <-  as.character(as.numeric(df_matches$Month) - 1)

df_matches <-  df_matches[,-1]



# The site offers weather archives by hour. So if a match was played for example at 1:30 p.m.,
# I will change and put 2:00 p.m. If it's 1:35 p.m., I put 2:00 p.m. If it's 1:25 p.m., I put 1:00 p.m.
# But first, I break down the time variable into 2 variables : Hour and Minutes.

df_matches$Hour    <-  substr(df_matches$X2,1,2)
df_matches$Minutes <-  substr(df_matches$X2,4,5)

for(i in 1:nrow(df_matches)){
	
if(between(as.numeric(df_matches$Minutes[i]),30,59)){df_matches$Hour[i] <-  as.character(as.numeric(df_matches$Hour[i]) + 1)}
if(between(as.numeric(df_matches$Minutes[i]),01,29)){df_matches$Hour[i] <-  as.character(as.numeric(df_matches$Hour[i]) - 1)}

}

df_matches$Hour <-  as.numeric(df_matches$Hour)

df_matches <-  df_matches[,-c(1,2,10)]
	


# Now I have all informations to scrap meteo values for each match


# I create 3 variables which will then be filled. Temperature, Wind, Rain.

df_matches$Temperature <-  NA
df_matches$Wind        <-  NA
df_matches$Rain        <-  NA


for(i in 1:nrow(df_matches)){
	
meteo_df <-  (read_html(paste(df_matches$url_meteo[i],"&jour2=",
														  df_matches$Day[i],"&mois2=",
														  df_matches$Month[i],"&annee2=",
														  df_matches$Year[i],
														  sep = ""
															)
												) %>% 
								html_nodes('table') %>% 
								.[8] %>% 
								html_table(fill = T)
							)[[1]]


colnames(meteo_df)   <-  meteo_df[1,]
meteo_df             <-  meteo_df[-1,]
meteo_df$Heurelocale <-  ifelse(nchar(meteo_df$Heurelocale) == 4, 
															substr(meteo_df$Heurelocale,1,2),
															substr(meteo_df$Heurelocale,1,1)
															)

meteo_df$Heurelocale <-  as.numeric(meteo_df$Heurelocale)
	
# I select the time of the match and the time after it to then take an average, because a match 
# lasts around 2 hours with stoppages in play.

meteo_df <-  meteo_df[which(meteo_df$Heurelocale %in% c(df_matches$Hour[i],df_matches$Hour[i]+1)),] 
if(length(which(colnames(meteo_df) == "Vent (rafales)")) == 2){meteo_df <-  meteo_df[,-which(colnames(meteo_df) == "Vent (rafales)")[1]]}
meteo_df <-  meteo_df %>% select(Température,`Vent (rafales)`,`Précip. mm/h`)



# Extract values

meteo_df$Température <-  parse_number(meteo_df$Température)

meteo_df$`Vent (rafales)` <-  parse_number(meteo_df$`Vent (rafales)`)

meteo_df$`Précip. mm/h` <-  sub(pattern = "traces", replacement = "" , x = meteo_df$`Précip. mm/h`)
meteo_df$`Précip. mm/h` <-  sub(pattern = "aucune", replacement = "" , x = meteo_df$`Précip. mm/h`)
meteo_df$`Précip. mm/h` <-  parse_number(meteo_df$`Précip. mm/h`)
meteo_df$`Précip. mm/h` <-  replace(meteo_df$`Précip. mm/h`,is.na(meteo_df$`Précip. mm/h`), 0)



# Calculate mean values and insert it

df_matches[i,8:10] <-  c(mean(meteo_df$Température[1:2]),
										 	   mean(meteo_df$`Vent (rafales)`[1:2]),
											   mean(meteo_df$`Précip. mm/h`[1:2])
											 )

print(i)
}



# There are missing values. I replace them by the mean by city and month

for(i in 1:nrow(df_matches)){
	
# For Temperature
if(is.na(df_matches$Temperature[i])){df_matches$Temperature[i] <-  mean(df_matches$Temperature[df_matches$City == df_matches$City[i] & 
																																														 df_matches$Month == df_matches$Month[i]], 
																																			na.rm = T)}
	
	
# For Rain
if(is.na(df_matches$Rain[i])){df_matches$Rain[i] <-  mean(df_matches$Rain[df_matches$City == df_matches$City[i] & 
																																				df_matches$Month == df_matches$Month[i]], 
																												na.rm = T)}
	
	
# For Wind
if(is.na(df_matches$Wind[i])){df_matches$Wind[i] <-  mean(df_matches$Wind[df_matches$City == df_matches$City[i] & 
																																				df_matches$Month == df_matches$Month[i]], 
																												na.rm = T)}

print(i)
}





df_matches <-  df_matches %>% 
														group_by(City,Month) %>% 
														mutate(Temperature = mean(Temperature),
																	 Rain        = mean(Rain),
																	 Wind        = mean(Wind)
																	 ) %>% 
														slice(1)

# Need for the plot to edit the class of Month

df_matches$Month <-  as.numeric(df_matches$Month)


df_matches$Month <-  as.numeric(df_matches$Month) + 1


# I can remove useless columns

df_matches <-  df_matches[,-c(1,4,6,7)]


# Cities coordinate grid 

grid_plot_Ligue_1 <-  data.frame(row  = c(3,4,2,3,2,1,3,4,5,2,5,5,3,5,2,2,2,4,2,5), 
															   col  = c(3,3,3,6,1,4,1,6,6,7,8,5,2,7,4,5,2,5,8,4), 
															   code = c("SCO","FCGB","SMC","DFCO","EAG","LOSC","FCL",
															 				    "OL","OM","FCM","ASM","MHSC","FCN","OGCN",
															 				    "PSG","SR","SRFC","ASSE","RCS","TFC"), 
															   name = df_matches$City %>% unique() %>% sort()
															 )

grid_plot_Ligue_1$name[grid_plot_Ligue_1$name == "Saint-Étienne"] <-  "Saint-\nÉtienne"
df_matches$City[df_matches$City == "Saint-Étienne"]               <-  "Saint-\nÉtienne"



	
# Plot ----

# Inspiration : https://hafen.github.io/geofacet/


# Unfortunately, the rain values are almost non-existent so I will remove them from the graph.



plot <-  ggplot(df_matches, aes(Month)) +
	        # Area & lines
				  stat_difference(aes(ymin = Wind, 
				  										ymax = Temperature), 
				  								alpha = 0.5
				  								) +
				  geom_line(aes(y     = Wind, 
				  							color = "Wind (km/h)")
				  					) +
				  geom_line(aes(y     = Temperature, 
				  							color = "Temperature (°C)")
				  					) +
					# Scales
				  scale_x_continuous(breaks = seq(1,12,1),
				  									 labels = c('Jan.','Feb.','Mar.','Apr.','May','Jun.',
				  									 					 'Jul.','Aug.','Sep.','Oct.','Nov.','Dec.'
				  									 					 )
				  									 ) +
				  scale_y_continuous(breaks = seq(0, 30, 10)) +
					scale_color_manual(values = c("#770C00", "#CAAC01")) + 
				  scale_fill_manual(values  = c(lighten("#770C00"),lighten("#CAAC01"))) +
	        coord_cartesian(ylim = c(0, 30)) +
	        # Legend
				  guides(fill = 'none') + 
	        # Titles
				  labs(title    = "Ligue 1 matches weather from\n2013/2014 to 2022/2023 by city",
				  		 subtitle = "Only teams with at least 5 years of appearance in\nLigue 1 during the period were selected.\nWeather values are monthly averages. Raw values\nbefore averages are those at the time of each match.",
				       caption  = "Visualisation by DENIAUX Maxime inspired by KARAMANIS Georgios | Data : worldfootball.net, meteociel.fr"
				       ) +
	        # Facet
				  facet_geo(vars(City), 
				  					grid = grid_plot_Ligue_1
				  					) +
	        # Theme
				  theme_minimal(base_family = "Fira Sans Compressed") +
					theme(# Axis text
						    axis.text.x        = element_text(face  = "bold",  
																									angle = 90,
																									color = "grey40",
																									size  = 4.5
																									),
								axis.text.y        = element_text(face  = "bold",
																									color = "grey40",
																									size  = 7
																									),
								# Panel custom
								panel.spacing.x    = unit(0.75,'lines'),
								panel.spacing.y    = unit(1.5,'lines'),
								panel.grid.major.y = element_line(colour   = '#C2C2C2',
																									linetype = 'longdash'
																									),
								panel.grid.minor   = element_blank(),
								panel.grid.major.x = element_blank(),
								# Legend custom
						    legend.pos         = c(0.88, 0.97),
						    legend.direction   = "vertical",
						    legend.box         = "vertical",
						    legend.title       = element_blank(),
						    # Plot custom
						    plot.background    = element_rect(fill  = "#F5F4EF", 
						    															    color = NA
						    																	),
						    plot.margin        = margin(60, 30, 40, 30),
						    plot.title         = element_text(margin     = margin(-45, 0, 0, 0), 
																						      size       = 17.5, 
																						      family     = "KyivType Sans", 
																						      face       = "bold", 
																						      vjust      = 0, 
																						      color      = "grey25",
										    													lineheight = 1
																						      ),
						    plot.subtitle      = element_text(margin     = margin(15, 0, -95, 0), 
																						      size       = 10, 
																						      family     = "KyivType Sans", 
																						      face       = "bold", 
																						      vjust      = 0, 
																						      color      = "#848484",
								    														  lineheight = 1.25
								    														  ),
						    plot.caption       = element_text(margin = margin(50, 0, 0, 0), 
																					        size   = 9, 
																					        family = "KyivType Sans", 
																					        face   = "bold", 
																					        vjust  = 0, 
																					        color  = "#7A7979",
									    														hjust  = 0.35
									    														),
						    axis.title = element_blank(),
						    # Facet custom
						    strip.text = element_text(face   = "bold", 
						    													color  = "grey20", 
						    													size   = 10.5,
						    													margin = margin(0,0,7,0)
						    													)
								)




# Save plot ----


ggsave(plot =  plot,
			 filename = "Plot.png",
			 width    = 12,
			 height   = 10,
			 device   = 'png'
			 )


