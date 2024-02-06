
# Packages ----

library("ggplot2")
library("rvest")
library("tidyverse")
library("lubridate")
library("ggimage")
library("cowplot")




# Directory

setwd("C:/Users/maxim/Documents/Site_Web/Git/R-Dataviz/Football/5. Neymar_Injuries")





# Data  ----

# (only club matches between 2009 and 2022)


# Looking for the number of table by club on the webpage (manually)


# c('SANTOS','BARCELONA','PARIS')

tables_vector <-  c(6,6,5)



# Template for the futur dataframe

df					  <-  data.frame(matrix(nrow = 1,ncol = 18))
colnames(df)  <-  c("Journée","Date","Équipe domicile","Équipe domicile","Équipe visiteuse","Équipe visiteuse","Résultat","Pos.",
									"","","","","","","","","","")



# Scrap tables

for(i_tables_vector in 1:length(tables_vector)){
	
	if(i_tables_vector == 1){link <-  'https://www.transfermarkt.fr/neymar/leistungsdatendetails/spieler/68290/plus/1?saison=&verein=221&liga=&wettbewerb=&pos=&trainer_id='}
	if(i_tables_vector == 2){link <-  'https://www.transfermarkt.fr/neymar/leistungsdatendetails/spieler/68290/plus/1?saison=&verein=131&liga=&wettbewerb=&pos=&trainer_id='}
	if(i_tables_vector == 3){link <-  'https://www.transfermarkt.fr/neymar/leistungsdatendetails/spieler/68290/plus/1?saison=&verein=583&liga=&wettbewerb=&pos=&trainer_id='}

	for(j in 3:tables_vector[i_tables_vector]){df <-  rbind(df,(read_html(link) %>% html_nodes('table') %>% .[j] %>% html_table())[[1]])}

}

rm(i_tables_vector)
rm(link)
rm(tables_vector)
rm(j)



# Data processing ----


# First line is a blank line

df <-  df[-1,] 



# Blank value in the first column = blank line

df <-  df[-which(df[,1] == ""),] 



# Remove "fake" columns due to import

df <-  df[,-c(3,5,18)] 



# English translation and fill blank column name value

colnames(df)  <-  c("Matchday","Date","HomeTeam","AwayTeam","Result","Position",
									"Goal","Assist","Own goal","YellowCard","RedCardAfterSecondYellowCard",
									"RedCard","SubIn","SubOff","PlayTime")



# Remove "fake" lines

lines <-  c(1) 
for(i in 1:nrow(df)){if(substr(df$Date[i],1,3) == "Eff"){lines <-  c(lines,i)}}

df <-  df[-lines,]

rm(lines)
rm(i)


# Rewriting PlayTime column

# I consider that 'Not in the squad' is probably an injury because at Santos for example, he was often the starter so there was 
# no particular reason for him not to be in the squad apart from injury.

df$PlayTime[df$PlayTime == "Pas dans l'effectif"] <-  "Injury"
df$PlayTime[df$PlayTime %in% c("Problèmes musculaires","Problèmes à la cheville","Problèmes à la cuisse",
															 "Opération de la cheville","Malade","Fracture du métatarse",
															 "Douleurs aux adducteurs","Entorse à la cheville","Déchirure musculaire au niveau des adducteurs",
															 "Coronavirus","Coup","Contusion thoracique","Blessure mineure",
															 "Blessure à la malléole","Blessure à l'ischio","Blessure au pied")] <-  "Injury"

df$PlayTime[df$PlayTime == "Sur le banc"] <-  "Sub."

df$PlayTime[df$PlayTime %in% c("Suspension pour deux avertissements","Suspension pour carton rouge",
															 "Supension pour accumulation de cartons jaunes",
															 "suspension via comission de discipline")] <-  "Suspension"

df$PlayTime[which(df$PlayTime %in% c("Repos","Manque d'entrainement",
																		 "Jeux Olympiques","Autorisation spéciale",
																		 "Non-éligible"))] <-  "Other"


df$PlayTime <-  sub(x = df$PlayTime, pattern = "'", replacement = "", fixed = T)



# Create new variable for the theoric maximum play time

df$Max_Playtime <-  ifelse(nchar(df$Result) > 3, 120, 90)
rownames(df)    <-  1:nrow(df)

# Edit ! In franch national cup since 2020-2021, the two overtime periods no longer exist (except for the final)
# So, here i need to edit the Max_Playtime for the matchs against Nice (2022) and Montpellier (2021)

df$Max_Playtime[which(df$HomeTeam == "Montpellier" & df$Result == "7:8  tab")] <-  90
df$Max_Playtime[which(df$AwayTeam == "OGC Nice" & df$Result == "5:6  tab")]    <-  90





# Rewriting date column

for(i in 1:nrow(df)){
	
df$Date[i] <-  sub(x = df$Date[i], pattern = 'janv.' , replacement = "01", fixed = T)
df$Date[i] <-  sub(x = df$Date[i], pattern = 'févr.' , replacement = "02", fixed = T)
df$Date[i] <-  sub(x = df$Date[i], pattern = 'mars'  , replacement = "03", fixed = T)
df$Date[i] <-  sub(x = df$Date[i], pattern = 'avr.'  , replacement = "04", fixed = T)
df$Date[i] <-  sub(x = df$Date[i], pattern = 'mai'   , replacement = "05", fixed = T)
df$Date[i] <-  sub(x = df$Date[i], pattern = 'juin'  , replacement = "06", fixed = T)
df$Date[i] <-  sub(x = df$Date[i], pattern = 'juil.' , replacement = "07", fixed = T)
df$Date[i] <-  sub(x = df$Date[i], pattern = 'août'  , replacement = "08", fixed = T)
df$Date[i] <-  sub(x = df$Date[i], pattern = 'sept.' , replacement = "09", fixed = T)
df$Date[i] <-  sub(x = df$Date[i], pattern = 'oct.'  , replacement = "10", fixed = T)
df$Date[i] <-  sub(x = df$Date[i], pattern = 'nov.'  , replacement = "11", fixed = T)
df$Date[i] <-  sub(x = df$Date[i], pattern = 'déc.'  , replacement = "12", fixed = T)

}


# unique(nchar(df$Date)) : 10 9 

for(i in 1:nrow(df)){
if(nchar(df$Date[i]) == 9){df$Date[i] <-  paste("0",substr(df$Date[i],1,1),"-",
																						  substr(df$Date[i],3,4),"-",
																						  substr(df$Date[i],6,9),
																						  sep = "")
}
	
if(nchar(df$Date[i]) == 10){df$Date[i] <-  paste(substr(df$Date[i],1,2),
																						  substr(df$Date[i],4,5),
																						  substr(df$Date[i],7,10),
																						  sep = "-")	
}
}

df$Date <-  dmy(df$Date)



# Season variable

df$Season <-  ifelse(df$Date < as_date("2010-05-08"),2009,
									 ifelse(df$Date < as_date("2011-02-16"),2010,
									 			 ifelse(df$Date < as_date("2012-02-15"),2011,
									 			 			 ifelse(df$Date < as_date("2013-05-26"),2012,
									 			 			 			 ifelse(df$Date < as_date("2014-08-24"),2013,
									 			 			 			 			 ifelse(df$Date < as_date("2015-08-11"),2014,
									 			 			 			 			 			 ifelse(df$Date < as_date("2016-08-20"),2015,
									 			 			 			 			 			 			 ifelse(df$Date < as_date("2017-08-05"),2016,
									 			 			 			 			 			 			 			 ifelse(df$Date < as_date("2018-08-12"),2017,
									 			 			 			 			 			 			 			 			 ifelse(df$Date < as_date("2019-08-11"),2018,
									 			 			 			 			 			 			 			 			 			 ifelse(df$Date < as_date("2020-09-10"),2019,
									 			 			 			 			 			 			 			 			 			 			 ifelse(df$Date < as_date("2021-08-07"),2020,
									 			 			 			 			 			 			 			 			 			 			 			 ifelse(df$Date < as_date("2022-08-06"),2021,2022)))))))))))))
									 

# I see a match for the 23-24 season, I remove it

df <-  df[-which(df$Date >= as_date("2023-08-12")),]



# Goals and Assists columns

df$Goal[which(nchar(df$Goal) > 1)]  <-  NA
df$Goal[which(nchar(df$Goal) == 0)] <-  0

df$Assist[which(nchar(df$Assist) > 1)]  <-  NA
df$Assist[which(nchar(df$Assist) == 0)] <-  0



# Suboff time variable

df$Suboff_Time <-  NA

for(i in 1:nrow(df)){
	
	# With SubOff variable
	if( (substr(df$SubOff[i],1,1) == substr(df$PlayTime[i],1,1) ) & (nchar(df$SubOff[i]) <= 3) ){
		df$Suboff_Time[i] <-  df$Max_Playtime[i] - as.numeric(df$PlayTime[i])
	}
	
	# With SubIn variable
	if( between(nchar(df$SubIn[i]),1,3) ){
		df$Suboff_Time[i] <-  df$Max_Playtime[i] - as.numeric(df$PlayTime[i])
	}
	
	# With redcard variable
	if( (substr(df$RedCard[i],1,1) %in% as.character(0:9)) | (substr(df$RedCardAfterSecondYellowCard[i],1,1) %in% as.character(0:9))){
		df$Suboff_Time[i] <-  df$Max_Playtime[i] - as.numeric(df$PlayTime[i])
	}
	
}


for(i in 1:nrow(df)){
	if( (df$PlayTime[i] == df$Max_Playtime[i]) & (is.na(df$Suboff_Time[i])) ){df$Suboff_Time[i] <-  0 }
}


df$Suboff_Time[which(is.na(df$Suboff_Time))] <-  0



# Category variable

df$Category                                                             <-  df$PlayTime
df$Category[! df$PlayTime %in% c("Suspension","Other","Sub.","Injury")] <-  "Playing"


# For all categories except Playing, Playtime become Max_Playtime value

df$PlayTime_Copy                                                             <-  df$PlayTime
df$PlayTime[which(df$Category %in% c("Suspension","Other","Sub.","Injury"))] <-  df$Max_Playtime[which(df$Category %in% c("Suspension","Other","Sub.","Injury"))]






# Club variable

df$Club <-  ifelse(df$Date < as_date("2013-08-31"), "Santos",
								   ifelse(between(df$Date, as_date("2013-09-01"), as_date("2017-08-01")), "Barcelona",
								   "Paris"))



# Summarise playtime informations 

Category_Summary <-  df %>% group_by(Season, Category) %>% summarise(count = sum(as.numeric(PlayTime)))


# I need to add (for the category Suspension) the time not played because of redcard

Time_Red_Card          <-  df %>% filter(Category == "Playing", RedCardAfterSecondYellowCard != "" | RedCard != "") %>% group_by(Season) %>% summarise(count = sum(Suboff_Time))
Time_Red_Card$Category <-  "Suspension"



# I need to add (for the category Sub.) the time not played because he didn't start the macth

Time_Playing_Being_Sub          <-  df %>% filter(Category == "Playing", SubIn != "" | SubOff != "") %>% group_by(Season) %>% summarise(count = sum(Suboff_Time)) 
Time_Playing_Being_Sub$Category <- "Sub."


# Now, bind_rows and group_by again to sum the new values


Category_Summary <-  bind_rows(Category_Summary,Time_Red_Card,Time_Playing_Being_Sub) %>% group_by(Season, Category) %>% summarise(count = sum(count))


Category_Summary$Category <-  factor(Category_Summary$Category,levels = rev(c("Playing","Sub.","Injury","Suspension","Other")))


Category_Summary <-  Category_Summary %>% group_by(Season) %>% summarise(Season          = Season, 
																																			   Category        = Category,
																																			   count           = count,
																																			   count_pct       = round(count * 100 / sum(count), digits = 3),
																																			   count_pct_label = round(count * 100 / sum(count), digits = 0)
																																			   ) 


# Due to rounding for labels, the sum sometimes exceeds 100. I correct it manually.
# Category_Summary %>% group_by(Season) %>% summarise(somme = sum(count_pct_label)) : concerned years -> 2013, 2018, 2020, 2022

Category_Summary$count_pct_label[17] <-  54
Category_Summary$count_pct_label[37] <-  40
Category_Summary$count_pct_label[49] <-  45
Category_Summary$count_pct_label[58] <-  51



# Also, I need to edit the value 0 

Category_Summary$count_pct_label[which(Category_Summary$count_pct_label == 0)] <-  NA




# Data frame with team png images

df_team_png <-  data.frame(x   = seq(2009,2022,1),
										       y   = rep(105,14),
												   url = c(rep("Santos.png",4),
												 		 		   rep("Barcelona.png",4),
												 		 		   rep("PSG.png",6)
												   				)
												   ) 


# I add the Neymar png
df_png_neymar <-  data.frame(x   = 2006,
													   y   = 50,
													   url = "Neymar.png"
													   ) 
									 

caption <-  data.frame(x     = 2002.1,
										   y     = 18,
										   label = "Visualisation by Deniaux Maxime inspired by Scherer Cedric | Data : Transfermarkt")




# Plot ----



# Barplot


barplot <-  ggplot() + 
					  geom_col(data = Category_Summary, aes(fill = Category, y = count_pct, x = Season),
					  				 position = "stack",
					  				 width    = 0.6, 
					  				 alpha    = 0.85, 
					  				 col      = 'white') +
						# Pct values
						geom_text(data = Category_Summary, 
					            aes(x     = Season, 
					            		y     = count_pct, 
					            		label = count_pct_label,
					            		group = Category
					            		), 
					            color    = "white", 
											size     = 1.85,
											fontface = "bold",
											position = position_stack(vjust = 0.5)
											) +
						# Images
						geom_image(data = df_team_png, aes(x = x, y = y, image = url),size = 0.035) + 
						geom_image(data = df_png_neymar, aes(x = x, y = y, image = url),size = 0.08) +
						# Scales
						scale_y_continuous(limits   = c(-10,120),
															 breaks   = c(-8,0,20,40,60,80,100),
															 position = "right",
															 labels   = c("Age",paste(c(0,20,40,60,80,100), "%", sep = ""))
															 ) +
						scale_x_continuous(limits = c(2002,2022.3),
															 breaks = seq(2009,2022,1),
															 labels = as.character(seq(17,30,1))
															 ) +
						# Theme
						theme(axis.text.y       = element_text(margin = margin(0,-45,0,0),
																	       					 colour = "#2E2E2E"
																									 ),
									axis.text.x       = element_text(colour = "#2E2E2E"),
									plot.margin       = margin(margin(20,0,20,50)),
					        axis.ticks        = element_blank(),
					        panel.background  = element_rect(fill = "white"),
									plot.background   = element_rect(fill = "white"),
									panel.grid        = element_blank(),
									legend.background = element_rect(fill = "white"),
									legend.direction  = "horizontal",
									legend.position   = c(0.5,0.125),
									plot.title        = element_markdown(size   = 15,
																											 hjust  = 0.5, 
                  																		 margin = margin(0,0,20,0)
																											 ),
                  plot.subtitle     = element_markdown(size       = 10, 
                  																		 color      = "#494949", 
                  																		 hjust      = 0, 
                  																		 margin     = margin(0,0,40,0),
                  																		 lineheight = 1.5
                  																		 )
									) +
						# Legend custom
						guides(fill = guide_legend(reverse        = TRUE,
																			 label.position = "bottom",
																			 keywidth       = unit(5.5,'lines'),
																			 keyheight      = unit(0.75,'lines'),
																			 title.position = "top"
																			 )
									 ) +
					  labs(x        = NULL, 
					  		 y        = NULL, 
					  		 fill     = NULL,
					  		 title    = "Neymar, extraordinary talent with a <span style='color:#B93436'>fragile body</span> and a <span style='color:#E7B6F1'>missing luck</span>.",
					  		 subtitle = "Neymar is probably one of the most talented footballers in history. His statistics, decisive 489 times (296 goals and 193 assists) in 503 matches<br>
										  		  so far, or 0.97 times per match, make him a player who inevitably has an impact on the score of a match. Unfortunately, he is also known for his <br>
										  		  very frequent and sometimes serious injuries, causing him to miss many matches and sometimes very important competitions. In order to better <br>
										  		  visualize how far he moved away from the field during his career in club between 2009-2010 and 2022-2023, I retraced the latter."
					  		 ) +
						# Fill colors
						scale_fill_manual(values = rev(c('#264653', '#2a9d8f', '#e9c46a', '#f4a261', '#e76f51')),
															labels = rev(c("Playing", "Substitute or replaced", "Injury", "Suspension","Other"))
															) +
						# Caption
						geom_textbox(data = caption,
												 aes(x     = x, 
												 		 y     = y, 
												 		 label = label
												 		), 
												 size     = 2.75, 
												 color    = "#6E6E6E", 
												 hjust    = 0, 
												 vjust    = 1, 
												 box.size = NA, 
												 fill     = NA,
												 width    = unit(6.5, "inch")) +
						# Reeding informations
						annotate(
					    geom = "curve", 
					    y         = 10, 
					    x         = 2007.25, 
					    yend      = 22, 
					    xend      = 2008.70, 
					    curvature = 0.2, 
					    arrow     = arrow(length = unit(2, "mm")), 
					    col       = "#6E6E6E"
					  ) +
						geom_textbox(data = data.frame(x     = 2007, 
																	       	 y     = 0.5, 
																		       label = "In his first season at Santos, Neymar played around 48% of the maximum playing time he could expect."
																					 ), 
												 aes(x     = x, 
												 		 y     = y, 
												 		 label = label
												 		), 
							  				 hjust = 0,
							  				 size  = 2.5,
							  				 col   = "#514F4F",
							  				 fill  = "#FFFADF",
												 width = unit(1.8, "inch")
												 ) +
						annotate(geom = "segment", 
									   y         = -3.75, 
									   x         = 2008.7, 
									   yend      = -3.75,
									   xend      = 2009.85,  
									   arrow     = arrow(length = unit(2, "mm")), 
									   col       = "#BD9BCC"
					  ) +
						# Axis reverse
						coord_flip() 








# Save plot ----


ggsave(barplot,
			 filename = "Plot.png", 
       width    = 9.5, 
			 height   = 7.5,
			 device   = "png")

