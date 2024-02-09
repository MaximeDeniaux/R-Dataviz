

# Packages ----

library("rvest")
library("tidyverse")
library("gghighlight")
library("ggtext")
library("ggforce")
library("glue")
library("ggstar")
library("cowplot")



# Directory

setwd("C:/Users/maxim/Documents/Site_Web/Git/R-Dataviz/Football/3. LDC_Qualification")





# Data : Ranking season 22-23 ----

i <- 1

Ranking_By_Round           <-  data.frame(matrix(nrow = 1,ncol = 4))
colnames(Ranking_By_Round) <- c("Rank","Team","Round","Points")


while(i <= 35){

if(i == 1){
	
Ranking_Round_i          <- as.data.frame((read_html(paste('https://www.mondefootball.fr/calendrier/fra-ligue-1-2022-2023-spieltag/',
																													 i,
																													 sep = ""
																													 )
																										 ) %>% 
																							html_nodes('table') %>% 
																							.[4] %>% 
																							html_table(fill = T)
																					 )[[1]]
																					)

Ranking_Round_i           <- Ranking_Round_i[,c(1,3,10)]
colnames(Ranking_Round_i) <- c("Rank","Team","Points")

Ranking_Round_i$Rank      <- 'None'

}
	
if(i != 1){
	
Ranking_Round_i           <- as.data.frame((read_html(paste('https://www.mondefootball.fr/calendrier/fra-ligue-1-2022-2023-spieltag/',i,sep = "")) %>% 
																			        html_nodes('table') %>% 
																			        .[4] %>% 
																			        html_table(fill = T)
																						)[[1]]
																					 )
	
Ranking_Round_i           <- Ranking_Round_i[,c(1,3,10)]
colnames(Ranking_Round_i) <- c("Rank","Team","Points")



# NA replace

for(j in 1:nrow(Ranking_Round_i)){
																	if(is.na(Ranking_Round_i$Rank[j])){
																																		 Ranking_Round_i$Rank[j] <- Ranking_Round_i$Rank[j-1]
																	}
}
	
Ranking_Round_i$Rank <- as.character(Ranking_Round_i$Rank)

}
	
	

Ranking_Round_i$Round <- i

Ranking_By_Round      <- bind_rows(Ranking_By_Round,Ranking_Round_i)

print(i)
i <- i + 1

}


if(is.na(Ranking_By_Round$Rank[1])){Ranking_By_Round <- Ranking_By_Round[-1,]}



Ranking_By_Round$Team[Ranking_By_Round$Team == "Olympique Marseille"] <- "OM"
Ranking_By_Round$Team[Ranking_By_Round$Team == "Paris Saint-Germain"] <- "PSG"
Ranking_By_Round$Team[Ranking_By_Round$Team == "RC Lens"]             <- "Lens"



rm(Ranking_Round_i)
rm(i)
rm(j)






# Data : Budget 22-23 ----

# Source : https://sportune.20minutes.fr/sport-business/football/om-psg-ol-losc-asm-les-budgets-de-la-ligue-1-saison-2022-2023-288836/2

Budget <- data.frame(Team  = c("Paris","Lyon","Marseille","Monaco","Lille","Nice","Rennes",
															 "Nantes","Reims","Lens","Montpellier","Lorient","Brest",
															 "Strasbourg","Troyes","Angers","Toulouse","Auxerre",
															 "Clermont","Ajaccio"
															 ),
										Value = c(700,250,250,240,100,100,90,75,70,62,51.5,50,48,45,45,40,40,32,25,22)
										)














# Data : Ranking history 2000-2023 ----

# Source : https://fr.wikipedia.org/wiki/Bilan_saison_par_saison_du_Racing_Club_de_Lens



# Lens

Lens_History_Ranking <- (read_html('https://fr.wikipedia.org/wiki/Bilan_saison_par_saison_du_Racing_Club_de_Lens') %>% 
														html_nodes('table') %>% 
														.[1] %>% 
														html_table(fill = T))[[1]] %>% 
																													 select(1,2,3,15) %>%
																												   slice(80:101)


colnames(Lens_History_Ranking) <- c("Season","Division","Rank","C1_Qualif")


Lens_History_Ranking$Season    <- seq(2000,2021,1)


Lens_History_Ranking$Division  <- case_when(Lens_History_Ranking$Division == "L1" ~ "D1",
																		    	  Lens_History_Ranking$Division == "L2" ~ "D2",
																			      TRUE ~ Lens_History_Ranking$Division)


Lens_History_Ranking <- separate(Lens_History_Ranking, 
															   col  = Rank, 
															   into = c('Rank', 'e'), 
														  	 sep  = 'e') %>% select(1,2,3,5)


for(i in 1:nrow(Lens_History_Ranking)){
	
	if(substr(Lens_History_Ranking$C1_Qualif[i],1,2) == "C1"){
																												 Lens_History_Ranking$C1_Qualif[i-1] <- "Yes"
																												 Lens_History_Ranking$C1_Qualif[i]   <- "No"
	}
	
	else{Lens_History_Ranking$C1_Qualif[i] <- "No"}
}


Lens_History_Ranking$Team <- "Lens"



# OM

OM_History_Ranking <- (read_html('https://fr.wikipedia.org/wiki/Bilan_saison_par_saison_de_l%27Olympique_de_Marseille') %>% 
												  html_nodes('table') %>% 
													.[1] %>% 
													html_table(fill = T))[[1]] %>% 
																												 select(1,2,13) %>%
																												 slice(134:174)


OM_History_Ranking <- OM_History_Ranking[-which(duplicated(OM_History_Ranking[,c(1,2)])),]


OM_History_Ranking <- separate(OM_History_Ranking, 
															 col  = Championnat, 
															 into = c('Division', 'Rank'), 
														   sep  = ': ') 

OM_History_Ranking$Rank[which(substr(OM_History_Ranking$Rank,1,8) == "Champion")] <- "1e"

OM_History_Ranking <- separate(OM_History_Ranking, 
															 col  = Rank, 
															 into = c('Rank', 'remove'), 
														   sep  = 'e') %>% select(1,2,3,5)

colnames(OM_History_Ranking) <- c("Season","Division","Rank","C1_Qualif")


OM_History_Ranking$Season <- seq(2000,2021,1)


OM_History_Ranking$Division <- "D1"


for(i in 1:nrow(OM_History_Ranking)){
	
	if(substr(OM_History_Ranking$C1_Qualif[i],1,2) == "C1"){
																												  OM_History_Ranking$C1_Qualif[i-1] <- "Yes"
																												  OM_History_Ranking$C1_Qualif[i]   <- "No"
	}
	
	else{OM_History_Ranking$C1_Qualif[i] <- "No"}
}


if(OM_History_Ranking$Rank[nrow(OM_History_Ranking)] %in% c(1,2)){OM_History_Ranking$C1_Qualif[nrow(OM_History_Ranking)] <- "Yes"}

OM_History_Ranking$Team <- "Marseille"




# PSG

PSG_History_Ranking <- (read_html('https://fr.wikipedia.org/wiki/Bilan_saison_par_saison_du_Paris_Saint-Germain') %>% 
															html_nodes('table') %>% 
															.[3] %>% 
															html_table(fill = T))[[1]] %>% 
															   														select(1,2,3,15)%>%
																						                slice(31:52)

PSG_History_Ranking <- separate(PSG_History_Ranking, 
															  col  = Cl, 
															  into = c('Rank', 'remove'), 
														    sep  = 'e') %>% select(1,2,3,5)


colnames(PSG_History_Ranking) <- c("Season","Division","Rank","C1_Qualif")


PSG_History_Ranking$Season <- seq(2000,2021,1)


PSG_History_Ranking$Division = "D1"


for(i in 1:nrow(PSG_History_Ranking)){
	
	if(substr(PSG_History_Ranking$C1_Qualif[i],1,2) == "C1"){
																												  PSG_History_Ranking$C1_Qualif[i-1] <- "Yes"
																												  PSG_History_Ranking$C1_Qualif[i]   <- "No"
	}
	
	else{PSG_History_Ranking$C1_Qualif[i] <- "No"}
}


if(PSG_History_Ranking$Rank[nrow(PSG_History_Ranking)] %in% c(1,2)){PSG_History_Ranking$C1_Qualif[nrow(PSG_History_Ranking)] <- "Yes"}


PSG_History_Ranking$Team <- "Paris"




# Combine dataframes

df_Qualif_Champions_League <- bind_rows(PSG_History_Ranking,OM_History_Ranking,Lens_History_Ranking)

rm(PSG_History_Ranking)
rm(OM_History_Ranking)
rm(Lens_History_Ranking)
rm(i)




# Graph 1 : Race ----


# Flags

download.file(url = "https://images.fotmob.com/image_resources/logo/teamlogo/9847.png",
							destfile = "PSG.png",
							mode = 'wb')

download.file(url = "https://images.fotmob.com/image_resources/logo/teamlogo/8588.png",
							destfile = "Lens.png",
							mode = 'wb')

download.file(url = "https://images.fotmob.com/image_resources/logo/teamlogo/8592.png",
							destfile = "OM.png",
							mode = 'wb')

img_PSG  <- png::readPNG("PSG.png")
PSG      <- grid::rasterGrob(img_PSG, interpolate = T,width = 1,height = 1)

img_Lens <- png::readPNG("Lens.png")
Lens     <- grid::rasterGrob(img_Lens, interpolate = T,width = 1,height = 1)

img_OM   <- png::readPNG("OM.png")
OM       <- grid::rasterGrob(img_OM, interpolate = T,width = 1,height = 1)





# Plot

Race <- ggplot(Ranking_By_Round) +
					  geom_line(aes(x     = Round, 
					  							y     = Points, 
					  							group = Team, 
					  							color = Team), 
					  					linewidth = 1.25
					  					) +
						gghighlight(max(Points) > 70, 
												unhighlighted_params = list(color     = "#DFDFDF",
																										linewidth = 0.5
																										),
												label_key            = Points,
												label_params         = list(fontface      = "bold",
																										size          = 3.5,
																										direction     = "y",
																										nudge_x       = 0.85,
																										segment.color = NA
																										)
												) +
				  	annotation_custom(PSG,  xmin = 36.75, xmax = 38, ymin = 81-1.75,   ymax = 81+2.25) +
				    annotation_custom(Lens, xmin = 36.75, xmax = 38, ymin = 75-1,      ymax = 75+3) +
				    annotation_custom(OM,   xmin = 36.75, xmax = 38, ymin = 68.75,     ymax = 69.5+4) +
					  scale_x_continuous(breaks = seq(0, 35, 5), 
					  									 limits = c(0, 39), 
					  									 expand = c(0, 0)) +
						scale_y_continuous(breaks = seq(0, 90, 10)) +
					  scale_color_manual(values = c("Lens" = "#E7C911", 
					  															"PSG"  = "#DE4B41",
					  															"OM"   = "#009EDD")
					  									 ) +
					  labs(title    = "Race for a direct qualification for the Champions League",
						     subtitle = "<span style='color:#F4D30F;font-size:15pt;'>Le</span><span style='color:#A71A1B;font-size:15pt;'>ns</span><span style='color:#FFFFFF;font-size:15pt;'> for an incredible 2nd place, 
						                  3 matchs from the end of the championship ? <br> 
						                  This is what all Ligue 1 followers are wondering. Marseille and Lens have battled all season for a 2nd place synonymous with <br>
						                  direct qualification for the Champions League. This 2nd place has changed owners several times as can be seen on the graph. <br> 
						                  However, today the gap is very small (2 points and a better goal-average for Lens : +32 against +30). <br>
						                  The end of the championship in these conditions makes it very exciting.</span>",
						     x        = "Matchday",
						     y        = "Points"
					  ) +
					  theme(panel.background   = element_rect(fill  = "#333333", 
					  																				color = "white"
					  																				),
					  			plot.background    = element_rect(fill  = "#333333", 
					  																				color = "#333333"
					  																				),
					  			axis.ticks.x       = element_blank(),
					        axis.title.x       = element_text(size   = 14,
					        																	color  = "white",
					        																	face   = "bold",
					        																	margin = margin(20, 0, 0, 0)
					        																	),
					        axis.text.x        = element_text(size   = 12,
					        																	color  = "#F8C9EC",
					        																	face   = "bold"
					        																	),
					        axis.text.y        = element_text(size   = 12,
					        																	color  = "white",
					        																	face   = "bold"
					        																	),
					        axis.title.y       = element_text(size   = 14,
					        																	color  = "white",
					        																	face   = "bold",
					        																	angle  = 0,
					        																	vjust  = 0.5,
					        																	margin = margin(0, 30, 0, 0)
					        																	),
					  			axis.ticks.y       = element_line(color = "white"),
							    panel.grid.major.x = element_blank(),
							    panel.grid.minor.x = element_blank(),
							    panel.grid.major.y = element_blank(),
							    panel.grid.minor.y = element_blank(),
							    plot.title         = element_text(margin = margin(0, 0, 25, 0),
							    									        			  size   = 29, 
							    																	face   = "bold", 
							    																	color  = "#E7BB62"
							    																	),
							    plot.margin        = margin(20, 20, 50, 20),
							    plot.subtitle      = element_markdown(margin     = margin(5, 0, 30, 0),
							    																			lineheight = 1.7
							    																			)
					  )






# Graph 2 : Rank vs Budget ----


# Join the rank at matchday 35

Ranking_By_Round_J35      <-  Ranking_By_Round %>% filter(Round == 35)
Ranking_By_Round_J35$Team <-  case_when(Ranking_By_Round_J35$Team == "PSG" ~ "Paris",
																				Ranking_By_Round_J35$Team == "OM" ~ "Marseille",
																				Ranking_By_Round_J35$Team == "AS Monaco" ~ "Monaco",
																				Ranking_By_Round_J35$Team == "Lille OSC" ~ "Lille",
																				Ranking_By_Round_J35$Team == "Stade Rennes" ~ "Rennes",
																				Ranking_By_Round_J35$Team == "Olympique Lyon" ~ "Lyon",
																				Ranking_By_Round_J35$Team == "Clermont Foot 63" ~ "Clermont",
																			  Ranking_By_Round_J35$Team == "FC Lorient" ~ "Lorient",
																				Ranking_By_Round_J35$Team == "OGC Nice" ~ "Nice",
																				Ranking_By_Round_J35$Team == "Stade Reims" ~ "Reims",
																				Ranking_By_Round_J35$Team == "Montpellier HSC" ~ "Montpellier",
																				Ranking_By_Round_J35$Team == "Toulouse FC" ~ "Toulouse",
																				Ranking_By_Round_J35$Team == "RC Strasbourg" ~ "Strasbourg",
																				Ranking_By_Round_J35$Team == "Stade Brest" ~ "Brest",
																				Ranking_By_Round_J35$Team == "AJ Auxerre" ~ "Auxerre",
																				Ranking_By_Round_J35$Team == "FC Nantes" ~ "Nantes",
																				Ranking_By_Round_J35$Team == "AC Ajaccio" ~ "Ajaccio",
																				Ranking_By_Round_J35$Team == "ESTAC Troyes" ~ "Troyes",
																				Ranking_By_Round_J35$Team == "Angers SCO" ~ "Angers",
																				TRUE ~ Ranking_By_Round_J35$Team
																				)


Budget_vs_Rank             <-  Budget 
Budget_vs_Rank$Rank_Budget <-  seq(1,20,1)

Budget_vs_Rank <-  left_join(Budget_vs_Rank,
													   Ranking_By_Round_J35[,c(1,2)], 
													   by = "Team")

Budget_vs_Rank$Rank_Budget <-  as.numeric(Budget_vs_Rank$Rank_Budget)
Budget_vs_Rank$Rank        <-  as.numeric(Budget_vs_Rank$Rank)


Budget_vs_Rank <-  Budget_vs_Rank %>% mutate(color = case_when(Budget_vs_Rank$Rank < Budget_vs_Rank$Rank_Budget ~ "#B4EF93",
																														   Budget_vs_Rank$Rank > Budget_vs_Rank$Rank_Budget ~ "#C84440",
																														   TRUE ~ "#D4D4D4"))
Budget_vs_Rank$color <-  as.factor(Budget_vs_Rank$color)





# Plot

Perf <-  Budget_vs_Rank %>% mutate(Team = case_when(Team == "Paris" ~ glue("<span style='color:#DE4B41;'>{Team} ({Value} M€)</span>"),
																									  Team == "Marseille" ~ glue("<span style='color:#009EDD;'>{Team} ({Value} M€)</span>"),
																									  Team == "Lens" ~ glue("<span style='color:#E7C911;'>{Team} ({Value} M€)</span>"),
				    																			  TRUE ~ glue("<span style='color:white;'>{Team} ({Value} M€)</span>")
																									  ),
																	 Team = fct_reorder(Team, desc(Rank_Budget))
																	 ) %>%
																				ggplot() +
																			  
																			  # Comets
																			  geom_link(aes(x        = Rank_Budget , 
																							    		xend     = Rank ,
																							    		y        = Team,        
																							    		yend     = Team, 
																							    		alpha    = 0.45,
																							        color    = color, 
																							        size     = after_stat(I(3.5*index))),
																							        n        = 500
																							    ) +
																			  geom_point(aes(y      = Team, 
																			  							 x      = Rank, 
																			  							 color  = color),
																									     shape  = 21,
																									     fill   = "white",
																									     size   = 3,
																									     stroke = 1.5
																									 ) +
																			  
																				# Coordinates + scales
																			  scale_x_continuous(breaks = seq(1,20,2), 
																			  									 limits = c(0, 21),
																			  									 expand = c(0, 0)
																			  									 ) +
																				expand_limits(y = c(0,21))+
																			  scale_color_manual(values = c("#B4EF93" , 
																			  															"#E99C66", 
																			  															"#B3B3B3"
																			  															)
																			  									 ) +
																				scale_fill_manual(values = c("#B4EF93" , 
																			  														 "#E99C66", 
																			  														 "#B3B3B3"
																			  														 )
																			  									 ) +
																				labs(title    = "Performance against budget",
																						 subtitle = "<span style='color:#FFFFFF;font-size:12pt;'> This end of the championship is particularly exciting because Lens is <br> 
																						             not a club supposed to play the leading roles. This is the tenth budget, <br> 4 times lower than Marseille.</span>",
																						 y        = "",
																						 x        = "Ligue 1 ranking / Budget ranking",
																						 caption  = "Visualization"
																						 ) +
																				 geom_richtext(
																										    aes(x = 13.5, 
																										    		y = 19.8,
																										        label = "<span style='color:#B4EF93;font-size:10pt;'>—— Good performance</span> <br>
																										    		         <span style='color:#E99C66;font-size:10pt;'>—— Bad performance</span><br>
																										    		         <span style='color:#B3B3B3;font-size:10pt;'>—— Expected performance</span>"
																										    		), 
																										    inherit.aes = FALSE,
																										    hjust       = 0,
																										    vjust       = 0.5,
																										    fill        = "#333333",
																										    color       = "white"
																										  ) +
																				geom_richtext( aes(x = 1.1, 
																										    		y = 9.5,
																										        label = "<span style='color:#B4EF93;font-size:10pt;'> Lens outperforms by being <br> 2nd with the 10th budget. </span>"
																										    		), 
																										    inherit.aes = FALSE,
																										    hjust       = 0,
																										    vjust       = 0.5,
																										    fill        = "#333333",
																										    color       = "white"
																										  ) +
																				geom_richtext(
																										    aes(x = 12.25, 
																										    		y = 14.5,
																										        label = "<span style='color:#E99C66;font-size:10pt;'> Nantes underperforms by being <br> 17th with the 8th budget. </span>"
																										    		), 
																										    inherit.aes = FALSE,
																										    hjust       = 0,
																										    vjust       = 0.5,
																										    fill        = "#333333",
																										    color       = "white"
																										  ) +
																				annotate("segment", x = 2,  xend = 2,  y = 10.17, yend = 10.77, colour = "white") +
																				annotate("segment", x = 17, xend = 17, y = 13.22, yend = 13.79, colour = "white") +
																				theme(panel.grid.major.y = element_blank(),
																							panel.grid.minor.y = element_blank(),
																							panel.grid.major.x = element_blank(),
																							panel.grid.minor.x = element_blank(),
																							panel.background   = element_rect(fill  = "#333333", 
																								  															color = "white"
																																								),
																							plot.background    = element_rect(fill  = "#333333",
																																								color = "#333333"
																																								),
																							axis.ticks.x       = element_blank(),
																							axis.title.x       = element_text(size   = 14,
																																								color  = "white",
																																								face   = "bold",
																																								margin = margin(20, 0, 0, 0)
																																								),
																							axis.text.x        = element_text(size   = 12,
																																								color  = "#F8C9EC",
																																								face   = "bold"),
																							axis.text.y        = element_markdown(size  = 12,
																																										margin = margin(0,30,0,0)
																																										),
																							axis.title.y       = element_text(size   = 14,
																																								color  = "white",
																																								face   = "bold",
																																								angle  = 0,
																																								vjust  = 1,
																																								margin = margin(0,-20,10,20)
																																								),
																							axis.ticks.y       = element_line(color  = "white"),
																							axis.ticks.length.y = unit(.025, "cm"),
																							axis.line.y        = element_line(arrow = arrow(length = unit(6, "pt"),
																																															type   = "closed",
																																															ends   = "last"),
																																								color = "white"
																																								),
																							plot.caption       = element_text(color = "#333333") ,
																						  plot.title         = element_text(margin = margin(0, 30, 10, 0),
																						  																	size   = 15, 
																						  																	face   = "bold",
																						  																	color  = "white",
																						  																	hjust  = 0.5
																						  																	),
																							plot.margin        = margin(10, 10, 10, 10),
																							plot.subtitle      = element_markdown(margin     = margin(10, 0, 30, 0),
																																										lineheight = 1.7
																																										)
																							) +
																				guides(color = 'none',
																							 fill  = 'none',
																							 alpha = 'none'
																							 ) 






# Graph 3 : History C1 Qualif ----



History <-  df_Qualif_Champions_League %>% filter(Team != "Paris") %>% 
						  ggplot(aes(Season, Team)) +
						  geom_star(aes(starshape = C1_Qualif,
						  							size      = C1_Qualif,
						  							fill      = Division
						  							),
						  					size = 3.75,
						  					color = "white"
						  					) +
						  scale_x_continuous(breaks = seq(2000,2022,4), 
						  									 limits = c(1998,2024),
						  									 expand = c(0, 0) 
						  									 ) +
							labs(title    = "The regular of the C1 against the outsider.",
									 subtitle = "<span style='color:#FFFFFF;font-size:12pt;'> Despite a lower frequency of qualification since 2013, Marseille remains an <br> 
									              experienced club while Lens moved up to Ligue 1 only 2 years ago. If Lens <br>
									              finishes 2nd,it would undoubtedly be one of the best performances of the century. </span>",
									 x        = "Year",
									 y        = "",
									 caption  = "Visualization : DENIAUX Maxime |  Data : worldfootball, sportune.20minutes, Wikipedia  | Inspiration : SCHERER Cédric") +
							scale_starshape_manual(values = c("Yes" = 1,
																								"No"  = 13) 
																		 ) +
							scale_fill_manual(values = c( "D1" = "#E4DD4F", 
						  														  "D2" = "#E4854F"
						  														)
						  								 ) +
							theme(panel.grid.minor.y = element_blank(),
										panel.grid.minor.x = element_blank(),
										panel.grid.major   = element_line(linetype = "dotted",
																											color    = "#B7C9FA" 
																											), 
										panel.background   = element_rect(fill  = "#333333", 
											  															color = "white"),
										plot.background    = element_rect(fill  = "#333333", 
																											color = "#333333"
																											),
										axis.ticks.x       = element_blank(),
										axis.title.x       = element_text(size   = 14,
																											color  = "#333333",
																											face   = "bold",
																											margin = margin(20, 0, 0, 0)
																											),
										axis.text.x        = element_text(size   = 12,
																											color  = "white",
																											face   = "bold"
																											),
										axis.text.y        = element_markdown(size  = 12,
																													color  = "white",
																													face   = "bold"
																													),
										axis.title.y       = element_text(size   = 11,
																											color  = "white",
																											face   = "bold",
																											angle  = 0,
																											vjust  = 1,
																											margin = margin(0,0, 50, 0)
																											),
										axis.ticks.y       = element_blank(),
										plot.caption       = element_text(hjust = 0.3, color = "#EDE3CA"),  
									  plot.title         = element_text(margin = margin(0, 30, 10, 0),
									  																	size   = 15, 
									  																	face   = "bold",
									  																	color  = "white",
									  																	hjust  = 0.5
									  																	),
										plot.margin        = margin(10, 10, 10, 30),
										plot.subtitle      = element_markdown(margin     = margin(10, 0, 30, 0),
																													lineheight = 1.7
																													),
										legend.background  = element_rect(fill = NA),
										legend.title       = element_text(color = "#EEC7F0"), 
										legend.key         = element_rect(fill = "transparent"),
										legend.text        = element_text(color = "white"),
										legend.spacing.y   = unit(0.25, 'cm')
										) + 
							guides(starshape = guide_legend(title = "C1 qualification", 
																							color = "white",
																							byrow = TRUE),
										 fill      = guide_legend(title = "Division",
										 										      override.aes = list(starshape = 15),
																							byrow = TRUE)
										 )
						        






# Save ----


bottom <-  plot_grid(Perf,History,
										 nrow        = 1, 
										 rel_widths  = c(0.50,0.50), 
										 rel_heights = c(0.45,0.55))

full  <-  plot_grid(Race,bottom, ncol = 1, rel_heights  = c(0.45,0.4))

ggsave(plot     = full, 
			 filename = "Plot.png", 
       width    = 15, 
			 height   = 20, 
			 dpi      = 300)





