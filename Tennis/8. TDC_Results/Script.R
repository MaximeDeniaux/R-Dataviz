


# Packages ----


library("rvest")
library("tidyverse")
library("gganimate")
library("ggrepel")
library("ggtext")



# Directory ----

setwd("C:/Users/maxim/Documents/Site_Web/Git/R-Dataviz/Tennis/8. TDC_Results")



# Data ----


##  Session ----

url   <- html_session("http://www.tennisdrawchallenge.com/login")

login <- url %>% 
							  html_node("form") %>% 
							  html_form() %>%
							  html_form_set(email    = "...", # to complete
							  							password = "...") # to complete




## Profils ----


# Tables

url_romain <-  paste('http://www.tennisdrawchallenge.com/user/profile/romybalt/brackets/2023')
url_pierre <-  paste('http://www.tennisdrawchallenge.com/user/profile/vauv30/brackets/2023')
url_max    <-  paste('http://www.tennisdrawchallenge.com/user/profile/maximusmass/brackets/2023')
url_gimpel <-  paste('http://www.tennisdrawchallenge.com/user/profile/gimpel/brackets/2023')



table_romain <- url %>% submit_form(login) %>% session_jump_to(url = url_romain) %>% html_nodes('table') %>% .[2] %>% html_table(fill = T) %>% as.data.frame()
table_pierre <- url %>% submit_form(login) %>% session_jump_to(url = url_pierre) %>% html_nodes('table') %>% .[2] %>% html_table(fill = T) %>% as.data.frame()
table_max    <- url %>% submit_form(login) %>% session_jump_to(url = url_max)    %>% html_nodes('table') %>% .[2] %>% html_table(fill = T) %>% as.data.frame()
table_gimpel <- url %>% submit_form(login) %>% session_jump_to(url = url_gimpel) %>% html_nodes('table') %>% .[2] %>% html_table(fill = T) %>% as.data.frame()



table_romain$Percent.Correct <-  gsub("%","",table_romain$Percent.Correct)
table_romain$Percent.Correct <-  table_romain$Percent.Correct %>% as.numeric()
table_romain$Player          <-  "Romybalt"


table_pierre$Percent.Correct <- gsub("%","",table_pierre$Percent.Correct)
table_pierre$Percent.Correct <- table_pierre$Percent.Correct %>% as.numeric()
table_pierre$Player          <- "Vauv30"


table_max$Percent.Correct <- gsub("%","",table_max$Percent.Correct)
table_max$Percent.Correct <- table_max$Percent.Correct %>% as.numeric()
table_max$Player          <- "Maximusmass"

table_gimpel$Percent.Correct <- gsub("%","",table_max$Percent.Correct)
table_gimpel$Percent.Correct <- table_max$Percent.Correct %>% as.numeric()
table_gimpel$Player          <- "gimpel"





# Bind rows

full_table <- bind_rows(table_max,table_pierre,table_romain,table_gimpel)




# Data processing ----


# Tournament level

url_tournaments_level    <-  paste('http://www.tennisdrawchallenge.com/standings/view/2023')
table_tournaments_level  <-  url %>% submit_form(login) %>% session_jump_to(url = url_tournaments_level) %>% html_nodes('table') %>% .[1] %>% html_table(fill = T) %>% as.data.frame()


# Join

full_table <-  left_join(full_table,
											   table_tournaments_level[,c('Tournament','Tier')],
											   by = "Tournament")




# Cleaning 

full_table$Tournament <-  gsub("2023 ","",full_table$Tournament)

full_table$Rank       <-  gsub("#","",full_table$Rank)

full_table            <-  separate(full_table, col = Rank, into = c('Rank','Players_Number'), sep = " / ")

colnames(full_table)  <-  c("Tournament","Rank","Players_Number","Points",
												    "Correct_Picks","Incorrect_Picks","Percent_Correct",
												    "Player","ATP_lvl"
														)

full_table            <-  full_table[, c("Player","Tournament","ATP_lvl","Rank","Players_Number","Points",
										                		 "Correct_Picks","Incorrect_Picks","Percent_Correct"
												                 )
																		 ]




# Tournament number

full_table <- full_table %>% group_by(Player) %>% mutate(Tournament_Number = n():1)


# One line is missing for RomyBalt

full_table %>% group_by(Player) %>% summarise(max = max(Tournament_Number))
full_table <- full_table %>% select(-Tournament_Number)

setdiff(full_table$Tournament[full_table$Player == "Vauv30"],
				full_table$Tournament[full_table$Player == "Romybalt"])


# Adding manually the missing line :

full_table                        <- as.data.frame(full_table)

full_table[nrow(full_table) + 1,] <- c("Romybalt",
																			 "Cincinnati",
																			 "ATP 1000",
																			 NA,
																			 464,
																			 0,
																			 NA,
																			 NA,
																			 NA)
	




# Class & factor variables

full_table$ATP_lvl              <- factor(x      = full_table$ATP_lvl, 
																			    levels = unique(full_table$ATP_lvl)
																			    )
full_table$Tournament           <- factor(x      = full_table$Tournament, 
																			    levels = rev(unique(full_table$Tournament[full_table$Player == "Maximusmass"]))
																			   )
full_table$Rank                <- full_table$Rank              %>% as.numeric()
full_table$Players_Number      <- full_table$Players_Number    %>% as.numeric()
full_table$Points              <- full_table$Points            %>% as.numeric()
full_table$Correct_Picks       <- full_table$Correct_Picks     %>% as.numeric()
full_table$Incorrect_Picks     <- full_table$Incorrect_Picks   %>% as.numeric()
full_table$Percent_Correct     <- full_table$Percent_Correct   %>% as.numeric()


# Add tournament number variable

full_table <- full_table %>% arrange(Player, Tournament)
full_table <- full_table %>% group_by(Player) %>% mutate(Tournament_Number = 1:n())


# Add fake lines for the tournament "0"

full_table[nrow(full_table) + 1,] <- list("Maximusmass", "Adelaide", "ATP 250",NA,NA,0,NA,NA,NA,0)
full_table[nrow(full_table) + 1,] <- list("Romybalt", "Adelaide", "ATP 250",NA,NA,0,NA,NA,NA,0)
full_table[nrow(full_table) + 1,] <- list("Vauv30", "Adelaide", "ATP 250",NA,NA,0,NA,NA,NA,0)
full_table[nrow(full_table) + 1,] <- list("gimpel", "Adelaide", "ATP 250",NA,NA,0,NA,NA,NA,0)



# Cumsum of Points variable

full_table <- full_table %>% group_by(Player) %>% arrange(Tournament_Number) %>% mutate(cum_points = cumsum(Points))


# Gap points variable between Gimpel and the others

full_table                                              <- full_table %>% group_by(Tournament_Number) %>% mutate(gap_points = cum_points - max(cum_points))
full_table$gap_points                                   <- paste(full_table$gap_points," Pts",sep="")
full_table$gap_points[full_table$gap_points == "0 Pts"] <- ""


# Ranking by tournament

full_table <- full_table %>% group_by(Tournament_Number) %>% arrange(desc(cum_points)) %>% mutate(ranking = 1:n())


# Plot ----


plot1 <- ggplot(full_table %>% mutate(label = ifelse(Tournament_Number == 64, Player, NA_character_)) ) +
					  geom_line(aes(x     = Tournament_Number,
					  							y     = cum_points,
					  							group = Player,
					  							color = Player),
					  					linewidth = 1.25,
					  					alpha     = 0.5
					  					) +
					  geom_text_repel(aes(x     = Tournament_Number + 0.5, 
					  										y     = cum_points, 
					  										label = label, 
					  										color = Player
					  										),
					  								size          = 3.5,
					  								direction     = "y",
					  								hjust         = 0,
					  								segment.color = NA
					  								) +
					  scale_x_continuous(breaks = seq(0, 65, 5),
					  									 limits = c(0, 75),
					  									 expand = c(0, 0)
					  									 ) +
						scale_y_continuous(breaks = seq(0, 3250, 250),
					  									 limits = c(0, 3300),
					  									 expand = c(0, 0)
															 ) +
					  scale_color_manual(values = c("Maximusmass" = "#E7C911", 
					  														  "Vauv30"      = "#DE4B41",
					  														  "Romybalt"    = "#009EDD",
					  														  "gimpel"      = "#DFDFDF"
					  														  )
					  									) +
					  labs(title    = "2023 Tennis Draw Challenge Season",
					  		 subtitle = "Tennis Draw Challenge is a free tennis betting game. You must fill out your draw before each tournament begins.<br> Below is the evolution of the number of points between 4 players (the 1st in the final ranking, me, and 2 friends)",
					  		 x        = "Tournament number", 
					  		 y        = "Total points", 
					  		 caption  = "Visualization by DENIAUX Maxime | Data : TennisDrawChallenge "
					  		 ) +
						theme(panel.background   = element_rect(fill  = "#333333", 
																										color = "white"
																										),
								  plot.background    = element_rect(fill  = "#333333", 
																									  color = "#333333"
										  															),
									axis.ticks.x         = element_blank(),
									axis.ticks.y         = element_blank(),
									axis.title.x         = element_text(size   = 12,
																										  color  = "white",
																										  face   = "bold",
																										  margin = margin(20, 0, 0, 0)
																										  ),
									axis.title.y       = element_text(size   = 12,
																										color  = "white",
																										face   = "bold",
																										angle  = 0,
																										vjust  = 0.5,
																										margin = margin(0, 20, 0, 0)
																										),
									axis.text.y        = element_text(size   = 11,
																										color  = "white",
																										face   = "bold"
																										),
									axis.text.x        = element_text(size   = 11,
																										color  = "white",
																										face   = "bold"
																										),
									panel.grid.major.x = element_blank(),
									panel.grid.minor.x = element_blank(),
									panel.grid.major.y = element_blank(),
									panel.grid.minor.y = element_blank(),
									plot.title         = element_markdown(margin = margin(10, 0, 35, 0),
												    									        	size   = 20, 
												    														face   = "bold", 
												    														color  = "#E7BB62",
												    														hjust  = 0.5
												    														),
									plot.subtitle         = element_markdown(margin     = margin(10, 0, 25, 0),
												    									        		 size       = 11.5, 
												    															 face       = "bold", 
												    															 color      = "gray95",
												    															 hjust      = 0.25,
												    															 lineheight = 1.5,
												    															 ),
									plot.caption         = element_markdown(margin = margin(25, 0, -5, 0),
												    									        	  size   = 8.75, 
												    															face   = "bold", 
												    															color  = "gray95",
												    															hjust  = 0.5
												    															),
									plot.margin        = margin(30, 30, 30, 30)
									) + 
	          guides(color = 'none')



plot2 <-  ggplot(full_table, aes(ranking, 
																 group = Player,
																 fill = as.factor(Player), 
																 color = as.factor(Player)
																 )
								 ) +
									  geom_tile(aes(y      = cum_points/2,
									                height = cum_points,
									                width  = 0.9
									  							),
									  					alpha = 0.5,
									  					color = NA
									  					) +
									  geom_text(aes(y     = 0, 
									  							label = paste(Player, " "), 
									  							color = Player
									  							), 
									  					vjust = 0.2, 
									  					hjust = 1, 
									  					size  = 5.5
									  					
									  					) +
									  geom_text(aes(y     = cum_points + 200 , 
									  							label = gap_points
									  							), 
									  					vjust = 0.2, 
									  					hjust = 1, 
									  					size  = 5.5
									  					) +
									  coord_flip(clip   = "off", 
									  					 expand = FALSE
									  					 ) +
									  scale_y_continuous(labels = c("0 Pt", "1 000 Pts", "2 000 Pts","3 000 Pts")) +
									  scale_x_reverse() +
									  guides(color = FALSE, 
									  			 fill  = FALSE
									  			 ) +
									  labs(title    = "2023 Tennis Draw Challenge Season",
									  		 subtitle = "Tennis Draw Challenge is a free tennis betting game. You must fill out your draw before each tournament begins.<br> Below is the evolution of the number of points between 4 players (the 1st in the final ranking, me, and 2 friends)",
									  		 tag      = paste("Tournament n° : ",'{closest_state}',sep = ""), 
									  		 x        = "", 
									  		 y        = "", 
									  		 caption  = "Visualization by DENIAUX Maxime | Data : TennisDrawChallenge | Inspiration : Jon Spring - Stackoverflow (nov 2018)"
									  		 ) +
									  scale_fill_manual(values = c("Maximusmass" = "#E7C911", 
									  														 "Vauv30"      = "#DE4B41",
									  														 "Romybalt"    = "#009EDD",
									  														 "gimpel"      = "#DFDFDF"
									  														 )
									  									) +
									  scale_color_manual(values = c("Maximusmass" = "#E7C911", 
									  														 "Vauv30"      = "#DE4B41",
									  														 "Romybalt"    = "#009EDD",
									  														 "gimpel"      = "#DFDFDF"
									  														 )
									  									) +
									  theme(panel.background   = element_rect(fill  = "#333333", 
									  																				color = "white"
									  																				),
												  plot.background    = element_rect(fill  = "#333333", 
												  																	color = "#333333"
												  																	),
									  			axis.ticks.x         = element_blank(),
									  			axis.ticks.y         = element_blank(),
									  			axis.title.x         = element_text(size   = 16,
														        																	color  = "white",
														        																	face   = "bold",
														        																	margin = margin(20, 0, 0, 0)
														        																	),
									  			axis.title.y       = element_text(size   = 14,
														        																	color  = "white",
														        																	face   = "bold",
														        																	angle  = 0,
														        																	vjust  = 0.5,
														        																	margin = margin(0, 0, 0, 0)
														        																	),
									  			axis.text.y        = element_blank(),
									  			axis.text.x        = element_text(size   = 15,
														        																	color  = "white",
														        																	face   = "bold"
														        																	),
									  			panel.grid.major.x = element_blank(),
									  			panel.grid.minor.x = element_blank(),
									  			panel.grid.major.y = element_blank(),
									  			panel.grid.minor.y = element_blank(),
									  			plot.title         = element_markdown(margin = margin(10, 0, 35, 0),
									  																						size   = 20, 
																    														face   = "bold", 
																    														color  = "#E7BB62",
																    														hjust = 0.5
																    														),
									  			plot.subtitle      = element_markdown(margin = margin(10, 0, 25, 0),
																    									        	size   = 14.5, 
																    														face   = "bold", 
																    														color  = "gray95",
																    														hjust = 0,
																    														lineheight = 1.5,
																    														),
									  			plot.caption         = element_markdown(margin = margin(25, 0, -5, 0),
																    									        		size   = 12.5, 
																    															face   = "bold", 
																    															color  = "gray95",
																    															hjust = 0.5
																    															),
									  			plot.margin        = margin(30, 80, 30, 150),
									  			plot.tag           = element_markdown(hjust = 1, 
																    														size = 14.5, 
																    														face   = "bold",
																    														color = "#B8FCA1"
																    														),
									  			plot.tag.position = c(0.99,0.89),
									  			) + 
										transition_states(Tournament_Number,
																			transition_length = 5, 
																			state_length      = 1.5
																			)  + 
										enter_fly(y_loc = -5) + 
										exit_fly(y_loc  =  -5) +
									  ease_aes('linear')


# Save plots ----



ggsave(plot     = plot1,
			 filename = "Plot_Static.png",
			 width    = 12,
			 height   = 10
			 )

anim_save(filename = "Plot_Animate.gif",
					animate(plot2, 
									fps          = 20, 
									duration     = 40,
									end_pause    = 5,
									start_pause  = 5,
									width        = 1250,
									height       = 800
									)
			 	 )
