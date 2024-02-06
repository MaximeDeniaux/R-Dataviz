
# Packages ----

library(rvest)
library(tidyverse)
library(cowplot)
library(ggtext)
library(lubridate)
library(gghighlight)
library(ggrepel)



# Directory ----

setwd("C:/Users/maxim/Documents/Site_Web/Git/R-Dataviz/Football/11. Goals_Distribution")


# Data ----




years               <- 1995:2020

url_leagues_matches <- c("https://www.transfermarkt.co.uk/ligue-1/gesamtspielplan/wettbewerb/FR1/saison_id/",
												 "https://www.transfermarkt.co.uk/premier-league/gesamtspielplan/wettbewerb/GB1/saison_id/",
												 "https://www.transfermarkt.co.uk/serie-a/gesamtspielplan/wettbewerb/IT1/saison_id/",
												 "https://www.transfermarkt.co.uk/bundesliga/gesamtspielplan/wettbewerb/L1/saison_id/",
												 "https://www.transfermarkt.co.uk/laliga/gesamtspielplan/wettbewerb/ES1/saison_id/"
												 )

Leagues             <- c("Ligue 1",
												 "Premier League",
												 "Serie A",
												 "Bundesliga",
												 "Liga"
												 )




# Scrap

y <- 1
u <- 1
i <- 2



Data <- data.frame(a = "",
									 b = "",
									 c = "",
									 d = "",
									 e = ""
 									 )

colnames(Data) <- c(colnames((read_html(paste(url_leagues_matches[u],years[y],sep = "")) %>% html_nodes('table') %>% .[1+i] %>% html_table(fill = T))[[1]] %>% select(1,3,5,7) %>% slice(-1) %>% as.data.frame()),"League")


for(y in 1:length(years)){

for(u in 1:length(url_leagues_matches)){
	
matchweek_number         <- length(read_html(paste(url_leagues_matches[u],years[y],sep = "")) %>% html_nodes('table'))

for(i in 2:matchweek_number){
	
Data <- bind_rows(Data,(read_html(paste(url_leagues_matches[u],years[y],sep = "")) %>% html_nodes('table') %>% .[i] %>% html_table(fill = T))[[1]] %>% select(1,3,5,7) %>% slice(-1) %>% as.data.frame() %>% mutate(League = Leagues[u]))

cat("i :",i,"  .  ",	"u :",u,"  .  ","y :",y,"\n")
			
	
}

}
}











# Data processing ----

colnames(Data) <- c('Date','HomeTeam','Result','AwayTeam','League')


# Remove useless lines

Data <- Data %>% slice(-1)

Data <- Data %>% mutate(Result = ifelse(nchar(Result) > 4,"",Result))
Data <- Data[-which(Data$Result == ""),]


# Edit HomeTeam & AwayTeam variables

i <- 1
for(i in 1:nrow(Data)){
	Data$HomeTeam[i] = strsplit(Data$HomeTeam[i],split = "[)]")[[1]][2]
}


i <- 1
for(i in 1:nrow(Data)){
	Data$AwayTeam[i] = strsplit(Data$AwayTeam[i],split = "[(]")[[1]][1]
}


i <- 1
for(i in 1:nrow(Data)){
  Data$AwayTeam[i] = substr(Data$AwayTeam[i],
  													1,
  													gregexpr(text = Data$AwayTeam[i],pattern = "[a-zA-Z]")[[1]][length(gregexpr(text = Data$AwayTeam[i],pattern = "[a-zA-Z]")[[1]])])
}


i <- 1
for(i in 1:nrow(Data)){
  Data$HomeTeam[i] = substr(Data$HomeTeam[i],
  													gregexpr(text = Data$HomeTeam[i],pattern = "[a-zA-Z]")[[1]][1],
                            nchar(Data$HomeTeam[i]))
}




# Edit Date variable 

i <- 1
for(i in 1:nrow(Data)){
	if(Data$Date[i] == ""){Data$Date[i] = Data$Date[i - 1]}
}

i <- 1
for(i in 1:nrow(Data)){
  Data$Date[i] = strsplit(x = Data$Date[i],split = "                                                ")[[1]][2]
}

Data$Date <- lubridate::mdy(Data$Date)

Data      <- Data %>% mutate(Year = year(Data$Date))


# I want the year to match the season, so for example the values 2015 and 2016 can both match the 2015-2016 
# and 2016-2017 season, I need to differentiate them. The 2016-2017 season will be the year 2016 for example.


Data <- Data %>% mutate(Yearbis = ifelse(as.numeric(substr(Date,6,7)) < 7, Year - 1, Year) ) 


# New variable :  Number of goals

Data <- separate(data = Data,col = "Result",into = c("HomeGoals","AwayGoals"),sep = ":",remove = FALSE)

Data <- Data %>% mutate(Goals_Number = as.numeric(HomeGoals) + as.numeric(AwayGoals))



# Final datas for plots

Data_Plot                <- Data %>% group_by(Yearbis, League) %>% summarise(Mean_goals = round(mean(Goals_Number),
																																																digits = 2
																																																)
																																						 )
Data_Plot_bis            <- Data %>% group_by(Yearbis) %>% summarise(Mean_goals = round(mean(Goals_Number),
																																												digits = 2
																																												)
																																		 )
Mean_Goals_All_Countries <- mean(Data_Plot_bis$Mean_goals)


# For the 2nd plot, I need a classification of Score variable (because 66 differents score is too much)
# For example, I will combine 1-2 and 2-1, 0-1 and 1-0...

Data_Classif <- Data$Result %>% 
										table() %>% 
										as.data.frame() %>% 
										rename(Score = ".") %>%
										separate(col = "Score",into = c("HomeGoals","AwayGoals") ,sep = ":",remove = F) %>% 
										mutate(Total_Goals = as.numeric(HomeGoals) + as.numeric(AwayGoals)) %>% 
										arrange(Total_Goals)


# Algo to know wich result is duplicate. 

i <- 1
ibis <- 2
Data_Classif$same_values = ""
for(i in 1:nrow(Data_Classif)){
	for(ibis in 1:nrow(Data_Classif)){
		if((length(c(setdiff(c(Data_Classif$HomeGoals[i],Data_Classif$AwayGoals[i]),
											 c(Data_Classif$HomeGoals[ibis],Data_Classif$AwayGoals[ibis])
											),
								 setdiff(c(Data_Classif$HomeGoals[ibis],Data_Classif$AwayGoals[ibis]),
								 				 c(Data_Classif$HomeGoals[i],Data_Classif$AwayGoals[i])
											  )
								 )
		          ) == 0) && (ibis != i)
		){Data_Classif$same_values[i] = "Yes"
			break
			}
	}
	if(i == nrow(Data_Classif)){Data_Classif$same_values[which(Data_Classif$same_values == "")] = "No"}
}

Data_Classif

# Now we can combine results to create new groups result

Data_2nd_plot <- Data
	
Data_2nd_plot <- Data_2nd_plot %>% mutate(ResultBis = case_when(Result %in% c("0:1","1:0") ~ "1:0 - 0:1",
																																Result %in% c("0:2","2:0") ~ "2:0 - 0:2",
																																Result %in% c("0:3","3:0") ~ "3:0 - 0:3",
																																Result %in% c("2:1","1:2") ~ "2:1 - 1:2",
																																Result %in% c("0:4","4:0") ~ "4:0 - 0:4",
																																Result %in% c("3:1","1:3") ~ "3:1 - 1:3",
																																Result %in% c("0:5","5:0") ~ "5:0 - 0:5",
																																Result %in% c("4:1","1:4") ~ "4:1 - 1:4",
																																Result %in% c("3:2","2:3") ~ "3:2 - 2:3",
																																Result %in% c("0:6","6:0") ~ "6:0 - 0:6",
																																Result %in% c("5:1","1:5") ~ "5:1 - 1:5",
																																Result %in% c("4:2","2:4") ~ "4:2 - 2:4",
																																Result %in% c("0:7","7:0") ~ "7:0 - 0:7",
																																Result %in% c("6:1","1:6") ~ "6:1 - 1:6",
																																Result %in% c("5:2","2:5") ~ "5:2 - 2:5",
																																Result %in% c("4:3","3:4") ~ "4:3 - 3:4",
																																Result %in% c("0:8","8:0") ~ "8:0 - 0:8",
																																Result %in% c("7:1","1:7") ~ "7:1 - 1:7",
																																Result %in% c("6:2","2:6") ~ "6:2 - 2:6",
																																Result %in% c("5:3","3:5") ~ "5:3 - 3:5",
																																Result %in% c("0:9","9:0") ~ "9:0 - 0:9",
																																Result %in% c("8:1","1:8") ~ "8:1 - 1:8",
																																Result %in% c("7:2","2:7") ~ "7:2 - 2:7",
																																Result %in% c("6:3","3:6") ~ "6:3 - 3:6",
																																Result %in% c("5:4","4:5") ~ "5:4 - 4:5",
																																Result %in% c("9:1","1:9") ~ "9:1 - 1:9",
																																Result %in% c("8:2","2:8") ~ "8:2 - 2:8",
																																T ~Result
																														)
																					)

# Create the freq result 

data_freq_league <- Data_2nd_plot %>% group_by(League,Goals_Number) %>% summarise(n = n()) %>% group_by(League) %>% mutate(freq_League = n/sum(n)) 

data_freq_score <- Data_2nd_plot %>% group_by(League,ResultBis) %>% summarise(n = n()) %>% group_by(League) %>% mutate(freq_score = n/sum(n)) 
data_freq_score <- left_join(data_freq_score,
														 Data_2nd_plot[,c('ResultBis','Goals_Number')] %>% distinct()
														 )
data_freq_score <- left_join(data_freq_score,
														 data_freq_league[,c('League','Goals_Number','freq_League')], 
														 by = c('League','Goals_Number')
														 )






# Plots ----

# Labels facet wrap

Facet_Labels <- c(`Bundesliga`      = "<span style = 'color:#F8766D;'>BUNDESLIGA</span>",
                  `Liga`            = "<span style = 'color:#A3A500;'>LIGA</span>",
                  `Ligue 1`         = "<span style = 'color:#00BF7D;'>LIGUE 1</span>",
                  `Premier League`  = "<span style = 'color:#00B0F6;'>PREMIER LEAGUE</span>",
                  `Serie A`         = "<span style = 'color:#E76BF3;'>SERIE A</span>"
                  )

# Plot 

Top_Plot = ggplot(Data_Plot) + 
	            # Main lines
							geom_line(aes(Yearbis, Mean_goals, color = League), linewidth = 1.75, alpha = 0.6) +
	            # End lines points
							geom_point(data  = Data_Plot %>% filter(Yearbis == 2020), aes(Yearbis, Mean_goals, color = League), 
												 size  = 3, 
												 alpha = 0.8
												 ) +
							geom_point(data  = Data_Plot %>% filter(Yearbis == 1995), aes(Yearbis, Mean_goals, color = League), 
												 size  = 3, 
												 alpha = 0.8
												 ) +
	            # Mean values line
							geom_line(data      = Data_Plot_bis, aes(Yearbis, Mean_goals),
												color     = "#504D4D", 
												linewidth = 1, 
												alpha     = 0.45
												) +
							geom_point(data  = Data_Plot_bis %>% filter(Yearbis == 2020), aes(Yearbis, Mean_goals),
												 color = "#7D7676", 
												 size  = 3
												 ) +
							geom_point(data  = Data_Plot_bis %>% filter(Yearbis == 1995), aes(Yearbis, Mean_goals),
												 color = "#7D7676", 
												 size  = 3
												 ) +
	            # Secondary lines
						  gghighlight(label_key            = Mean_goals,
						  						line_label_type      = "ggrepel_text",
						  						unhighlighted_params = list(linewidth = 1.15, 
						  																				color     = "#C2BFBF"
						  																				), 
						  						label_params         = list(fontface      = "bold",
										  																size          = 6.5,
										  																nudge_x       = 2.05,
										  																force         = 0.5,
										  																direction     = "x",
										  																segment.color = NA
										  																)
						  						) +
	            # Points annotations
	            geom_text_repel(data = Data_Plot %>% filter(Yearbis == 1995), 
	            								aes(Yearbis, Mean_goals, 
	            										label     = Mean_goals,
	            										color     = League,
	            										fontface  = "bold"),
	            								nudge_x       = -1.95,
	            								force         = 0.5, 
	            								size          = 6.5, 
	            								direction     = "x", 
	            								segment.color = NA
	            								) +
							# Scales
	            scale_y_continuous(breaks = c(2.00,2.25,2.50,2.75,3.00,3.25),
																 limits = c(2,3.3),
																 labels = paste(c("","","mean of the 5 leagues","","",""))
																 ) +
							scale_x_continuous(breaks = seq(1995,2020,5),
																 limits = c(1990,2025),
																 expand = c(0,0)
																 ) +
	            # Titles
							labs(x = "", y = "") + 
	            # Theme
							theme(panel.background   = element_rect(fill  = "#F4F5F1", 
																											color = NA
																											),
										plot.background    = element_rect(fill  = "#F4F5F1", 
																											color = NA
																											),
										panel.grid.major.y = element_line(color     = "#D8D8D8",
																											linewidth = 0.15
																											), 
										panel.grid.minor.y = element_blank(), 
										panel.grid.major.x = element_blank(), 
										panel.grid.minor.x = element_blank(), 
										axis.text.x        = element_text(size = 15),
										axis.ticks.y       = element_blank(), 
										axis.line.x        = element_line(linewidth = 0.35, 
																											linetype  = "solid", 
																											colour    = "black"
																											),
										axis.text.y        = element_text(margin = margin(r = -45), 
																											color  = "#504D4D", 
																											size   = 20, 
																											vjust  = -1
																											), 
										plot.title         = element_text(hjust = 0.45),
										plot.margin        = margin(r = 80, b = 0),
										panel.spacing.x    = unit(4.25, "lines"),
										strip.background   = element_rect(fill = NA),
										strip.text         = element_markdown(face = "bold",
																													size = 20
																													)
										) + 
	             # Facets
			         facet_wrap(~League,nrow = 1,
			         					  labeller     = as_labeller(Facet_Labels)
			         					 )



Bottom_plot <- ggplot(data_freq_score,
											aes(fct_reorder(ResultBis,Goals_Number),freq_score)) +                       
								  geom_bar(aes(y = 0.22,fill = as.character(Goals_Number)),
								  				 stat    = "identity",
								  				 width   = 1,
								  				 colour  = "white",
								  				 alpha   = 0.35
								  				 ) +                                                                                           #change alphe to make it more or less visible
								  geom_bar(aes(fill = as.character(Goals_Number)),
								  				 stat   = "identity",
								  				 width  = 1,
								  				 colour = "white"
								  				 ) +                     
								  coord_radial(expand = FALSE) +                                                    
								  geom_label(aes(label = ifelse(freq_score >= 0.06,paste(100*round(freq_score,digits = 2),"%"),NA),
								  							 fill  = as.character(Goals_Number)),
								  					 size        = 2.5,
								  					 color       = "white",
								  					 show.legend = FALSE,
								  					 nudge_y     = 0.01
								  					 ) +    
								  scale_fill_manual(values=c("0"  = "#ff0000",
								 														 "1"  = "#ff8000", 
								 													   "2"  = "#E1D930", 
								 													   "3"  = "#80ff00", 
								 													   "4"  = "#00ff00", 
								 													   "5"  = "#00ff80", 
								 													   "6"  = "#00ffff", 
								 													   "7"  = "#0080ff", 
								 													   "8"  = "#0000ff",
								 													   "9"  = "#8000ff", 
								 													   "10" = "#ff00ff", 
								 													   "11" = "#ff0080", 
								 													   "12" = "#AD3358"
								 													  )
								 									 ) +
								  scale_y_continuous(limits = c(0,0.23))+                                                 
								  labs(fill    = "",                                                                         
								       caption = "", 
								  		 y       = "mean of the 5 leagues" 
								  		 ) +                                                    
								  theme_minimal() +                                                                     
								  theme(legend.position    = "top",
								        axis.title.y       = element_text(color = "#F4F5F1", 
								        																	angle = 0, 
								        																	size  = 13.5),
								        axis.title.x       = element_blank(),
								        axis.text.y        = element_blank(),
								        axis.text.x        = element_text(size  = 12,
								        																	face  = "bold",
								        																	vjust = 10
								        																	),  
								  			plot.title         = element_text(hjust = 0.5),
								        plot.caption       = element_text(hjust = 0.5, 
								        																	size  = 6
								        																	),
								        panel.grid.major   = element_blank(), 
								        panel.grid.minor   = element_blank(),
								  			panel.background   = element_rect(fill  = "#F4F5F1", 
																											    color = NA
																											    ),
										    plot.background    = element_rect(fill  = "#F4F5F1", 
																											    color = NA
																											    ),
								  			strip.text         = element_blank(),
										    plot.margin        = margin(r = 60, b = 0, t = 0),
										    panel.spacing.x    = unit(2.5, "lines")
								  			) + 
									 guides(fill = 'none',
									 			  theta = guide_axis_theta(angle = 90)
									 			 ) +
									 # Facets
									 facet_wrap(~League, nrow = 1)




# Title 

Title <-  ggplot(data.frame(x = 1, y = 1)) + 
							theme_void() +
							labs(title = "Distribution of the number of goals by match and scores in the 5 major leagues between 1995 and 2020") +
							theme(panel.background      = element_rect(fill  = "#F4F5F1",
																												 color = "#F4F5F1"
																												 ),
										plot.background       = element_rect(fill  = "#F4F5F1",
																												 color = "#F4F5F1"
																												 ), 
										plot.title            = element_text(hjust  = 0.5, 
																												 color  = "gray30", 
																												 size   = 40, 
																												 face   = "bold",
																												 margin = margin(0,0,0,0)
																												 ),
										plot.margin           = margin(t = 40, b = 30)
										)



# Caption 

Caption <-  ggplot(data.frame(x = 1, y = 1)) + 
	            annotate('rect',
	            				 xmin  = 0.3, xmax = 0.7,   
	            				 ymin  = 4,   ymax = 9, 
	            				 alpha = 0.95, 
	            				 fill  = "gray80", 
	            				 color = "gray40"
	            				 ) +
	            annotate('rect', 
	            				 xmin  = 0.345, xmax = 0.395, 
	            				 ymin  = 5,     ymax = 6, 
	            				 fill  = "white", 
	            				 color = "white"
	            				 ) +
	            annotate('text', 
	            				 label = "The result does not exist", 
	            				 x     = 0.37, 
	            				 y     = 7,
	            				 size  = 5
	            				 ) +
	            annotate('rect', 
	            				 xmin  = 0.475, xmax = 0.525,
	            				 ymin  = 5,     ymax = 6,
	            				 alpha = 0.3,
	            				 fill  = "gray20",
	            				 color = "white"
	            				 ) +
	            annotate('text', 
	            				 label = "The result exist", 
	            				 x     = 0.5,
	            				 y     = 7,
	            				 size = 5
	            				 ) +
	            annotate('rect', 
	            				 xmin  = 0.61, xmax = 0.66,
	            				 ymin  = 5,    ymax = 6,
	            				 alpha = 0.3,
	            				 fill  = "gray20", 
	            				 color = "white"
	            				 ) +
	            annotate('rect',
	            				 xmin  = 0.61, xmax = 0.63, 
	            				 ymin  = 5,    ymax = 6,
	            				 alpha = 0.8,
	            				 fill  = "gray20", 
	            				 color = "white"
	            				 ) +
	            annotate('text', 
	            				 label = "Result proportion", 
	            				 x     = 0.635, 
	            				 y     = 7,
	            				 size  = 5
	            				 ) +
	            scale_y_continuous(limits = c(0,10)) +
	            scale_x_continuous(limits = c(0,1)) +
							theme_void() +
							labs(caption = "Visualisation by DENIAUX Maxime   |   Data : Transfermarkt   |   Inspiration : Gilbert Fontana [Consumer Confidence] & Getting Blue Fingers [Percentile Radars/Pizza’s]") +
							theme(panel.background      = element_rect(fill  = "#F4F5F1",
																												 color = "#F4F5F1"
																												 ),
										plot.background       = element_rect(fill  = "#F4F5F1",
																												 color = "#F4F5F1"
																												 ), 
										plot.caption          = element_text(hjust  = 0.5, 
																												 color  = "gray30", 
																												 size   = 20, 
																												 margin = margin(0,0,0,0)
																												 ),
										plot.margin           = margin(t = 0, b = 20)
										)






# Save ----



ggsave(plot = plot_grid(Title,
												Top_Plot,
												Bottom_plot,
												Caption,
												nrow        = 4,
												rel_heights = c(0.13,0.25,0.52,0.10)
												),
			 filename = "Plot.png",
			 width    = 35,
			 height   = 20,
			 bg       = "#F4F5F1",
			 res = 320)
			 






