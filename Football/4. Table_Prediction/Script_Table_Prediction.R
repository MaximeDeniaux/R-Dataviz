

# Packages ----

library(dplyr)
library(gt)
library(readr)
library(webshot2)


# Directory

setwd("C:/Users/maxim/Documents/Site_Web/Git/R-Dataviz/Football/4. Table_Prediction")


# Data ----


data <-  read_delim("data.txt", delim = ";", escape_double = FALSE, trim_ws = TRUE)



# GT table ----

dplyr::tibble(Round     = data$Round,
						  Equipe_1  = data$HomeTeam,
						  Versus    = rep('vs.',nrow(data)),
						  Equipe_2  = data$AwayTeam,
						  Prob_Home = data$H,
						  Prob_Draw = data$D,
						  Prob_Away = data$A,
						  To_Play   = data$To_Play
						  ) %>% 
									  gt(id = "FR_23_24") %>%
									  tab_header(title = md("<br>**Ligue 1 predictions**<br><br>")) %>%
									  text_transform(locations = cells_body(columns = c(Equipe_1,Equipe_2,To_Play)),
									  							 fn = function(x) {local_image(filename = paste(x,
									  							 																							 '.png',
									  							 																							 sep = ""
									  							 																							 ),
									  							 															height = as.numeric(50)
									  							 															)
									  							 	                 }
									  							 ) %>%
									  cols_label(Round     = "Matchweek",
									             Equipe_1  = "",
									             Equipe_2  = "",
									             Versus    = "",
									             Prob_Home = "Home Win",
									             Prob_Draw = "Draw",
									             Prob_Away = "Away Win",
									             To_Play  = "Confidence"
									  					 ) %>% 
									  cols_width(Round     ~ px(120),
													     Equipe_1  ~ px(120),
													     Equipe_2  ~ px(120),
													     Versus    ~ px(20),
													     Prob_Home ~ px(120),
													     Prob_Draw ~ px(120),
													     Prob_Away ~ px(120),
													     To_Play   ~ px(120)
									  					 ) %>% 
									  cols_align(align = "center",
									  					 columns = Round
									  					 ) %>% 
									  cols_align(align = "center",
									  					 columns = Equipe_1
									  					 ) %>% 
									  cols_align(align = "center",
									  					 columns = Equipe_2
									  					 ) %>% 
									  cols_align(align = "center",
									  					 columns = Versus
									  					 )%>% 
									  cols_align(align = "center",
									  					 columns = c(Prob_Home,Prob_Draw,Prob_Away)
									  					 ) %>% 
									  cols_align(align = "center",
									  					 columns = To_Play
									  					 ) %>% 
									  fmt_percent(columns = c(Prob_Home,Prob_Draw,Prob_Away),
									  						decimals = 2
									  						) %>% 
									  data_color(columns = c(Prob_Home),
									  					 colors = scales::col_numeric(
									  					 	palette = c("#f8eadb","#669a82"),
									  					 	domain = c(0,1)
									  					 	)
									  					 ) %>% 
									  data_color(columns = c(Prob_Draw),
									  					 colors = scales::col_numeric(
									  					 	palette = c("#f8eadb","#d0e0e3"),
									  					 	domain = c(0,0.7)
									  					 	)
									  					 ) %>% 
									  data_color(	columns = c(Prob_Away),
									  						colors = scales::col_numeric(
									  							palette = c("#f8eadb","#983737"),
									  							domain = c(0,1)
									  							)
									  						) %>%
									   opt_css( css = "
									  
									  #FR_23_24 .gt_col_heading {
									  color: #ffffff;
									  background-color: #242A2F;
									  font-size: 100%;
									  font-weight: normal;
									  text-transform: inherit;
									  border-left-style: none;
									  border-left-width: 1px;
									  border-left-color: #D3D3D3;
									  border-right-style: none;
									  border-right-width: 1px;
									  border-right-color: #D3D3D3;
									  vertical-align: middle;
									  padding-top: 15px;
									  padding-bottom: 15px;
									  padding-left: 5px;
									  padding-right: 5px;
									  overflow-x: hidden;
									}
									
									#FR_23_24 .gt_heading {
									    background-color: #F7FAB7;
									    text-align: center;
									    border-bottom-style: solid;
									    border-bottom-width: 2px;
									    border-bottom-color: #242A2F;
									    border-left-style: solid;
									    border-left-width: 2px;
									    border-left-color: #242A2F;
									    border-right-style: solid;
									    border-right-width: 2px;
									    border-right-color: #242A2F;
									    border-top-style: solid;
									    border-top-width: 2px;
									    border-top-color: #242A2F;
									}
									
									#FR_23_24 .gt_col_headings {
									    border-top-style: solid;
									    border-top-width: 2px;
									    border-top-color: #242A2F;
									    border-bottom-style: solid;
									    border-bottom-width: 2px;
									    border-bottom-color: #242A2F;
									    border-left-style: solid;
									    border-left-width: 2px;
									    border-left-color: #242A2F;
									    border-right-style: solid;
									    border-right-width: 2px;
									    border-right-color: #242A2F;
									}
									
									#FR_23_24 .gt_row {
									  padding-top: 8px;
									  padding-bottom: 8px;
									  padding-left: 5px;
									  padding-right: 5px;
									  margin: 10px;
									  border-bottom-style: solid;
									  border-bottom-width: 1px;
									  border-bottom-color: #bcbcbc;
									  border-left-style: none;
									  border-left-width: 1px;
									  border-left-color: #D3D3D3;
									  border-right-style: none;
									  border-right-width: 1px;
									  border-right-color: #D3D3D3;
									  vertical-align: middle;
									  overflow-x: hidden;
									}
									
									#FR_23_24 .gt_table_body {
									    border-top-style: solid;
									    border-top-width: 2px;
									    border-top-color: #242A2F;
									    border-bottom-style: solid;
									    border-bottom-width: 2px;
									    border-bottom-color: #242A2F;
									    border-left-style: solid;
									    border-left-width: 2px;
									    border-left-color: #242A2F;
									    border-right-style: solid;
									    border-right-width: 2px;
									    border-right-color: #242A2F;
									}
									
									"
									) %>% gtsave("FR_23_24.html")

webshot2::webshot(url = "FR_23_24.html",file = "FR_23_24.png",vwidth = 950,vheight = 600) 

