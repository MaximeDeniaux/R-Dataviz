

# Sports Licenses in France (2002-2024)
# 3 interactive dataviz: Bump chart, Small multiples, Feminization



# Packages ----

library(here)
library(readxl)
library(tidyverse)
library(ggiraph)
library(ggtext)
library(htmltools)
library(scales)



# Config ----

Excel_Path <- here("data", "Historique licences 2002-2024.xlsx")

Exclude_Patterns <- c(
	"UNSS", "Enseignement", "UFOLEP", "Laïques", "Scolaire", "Universitaire",
	"FSGT", "Retraite", "Travail", "Maccabi", "Médaillés", "Pompiers",
	"Police", "Armées", "Anciens", "Léo Lagrange", "ASPTT",
	"Éducation Physique et de Gym", "Sports Pour Tous"
)

# French -> English sport names

Short_Names <- c(
	"Fédération Française de Football" = "Football",
	"Fédération Française de Tennis" = "Tennis",
	"Fédération Française d'Équitation" = "Horse Riding",
	"Fédération Française de Basketball" = "Basketball",
	"Fédération Française de Handball" = "Handball",
	"Fédération Française de Judo, Jujitsu, Kendo et Disciplines Associées" = "Judo",
	"Fédération Française de Golf" = "Golf",
	"Fédération Française de Rugby" = "Rugby",
	"Fédération Française de Natation" = "Swimming",
	"Fédération Française de Gymnastique" = "Gymnastics",
	"Fédération Française d'Athlétisme" = "Athletics",
	"Fédération Française de Pétanque et Jeu Provençal" = "Boules",
	"Fédération Française de Voile" = "Sailing",
	"Fédération Française de Tir" = "Shooting",
	"Fédération Française de Karaté et Disciplines Associées" = "Karate",
	"Fédération Française de la Randonnée Pédestre" = "Hiking",
	"Fédération Française de Badminton" = "Badminton",
	"Fédération Française de Tennis de Table" = "Table Tennis",
	"Fédération Française de Volley" = "Volleyball",
	"Fédération Française de Cyclisme" = "Cycling"
)

# Theme

Dark_BG    <- "#0a0f1a"
Dark_PANEL <- "#0a0f1a"
Dark_TEXT  <- "#d1d5db"
Dark_MUTED <- "#6b7280"

theme_dark_sport <- function(base_size = 11) {
	theme_minimal(base_size = base_size) +
		theme(
			plot.background    = element_rect(fill = Dark_BG, color = NA),
			panel.background   = element_rect(fill = Dark_PANEL, color = NA),
			panel.grid.major   = element_line(color = "grey20", linewidth = 0.3),
			panel.grid.minor   = element_blank(),
			text               = element_text(color = Dark_TEXT, family = "sans"),
			plot.title         = element_text(color = "#f0f0f0", size = 16, face = "bold",
																				margin = margin(b = 4), hjust = 0.5),
			plot.subtitle      = element_text(color = Dark_MUTED, size = 10,
																				margin = margin(b = 16), hjust = 0.5),
			plot.caption       = element_blank(),
			axis.text          = element_text(color = "#9ca3af", size = 9),
			axis.title         = element_text(color = Dark_MUTED, size = 9),
			legend.text        = element_text(color = Dark_TEXT, size = 9),
			legend.title       = element_text(color = Dark_TEXT, size = 9),
			legend.background  = element_rect(fill = Dark_BG, color = NA),
			legend.key         = element_rect(fill = Dark_BG, color = NA),
			plot.margin        = margin(20, 20, 20, 20)
		)
}

# Unified palette

Color_Rise <- "#5EB87E"
Color_Stable <- "#E8E8E8"
Color_Decline <- "#B85E5E"
cat_colors <- c("Rise" = Color_Rise, "Decline" = Color_Decline)



# Data ----


df_lic_raw <- read_excel(Excel_Path, sheet = "3.Lics_02_24", skip = 3)

df_lic <- df_lic_raw |>
	rename(code = 1, federation = 2) |>
	filter(!is.na(code), str_detect(code, "^\\d+$")) |>
	filter(!str_detect(federation, paste(Exclude_Patterns, collapse = "|"))) |>
	mutate(across(matches("^20\\d{2}$"), ~ as.numeric(.x))) |>
	pivot_longer(cols = matches("^20\\d{2}$"), names_to = "annee", values_to = "licences") |>
	mutate(annee = as.integer(annee)) |>
	filter(!is.na(licences))

top_codes <- df_lic |>
	filter(annee == 2024) |>
	slice_max(licences, n = 19) |>
	pull(code)

df_top <- df_lic |>
	filter(code %in% top_codes) |>
	mutate(sport = recode(federation, !!!Short_Names, .default = federation))

df_ranked <- df_top |>
	group_by(annee) |>
	mutate(rang = rank(-licences, ties.method = "first")) |>
	ungroup()

df_growth <- df_top |>
	filter(annee %in% c(2002, 2024)) |>
	pivot_wider(names_from = annee, values_from = licences, names_prefix = "y") |>
	mutate(
		growth_pct = (y2024 - y2002) / y2002 * 100,
		categorie = case_when(
			growth_pct >= 0 ~ "Rise",
			TRUE ~ "Decline"
		)
	) |>
	select(code, sport, growth_pct, categorie, y2002, y2024)

df_ranked <- df_ranked |>
	left_join(df_growth |> select(code, categorie, growth_pct), by = "code")

# Feminization
df_fem_raw <- read_excel(Excel_Path, sheet = "5.Taux_fem_02_24", skip = 3)

df_fem <- df_fem_raw |>
	rename(code = 1, federation = 2) |>
	filter(!is.na(code), code %in% unique(df_top$code)) |>
	mutate(across(matches("^20\\d{2}$"), ~ as.numeric(.x))) |>
	pivot_longer(cols = matches("^20\\d{2}$"), names_to = "annee", values_to = "taux_fem") |>
	mutate(annee = as.integer(annee), taux_fem = taux_fem * 100) |>
	filter(!is.na(taux_fem)) |>
	mutate(sport = recode(federation, !!!Short_Names, .default = federation))



# Chart 1 : BUMP CHART (Ranking) ----

df_rang_02 <- df_ranked |> filter(annee == 2002) |> select(code, rang_2002 = rang)
df_rang_24 <- df_ranked |> filter(annee == 2024) |> select(code, rang_2024 = rang)

df_ranked <- df_ranked |>
	left_join(df_rang_02, by = "code") |>
	left_join(df_rang_24, by = "code") |>
	left_join(df_growth |> select(sport, y2002, y2024), by = "sport")

# Category based on RANK change
df_rank_cat <- df_ranked |>
	filter(annee %in% c(2002, 2024)) |>
	select(code, sport, annee, rang) |>
	pivot_wider(names_from = annee, values_from = rang, names_prefix = "r") |>
	mutate(
		rank_delta = r2002 - r2024,
		cat_rang = case_when(
			rank_delta >= 1  ~ "Rise",
			rank_delta <= -1 ~ "Decline",
			TRUE ~ "Stable"
		)
	) |>
	select(code, cat_rang)

df_ranked <- df_ranked |>
	left_join(df_rank_cat, by = "code")

df_labels_start <- df_ranked |> filter(annee == 2002)
df_labels_end   <- df_ranked |> filter(annee == 2024)

rank_cat_colors <- c("Rise" = Color_Rise, "Stable" = Color_Stable, "Decline" = Color_Decline)

p_bump <- ggplot(df_ranked, aes(x = annee, y = rang, group = sport, color = cat_rang)) +
	# COVID - more visible yellow
	annotate("rect", xmin = 2019.7, xmax = 2021.3, ymin = 0.3, ymax = 19.7,
					 fill = "#D6B822", alpha = 0.30) +
	# Lines
	geom_line_interactive(
		aes(
			tooltip = paste0(
				"<b>", sport, "</b> (", annee, ")<br><br>",
				"<span style='color:#6b7280'>2002: #", rang_2002,
				" (", format(y2002, big.mark = ","), ")</span><br>",
				"<span style='color:#f0f0f0'>2024: #", rang_2024,
				" (", format(y2024, big.mark = ","), ")</span>"
			),
			data_id = sport
		),
		linewidth = 1.2, alpha = 0.6
	) +
	# Dots
	geom_point_interactive(
		data = bind_rows(df_labels_start, df_labels_end),
		aes(
			tooltip = paste0(
				"<b>", sport, "</b> (", annee, ")<br><br>",
				"<span style='color:#6b7280'>2002: #", rang_2002,
				" (", format(y2002, big.mark = ","), ")</span><br>",
				"<span style='color:#f0f0f0'>2024: #", rang_2024,
				" (", format(y2024, big.mark = ","), ")</span>"
			),
			data_id = sport
		),
		size = 2.5
	) +
	geom_text(data = df_labels_start, aes(label = sport, x = annee - 0.3),
						hjust = 1, size = 3.2, show.legend = FALSE) +
	geom_text(data = df_labels_end, aes(label = sport, x = annee + 0.3),
						hjust = 0, size = 3.2, show.legend = FALSE) +
	scale_y_reverse(
		breaks = 1:19,
		labels = paste0("#", 1:19),
		sec.axis = dup_axis(labels = paste0("#", 1:19))
	) +
	scale_x_continuous(
		breaks = seq(2002, 2024, 2),
		limits = c(1996, 2030),
		expand = c(0, 0)
	) +
	scale_color_manual(values = rank_cat_colors, name = NULL) +
	labs(
		title = NULL,
		x = NULL, y = NULL
	) +
	theme_dark_sport() +
	theme(
		panel.grid.major.x = element_blank(),
		panel.grid.major.y = element_line(color = "grey12", linewidth = 0.15),
		legend.position = "none",
		axis.text.y.left  = element_text(color = "#b0b0b0", size = 9),
		axis.text.y.right = element_text(color = "#b0b0b0", size = 9),
		axis.text.x = element_text(color = "#9ca3af", size = 9)
	)

gi_bump <- girafe(
	ggobj = p_bump,
	width_svg = 12, height_svg = 8.5,
	options = list(
		opts_hover(css = "stroke-width:3.5;opacity:1;"),
		opts_hover_inv(css = "opacity:0.08;"),
		opts_tooltip(
			css = paste0(
				"background:#141926;color:#f0f0f0;",
				"border:1px solid rgba(255,255,255,0.1);border-radius:8px;",
				"padding:10px 14px;font-family:sans-serif;font-size:12px;",
				"box-shadow:0 4px 20px rgba(0,0,0,0.5);line-height:1.6;"
			),
			use_fill = FALSE
		),
		opts_toolbar(saveaspng = FALSE)
	)
)


# Chart 2 : SMALL MULTIPLES (Growth) ----

sport_order <- df_growth |> arrange(desc(growth_pct)) |> pull(sport)

df_small <- df_top |>
	left_join(df_growth |> select(code, growth_pct, categorie), by = "code") |>
	mutate(
		sport = factor(sport, levels = sport_order),
		label_growth = paste0(ifelse(growth_pct >= 0, "+", ""), round(growth_pct, 0), "%")
	)

df_covid_stats <- df_top |>
	filter(annee %in% c(2019, 2021, 2024)) |>
	pivot_wider(names_from = annee, values_from = licences, names_prefix = "y") |>
	mutate(
		pic_covid = round((y2021 - y2019) / y2019 * 100, 0),
		rebond = round((y2024 - y2021) / y2021 * 100, 0)
	) |>
	select(code, sport, pic_covid, rebond)

df_small <- df_small |>
	left_join(df_covid_stats |> select(code, pic_covid, rebond), by = "code")

fmt_k <- function(x) paste0(round(x / 1000), "k")

# Strip labels with ggtext: "1 • Sport          +XX%" with colored %
df_strip <- df_small |>
	distinct(sport, label_growth, categorie) |>
	mutate(
		rank_num = match(sport, sport_order),
		growth_color = case_when(
			categorie == "Rise"    ~ Color_Rise,
			TRUE ~ Color_Decline
		),
		strip_html = paste0(
			rank_num, " &bull; ", sport,
			"    <span style='float:right;color:", growth_color, ";font-weight:700;'>",
			label_growth, "</span>"
		)
	)

strip_levels <- df_strip |> arrange(rank_num) |> pull(strip_html)
df_small <- df_small |>
	left_join(df_strip |> select(sport, strip_html), by = "sport") |>
	mutate(strip_label = factor(strip_html, levels = strip_levels))

df_facet_info <- df_small |>
	filter(annee == 2024) |>
	distinct(sport, strip_label, label_growth, pic_covid, rebond, categorie) |>
	left_join(df_small |> filter(annee == 2002) |> select(sport, val_2002 = licences), by = "sport") |>
	left_join(df_small |> filter(annee == 2024) |> select(sport, val_2024 = licences), by = "sport")

p_small <- ggplot(df_small, aes(x = annee, y = licences, group = sport)) +
	# COVID peak rect
	annotate("rect", xmin = 2019.5, xmax = 2021.5, ymin = -Inf, ymax = Inf,
					 fill = "#D6B822", alpha = 0.30) +
	# Rebound rect (blue, more visible)
	annotate("rect", xmin = 2021.5, xmax = 2025, ymin = -Inf, ymax = Inf,
					 fill = "#60a5fa", alpha = 0.30) +
	# Line
	geom_line(aes(color = categorie), linewidth = 0.9) +
	# Points
	geom_point(data = df_small |> filter(annee %in% c(2002, 2024)),
						 aes(color = categorie), size = 1.8) +
	# COVID peak value inside zone (bottom)
	geom_text(data = df_facet_info,
						aes(x = 2020.5, y = -Inf, label = paste0(pic_covid, "%")),
						color = "#D6B822", size = 2.2, vjust = -0.5, hjust = 0.5, fontface = "bold") +
	# Rebound value inside blue zone (bottom)
	geom_text(data = df_facet_info,
						aes(x = 2023, y = -Inf, label = paste0("+", rebond, "%")),
						color = "#60a5fa", size = 2.2, vjust = -0.5, hjust = 0.5, fontface = "bold") +
	facet_wrap(~strip_label, scales = "free_y", ncol = 3) +
	scale_color_manual(values = cat_colors, guide = "none") +
	scale_x_continuous(limits = c(2002, 2025), expand = c(0, 0)) +
	scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "k"),
										 expand = expansion(mult = c(0.12, 0.12))) +
	labs(
		title = NULL,
		subtitle = NULL,
		x = NULL, y = NULL
	) +
	theme_dark_sport(base_size = 9) +
	theme(
		strip.text = element_markdown(color = "#e5e7eb", size = 8.5, face = "bold", hjust = 0,
																	margin = margin(b = 2, t = 2, l = 4, r = 4)),
		strip.background = element_rect(fill = "#111827", color = NA),
		panel.spacing.x = unit(0.8, "lines"),
		panel.spacing.y = unit(2.5, "lines"),
		panel.background = element_rect(fill = "#F5F5F51A", color = "#FFFFFF33",
																		linewidth = 0.5),
		panel.grid.major = element_line(color = "grey15", linewidth = 0.2),
		panel.grid.major.x = element_blank(),
		axis.text.y = element_text(size = 7, color = "#4b5563"),
		axis.text.x = element_blank(),
		axis.ticks.x = element_blank()
	)

# Bottom labels (values 2002/2024 below panels)
p_small <- p_small +
	geom_text(data = df_facet_info,
						aes(x = 2002, y = -Inf, label = fmt_k(val_2002)),
						vjust = 2.0, hjust = 0, size = 2.5, color = "#6b7280") +
	geom_text(data = df_facet_info,
						aes(x = 2024, y = -Inf, label = fmt_k(val_2024)),
						vjust = 2.0, hjust = 1, size = 2.5, color = "#d1d5db", fontface = "bold") +
	geom_text(data = df_facet_info,
						aes(x = 2013, y = -Inf, label = "2002 \u2014 2024"),
						vjust = 2.0, hjust = 0.5, size = 2.2, color = "#374151") +
	coord_cartesian(clip = "off")

gi_small <- girafe(
	ggobj = p_small,
	width_svg = 11, height_svg = 16,
	options = list(
		opts_toolbar(saveaspng = FALSE)
	)
)


# Chart 3 : FEMINIZATION (Dumbbell) ----

Color_Fem_Up     <- Color_Rise
Color_Fem_Down   <- Color_Decline
Color_Fem_Stable <- Color_Stable

df_fem_dumbbell <- df_fem |>
	filter(annee %in% c(2002, 2024)) |>
	pivot_wider(names_from = annee, values_from = taux_fem, names_prefix = "y") |>
	mutate(
		delta = y2024 - y2002,
		fem_cat = case_when(
			round(delta, 1) > 0  ~ "Rise",
			round(delta, 1) < 0  ~ "Decline",
			TRUE ~ "Stable"
		)
	) |>
	arrange(delta) |>
	mutate(sport = fct_inorder(sport))

fem_cat_colors <- c(
	"Rise"    = Color_Fem_Up,
	"Decline" = Color_Fem_Down,
	"Stable"  = Color_Fem_Stable
)

sport_levels <- levels(df_fem_dumbbell$sport)
df_stripes <- tibble(
	sport_name = sport_levels,
	idx = seq_along(sport_levels)
) |>
	filter(idx %% 2 == 0)

p_fem <- ggplot(df_fem_dumbbell) +
	geom_rect(
		data = df_stripes,
		aes(ymin = idx - 0.5, ymax = idx + 0.5),
		xmin = 0, xmax = 115,
		fill = "white", alpha = 0.02, inherit.aes = FALSE
	) +
	geom_vline(xintercept = 50, linetype = "dashed", color = "grey40", linewidth = 0.4) +
	geom_segment_interactive(
		aes(x = y2002, xend = y2024, y = sport, yend = sport, color = fem_cat,
				tooltip = paste0(
					"<b>", sport, "</b><br>",
					"2002: ", round(y2002, 1), "%<br>",
					"2024: ", round(y2024, 1), "%<br>",
					"Change: ", ifelse(delta >= 0, "+", ""), round(delta, 1), " pts"
				),
				data_id = sport),
		linewidth = 1.2, alpha = 0.5
	) +
	geom_point_interactive(
		aes(x = y2002, y = sport,
				tooltip = paste0(sport, " (2002): ", round(y2002, 1), "%"),
				data_id = sport),
		color = "#4b5563", size = 2.5
	) +
	geom_point_interactive(
		aes(x = y2024, y = sport, color = fem_cat,
				tooltip = paste0(sport, " (2024): ", round(y2024, 1), "%"),
				data_id = sport),
		size = 3.5
	) +
	geom_text(
		aes(x = 102, y = sport,
				label = paste0(ifelse(delta >= 0, "+", ""), round(delta, 1), " pts"),
				color = fem_cat),
		hjust = 0, size = 2.8, fontface = "bold", show.legend = FALSE
	) +
	annotate("text", x = 51, y = Inf, label = "50% parity",
					 color = Dark_MUTED, size = 2.5, hjust = 0, vjust = 1.5) +
	scale_color_manual(values = fem_cat_colors, name = NULL) +
	scale_x_continuous(
		breaks = c(0, 25, 50, 75, 100),
		labels = paste0(c(0, 25, 50, 75, 100), "%"),
		limits = c(0, 118)
	) +
	labs(
		title = NULL,
		subtitle = NULL,
		x = NULL, y = NULL
	) +
	theme_dark_sport() +
	theme(
		panel.grid.major.y = element_blank(),
		panel.grid.major.x = element_line(color = "grey15", linewidth = 0.2),
		legend.position = "none",
		axis.text.y = element_text(size = 9, face = "bold")
	)

gi_fem <- girafe(
	ggobj = p_fem,
	width_svg = 10, height_svg = 8,
	options = list(
		opts_hover(css = "stroke-width:3;opacity:1;r:6px;"),
		opts_hover_inv(css = "opacity:0.15;"),
		opts_tooltip(
			css = paste0(
				"background:#141926;color:#f0f0f0;",
				"border:1px solid rgba(255,255,255,0.1);border-radius:8px;",
				"padding:10px 14px;font-family:sans-serif;font-size:12px;",
				"box-shadow:0 4px 20px rgba(0,0,0,0.5);line-height:1.6;"
			),
			use_fill = FALSE
		),
		opts_toolbar(saveaspng = FALSE)
	)
)


# HTML ----


tab_css <- tags$style(HTML("
  body {
    background: #0a0f1a;
    font-family: 'Segoe UI', sans-serif;
    margin: 0;
    padding: 20px;
  }
  .tab-container {
    max-width: 1100px;
    margin: 0 auto;
    text-align: center;
  }
  .tab-header {
    display: flex;
    gap: 6px;
    margin-bottom: 24px;
    justify-content: center;
    flex-wrap: wrap;
  }
  .tab-btn {
    padding: 10px 20px;
    border: none;
    border-radius: 8px;
    cursor: pointer;
    font-family: 'Segoe UI', sans-serif;
    font-size: 14px;
    font-weight: 500;
    background: rgba(255,255,255,0.03);
    color: #6b7280;
    transition: all 0.2s;
    border-bottom: 2px solid transparent;
  }
  .tab-btn:hover {
    background: rgba(255,255,255,0.06);
    color: #d1d5db;
  }
  .tab-btn.active {
    background: rgba(255,255,255,0.1);
    color: #f0f0f0;
    font-weight: 600;
    border-bottom: 2px solid #3b82f6;
  }
  .tab-content { display: none; text-align: left; }
  .tab-content.active { display: block; }
  .main-title {
    color: #f0f0f0; font-size: 28px; font-weight: 700;
    margin: 0 0 4px; letter-spacing: -0.5px;
  }
  .main-subtitle {
    color: #4b5563; font-size: 13px; margin: 0 0 24px; font-weight: 300;
  }
  .footer {
    max-width: 1100px; margin: 24px auto 0; padding-top: 8px;
    color: #4b5563; font-size: 11px; text-align: center;
  }
  .girafe_container_std .ggiraph-toolbar { opacity: 0.8; }
  .girafe_container_std .ggiraph-toolbar button {
    background: rgba(255,255,255,0.1) !important;
    border-radius: 4px !important;
    margin: 2px !important;
  }
  .girafe_container_std .ggiraph-toolbar button:hover {
    background: rgba(255,255,255,0.25) !important;
  }
  .girafe_container_std .ggiraph-toolbar svg {
    fill: #9ca3af !important;
  }
  .girafe_container_std .ggiraph-toolbar button:hover svg {
    fill: #f0f0f0 !important;
  }
  .legend-row {
    display: flex;
    justify-content: center;
    align-items: center;
    gap: 0;
    margin: 16px 0 8px;
    font-family: 'Segoe UI', sans-serif;
    font-size: 13px;
  }
  .legend-group {
    display: flex;
    align-items: center;
    gap: 24px;
  }
  .legend-sep {
    width: 60px;
  }
  .legend-item {
    display: flex;
    align-items: center;
    gap: 6px;
  }
  .legend-dot {
    width: 10px;
    height: 10px;
    border-radius: 50%;
    display: inline-block;
  }
  .legend-rect {
    width: 14px;
    height: 14px;
    border-radius: 3px;
    display: inline-block;
  }
"))

tab_js <- tags$script(HTML("
  function switchTab(tabId) {
    document.querySelectorAll('.tab-content').forEach(el => el.classList.remove('active'));
    document.querySelectorAll('.tab-btn').forEach(el => el.classList.remove('active'));
    document.getElementById(tabId).classList.add('active');
    event.target.classList.add('active');
  }
"))

# Build HTML legends
legend_ranking <- tags$div(class = "legend-row",
													 tags$div(class = "legend-group",
													 				 tags$div(class = "legend-item",
													 				 				 tags$span(class = "legend-dot", style = paste0("background:", Color_Rise, ";")),
													 				 				 tags$span(style = paste0("color:", Color_Rise, ";"), "Rise")
													 				 ),
													 				 tags$div(class = "legend-item",
													 				 				 tags$span(class = "legend-dot", style = paste0("background:", Color_Stable, ";")),
													 				 				 tags$span(style = paste0("color:", Color_Stable, ";"), "Stable")
													 				 ),
													 				 tags$div(class = "legend-item",
													 				 				 tags$span(class = "legend-dot", style = paste0("background:", Color_Decline, ";")),
													 				 				 tags$span(style = paste0("color:", Color_Decline, ";"), "Decline")
													 				 )
													 ),
													 tags$div(class = "legend-sep"),
													 tags$div(class = "legend-group",
													 				 tags$div(class = "legend-item",
													 				 				 tags$span(class = "legend-rect", style = "background:#D6B822;opacity:0.5;"),
													 				 				 tags$span(style = "color:#D6B822;", "COVID peak (2019\u21922021)")
													 				 )
													 )
)

legend_growth <- tags$div(class = "legend-row",
													tags$div(class = "legend-group",
																	 tags$div(class = "legend-item",
																	 				 tags$span(class = "legend-rect", style = "background:#D6B822;opacity:0.5;"),
																	 				 tags$span(style = "color:#D6B822;", "COVID peak (2019\u21922021)")
																	 )
													),
													tags$div(class = "legend-sep"),
													tags$div(class = "legend-group",
																	 tags$div(class = "legend-item",
																	 				 tags$span(class = "legend-rect", style = "background:#60a5fa;opacity:0.5;"),
																	 				 tags$span(style = "color:#60a5fa;", "Rebound (2021\u21922024)")
																	 )
													)
)

legend_fem <- tags$div(class = "legend-row",
											 tags$div(class = "legend-group",
											 				 tags$div(class = "legend-item",
											 				 				 tags$span(class = "legend-dot", style = paste0("background:", Color_Rise, ";")),
											 				 				 tags$span(style = paste0("color:", Color_Rise, ";"), "Rise")
											 				 ),
											 				 tags$div(class = "legend-item",
											 				 				 tags$span(class = "legend-dot", style = paste0("background:", Color_Stable, ";")),
											 				 				 tags$span(style = paste0("color:", Color_Stable, ";"), "Stable")
											 				 ),
											 				 tags$div(class = "legend-item",
											 				 				 tags$span(class = "legend-dot", style = paste0("background:", Color_Decline, ";")),
											 				 				 tags$span(style = paste0("color:", Color_Decline, ";"), "Decline")
											 				 )
											 ),
											 tags$div(class = "legend-sep"),
											 tags$div(class = "legend-group",
											 				 tags$div(class = "legend-item",
											 				 				 tags$span(class = "legend-dot", style = "background:#4b5563;"),
											 				 				 tags$span(style = "color:#4b5563;", "2002")
											 				 ),
											 				 tags$div(class = "legend-item",
											 				 				 tags$span(class = "legend-dot", style = "background:#d1d5db;"),
											 				 				 tags$span(style = "color:#d1d5db;font-weight:600;", "2024")
											 				 )
											 )
)

page <- tagList(
	tags$head(
		tags$meta(charset = "UTF-8"),
		tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
		tags$title("Sports licences in France \u00b7 2002-2024"),
		tab_css
	),
	tags$body(
		tags$div(class = "tab-container",
						 tags$div(class = "tab-header",
						 				 tags$button(class = "tab-btn active", onclick = "switchTab('tab-bump')", "Ranking"),
						 				 tags$button(class = "tab-btn", onclick = "switchTab('tab-small')", "Growth"),
						 				 tags$button(class = "tab-btn", onclick = "switchTab('tab-fem')", "Feminization")
						 ),
						 # --- RANKING TAB ---
						 tags$div(id = "tab-bump", class = "tab-content active",
						 				 tags$h2(style = "color:#f0f0f0; font-size:20px; font-weight:700; text-align:center; margin:16px 0 8px;",
						 				 				"How the ranking of the most popular sports in France (by registered players) has evolved over the last two decades (2002-2024)"
						 				 ),
						 				 legend_ranking,
						 				 gi_bump
						 ),
						 # --- GROWTH TAB ---
						 tags$div(id = "tab-small", class = "tab-content",
						 				 tags$h2(style = "color:#f0f0f0; font-size:20px; font-weight:700; text-align:center; margin:16px 0 4px;",
						 				 				HTML(paste0(
						 				 					"Who's <span style='color:", Color_Rise, ";'>rising</span> ",
						 				 					"and who's <span style='color:", Color_Decline, ";'>declining</span>? (2002-2024)"
						 				 				))
						 				 ),
						 				 tags$p(style = "color:#6b7280; font-size:13px; text-align:center; margin:0 0 8px;",
						 				 			 "Change in number of license holders between 2002 and 2024, sorted by growth rate"
						 				 ),
						 				 legend_growth,
						 				 gi_small
						 ),
						 # --- FEMINIZATION TAB ---
						 tags$div(id = "tab-fem", class = "tab-content",
						 				 tags$h2(style = "color:#f0f0f0; font-size:20px; font-weight:700; text-align:center; margin:16px 0 4px;",
						 				 				"How far has the feminization of sports come? (2002-2024)"
						 				 ),
						 				 tags$p(style = "color:#6b7280; font-size:13px; text-align:center; margin:0 0 8px;",
						 				 			 "Share of female license holders, sorted by change"
						 				 ),
						 				 legend_fem,
						 				 gi_fem
						 )
		),
		tags$div(class = "footer",
						 "Visualisation by Maxime Deniaux | Data: INJEP-MEDES, Recensement des licences et clubs sportifs 2024"
		),
		tab_js
	)
)

# Export ----

htmltools::save_html(page, file = here("output", "sports_licenses_france_2002_2024.html"))