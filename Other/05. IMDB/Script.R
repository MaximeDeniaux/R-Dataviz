# Interactive Dataviz — Are TV Series Getting Shorter?
# Evolution of episode count per season (2000-2024)
# Source: IMDb Non-Commercial Datasets


# Packages ----


library(data.table)
library(here)
library(ggplot2)
library(ggiraph)
library(ggdist)
library(htmltools)
library(scales)
library(glue)



# Data loading & preparation ----

# https://datasets.imdbws.com/

basics   <- fread(here("data", "title.basics.tsv.gz"),   na.strings = "\\N")
episodes <- fread(here("data", "title.episode.tsv.gz"),  na.strings = "\\N")
ratings  <- fread(here("data", "title.ratings.tsv.gz"),  na.strings = "\\N")



## Filters ----

series <- basics[titleType %in% c("tvSeries", "tvMiniSeries") & 
                   startYear >= 2000 & startYear <= 2024 &
                   isAdult == 0]

excluded_genres <- c("Talk-Show", "News", "Reality-TV", "Game-Show")
series <- series[!sapply(genres, function(g) {
  any(strsplit(g, ",")[[1]] %in% excluded_genres)
})]

series <- merge(series, ratings, by = "tconst", all.x = FALSE)
series <- series[numVotes >= 10000]
series[, runtimeMinutes := as.numeric(runtimeMinutes)]




## Top 20 per year (by rating) ----

series <- series[, .SD[order(-averageRating, -numVotes)][1:min(.N, 20)], 
                 by = startYear]

cat("Total number of series (top 20/year):", nrow(series), "\n")



## Episodes per season ----

ep_series <- merge(episodes, 
                   series[, .(tconst, primaryTitle, startYear, averageRating, 
                              numVotes, runtimeMinutes, genres)], 
                   by.x = "parentTconst", by.y = "tconst")

ep_series <- ep_series[!is.na(seasonNumber) & seasonNumber > 0]



## Average rating per season (from individual episode ratings) ----

ep_ratings <- merge(
  ep_series[, .(tconst, parentTconst, seasonNumber)],
  ratings,
  by = "tconst",
  suffixes = c("", "_ep")
)

note_par_saison <- ep_ratings[, .(
  rating_saison = round(mean(averageRating, na.rm = TRUE), 2),
  nb_votes_saison = sum(numVotes, na.rm = TRUE)
), by = .(parentTconst, seasonNumber)]

# Count episodes and join season rating
episodes_per_season <- ep_series[, .(
  nb_episodes = .N
), by = .(parentTconst, primaryTitle, startYear, averageRating, 
          numVotes, runtimeMinutes, genres, seasonNumber)]

# Replace global rating with season-level rating
episodes_per_season <- merge(
  episodes_per_season,
  note_par_saison,
  by = c("parentTconst", "seasonNumber"),
  all.x = TRUE
)

# Use season rating if available; otherwise keep global rating
episodes_per_season[!is.na(rating_saison), averageRating := rating_saison]

episodes_per_season <- episodes_per_season[nb_episodes >= 4 & nb_episodes <= 30]

cat("Season-level ratings computed for", 
    sum(!is.na(episodes_per_season$rating_saison)), "seasons out of", 
    nrow(episodes_per_season), "\n")



## Aggregated trend (mean) ----

trend <- episodes_per_season[, .(
  average  = round(mean(nb_episodes), 1),
  q1       = as.double(quantile(nb_episodes, 0.25)),
  q3       = as.double(quantile(nb_episodes, 0.75)),
  nb_series = uniqueN(parentTconst)
), by = startYear][order(startYear)]




# Custom theme : Ghost blog style ----

bg_color      <- "#f1f1f1"
text_color    <- "#2C2C2A"
text_muted    <- "#73726C"
dry_sage      <- "#c9cba3"    
soft_peach    <- "#ffe1a8"    
vibrant_coral <- "#e26d5c"    
wine_plum     <- "#723d46"    
dark_slate    <- "#373f51"    
grid_color    <- "#E0E0E0"
card_bg       <- "#FFFFFF"


theme_series <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background     = element_rect(fill = bg_color, color = NA),
      panel.background    = element_rect(fill = bg_color, color = NA),
      panel.grid.major.y  = element_line(color = grid_color, linewidth = 0.3),
      panel.grid.major.x  = element_blank(),
      panel.grid.minor    = element_blank(),
      axis.text           = element_text(color = text_muted, size = rel(0.85)),
      axis.title          = element_text(color = text_color, size = rel(0.9)),
      axis.title.y        = element_text(margin = margin(r = 10)),
      axis.title.x        = element_text(margin = margin(t = 10)),
      plot.title          = element_text(color = text_color, face = "bold", 
                                         size = rel(1.4), margin = margin(b = 4)),
      plot.subtitle       = element_text(color = text_muted, size = rel(0.95),
                                         margin = margin(b = 15)),
      plot.caption        = element_text(color = text_muted, size = rel(0.7),
                                         hjust = 0, margin = margin(t = 15)),
      plot.margin         = margin(20, 20, 15, 20),
      legend.position     = "none"
    )
}


# Plot 1 : Overall trand ----

tooltip_trend <- glue_data(trend,
                           "<b>{startYear}</b><br>",
                           "Average: <b>{average} episodes</b>/season<br>",
                           "IQR: {q1} – {q3}<br>",
                           "Series: {nb_series}"
)

smooth_q3  <- predict(loess(q3 ~ startYear, data = trend, span = 0.4))
smooth_q1  <- predict(loess(q1 ~ startYear, data = trend, span = 0.4))
smooth_avg <- predict(loess(average ~ startYear, data = trend, span = 0.4))

trend_smooth <- data.frame(
  startYear = trend$startYear,
  smooth_q1 = smooth_q1, smooth_q3 = smooth_q3, smooth_avg = smooth_avg
)

p1 <- ggplot() +
  geom_ribbon(data = trend_smooth,
              aes(x = startYear, ymin = smooth_q1, ymax = smooth_q3), 
              fill = vibrant_coral, alpha = 0.15) +
  geom_line(data = trend_smooth,
            aes(x = startYear, y = smooth_avg),
            color = vibrant_coral, linewidth = 1.2) +
  geom_point_interactive(
    data = trend,
    aes(x = startYear, y = average, 
        tooltip = tooltip_trend, data_id = startYear),
    color = vibrant_coral, size = 3, fill = bg_color, shape = 21, stroke = 1.5
  ) +
  annotate("segment", x = 2007, xend = 2007, y = 0, yend = 19,
           color = text_muted, linewidth = 0.3, linetype = "dashed") +
  annotate("segment", x = 2013, xend = 2013, y = 0, yend = 17,
           color = text_muted, linewidth = 0.3, linetype = "dashed") +
  annotate("segment", x = 2019.5, xend = 2019.5, y = 0, yend = 12,
           color = text_muted, linewidth = 0.3, linetype = "dashed") +
  annotate("text", x = 2007, y = 21, 
           label = "Netflix launches\nstreaming (2007)",
           color = text_muted, size = 3, hjust = 0.5, lineheight = 0.9) +
  annotate("text", x = 2013, y = 19, 
           label = "Lilyhammer (2012)\nHouse of Cards (2013)\nFirst Netflix originals",
           color = text_muted, size = 2.8, hjust = 0.5, lineheight = 0.9) +
  annotate("text", x = 2019.5, y = 14, 
           label = "Disney+, Apple TV+ (2019)\nHBO Max, Peacock (2020)\nParamount+ (2021)",
           color = text_muted, size = 2.8, hjust = 0.5, lineheight = 0.9) +
  scale_x_continuous(breaks = seq(2000, 2024, 4)) +
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +
  labs(
    title    = "Seasons are getting shorter",
    subtitle = "Average number of episodes per season for the top 20 highest-rated series each year (10,000+ IMDb votes)",
    x = NULL, y = "Episodes per season",
    caption  = "Shaded area represents the smoothed interquartile range (Q1–Q3)"
  ) +
  theme_series()




# Plot 2 : Strip plot with search ----

set.seed(42)
episodes_per_season[, jitter_x := startYear + runif(.N, -0.35, 0.35)]
episodes_per_season[, main_genre := sapply(strsplit(genres, ","), `[`, 1)]

tooltip_strip <- glue_data(episodes_per_season,
                           "<b>{primaryTitle}</b> (S{seasonNumber})<br>",
                           "{nb_episodes} episodes<br>",
                           "Season rating: {averageRating}/10<br>",
                           "Genre: {main_genre}<br>",
                           "Year: {startYear}"
)

# 4 categories (same colors as chart 3)
episodes_per_season[, rating_cat := fcase(
  averageRating >= 9.0, "9+",
  averageRating >= 8.0, "8 – 9",
  averageRating >= 7.5, "7.5 – 8",
  default = "7 – 7.5"
)]

rating_colors <- c(
  "9+"       = "#1B9E77",
  "8 – 9"    = "#D95F02",
  "7.5 – 8"  = "#7570B3",
  "7 – 7.5"  = "#E7298A"
)

episodes_per_season[, rating_cat := factor(rating_cat, levels = names(rating_colors))]

set.seed(123)
episodes_per_season[, jitter_y := nb_episodes + runif(.N, -0.4, 0.4)]

# Clean search identifier
episodes_per_season[, search_id := tolower(gsub("[^a-z0-9]", "", tolower(primaryTitle)))]

p2 <- ggplot(episodes_per_season, aes(x = jitter_x, y = jitter_y)) +
  geom_point_interactive(
    aes(fill = rating_cat,
        tooltip = tooltip_strip,
        data_id = rating_cat),
    size = 2.2, alpha = 0.7, shape = 21, color = "black", stroke = 0.3
  ) +
  scale_fill_manual(values = rating_colors, name = "IMDb Rating") +
  scale_x_continuous(breaks = seq(2000, 2024, 4)) +
  scale_y_continuous(limits = c(0, 31), breaks = seq(0, 30, 5)) +
  guides(fill = guide_legend(override.aes = list(size = 6, alpha = 1))) +
  labs(
    title    = "Each dot, a season",
    subtitle = "Hover over a dot to highlight series within the same rating category",
    x = NULL, y = "Episodes per season"
  ) +
  theme_series() +
  theme(legend.position = "top",
        legend.justification = "left",
        legend.text = element_text(color = text_muted, size = 12),
        legend.title = element_text(color = text_color, size = 13),
        legend.key.size = unit(1.5, "lines"),
        legend.margin = margin(0, 0, 0, 0))




# Plot 3 : Half-eye + boxplot ----

series_duration <- series[!is.na(runtimeMinutes) & runtimeMinutes >= 10 & runtimeMinutes <= 120]

series_duration[, period := fcase(
  startYear <= 2008, "2000 – 2008",
  startYear <= 2017, "2009 – 2017",
  default = "2018 – 2024"
)]

series_duration[, period := factor(period, levels = c("2018 – 2024", "2009 – 2017", 
                                                      "2000 – 2008"))]

period_colors <- c(
  "2000 – 2008" = "#c9cba3",
  "2009 – 2017" = "#e26d5c",
  "2018 – 2024" = "#373f51"
)

p3 <- ggplot(series_duration, aes(x = period, y = runtimeMinutes)) +
  # Density (half-eye above the line)
  stat_halfeye(
    aes(fill = period),
    alpha = 0.3,
    adjust = 0.8,
    .width = 0,
    point_colour = NA
  ) +
  # Boxplot (below the density)
  geom_boxplot(
    aes(fill = period),
    width = 0.12,
    outlier.shape = NA,
    alpha = 0.6,
    color = "#383332",
    linewidth = 0.5
  ) +
  scale_fill_manual(values = period_colors) +
  scale_y_continuous(breaks = seq(10, 120, 10),
                     labels = function(x) paste0(x, " min")) +
  coord_flip(ylim = c(10, 90)) +
  scale_x_discrete(expand = expansion(mult = c(0.15, 0.25))) +
  guides(fill = "none") +
  labs(
    title    = "Longer episodes to compensate?",
    subtitle = "Episode runtime distribution by period — density and boxplot",
    x = NULL,
    y = NULL,
    caption  = "Vertical line = median"
  ) +
  theme_series() +
  theme(axis.text.y = element_text(color = text_color, size = 12))



# Interactive html assembly ----

tooltip_css <- glue(
  "background:{card_bg};color:{text_color};",
  "padding:8px 12px;border-radius:6px;",
  "font-size:13px;border:1px solid {grid_color};",
  "font-family:'Inter', system-ui, sans-serif;",
  "box-shadow: 0 2px 8px rgba(0,0,0,0.1);"
)

g1 <- girafe(ggobj = p1, width_svg = 10, height_svg = 5.5,
             options = list(
               opts_hover(css = glue("fill:{vibrant_coral};stroke:{vibrant_coral};r:5px;")),
               opts_tooltip(css = tooltip_css),
               opts_toolbar(saveaspng = FALSE)
             ))

g2 <- girafe(ggobj = p2, width_svg = 10, height_svg = 7.5,
             options = list(
               opts_hover(css = "opacity:1;"),
               opts_hover_inv(css = "opacity:0.08;"),
               opts_tooltip(css = tooltip_css),
               opts_toolbar(saveaspng = FALSE)
             ))

g3 <- girafe(ggobj = p3, width_svg = 10, height_svg = 6,
             options = list(
               opts_tooltip(css = tooltip_css),
               opts_toolbar(saveaspng = FALSE)
             ))


## Generate lookup table for JS search (chart 2) ----

search_data <- episodes_per_season[, .(
  name = primaryTitle, 
  sid = search_id,
  season = seasonNumber,
  x = round(jitter_x, 4), 
  y = round(jitter_y, 4)
)]
search_json <- jsonlite::toJSON(search_data, auto_unbox = TRUE)


## Full HTML page ----

page <- htmltools::tagList(
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML(glue("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;700&display=swap');
      
      *, *::before, *::after {{
        box-sizing: border-box;
      }}
      
      html, body {{
        margin: 0;
        padding: 0;
        height: 100%;
        overflow: hidden;
        background-color: {bg_color};
        color: {text_color};
        font-family: 'Inter', system-ui, sans-serif;
      }}
      
      .scroll-container {{
        height: 100vh;
        overflow-y: auto;
        scroll-snap-type: y mandatory;
        scroll-behavior: smooth;
        -ms-overflow-style: none;
        scrollbar-width: none;
      }}
      .scroll-container::-webkit-scrollbar {{
        display: none;
      }}
      
      .scroll-section {{
        height: 100vh;
        scroll-snap-align: start;
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 40px 20px;
        position: relative;
      }}
      
      .section-inner {{
        max-width: 960px;
        width: 100%;
        max-height: 90vh;
        overflow-y: auto;
        -ms-overflow-style: none;
        scrollbar-width: none;
      }}
      .section-inner::-webkit-scrollbar {{
        display: none;
      }}
      
      .hero {{
        text-align: center;
      }}
      
      .hero h1 {{
        font-size: 2.4rem;
        font-weight: 700;
        margin-bottom: 8px;
        color: {text_color};
        line-height: 1.2;
      }}
      
      .hero h1 span {{
        color: {vibrant_coral};
      }}
      
      .hero .subtitle {{
        font-size: 1.1rem;
        color: {text_muted};
        max-width: 650px;
        margin: 0 auto;
        line-height: 1.5;
      }}
      
      .key-number {{
        display: inline-block;
        background: {card_bg};
        padding: 12px 20px;
        border-radius: 8px;
        margin: 5px;
        text-align: center;
        border: 1px solid {grid_color};
      }}
      
      .key-number .value {{
        font-size: 1.8rem;
        font-weight: 700;
        color: {vibrant_coral};
        display: block;
      }}
      
      .key-number .label {{
        font-size: 0.8rem;
        color: {text_muted};
      }}
      
      .key-numbers {{
        display: flex;
        flex-wrap: wrap;
        gap: 10px;
        justify-content: center;
        margin: 30px 0;
      }}
      
      .scroll-hint {{
        position: absolute;
        bottom: 30px;
        left: 50%;
        transform: translateX(-50%);
        display: flex;
        flex-direction: column;
        align-items: center;
        gap: 6px;
        color: {text_muted};
        font-size: 0.75rem;
        letter-spacing: 0.05em;
        animation: pulse-down 2s ease-in-out infinite;
      }}
      .scroll-hint svg {{
        width: 20px;
        height: 20px;
        stroke: {text_muted};
      }}
      @keyframes pulse-down {{
        0%, 100% {{ opacity: 0.4; transform: translateX(-50%) translateY(0); }}
        50% {{ opacity: 1; transform: translateX(-50%) translateY(6px); }}
      }}
      
      .nav-dots {{
        position: fixed;
        right: 20px;
        top: 50%;
        transform: translateY(-50%);
        display: flex;
        flex-direction: column;
        gap: 12px;
        z-index: 100;
      }}
      .nav-dot {{
        width: 10px;
        height: 10px;
        border-radius: 50%;
        background: {grid_color};
        border: none;
        cursor: pointer;
        transition: background 0.3s, transform 0.3s;
        padding: 0;
      }}
      .nav-dot:hover {{
        background: {text_muted};
      }}
      .nav-dot.active {{
        background: {vibrant_coral};
        transform: scale(1.3);
      }}
      
      .section .intro {{
        font-size: 1rem;
        color: {text_muted};
        line-height: 1.7;
        max-width: 100%;
        margin-bottom: 15px;
      }}
      
      .section .intro strong {{
        color: {text_color};
      }}
      
      .chart-container {{
        margin: 10px 0;
      }}
      
      .source {{
        text-align: center;
        font-size: 0.8rem;
        color: {text_muted};
        margin-top: 20px;
      }}
      
      .source a {{
        color: {vibrant_coral};
        text-decoration: none;
      }}
      
      .plot2-wrapper {{
        position: relative;
      }}
      
      .search-container {{
        position: absolute;
        top: 125px;
        right: 20px;
        display: flex;
        align-items: center;
        gap: 8px;
        z-index: 10;
      }}
      
      .search-container input {{
        width: 220px;
        padding: 6px 10px;
        border: 1px solid {grid_color};
        border-radius: 6px;
        background: {card_bg};
        color: {text_color};
        font-family: 'Inter', system-ui, sans-serif;
        font-size: 13px;
        outline: none;
      }}
      
      .search-container input:focus {{
        border-color: {vibrant_coral};
      }}
      
      .search-container label {{
        font-size: 12px;
        color: {text_muted};
      }}
      
      .search-results {{
        font-size: 11px;
        color: {text_muted};
        text-align: right;
        min-height: 16px;
        margin-bottom: 2px;
      }}
    ")))
  ),
  
  tags$body(
    # Navigation dots (fixed)
    tags$div(class = "nav-dots",
             tags$button(class = "nav-dot active", onclick = "scrollToSection(0)"),
             tags$button(class = "nav-dot", onclick = "scrollToSection(1)"),
             tags$button(class = "nav-dot", onclick = "scrollToSection(2)"),
             tags$button(class = "nav-dot", onclick = "scrollToSection(3)")
    ),
    
    tags$div(class = "scroll-container", id = "scroll-container",
             
             # Section 0 : Hero 
             tags$div(class = "scroll-section", `data-section` = "0",
                      tags$div(class = "section-inner",
                               tags$div(class = "hero",
                                        tags$h1(HTML("Are TV series getting <span>shorter</span>?")),
                                        tags$p(class = "subtitle",
                                               "Analyzing the top 20 highest-rated series each year on IMDb (2000-2024). ",
                                               "How streaming transformed the TV series format."
                                        )
                               ),
                               tags$div(class = "key-numbers",
                                        tags$div(class = "key-number",
                                                 tags$span(class = "value", 
                                                           paste0(trend[startYear == 2000, average], " \u2192 ", 
                                                                  trend[startYear == 2024, average])),
                                                 tags$span(class = "label", "episodes/season (average)")
                                        ),
                                        tags$div(class = "key-number",
                                                 tags$span(class = "value", paste0("\u00F7 ", round(
                                                   trend[startYear == 2000, average] / trend[startYear == 2024, average], 1))),
                                                 tags$span(class = "label", "in 25 years")
                                        ),
                                        tags$div(class = "key-number",
                                                 tags$span(class = "value", nrow(series)),
                                                 tags$span(class = "label", "series analyzed")
                                        ),
                                        tags$div(class = "key-number",
                                                 tags$span(class = "value", "Top 20/year"),
                                                 tags$span(class = "label", "by IMDb rating")
                                        )
                               )
                      ),
                      tags$div(class = "scroll-hint",
                               HTML('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="6 9 12 15 18 9"></polyline></svg>'),
                               tags$span("scroll")
                      )
             ),
             
             # Section 1 : Plot 1 
             tags$div(class = "scroll-section", `data-section` = "1",
                      tags$div(class = "section-inner section",
                               tags$p(class = "intro", HTML(paste0(
                                 "In 2000, a TV season averaged <strong>",
                                 trend[startYear == 2000, average],
                                 " episodes</strong>. By 2024, that number had dropped to <strong>",
                                 trend[startYear == 2024, average],
                                 " episodes</strong>. ",
                                 "This shift accelerated with the rise of streaming platforms, ",
                                 "which favor shorter, more focused seasons. ",
                                 "Viewers, overwhelmed by choice and short on time, ",
                                 "seem to embrace this format too: the highest-rated series ",
                                 "are increasingly shorter."
                               ))),
                               tags$div(class = "chart-container", g1)
                      )
             ),
             
             # Section 2 : Plot 2 
             tags$div(class = "scroll-section", `data-section` = "2",
                      tags$div(class = "section-inner section",
                               tags$p(class = "intro", HTML(
                                 "Behind the trend, each dot represents one season of one series. ",
                                 "You can see the <strong>\"cloud\" compressing downward</strong> over the years: ",
                                 "where the 2000s mixed seasons of 6 to 24 episodes, ",
                                 "the 2020s cluster mostly between 4 and 10. ",
                                 "Hover over a dot to highlight series within the same rating bracket, ",
                                 "or use the search bar to find a specific series."
                               )),
                               tags$div(class = "plot2-wrapper",
                                        tags$div(class = "search-container",
                                                 tags$label(`for` = "seriesSearch", "Search:"),
                                                 tags$input(type = "text", id = "seriesSearch", 
                                                            placeholder = "e.g. Breaking Bad...")
                                        ),
                                        tags$div(id = "searchResults", class = "search-results"),
                                        tags$div(class = "chart-container", id = "plot2-container", g2)
                               )
                      )
             ),
             
             # Section 3 : Plot 3 + Source 
             tags$div(class = "scroll-section", `data-section` = "3",
                      tags$div(class = "section-inner section",
                               tags$p(class = "intro", HTML(
                                 "Fewer episodes, but <strong>longer ones?</strong> ",
                                 "The runtime distribution reveals a clear shift: the 22-minute sitcom format, ",
                                 "king of the 2000s, is gradually losing ground ",
                                 "to 45-to-60-minute drama formats. Recent series compensate ",
                                 "for fewer episodes with denser, longer installments."
                               )),
                               tags$div(class = "chart-container", g3),
                               tags$div(class = "source",
                                        HTML("Data visualization by Maxime Deniaux | Data: IMDb Non-Commercial Datasets (March 2026) — excludes talk shows, reality TV & news")
                               )
                      )
             )
    ),
    
    # JavaScript: scrollytelling nav + search 
    tags$script(HTML(glue("
      // --- Navigation dots ---
      function scrollToSection(index) {{
        const sections = document.querySelectorAll('.scroll-section');
        if (sections[index]) {{
          sections[index].scrollIntoView({{ behavior: 'smooth' }});
        }}
      }}
      
      (function() {{
        const container = document.getElementById('scroll-container');
        const dots = document.querySelectorAll('.nav-dot');
        const hint = document.querySelector('.scroll-hint');
        
        if (!container || dots.length === 0) return;
        
        const updateDots = function() {{
          const scrollTop = container.scrollTop;
          const sectionHeight = container.clientHeight;
          const currentIndex = Math.round(scrollTop / sectionHeight);
          
          dots.forEach(function(dot, i) {{
            dot.classList.toggle('active', i === currentIndex);
          }});
          
          if (hint) {{
            hint.style.opacity = currentIndex > 0 ? '0' : '';
          }}
        }};
        
        container.addEventListener('scroll', updateDots);
      }})();
      
      // Series search (chart 2) 
      const seriesData = {search_json};
      
      setTimeout(function() {{
        const searchInput = document.getElementById('seriesSearch');
        const resultsDiv = document.getElementById('searchResults');
        const svgContainer = document.getElementById('plot2-container');
        
        if (!searchInput || !svgContainer) return;
        
        const interactiveCircles = Array.from(
          svgContainer.querySelectorAll('circle[data-id]')
        );
        
        searchInput.addEventListener('input', function() {{
          const query = this.value.toLowerCase().trim();
          const cleanQuery = query.replace(/[^a-z0-9]/g, '');
          
          if (cleanQuery.length < 2) {{
            interactiveCircles.forEach(el => {{
              el.style.opacity = '';
              el.style.stroke = '';
              el.style.strokeWidth = '';
              el.style.pointerEvents = '';
            }});
            resultsDiv.textContent = '';
            return;
          }}
          
          const matchIndices = new Set();
          const matchedNames = new Set();
          seriesData.forEach((s, i) => {{
            if (s.sid.includes(cleanQuery) || s.name.toLowerCase().includes(query)) {{
              matchIndices.add(i);
              matchedNames.add(s.name);
            }}
          }});
          
          if (matchIndices.size === 0) {{
            resultsDiv.textContent = 'No series found';
            interactiveCircles.forEach(el => {{ 
              el.style.opacity = '0.06'; 
              el.style.pointerEvents = 'none';
            }});
            return;
          }}
          
          const nameArr = [...matchedNames];
          resultsDiv.textContent = nameArr.length + ' series: ' + 
            nameArr.slice(0, 3).join(', ') + 
            (nameArr.length > 3 ? '...' : '');
          
          interactiveCircles.forEach((el, i) => {{
            if (matchIndices.has(i)) {{
              el.style.opacity = '1';
              el.style.stroke = '#000000';
              el.style.strokeWidth = '1.5px';
              el.style.pointerEvents = '';
            }} else {{
              el.style.opacity = '0.06';
              el.style.stroke = '';
              el.style.strokeWidth = '';
              el.style.pointerEvents = 'none';
            }}
          }});
        }});
      }}, 2000);
    ")))
  )
)

# Save ----

save_html(page, file = here("output", "tv_series_imdb_2000_2024.html"))