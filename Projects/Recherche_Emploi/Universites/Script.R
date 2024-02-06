# Job offer scraping - French universities (all platforms)
# Uses chromote (Chrome headless browser)

# PREREQUISITES:
# Google Chrome installed on the PC

# TWO USAGE MODES:
# - "initial" MODE : first run, scrapes ALL offers
# - "update" MODE  : subsequent runs, merges without duplicates


# Packages ----

library(chromote)    # R interface to Chrome DevTools Protocol (headless browser)
library(rvest)       # HTML parsing and CSS/XPath extraction
library(tidyverse)   # Data manipulation (dplyr, stringr, purrr, readr, tibble)
library(stringi)     # Advanced string operations (accent removal)
library(jsonlite)    # JSON parsing (results from JS scripts injected into Chrome)
library(here)        # Automatic path resolution relative to R project



# CONFIGURATION ----

run_mode <- "update"





# Files ----


# Reference CSV file containing the list of French universities,
# their recruitment URLs, and the "CSS family" of each site

universities_csv_path <- here("Data", "universites_france_metro.csv")


# FULL file: all raw offers before filtering

full_csv_path     <- here("Data", "All_Offers_Universities.csv")


# FILTERED file: offers matching technical keywords

filtered_csv_path      <- here("Data", "Offers_Universities.csv")


# Intermediate backup every 50 offers (crash protection)

backup_path      <- here("Data", "Intermediate_Backup_Univ.csv")


dir.create(here("Data"), showWarnings = FALSE, recursive = TRUE)




# Wait times (seconds)

page_wait  <- 6   # Wait after navigating to a listing page (university sites are often slow to load)
offer_wait <- 1   # Minimum time between each offer (+ smart <h1> wait)


# Global timer to measure total execution time

script_start <- Sys.time()
click_wait  <- 2   # Wait after each "show more" click (AJAX pagination)




# Utility functions ----


# JavaScript: click on "show more" / AJAX pagination buttons


# Many university sites use an "Afficher plus" or
# "Charger plus" button to reveal the next offers (client-side pagination)
# This JS script looks for these buttons and clicks on them automatically
# It handles two cases:
#   1. The #plus-offres button (specific to "nous-recrutons" / jet-engine sites)
#   2. Any button/link whose text contains pagination keywords


js_click_show_more <- '
(function() {
  // Method 1: #plus-offres button (nous-recrutons / jet-engine)
  var plusOffres = document.querySelector("#plus-offres");
  if (plusOffres) { plusOffres.click(); return 1; }
  
  // Method 2: buttons/links with short and relevant text
  var clicked = 0;
  document.querySelectorAll("button, a, span, div, [role=button]").forEach(function(el) {
    var txt = (el.innerText || "").trim().toLowerCase();
    if (txt.length > 80) return;
    if (txt.match(/afficher plus|charger plus|voir plus|voir tout|load more|plus d.offres|afficher la suite|afficher les offres suivantes/)) {
      el.click();
      clicked++;
    }
  });
  return clicked;
})()'




# JavaScript: expand all hidden content


js_expand_all <- '
(function() {
  document.querySelectorAll("button, a, span, div").forEach(function(el) {
    var txt = (el.innerText || "").trim().toLowerCase();
    if (txt.match(/afficher la suite|voir plus|lire la suite|voir tout|voir l.offre|show more/i)) {
      el.click();
    }
  });
  document.querySelectorAll("[aria-hidden=true], .collapsed, .hidden-content, .truncated, details:not([open])").forEach(function(el) {
    if (el.tagName === "DETAILS") { el.open = true; }
    el.setAttribute("aria-hidden", "false");
    el.classList.remove("collapsed", "hidden-content", "truncated");
    el.style.display = "";
    el.style.maxHeight = "none";
    el.style.overflow = "visible";
  });
  document.querySelectorAll("[style*=max-height], [style*=overflow]").forEach(function(el) {
    el.style.maxHeight = "none";
    el.style.overflow = "visible";
  });
  return "done";
})()
'



# JavaScript: extract main text from a page

js_get_main_text <- '
(function() {
  var selectors = ["#main-content", "main", "article", "#content", ".content", "#main"];
  for (var i = 0; i < selectors.length; i++) {
    var el = document.querySelector(selectors[i]);
    if (el && el.innerText.trim().length > 50) {
      return el.innerText;
    }
  }
  return document.body.innerText;
})()
'



# FUNCTIONS: Generate collection JS based on CSS family ----


# This is the core of the multi-site adaptation system. Each university uses
# a different CMS or framework (WordPress + Jet Engine, Flatchr, WP Job Manager,
# custom sites, etc.). The reference CSV file associates each university with
# a "CSS family" identified by characteristic selectors

# This function takes the CSS family class as input and returns
# the adapted JavaScript code to extract offers from that family.
# Each JS script returns a JSON array: [{href: "...", text: "..."}, ...]

# Supported families:

#   1.  jet-engine         : "nous-recrutons" sites (Elementor + Jet Engine)
#   2a. job__title          : AMU (Aix-Marseille Université)
#   2b. title-offers        : Artois / Flatchr (SaaS recruitment platform)
#   2c. job-card-container  : Avignon
#   2d. job-offer-data      : Bordeaux (AJAX loading via form)
#   2e. child__infos        : La Rochelle
#   2f. data-custom-edit    : Littoral (Elementor custom)
#   2g. offresstagesemplois : Côte d'Azur
#   2h. simple table        : Nîmes (offers in HTML table)
#   2i. job_listings        : Reims / Rennes (WP Job Manager)
#   2j. striped-item        : Rouen
#   2k. card__inner         : Strasbourg


generate_collection_js <- function(class_info) {
	
	
	# Normalize
	
	ci <- str_replace_all(class_info, "\u00a0", " ") %>% str_trim()
	
	
	# FAMILY 1: jet-engine (nous-recrutons and variants)
	
	if (str_detect(ci, "jet-engine-listing-overlay-link")) {
		
		
		# The title can be in different places depending on the variant
		# We search all possible locations with a fallback chain
		
		js <- '
(function() {
  var results = [];
  var seen = {};
  document.querySelectorAll("[data-post-id]").forEach(function(el) {
    var a = el.querySelector("a.jet-engine-listing-overlay-link");
    if (!a) return;
    var href = a.href;
    if (!href || seen[href]) return;
    
    // Fallback chain for title
    var text = "";
    var titreEl = el.querySelector(".jet-listing-dynamic-field__content");
    if (titreEl) text = titreEl.innerText.trim();
    if (!text) {
      titreEl = el.querySelector("h3.elementor-heading-title, .elementor-heading-title");
      if (titreEl) text = titreEl.innerText.trim();
    }
    if (!text) {
      titreEl = el.querySelector("h3");
      if (titreEl) text = titreEl.innerText.trim();
    }
    if (!text) text = a.innerText.trim();
    
    seen[href] = true;
    results.push({href: href, text: text.substring(0, 200)});
  });
  return JSON.stringify(results);
})()'
		
		return(js)
	}
	
	
	
	# FAMILY 2a: AMU (job__title)
	
	if (str_detect(ci, "job__title")) {
		return('
(function() {
  var results = [];
  var seen = {};
  document.querySelectorAll(".job__title").forEach(function(el) {
    var a = el.querySelector("a[href]") || el.closest("a[href]");
    if (!a) return;
    var href = a.href;
    if (!href || seen[href] || href.match(/[?&]f(%5B|\\[)/)) return;
    if (href.match(/\\/interne|\\/emplois-etudiant/)) return;
    var span = el.querySelector("span") || el;
    var text = span.innerText.trim();
    seen[href] = true;
    results.push({href: href, text: text.substring(0, 200)});
  });
  return JSON.stringify(results);
})()')
	}
	
	
	# FAMILY 2b: Artois / Flatchr (title-offers)
	
	if (str_detect(ci, "title-offers")) {
		return('
(function() {
  var results = [];
  var seen = {};
  document.querySelectorAll(".title-offers").forEach(function(el) {
    var a = el.querySelector("a[href]") || el.closest("a[href]");
    if (!a) return;
    var href = a.href;
    if (!href || seen[href]) return;
    var text = a.innerText.trim();
    seen[href] = true;
    results.push({href: href, text: text.substring(0, 200)});
  });
  return JSON.stringify(results);
})()')
	}
	
	
	# FAMILY 2c: Avignon (job-card-container + stretched-link)
	
	if (str_detect(ci, "job-card-container")) {
		return('
(function() {
  var results = [];
  var seen = {};
  document.querySelectorAll(".job-card-container").forEach(function(el) {
    var a = el.querySelector("a.stretched-link") || el.querySelector("a[href]");
    if (!a) return;
    var href = a.href;
    if (!href || seen[href]) return;
    var text = a.innerText.trim();
    seen[href] = true;
    results.push({href: href, text: text.substring(0, 200)});
  });
  return JSON.stringify(results);
})()')
	}
	
	
	# FAMILY 2d: Bordeaux (job-offer-data)
	
	# Offers are loaded via AJAX (#formFilterOffres form)
	# The JS must first check if offers are already present,
	# otherwise submit the form and return empty (the script will re-collect after waiting)
	
	if (str_detect(ci, "job-offer-data")) {
		return('
(function() {
  var results = [];
  var seen = {};
  // Look for already loaded offers
  document.querySelectorAll(".job-offer-data, .job-offer").forEach(function(el) {
    var a = el.querySelector("a.delegate-hover") || el.closest("article").querySelector("a[href]") || el.querySelector("a[href]");
    if (!a) return;
    var href = a.href;
    if (!href || seen[href]) return;
    var titreEl = el.closest("article") ? el.closest("article").querySelector(".card-item-title, .job-offer-title, h2") : null;
    if (!titreEl) titreEl = el.querySelector(".card-item-title, .job-offer-title");
    var text = titreEl ? titreEl.innerText.trim() : a.innerText.trim();
    seen[href] = true;
    results.push({href: href, text: text.substring(0, 200)});
  });
  // If nothing found, try to submit the form
  if (results.length === 0) {
    var form = document.querySelector("#formFilterOffres");
    if (form && typeof jQuery !== "undefined") {
      jQuery("#formFilterOffres").submit();
    }
  }
  return JSON.stringify(results);
})()')
	}
	
	
	# FAMILY 2e: La Rochelle (child__infos)
	
	if (str_detect(ci, "child__infos")) {
		return('
(function() {
  var results = [];
  var seen = {};
  document.querySelectorAll(".child__infos").forEach(function(el) {
    var a = el.querySelector("a[href]");
    if (!a) return;
    var href = a.href;
    if (!href || seen[href]) return;
    var text = a.innerText.trim();
    seen[href] = true;
    results.push({href: href, text: text.substring(0, 200)});
  });
  return JSON.stringify(results);
})()')
	}
	
	
	# FAMILY 2f: Littoral (data-custom-edit-handle + elementor-heading-title)
	
	if (str_detect(ci, "data-custom-edit-handle")) {
		return('
(function() {
  var results = [];
  var seen = {};
  document.querySelectorAll("[data-custom-edit-handle]").forEach(function(el) {
    var titreEl = el.querySelector(".elementor-heading-title");
    if (!titreEl) return;
    var a = titreEl.querySelector("a[href]") || titreEl.closest("a[href]");
    if (!a) return;
    var href = a.href;
    if (!href || seen[href]) return;
    var text = a.innerText.trim();
    seen[href] = true;
    results.push({href: href, text: text.substring(0, 200)});
  });
  return JSON.stringify(results);
})()')
	}
	
	
	# FAMILY 2g: Côte d'Azur (offresstagesemplois--type-0001)
	
	if (str_detect(ci, "offresstagesemplois")) {
		return('
(function() {
  var results = [];
  var seen = {};
  document.querySelectorAll(".offresstagesemplois--type-0001, [class*=offresstagesemplois]").forEach(function(el) {
    var a = el.querySelector("a[href]");
    if (!a) return;
    var href = a.href;
    if (!href || seen[href]) return;
    var text = a.innerText.trim();
    seen[href] = true;
    results.push({href: href, text: text.substring(0, 200)});
  });
  return JSON.stringify(results);
})()')
	}
	
	
	# FAMILY 2h: Nîmes (table.simple)
	
	if (str_detect(ci, "table") && str_detect(ci, "simple")) {
		return('
(function() {
  var results = [];
  var seen = {};
  document.querySelectorAll("table.simple tr, table tr").forEach(function(tr) {
    var tds = tr.querySelectorAll("td");
    if (tds.length === 0) return;
    var a = tr.querySelector("a[href]");
    var href = a ? a.href : "";
    var text = tds[0].innerText.trim();
    if (text.length < 3) return;
    if (!href) {
      // No link, build a pseudo-link from the text
      href = window.location.href + "#offre-" + text.replace(/\\s+/g, "-").substring(0, 50);
    }
    if (seen[href]) return;
    seen[href] = true;
    results.push({href: href, text: text.substring(0, 200)});
  });
  return JSON.stringify(results);
})()')
	}
	
	
	# FAMILY 2i: Reims / Rennes (job_listings)
	
	if (str_detect(ci, "job_listings")) {
		return('
(function() {
  var results = [];
  var seen = {};
  document.querySelectorAll(".job_listings li, ul.job_listings > li").forEach(function(li) {
    var a = li.querySelector("a[href]");
    if (!a) return;
    var href = a.href;
    if (!href || seen[href]) return;
    var titreEl = li.querySelector("h3.position, .position h3, .position");
    var text = titreEl ? titreEl.innerText.trim() : a.innerText.trim();
    if (text.length < 3) return;
    seen[href] = true;
    results.push({href: href, text: text.substring(0, 200)});
  });
  return JSON.stringify(results);
})()')
	}
	
	
	# FAMILY 2j: Rouen (striped-item--wrapper)
	
	if (str_detect(ci, "striped-item")) {
		return('
(function() {
  var results = [];
  var seen = {};
  document.querySelectorAll("[class*=striped-item]").forEach(function(el) {
    var a = el.querySelector(".card-title a[href]") || el.querySelector("a[href]");
    if (!a) return;
    var href = a.href;
    if (!href || seen[href]) return;
    var text = a.innerText.trim();
    seen[href] = true;
    results.push({href: href, text: text.substring(0, 200)});
  });
  return JSON.stringify(results);
})()')
	}
	
	
	# FAMILY 2k: Strasbourg (card__inner + fully-clickable)
	
	if (str_detect(ci, "card__inner")) {
		return('
(function() {
  var results = [];
  var seen = {};
  document.querySelectorAll(".card__inner").forEach(function(el) {
    var a = el.querySelector("a.fully-clickable") || el.querySelector("a[href]");
    if (!a) return;
    var href = a.href;
    if (!href || seen[href]) return;
    var titreEl = el.querySelector(".caption-1, [class*=caption]");
    var text = titreEl ? titreEl.innerText.trim() : a.innerText.trim();
    seen[href] = true;
    results.push({href: href, text: text.substring(0, 200)});
  });
  return JSON.stringify(results);
})()')
	}
	
	
	# FALLBACK: no recognized family => return NULL
	
	return(NULL)
}





# Extract a text section between two markers ----


extract_section <- function(text, start, ends) {
	end_pattern <- paste(ends, collapse = "|")
	pattern <- paste0("(?s)", start, "\\s*\\n(.+?)(?=", end_pattern, "|$)")
	match <- str_match(text, regex(pattern, ignore_case = TRUE))
	if (is.na(match[1, 2])) return(NA_character_)
	str_trim(match[1, 2])
}


## Common section ends ----

section_ends <- c(
	"Profil recherch[ée]",
	"Descriptif du profil",
	"Comp[ée]tences",
	"Niveau d'[ée]tudes",
	"Qui sommes[- ]nous",
	"Localisation",
	"[ÉE]l[ée]ments de candidature",
	"Informations compl[ée]mentaires",
	"Fondement juridique",
	"Conditions particuli[èe]res",
	"Pi[èe]ces [àa] fournir",
	"Personne [àa] contacter",
	"Renseignements",
	"Contact",
	"Management\\s*(Oui|Non)",
	"Cat[ée]gorie",
	"Candidater",
	"Postuler",
	"Date limite",
	"Pour candidater",
	"Merci d'adresser"
)


## Clean raw text ----

clean_text <- function(txt) {
	if (is.na(txt)) return(NA_character_)
	txt %>%
		str_replace_all("(?i)Afficher la suite", "") %>%
		str_replace_all("\\n{3,}", "\n\n") %>%
		str_trim()
}



## Check/relaunch Chrome if needed ----

check_chrome <- function(b) {
	ok <- tryCatch({ b$Runtime$evaluate("1+1"); TRUE }, error = function(e) FALSE)
	if (!ok) {
		cat("  🔄 Relaunching Chrome...\n")
		tryCatch(b$close(), error = function(e) NULL)
		Sys.sleep(2)
		b <- ChromoteSession$new()
		Sys.sleep(1)
		cat("  ✅ New Chrome session\n")
	}
	b
}


## Extract a field from raw text ----

extract_field <- function(text, pattern) {
	m <- str_match(text, regex(pattern, ignore_case = TRUE))
	if (is.na(m[1, 2])) return(NA_character_)
	str_squish(m[1, 2])
}







# STEP 1: Read the university reference file ----


# The universites_france_metro.csv file contains for each university:
# - nom_universite: official name
# - ville: main city
# - url_recrutement: recruitment page URL
# - class: "CSS family" identifier (e.g. "jet-engine-listing-overlay-link") or "manuellement" if the site cannot be scraped automatically

# We separate universities into two groups:
# - auto_df: automatically scrapable sites (recognized class)
# - manual_df: sites requiring manual verification


cat("MODE:", toupper(run_mode), "\n")
cat("STEP 1: Reading universities file \n\n")

univ_df <- read_delim(universities_csv_path, delim = ";",
											col_types = cols(.default = "c"),
											locale = locale(encoding = "UTF-8")) %>%
	rename_with(~ str_trim(.x)) %>%   # remove spaces in column names
	filter(!is.na(url_recrutement) & url_recrutement != "") %>%
	mutate(class = str_replace_all(class, "\u00a0", " ") %>% str_trim())


# Separate automatic sites from manual ones

auto_df <- univ_df %>% filter(class != "manuellement" & !is.na(class) & class != "")
manual_df <- univ_df %>% filter(class == "manuellement" | is.na(class) | class == "")


cat("Total universities      :", nrow(univ_df), "\n")
cat("Automatic universities  :", nrow(auto_df), "\n")
cat("Manual universities     :", nrow(manual_df), "\n\n")

if (nrow(manual_df) > 0) {
	cat("Manual sites (check yourself):\n")
	pwalk(manual_df, ~ cat(sprintf("  - %s (%s)\n", ..1, ..2)))
	cat("\n")
}




# STEP 2: Retrieve offer links from each listing page ----


# For each "automatic" university, we:
# 1. Navigate to its recruitment page with Chrome
# 2. Click on "Afficher plus" as many times as needed (max 20)
# 3. Execute the collection JS adapted to its CSS family
# 4. Retrieve the list of offers (title + URL)

# Note: some sites (e.g. Bordeaux) load offers via AJAX
# after form submission. The script handles this case with a retry
# after 5 seconds of additional waiting


cat("STEP 2: Retrieving offer links \n\n")

b <- ChromoteSession$new()
Sys.sleep(2)

all_links <- list()

for (i in seq_len(nrow(auto_df))) {
	
	name  <- auto_df$nom_universite[i]
	url   <- auto_df$url_recrutement[i]
	city  <- auto_df$ville[i]
	ci    <- auto_df$class[i]
	
	cat(sprintf("[%d/%d] %s\n", i, nrow(auto_df), name))
	
	
	# Generate the adapted collection JS
	
	collection_js <- generate_collection_js(ci)
	
	if (is.null(collection_js)) {
		cat("⚠ No recognized CSS family, skip\n\n")
		next
	}
	
	b <- check_chrome(b)
	
	tryCatch({
		
		b$Page$navigate(url)
		Sys.sleep(page_wait)
		
		
		# Click on "show more" multiple times if needed
		
		for (click in 1:20) {
			nb_clicks <- tryCatch(
				b$Runtime$evaluate(js_click_show_more)$result$value,
				error = function(e) 0
			)
			if (is.null(nb_clicks) || nb_clicks == 0) break
			cat("  Click 'show more' #", click, "\n")
			Sys.sleep(3)  # longer wait to let AJAX loading complete
		}
		
		
		# Wait a bit after clicks for loading
		
		Sys.sleep(2)
		
		
		# Retrieve links with the adapted JS
		
		links_json <- tryCatch(
			b$Runtime$evaluate(collection_js)$result$value,
			error = function(e) "[]"
		)
		links <- tryCatch(fromJSON(links_json), error = function(e) data.frame())
		
		
		# If 0 results, retry after waiting (useful for AJAX, e.g. Bordeaux)
		
		if (!is.data.frame(links) || nrow(links) == 0) {
			cat("⏳ 0 offers, retry after waiting...\n")
			Sys.sleep(5)
			links_json <- tryCatch(
				b$Runtime$evaluate(collection_js)$result$value,
				error = function(e) "[]"
			)
			links <- tryCatch(fromJSON(links_json), error = function(e) data.frame())
		}
		
		if (is.data.frame(links) && nrow(links) > 0) {
			univ_links <- links %>% distinct(href, .keep_all = TRUE)
		} else {
			univ_links <- tibble(href = character(), text = character())
		}
		
		cat("Total:", nrow(univ_links), "offer(s) found\n")
		
		if (nrow(univ_links) > 0) {
			all_links[[i]] <- univ_links %>%
				mutate(
					nom_universite = name,
					ville = city,
					url_listing = url
				)
		}
		
	}, error = function(e) {
		cat("⚠ Error:", conditionMessage(e), "\n")
	})
	
	cat("\n")
}



# Assemble all links

links_df <- bind_rows(all_links) %>%
	rename(offer_url = href, listing_title = text) %>%
	distinct(offer_url, .keep_all = TRUE)

cat("=== Total:", nrow(links_df), "unique offers found ===\n\n")




# In update mode: exclude already known offers

if (run_mode == "update" && file.exists(full_csv_path)) {
	existing_df <- read_csv2(full_csv_path, show_col_types = FALSE)
	existing_urls <- existing_df$url
	
	nb_before <- nrow(links_df)
	links_df <- links_df %>% filter(!(offer_url %in% existing_urls))
	cat("Already known offers:", nb_before - nrow(links_df), "\n")
	cat("New offers to scrape:", nrow(links_df), "\n\n")
	
	if (nrow(links_df) == 0) {
		cat("No new offers. Moving to filtering.\n")
	}
}




# STEP 3: Scrape the detail of each individual offer ----


# For each new offer found in step 2, we:
# 1. Navigate to the offer page with Chrome
# 2. Wait for <h1> to load (smart wait, max 10s)
# 3. Expand hidden content (JS expand_all)
# 4. Extract the title (h1 > title > listing title as fallback)
# 5. Extract the main text (main > article > body as fallback)
# 6. Extract structured fields via regex (contract type, category, etc.)
# 7. Extract long sections (description, profile, skills)

# Resilience mechanism: in case of 3 consecutive errors, Chrome is
# automatically relaunched (the session can crash on heavy sites)


if (nrow(links_df) > 0) {
	
	cat("STEP 3: Scraping the detail of each offer\n\n")
	
	results <- list()
	consecutive_errors <- 0
	
	for (j in seq_len(nrow(links_df))) {
		
		offer_url  <- links_df$offer_url[j]
		univ_name  <- links_df$nom_universite[j]
		univ_city  <- links_df$ville[j]
		list_title <- links_df$listing_title[j]
		
		cat(sprintf("[%d/%d] %s\n", j, nrow(links_df), str_trunc(offer_url, 90)))
		
		tryCatch({
			
			b <- check_chrome(b)
			
			b$Page$navigate(offer_url)
			
			
			# Wait for <h1> to load
			
			page_ready <- FALSE
			for (attempt in 1:10) {
				Sys.sleep(offer_wait)  # 1s per attempt
				check <- tryCatch(
					b$Runtime$evaluate('document.querySelector("h1") !== null')$result$value,
					error = function(e) FALSE
				)
				if (isTRUE(check)) { page_ready <- TRUE; break }
			}
			
			if (!page_ready) {
				cat("⚠ Page not loaded after 10s, trying anyway\n")
			}
			
			
			# Expand hidden content (single call + short pause)
			
			b$Runtime$evaluate(js_expand_all)
			Sys.sleep(0.5)
			
			
			# Retrieve rendered HTML
			
			rendered_html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
			offer_html <- read_html(rendered_html)
			
			
			# Title
			
			title <- offer_html %>%
				html_element("h1") %>%
				html_text2() %>%
				str_squish()
			
			
			# If title empty
			
			if (is.na(title) || nchar(title) < 3) {
				cat("⏳ Empty title, retry...\n")
				b$Page$navigate(offer_url)
				for (attempt in 1:10) {
					Sys.sleep(1)
					check <- tryCatch(
						b$Runtime$evaluate('document.querySelector("h1") !== null')$result$value,
						error = function(e) FALSE
					)
					if (isTRUE(check)) break
				}
				
				rendered_html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
				offer_html <- read_html(rendered_html)
				
				title <- offer_html %>%
					html_element("h1") %>%
					html_text2() %>%
					str_squish()
			}
			
			
			# Fallback: page <title>
			
			if (is.na(title) || nchar(title) < 3) {
				title <- offer_html %>%
					html_element("title") %>%
					html_text2() %>%
					str_squish()
			}
			
			
			# Last fallback: listing title
			
			if (is.na(title) || nchar(title) < 3) {
				title <- list_title
			}
			
			
			# Retrieve main text
			
			raw_text <- tryCatch(
				b$Runtime$evaluate(js_get_main_text)$result$value,
				error = function(e) ""
			)
			
			if (is.null(raw_text) || nchar(raw_text) < 50) {
				raw_text <- offer_html %>%
					html_element("main, #main-content, #content, article") %>%
					html_text2()
				if (is.na(raw_text)) raw_text <- ""
			}
			
			
			# Last text fallback: entire body
			
			if (nchar(raw_text) < 50) {
				raw_text <- tryCatch(
					b$Runtime$evaluate("document.body.innerText")$result$value,
					error = function(e) ""
				)
				if (is.null(raw_text)) raw_text <- ""
			}
			
			
			
			# Field extraction
			
			contract_type <- extract_field(raw_text,
																		 "(?:Type de contrat|Statut|Nature de l'emploi|Nature du contrat)\\s*:?\\s*(.+?)(?:\\n|$)")
			
			category <- extract_field(raw_text,
																"Cat[ée]gorie\\s*:?\\s*([ABC][+]?(?:\\s*\\(.+?\\))?)(?:\\n|$)")
			
			location <- extract_field(raw_text,
																"(?:Localisation|Lieu d'exercice|Lieu|Affectation|Site)\\s*:?\\s*(.+?)(?:\\n|$)")
			
			start_date <- extract_field(raw_text,
																	"(?:Date de d[ée]but|Prise de (?:poste|fonction)|Date pr[ée]visionnelle)\\s*:?\\s*(.+?)(?:\\n|$)")
			
			deadline <- extract_field(raw_text,
																"(?:Date limite|Candidater avant|Fin de publication|Date de fin|Ouvert jusqu)\\s*:?\\s*(.+?)(?:\\n|$)")
			
			duration <- extract_field(raw_text,
																"(?:Dur[ée]e du contrat|Dur[ée]e)\\s*:?\\s*(.+?)(?:\\n|$)")
			
			
			description <- extract_section(raw_text,
																		 "(?:Missions?|Description du poste|Votre mission|Descriptif|Activit[ée]s? principales?|Fiche de poste|D[ée]tail)",
																		 section_ends)
			
			profile <- extract_section(raw_text,
																 "(?:Profil recherch[ée]|Votre profil|Descriptif du profil|Profil souhait[ée]|Comp[ée]tences et qualit[ée]s requises)",
																 section_ends)
			
			skills <- extract_section(raw_text,
																"(?:Comp[ée]tences|Savoir[- ]faire|Connaissances)",
																section_ends)
			
			
			# If no description, raw text fallback
			
			if (is.na(description) || nchar(description) < 20) {
				description <- raw_text %>%
					str_replace(".*?(?=\\n)", "") %>%
					str_sub(1, 3000) %>%
					str_trim()
			}
			
			description <- clean_text(description)
			profile     <- clean_text(profile)
			skills      <- clean_text(skills)
			
			
			# Store
			
			results[[j]] <- tibble(
				title            = title,
				nom_universite   = univ_name,
				ville            = univ_city,
				contract_type    = contract_type,
				category         = category,
				location         = location,
				start_date       = start_date,
				deadline         = deadline,
				duration         = duration,
				description      = description,
				profile          = profile,
				skills           = skills,
				url              = offer_url,
				still_online     = "oui",
				scraping_date    = as.character(Sys.Date())
			)
			
			consecutive_errors <- 0
			cat("✅", str_trunc(title %||% "(no title)", 60), "\n")
			
		}, error = function(e) {
			cat("⚠ Error:", conditionMessage(e), "\n")
			results[[j]] <<- tibble(
				title = list_title, nom_universite = univ_name, ville = univ_city,
				contract_type = NA, category = NA,
				location = NA, start_date = NA, deadline = NA,
				duration = NA, description = NA, profile = NA, skills = NA,
				url = offer_url,
				still_online = "oui",
				scraping_date    = as.character(Sys.Date())
			)
			consecutive_errors <<- consecutive_errors + 1
			
			if (consecutive_errors >= 3) {
				cat("  🔄 3 consecutive errors, relaunching Chrome...\n")
				tryCatch(b$close(), error = function(e) NULL)
				Sys.sleep(2)
				b <<- ChromoteSession$new()
				Sys.sleep(1)
				consecutive_errors <<- 0
				cat("✅ New Chrome session\n")
			}
		})
		
		
		# Intermediate backup every 50 offers
		
		if (j %% 50 == 0) {
			df_temp <- bind_rows(results)
			write_csv2(df_temp, backup_path)
			cat(sprintf("  💾 Backup: %d offers (%d non-empty)\n\n",
									nrow(df_temp), sum(!is.na(df_temp$title))))
		}
	}
	
	tryCatch(b$close(), error = function(e) NULL)
	
	
	
	
	
	# STEP 4: Assemble and merge with existing ----
	
	
	# In addition to the classic merge, this script also manages
	# the "still_online" tracking: if an offer was in our file but
	# no longer appears on any listing today, we mark it as "non"
	# (the offer was probably filled or removed). This allows me to see if an offer I had strong interest in
	# among the filtered offers is still present or not
	
	
	cat("\n STEP 4: Assembly \n")
	
	new_df <- bind_rows(results)
	cat("Offers scraped:", nrow(new_df), "\n")
	cat("Offers with title:", sum(!is.na(new_df$title)), "\n")
	
	if (run_mode == "update" && file.exists(full_csv_path)) {
		existing_df <- read_csv2(full_csv_path, show_col_types = FALSE)
		
		
		# URLs seen today on listings (includes new + already known)
		
		urls_seen_today <- links_df$offer_url
		
		
		# New offers (not yet in the file)
		
		unique_new_df <- new_df %>%
			filter(!(url %in% existing_df$url))
		cat("New offers:", nrow(unique_new_df), "\n")
		
		
		# Update still_online for existing ones
		
		existing_df <- existing_df %>%
			mutate(
				still_online = if_else(url %in% urls_seen_today, "oui", "non")
			)
		
		nb_gone <- sum(existing_df$still_online == "non")
		cat("Offers gone from listings:", nb_gone, "\n")
		
		offers_df <- bind_rows(unique_new_df, existing_df)
		cat("Total after merge:", nrow(offers_df), "\n")
	} else {
		offers_df <- new_df
	}
	
	write_csv2(offers_df, full_csv_path)
	cat("Full file saved:", full_csv_path, "\n")
	
} else {
	if (file.exists(full_csv_path)) {
		offers_df <- read_csv2(full_csv_path, show_col_types = FALSE)
		
		
		# Even without new offers, update still_online
		
		if (run_mode == "update" && exists("links_df") && nrow(links_df) >= 0) {
			urls_seen_today <- links_df$offer_url
			offers_df <- offers_df %>%
				mutate(
					still_online = if_else(url %in% urls_seen_today, "oui", "non")
				)
			nb_gone <- sum(offers_df$still_online == "non")
			cat("Offers gone from listings:", nb_gone, "\n")
			write_csv2(offers_df, full_csv_path)
			cat("File updated (still_online):", full_csv_path, "\n")
		}
	} else {
		cat("No offers found and no existing file.\n")
		quit()
	}
}





# STEP 5: Technical keyword filtering ----


# Same logic as for the Choisirleservicepublic script: we only keep offers mentioning
# relevant technical skills. The search is insensitive to
# accents and case. Matched keywords are stored for each offer


cat("\n STEP 5: Keyword filtering \n")


# Function to remove all accents from a string

remove_accents <- function(x) stri_trans_general(x, "Latin-ASCII")


# Technical keywords (identical to Script 1 for consistency)

filter_keywords <- c(
	"donnee[s]?",
	"statistique[s]?",
	"modelisation[s]?",
	"geomatique",
	"\\bSIG\\b",
	"\\bR\\b",
	"langage.?R",
	"shiny",
	"rstudio",
	"data.?scientist[s]?",
	"data.?analyst[s]?",
	"data.?engineer",
	"machine.?learning",
	"deep.?learning",
	"intelligence.?artificielle",
	"\\bIA\\b",
	"\\bSQL\\b",
	"\\bSAS\\b",
	"tableau[x]?\\s*de\\s*bord",
	"visualisation",
	"analyse",
	"open.?data",
	"big.?data",
	"entrepot[s]?\\s*de\\s*donnee",
	"base[s]?\\s*de\\s*donnee"
)

filter_pattern <- paste(filter_keywords, collapse = "|")

detect <- function(text, pattern) {
	str_detect(
		remove_accents(tolower(coalesce(text, ""))),
		pattern
	)
}

filtered_df <- offers_df %>%
	filter(
		detect(description, filter_pattern) |
			detect(profile, filter_pattern) |
			detect(skills, filter_pattern) |
			detect(title, filter_pattern)
	)

cat("Offers after filter:", nrow(filtered_df), "/", nrow(offers_df), "\n")


# Matched keywords for each offer

filtered_df <- filtered_df %>%
	rowwise() %>%
	mutate(
		matched_keywords = {
			full_text <- remove_accents(tolower(paste(
				coalesce(title, ""), coalesce(description, ""),
				coalesce(profile, ""), coalesce(skills, "")
			)))
			matches <- filter_keywords[map_lgl(filter_keywords, ~ str_detect(full_text, .x))]
			paste(matches, collapse = ", ")
		}
	) %>%
	ungroup()


# In update mode, add to existing filtered offers

if (run_mode == "update" && file.exists(filtered_csv_path)) {
	existing_filtered_df <- read_csv2(filtered_csv_path, show_col_types = FALSE)
	
	new_filtered_df <- filtered_df %>%
		filter(!(url %in% existing_filtered_df$url))
	
	cat("New filtered offers:", nrow(new_filtered_df), "\n")
	filtered_df <- bind_rows(new_filtered_df, existing_filtered_df)
	cat("Total filtered:", nrow(filtered_df), "\n")
}

filtered_df <- filtered_df %>%
	select(title, matched_keywords, nom_universite, ville, everything()) %>%
	mutate(number = row_number(), .before = 1)

write_csv2(filtered_df, filtered_csv_path)
cat("Filtered file saved:", filtered_csv_path, "\n")




# SUMMARY ----

cat("\n SUMMARY \n\n")

cat("Universities scraped (auto):", nrow(auto_df), "\n")
cat("Manual universities        :", nrow(manual_df), "\n")
cat("Total offers                :", nrow(offers_df), "\n")
cat("Filtered offers             :", nrow(filtered_df), "\n\n")


if (nrow(filtered_df) > 0) {
	
	cat("--- Filtered offers by university ---\n")
	
	filtered_df %>%
		count(nom_universite, sort = TRUE) %>%
		pwalk(~ cat(sprintf("  %-45s : %d offer(s)\n", ..1, ..2)))
	
	
	cat("\n--- Filtered offers detail ---\n\n")
	
	for (k in 1:min(nrow(filtered_df), 30)) {
		cat(sprintf("--- Offer %d ---\n", k))
		cat("Title       :", filtered_df$title[k], "\n")
		cat("University  :", filtered_df$nom_universite[k], "\n")
		cat("City        :", filtered_df$ville[k], "\n")
		cat("Contract    :", filtered_df$contract_type[k], "\n")
		cat("Category    :", filtered_df$category[k], "\n")
		cat("Keywords    :", filtered_df$matched_keywords[k], "\n")
		cat("URL         :", filtered_df$url[k], "\n\n")
	}
}

cat("\n Done! \n")
duration <- round(difftime(Sys.time(), script_start, units = "mins"), 1)
cat("Total duration:", duration, "minutes\n")