# Job offer scraping - Choisir le Service Public (v6)
# Uses chromote (Chrome headless browser)

# PREREQUISITES:
# Google Chrome installed on the PC

# TWO USAGE MODES:
# - "INITIAL" MODE  : first run, scrapes ALL offers (20 pages but could be much more)
# - "UPDATE" MODE   : subsequent runs, scrapes only the last 7 days and merges with existing CSV without duplicates



# Packages ----

library(chromote)   # R interface to Chrome DevTools Protocol (headless browser)
library(rvest)      # HTML parsing and CSS/XPath extraction
library(tidyverse)  # Data manipulation (dplyr, stringr, purrr, readr, tibble)
library(stringi)    # Advanced string operations (notably accent removal)
library(here)       # Automatic path resolution relative to R project




# Run mode ----


run_mode <- "update"







# File paths ----


# FULL file: all raw offers before keyword filtering

full_csv_path <- here("Data", "All_Offers.csv")


# FILTERED file: subset of offers matching technical keywords

filtered_csv_path <- here("Data", "Offers.csv")


# Intermediate backup: written every 50 offers in case of crash

intermediate_backup_path <- here("Data", "Intermediate_Backup.csv")


# Automatic creation of Data/ folder if it doesn't exist yet

dir.create(here("Data"), showWarnings = FALSE, recursive = TRUE)





# Search URLs on the Choisir le Service Public portal ----


# The keywords "statistique données ingénieur" are encoded in the URL

# Initial mode URL: all matching offers, no date filter

url_all <- "https://choisirleservicepublic.gouv.fr/nos-offres/filtres/mot-cles/statistique%20donn%C3%A9es%20ing%C3%A9nieur/page/"


# Update mode URL: only offers published in the last 7 days

url_7days  <- "https://choisirleservicepublic.gouv.fr/nos-offres/filtres/mot-cles/statistique%20donn%C3%A9es%20ing%C3%A9nieur/date-de-publication/7_derniers_jours/page/"





# Automatic detection of the number of pages in update mode ----


# We read the results page (last 7 days) and retrieve the number
# of the last page from the pagination link ("last page" button)


max_pages <- read_html("https://choisirleservicepublic.gouv.fr/nos-offres/filtres/mot-cles/statistique%20donn%C3%A9es%20ing%C3%A9nieur/date-de-publication/7_derniers_jours/") %>%
	html_element("a.fr-pagination__link--last") %>%    # CSS selector for the "last page" button
	html_attr("href") %>%                              # Retrieves the target URL of the link
	str_extract("page/(\\d+)", group = 1) %>%          # Extracts the page number
	as.numeric()


# print(max_pages)



# Select URL and number of pages based on chosen mode

if (run_mode == "initial") {
	base_url <- url_all
	nb_pages <- 20                # Fixed number of pages in initial mode
} else {
	base_url <- url_7days
	nb_pages <- max_pages         # Dynamically detected number
}



# Minimum wait time (in seconds) between each scraped offer
# The script also waits for the actual <h1> to load

offer_wait <- 1


# Global timer to measure total execution time

script_start <- Sys.time()





# STEP 1: Retrieve offer links from listing pages ----



# We use rvest::read_html() (not Chrome) because the site's listing pages
# are server-side rendered (static HTML). This is much faster than Chrome
# Chrome will only be used in step 2 for individual pages
# that contain dynamic content (JavaScript)



cat("MODE:", toupper(run_mode), "\n")
cat("STEP 1: Retrieving offer links \n")
cat("(Uses read_html — faster than Chrome for listing pages)\n\n")



# Vector to accumulate all found offer links

all_links <- character()


for (pg in 1:nb_pages) {
	
	url_page <- paste0(base_url, pg, "/")
	cat("Page", pg, "/", nb_pages, ":", url_page, "\n")
	
	
	# Page reading with error handling + 1 automatic retry
	
	page_html <- tryCatch(
		read_html(url_page),
		error = function(e) {
			cat("  ⚠ Error reading page:", conditionMessage(e), "\n")
			Sys.sleep(2)
			tryCatch(read_html(url_page), error = function(e2) NULL)
		}
	)
	
	
	# If the page is unreadable even after retry, skip to the next one
	
	if (is.null(page_html)) {
		cat("  -> Failed after retry, skipping to next\n")
		next
	}
	
	
	# Extract all <a href="..."> links containing "/offre-emploi/"
	
	links <- page_html %>%
		html_elements("a") %>%
		html_attr("href") %>%
		na.omit() %>%
		unique() %>%
		keep(~ str_detect(., "/offre-emploi/"))   # Keep only offer links
	
	
	# Convert relative links to absolute links
	
	links <- ifelse(
		str_starts(links, "http"),
		links,
		paste0("https://choisirleservicepublic.gouv.fr", links)
	)
	
	
	# End of pagination detection: if a page returns nothing, we stop
	
	if (length(links) == 0) {
		cat("  -> 0 offers, end of pagination\n")
		break
	}
	
	cat("  ->", length(links), "offers found\n")
	all_links <- c(all_links, links)
	
	Sys.sleep(0.5)   # Pause to avoid overloading the server
}


# Deduplication of links (an offer can appear on multiple pages)

all_links <- unique(all_links)
cat("\nTotal:", length(all_links), "unique offers to scrape\n\n")




# In update mode: exclude offers already in the existing CSV ----


# This avoids re-scraping offers we already have. We compare URLs

if (run_mode == "update" && file.exists(full_csv_path)) {
	existing_df <- read_csv2(full_csv_path, show_col_types = FALSE)
	
	
	# Retrocompatible cleanup: removal of an obsolete old column
	
	if ("additional_info" %in% names(existing_df)) {
		existing_df <- existing_df %>% select(-additional_info)
	}
	
	existing_urls <- existing_df$url
	
	# setdiff(): keep only links NOT already present in the file
	
	new_links <- setdiff(all_links, existing_urls)
	cat("Already known offers:", length(all_links) - length(new_links), "\n")
	cat("New offers to scrape:", length(new_links), "\n\n")
	
	all_links <- new_links
	
	
	# If no new offers, load the existing data to rerun the filter
	# (useful if filtering keywords were modified between two runs)
	
	if (length(all_links) == 0) {
		cat("No new offers. The file is already up to date!\n")
		offers_df <- existing_df
	}
}





# STEP 2: Scrape the detail of each offer (via Chrome headless) ----



# Individual offer pages use JavaScript to display
# the full content (expandable sections, AJAX content). We therefore
# need to use a Chrome browser driven by chromote to get the HTML
# rendered after JS execution


if (length(all_links) > 0) {
	
	cat("STEP 2: Retrieving the detail of each offer \n")
	
	
	# Open a Chrome headless session (not needed before, step 1 doesn't use Chrome)
	
	b <- ChromoteSession$new()
	
	
	## JavaScript injected into each page to expand hidden content ----
	
	# Some offers have "Afficher la suite" or "Voir plus" sections that
	# hide text by default. This JS script:
	# 1. Clicks on all "afficher la suite" / "voir plus" buttons
	# 2. Forces display of hidden elements (aria-hidden, collapsed, etc.)
	# 3. Removes height and overflow CSS limits
	# 4. Removes text truncation classes (line-clamp, etc.)
	
	
	js_expand_all <- '
(function() {
  document.querySelectorAll("button, a, span").forEach(function(el) {
    if (el.innerText && el.innerText.trim().match(/afficher la suite|voir plus|lire la suite|voir tout/i)) {
      el.click();
    }
  });
  document.querySelectorAll("[aria-hidden=true], .collapsed, .hidden-content, .truncated").forEach(function(el) {
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
  document.querySelectorAll(".text-truncate, .truncate, .clamp, .line-clamp").forEach(function(el) {
    el.classList.remove("text-truncate", "truncate", "clamp", "line-clamp");
    el.style.webkitLineClamp = "unset";
    el.style.maxHeight = "none";
  });
  return "done";
})()
'
	
	
	## Utility functions ----
	
	# extract_section(): extracts a block of text between a start title
	# (e.g. "Vos missions en quelques mots") and one of the possible end titles
	# (e.g. "Profil recherché", "Compétences attendues", etc.)
	# Uses a regex with capture group to isolate the content between the two
	
	
	extract_section <- function(text, start, ends) {
		end_pattern <- paste(ends, collapse = "|")
		pattern <- paste0("(?s)", start, "\\s*\\n(.+?)(?=", end_pattern, "|$)")
		match <- str_match(text, regex(pattern, ignore_case = TRUE))
		if (is.na(match[1, 2])) return(NA_character_)
		result <- match[1, 2]
		result <- str_replace_all(result, "(?i)\\n?\\s*Afficher la suite\\s*\\n?", "\n")
		str_trim(result)
	}
	
	
	# List of all possible section titles on the site
	# Used as end markers for extract_section()
	# The regex handle variants with/without accents (é/e, è/e)
	
	section_ends <- c(
		"Profil recherch[ée]",
		"Descriptif du profil",
		"Comp[ée]tences (?:attendues|requises|techniques|cl[ée]s|demand[ée]es)",
		"Comp[ée]tences\\s*\\n",
		"Niveau d'[ée]tudes",
		"Qui sommes[- ]nous",
		"Localisation du poste",
		"Localisation\\s*\\n\\+",
		"[ÉE]l[ée]ments de candidature",
		"Informations compl[ée]mentaires",
		"Fondement juridique",
		"Conditions particuli[èe]res",
		"Liste des pi[èe]ces",
		"Personne [àa] contacter",
		"Renseignements et contact",
		"Management\\s*(Oui|Non)"
	)
	
	
	# clean_text(): post-processing of extracted text
	# - Removes leftover "Afficher la suite" residues from JS
	# - Removes Leaflet/OpenStreetMap map artifacts
	# - Removes residual location blocks
	# - Reduces multiple line breaks (>3) to a single double break
	
	clean_text <- function(txt) {
		if (is.na(txt)) return(NA_character_)
		txt %>%
			str_replace_all("(?i)Afficher la suite", "") %>%
			str_replace_all("\\+\\s*\\n\\s*−\\s*\\n\\s*Leaflet.*?OpenStreetMap", "") %>%
			str_replace_all("Localisation\\s*:\\s*[^\\n]+$", "") %>%
			str_replace_all("\\n{3,}", "\n\n") %>%
			str_trim()
	}
	
	
	# List to store results (one tibble per offer)
	
	results <- list()
	
	
	# Consecutive error counter to detect a Chrome crash
	
	consecutive_errors <- 0
	
	
	## Main loop: scraping each offer individually ----
	
	for (i in seq_along(all_links)) {
		
		offer_url <- all_links[i]
		cat(sprintf("[%d/%d] %s\n", i, length(all_links), offer_url))
		
		tryCatch({
			
			
			# Chrome crash check: test "1+1" via the protocol
			# If the session is dead (crash, timeout), we relaunch it automatically
			
			session_ok <- tryCatch({
				b$Runtime$evaluate("1+1")
				TRUE
			}, error = function(e) FALSE)
			
			if (!session_ok) {
				cat("  🔄 Chrome session dead, relaunching...\n")
				tryCatch(b$close(), error = function(e) NULL)
				Sys.sleep(2)
				b <- ChromoteSession$new()
				Sys.sleep(1)
				cat("✅ New Chrome session opened\n")
			}
			
			b$Page$navigate(offer_url)
			
			
			# Smart wait: DOM polling until <h1> is present
			# Instead of waiting a fixed time (which would be either too short or too long),
			# we check every second if the <h1> exists in the DOM
			# Maximum 10 attempts (10 seconds)
			
			page_ready <- FALSE
			for (attempt in 1:10) {
				Sys.sleep(offer_wait)
				check <- tryCatch(
					b$Runtime$evaluate('document.querySelector("h1") !== null')$result$value,
					error = function(e) FALSE
				)
				if (isTRUE(check)) { page_ready <- TRUE; break }
			}
			
			if (!page_ready) {
				cat("⚠ Page not loaded after 10s, trying anyway\n")
			}
			
			
			# Inject JS to expand all hidden content
			
			b$Runtime$evaluate(js_expand_all)
			Sys.sleep(0.5)   # Short pause to let the DOM update
			
			
			# Retrieve raw text from the page
			# Priority: #main-content > main > article (main content areas)
			
			raw_text <- b$Runtime$evaluate(
				'document.querySelector("#main-content, main, article").innerText'
			)$result$value
			
			
			# Fallback: if JS text is empty, retrieve the rendered HTML
			# and parse it with rvest on the R side
			
			if (is.null(raw_text) || raw_text == "") {
				rendered_html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
				raw_text <- read_html(rendered_html) %>%
					html_element("#main-content, main") %>%
					html_text2()
			}
			
			
			# Retrieve the full rendered HTML for structured extraction
			
			rendered_html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
			offer_html <- read_html(rendered_html)
			
			
			# Title extraction (<h1> tag)
			
			title <- offer_html %>%
				html_element("h1") %>%
				html_text2() %>%
				str_squish()   # Removes multiple spaces and line breaks
			
			
			# Short field extraction via regex on raw text
			# extract_field(): looks for a "Label: Value" pattern and captures the value
			# The regex are case-insensitive and handle accent variants
			
			extract_field <- function(pattern) {
				m <- str_match(raw_text, regex(pattern, ignore_case = TRUE))
				if (is.na(m[1, 2])) return(NA_character_)
				str_squish(m[1, 2])
			}
			
			
			reference          <- extract_field("R[ée]f[.\\s]*R[ée]f[ée]rence\\s*:?\\s*([A-Z0-9][-A-Z0-9]+)")
			employer           <- extract_field("Employeur\\s*:?\\s*(.+?)(?:\\n|$)")
			location           <- extract_field("Localisation\\s*:?\\s*(.+?)(?:\\n|$)")
			domain             <- extract_field("Domaine\\s*:?\\s*(.+?)(?:\\n|$)")
			category           <- extract_field("Cat[ée]gorie\\s+(Cat[ée]gorie\\s+[ABC+()\\s\\w]+?)(?:\\n|$)")
			employment_type    <- extract_field("Nature de l'emploi\\s*(.+?)(?:\\n|$)")
			experience         <- extract_field("Exp[ée]rience souhait[ée]e\\s*(.+?)(?:\\n|$)")
			remote_work        <- extract_field("T[ée]l[ée]travail possible\\s*(Oui|Non)")
			management         <- extract_field("Management\\s*(Oui|Non)")
			public_service     <- extract_field("Fonction publique\\s*:?\\s*(.+?)(?:\\n|$)")
			education_level    <- extract_field("Niveau d'[ée]tudes\\s*(?:min[.]?\\s*(?:requis|souhait[ée]))?\\s*:?\\s*(.+?)(?:\\n|$)")
			posting_date       <- extract_field("En ligne depuis le\\s*(.+?)(?:\\n|$)")
			
			
			
			# Long section extraction (description, profile, skills)
			# We use extract_section() which splits the raw text between section
			# titles. For example: all text between "Vos missions en quelques mots"
			# and the next recognized title (profile, skills, etc.)
			
			description <- extract_section(raw_text, "Vos missions en quelques mots", section_ends)
			profile     <- extract_section(raw_text, "Profil recherch[ée]", section_ends)
			
			
			# Fallback: some offers use "Descriptif du profil" instead of "Profil recherché"
			
			if (is.na(profile)) {
				profile <- extract_section(raw_text, "Descriptif du profil", section_ends)
			}
			skills <- extract_section(raw_text, "Comp[ée]tences (?:attendues|requises|techniques|cl[ée]s|demand[ée]es)?", section_ends)
			
			
			# Post-processing: cleaning residual HTML/JS artifacts
			
			description <- clean_text(description)
			profile     <- clean_text(profile)
			skills      <- clean_text(skills)
			
			
			# Store result in the list
			
			results[[i]] <- tibble(
				title, reference, employer, public_service, location, domain,
				category, employment_type, experience, management, remote_work,
				education_level, posting_date, description, profile, skills,
				url = offer_url
			)
			
			consecutive_errors <- 0
			
		}, error = function(e) {
			
			# On error, store an empty tibble with the URL to not lose it
			
			cat("⚠ Error:", conditionMessage(e), "\n")
			results[[i]] <<- tibble(
				title = NA, reference = NA, employer = NA, public_service = NA,
				location = NA, domain = NA, category = NA, employment_type = NA,
				experience = NA, management = NA, remote_work = NA, education_level = NA,
				posting_date = NA, description = NA, profile = NA, skills = NA,
				url = offer_url
			)
			consecutive_errors <<- consecutive_errors + 1
			
			
			# Resilience mechanism: if 3 errors in a row, Chrome has probably
			# crashed → we close the session and open a new one
			
			if (consecutive_errors >= 3) {
				cat("  🔄 3 consecutive errors, forced Chrome relaunch...\n")
				tryCatch(b$close(), error = function(e) NULL)
				Sys.sleep(2)
				b <<- ChromoteSession$new()
				Sys.sleep(1)
				consecutive_errors <<- 0
				cat("✅ New Chrome session opened\n")
			}
		})
		
		# Intermediate backup every 50 offers
		
		if (i %% 50 == 0) {
			df_temp <- bind_rows(results)
			write_csv2(df_temp, intermediate_backup_path)
			cat(sprintf("  💾 Intermediate backup: %d offers (%d non-empty)\n",
									nrow(df_temp), sum(!is.na(df_temp$title))))
		}
	}
	
	tryCatch(b$close(), error = function(e) NULL)
	
	
	
	
	# STEP 3: Assemble results and merge with existing ----
	
	# We assemble all individual tibbles into a single dataframe
	# In update mode, we merge with the existing file avoiding duplicates
	# (double check: by URL AND by offer reference)
	
	
	cat("\n STEP 3: Assembly \n")
	
	new_df <- bind_rows(results)
	cat("New offers scraped:", nrow(new_df), "\n")
	
	if (run_mode == "update" && file.exists(full_csv_path)) {
		
		# Load existing and merge
		
		existing_df <- read_csv2(full_csv_path, show_col_types = FALSE)
		
		
		# Remove additional_info from old file if it still exists
		
		if ("additional_info" %in% names(existing_df)) {
			existing_df <- existing_df %>% select(-additional_info)
		}
		
		
		# Double safety anti-duplicate:
		# we check that the URL doesn't already exist
		# we also check that the offer reference doesn't already exist
		# (because the same offer can sometimes have a slightly different URL)
		
		unique_new_df <- new_df %>%
			filter(
				!(url %in% existing_df$url) &
					(is.na(reference) | !(reference %in% na.omit(existing_df$reference)))
			)
		
		cat("Unique new offers:", nrow(unique_new_df), "\n")
		
		
		# Stack: new on top, old on bottom
		
		offers_df <- bind_rows(unique_new_df, existing_df)
		cat("Total after merge:", nrow(offers_df), "\n")
		
	} else {
		offers_df <- new_df
	}
	
	
	# Save the full file (ALL offers, before filtering)
	
	write_csv2(offers_df, full_csv_path)
	cat("Full file saved:", full_csv_path, "\n")
	
}



# STEP 4: Filter on technical keywords ----


# The full file contains ALL offers (e.g. secretary, accountant, etc.)
# This step only keeps those that mention relevant technical skills
# (R, SQL, data scientist, etc.) in their title, description,
# profile or skills

# Note: the search is accent-insensitive thanks to stri_trans_general()
# which converts "données" → "donnees", "modélisation" → "modelisation", etc
# This allows matching offers even if they contain typos



cat("\n STEP 4: Keyword filtering \n")


## Function to remove all accents from a string ----

remove_accents <- function(x) stri_trans_general(x, "Latin-ASCII")


# List of technical keywords with flexible regex

# Each pattern is a regex that handles variants (plural, hyphen, space, etc.)

filter_keywords <- c(
	"donnee[s]?",              # données, donnees, donnée, donnee
	"statistique[s]?",         # statistique(s)
	"modelisation[s]?",        # modélisation(s)
	"geomatique",              # géomatique
	"\\bSIG\\b",               # SIG (exact, not "design")
	"\\bR\\b",                 # R (exact, not "r" in a word)
	"langage.?R",
	"shiny",
	"rstudio",
	"data.?scientist[s]?",     # data scientist, data-scientist
	"data.?analyst[s]?",       # data analyst, data-analyst
	"data.?engineer",          # data engineer
	"machine.?learning",       # machine learning, machine-learning
	"deep.?learning",
	"intelligence.?artificielle",
	"\\bIA\\b",                # IA (exact)
	"\\bSQL\\b",
	"\\bSAS\\b",
	"tableau[x]?\\s*de\\s*bord", # tableau(x) de bord
	"visualisation",
	"analyse",
	"open.?data",
	"big.?data",
	"entrepot[s]?\\s*de\\s*donnee", # entrepôt(s) de données
	"base[s]?\\s*de\\s*donnee"      # base(s) de données
)


# Concatenation of all patterns into a single "mega-pattern" with |

filter_pattern <- paste(filter_keywords, collapse = "|")


# Detection function: normalizes text (lowercase + no accent) then tests
# Uses coalesce() to replace NAs with "" and avoid errors

detect <- function(text, pattern) {
	str_detect(
		remove_accents(tolower(coalesce(text, ""))),
		pattern
	)
}


# Apply filter: keep an offer if AT LEAST ONE of the fields
# (description, profile, skills, title) contains a keyword

filtered_df <- offers_df %>%
	filter(
		detect(description, filter_pattern) |
			detect(profile, filter_pattern) |
			detect(skills, filter_pattern) |
			detect(title, filter_pattern)
	)

cat("Offers after technical keyword filter:", nrow(filtered_df), "\n")




## Identify matched keywords for each offer ----

# For each filtered offer, we concatenate all text fields, then test
# each keyword individually to know WHICH ones matched
# The result is stored in a new column "matched_keywords"

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



## In update mode: smart merge with existing filtered file ----


# We do NOT replace the existing filtered file entirely, because the user
# may have manually deleted irrelevant offers. We only add
# NEW filtered offers on top of the existing file

if (run_mode == "update" && file.exists(filtered_csv_path)) {
	existing_filtered_df <- read_csv2(filtered_csv_path, show_col_types = FALSE)
	
	
	# Remove additional_info from old file if it still exists
	
	if ("additional_info" %in% names(existing_filtered_df)) {
		existing_filtered_df <- existing_filtered_df %>% select(-additional_info)
	}
	
	
	# Keep only offers not already in the filtered file
	
	new_filtered_df <- filtered_df %>%
		filter(
			!(url %in% existing_filtered_df$url) &
				(is.na(reference) | !(reference %in% na.omit(existing_filtered_df$reference)))
		)
	
	cat("New filtered offers to add:", nrow(new_filtered_df), "\n")
	
	
	# Stack: new on top, old on bottom
	
	filtered_df <- bind_rows(new_filtered_df, existing_filtered_df)
	cat("Total filtered offers (old + new):", nrow(filtered_df), "\n")
}



## Reorder columns: matched_keywords in 2nd position ----

filtered_df <- filtered_df %>%
	select(title, matched_keywords, everything())

write_csv2(filtered_df, filtered_csv_path)
cat("Filtered file saved:", filtered_csv_path, "\n")




# BONUS: Summary of filtered offers

if (nrow(filtered_df) > 0) {
	cat("\n Filtered offers summary \n\n")
	for (j in 1:nrow(filtered_df)) {
		cat(sprintf("--- Offer %d ---\n", j))
		cat("Title       :", filtered_df$title[j], "\n")
		cat("Employer    :", filtered_df$employer[j], "\n")
		cat("Location    :", filtered_df$location[j], "\n")
		cat("Category    :", filtered_df$category[j], "\n")
		cat("Remote work :", filtered_df$remote_work[j], "\n")
		cat("Posted      :", filtered_df$posting_date[j], "\n")
		cat("Keywords    :", filtered_df$matched_keywords[j], "\n")
		cat("URL         :", filtered_df$url[j], "\n\n")
	}
}

cat("\n Done! \n")
duration <- round(difftime(Sys.time(), script_start, units = "mins"), 1)
cat("Total duration:", duration, "minutes\n")
cat("Mode          :", run_mode, "\n")
cat("Total offers  :", nrow(offers_df), "\n")
cat("Filtered      :", nrow(filtered_df), "\n")