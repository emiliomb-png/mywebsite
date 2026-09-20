## ============================================================
## Cost of Living vs. Population: 100 Largest World Cities
## Source 1: Wikipedia "List of largest cities" (city population)
## Source 2: Numbeo cost-of-living rankings
## ============================================================
## KNOWN DATA LIMITATION (read before drawing conclusions):
## Numbeo's coverage is crowdsourced and skews toward wealthier, more-
## traveled cities. Several of the world's most populous cities
## (particularly in South Asia and Sub-Saharan Africa) may have thin
## data or be missing from their rankings table entirely. This script
## reports the match rate explicitly so that gap is visible rather than
## silently dropped -- and it's arguably an interesting finding in its
## own right (which large cities lack cost-of-living coverage at all).


install.packages("stringdist")

library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(tidyr)

## ---- 1. Scrape the 100 largest cities by population (Wikipedia) ------
## NOTE ON DATA VINTAGE: this table conveniently includes an explicit
## per-city "Year" column, so unlike a table with one implied reference
## date, the vintage of each city's figure is captured directly rather
## than assumed to be uniform.

get_largest_cities <- function(n = 100) {
  url <- "https://en.wikipedia.org/wiki/List_of_cities_with_over_one_million_inhabitants"
  page <- read_html(url)
  
  tables <- html_table(page, fill = TRUE)
  
  # This table has a clean, single-row header: City, Country, Inhabitants,
  # Continent, Year, City definition -- find it by requiring both City
  # and an Inhabitants-type column, rather than hardcoding a table index.
  candidate <- tables %>%
    keep(~ all(c("City", "Country") %in% names(.x))) %>%
    keep(~ any(str_detect(names(.x), regex("Inhabitant|Population", ignore_case = TRUE)))) %>%
    pluck(1)
  
  if (is.null(candidate)) {
    stop("Couldn't find the cities table -- inspect html_table(read_html(url)) manually.")
  }
  
  pop_col <- names(candidate)[str_detect(names(candidate), regex("Inhabitant|Population", ignore_case = TRUE))][1]
  year_col <- names(candidate)[str_detect(names(candidate), regex("^Year$", ignore_case = TRUE))][1]
  
  candidate %>%
    transmute(
      city    = City,
      country = Country,
      population = .data[[pop_col]] %>%
        str_remove_all(",") %>%
        str_extract("\\d+") %>%
        as.numeric(),
      population_year = if (!is.na(year_col)) .data[[year_col]] else NA
    ) %>%
    filter(!is.na(population)) %>%
    arrange(desc(population)) %>%
    slice_head(n = n) %>%
    mutate(pop_rank = row_number())
}

## ---- 2. Scrape Numbeo's cost-of-living rankings table -----------------
## NOTE ON DATA VINTAGE: Numbeo publishes two releases per year -- an
## annual one (title=YYYY) and a mid-year one (title=YYYY-mid). Which
## one is "current" depends on when you run this. RANKINGS_YEAR below
## defaults to the current calendar year's annual release; if the
## request comes back empty or errors, try the "-mid" variant instead
## (e.g. "2026-mid") -- that likely means the annual release isn't out
## yet and only the mid-year snapshot exists.

RANKINGS_YEAR <- format(Sys.Date(), "%Y")  # e.g. "2026" -- change to "2026-mid" if needed

get_col_rankings <- function(year = RANKINGS_YEAR) {
  url <- sprintf("https://www.numbeo.com/cost-of-living/rankings.jsp?title=%s", year)
  page <- read_html(url)
  
  tables <- html_table(page, fill = TRUE)
  
  # Find the table containing the expected index columns
  candidate <- tables %>%
    keep(~ any(str_detect(names(.x), "Cost of Living Index"))) %>%
    pluck(1)
  
  if (is.null(candidate)) {
    stop("Couldn't find the rankings table -- inspect html_table(page) manually, ",
         "or try a different `year` (e.g. '2025-mid').")
  }
  
  city_col <- names(candidate)[str_detect(names(candidate), regex("City", ignore_case = TRUE))][1]
  
  candidate %>%
    rename(location = all_of(city_col)) %>%
    mutate(
      # Numbeo formats location like "New York, NY, United States" or
      # "Zurich, Switzerland" -- take everything before the first comma
      # as the city name, and everything after the last comma as the
      # country (needed later for country-scoped fuzzy matching).
      city_clean    = str_trim(str_extract(location, "^[^,]+")),
      country_clean = str_trim(str_extract(location, "[^,]+$")),
      col_index        = as.numeric(`Cost of Living Index`),
      rent_index       = as.numeric(`Rent Index`),
      groceries_index  = as.numeric(`Groceries Index`),
      restaurant_index = as.numeric(`Restaurant Price Index`),
      purchasing_power = as.numeric(`Local Purchasing Power Index`)
    ) %>%
    select(location, city_clean, country_clean, col_index, rent_index,
           groceries_index, restaurant_index, purchasing_power)
}

## ---- 3. Run the scrape (two requests total) ---------------------------
## We pull more candidate cities than we ultimately need (N_CANDIDATES),
## since not every large city will have Numbeo data. We then keep the
## N_TARGET highest-population cities that DO match, backfilling from
## further down the population list as needed.
## NOTE: this changes what "top 100" means -- see the comment at
## section 4 below.

N_CANDIDATES <- 250
N_TARGET     <- 100

cities <- get_largest_cities(n = N_CANDIDATES)
Sys.sleep(2)  # brief, polite pause between the two requests
col_data <- get_col_rankings()

## ---- 4. Match the two datasets by city name ---------------------------
## IMPORTANT FRAMING NOTE FOR YOUR WRITE-UP: because we widen the pool
## and keep the top N_TARGET *matched* cities, the final sample is best
## described as "the 100 largest cities for which Numbeo has cost-of-
## living data" rather than a strict top-100-by-population comparison.
## This also does NOT fix Numbeo's underlying coverage skew (toward
## wealthier, more-documented cities) flagged earlier -- it just
## backfills with the next-largest cities that happen to have data,
## which are likely skewed the same way.
##
## Matching happens in two passes:
##   1. Exact match on cleaned city name (fast, no false positives)
##   2. Fuzzy match (stringdist) on whatever's left, restricted to the
##      same country so we don't confuse e.g. one country's Springfield
##      with another's -- this catches spelling/accent/punctuation
##      differences like "Sao Paulo" vs "São Paulo" without needing a
##      hand-built alias for every case.

library(stringdist)  # install.packages("stringdist") if needed

alias_map <- c(
  "Ho Chi Minh City" = "Ho Chi Minh City",
  "New York City"    = "New York",
  "Washington, D.C."  = "Washington",
  NULL
)

cities <- cities %>%
  mutate(city_for_match = recode(city, !!!alias_map, .default = city))

# Pass 1: exact match
exact_matched <- cities %>%
  inner_join(col_data, by = c("city_for_match" = "city_clean"))

still_unmatched <- cities %>%
  filter(!city_for_match %in% exact_matched$city_for_match)

# Pass 2: fuzzy match remaining cities, restricted to same country
fuzzy_match_one <- function(city_name, country_name, col_data, max_dist = 2) {
  candidates <- col_data %>%
    filter(str_to_lower(country_clean) == str_to_lower(country_name))
  
  if (nrow(candidates) == 0) return(NA_integer_)
  
  dists <- stringdist(str_to_lower(city_name), str_to_lower(candidates$city_clean), method = "osa")
  best <- which.min(dists)
  
  if (length(best) == 0 || dists[best] > max_dist) return(NA_integer_)
  which(col_data$city_clean == candidates$city_clean[best] &
          col_data$country_clean == candidates$country_clean[best])[1]
}

fuzzy_rows <- map_int(seq_len(nrow(still_unmatched)), function(i) {
  idx <- fuzzy_match_one(still_unmatched$city_for_match[i],
                         still_unmatched$country[i], col_data)
  if (is.na(idx)) NA_integer_ else idx
})

fuzzy_matched <- still_unmatched %>%
  mutate(.match_idx = fuzzy_rows) %>%
  filter(!is.na(.match_idx)) %>%
  bind_cols(col_data[.$.match_idx, ] %>% select(-city_clean, -country_clean)) %>%
  select(-.match_idx)

# Combine both passes, then keep the top N_TARGET by population among matches
merged_all <- bind_rows(exact_matched, fuzzy_matched) %>%
  distinct(city, country, .keep_all = TRUE) %>%
  arrange(pop_rank)

merged <- merged_all %>% slice_head(n = N_TARGET)

deepest_rank <- max(merged$pop_rank)
message(sprintf(
  "Found %d matched cities (target: %d). Had to reach down to population rank #%d to fill the sample.",
  nrow(merged), N_TARGET, deepest_rank
))

still_missing <- cities %>%
  filter(pop_rank <= deepest_rank, !city %in% merged$city) %>%
  select(pop_rank, city, country)
message(sprintf("%d cities within that range still have no Numbeo match:", nrow(still_missing)))
print(still_missing)

## ---- 5. Save raw + merged data -----------------------------------------

write_csv(cities, "largest_cities_population.csv")
write_csv(col_data, "numbeo_col_rankings.csv")
write_csv(merged, "cities_col_merged.csv")

## ---- 6. Analysis --------------------------------------------------------

matched <- merged  # already filtered to matched rows by the pipeline above

# Does population size relate to cost of living at all?
cor.test(matched$population, matched$col_index)

# "Best value" large cities: low cost of living relative to local
# purchasing power (higher ratio = your money goes further there)
matched <- matched %>%
  mutate(value_score = purchasing_power / col_index) %>%
  arrange(desc(value_score))

# Top 10 best-value large cities
matched %>%
  select(city, country, population, col_index, purchasing_power, value_score) %>%
  slice_head(n = 10)

# Bottom 10 (worst value large cities)
matched %>%
  select(city, country, population, col_index, purchasing_power, value_score) %>%
  slice_tail(n = 10)

## ---- 7. Visualize --------------------------------------------------------

library(ggplot2)

# Graph 1: Population vs. Cost of Living Index across the matched cities
ggplot(matched, aes(x = population, y = col_index)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10(labels = scales::comma) +
  labs(title = "Population vs. Cost of Living Index",
       x = "Population (log scale)", y = "Cost of Living Index (NYC = 100)")

# Graph 2: Top 10 best-value cities (highest purchasing power relative
# to cost of living -- your money goes furthest here)
matched %>%
  slice_max(value_score, n = 10) %>%
  ggplot(aes(x = reorder(city, value_score), y = value_score)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Best-Value Large Cities",
       subtitle = "Local Purchasing Power Index / Cost of Living Index",
       x = NULL, y = "Value Score")

# Graph 3: Top 10 worst-value cities (lowest purchasing power relative
# to cost of living -- your money goes least far here)
matched %>%
  slice_min(value_score, n = 10) %>%
  ggplot(aes(x = reorder(city, value_score), y = value_score)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(title = "Top 10 Worst-Value Large Cities",
       subtitle = "Local Purchasing Power Index / Cost of Living Index",
       x = NULL, y = "Value Score")
