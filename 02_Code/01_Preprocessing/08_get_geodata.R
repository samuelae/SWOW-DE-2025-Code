# Dependencies -----------------------------------------------------------------

library(tidyverse)
library(httr2)

# latest version of full participant data
data <- read_csv("01_Data/Raw/07_data.csv")

# Prepare geodata retrieval ----------------------------------------------------

# Make sure all lat and lon values are rounded (should be already) and select 
# unique pairs
geo_data <- data %>% 
  mutate(latitude = round(latitude, digits = 2),
         longitude = round(longitude, digits = 2)) %>% 
  select(latitude, longitude) %>% 
  distinct()

# Remove any coordinates that are likely nonsense
geo_data <- geo_data %>% 
  filter(!(latitude == 0 & longitude == 0)) %>% 
  na.omit()


# Set up the API call function -------------------------------------------------

reverse_geocode <- function(lat, lon) {
  
  Sys.sleep(1.1)  # API has 1 request per second rate limit
  lat <- as.character(lat) # strings needed
  lon <- as.character(lon) # strings needed
  
  resp <- request("https://nominatim.openstreetmap.org/reverse") %>% 
    req_url_query(`accept-language` = "en-US", lat = lat, lon = lon, 
                  format = "json", zoom = 13) %>% 
    req_user_agent("Small World of Words (samuel.aeschbach@unibas.ch)") %>% 
    req_perform() %>% 
    resp_body_json()
  
  addr <- resp$address
  
  # hierarchy for locality (urban/rural)
  locality <- addr$city %||% 
    addr$town %||% 
    addr$village %||% 
    addr$municipality %||% 
    addr$hamlet %||% 
    NA
  
  # region level
  region <- addr$state %||% 
    addr$county %||% 
    addr$province %||% 
    addr$city %||% # needed for places like Berlin, where "Bundesland"==city
    NA
  
  country <- addr$country %||% NA
  
  tibble(locality, region, country)
  
}

# Run on the full data ---------------------------------------------------------

# chunk into sets of 100 rows
chunks <- geo_data %>% 
  mutate(chunk = ((row_number() - 1) %/% 100) + 1) |>
  group_split(chunk)

# run each chunk through API and save to disk after each
walk(seq_along(chunks), \(i) {
  result <- chunks[[i]] %>% 
    select(-chunk) %>% 
    mutate(geo = map2(latitude, longitude, reverse_geocode, .progress = TRUE)) %>% 
    unnest(geo)
  
  write_csv(result, sprintf("01_Data/Varia/Geocode/Partial/geocode_chunk_%02d.csv", i))
  cli::cli_alert_success("Chunk {i}/{length(chunks)} done")
})

# combine chunks into one lookup tibble
geo_data_lookup <- list.files(path = "01_Data/Varia/Geocode/Partial/", full.names = TRUE) %>% 
  map(read_csv) %>% 
  bind_rows()
write_csv(geo_data_lookup, "01_Data/Varia/Geocode/geo_data_lookup.csv")


