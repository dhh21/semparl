library(geofi)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

d1 <- get_municipalities(year = 2020) 

# Combined population data from Statistics Finland
population_data <- read.csv("statfi_population.csv", stringsAsFactors = FALSE) %>% 
  rename(city = Kunta, 
         year = Vuosi, 
         population = Kuntien.tunnusluvut) %>%
  mutate(year = as.numeric(year))



city_list <- read.csv("kaupunkilista_siistitty.csv", stringsAsFactors = FALSE)



process_data <- function(mentions_file_path, population_data, city_list, merger_cities, geo_info = d1){
  
  mentions <- read.csv(mentions_file_path, stringsAsFactors=FALSE) %>%
    complete(city, year, fill = list(mention_count = 0))
  
  # ## Cities, which have merged as of 2020
  # merger_cities <- city_list %>%
  #   filter(!(kaupunki %in% population_data$city)) %>%
  #   pull(kaupunki)
  # 
  # cat("Following cities have probably merged:\n", merger_cities, "\n")
  
  # # Cities with 0 population
  # zero_pop_cities <- population_data %>% 
  #   filter(population == 0) %>%
  #   pull(city) %>%
  #   unique()
  # 
  # cat("Following cities had 0 population in some years:\n", zero_pop_cities,"\n")
  
  # Extract map shape files from Statistics Finland
  map_data <- geo_info %>% 
    select(name_fi, geom) %>%
    rename(city = name_fi)
  
  # Filter based on the list of 107 cities in Finland (n.b. city != municipality)
  city_map_data <- map_data %>%
    filter(city %in% city_list$kaupunki)
  
  # join shapefiles with mentions
  map_values <- city_map_data %>% 
    left_join(mentions, by = "city") %>%
    left_join(population_data, by = c("city", "year")) %>%
    filter(!is.na(population),
           !(city %in% merger_cities)) %>%
    mutate(per_capita = ifelse(population > 0, mention_count/population, NA))
  
  # # add dummy rows, i.e. 0 for missing rows
  # dummy_cities <- city_map_data %>%
  #   filter(city %in% city_list$kaupunki,
  #          !(city %in% merger_cities)) %>%
  #   group_by(city) %>%
  #   summarise(n = n()) %>%
  #   filter(n<10) %>%
  #   pull(city)
  # 
  # time_period <- min(mentions$year, na.rm = TRUE):max(mentions$year, na.rm = TRUE)
  # 
  # dummy_data <- lapply(dummy_cities, function(x, years = time_period){
  #   dummy_df <- data.frame(city = rep(x, length(years)),
  #                          year = years,
  #                          mention_count_dummy = rep(0, length(years))
  #   )
  #   
  #   geom_df <- data.frame(city = x, geom = map_data[map_data$city == x, "geom"])
  #   
  #   dummy_df <- dummy_df %>% 
  #     left_join(geom_df, by = "city")
  #   
  #   return(dummy_df)
  # }) %>%  do.call("rbind", .)
  # 
  
  # map_values <- dummy_data %>% 
  #   left_join(map_values, by = c("city", "year", "geom")) %>%
  #   mutate(mention_count = ifelse(is.na(mention_count), mention_count_dummy, mention_count)) %>%
  #   select(-mention_count_dummy) %>%
  #   left_join(population_data, by = c("city", "year")) %>%
  #   filter(!is.na(population),
  #          !(city %in% merger_cities)) %>%
  #   mutate(per_capita = ifelse(population > 0, mention_count/population, NA))
  
  return(map_values)
}


city_count_files <- list.files("data/", pattern = "city_counts_statistics_\\d+", full.names = TRUE)


excluded_cities <- c("Raasepori", "Sastamala", "Akaa", "Mänttä-Vilppula")


city_df_list <- lapply(city_count_files, process_data, 
                       population_data = population_data, 
                       city_list = city_list, 
                       merger_cities = excluded_cities)

names(city_df_list) <- c("2004-2013", "1986-1995")

map_coordinates <- city_df_list %>%
  bind_rows(.id = "period")

saveRDS(map_coordinates, "citiesinparl/map_coordinates.rds")

#not cities
not_city <- d1 %>%
  filter(!(name_fi %in% city_list$kaupunki) | name_fi %in% excluded_cities) %>%
  rename(city = name_fi) %>%
  select(city, geom)

saveRDS(not_city, "citiesinparl/not_city.rds")


# parties

process_parties <- function(mentions_file_path, geo_info = d1){
  mentions <- read.csv(mentions_file_path, stringsAsFactors=FALSE) %>%
    complete(city, year, party, fill = list(mention_count = NA))
  
  
  # Extract map shape files from Statistics Finland
  map_data <- geo_info %>% 
    select(name_fi, geom) %>%
    rename(city = name_fi)
  
  map_values <- map_data %>% 
    left_join(mentions, by = "city")
  
  return(as.data.frame(map_values))
  
}

party_count_files <- list.files("data/", pattern = "city_counts_statistics_by_parties_\\d+", full.names = TRUE)

party_df_list <- lapply(party_count_files, process_parties)

names(party_df_list) <- c("2004-2013", "1986-1995")

party_map <- party_df_list %>% bind_rows(.id = "period")

saveRDS(party_map, "citiesinparl/party_map.rds")
