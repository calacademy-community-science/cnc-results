#'############################################################################
#'
#'





# Libraries
library(tidyverse)
library(jsonlite)
library(httr)



# function to get total number of observations
## this is throttled with a delay of 2 seconds
## will retry request with an additional delay if it fails
get_inat_observation_number <- purrr:::slowly(
  function(URL){
    ping <- paste0(URL, "&page=1&per_page=1")
    max_results <- fromJSON(content(RETRY(verb = "GET",url = ping), as = "text"))[[1]]
    return(max_results)
  }, rate = rate_delay(2))

# Function for getting inat data 
## this is throttled with a delay of 1 second
get_inat_data <- purrr:::slowly(function(x) {
  fromJSON(content(GET(x), as = "text"))$results 
}, rate = rate_delay(1))


# Function to parse project name from iNat links in results sheet
get_project_name <- function(link){
  link_sections <- (link %>% str_split(pattern = "/projects/"))[[1]] 
  if(length(link_sections) == 2){
    name <- link_sections[2]
  } else {name <- NA}
  return(name)
}

# Function to parse cut-off datetime from current date notation in results sheet
generate_cutoff_date <- function(dt){
  day <-(str_squish(dt) %>%
           str_split(pattern = " "))[[1]][2]
  paste0("2023-05-",
         str_pad(day,width=2, pad = 0))
}


#############################################################################

# basic inat api query urls for number of observations, species, and observers ("people")
## CNC doesn't need to track number of identifiers
obs_query <- "https://api.inaturalist.org/v1/observations?project_id="
species_query <- "https://api.inaturalist.org/v1/observations/species_counts?project_id="
people_query <- "https://api.inaturalist.org/v1/observations/observers?project_id="



#' test code to filter data for the desired cut-off datetime
#' 
#'  relevant iNat API parameters:
#'  - created_d2	(created at or before datetime)
#'  - updated_since updated since datetime)
#'  
#'  right now I am running 2 requests:
#'  1. all records created before the cut-off
#'  2. all records created before the cut-off and modified after the cut-off
#'  
#'  and subtracting #2 from #1 to get records created+updated before cut-off
#'  
#' if we are running the requests within a day of the cut-off times, expect #2
#' to be small, not sure if it's significant enough for CNC reporting




# test this with 2023 CNC projects

# import 2023 CNC results doc and parse project name, create cut-off datetime
cnc_sheet <- read_csv("data/Reference Copy of CNC 2023 RESULTS - RESULTS MASTER.csv") %>%
  filter(!is.na(`Name of your project`)) %>%
  transmute(cnc_end_date = purrr::map(`CNC End Date (PDT)`, generate_cutoff_date),
            cnc_end_time = `CNC End Time (PDT)`,
            cnc_cutoff = paste0(cnc_end_date,"T",
                                hour(cnc_end_time) %>% str_pad(width=2, pad = 0),":",
                                minute(cnc_end_time) %>% str_pad(width=2, pad = 0),
                                ":00-07:00"),
            city_name = `City (or Cities)`,
            project_name = `Name of your project`,
            project_link = `Results Link`,
            project_id = purrr::map_chr(project_link, get_project_name)) %>%
  filter(!is.na(project_id))





# function to get cnc stats (just totals for now, not split into verifiable/research-grade)
## this makes 6 requests per city. 473 cities x 6 = 2838 

get_cnc_stats <- function(id){
  cutoff <- cnc_sheet %>% filter(project_id == id) %>% pull(cnc_cutoff)
  obs <- get_inat_observation_number(paste0(obs_query,id,"&created_d2=",cutoff))
  species <- get_inat_observation_number(paste0(species_query,id,"&created_d2=",cutoff))
  people <- get_inat_observation_number(paste0(people_query,id,"&created_d2=",cutoff))
  obs_updated <- get_inat_observation_number(paste0(obs_query,id,"&created_d2=",cutoff,"&updated_since=", cutoff))
  species_updated <- get_inat_observation_number(paste0(species_query,id,"&created_d2=",cutoff,"&updated_since=", cutoff))
  people_updated <- get_inat_observation_number(paste0(people_query,id,"&created_d2=",cutoff,"&updated_since=", cutoff))
  
  tibble(project_id = id,
         cnc_cutoff = cutoff,
         observations = obs,
         species = species,
         people = people,
         mod_observations = obs_updated,
         mod_species = species_updated,
         mod_people = people_updated)

}


# create safely wrapper for the get_cnc_stats function
safely_get_cnc_stats <- purrr::safely(.f = get_cnc_stats)


# test
filtered_project_results_test <- purrr::map(cnc_sheet$project_id[1:30], safely_get_cnc_stats)


###############################################################################

