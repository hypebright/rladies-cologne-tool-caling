library(shiny) # at least version 1.8.1
library(bslib) # at least version 0.7.0
library(ellmer) # at least version 0.4.0
library(httr2)
library(dplyr)

data_model <- type_object(
  city = type_string(
    description = "The city for which the weather was checked."
  ),
  baby_age_months = type_integer(
    description = "The age of the baby in months."
  ),
  temperature_c = type_number(description = "Current temperature in Celsius."),
  condition = type_string(
    description = "Current weather condition (e.g., sunny, rainy, cloudy, snowy, windy)."
  ),
  activities = type_array(
    description = "List of 10 suggested activities for the baby based on the weather and age.",
    type_object(
      name = type_string(description = "Short title for the activity."),
      fun_score = type_integer(
        description = "Score from 1–10 indicating how fun the activity is likely to be."
      ),
      duration_minutes = type_integer(
        description = "Estimated activity length in minutes."
      ),
      description = type_string(
        description = "One-sentence description of the activity."
      )
    )
  )
)

#' Get the current weather for a given city using Open-Meteo API
#'
#' @param city The name of the city
#' @return A tibble with city, temperature in Celsius, windspeed, and weather code
get_current_weather <- function(city) {
  # Geocoding API to get latitude/longitude from city name
  geo_resp <- request("https://geocoding-api.open-meteo.com/v1/search") %>%
    req_url_query(name = city, count = 1) %>%
    req_perform() %>%
    resp_body_json()

  if (is.null(geo_resp$results)) {
    stop("City not found: ", city)
  }

  lat <- geo_resp$results[[1]]$latitude
  lon <- geo_resp$results[[1]]$longitude

  # Weather API for current weather at that location
  weather_resp <- request("https://api.open-meteo.com/v1/forecast") %>%
    req_url_query(latitude = lat, longitude = lon, current_weather = "true") %>%
    req_perform() %>%
    resp_body_json()

  current <- weather_resp$current_weather

  return(
    tibble(
      city = city,
      temperature_c = current$temperature,
      windspeed = current$windspeed,
      weather_code = current$weathercode
    )
  )
}

get_current_weather <- tool(
  get_current_weather,
  "Returns the current weather for a given city",
  city = type_string(
    "The name of the city for which to get the weather",
    required = TRUE
  )
)

full_system_prompt <- interpolate_file(
  path = here::here("examples", "baby-activities-system-prompt.md"),
  city = "Rotterdam",
  age = 12
)

chat <- chat("claude/claude-sonnet-4-5", system_prompt = full_system_prompt)

# Register the tool with the chat
chat$register_tool(get_current_weather)

# Start conversation with the chat
# Task 1: regular chat to extract meta-data
chat_res <- chat$chat(
  "Execute Task 1 (get weather)"
)

# Execute next task
# Task 2: structured chat to further analyse the slides
chat$chat_structured(
  "Execute Task 2 (activity suggestions)",
  type = data_model
)
