# weather tool example

# The `get_current_weather` tool is going to return a custom `ContentToolResult`
# object with it's own custom display via `contents_shinychat`.
WeatherToolResult <- S7::new_class(
  "WeatherToolResult",
  parent = ellmer::ContentToolResult
)

contents_shinychat <- S7::new_external_generic(
  "shinychat",
  "contents_shinychat",
  "content"
)

S7::method(contents_shinychat, WeatherToolResult) <- function(content, ...) {
  card_weather(content@value, location_name = content@call_args$name)
}


#' Get and display current weather for a location
#'
#' @param lat Latitude as a string
#' @param lng Longitude as a string
#' @return A bslib card displaying the current weather
#'
get_current_weather <- function(lat, lng, name = "") {
  response <-
    httr2::request("https://api.open-meteo.com/v1/forecast") |>
    httr2::req_url_query(
      latitude = lat,
      longitude = lng,
      current = "temperature_2m,weather_code,is_day",
      daily = "temperature_2m_max,temperature_2m_min",
      temperature_unit = "fahrenheit",
      timezone = "auto"
    ) |>
    httr2::req_perform()

  httr2::resp_check_status(response)

  weather_data <- httr2::resp_body_json(response)

  # Extract the relevant weather information
  conditions <- list(
    temp = as.numeric(weather_data$current$temperature_2m),
    temp_max = as.numeric(weather_data$daily$temperature_2m_max[1]),
    temp_min = as.numeric(weather_data$daily$temperature_2m_min[1]),
    # Get weather condition description and icon based on weather code
    info = get_weather_info(
      weather_data$current$weather_code,
      weather_data$current$is_day
    ),
    location = list(lat = lat, lng = lng)
  )

  WeatherToolResult(
    value = conditions,
    detail = list(response = weather_data)
  )
}

card_weather <- function(conditions, location_name = "") {
  title <- "Current Weather"
  if (!is.null(location_name) && nzchar(location_name)) {
    title <- paste(title, "in", location_name)
  }

  bslib::card(
    class = "weather-card shadow",
    style = "max-width: 500px; margin-inline: auto;",

    # Card header with title and weather icon
    bslib::card_header(
      class = "bg-primary text-white d-flex align-items-center",
      title
    ),

    # Conditions icon and temp
    bslib::card_body(
      class = "text-center",
      bslib::layout_columns(
        col_widths = bslib::breakpoints(xs = c(6, 6)),
        max_height = 100,
        shiny::div(
          class = "d-flex justify-content-center align-items-center",
          bsicons::bs_icon(
            conditions$info$icon,
            size = "auto",
            style = "max-width: 100px; max-height: 100px;"
          )
        ),
        shiny::div(
          class = "d-flex flex-column justify-content-center align-items-center h-100",
          shiny::div(
            class = "display-4 mb-0",
            paste0(round(conditions$temp), "°F")
          ),
          shiny::p(
            class = "lead text-muted",
            conditions$info$description
          )
        )
      ),

      # High and low temps ----
      shiny::div(
        class = "d-flex justify-content-around",
        shiny::div(
          class = "text-center",
          bsicons::bs_icon("arrow-down", class = "text-info"),
          shiny::span(class = "ms-1", paste0(round(conditions$temp_min), "°F")),
          shiny::p(class = "small text-muted mb-0", "Low")
        ),
        shiny::div(
          class = "text-center",
          bsicons::bs_icon("arrow-up", class = "text-success"),
          shiny::span(class = "ms-1", paste0(round(conditions$temp_max), "°F")),
          shiny::p(class = "small text-muted mb-0", "High")
        ),
      )
    ),

    # Footer with location ----
    bslib::card_footer(
      class = "text-muted text-center",
      paste("Location:", conditions$location$lat, ", ", conditions$location$lng)
    )
  )
}

#' Helper function to get weather description and icon based on weather code
#'
#' @param code Weather code from OpenMeteo API
#' @param is_day Boolean indicating if it's daytime
#' @return List with description and icon
get_weather_info <- function(code, is_day) {
  # Weather code mapping based on OpenMeteo documentation
  # https://open-meteo.com/en/docs
  # fmt: skip
  codes <- dplyr::tribble(
    ~code, ~description, ~day_icon, ~night_icon,
    "0", "Clear sky", "sun", "moon-stars",
    "1", "Mainly clear", "sun", "moon-stars",
    "2", "Partly cloudy", "cloud-sun", "cloud-moon",
    "3", "Overcast", "clouds", "clouds",
    "45", "Fog", "cloud-fog", "cloud-fog",
    "48", "Depositing rime fog", "cloud-fog", "cloud-fog",
    "51", "Light drizzle", "cloud-drizzle", "cloud-drizzle",
    "53", "Moderate drizzle", "cloud-drizzle", "cloud-drizzle",
    "55", "Dense drizzle", "cloud-drizzle", "cloud-drizzle",
    "56", "Light freezing drizzle", "cloud-sleet", "cloud-sleet",
    "57", "Dense freezing drizzle", "cloud-sleet", "cloud-sleet",
    "61", "Slight rain", "cloud-rain", "cloud-rain",
    "63", "Moderate rain", "cloud-rain", "cloud-rain",
    "65", "Heavy rain", "cloud-rain-heavy", "cloud-rain-heavy",
    "66", "Light freezing rain", "cloud-sleet", "cloud-sleet",
    "67", "Heavy freezing rain", "cloud-sleet", "cloud-sleet",
    "71", "Slight snow fall", "cloud-snow", "cloud-snow",
    "73", "Moderate snow fall", "cloud-snow", "cloud-snow",
    "75", "Heavy snow fall", "cloud-snow", "cloud-snow",
    "77", "Snow grains", "cloud-snow", "cloud-snow",
    "80", "Slight rain showers", "cloud-rain", "cloud-rain",
    "81", "Moderate rain showers", "cloud-rain", "cloud-rain",
    "82", "Violent rain showers", "cloud-rain-heavy", "cloud-rain-heavy",
    "85", "Slight snow showers", "cloud-snow", "cloud-snow",
    "86", "Heavy snow showers", "cloud-snow", "cloud-snow",
    "95", "Thunderstorm", "cloud-lightning", "cloud-lightning",
    "96", "Thunderstorm with slight hail", "cloud-lightning-rain", "cloud-lightning-rain",
    "99", "Thunderstorm with heavy hail", "cloud-lightning-rain", "cloud-lightning-rain"
  )

  code <- as.character(code)

  description <- "Unknown weather condition"
  icon_name <- "question-circle"

  if (code %in% codes$code) {
    description <- codes$description[codes$code == code]
    icon_var <- if (is_day) "day_icon" else "night_icon"
    icon_name <- codes[[icon_var]][codes$code == code]
  }

  list(
    description = description,
    icon = icon_name
  )
}
