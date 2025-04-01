library(S7)
# pkgload::load_all()
library(ellmer)

source("tool-weather.R")
source("tool-interactive.R")

client <- chat_openai(model = "gpt-4o-mini")

client$register_tool(
  tool(
    function(tz = "") {
      if (runif(1) < 0.25) {
        stop("The quartz crystals weren't aligned. Try your request again.")
      }
      Sys.sleep(runif(1, 0, 5))
      strftime(Sys.time(), "%F %T", tz = tz)
    },
    "Get the current time and date.",
    tz = type_string(
      paste(
        "The timezone in which to display the current time and date.",
        "Must be a name in the Olson database of known locations,",
        "or pass an empty string to get the time in the user's time zone."
      ),
      required = FALSE
    ),
    .name = "current_time"
  )
)

client$register_tool(
  tool(
    get_current_weather,
    .description = paste(
      "Get the current weather for a specific location.",
      "The tool result will be shown to the user in an infographic.",
      "DO NOT DESCRIBE THE RESULT for the user, just remember it for future questions."
    ),
    lat = type_string("Latitude"),
    lng = type_string("Longitude"),
    name = type_string("Location name", required = FALSE)
  )
)

client$register_tool(
  tool(
    ask_user_for_name,
    .description = "Ask the user for their name."
  )
)

# pkgload::load_all("~/work/posit-dev/btw")
# btw_client <- btw_client(client = chat_openai(model = "gpt-4o-mini"))

live_browser(client)
