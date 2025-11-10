#Loading libraries
library(tidyverse)
library(jsonlite)
library(rvest)
library(DatawRappr)

#Loading API key, chart keys
api_key <- Sys.getenv("API_KEY")
origin_chart <- Sys.getenv("ORIGIN_KEY")
destination_chart <- Sys.getenv('DESTINATION_CHART')
airline_chart <- Sys.getenv('AIRLINE_CHART')

datawrapper_auth(api_key =  api_key, overwrite=TRUE)


#Pulling data from FlightAware
origin_html <- read_html('https://www.flightaware.com/ajax/airport/cancelled_count.rvt?type=origin&timePeriod=today&airportFilter=')
airline_html <- read_html('https://www.flightaware.com/ajax/airport/cancelled_count.rvt?type=airline&timePeriod=today&airportFilter=')
destination_html <-read_html('https://www.flightaware.com/ajax/airport/cancelled_count.rvt?type=destination&timePeriod=today&airportFilter=')

#Writing function to parse the table
parse_table_rows <- function(page) {
  rows <- page %>% html_elements("tr")
  
  table_data <- map(rows, function(row) {
    cells <- row %>% html_elements("td")
    
    #Pulling text from tds
    td_text <- cells %>% html_text(trim = TRUE)
    
    #Pulling title from the fifth td
    hint_title5 <- cells[5] %>%
      html_element("span.hint") %>%
      html_attr("title")
    
    #Combining columns
    c(td_text, airport_name = hint_title5)
  })
  
  #Putting them into a dataframe
  df <- map_dfr(table_data, ~as.data.frame(t(.x), stringsAsFactors = FALSE))
  
  #Adjusting column names
  names(df) <- c("num_cancelled", "per_cancelled", "num_delayed", "per_delayed", "Airport", "airport_name")
  
  #Dropping first two rows
  df <- df[-c(1,2),]
  
  df <- df %>%
    mutate(Cancelled = paste(num_cancelled, ' (',per_cancelled,')', sep=''),
           Delayed = paste(num_delayed, ' (',per_delayed,')', sep='' )) %>%
    select(Cancelled, Delayed, Airport)
}

#Parsing through airline, origin, and destination cancellations and delays
origin <- parse_table_rows(origin_html)
destination <- parse_table_rows(destination_html)
airline <- parse_table_rows(airline_html)

###Now to make the Datawrapper....

#Pulling the date
today <- format(as.POSIXct(Sys.time(), tz = "America/New_York"), "%b. %d at %I:%M %p %Z")
today <- sub(" at 0", " at ", today)
today <- gsub("AM", "a.m.", today)
today <- gsub("PM", "p.m.", today)
today

#####Origin chart
#Editing the chart
dw_edit_chart(
  chart_id = origin_chart,
  title = 'Flight cancellations, delays by origin airport',
  intro = 'Below are the current number and percentage of flights delayed and cancelled leaving from each airport.',
  byline = 'Susie Webb/Get the Facts Data Team',
  source_name = 'Flight Aware',
  source_url = 'flightaware.com',
  annotate = paste("<i>Data as of ",today,".</i>")
)

#Adding data to the chart
dw_data_to_chart(origin,
                 chart_id = origin_chart
)

#Republishing the chart
dw_publish_chart(origin_chart)

#####Destination chart
#Editing the chart
dw_edit_chart(
  chart_id = destination_chart,
  title = 'Flight cancellations, delays by destination airport',
  intro = 'Below are the current number and percentage of flights delayed and cancelled arriving to each airport.',
  byline = 'Susie Webb/Get the Facts Data Team',
  source_name = 'Flight Aware',
  source_url = 'flightaware.com',
  annotate = paste("<i>Data as of ",today,".</i>")
)

#Adding data to the chart
dw_data_to_chart(destination,
                 chart_id = destination_chart
)

#Republishing the chart
dw_publish_chart(destination_chart)

#####Airline chart
#Editing the chart
dw_edit_chart(
  chart_id = airline_chart,
  title = 'Flight cancellations, delays by airline',
  intro = 'Below are the current number and percentage of flights delayed and cancelled by each airline.',
  byline = 'Susie Webb/Get the Facts Data Team',
  source_name = 'Flight Aware',
  source_url = 'flightaware.com',
  annotate = paste("<i>Data as of ",today,".</i>")
)

#Adding data to the chart
dw_data_to_chart(airline,
                 chart_id = airline_chart
)

#Republishing the chart
dw_publish_chart(airline_chart)
