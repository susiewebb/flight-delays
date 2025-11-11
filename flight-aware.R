#Loading libraries
library(tidyverse)
library(jsonlite)
library(rvest)
library(DatawRappr)

#Loading API key, chart keys
api_key <- Sys.getenv("API_KEY")
origin_chart <- Sys.getenv("ORIGIN_KEY")
destination_chart <- Sys.getenv("DESTINATION_KEY")
airline_chart <- Sys.getenv("AIRLINE_KEY")

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
  names(df) <- c("Canceled", "% C", "Delayed", "% D", "Airport", "airport_name")
  
  #Dropping first two rows
  df <- df[-c(1,2),]
  
     
  norm_airport <- function(x) {
    x %>%
      str_replace_all("’", "'") %>%     #Normalize the airport info
      str_replace_all("\\s*\\(", " (") %>%  #Exclude double spacing
      str_squish() %>%
      str_to_lower()                    #Lower case
  }
  
  
  #Editing airport names
  lookup <- tribble(
    ~from,                                 ~to,
    "Baltimore/Washington Intl(BWI)",      "Baltimore/Washington Intl Thurgood Marshall (BWI)",
    "Hartsfield-Jackson Intl (ATL)",       "Hartsfield-Jackson Atlanta International Airport (ATL",
    "Manila Int'l (MNL)",                  "Ninoy Aquino Int'l (MNL)",
    "Dallas-Fort Worth Intl (DFW)",         "Dallas Fort Worth Intl (DFW)",
    "John F Kennedy Intl (JFK)	", "John F. Kennedy Intl (JFK)",
    "Charlotte/Douglas Intl (CLT)", "Charlotte Douglas Intl (CLT)",
    "Minneapolis/St Paul Intl (MSP)", "Minneapolis-St Paul Intl (MSP)",
    "Houston Bush Int'ctl (IAH)", "George Bush Int'ctl (IAH)",
    "Cleveland-Hopkins Intl (CLE)", "Cleveland Hopkins Intl (CLE)",
    "St Louis Lambert Intl (STL)", "St. Louis Lambert Intl (STL)",
    "Dallas Love Fld (DAL)", "Dallas Love Field (DAL)",
    "William P Hobby (HOU)", "William P. Hobby (HOU)",
    "Urumqi Diwopu Int'l (URC)", "Ürümqi Diwopu Int'l (URC)",
    "Gerald R Ford Intl (GRR)", "Gerald R. Ford Intl (GRR)",
    "Montreal-Trudeau (YUL)", "Montréal-Pierre Elliott Trudeau (YUL)",
    "Frederick Douglass/Greater Rochester Intl (ROC)", "Frederick Douglass Greater Rochester Intl (ROC)",
    "Greenville/Spartanburg Intl (GSP)", "Greenville Spartanburg Intl (GSP)",
    "Daniel K Inouye Intl (HNL)", "Daniel K. Inouye Intl (HNL)"
    
  ) %>%
    mutate(key = norm_airport(from)) %>%
    select(key, to)  
  
  df <- df %>%
    select(-airport_name) %>%
    mutate(key = norm_airport(Airport)) %>%
    left_join(lookup, by = "key") %>%
    mutate(Airport = coalesce(to, Airport)) %>%
    select(-key, -to)

}

#Parsing through airline, origin, and destination cancellations and delays
origin <- parse_table_rows(origin_html) %>%
  rename('Origin airport' = Airport)

destination <- parse_table_rows(destination_html)%>%
  rename('Destination airport' = Airport)

airline <- parse_table_rows(airline_html)%>%
  rename(Airline = Airport)

###Now to make the Datawrapper....

#Pulling the date
today <- format(as.POSIXct(Sys.time(), tz = "America/New_York"), "%b. %d at %I:%M %p %Z")
today <- sub(" at 0", " at ", today)
today <- gsub("AM", "a.m.", today)
today <- gsub("PM", "p.m.", today)

today_head <- format(as.POSIXct(Sys.time(), tz = "America/New_York"), "%b. %d")


#####Origin chart
#Editing the chart
dw_edit_chart(
  chart_id = origin_chart,
  title = paste('Flight cancellations, delays by origin airport on', today_head),
  intro = 'Below are the current number and percentage of flights delayed and cancelled leaving from each airport.<br>
  <a target="_self" href="https://datawrapper.dwcdn.net/f9FDc/" style="background-color:#3E495D; padding:4px 8px; border-radius:5px; color:#fff; font-weight:700; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer; display: inline-block;margin-bottom:2px;margin-top:4px;  opacity: 100%; " rel="nofollow noopener">By origin</a> &nbsp
<a target="_self" href="https://datawrapper.dwcdn.net/o0LNE/" style="background-color:#3E495D; padding:4px 8px; border-radius:5px; color:#ffffff; font-weight:700; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer; display: inline-block;margin-bottom:2px;margin-top:4px;  opacity: 50%; " rel="nofollow noopener"> By destination</a> &nbsp
<a target="_self" href="https://datawrapper.dwcdn.net/2Fncn/" style="background-color: #3E495D; padding:4px 8px; border-radius:5px; color:#ffffff; font-weight:700; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer; display: inline-block;margin-bottom:2px;margin-top:12px;margin-bottom:9px;  opacity: 50%;rel="nofollow noopener">By airline</a>',
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
  title = paste('Flight cancellations, delays by destination airport on', today_head),
  intro = 'Below are the current number and percentage of flights delayed and cancelled arriving to each airport.<br>
  <a target="_self" href="https://datawrapper.dwcdn.net/f9FDc/" style="background-color:#3E495D; padding:4px 8px; border-radius:5px; color:#fff; font-weight:700; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer; display: inline-block;margin-bottom:2px;margin-top:4px;  opacity: 50%; " rel="nofollow noopener">By origin</a> &nbsp
<a target="_self" href="https://datawrapper.dwcdn.net/o0LNE/" style="background-color:#3E495D; padding:4px 8px; border-radius:5px; color:#ffffff; font-weight:700; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer; display: inline-block;margin-bottom:2px;margin-top:4px;  opacity: 100%; " rel="nofollow noopener"> By destination</a> &nbsp
<a target="_self" href="https://datawrapper.dwcdn.net/2Fncn/" style="background-color: #3E495D; padding:4px 8px; border-radius:5px; color:#ffffff; font-weight:700; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer; display: inline-block;margin-bottom:2px;margin-top:12px;margin-bottom:9px;  opacity: 50%;rel="nofollow noopener">By airline</a>',
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
  title = paste('Flight cancellations, delays by airline on', today_head),
  intro = 'Below are the current number and percentage of flights delayed and cancelled by each airline.<br>
  <a target="_self" href="https://datawrapper.dwcdn.net/f9FDc/" style="background-color:#3E495D; padding:4px 8px; border-radius:5px; color:#fff; font-weight:700; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer; display: inline-block;margin-bottom:2px;margin-top:4px;  opacity: 50%; " rel="nofollow noopener">By origin</a> &nbsp
<a target="_self" href="https://datawrapper.dwcdn.net/o0LNE/" style="background-color:#3E495D; padding:4px 8px; border-radius:5px; color:#ffffff; font-weight:700; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer; display: inline-block;margin-bottom:2px;margin-top:4px;  opacity: 50%; " rel="nofollow noopener"> By destination</a> &nbsp
<a target="_self" href="https://datawrapper.dwcdn.net/2Fncn/" style="background-color: #3E495D; padding:4px 8px; border-radius:5px; color:#ffffff; font-weight:700; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer; display: inline-block;margin-bottom:2px;margin-top:12px;margin-bottom:9px;  opacity: 100%;rel="nofollow noopener">By airline</a> ',
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
