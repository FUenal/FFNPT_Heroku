# load required packages
library(magrittr)
library(rvest)
library(stringr)
library(stringi)
library(readxl)
library(dplyr)
library(maps)
library(ggplot2)
library(reshape2)
library(ggiraph)
library(RColorBrewer)
library(leaflet)
library(geojsonio)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(sjmisc)
library(lubridate)
library(kableExtra)
library(gridExtra)
library(shinyjs)


# pkgs
suppressPackageStartupMessages(library(shiny))

# Load Image
# load("app/image.RData")

# set mapping color for each category of policy
#All_policies  = "016c59"
Moratoria_bans_limits = "#253550"
Subsidy_removal = "#0C101E"
Divestment = "#045a8d"
Government_policies = "#4d004b"
Non_Government_policies = "#016c59"
Cities_regions_states = "#FAB733"
FFNPT_total = "#FF9100"
Coal = "#FF8E15"


# Added ISO-normed country codes manually into the main xlsx data sheet (sheet 8) and manually updated country
# names in all sheets to standard format. Cleaning & Wrangling Process is documented in the BitsBites.R file

# import data
country_overview = read_excel("input_data/FF NPT Tracker DRAFT WIP.xlsx", sheet = 2)
regional_breakdown = read_excel("input_data/FF NPT Tracker DRAFT WIP.xlsx", sheet = 3)
state_city_breakdown = read_excel("input_data/FF NPT Tracker DRAFT WIP.xlsx", sheet = 4)
moratoria_bans_limits = read_excel("input_data/FF NPT Tracker DRAFT WIP.xlsx", sheet = 5)
subsidy_removal = read_excel("input_data/FF NPT Tracker DRAFT WIP.xlsx", sheet = 6)
divestment = read_excel("input_data/FF NPT Tracker DRAFT WIP.xlsx", sheet = 7)
divestment_new = read_excel("input_data/FF NPT Tracker DRAFT WIP.xlsx", sheet = 8)
divestment_scraped = read.csv("input_data/divestment_scraped.csv")
countries = read.csv("input_data/countries_codes_and_coordinates.csv")
worldcountry = geojson_read("input_data/50m.geojson", what = "sp")
country_geoms = read.csv("input_data/country_geoms.csv")
fossil_fuel_primary_energy = read.csv("input_data/fossil-fuel-primary-energy.csv")
fossil_fuels_share_energy = read.csv("input_data/fossil-fuels-share-energy.csv")
annual_share_of_co2_emissions = read.csv("input_data/annual-share-of-co2-emissions.csv")
oil_production = read.csv("input_data/oil-production-by-country.csv")
gas_production = read.csv("input_data/gas-production-by-country.csv")
coal_production = read.csv("input_data/coal-production-by-country.csv")

# Add geo data to sheets
country_overview['latitude'] <- countries$latitude[match(country_overview$ISO3, countries$ISO3)]
country_overview['longitude'] <- countries$longitude[match(country_overview$ISO3, countries$ISO3)]
country_overview['global_level'] <- countries$global_level[match(country_overview$ISO3, countries$ISO3)]
country_overview['continent_level'] <- countries$continent_level[match(country_overview$ISO3, countries$ISO3)]

# Added ISO Codes to all sheets manually due to mismatching name.

### MAP FUNCTIONS ###
# select large countries for mapping polygons
country_overview_large = country_overview %>% filter(ISO3 %in% worldcountry$ADM0_A3)
if (all(country_overview_large$ISO3 %in% worldcountry$ADM0_A3)==FALSE) { print("Error: inconsistent country names")}
country_overview_large = country_overview_large[order(country_overview_large$ISO3),]

### DATA PROCESSING: Policy Tracker Mapping: number of governmental and non-governmental and total number of policies and transfer to country_overview_large file###
country_overview_large <- country_overview_large %>% replace(is.na(.), 0)

## Calculate number of governmental and non-governmental policies
moratoria_bans_limits <- moratoria_bans_limits %>% group_by(ISO3) %>% mutate(a = sum(mbl_country))
moratoria_bans_limits <- moratoria_bans_limits %>% group_by(ISO3) %>% mutate(b = sum(mbl_city_region))
divestment_new <- divestment_new %>% group_by(ISO3) %>% mutate(a = sum(divestment_city_region))
divestment_new <- divestment_new %>% group_by(ISO3) %>% mutate(b = sum(divestment_non_government))
divestment <- divestment %>% group_by(ISO3) %>% mutate(a = sum(divestment_city_region))
divestment <- divestment %>% group_by(ISO3) %>% mutate(b = sum(divestment_non_government))
subsidy_removal <- subsidy_removal %>% group_by(ISO3) %>% mutate(a = sum(Policy))

# Replace NAs
#country_overview_large <- country_overview_large %>% replace(is.na(.), 0)

## Transfer total number and breakdowns of policies to country_overview_large file
country_overview_large['mbl_country'] <- moratoria_bans_limits$a[match(country_overview_large$ISO3, moratoria_bans_limits$ISO3)]
country_overview_large['mbl_city_region'] <- moratoria_bans_limits$b[match(country_overview_large$ISO3, moratoria_bans_limits$ISO3)]
country_overview_large['divestment_city_region'] <- divestment_new$a[match(country_overview_large$ISO3, divestment_new$ISO3)]
country_overview_large['divestment_non_government'] <- divestment_new$b[match(country_overview_large$ISO3, divestment_new$ISO3)]
country_overview_large['subsidy_removal'] <- subsidy_removal$a[match(country_overview_large$ISO3, subsidy_removal$ISO3)]

## Calculate total number of policies
country_overview_large <- country_overview_large %>% group_by(ISO3) %>%
        mutate(Moratoria_bans_limits_total = mbl_country + mbl_city_region)

country_overview_large <- country_overview_large %>% group_by(ISO3) %>%
        mutate(Divestment_total = divestment_city_region + divestment_non_government)

country_overview_large <- country_overview_large %>% group_by(ISO3) %>%
        mutate(Subsidy_removal_total = sum(subsidy_removal))

country_overview_large <- country_overview_large %>% replace(is.na(.), 0)

country_overview_large <- country_overview_large %>% group_by(ISO3) %>%
        mutate(Policy_total = Moratoria_bans_limits_total + Subsidy_removal_total + Divestment_total)

country_overview_large <- country_overview_large %>% group_by(ISO3) %>%
        mutate(Government_policies_total = Moratoria_bans_limits_total + Subsidy_removal_total + divestment_city_region)

country_overview_large <- country_overview_large %>% group_by(ISO3) %>%
        mutate(Non_Government_policies_total = divestment_non_government)


## Total number of state, city, region policies
state_city_breakdown_map <- state_city_breakdown %>%
        select(c("State_city_region", "Country", "latitude", "longitude", "ISO3", "Moratoria_bans_limits", "Subsidy_removal", "Divestment", "FFNPT")) %>%
        replace(is.na(.), 0)

state_city_breakdown_map <- state_city_breakdown_map %>% group_by(State_city_region) %>%
        mutate(Moratoria_bans_limits_total = sum(Moratoria_bans_limits))

state_city_breakdown_map <- state_city_breakdown_map %>% group_by(State_city_region) %>%
        mutate(Divestment_total = sum(Divestment))

state_city_breakdown_map <- state_city_breakdown_map %>% group_by(State_city_region) %>%
        mutate(Subsidy_removal_total = sum(Subsidy_removal))

state_city_breakdown_map <- state_city_breakdown_map %>% group_by(State_city_region) %>%
        mutate(ffnpt_total = sum(FFNPT))

state_city_breakdown_map <- state_city_breakdown_map %>% group_by(State_city_region) %>%
        mutate(City_region_state_total = sum(Moratoria_bans_limits + Divestment + Subsidy_removal + FFNPT))

# Matching dates from original divestment file with newly scraped divestment file
# divestment_new$Start <- divestment$Start[match(divestment_new$Organisation, divestment$Organisation)]

# Factor and label CAT_rating
#country_overview_large$CAT_rating <- factor(country_overview_large$CAT_rating, levels = c(0,1,2,3,4,5,6,7),
#labels = c("Critically Insufficient","Highly Insufficient","Insufficient",
#"2°C Compatible","1.5°C Paris Agreement Compatible", "Role Model","No Rating","No Data"))

# Factor, label, and reorder MTCO2e
country_overview_large$MTCO2e_cat <- cut(country_overview_large$MTCO2e, breaks = c(-100, -1, 1, 169, 500, 1000, 5000, 10000, 20000),
                                         labels = c("<0 MTCO2e", "No data", "1-169 MTCO2e", "169-500 MTCO2e", "500-1000 MTCO2e", "1000-5000 MTCO2e",
                                                    "5000-10000 MTCO2e", ">10000 MTCO2e"), right = FALSE)

country_overview_large$MTCO2e_cat = factor(country_overview_large$MTCO2e_cat,levels(country_overview_large$MTCO2e_cat)[c(2,1,3:8)])

### Pull % Global Emissions to country_overview file via ISO3
annual_share_of_co2_emissions_2019 <- annual_share_of_co2_emissions %>%
        filter(Year == 2019)

country_overview_large["global_emissions_percent"] <- annual_share_of_co2_emissions_2019$Share.of.global.CO2.emissions[match(country_overview_large$ISO3,
                                                                                                                             annual_share_of_co2_emissions_2019$Code)]

### Pull Fossil-Fuel share of primary energy to country_overview file via ISO3 and filter by Year 2019
fossil_fuels_share_energy_2019 <- fossil_fuels_share_energy %>%
        filter(Year == 2019)

country_overview_large["fossil_fuel_share_energy_2019"] <- fossil_fuels_share_energy_2019$Fossil.fuels....sub.energy.[match(country_overview_large$ISO3,
                                                                                                                            fossil_fuels_share_energy_2019$Code)]
country_overview_large <- country_overview_large %>% replace(is.na(.), 0)

# Factor and label Fossil Fuel share across countries in 2019
country_overview_large$ff_share_2019_cat <- cut(country_overview_large$fossil_fuel_share_energy_2019, breaks = c(-1, 1, 20, 40, 60, 80, 90, 95, 100),
                                                labels = c("No data", "1-20%", "20-40%","40-60%", "60-80%","80-90%", "90-95", "95-100%"), right = TRUE)


# Clean oil, gas, coal country names
# Oil
oil_production <- oil_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'United States', replacement = "United States of America")
})

oil_production <- oil_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'Russia', replacement = "Russian Federation")
})

oil_production <- oil_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'Iran', replacement = "Iran, Islamic Republic of")
})

oil_production <- oil_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'Democratic Republic of Congo', replacement = "Congo, (Kinshasa)")
})

oil_production <- oil_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'South Korea', replacement = "Korea, (South)")
})

oil_production <- oil_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'North Korea', replacement = "Korea, (North)")
})

oil_production <- oil_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'Tanzania', replacement = "Tanzania, United Republic of")
})

oil_production <- oil_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'Taiwan', replacement = "Taiwan, Republic of China")
})

oil_production <- oil_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'Macau', replacement = "Macao, SAR China")
})

oil_production <- oil_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'Laos', replacement = "Lao PDR")
})

# Gas
gas_production <- gas_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'United States', replacement = "United States of America")
})

gas_production <- gas_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'Russia', replacement = "Russian Federation")
})

gas_production <- gas_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'Iran', replacement = "Iran, Islamic Republic of")
})

gas_production <- gas_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'Democratic Republic of Congo', replacement = "Congo, (Kinshasa)")
})

gas_production <- gas_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'South Korea', replacement = "Korea, (South)")
})

gas_production <- gas_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'North Korea', replacement = "Korea, (North)")
})

gas_production <- gas_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'Tanzania', replacement = "Tanzania, United Republic of")
})

gas_production <- gas_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'Taiwan', replacement = "Taiwan, Republic of China")
})

gas_production <- gas_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'Macau', replacement = "Macao, SAR China")
})

gas_production <- gas_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'Laos', replacement = "Lao PDR")
})

# Coal
coal_production <- coal_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'United States', replacement = "United States of America")
})

coal_production <- coal_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'Russia', replacement = "Russian Federation")
})

coal_production <- coal_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'Iran', replacement = "Iran, Islamic Republic of")
})

coal_production <- coal_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'Democratic Republic of Congo', replacement = "Congo, (Kinshasa)")
})

coal_production <- coal_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'South Korea', replacement = "Korea, (South)")
})

coal_production <- coal_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'North Korea', replacement = "Korea, (North)")
})

coal_production <- coal_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'Tanzania', replacement = "Tanzania, United Republic of")
})

coal_production <- coal_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'Taiwan', replacement = "Taiwan, Republic of China")
})

coal_production <- coal_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'Macau', replacement = "Macao, SAR China")
})

coal_production <- coal_production %>% mutate_at(1, function(t) {
        str_replace(t, pattern = 'Laos', replacement = "Lao PDR")
})

# write country_overview_large file
write.csv(country_overview_large, file = "input_data/country_overview_large.csv")

# ### DATA PROCESSING: Policies converting Year from numeric to date format###
# moratoria_bans_limits$Date <-lubridate::ymd(moratoria_bans_limits$Start, truncated = 2L)
# subsidy_removal$Date <-lubridate::ymd(subsidy_removal$Start, truncated = 2L)
# divestment$Date <-lubridate::ymd(divestment$Start, truncated = 2L)
#
# # extract dates from moratoria_bans_limits data
# moratoria_bans_limits$date = as.Date(moratoria_bans_limits$Date, format="%d/%m/%Y")
# moratoria_bans_limits_min_date = min(moratoria_bans_limits$date)
# moratoria_bans_limits_max_date = max(moratoria_bans_limits$date)
# moratoria_bans_limits_max_date_clean = format(as.POSIXct(moratoria_bans_limits_max_date),"%d/%m/%Y")
#
# # extract dates from subsidy removaldata
# subsidy_removal$date = as.Date(subsidy_removal$Date, format="%d/%m/%Y")
# subsidy_removal_min_date = min(subsidy_removal$date)
# subsidy_removal_max_date = max(subsidy_removal$date)
# subsidy_removal_max_date_clean = format(as.POSIXct(subsidy_removal_max_date),"%d/%m/%Y")
#
# # extract dates from divestment data
# divestment$date = as.Date(divestment$Date, format="%d/%m/%Y")
# divestment_min_date = min(divestment$date)
# divestment_max_date = max(divestment$date)
# divestment_max_date_clean = format(as.POSIXct(divestment_max_date),"%d/%m/%Y")
#
### DATA PROCESSING: Fossil Fuel Production (gas, coal, and oil) converting Year from numeric to date format###
oil_production$Date <-lubridate::ymd(oil_production$Year, truncated = 2L)
gas_production$Date <-lubridate::ymd(gas_production$Year, truncated = 2L)
coal_production$Date <-lubridate::ymd(coal_production$Year, truncated = 2L)

# extract dates from oil production data
oil_production$date = as.Date(oil_production$Date, format="%d/%m/%Y")
oil_production_min_date = min(oil_production$date)
oil_production_max_date = max(oil_production$date)
oil_production_max_date_clean = format(as.POSIXct(oil_production_max_date),"%d/%m/%Y")

# extract dates from gas production data
gas_production$date = as.Date(gas_production$Date, format="%d/%m/%Y")
gas_production_min_date = min(gas_production$date)
gas_production_max_date = max(gas_production$date)
gas_production_max_date_clean = format(as.POSIXct(gas_production_max_date),"%d/%m/%Y")

# extract dates from mcoal production data
coal_production$date = as.Date(coal_production$Date, format="%d/%m/%Y")
coal_production_min_date = min(coal_production$date)
coal_production_max_date = max(coal_production$date)
coal_production_max_date_clean = format(as.POSIXct(coal_production_max_date),"%d/%m/%Y")

# #Set Main Plot output
# policy_count = function(country_overview_large){
#         x <- sum(country_overview_large$Policy_total)
#         print(x)
# }
#
# mbl_count = function(country_overview_large){
#         x <- sum(country_overview_large$Moratoria_bans_limits_total)
#         x
# }
#
# sr_count = function(country_overview_large){
#         x <- sum(country_overview_large$Subsidy_removal_total)
#         x
# }
#
# div_count = function(country_overview_large){
#         x <- sum(country_overview_large$Divestment_total)
#         x
# }
#
# gov_pol_count = function(country_overview_large){
#         x <- sum(country_overview_large$Government_policies_total)
#         x
# }
#
# Non_gov_pol_count = function(country_overview_large){
#         x <- sum(country_overview_large$Non_Government_policies_total)
#         x
# }

### MAP FUNCTIONS ###
# # function to plot cumulative Moratoria, Bans, and Limit Policies by date
# cumulative_mbl_plot = function(moratoria_bans_limits) {
#         g1 <- ggplot(moratoria_bans_limits, aes(x = date, y = Policy)) +
#                 geom_bar(position="stack", stat="identity", fill = Moratoria_bans_limits) +
#                 ylab("Moratoria, Bans, & Limit Policies") +  xlab("Year") + theme_bw() + ylim(0,30) +
#                 scale_fill_manual(values=c(Moratoria_bans_limits)) +
#                 xlim(c(moratoria_bans_limits_min_date,moratoria_bans_limits_max_date)) +
#                 scale_x_date(date_labels = "%Y", limits=c(moratoria_bans_limits_min_date,moratoria_bans_limits_max_date)) +
#                 theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10),
#                       plot.margin = margin(5, 10, 5, 5))
#         g1
# }
#
# # function to plot cumulative Divestments by date
# cumulative_div_plot = function(divestment_new) {
#         g2 <- ggplot(divestment_new, aes(x = date, y = Policy)) +
#                 geom_bar(position="stack", stat="identity", fill = Divestment) +
#                 ylab("Divestments") +  xlab("Year") + theme_bw() + ylim(0,300) +
#                 scale_fill_manual(values=c(Divestment)) +
#                 xlim(c(divestment_min_date,divestment_max_date)) +
#                 scale_x_date(date_labels = "%Y", limits=c(divestment_min_date,divestment_max_date)) +
#                 theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10),
#                       plot.margin = margin(5, 10, 5, 5))
#         g2
# }
#
# # function to plot cumulative subsidy_removal by date
# cumulative_sr_plot = function(subsidy_removal) {
#         g3 <- ggplot(subsidy_removal, aes(x = date, y = Policy)) +
#                 geom_bar(position="stack", stat="identity", fill = Subsidy_removal) +
#                 ylab("Subsidy Removals") +  xlab("Year") + theme_bw() + ylim(0,8) +
#                 scale_fill_manual(values=c(Subsidy_removal)) +
#                 xlim(c(subsidy_removal_min_date,subsidy_removal_max_date)) +
#                 scale_x_date(date_labels = "%Y", limits=c(subsidy_removal_min_date,subsidy_removal_max_date)) +
#                 theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10),
#                       plot.margin = margin(5, 10, 5, 5))
#         g3
# }


## create plotting parameters for map
#cv_pal <- colorFactor(palette = c("#FF0D0D","#FF4E11", "#FF8E15", "#FAB733", "#ACB334", "#69B34C", "#B1B6B9"), country_overview_large$CAT_rating)  ### Alternative baseline
cv_pal <- colorFactor(palette = c("#EFEFEF", "#FCDE9C", "#BEC5A9", "#8DA8AD", "#668BA8", "#466A9F", "#2C4B93", "#062A89"), country_overview_large$ff_share_2019_cat)
plot_map <- worldcountry[worldcountry$ADM0_A3 %in% country_overview_large$ISO3, ]


## Filter data for mapping by selecting only rows with policies > 0 for polygons and some circles
country_overview_large_map = country_overview_large %>% filter(Government_policies_total > 0 & Non_Government_policies_total > 0)

# Filter data for mapping by selecting only rows with policies > 0 for state_city_breakdown
state_city_breakdown_map_ffnpt = state_city_breakdown_map %>% filter(ffnpt_total > 0)


# create base map 
basemap = leaflet(plot_map) %>% 
        addTiles() %>% 
        addLayersControl(
                position = "topright",
                overlayGroups = c("Cities, States, Regions", "Governmental Policies", "Non-Governmental Policies", "Fossil Fuel Non-Proliferation"),
                options = layersControlOptions(collapsed = TRUE, autoZIndex = TRUE)) %>% 
        hideGroup(c("Cities, States, Regions", "Governmental Policies", "Non-Governmental Policies", "Fossil Fuel Non-Proliferation")) %>% 
        htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\"><strong>Policy Level/Type</strong><br/></label>');
        }
    ") %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        fitBounds(~-100,-60,~60,70) %>%
        addLegend("bottomleft", colors = c("#EFEFEF", "#FCDE9C", "#BEC5A9", "#8DA8AD", "#668BA8", "#466A9F", "#2C4B93", "#062A89"), 
                  labels =  c("No data", "1-20%", "20-40%","40-60%", "60-80%","80-90%","90-95%","95-100%"), values = ~country_overview_large$ff_share_2019_cat,
                  title = "Fossil fuels<br/>(% sub energy)<br/>2019") %>%  
        
        addPolygons(stroke = FALSE, smoothFactor = 0.4, fillOpacity = 0.65, fillColor = ~cv_pal(country_overview_large$ff_share_2019_cat),
                    label = sprintf("<strong>%s</strong><br/><small>Fossil-Fuels Sub Energy: %s </small><br/><small>Climate Risk Index: %s</small><br/><small>Total number of policies: %g</small>", 
                                    country_overview_large$Country, country_overview_large$ff_share_2019_cat, country_overview_large$cri, country_overview_large$Policy_total) %>% 
                            lapply(htmltools::HTML),
                    labelOptions = labelOptions(
                            style = list("font-weight" = "normal", "font-family" = "Poppins", padding = "3px 8px", "color" = cv_pal),
                            textsize = "15px", direction = "auto")) %>%
        
        addCircleMarkers(data = country_overview_large_map, lat = ~ latitude, lng = ~ longitude, weight = 1, 
                         radius = ~(Government_policies_total)^(0.55),
                         fillOpacity = 0.2, color = Government_policies, group = "Governmental Policies",
                         label = sprintf("<strong>%s</strong><br/>Governmental Policies: %g", country_overview_large_map$Country, 
                                         country_overview_large_map$Government_policies_total) %>% lapply(htmltools::HTML),
                         labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", "font-family" = "Poppins", padding = "3px 8px", "color" = Government_policies),
                                 textsize = "15px", direction = "auto")) %>% 
        
        addCircleMarkers(data = country_overview_large_map, lat = ~ latitude, lng = ~ longitude, weight = 1, 
                         radius = ~(Non_Government_policies_total)^(0.55),
                         fillOpacity = 0.2, color = Non_Government_policies, group = "Non-Governmental Policies",
                         label = sprintf("<strong>%s</strong><br/>Non-Governmental Policies: %g", country_overview_large_map$Country, 
                                         country_overview_large_map$Non_Government_policies_total) %>% lapply(htmltools::HTML),
                         labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", "font-family" = "Poppins", padding = "3px 8px", "color" = Non_Government_policies),
                                 textsize = "15px", direction = "auto")) %>%
        
        addCircleMarkers(data = state_city_breakdown_map, lat = ~ latitude, lng = ~ longitude, weight = 2, 
                         radius = ~(City_region_state_total)^(0.89),
                         fillOpacity = 0.2, color = Cities_regions_states, group = "Cities, States, Regions",
                         label = sprintf("<strong>%s</strong><br/><small>Moratoria, Bans, Limits: %g</small><br/><small>Subsidy Removals: %d</small><br/><small>Divestments: %g</small><br/><small>FF NPT: %g</small>", 
                                         state_city_breakdown_map$State_city_region, state_city_breakdown_map$Moratoria_bans_limits_total,state_city_breakdown_map$Subsidy_removal_total,
                                         state_city_breakdown_map$Divestment_total, state_city_breakdown_map$ffnpt_total) %>% lapply(htmltools::HTML),
                         labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", "font-family" = "Poppins", padding = "3px 8px", "color" = Cities_regions_states),
                                 textsize = "15px", direction = "auto")) %>% 
        
        addCircleMarkers(data = state_city_breakdown_map_ffnpt, lat = ~ latitude, lng = ~ longitude, weight = 4, 
                         fillOpacity = 0.9, color = FFNPT_total, group = "Fossil Fuel Non-Proliferation",
                         label = sprintf("<strong>%s</strong><br/><small>Fossil Fuel Non-Proliferation: %g</small>", 
                                         state_city_breakdown_map_ffnpt$State_city_region, state_city_breakdown_map_ffnpt$ffnpt_total) %>% lapply(htmltools::HTML),
                         labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", "font-family" = "Poppins", padding = "3px 8px", "color" = FFNPT_total),
                                 textsize = "15px", direction = "auto"))



### SHINY SERVER ###

server = function(input, output) {
        
        # Policy tab 
        output$mymap <- renderLeaflet({ 
                basemap
        })
        
        output$cumulative_mbl_plot <- renderPlot({
                cumulative_mbl_plot(moratoria_bans_limits)
        })
        
        output$cumulative_div_plot <- renderPlot({
                cumulative_div_plot(divestment)
        })
        output$cumulative_sr_plot <- renderPlot({
                cumulative_sr_plot(subsidy_removal)
        })
        
        output$policy_count <- renderText({
                paste0(prettyNum(sum(country_overview_large$Policy_total), big.mark=","), " Policies Overall")
        })
        
        output$gov_pol_count <- renderText({
                paste0(prettyNum(sum(country_overview_large$Government_policies_total), big.mark=","), " Government policies")
        })
        
        output$Non_gov_pol_count <- renderText({
                paste0(prettyNum(sum(country_overview_large$Non_Government_policies_total), big.mark=","), " Non-government policies")
        })
        
        output$mbl_count <- renderText({
                paste0(prettyNum(sum(country_overview_large$Moratoria_bans_limits_total), big.mark=","), " Moratoria, bans, limits")
        })
        
        output$sr_count <- renderText({
                paste0(prettyNum(sum(country_overview_large$Subsidy_removal_total), big.mark=","), " Subsidy removals")
        })
        
        output$div_count <- renderText({
                paste0(prettyNum(sum(country_overview_large$Divestment_total), big.mark=","), "  Divestments")
        })
        
        
        # render report when button clicked
        observeEvent(input$country, {
                country_overview_largeDF <- country_overview_large[country_overview_large$Country == input$country, ]
                moratoria_bans_limitsDF <- moratoria_bans_limits[moratoria_bans_limits$Country == input$country, ]
                subsidy_removalDF <- subsidy_removal[subsidy_removal$Country == input$country, ]
                divestmentDF <- divestment[divestment$Country == input$country, ]
                oilDF <- oil_production[oil_production$Entity == input$country, ]
                gasDF <- gas_production[gas_production$Entity == input$country, ]
                coalDF <- coal_production[coal_production$Entity == input$country, ]
                output$report <- renderUI({
                        includeHTML(
                                rmarkdown::render(
                                        "report_template.Rmd",
                                        params = list(
                                                selection = input$country,
                                                data = country_overview_largeDF,
                                                data = country_overview_largeDF,
                                                data_mbl = moratoria_bans_limitsDF,
                                                data_sr = subsidy_removalDF,
                                                data_div = divestmentDF,
                                                data_oil = oilDF,
                                                data_gas = gasDF,
                                                data_coal = coalDF
                                        )
                                )
                        )
                })
        })
        
        
        # render report when button clicked
        observeEvent(input$country, {
                moratoria_bans_limitsDF <- moratoria_bans_limits[moratoria_bans_limits$Country == input$country, ]
                output$report1 <- renderUI({
                        includeHTML(
                                rmarkdown::render(
                                        "report_template1.Rmd",
                                        params = list(
                                                selection = input$country,
                                                data_mbl = moratoria_bans_limitsDF
                                        )
                                )
                        )
                })
        })
        
        # render report when button clicked
        observeEvent(input$country, {
                subsidy_removalDF <- subsidy_removal[subsidy_removal$Country == input$country, ]
                output$report2 <- renderUI({
                        includeHTML(
                                rmarkdown::render(
                                        "report_template2.Rmd",
                                        params = list(
                                                selection = input$country,
                                                data_sr = subsidy_removalDF
                                        )
                                )
                        )
                })
        })
        
        # render report when button clicked
        observeEvent(input$country, {
                divestmentDFN <- divestment_scraped[divestment_scraped$Country == input$country, ]
                output$report3 <- renderUI({
                        includeHTML(
                                rmarkdown::render(
                                        "report_template3.Rmd",
                                        params = list(
                                                selection = input$country,
                                                data_divN = divestmentDFN
                                        )
                                )
                        )
                })
        })
        
}
