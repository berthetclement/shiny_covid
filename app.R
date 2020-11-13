
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(leaflet)
library(dplyr)

library(htmltools)
library(plotly)
library(utils)
library(icon)
library(gt)

library(data.table)
library(geojsonio)
library(RColorBrewer)
library(tidyr)

library(covid)

source("modules/map_monde_covid.R", encoding = "UTF-8")
source("modules/pays.R", encoding = "UTF-8")
source("modules/export.R", encoding = "UTF-8")
source("modules/apropos.R", encoding = "UTF-8")
source("modules/france.R", encoding = "UTF-8")



# CSSE ------------------------------------------------------------------------------
# INPUT CSSE ----
countries <- read.csv("input_data/countries_codes_and_coordinates.csv")
worldcountry <- geojson_read("input_data/50m.geojson", what = "sp")
ref_reco <- fread("input_data/input_country.csv")

# chargement des donnees
# imports CSSE ----
cible_cases <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
df_cases <- data_covid(cible_cases)

cible_deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
df_deaths <- data_covid(cible_deaths)

cible_recover <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
df_recover <- data_covid(cible_recover)

# recodages pays (country) ----
df_cases_recod <- data_recod("input_data/input_country.csv", df_cases)
df_deaths_recod <- data_recod("input_data/input_country.csv", df_deaths)
df_recover_recod <- data_recod("input_data/input_country.csv", df_recover)

# format tidy ----
tidy_cases <- make_tidy_df(df_cases_recod) %>% rename(cases=count)
tidy_deaths <- make_tidy_df(df_deaths_recod) %>% rename(deaths=count)
tidy_recover <- make_tidy_df(df_recover_recod) %>% rename(recovered=count)

# merge ---
tidy_global <- merge(tidy_cases, tidy_deaths, by=c("Country", "date"))
tidy_global <- merge(tidy_global, tidy_recover, by=c("Country", "date"))

# Ajout des stats nouveaux cas/morts/gueris ----
tidy_global <- add_stats(tidy_global)



# selection d'une journee pour base map 
df_a_day <- tidy_global %>% filter(date %in% max(tidy_global$date))

# jointure avec countries
names(df_a_day)[1] <- "jhu_ID" # pour jointure
df_a_day <- merge(df_a_day, countries, by = "jhu_ID")

# selection des pays pour map
df_large_countries <- df_a_day %>% filter(alpha3 %in% worldcountry$ADM0_A3)

df_large_countries <- df_large_countries[order(df_large_countries$alpha3), ]

# indicateur cas/population/100k (coloriage map)
df_large_countries <- add_map_stats(df_large_countries)


# pour cumul plot
covid_col = "#cc4c02"

# df agrege pour plot (global)
comptage_global <- global_stats(tidy_global)
comptage_global$date <- as.Date(comptage_global$date,"%Y-%m-%d") # pour axe x


# base map pour observevent ----
# create plotting parameters for map
bins = c(0,1,5,10,50,100,Inf)
cv_pal <- colorBin("BuPu", domain = df_large_countries$deathsper100k, bins = bins) # fond de carte
plot_map <- worldcountry[worldcountry$ADM0_A3 %in% df_large_countries$alpha3 , ]# "SpatialPolygonsDataFrame"

base_map <- base_map_global(df_large_countries, plot_map, cv_pal)


# data pour onglet pays ----

# gestion des couleurs pays ---- 
# assign colours to countries to ensure consistency between plots 
cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), 
            brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), 
            brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  
            brewer.pal(9, "Pastel1"),  
            brewer.pal(8, "Pastel2")),4)

cls_names = c(as.character(unique(df_large_countries$country)), 
              as.character(unique(df_large_countries$continent)),
              "Global")

country_cols = cls[1:length(cls_names)]
names(country_cols) = cls_names

# Globales dates pour sliderInput (mapUI) 
min_max_dates <- c(min(tidy_global$date), max(tidy_global$date))

# FRANCE ---------------------
# INPUT FRANCE -------------------
# carte geo geojson
reg_geojson = geojson_read("input_data/reg_fra.geojson", what = "sp")

# referentiel pour coord gps des regions 
reg_gps <- fread("input_data/ref_reg.csv", colClasses=c(code = "character"))

# import opencovid19-fr ------------------------------------------------
cible_cases <- "https://raw.githubusercontent.com/opencovid19-fr/data/master/dist/chiffres-cles.csv"
df_france <- data_covid(cible_cases, encoding='UTF-8')

# merge csse opencovid-fr --------------------------------------------------
# 1 - CSSE France -------------------

df_cases_fr <- df_cases %>% filter(`Country/Region` %in% "France")
names(df_cases_fr)[1:2] <- c("region", "pays")

df_deaths_fr <- df_deaths %>% filter(`Country/Region` %in% "France")
names(df_deaths_fr)[1:2] <- c("region", "pays")

df_recover_fr <- df_recover %>% filter(`Country/Region` %in% "France")
names(df_recover_fr)[1:2] <- c("region", "pays")

# 2 - format tidy France + detail DOM TOM  CSSE ----------------------------------------------------

tidy_cases_fr <- df_cases_fr %>%
  select(-c(Long, Lat)) %>% 
  pivot_longer(-c(region, pays), names_to = "date", values_to = "cas")

tidy_deaths_fr <- df_deaths_fr %>%
  select(-c(Long, Lat)) %>% 
  pivot_longer(-c(region, pays), names_to = "date", values_to = "morts")

tidy_recover_fr <- df_recover_fr %>%
  select(-c(Long, Lat)) %>% 
  pivot_longer(-c(region, pays), names_to = "date", values_to = "gueris")

tidy_cases_fr <- data_recod_dom("input_data/input_country.csv", tidy_cases_fr)
tidy_deaths_fr <- data_recod_dom("input_data/input_country.csv", tidy_deaths_fr)
tidy_recover_fr <- data_recod_dom("input_data/input_country.csv", tidy_recover_fr)

# 3 - merge CSSE (France) --------------------------------------------------------------------
# merge des data CSSE France (detail dom tom)

tidy_global_fr <- merge(tidy_cases_fr, tidy_deaths_fr)
tidy_global_fr <- merge(tidy_global_fr, tidy_recover_fr)


# ordonner par date 
tidy_global_fr$date <- as.Date(tidy_global_fr$date, "%m/%d/%y")
tidy_global_fr <- tidy_global_fr[order(tidy_global_fr$pays, tidy_global_fr$date),]

# reco "" -> France pour jointure des chiffres 
tidy_global_fr[tidy_global_fr$region %in% "", ]$region <- "France"

# 4- merge opencovid19-fr ---------------------------------------------------------

# enlever accent la reunion pour merge
names(df_france)[4] = "region" # (maille_nom -> region)
df_france_reco <- data_recod_dom("input_data/input_country.csv", df_france)

# mise au format date (Idate -> date)
df_france_reco$date <- as.Date(df_france_reco$date)

# full join data ------------------------------------------
full_df_fr <- full_join(df_france_reco, tidy_global_fr, by= c("date", "region"))

# date input slider shiny ---------------

# "2020-01-24" min des donnes open covid fr

date_in_fra <- c("2020-01-24", format(max(full_df_fr$date), "%Y-%m-%d")) 

# pb de date apres la mise a jour, a date maj, 
# ttes les data regions sont a NA le tps que les donnees remontent

# selection de la date - 1 jour
max_date <- as.POSIXlt(date_in_fra[2])
max_date$mday <- max_date$mday - 1
max_date <- as.Date(max_date)

date_in_fra <- c("2020-01-24", format(max_date, "%Y-%m-%d")) 

# stockage region et code region (maille_code)
nom_regions <- full_df_fr %>% filter(granularite %in% "region") %>% 
  select(region, maille_code) %>% unique

# merge des regions (y compris global france) --------------
df_region <- merge(full_df_fr, reg_gps, by = "maille_code")
names(df_region)[grepl("region.x", names(df_region))] <- "region"

# df_region %>% select(region) %>% unique (france inclu)

df_region_a_date <- df_region %>% filter(date %in% as.Date(date_in_fra[2]) )

# ordonner les deux fichiers, meme ordre code region 
df_region_a_date <- df_region_a_date[match(reg_geojson@data[["code"]], df_region_a_date$code), ]

# base map france --------------------------------------

# couleurs des cercles
hopsi_cercle_col <- "#cc4c02"
global_cercle_col <- "#2ed15d " 

# calcul a date
# couleurs fonds de carte sur deces
quant_fr <- round(as.vector(quantile(df_region_a_date$deces, probs = seq(0, 1, 0.15))))-0.7
bins_fr = c(0, quant_fr, Inf)
cv_pal_fr <- colorBin("BuPu", 
                      domain = df_region_a_date$deces, 
                      bins = bins_fr) # fond de carte

# coord gps fra global
set_view_fr <- reg_gps %>% filter(region %in% "France")

base_map_fr <- leaflet(reg_geojson) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(lng = set_view_fr$longitude, lat = set_view_fr$latitude, 5)%>%
  addLegend("bottomright", pal = cv_pal_fr, values = df_region_a_date$deces, # les intervalles (0-50-100)
            title = "<small>Nombre des deces (cumul)</small>") 

# base map REUNION -------------------------------------------------
# coord gps 
set_view_run <- reg_gps %>% filter(region %in% "La_Reunion")

base_map_run <- leaflet(reg_geojson, 
                        options = leafletOptions(
                          attributionControl=FALSE))  %>% 
  addTiles() %>%  
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(lng = set_view_run$longitude, lat = set_view_run$latitude, 9) 

# base map Guyane -------------------------------------------------
# coord gps 
set_view_guy <- reg_gps %>% filter(region %in% "Guyane")

base_map_guy <- leaflet(reg_geojson, 
                        options = leafletOptions(
                          attributionControl=FALSE))  %>% 
  addTiles() %>%  
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(lng = set_view_guy$longitude, lat = set_view_guy$latitude, 6)  


# base map Martinique  -------------------------------------------------
# coord gps 
set_view_mar <- reg_gps %>% filter(region %in% "Martinique")

base_map_mar <- leaflet(reg_geojson, 
                        options = leafletOptions(
                          attributionControl=FALSE))  %>% 
  addTiles() %>%  
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(lng = set_view_mar$longitude, lat = set_view_mar$latitude, 9)  

# base map Guadeloupe  -------------------------------------------------
# coord gps 
set_view_gua <- reg_gps %>% filter(region %in% "Guadeloupe")

base_map_gua <- leaflet(reg_geojson, 
                        options = leafletOptions(
                          attributionControl=FALSE))  %>% 
  addTiles() %>%  
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(lng = set_view_gua$longitude, lat = set_view_gua$latitude, 9)

# base map Mayotte  -------------------------------------------------
# coord gps 
set_view_mayo <- reg_gps %>% filter(region %in% "Mayotte")

base_map_mayo <- leaflet(reg_geojson, 
                         options = leafletOptions(
                           attributionControl=FALSE))  %>% 
  addTiles() %>%  
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(lng = set_view_mayo$longitude, lat = set_view_mayo$latitude, 9)



# ui ----

ui <- function(){
  
  #bootstrapPage(
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             "COVID-19", id="nav",
             header = tagList(
               useShinydashboard(),
               setBackgroundColor(color = c("GhostWhite"))
             )
             ,
             tabPanel("Map monde",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          mapUI("mymap", min_max_dates)
                      )
             ), # fin map
             tabPanel("Pays",
                      paysUI("id_paysui", min_max_dates, df_large_countries)
             ), # fin pays
             tabPanel("France",
                      franceUI("id_franceui", date_in_fra)
             ), # fin France
             tabPanel("Data CSSE",
                      exportUI("id_export")
             ), # fin export
             tabPanel("A propos",
                      aproposUI("id_apropos"))
  )
  #)
}


# serveur ----

server <- function(input, output, session){
  
  # map monde ----
  mapServer("mymap", tidy_global, worldcountry, base_map, plot_map, cv_pal, countries, comptage_global, covid_col)
  
  # Pays ----
  paysServer("id_paysui", min_max_dates, tidy_global, countries, df_large_countries, country_cols)
  
  # France -----
  fraServer("id_franceui", full_df_fr, base_map_fr, reg_geojson, 
            reg_gps, cv_pal_fr, hopsi_cercle_col, global_cercle_col, 
            base_map_run, base_map_guy, base_map_mar, base_map_gua, base_map_mayo)
  
  # export ----
  exportServer("id_export", tidy_global, countries, min_max_dates[2])
}

# Run the application 
shinyApp(ui = ui, server = server)



