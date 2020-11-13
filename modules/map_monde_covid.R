
# mapping principal ----

mapUI <- function(id, dates_param){
  ns <- NS(id)
  
  fluidPage(theme = "styles.css",
    leafletOutput(ns("id_map"), width="100%", height=600),
    
    absolutePanel(id = "controls", class = "panel panel-default",
                  top = 75, left = 55, width = 250, fixed=TRUE,
                  draggable = TRUE, height = "auto",
                  
                  span(
                    tags$i(
                      h6(
                        "Les données proviennent du Centre des sciences et de l'ingénierie des systèmes de l'Université Johns Hopkins (JHU CSSE) ."
                        , 
                        style="color:#045a8d")
                      )),
                  # color:#cc4c02
                  h3(textOutput(ns("case_count")), align = "right"),
                  h4(textOutput(ns("death_count")), align = "right"),
                  
                  h6(textOutput(ns("date_reactive")), align = "right"),
                  h6(textOutput(ns("country_count")), align = "right"),
                  
                  plotOutput(ns("epi_curve"), height="130px", width="100%"),
                  plotOutput(ns("cumulative_plot"), height="130px", width="100%"),

                  sliderInput(ns("plot_date"),
                              label = h5("Sélection de la date"),
                              min = as.Date(dates_param[1],"%Y-%m-%d"),
                              max = as.Date(dates_param[2]),
                              value = as.Date(dates_param[2]),
                              timeFormat = "%d %b",
                              animate=animationOptions(interval = 3000, loop = FALSE))
    )
  )
  
  
  
}


mapServer  <- function(id, data_sf, map_json, obj_map_base, obj_plot_map, func_pal, df_countries, df_agreg, col_cercle){
  moduleServer(
    id,
    function(input, output, session){
      
      # data reactiv pour map
      map <- reactive({
        # selection d'une journee
        df_a_day <- data_sf %>% filter(date %in% format(input$plot_date)) # input

        # jointure avec countries
        names(df_a_day)[1] <- "jhu_ID" # pour jointure
        df_a_day <- merge(df_a_day, df_countries, by = "jhu_ID")

        # selection des pays pour map
        df_large <- df_a_day %>% filter(alpha3 %in% map_json$ADM0_A3)

        df_large <- df_large[order(df_large$alpha3), ]

        # indicateur cas/population/100k (coloriage map)
        df_large <- add_map_stats(df_large)
        df_large
        
      })
      
      # plot map 
      # output$id_map = renderLeaflet(map_global(map(), map_json))
      output$id_map = renderLeaflet({
        obj_map_base
      })
       
      observeEvent(input$plot_date, {
        leafletProxy("id_map") %>% 
           clearMarkers() %>%
           clearShapes() %>% 
          addPolygons(data = obj_plot_map, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.6,
                      fillColor = func_pal(map()$deathsper100k)) %>%
          addCircleMarkers(data = map(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/5),
                           fillOpacity = 0.2, color = col_cercle, group = "2020-COVID (cas cumules)",
                           label = sprintf("<strong>%s (cumul)</strong><br/>Cas confirmes: %g<br/>Morts: %d<br/>Guerisons: %d<br/>Cas pour 100,000: %g<br/>Morts pour 100,000: %g",
                                           map()$country, map()$cases,
                                           map()$deaths, map()$recover,
                                           map()$per100k,
                                           map()$deathsper100k) %>%
                             lapply(htmltools::HTML),
                           labelOptions = labelOptions(
                             style = list("font-weight" = "normal", padding = "3px 8px", "color" = col_cercle),
                             textsize = "15px", direction = "auto")) %>%
          addCircleMarkers(data = map(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(new_cases)^(1/5),
                           fillOpacity = 0.2, color = col_cercle, group = "2020-COVID (nouveaux cas)",
                           label = sprintf("<strong>%s (nouveaux cas)</strong><br/>Nouveaux cas: %g<br/>Deces: %d<br/>Guerisons: %d<br/>Cas pour 100,000: %g<br/>Morts pour 100,000: %g",
                                           map()$country, map()$new_cases,
                                           map()$new_deaths, map()$new_recovered,
                                           map()$newper100k, map()$newdeathsper100k) %>%
                             lapply(htmltools::HTML),
                           labelOptions = labelOptions(
                             style = list("font-weight" = "normal", padding = "3px 8px", "color" = col_cercle),
                             textsize = "15px", direction = "auto"))
          
      })
      
      # date du slider 
      output$date_reactive <- renderText({
        format(as.POSIXct(input$plot_date),"%d %B %Y")
      })
      
      # cumul plot 
      output$cumulative_plot <- renderPlot({
        var <- sym("cases")
        cumulative_plot(df_agreg, input$plot_date, var, "cases", col_cercle)
      })
      
      # nouveaux cas plot 
      output$epi_curve <- renderPlot({
        var <- sym("new_cases")
        new_cases_plot(df_agreg, input$plot_date, var, "nouveaux cas", col_cercle)
      })
      
      # stats globales 
      # cases
      output$case_count <- renderText({
        count_case <- df_agreg %>% filter(date %in% as.Date(input$plot_date)) %>% 
          select(cases)
        
        
        paste0(prettyNum(count_case, big.mark=","), " cas")
        
      })
      
      # deaths 
      output$death_count <- reactive({
        count_death <- df_agreg %>% filter(date %in% as.Date(input$plot_date)) %>% 
          select(deaths)
        
        paste0(prettyNum(count_death, big.mark=","), " morts")
      })
      
      # nb pays 
      output$country_count <- reactive({
        #count_country <-length(unique(data_sf$Country))   
        nb_country <- map() %>% filter(cases>0) %>% 
          select(jhu_ID) %>% 
          dim()
        
        paste0(prettyNum(nb_country[1], big.mark=","), " pays comptabilisés")
      })
      
    }
    
  )
}