
franceUI <- function(id, date_in){
  ns <- NS(id)
  
  
  fluidRow(
    column( width = 3,
            sidebarPanel(width = 12,
                         # Cas
                         fluidRow(
                           span(tags$i(h3("COVID-19 - France", align = "center"))),
                           h6(textOutput(ns("date_react_fra")), align = "center"),
                           valueBoxOutput(ns("tot_cas"), width = 6),
                           valueBoxOutput(ns("tot_deces"), width = 6)
                         ),
                         
                         # Hospitalisations
                         fluidRow(
                           span(tags$i(h3("Hospitalisations", align = "center"))),
                           valueBoxOutput(ns("tot_hospi"), width = 6),
                           valueBoxOutput(ns("tot_rea"), width = 6)
                         ),
                         # ehpad
                         fluidRow(
                           span(tags$i(h3("Ehpad", align = "center"))),
                           valueBoxOutput(ns("tot_ehpad"), width = 6),
                           valueBoxOutput(ns("tot_dece_ehpad"), width = 6)
                         ),
                         
                         # select date
                         fluidRow(
                           column(width = 12,
                                  sliderInput(ns("slider_date"),
                                              label = h5("S\u00e9lection de la date"),
                                              min = as.Date(date_in[1],"%Y-%m-%d"),
                                              max = as.Date(date_in[2]),
                                              value = as.Date(date_in[2]),
                                              timeFormat = "%d %b",
                                              animate=animationOptions(interval = 4000, loop = FALSE)
                                  )
                           )
                         ),
                         fluidRow(
                           column(12,
                                  plotOutput(ns("new_hosp_fr"), height="130px", width="100%"),
                                  plotOutput(ns("cas_plot_fr"), height="130px", width="100%"),
                           )
                         )
                         
            ) # fin sliderbar gauche
    )
    
    ,
    column( width = 7,
            # carte France ------------------------------------------------------
            box(title = "France",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                width = NULL,
                leafletOutput(ns("map_france"), height = 500)
            ),
            column(width = 12, 
                   fluidRow(
                     # carte REUNION------------------------------
                     box(title = "R\u00e9union",
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 4,
                         leafletOutput(ns("map_reu"), height = 200)
                     ),
                     # carte Guyane ------------------------------
                     box(title = "Guyane",
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 4,
                         leafletOutput(ns("map_guyane"), height = 200)
                     ),
                     # carte Martinique ------------------------------
                     box(title = "Martinique",
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 4,
                         leafletOutput(ns("map_martini"), height = 200)
                     )
                   ),
                   fluidRow(
                     column(12, offset = 2,
                            # carte Guadeloupe ------------------------------
                            box(title = "Guadeloupe",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                width = 4,
                                leafletOutput(ns("map_guada"), height = 200)
                            ),
                            # carte Mayotte ------------------------------
                            box(title = "Mayotte",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                width = 4,
                                leafletOutput(ns("map_mayo"), height = 200)
                            )
                     )
                   )
                   
                   
            )
            
            
    ) # fin colonnes des map
    
    
    
  )
  
  
}

fraServer <- function(id, df_full, bmap_fr, in_mapgeojson, in_region_gps, fct_pal, col_hospi, col_glob_fra, 
                      bmap_run, bmap_guy, bmap_mar, bmp_gua, bmp_mayo){
  moduleServer(
    id,
    function(input, output, session){
      
      # partie gauche chiffres
      # data reactiv global FR ----
      # inclure min date opencovid 
      global_df <- reactive({
        # selection d'une journee
        df <- df_full %>% filter(date %in% input$slider_date) %>% #input react
          filter(source_type %in% "ministere-sante") %>% 
          filter(granularite %in% "pays")
        df
      })
      
      # global chiffres ----
      output$tot_cas <- renderValueBox({
        res <- global_df()$cas_confirmes
        res %>% 
          prettyNum(., big.mark=",") %>% 
          valueBox(
            value = tags$p(., style = "font-size: 80%;"),
            subtitle = tags$p("Cas confirm\u00e9s", style = "font-size: 80%;"),
            icon = icon("fas fa-user-check"),
            color = "orange"
          ) 
      })
      
      output$tot_deces <- renderValueBox({
        dece <- sum(global_df()$deces, global_df()$deces_ehpad, na.rm = TRUE)
        dece %>% 
          prettyNum(., big.mark=",") %>% 
          valueBox(
            value = tags$p(., style = "font-size: 80%;"),
            subtitle = tags$p("D\u00e9c\u00e8s", style = "font-size: 80%;"),
            icon = icon("fas fa-user-check"),
            color = "orange"
          )
      })
      
      output$tot_hospi <- renderValueBox({
        res <- global_df()$hospitalises
        res %>% 
          prettyNum(., big.mark=",") %>% 
          valueBox(
            value = tags$p(., style = "font-size: 80%;"),
            subtitle = tags$p("Hospitalis\u00e9s", style = "font-size: 80%;"),
            icon = icon("fas fa-user-check"),
            color = "orange"
          )
      })
      
      output$tot_rea <- renderValueBox({
        res <- global_df()$reanimation
        res %>% 
          prettyNum(., big.mark=",") %>% 
          valueBox(
            value = tags$p(., style = "font-size: 80%;"),
            subtitle = tags$p("R\u00e9animations", style = "font-size: 80%;"),
            icon = icon("fas fa-user-check"),
            color = "orange"
          )
      })
      
      output$tot_ehpad <- renderValueBox({
        res <- global_df()$cas_confirmes_ehpad
        res %>% 
          prettyNum(., big.mark=",") %>% 
          valueBox(
            value = tags$p(., style = "font-size: 80%;"),
            subtitle = tags$p("Cas ehpad", style = "font-size: 80%;"),
            icon = icon("fas fa-user-check"),
            color = "orange"
          )
      })
      
      output$tot_dece_ehpad <- renderValueBox({
        res <- global_df()$deces_ehpad
        res %>% 
          prettyNum(., big.mark=",") %>% 
          valueBox(
            value = tags$p(., style = "font-size: 80%;"),
            subtitle = tags$p("D\u00e9c\u00e8s ehpad", style = "font-size: 80%;"),
            icon = icon("fas fa-user-check"),
            color = "orange"
          )
      })
      
      output$cas_plot_fr <- renderPlot({
        var <- sym("cas_confirmes")
        df <- df_full %>% 
          filter(granularite %in% "pays")
        
        cumulative_plot(df, input$slider_date, var, "cas confirmes", col_hospi)
      })
      
      output$new_hosp_fr <- renderPlot({
        var <- sym("nouvelles_hospitalisations")
        df <- df_full %>% 
          filter(granularite %in% "pays") %>% 
          filter(source_type %in% "opencovid19-fr") 
        
        new_cases_plot(df, input$slider_date, var, "nouvelles hospitalisations", col_hospi)
      })
      
      
      # FIN partie gauche chiffres
      
      # region reactiv ----------------
      region_df <- reactive({
        
        # selection d'une journee
        
        # merge des regions (y compris global france)
        df_region <- merge(df_full, in_region_gps, by = "maille_code")
        names(df_region)[grepl("region.x", names(df_region))] <- "region"
        
        # a date
        df_region_a_date <- df_region %>% filter(date %in% input$slider_date) #input react 
        
        # ordonner les deux fichiers, meme ordre code region 
        df_region_a_date <- df_region_a_date[match(in_mapgeojson@data[["code"]], df_region_a_date$code), ]
        
        # remise accent pour nom REUNION
        df_region_a_date$region[df_region_a_date$region %in% "La_Reunion"] <- "La R\u00e9union"
        
        df_region_a_date
      })
      
      # france reactiv ---------------
      fra_df <- reactive({
        df <- df_full %>% filter(date %in% input$slider_date) %>% #input react
          filter(source_type %in% "ministere-sante") %>% 
          filter(granularite %in% "pays")
        
        # debug df null => remplace par 0
        if(dim(df)[1]==0){
          df <- df_full %>% 
            filter(region %in% "France" & source_type %in% "ministere-sante") %>% 
            filter(date %in% as.Date("2020-04-04"))
          df[,] <- 0
          df$longitude <- 0
          df$latitude <- 0
        }else{
          # merge des regions (y compris global france)
          df <- merge(df, in_region_gps, by = "maille_code")
          names(df)[grepl("region.x", names(df))] <- "region"
        }
        df
        
      })
      
      # REUNION reactiv ---------------
      run_df <- reactive({
        df <- df_full %>% filter(date %in% input$slider_date) %>% #input react
          filter(region %in% "La_Reunion") %>% 
          mutate(maille_code = "REG-04")
        
        # debug df null => remplace par 0
        if(dim(df)[1]==0){
          df <- df_full %>% 
            filter(region %in% "La_Reunion") %>% 
            filter(date %in% as.Date("2020-04-04"))
          df[,] <- 0
          df$longitude <- 0
          df$latitude <- 0
        }else{
          # merge des regions (pour coord gps)
          df <- merge(df, in_region_gps, by = "maille_code")
          names(df)[grepl("region.x", names(df))] <- "region"
          df <- df %>% mutate(latitude = latitude + 0.05,
                              longitude = longitude + 0.05,
                              region = "La R\u00e9union")
        }
        df
      })
      
      # Guyane reactiv ------------------------
      guy_df <- reactive({
        df <- df_full %>% filter(date %in% input$slider_date) %>% #input react
          filter(region %in% "Guyane")%>% 
          mutate(maille_code = "REG-03")
        
        # debug df null => remplace par 0
        if(dim(df)[1]==0){
          df <- df_full %>% 
            filter(region %in% "Guyane") %>% 
            filter(date %in% as.Date("2020-04-04"))
          df[,] <- 0
          df$longitude <- 0
          df$latitude <- 0
        }else{
          # merge des regions (pour coord gps)
          df <- merge(df, in_region_gps, by = "maille_code")
          names(df)[grepl("region.x", names(df))] <- "region"
          df <- df %>% mutate(latitude = latitude + 0.3,
                              longitude = longitude + 0.3)
        }
        df
      })
      
      # Martinique reactiv ------------------------
      mar_df <- reactive({
        df <- df_full %>% filter(date %in% input$slider_date) %>% #input react
          filter(region %in% "Martinique")%>% 
          mutate(maille_code = "REG-02")
        
        # debug df null => remplace par 0
        if(dim(df)[1]==0){
          df <- df_full %>% 
            filter(region %in% "Martinique") %>% 
            filter(date %in% as.Date("2020-04-04"))
          df[,] <- 0
          df$longitude <- 0
          df$latitude <- 0
        }else{
          # merge des regions (pour coord gps)
          df <- merge(df, in_region_gps, by = "maille_code")
          names(df)[grepl("region.x", names(df))] <- "region"
          df <- df %>% mutate(latitude = latitude + 0.05,
                              longitude = longitude + 0.05)
        }
        df
      })
      
      # Guadeloupe reactiv ------------------------
      gua_df <- reactive({
        df <- df_full %>% filter(date %in% input$slider_date) %>% #input react
          filter(region %in% "Guadeloupe")%>% 
          mutate(maille_code = "REG-01")
        
        # debug df null => remplace par 0
        if(dim(df)[1]==0){
          df <- df_full %>% 
            filter(region %in% "Guadeloupe") %>% 
            filter(date %in% as.Date("2020-04-04"))
          df[,] <- 0
          df$longitude <- 0
          df$latitude <- 0
        }else{
          # merge des regions (pour coord gps)
          df <- merge(df, in_region_gps, by = "maille_code")
          names(df)[grepl("region.x", names(df))] <- "region"
          df <- df %>% mutate(latitude = latitude + 0.05,
                              longitude = longitude + 0.05)
        }
        df
      })
      
      # Mayotte reactiv ------------------------
      mayo_df <- reactive({
        df <- df_full %>% filter(date %in% input$slider_date) %>% #input react
          filter(region %in% "Mayotte")%>% 
          mutate(maille_code = "REG-06")
        
        # debug df null => remplace par 0
        if(dim(df)[1]==0){
          df <- df_full %>% 
            filter(region %in% "Mayotte") %>% 
            filter(date %in% as.Date("2020-04-04"))
          df[,] <- 0
          df$longitude <- 0
          df$latitude <- 0
        }else{
          # merge des regions (pour coord gps)
          df <- merge(df, in_region_gps, by = "maille_code")
          names(df)[grepl("region.x", names(df))] <- "region"
          df <- df %>% mutate(latitude = latitude + 0.05,
                              longitude = longitude + 0.05)
        }
        df
      })
      
      
      # plot map -----------------------------------------
      # base map fr --------------------------
      # base map + carte par defaut sur input sinon map vide (bug ?)
      output$map_france = renderLeaflet({
        label_bilan <- "<strong>%s (bilan \u00e0 date)</strong><br/>Hospitalis\u00e9s: %d
                                           <br/>R\u00e9animations: %d<br/>Nouvelles hospitalisations: %d
                                           <br/>Nouvelles r\u00e9animations: %d"
        label_cumul <- "<strong>%s (cumul \u00e0 date)</strong><br/>Cas: %d<br/>Deces (+Ehpad): %d<br/>Gu\u00e9risons: %d"
        
        bmap_fr %>%
          addPolygons(data = in_mapgeojson, stroke = TRUE, weight = 1 , smoothFactor = 0.2, fillOpacity = 0.6,
                      fillColor = fct_pal(region_df()$deces), label = in_mapgeojson$nom) %>%
          addCircleMarkers(data = region_df(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(hospitalises)^(1/3),
                           fillOpacity = 0.2, color = col_hospi,
                           label = sprintf(label_bilan,
                                           region_df()$region, region_df()$hospitalises,
                                           region_df()$reanimation, region_df()$nouvelles_hospitalisations,
                                           region_df()$nouvelles_reanimations) %>%
                             lapply(htmltools::HTML),
                           labelOptions = labelOptions(
                             style = list("font-weight" = "normal", padding = "3px 8px", "color" = col_hospi),
                             textsize = "15px", direction = "auto"))%>% 
          addCircleMarkers(data = fra_df(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cas_confirmes)^(1/4),
                           fillOpacity = 0.2, color = col_glob_fra, 
                           label = sprintf(label_cumul,
                                           fra_df()$region, fra_df()$cas_confirmes, 
                                           sum(fra_df()$deces, fra_df()$deces_ehpad, na.rm = TRUE), 
                                           fra_df()$gueris.x) %>%
                             lapply(htmltools::HTML),
                           labelOptions = labelOptions(
                             style = list("font-weight" = "normal", padding = "3px 8px", "color" = col_glob_fra),
                             textsize = "15px", direction = "auto"))
      })
      
      # base map REUNION ------------------------------------------------------------------------------------------------
      output$map_reu <- renderLeaflet({
        label_bilan <- "<strong>%s (bilan \u00e0 date)</strong><br/>Hospitalis\u00e9s: %d
                                           <br/>R\u00e9animations: %d<br/>Nouvelles hospitalisations: %d
                                           <br/>Nouvelles r\u00e9animations: %d"
        label_cumul <- "<strong>%s (cumul \u00e0 date)</strong><br/>Cas: %d<br/>Deces: %d<br/>Gu\u00e9risons: %d"
        
        bmap_run %>%
          addPolygons(data = in_mapgeojson, stroke = TRUE, weight = 1 , smoothFactor = 0.2, fillOpacity = 0.6,
                      fillColor = fct_pal(region_df()$deces), label = in_mapgeojson$nom) %>%
          addCircleMarkers(data = region_df(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(hospitalises)^(1/3),
                           fillOpacity = 0.2, color = col_hospi,
                           label = sprintf(label_bilan,
                                           region_df()$region, region_df()$hospitalises,
                                           region_df()$reanimation, region_df()$nouvelles_hospitalisations,
                                           region_df()$nouvelles_reanimations) %>%
                             lapply(htmltools::HTML),
                           labelOptions = labelOptions(
                             style = list("font-weight" = "normal", padding = "3px 8px", "color" = col_hospi),
                             textsize = "15px", direction = "auto")) %>%
          addCircleMarkers(data = run_df(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cas)^(1/4),
                           fillOpacity = 0.2, color = col_glob_fra,
                           label = sprintf(label_cumul,
                                           run_df()$region, run_df()$cas, 
                                           run_df()$morts, 
                                           run_df()$gueris.y) %>%
                             lapply(htmltools::HTML),
                           labelOptions = labelOptions(
                             style = list("font-weight" = "normal", padding = "3px 8px", "color" = col_glob_fra),
                             textsize = "15px", direction = "auto"))
        
      })
      
      # # base map GUYANE ------------------------------------------------------
      output$map_guyane <- renderLeaflet({
        label_bilan <- "<strong>%s (bilan \u00e0 date)</strong><br/>Hospitalis\u00e9s: %d
                                           <br/>R\u00e9animations: %d<br/>Nouvelles hospitalisations: %d
                                           <br/>Nouvelles r\u00e9animations: %d"
        label_cumul <- "<strong>%s (cumul \u00e0 date)</strong><br/>Cas: %d<br/>Deces: %d<br/>Gu\u00e9risons: %d"
        
        bmap_guy %>%
          addPolygons(data = in_mapgeojson, stroke = TRUE, weight = 1 , smoothFactor = 0.2, fillOpacity = 0.6,
                      fillColor = fct_pal(region_df()$deces), label = in_mapgeojson$nom) %>%
          addCircleMarkers(data = region_df(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(hospitalises)^(1/3),
                           fillOpacity = 0.2, color = col_hospi,
                           label = sprintf(label_bilan,
                                           region_df()$region, region_df()$hospitalises,
                                           region_df()$reanimation, region_df()$nouvelles_hospitalisations,
                                           region_df()$nouvelles_reanimations) %>%
                             lapply(htmltools::HTML),
                           labelOptions = labelOptions(
                             style = list("font-weight" = "normal", padding = "3px 8px", "color" = col_hospi),
                             textsize = "15px", direction = "auto")) %>%
          addCircleMarkers(data = guy_df(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cas)^(1/4),
                           fillOpacity = 0.2, color = col_glob_fra,
                           label = sprintf(label_cumul,
                                           guy_df()$region, guy_df()$cas, 
                                           guy_df()$morts, 
                                           guy_df()$gueris.y) %>%
                             lapply(htmltools::HTML),
                           labelOptions = labelOptions(
                             style = list("font-weight" = "normal", padding = "3px 8px", "color" = col_glob_fra),
                             textsize = "15px", direction = "auto"))
      })
      
      # base map Martinique ---------------------------------------------------------------
      output$map_martini <- renderLeaflet({
        label_bilan <- "<strong>%s (bilan \u00e0 date)</strong><br/>Hospitalis\u00e9s: %d
                                           <br/>R\u00e9animations: %d<br/>Nouvelles hospitalisations: %d
                                           <br/>Nouvelles r\u00e9animations: %d"
        label_cumul <- "<strong>%s (cumul \u00e0 date)</strong><br/>Cas: %d<br/>Deces: %d<br/>Gu\u00e9risons: %d"
        
        bmap_mar %>%
          addPolygons(data = in_mapgeojson, stroke = TRUE, weight = 1 , smoothFactor = 0.2, fillOpacity = 0.6,
                      fillColor = fct_pal(region_df()$deces), label = in_mapgeojson$nom) %>%
          addCircleMarkers(data = region_df(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(hospitalises)^(1/3),
                           fillOpacity = 0.2, color = col_hospi,
                           label = sprintf(label_bilan, 
                                           region_df()$region, region_df()$hospitalises,
                                           region_df()$reanimation, region_df()$nouvelles_hospitalisations,
                                           region_df()$nouvelles_reanimations) %>%
                             lapply(htmltools::HTML),
                           labelOptions = labelOptions(
                             style = list("font-weight" = "normal", padding = "3px 8px", "color" = col_hospi),
                             textsize = "15px", direction = "auto"))%>%
          addCircleMarkers(data = mar_df(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cas)^(1/4),
                           fillOpacity = 0.2, color = col_glob_fra,
                           label = sprintf(label_cumul,
                                           mar_df()$region, mar_df()$cas, 
                                           mar_df()$morts, 
                                           mar_df()$gueris.y) %>%
                             lapply(htmltools::HTML),
                           labelOptions = labelOptions(
                             style = list("font-weight" = "normal", padding = "3px 8px", "color" = col_glob_fra),
                             textsize = "15px", direction = "auto"))
        
      })
      
      # base map Guadeloupe --------------------------------------
      output$map_guada <- renderLeaflet({
        label_bilan <- "<strong>%s (bilan \u00e0 date)</strong><br/>Hospitalis\u00e9s: %d
                                           <br/>R\u00e9animations: %d<br/>Nouvelles hospitalisations: %d
                                           <br/>Nouvelles r\u00e9animations: %d"
        label_cumul <- "<strong>%s (cumul \u00e0 date)</strong><br/>Cas: %d<br/>Deces: %d<br/>Gu\u00e9risons: %d"
        
        bmp_gua %>%
          addPolygons(data = in_mapgeojson, stroke = TRUE, weight = 1 , smoothFactor = 0.2, fillOpacity = 0.6,
                      fillColor = fct_pal(region_df()$deces), label = in_mapgeojson$nom) %>%
          addCircleMarkers(data = region_df(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(hospitalises)^(1/3),
                           fillOpacity = 0.2, color = col_hospi,
                           label = sprintf(label_bilan, 
                                           region_df()$region, region_df()$hospitalises,
                                           region_df()$reanimation, region_df()$nouvelles_hospitalisations,
                                           region_df()$nouvelles_reanimations) %>%
                             lapply(htmltools::HTML),
                           labelOptions = labelOptions(
                             style = list("font-weight" = "normal", padding = "3px 8px", "color" = col_hospi),
                             textsize = "15px", direction = "auto"))%>%
          addCircleMarkers(data = gua_df(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cas)^(1/4),
                           fillOpacity = 0.2, color = col_glob_fra,
                           label = sprintf(label_cumul,
                                           gua_df()$region, gua_df()$cas,
                                           gua_df()$morts,
                                           gua_df()$gueris.y) %>%
                             lapply(htmltools::HTML),
                           labelOptions = labelOptions(
                             style = list("font-weight" = "normal", padding = "3px 8px", "color" = col_glob_fra),
                             textsize = "15px", direction = "auto"))
      })
      
      # base map Mayotte ---------------------------------- 
      output$map_mayo <- renderLeaflet({
        label_bilan <- "<strong>%s (bilan \u00e0 date)</strong><br/>Hospitalis\u00e9s: %d
                                           <br/>R\u00e9animations: %d<br/>Nouvelles hospitalisations: %d
                                           <br/>Nouvelles r\u00e9animations: %d"
        label_cumul <- "<strong>%s (cumul \u00e0 date)</strong><br/>Cas: %d<br/>Deces: %d<br/>Gu\u00e9risons: %d"
        
        bmp_mayo %>%
          addPolygons(data = in_mapgeojson, stroke = TRUE, weight = 1 , smoothFactor = 0.2, fillOpacity = 0.6,
                      fillColor = fct_pal(region_df()$deces), label = in_mapgeojson$nom) %>%
          addCircleMarkers(data = region_df(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(hospitalises)^(1/3),
                           fillOpacity = 0.2, color = col_hospi,
                           label = sprintf(label_bilan,
                                           region_df()$region, region_df()$hospitalises,
                                           region_df()$reanimation, region_df()$nouvelles_hospitalisations,
                                           region_df()$nouvelles_reanimations) %>%
                             lapply(htmltools::HTML),
                           labelOptions = labelOptions(
                             style = list("font-weight" = "normal", padding = "3px 8px", "color" = col_hospi),
                             textsize = "15px", direction = "auto"))%>%
          addCircleMarkers(data = mayo_df(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cas)^(1/4),
                           fillOpacity = 0.2, color = col_glob_fra,
                           label = sprintf(label_cumul,
                                           mayo_df()$region, mayo_df()$cas,
                                           mayo_df()$morts,
                                           mayo_df()$gueris.y) %>%
                             lapply(htmltools::HTML),
                           labelOptions = labelOptions(
                             style = list("font-weight" = "normal", padding = "3px 8px", "color" = col_glob_fra),
                             textsize = "15px", direction = "auto"))
        
      })
      
      # ==pas besoin observeEvent si pas de case a cocher ou d'element sur la map==
      
      # date du slider 
      output$date_react_fra <- renderText({
        format(as.POSIXct(input$slider_date),"%d %B %Y")
      })
      
    }
    
    
  )
}