

# onglet decoupage par pays 

paysUI <- function(id, dates_param, df){
  
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      
      span(tags$i(h6("Evolution comparative ou par pays du nombre de cas."))), #, style="color:#045a8d"
      span(tags$i(h6("Choix du niveau et de l'indicateur"))),
      
      pickerInput(ns("level_select"), "Niveau:",   
                  choices = c("Global", "Continent", "Pays"), 
                  selected = c("Pays"),
                  multiple = FALSE),
      
      pickerInput(ns("region_select"), "Continent/Pays:",   
                  choices = as.character(df[order(-df$cases),]$country), 
                  options = list(`actions-box` = TRUE, `none-selected-text` = "Faites votre sélection!"),
                  selected = df$country,
                  multiple = TRUE), 
      
      pickerInput(ns("indicateur_select"), "Indicateur:",   
                  choices = c("Cas (total)", "Morts (total)", "Cas pour 100,000", "Morts pour 100,000"), 
                  selected = c("Cas (total)"),
                  multiple = FALSE),
      
      
      sliderInput(ns("minimum_date"),
                  "Date de départ:",
                  min = as.Date(dates_param[1]),
                  max = as.Date(dates_param[2]),
                  value= as.Date(dates_param[1]),
                  timeFormat="%d %b"),
      
      "Faites votre choix de niveau, d'indicateur et actualisez les graphiques à partir d'une date de départ."
    ),
    
    mainPanel(
      # gestion messages d'erreurs shiny pour actualisation des graphs ----------------
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      tabsetPanel(
        tabPanel("Cumul", plotlyOutput(ns("country_plot_cumulative"))),
        tabPanel("Bilan \u00e0 date", plotlyOutput(ns("country_plot"))),
        tabPanel("Cumul (log10)", plotlyOutput(ns("country_plot_cumulative_log")))
      ),
      tags$br(), tags$br(),
      gt_output(outputId = ns("gt_table"))
    )
  )
  
}


paysServer <- function(id, dates_param, df, df_countries, df_large, col_pays){
  moduleServer(
    id,
    function(input, output, session){
      
      
      # data reactiv avec toutes les stats ----
      data_plot <- reactive({

        # selection d'une journee
        df_min_day <- df %>% filter(date >= as.Date(input$minimum_date)) # input
        
        # jointure avec countries
        names(df_min_day)[1] <- "jhu_ID" # pour jointure
        df_min_day <- merge(df_min_day, df_countries, by = "jhu_ID")
        
        # selection des pays pour map
        # df_min_day <- df_min_day %>% filter(alpha3 %in% worldcountry$ADM0_A3)
        
        df_min_day <- df_min_day[order(df_min_day$alpha3), ] # meme ordre de population 
        # indicateur cas/population/100k (coloriage map)
        
        # df_min_day <- add_map_stats(df_min_day)
        df_min_day
        
      })
      
      # recalcul indicateur new_cases ----
      df_stats2 <- reactive({
        pays <- df %>% add_stats2() 
        pays <- pays %>% filter(date >= as.Date(input$minimum_date)) 
        
        # ajout data population
        names(pays)[1] <- "jhu_ID" # pour jointure
        pays <- merge(pays, df_countries, by = "jhu_ID")
        pays
        })
      

      
      # pays par defaut ---- 
      # defaut    
      
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = as.character(df_large[order(-df_large$cases),]$country), 
                        selected = df_large[order(-df_large$cases),]$country[1:10])
      
      
      # data reactiv pays avec popuplation 
      pays_reactive_pop <- reactive({
        pays_agreg_pop <- data_plot() %>% select(population, country, date, 
                                                 cases, deaths, new_cases, new_deaths) %>%
          filter(!(population %in% NA)) %>% 
          filter(country %in% input$region_select) %>% 
          mutate(region=country)
        
        pays_agreg_pop <- add_map_stats(pays_agreg_pop)
        pays_agreg_pop
      }) 
      
      # onglet cumul
      
      output$country_plot_cumulative <- renderPlotly({
        # indicateurs
        if(input$indicateur_select=="Cas (total)"){
          num_var <- sym("cases")
          cumul_plotly(pays_reactive_pop(), num_var, "Cas", col_pays)
        }else if(input$indicateur_select=="Morts (total)"){
          num_var <- sym("deaths")
          cumul_plotly(pays_reactive_pop(), num_var, "D\u00e9c\u00e8s", col_pays)
        }else if(input$indicateur_select=="Cas pour 100,000"){ # avec population
          num_var <- sym("per100k")
          cumul_plotly(pays_reactive_pop(), num_var, "Cas pour 100k", col_pays)
        }else{ # avec population
          num_var <- sym("deathsper100k")
          cumul_plotly(pays_reactive_pop(), num_var, "D\u00e9c\u00e8s pour 100k", col_pays)
        }
        
      })
      
      # onglet new cases
      output$country_plot <- renderPlotly({
        # indicateurs
        if(input$indicateur_select=="Cas (total)"){
          num_var <- sym("new_cases")
          cumul_new_plotly(pays_reactive_pop(), num_var, "Cas", col_pays)
        }else if(input$indicateur_select=="Morts (total)"){
          num_var <- sym("new_deaths")
          cumul_new_plotly(pays_reactive_pop(), num_var, "D\u00e9c\u00e8s", col_pays)
        }else if(input$indicateur_select=="Cas pour 100,000"){ # avec population
          num_var <- sym("newper100k")
          cumul_new_plotly(pays_reactive_pop(), num_var, "Cas pour 100k", col_pays)
        }else{ # avec population
          num_var <- sym("newdeathsper100k")
          cumul_new_plotly(pays_reactive_pop(), num_var, "D\u00e9c\u00e8s pour 100k", col_pays)
        }
        
      })
      
      # onglet cases log 10
      output$country_plot_cumulative_log <- renderPlotly({
        # indicateurs
        if(input$indicateur_select=="Cas (total)"){
          num_var <- sym("cases")
          cumul_plotly_log(pays_reactive_pop(), num_var, "Cas", col_pays)
        }else if(input$indicateur_select=="Morts (total)"){
          num_var <- sym("deaths")
          cumul_plotly_log(pays_reactive_pop(), num_var, "D\u00e9c\u00e8s", col_pays)
        }else if(input$indicateur_select=="Cas pour 100,000"){ # avec population
          num_var <- sym("per100k")
          cumul_plotly_log(pays_reactive_pop(), num_var, "Cas pour 100k", col_pays)
        }else{ # avec population
          num_var <- sym("deathsper100k")
          cumul_plotly_log(pays_reactive_pop(), num_var, "D\u00e9c\u00e8s pour 100k", col_pays)
        }
        
      })
      
      # fin pays par defaut
      
      # gt tab pays defaut ----
      gt_tab_pays <- reactive({
        

        # selection des colonnes et pays
        df_pop <- df_stats2() %>% 
          select(country, date, population, new_cases, new_deaths, continent_level) %>% 
          filter(country %in% input$region_select) 
        
        pays <- df_pop %>% 
          select(-c(continent_level)) %>% 
          mutate(date = as.Date(input$minimum_date)) %>% 
          group_by(date, country, population) %>% 
          summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
          mutate(
            cases = new_cases,
            deaths = new_deaths
          ) %>% 
          add_map_stats() %>% 
          select(country,	cases,	deaths,	per100k,	deathsper100k,	date) %>% 
          ungroup %>%
          arrange(desc(cases)) %>%    
          do_gt_table("pays")
        
        pays
      })
     
      
      output$gt_table <- render_gt(
          expr = gt_tab_pays()
          # ,
          # height = px(600),
          # width = px(600)
        )
      
      
      
  # ObserEvent selon selection Niveau ----
  observeEvent(input$level_select, {
    # global ----
    if(input$level_select=="Global"){
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = "Global", selected = "Global")
      
      
      # data reactiv global avec popuplation 
      global_reactive_pop <- reactive({
        glob_agreg_pop <- data_plot() %>% select(population, date, cases, deaths, new_cases, new_deaths) 
       
        glob_agreg_pop$population[glob_agreg_pop$population %in% NA] = 0
        
        glob_agreg_pop <- glob_agreg_pop %>%  
          group_by(date) %>% summarise_each(funs(sum)) 

        glob_agreg_pop <- add_map_stats(glob_agreg_pop) %>% 
          mutate(region="Global")
        
        glob_agreg_pop
      }) 
      
      # gt tab global ----
      gt_tab_global <- reactive({
        
        # selection des colonnes
        df_pop <- df_stats2() %>% 
          select(country, date, population, new_cases, new_deaths, continent_level) 
        
        df_agreg_pop_glob <- df_pop %>% select(-c(continent_level)) %>% 
          mutate(date = as.Date(input$minimum_date)) %>% 
          group_by(date, country, population) %>% 
          summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
          mutate(
            cases = new_cases,
            deaths = new_deaths
          ) %>% 
          select(date, population, cases, deaths, new_cases, new_deaths) %>% 
          group_by(date) %>% 
          summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
          add_map_stats() %>% 
          select(cases, deaths, per100k, deathsper100k, date) %>% 
          arrange(desc(cases)) %>%
          do_gt_table("globales")
        
        df_agreg_pop_glob
      })
      
      
      output$gt_table <- render_gt(
        expr = gt_tab_global()
        # ,
        # height = px(600),
        # width = px(600)
      )
      
      # onglet cumul
      output$country_plot_cumulative <- renderPlotly({
       # indicateurs
        if(input$indicateur_select=="Cas (total)"){
          num_var <- sym("cases")
          cumul_plotly(global_reactive_pop(), num_var, "Cas",col_pays)
        }else if(input$indicateur_select=="Morts (total)"){
          num_var <- sym("deaths")
          cumul_plotly(global_reactive_pop(), num_var, "D\u00e9c\u00e8s",col_pays)
        }else if(input$indicateur_select=="Cas pour 100,000"){ # avec population
          num_var <- sym("per100k")
          cumul_plotly(global_reactive_pop(), num_var, "Cas pour 100k",col_pays)
        }else{ # avec population
          num_var <- sym("deathsper100k")
          cumul_plotly(global_reactive_pop(), num_var, "D\u00e9c\u00e8s pour 100k",col_pays)
        }
        
      })
      
      # onglet new cases
      output$country_plot <- renderPlotly({
        # indicateurs
        if(input$indicateur_select=="Cas (total)"){
          num_var <- sym("new_cases")
          cumul_new_plotly(global_reactive_pop(), num_var, "Cas",col_pays)
        }else if(input$indicateur_select=="Morts (total)"){
          num_var <- sym("new_deaths")
          cumul_new_plotly(global_reactive_pop(), num_var, "D\u00e9c\u00e8s", col_pays)
        }else if(input$indicateur_select=="Cas pour 100,000"){ # avec population
          num_var <- sym("newper100k")
          cumul_new_plotly(global_reactive_pop(), num_var, "Cas pour 100k", col_pays)
        }else{ # avec population
          num_var <- sym("newdeathsper100k")
          cumul_new_plotly(global_reactive_pop(), num_var, "D\u00e9c\u00e8s pour 100k", col_pays)
        }
        
      })
      # onglet cases log 10 
      output$country_plot_cumulative_log <- renderPlotly({
        # indicateurs
        if(input$indicateur_select=="Cas (total)"){
          num_var <- sym("cases")
          cumul_plotly_log(global_reactive_pop(), num_var, "Cas", col_pays)
        }else if(input$indicateur_select=="Morts (total)"){
          num_var <- sym("deaths")
          cumul_plotly_log(global_reactive_pop(), num_var, "D\u00e9c\u00e8s", col_pays)
        }else if(input$indicateur_select=="Cas pour 100,000"){ # avec population
          num_var <- sym("per100k")
          cumul_plotly_log(global_reactive_pop(), num_var, "Cas pour 100k", col_pays)
        }else{ # avec population
          num_var <- sym("deathsper100k")
          cumul_plotly_log(global_reactive_pop(), num_var, "D\u00e9c\u00e8s pour 100k", col_pays)
        }
        
      })
      
    }
    # continent ----
    if (input$level_select=="Continent") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America"), 
                        selected = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America"))
      
      # data reactiv continent avec popuplation 
      conti_reactive_pop <- reactive({
        conti_agreg_pop <- data_plot() %>% select(population, continent_level, date, 
                                                  cases, deaths, new_cases, new_deaths) %>%
          filter(!(population %in% NA)) %>%
          filter(continent_level %in% input$region_select) %>% 
          group_by(date, continent_level) %>% 
          summarise_each(funs(sum)) %>% 
          mutate(region=continent_level)
        
        conti_agreg_pop <- add_map_stats(conti_agreg_pop)
        conti_agreg_pop
      }) 
      
      # onglet cumul
      output$country_plot_cumulative <- renderPlotly({
        # indicateurs
        if(input$indicateur_select=="Cas (total)"){
          num_var <- sym("cases")
          cumul_plotly(conti_reactive_pop(), num_var, "Cas",col_pays)
        }else if(input$indicateur_select=="Morts (total)"){
          num_var <- sym("deaths")
          cumul_plotly(conti_reactive_pop(), num_var, "D\u00e9c\u00e8s", col_pays)
        }else if(input$indicateur_select=="Cas pour 100,000"){ # avec population
          num_var <- sym("per100k")
          cumul_plotly(conti_reactive_pop(), num_var, "Cas pour 100k", col_pays)
        }else{ # avec population
          num_var <- sym("deathsper100k")
          cumul_plotly(conti_reactive_pop(), num_var, "D\u00e9c\u00e8s pour 100k", col_pays)
        }
        
      })
      
      # onglet new cases
      output$country_plot <- renderPlotly({
        # indicateurs
        if(input$indicateur_select=="Cas (total)"){
          num_var <- sym("new_cases")
          cumul_new_plotly(conti_reactive_pop(), num_var, "Cas", col_pays)
        }else if(input$indicateur_select=="Morts (total)"){
          num_var <- sym("new_deaths")
          cumul_new_plotly(conti_reactive_pop(), num_var, "D\u00e9c\u00e8s", col_pays)
        }else if(input$indicateur_select=="Cas pour 100,000"){ # avec population
          num_var <- sym("newper100k")
          cumul_new_plotly(conti_reactive_pop(), num_var, "Cas pour 100k", col_pays)
        }else{ # avec population
          num_var <- sym("newdeathsper100k")
          cumul_new_plotly(conti_reactive_pop(), num_var, "D\u00e9c\u00e8s pour 100k", col_pays)
        }
        
      })
      
      # onglet cases log 10 
      output$country_plot_cumulative_log <- renderPlotly({
        # indicateurs
        if(input$indicateur_select=="Cas (total)"){
          num_var <- sym("cases")
          cumul_plotly_log(conti_reactive_pop(), num_var, "Cas", col_pays)
        }else if(input$indicateur_select=="Morts (total)"){
          num_var <- sym("deaths")
          cumul_plotly_log(conti_reactive_pop(), num_var, "D\u00e9c\u00e8s", col_pays)
        }else if(input$indicateur_select=="Cas pour 100,000"){ # avec population
          num_var <- sym("per100k")
          cumul_plotly_log(conti_reactive_pop(), num_var, "Cas pour 100k", col_pays)
        }else{ # avec population
          num_var <- sym("deathsper100k")
          cumul_plotly_log(conti_reactive_pop(), num_var, "D\u00e9c\u00e8s pour 100k", col_pays)
        }
        
      })
      
      # gt tab continent ----
      gt_tab_conti <- reactive({
        
        # selection des colonnes et continent
        df_pop <- df_stats2() %>% 
          select(country, date, population, new_cases, new_deaths, continent_level) %>% 
          filter(continent_level %in% input$region_select) 
        
        conti <- df_pop %>% 
          mutate(date = as.Date(input$minimum_date)) %>% 
          group_by(date, country, population, continent_level) %>% 
          summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
          mutate(
            cases = new_cases,
            deaths = new_deaths
          ) %>% 
          select(continent_level, date, population, cases, deaths, new_cases, new_deaths) %>% 
          group_by(continent_level, date) %>% 
          summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
          add_map_stats() %>% 
          select(continent_level, cases, deaths, per100k, deathsper100k, date) %>% 
          ungroup %>%
          arrange(desc(cases)) %>%
          do_gt_table("continents")
        
        conti
      })
     
      output$gt_table <- render_gt(
        expr = gt_tab_conti()
        # ,
        # height = px(600),
        # width = px(600)
      )
      
    }
    # pays ----
    if (input$level_select=="Pays") {
      updatePickerInput(session = session, inputId = "region_select",
                        choices = as.character(df_large[order(-df_large$cases),]$country),
                        selected = df_large[order(-df_large$cases),]$country[1:10])


      # onglet cumul
      output$country_plot_cumulative <- renderPlotly({
        # indicateurs
        if(input$indicateur_select=="Cas (total)"){
          num_var <- sym("cases")
          cumul_plotly(pays_reactive_pop(), num_var, "Cas", col_pays)
        }else if(input$indicateur_select=="Morts (total)"){
          num_var <- sym("deaths")
          cumul_plotly(pays_reactive_pop(), num_var, "D\u00e9c\u00e8s", col_pays)
        }else if(input$indicateur_select=="Cas pour 100,000"){ # avec population
          num_var <- sym("per100k")
          cumul_plotly(pays_reactive_pop(), num_var, "Cas pour 100k", col_pays)
        }else{ # avec population
          num_var <- sym("deathsper100k")
          cumul_plotly(pays_reactive_pop(), num_var, "D\u00e9c\u00e8s pour 100k", col_pays)
        }

      })

      # onglet new cases
      output$country_plot <- renderPlotly({
        # indicateurs
        if(input$indicateur_select=="Cas (total)"){
          num_var <- sym("new_cases")
          cumul_new_plotly(pays_reactive_pop(), num_var, "Cas", col_pays)
        }else if(input$indicateur_select=="Morts (total)"){
          num_var <- sym("new_deaths")
          cumul_new_plotly(pays_reactive_pop(), num_var, "D\u00e9c\u00e8s", col_pays)
        }else if(input$indicateur_select=="Cas pour 100,000"){ # avec population
          num_var <- sym("newper100k")
          cumul_new_plotly(pays_reactive_pop(), num_var, "Cas pour 100k", col_pays)
        }else{ # avec population
          num_var <- sym("newdeathsper100k")
          cumul_new_plotly(pays_reactive_pop(), num_var, "D\u00e9c\u00e8s pour 100k", col_pays)
        }

      })

      # onglet cases log 10
      output$country_plot_cumulative_log <- renderPlotly({
        # indicateurs
        if(input$indicateur_select=="Cas (total)"){
          num_var <- sym("cases")
          cumul_plotly_log(pays_reactive_pop(), num_var, "Cas", col_pays)
        }else if(input$indicateur_select=="Morts (total)"){
          num_var <- sym("deaths")
          cumul_plotly_log(pays_reactive_pop(), num_var, "D\u00e9c\u00e8s", col_pays)
        }else if(input$indicateur_select=="Cas pour 100,000"){ # avec population
          num_var <- sym("per100k")
          cumul_plotly_log(pays_reactive_pop(), num_var, "Cas pour 100k", col_pays)
        }else{ # avec population
          num_var <- sym("deathsper100k")
          cumul_plotly_log(pays_reactive_pop(), num_var, "D\u00e9c\u00e8s pour 100k", col_pays)
        }

      })
      
      output$gt_table <- render_gt(
        expr = gt_tab_pays()
        # ,
        # height = px(600),
        # width = px(600)
      )



     }
  }, # fin observevent
  ignoreInit = TRUE)
      
      
      
    }
  )# fin module
  
}

