# onglet regions/departements

regUI <- function(id, date_in){
  ns <- NS(id)
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        span(tags$i(h4("Statistiques pour la France, r\u00e9gions et d\u00e9partements", style="color:#045a8d"))),
        
        pickerInput(ns("niv_select"), "Niveau:",   
                    choices = c("Global", "R\u00e9gions", "D\u00e9partements"), 
                    selected = c("R\u00e9gions"),
                    multiple = FALSE),
        sliderInput(ns("id_slider_date"),
                    label = h5("S\u00e9lection de la date de d\u00e9part du comptage"),
                    min = as.Date(date_in[1],"%Y-%m-%d"),
                    max = as.Date(date_in[2]),
                    value = as.Date(date_in[1],"%Y-%m-%d"),
                    timeFormat = "%d %b"
        ),
        span(
          tags$i(
            h6(
              "Pour les r\u00e9gions (DOM), les donn\u00e9es (cas/d\u00e9c\u00e8s/gu\u00e9risons) ont \u00e9t\u00e9 consolid\u00e9es avec les donn\u00e9es de l'Universit\u00e9 Johns Hopkins (JHU CSSE) ."
              , 
              style="color:#045a8d")
          ))
      ),
      mainPanel(
        # gestion messages d'erreurs shiny pour aucun departement selectionne  ----------------
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        uiOutput(ns("tabsets_id")),
        
        
        
        tags$br(), tags$br(),
        gt_output(outputId = ns("gt_tab"))
      )
    )
  )
}



regServer <- function(id, data_full_fr, data_fr_glob_gt, data_reg_gt, colgraph, colreg, coldep, date_in) {
  
  moduleServer(
    id,
    function(input, output, session){
      
      # france globale react ---------------------
      global_france <- reactive({
        df <- data_full_fr %>% 
          filter(region %in% "France" & source_type %in% "ministere-sante") %>% 
          filter(date >= as.Date(input$id_slider_date) & date <= as.Date(date_in[2])) %>% 
          mutate(
            deces_tot = rowSums(.[,9:10], na.rm=TRUE)
          )
        
        # correction NA, et des valeurs de cumuls
        # correction du cumul uniquement appliquee sur les deces
        for(i in seq(length(df$date))){
          if(i>=4){
            # deces
            if( df$deces_tot[i] < mean(df$deces_tot[i:(i-4)], na.rm=TRUE) ){
              df$deces_tot[i] = NA
            }
          }
        }
        df
        
      })
      
      # france global gt table reactive ------------------
      # les donnees doivent etre recalculees sur des chiffre a date et non cumuls (nouveaux cas, ....)
      gt_glob_france <- reactive({
        
        df <- data_fr_glob_gt %>% 
          filter(date >= as.Date(input$id_slider_date) & date <= as.Date(date_in[2])) %>% 
          mutate(date = as.Date(input$id_slider_date)) %>% 
          group_by(date) %>% 
          summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
          select(cas, deces, gueris, nouvelles_hospitalisations, nouvelles_reanimations, date)
        
        
        
        df %>% 
          gt() %>%
          fmt_number(columns = vars(cas, deces, gueris), decimals = 0) %>% 
          fmt_number(columns = vars(nouvelles_hospitalisations, nouvelles_reanimations), decimals = 0) %>%
          tab_header(
            title = md("Cumul a partir de la *\"date de depart\"*"),
            subtitle = md("Selection par ***Niveau : Global/R\u00e9gions/D\u00e9partements***")
          ) %>%
          tab_source_note(md(paste0("*Tableau dynamique des donn\u00e9es* ", "***","Global France", "***"))) %>%
          cols_label(date = html("date de d&#233;part"), 
                     nouvelles_hospitalisations = "hospitalisations",
                     nouvelles_reanimations = html("r&#233;animations"))
        
      })
      
      # regions reactive data -------------------------------------------------------------------------------------
      regions_france <- reactive({
        
        data_full_fr %>% 
          filter(granularite %in% "region") %>% 
          filter(source_type %in% c("opencovid19-fr")) %>% 
          filter(date >= as.Date(input$id_slider_date) & date <= as.Date(date_in[2]))
      })
      
      # regions gt table reactive ---------------------------------------------------------------------------------
      gt_reg_france <- reactive({
        
        df <- data_reg_gt %>% 
          filter(date >= as.Date(input$id_slider_date) & date <= as.Date(date_in[2])) %>% 
          mutate(date = as.Date(input$id_slider_date)) %>% 
          group_by(date, region) %>% 
          summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
          select(region, cas, deces, gueris, nouvelles_hospitalisations, nouvelles_reanimations, date) %>% 
          ungroup
        
        # remise accent pour nom REUNION
        df$region[df$region %in% "La_Reunion"] <- "La R\u00e9union"
        
        df %>% 
          gt() %>%
          fmt_number(columns = vars(cas, deces, gueris), decimals = 0) %>% 
          fmt_number(columns = vars(nouvelles_hospitalisations, nouvelles_reanimations), decimals = 0) %>%
          tab_header(
            title = md("Cumul a partir de la *\"date de depart\"*"),
            subtitle = md("Selection par ***Niveau : Global/R\u00e9gions/D\u00e9partements***")
          ) %>%
          tab_source_note(md(paste0("*Tableau dynamique des donn\u00e9es* ", "***","R\u00e9gions", "***"))) %>%
          cols_label(date = html("date de d&#233;part"), 
                     nouvelles_hospitalisations = "hospitalisations",
                     nouvelles_reanimations = html("r&#233;animations"),
                     cas = "cas (DOM)",
                     deces = "deces (DOM)",
                     gueris = "gueris (DOM)"
          )
        
      })
      
      # departements reactive data label--------------------------------------------------------------------------------
      
      departement_france_label <- reactive({
        data_full_fr %>%
          filter(date >= as.Date(input$id_slider_date) & date <= as.Date(date_in[2])) %>%
          filter(granularite %in% "departement")%>%
          filter(source_type %in% "sante-publique-france-data") %>%
          group_by(region) %>% 
          slice(1)
        
        
        
      })
      
      
      
      # event choix -----------------------------------------
      observeEvent(input$niv_select, {
        
        # region UI ------------------
        if(input$niv_select %in% "R\u00e9gions"){
          
          output$tabsets_id <- renderUI({
            ns <- NS(id)
            tabsetPanel(
              tabPanel("Hospitalisations (\u00e0 date)", 
                       plotlyOutput(ns("reg_hospi"))
              )
              ,
              tabPanel("Nouvelles hospitalisations (\u00e0 date)", 
                       plotlyOutput(ns("reg_new_hospi"))
              ),
              tabPanel("R\u00e9animations (\u00e0 date)", 
                       plotlyOutput(ns("reg_rea"))
              ),
              tabPanel("Nouvelles r\u00e9animations (\u00e0 date)", 
                       plotlyOutput(ns("reg_new_rea"))
              )
            )
          }) # fin update main panel
          
          # region plot -------------------------------------------------
          
          # hospitalises
          output$reg_hospi <- renderPlotly({
            
            num_var <- sym("hospitalises")
            cumul_plotly(regions_france(), num_var, "Hospitalis\u00e9s", colreg)
            
          })
          
          # nouveaux hospitalises
          output$reg_new_hospi <- renderPlotly({
            
            num_var <- sym("nouvelles_hospitalisations")
            cumul_new_plotly(regions_france(), num_var, "Nouveaux hospitalis\u00e9s", colreg)
            
          })
          
          # reanimations
          output$reg_rea <- renderPlotly({
            
            num_var <- sym("reanimation")
            cumul_plotly(regions_france(), num_var, "R\u00e9animations", colreg)
            
          })
          
          # nouvelles reanimations
          output$reg_new_rea <- renderPlotly({
            
            num_var <- sym("nouvelles_reanimations")
            cumul_new_plotly(regions_france(), num_var, "Nouvelles r\u00e9animations", colreg)
            
          })
          
          # tableau gt region ------------------------------------------------------------------
          output$gt_tab <- render_gt({
            gt_reg_france()
          })
          
        }# fin if defaut region
        
        # France global ui -----------------
        
        if(input$niv_select %in% "Global"){
          output$tabsets_id <- renderUI({
            ns <- NS(id)
            tabsetPanel(
              tabPanel("Cas (cumul)", 
                       plotlyOutput(ns("glob_cas"))
              )
              ,
              tabPanel("D\u00e9c\u00e8s (cumul)", 
                       plotlyOutput(ns("glob_deces"))
              ),
              tabPanel("Gu\u00e9risons (cumul)", 
                       plotlyOutput(ns("glob_gueris"))
              ),
              tabPanel("Hospitalisations (\u00e0 date)",
                       plotlyOutput(ns("glob_hospi"))
              ),
              tabPanel("Nouvelles hospitalisations (\u00e0 date)",
                       plotlyOutput(ns("glob_new_hospi"))
              ),
              tabPanel("R\u00e9animations (\u00e0 date)",
                       plotlyOutput(ns("glob_rea"))
              ),
              tabPanel("Nouvelles R\u00e9animations (\u00e0 date)",
                       plotlyOutput(ns("glob_new_rea"))
              )
              
              
            )
          }) # fin update main panel
          
          # France plot -----------------
          # cas 
          output$glob_cas <- renderPlotly({
            # req(! (is.na(global_france()$cas_confirmes) & length(global_france()$cas_confirmes) == 1))
            if((is.na(global_france()$cas_confirmes) & length(global_france()$cas_confirmes) == 1)) 
              stop(paste0("Pas de donn\u00e9es disponible pour le ", global_france()$date))
            
            num_var <- sym("cas_confirmes")
            cumul_plotly(global_france(), num_var, "Cas", colgraph)
          })
          
          # deces
          output$glob_deces <- renderPlotly({
            # req(! (is.na(global_france()$deces_tot) & length(global_france()$deces_tot) == 1)) 
            if((is.na(global_france()$deces_tot) & length(global_france()$deces_tot) == 1)) 
              stop(paste0("Pas de donn\u00e9es disponible pour le ", global_france()$date))
            
            num_var <- sym("deces_tot")
            cumul_plotly(global_france(), num_var, "D\u00e9c\u00e8s", colgraph)
          })
          
          # guerisons
          output$glob_gueris <- renderPlotly({
            if( (is.na(global_france()$gueris.x) & length(global_france()$gueris.x) == 1))
              stop(paste0("Pas de donn\u00e9es disponible pour le ", global_france()$date))
            
            num_var <- sym("gueris.x")
            cumul_plotly(global_france(), num_var, "Gu\u00e9risons", colgraph)
            
          })
          
          # hospitalises 
          output$glob_hospi <- renderPlotly({
            if( (is.na(global_france()$hospitalises) & length(global_france()$hospitalises) == 1))
              stop(paste0("Pas de donn\u00e9es disponible pour le ", global_france()$date))
            
            num_var <- sym("hospitalises")
            cumul_new_plotly(global_france(), num_var, "Hospitalis\u00e9s", colgraph)
            
          })
          
          # nouvelles hospitalisations
          # filtre opencovid19-fr
          output$glob_new_hospi <- renderPlotly({
            df <- data_full_fr %>% 
              filter(granularite %in% "pays") %>% 
              filter(source_type %in% "opencovid19-fr") %>% 
              filter(date >= as.Date(input$id_slider_date) & date <= as.Date(date_in[2]))
            
            if( (is.na(df$nouvelles_hospitalisations) & length(df$nouvelles_hospitalisations) == 1))
              stop(paste0("Pas de donn\u00e9es disponible pour le ", df$date))
            
            num_var <- sym("nouvelles_hospitalisations")
            cumul_new_plotly(df, num_var, "Nouveaux hospitalis\u00e9s", colgraph)
            
          })
          
          # reanimations
          output$glob_rea <- renderPlotly({
            if( (is.na(global_france()$reanimation) & length(global_france()$reanimation) == 1))
              stop(paste0("Pas de donn\u00e9es disponible pour le ", global_france()$date))
            
            num_var <- sym("reanimation")
            cumul_new_plotly(global_france(), num_var, "R\u00e9animations", colgraph)
            
          })
          
          # nouvelles reanimations
          # filtre opencovid19-fr
          output$glob_new_rea <- renderPlotly({
            df <- data_full_fr %>% 
              filter(granularite %in% "pays") %>% 
              filter(source_type %in% "opencovid19-fr") %>% 
              filter(date >= as.Date(input$id_slider_date) & date <= as.Date(date_in[2]))
            
            if( (is.na(df$nouvelles_reanimations) & length(df$nouvelles_reanimations) == 1))
              stop(paste0("Pas de donn\u00e9es disponible pour le ", df$date))
            
            num_var <- sym("nouvelles_reanimations")
            cumul_new_plotly(df, num_var, "Nouvelles r\u00e9animations", colgraph)
            
          })
          
          # tableau gt France ---------------------
          output$gt_tab <- render_gt({
            gt_glob_france() 
          })
        }# fin if france global
        
        # selection departements
        if(input$niv_select %in% "D\u00e9partements"){
          
          # departements ui -------------------------------
          output$tabsets_id <- renderUI({
            ns <- NS(id)
            tagList(
              tabsetPanel(
                tabPanel("Hospitalisations (\u00e0 date)", 
                         plotlyOutput(ns("dep_hospi"))
                )
                ,
                tabPanel("Nouvelles hospitalisations (\u00e0 date)", 
                         plotlyOutput(ns("dep_new_hospi"))
                ),
                tabPanel("R\u00e9animations (\u00e0 date)", 
                         plotlyOutput(ns("dep_rea"))
                ),
                tabPanel("Nouvelles r\u00e9animations (\u00e0 date)", 
                         plotlyOutput(ns("dep_new_rea"))
                )
              )
              ,
              pickerInput(ns("niv_dep"), "Choix departements:",   
                          choices = unique(departement_france_label()$region), 
                          options = list(`actions-box` = TRUE, `none-selected-text` = "Faites votre sélection!"),
                          selected = unique(departement_france_label()$region)[1:10],
                          multiple = TRUE
              )
            )
            
          }) # fin update main panel
          
          updatePickerInput(session = session, inputId = "niv_dep",
                            choices = unique(departement_france_label()$region),
                            selected = unique(departement_france_label()$region)[1:10]
          )
          
          observeEvent(input$niv_dep, { # observe event dep ----------------------------------------------
            # departements reactive data --------------------------------------------------------------------------------
            
            departement_france <- reactive({
              data_full_fr %>% 
                filter(date >= as.Date(input$id_slider_date) & date <= as.Date(date_in[2])) %>% 
                filter(granularite %in% "departement")%>% 
                filter(source_type %in% "sante-publique-france-data") %>% 
                filter(region %in% input$niv_dep) %>% 
                select(c(date, region, hospitalises, nouvelles_hospitalisations, reanimation, nouvelles_reanimations)) %>% 
                right_join(data_full_fr %>% 
                             select(c(date, region, granularite)) %>% 
                             filter(granularite %in% "departement") %>% 
                             filter(region %in% input$niv_dep),
                           by = c("date", "region")
                )
              
              
            })
            
            # departement gt reactive --------------------------------------------------------------------------------------
            
            gt_dep_france <- reactive({
              df <- departement_france() %>% 
                select(c(date, region, nouvelles_hospitalisations, nouvelles_reanimations)) %>% 
                mutate(date = as.Date(input$id_slider_date)) %>% 
                group_by(date, region) %>% 
                summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
                select(region, nouvelles_hospitalisations, nouvelles_reanimations, date) %>% 
                ungroup
              
              df %>% 
                gt() %>%
                fmt_number(columns = vars(nouvelles_hospitalisations, nouvelles_reanimations), decimals = 0) %>%
                tab_header(
                  title = md("Cumul a partir de la *\"date de depart\"*"),
                  subtitle = md("Selection par ***Niveau : Global/R\u00e9gions/D\u00e9partements***")
                ) %>%
                tab_source_note(md(paste0("*Tableau dynamique des donn\u00e9es* ", "***","D\u00e9partements", "***"))) %>%
                cols_label(date = html("date de d&#233;part"), 
                           nouvelles_hospitalisations = "hospitalisations",
                           nouvelles_reanimations = html("r&#233;animations")
                )
              
              
            })
            
            
            
            # departements plot ----------------------------------------------------
            # hospitalises
            output$dep_hospi <- renderPlotly({
              
              num_var <- sym("hospitalises")
              cumul_plotly(departement_france(), num_var, "Hospitalis\u00e9s", coldep)
              
            })
            
            # nouveaux hospitalises
            output$dep_new_hospi <- renderPlotly({
              
              num_var <- sym("nouvelles_hospitalisations")
              cumul_new_plotly(departement_france(), num_var, "Nouveaux hospitalis\u00e9s", coldep)
              
            })
            
            # reanimations
            output$dep_rea <- renderPlotly({
              
              num_var <- sym("reanimation")
              cumul_plotly(departement_france(), num_var, "R\u00e9animations", coldep)
              
            })
            
            # nouvelles reanimations
            output$dep_new_rea <- renderPlotly({
              
              num_var <- sym("nouvelles_reanimations")
              cumul_new_plotly(departement_france(), num_var, "Nouvelles r\u00e9animations", coldep)
              
            })
            
            # tableau gt departements ------------------------------------------------------------------
            output$gt_tab <- render_gt({
              gt_dep_france()
            })
            
            
          })# fin observe event departement
          
          
        } # fin departements
        
      })# fin oberve event
      
      
    })# fin module server
  
  
  
}

