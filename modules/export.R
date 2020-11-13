

# export csv ----

exportUI <- function(id){
  ns <- NS(id)
  
  fluidPage(
    numericInput(ns("maxrows"), "Extrait des données enrichies", 25),
    verbatimTextOutput(ns("rawtable")),
    downloadButton(ns("downloadCsv"), "Télécharger au format CSV"),tags$br(),tags$br(),
    "Les données proviennent du repos Github : ", 
    tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", 
                                                       "Johns Hopkins Center for Systems Science and Engineering (CSSE).")
    
  )

}

exportServer <- function(id, df, base_countries, max_date){
  moduleServer(
    id,
    function(input, output, session){
      # data 
      df_exp <- df
      
      # jointure avec countries
      names(df_exp)[1] <- "jhu_ID" # pour jointure
      df_exp <- merge(df_exp, base_countries, by = "jhu_ID")
      
      df_exp <- df_exp[order(df_exp$alpha3), ] # meme ordre de population 
      
      # indicateur cas/population/100k (coloriage map)
      df_exp <- add_map_stats(df_exp)
      
      df_exp <- df_exp %>% 
        select(country, date, cases, deaths, recovered, new_cases, new_deaths, new_recovered, 
               per100k, newper100k, deathsper100k, newdeathsper100k)
        
      df_exp
      
      # output download data
      output$downloadCsv <- downloadHandler(
        filename = function() {
          paste("COVID_data_", max_date, ".csv", sep="")
        },
        content = function(file) {
          write.csv(df_exp, file)
        }
      )
      
      output$rawtable <- renderPrint({
        orig <- options(width = 1000)
        print(tail(df_exp, input$maxrows), row.names = FALSE)
        options(orig)
      })
    }
  )
  
  
}
