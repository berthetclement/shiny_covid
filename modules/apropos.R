# RMD 

aproposUI <- function(id){
  ns <- NS(id)
  
  fluidPage(includeCSS("styles_github.css"),
    # column(10,
    #        box(
    #         title = "Modèle arbre de décision binaire"
    #        )
    # )
    includeMarkdown("modules/a_propos.RMD"),
    tags$h2("Contact"),
    tags$a(class="fab  fa-github "),
    tags$a(href="https://github.com/berthetclement", ": repos Github"),
    tags$br(), tags$br(), 
    tags$a(href="mailto:clement.berthet013@gmail.com", class="email", "clement.berthet013@gmail.com")
  )
}