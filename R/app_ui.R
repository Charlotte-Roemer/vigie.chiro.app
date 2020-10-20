#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import data.table
#' @import dplyr
#' @import ggplot2
#' @import ggvis
#' @import shiny
#' @import data.table
#' @import lubridate
#' @import clipr
#' @noRd
app_ui <- function(request) {
  SpeciesList <- data.table::fread(system.file("app/data/SpeciesList.csv", 
                                   package = "vigie.chiro.app"),
                       encoding = "Latin-1")
  SpeciesList$color=factor(SpeciesList$Esp)
  groupes=unique(SpeciesList$GroupFR)
  especes=unique(SpeciesList$Esp)
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    pageWithSidebar(
      # Application title
      headerPanel(""),# ("Analyse Tadarida"),
      # Sidebar with controls to select parameters to plot, group, species etc.
      # Note the use of the br()
      # element to introduce extra vertical spacing
      sidebarPanel(width = 2,     
                   # Input: Select a file ----
                   fileInput("fileParticipation", "Importer votre fichier de participation",
                             multiple = FALSE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")),
                   #textInput("wavdirChoice", "Entrer le chemin d'accès répertoire son"),
                   actionButton("do", "Valider"),
                   uiOutput("paramschoix"),
                   uiOutput("idchoix"),
                   uiOutput("especechoix"),
                   sliderInput("conf", #s?lection sur le score de Tadarida
                               label = "Indice de confiance de l'espece :",
                               min = 0, max = 1, value = c(0.5, 1))
                   ,
                   sliderInput("frequence_mediane", #s?lection sur la fr?quence m?diane
                               label = "Frequence mediane",
                               min = 0, max = 250, value = c(0, 120))
                   
                   
                   ,
                   #uiOutput("espececorrige")
                   selectInput("espececorrige", #choix de validations
                               "Espece correction:",
                               c(especes))
                   ,
                   selectInput("probacorrige", #choix de validations
                               "Confiance:",
                               c("POSSIBLE","PROBABLE","SUR"))
                   ,
                   actionButton("submit","Valider") #pour soumettre cette validation
                   ,
                   downloadButton('downloadData', 'Sauver les corrections') #sauver les corrections
      ),
      mainPanel(
        
        tabsetPanel(
          tabPanel("titre",
                   shiny::column(9,
                                 ggvisOutput("plot")),
                   shiny::column(11, offset = 1,
                                 uiOutput("heures")),
                   tableOutput("testStr")
                   # ,
                   #shiny::column(12,
                   #             sliderInput("frequence_mediane",
                   #                     min = 0, max = 130, value = c(0, 130)
                   
          ),
          
          tabPanel("Table", dataTableOutput(outputId="table")),
          tabPanel("Dernier fichier ouvert"
                   ,
                   tableOutput(outputId="table2")
                   ,
                   dataTableOutput(outputId="table3")
                   # ,
                   # wellPanel(
                   # textInput('groupecorrige', "nouveau groupe","")
                   # ,
                   # textInput('espececorrige', "nouvelle espèce","")
                   # ,
                   # actionButton("submit","Valider")
          )
          # shiny::column(3,
          # selectInput("groupecorrige",
          # "Groupe :",
          # c("Tous",
          # groupes)))
          # ,
          # shiny::column(3, offset = 4,
          # selectInput("espececorrige",
          # "Espèce :",
          # c("Toutes",
          # colnames(seqScores))))
          # ,
          # shiny::column(3, offset = 3,
          # actionButton("update", "Valider la correction"))
          #       ,
          #      tableOutput(outputId="table3")
        )
        
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'vigie.chiro.app'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

