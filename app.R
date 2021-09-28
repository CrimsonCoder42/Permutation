#PermutePartial
#Change the name to Permutation when you finish the homework
library(shiny)
library(shinydashboard)
library(shinyWidgets)
#library(permutations)
source("jaxmat.R")

header <- dashboardHeader(title = "Permutations")
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(
    
    #first column
    column(
      width = 4,
      box(
          width = NULL, height = 220,
          h3("Input"),
          textInput("atext","a","(12)"),
          textInput("btext","b","(13)")
      ),
      box(
          width = NULL, height = 150,
          h3("Products"),
          h4(uiOutput("prodab")),
          h4(uiOutput("prodba"))
      ),
      box(
        width = NULL, height = 150,
        h3("Inverses"),
        uiOutput("a_inv_o"),
        uiOutput("b_inv_o")
        
      ),
      box(
        width = NULL, height = 150,
        h3("Conjugates"),
        uiOutput("a_conj_o"),
        uiOutput("b_conj_o")
        
      ),

    ),
    
    #second column
    column(
        width = 4,
        
        box(
            width = NULL, height = 350,
            h3("Powers of a"),
            uiOutput("powersa"),
            tags$head(tags$style("#powersa{color:red; font-size:20px;
            font-style:italic;
            overflow-y:scroll; max-height:250px;}"))
        ),
        
        box(
          width = NULL, height = 350,
          h3("Powers of ab"),
          uiOutput("powersab"),
          tags$head(tags$style("#powersab{color:red; font-size:20px;
            font-style:italic;
            overflow-y:scroll; max-height:250px;}"))
        )


    ),
    
    #third column
    column(
        width = 4,

        box(
          width = NULL, height = 350,
          h3("Powers of b"),
          uiOutput("powersb"),
          tags$head(tags$style("#powersb{color:red; font-size:20px;
            font-style:italic;
            overflow-y:scroll; max-height:250px;}"))
        ),
        
        box(
          width = NULL, height = 350,
          h3("Powers of ba"),
          uiOutput("powersba"),
          tags$head(tags$style("#powersba{color:red; font-size:20px;
            font-style:italic;
            overflow-y:scroll; max-height:250px;}"))
        )
        
    )
  ),
  
  actionBttn("btncalc","Calculate",
             color = "primary", size = "lg") #an awesome button from shinyWidgets
  
)

ui <- dashboardPage(header, sidebar, body)


source("permutecalc.R")    


server <- function(input, output) {
  
  output$prodab <- renderUI("ab = (132)")
  output$prodba <- renderUI("ba = (123)")
  output$powersa <- renderUI(HTML(paste("(12)","I",sep = "<br/>")))
  
  observeEvent(input$btncalc, {
    
    #calculate the products
    ab <- Perm.multiply(input$atext,input$btext)
    ba <- Perm.multiply(input$btext,input$atext)
    
    #output the product of ab and ba
    output$prodab <- renderUI(paste("ab =  ",ab))
    output$prodba <- renderUI(paste("ba =  ",ba))
    
    #output the powers of a and b
    output$powersa <- renderUI(HTML(Perm.powerString(input$atext)))
    output$powersb <- renderUI(HTML(Perm.powerString(input$btext)))

    #output the powers of ab and ba
    output$powersab <- renderUI(HTML(Perm.powerString( ab )))
    output$powersba <- renderUI(HTML(Perm.powerString( ba )))
    
    #calculate the inverse of a and b
    a_inv = Perm.inverse(input$atext)
    b_inv = Perm.inverse(input$btext)
    
    #output the inverse of a and b
    output$a_inv_o = renderUI({
      jaxI( paste0("a^{-1} = ", a_inv) )
    })
    output$b_inv_o = renderUI({
      jaxI( paste0("b^{-1} = ", b_inv) )
    })
    
    #calculate the conjugates
    a_conj = Perm.multiply(ab, a_inv)
    b_conj = Perm.multiply(ba, b_inv)
    
    #output the conjugates
    output$a_conj_o = renderUI({
      jaxI( paste0("aba^{-1} = ", a_conj) )
    })
    output$b_conj_o = renderUI({
      jaxI( paste0("bab^{-1} = ", b_conj) )
    })
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
