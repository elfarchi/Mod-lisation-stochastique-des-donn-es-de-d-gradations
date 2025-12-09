library(shiny)
library(bslib)

# -------------------------------
# User Interface
# -------------------------------
ui <- page_navbar(
  title = "Étude des modèles de dégradation",
  
  # ---- PAGE 1 ----
  nav_panel(
    "Processus Wiener",
    page_sidebar(
      sidebar = sidebar(
        selectInput(
          inputId = "model_choice",
          label = "La loi pour les dates de simulation :",
          choices = c("Constant", "Exponentiel", "Weibull")
        ),
        
        numericInput("pt_nbr", "Nombre points :", 5, min = -100, max = 100),
        numericInput("moyenne", "Moyenne :", 2, min = 0, max = 100),
        numericInput("variance", "Variance :", 1),
        numericInput("traj_nbr", "Nb trajectoires :", 1),
        numericInput("seuil", "Seuil :", 1)
      ),
      plotOutput("plot1")
    )
  ),
  
  # ---- PAGE 2 ----
  nav_panel(
    "Processus Gamma",
    page_sidebar(
      sidebar = sidebar(
        numericInput("lambda", "Lambda :", 1)
      ),
      plotOutput("plot2")
    )
  ),
  
  # ---- PAGE 3 ----
  nav_panel(
    "Laser Data",
    page_sidebar(
      sidebar = sidebar(
        numericInput("shape", "Shape :", 2),
        numericInput("scale", "Scale :", 1)
      ),
      plotOutput("plot3")
    )
  )
)

# -------------------------------
# SERVER
# -------------------------------
server <- function(input, output, session) {
  
  # ----- PAGE 1 -----
  output$plot1 <- renderPlot({
    model <- input$model_choice
    
    if (model == "Normal") {
      x <- rnorm(1000, mean = input$mean, sd = sqrt(input$variance))
    } else if (model == "Exponentiel") {
      x <- rexp(1000, rate = 1/input$mean)
    } else if (model == "Weibull") {
      x <- rweibull(1000, shape = input$param1, scale = input$param2)
    }
    
    hist(x, main = paste("Distribution:", model), xlab = "", col = "#007bc2", border = "white")
  })
  
  # ----- PAGE 2 -----
  output$plot2 <- renderPlot({
    x <- rexp(500, rate = input$lambda)
    hist(x, col = "#007bc2", border = "white",
         main = "Modèle 2")
  })
  
  # ----- PAGE 3 -----
  output$plot3 <- renderPlot({
    x <- rweibull(500, shape = input$shape, scale = input$scale)
    hist(x, col = "#007bc2", border = "white",
         main = "Modèle 3")
  })
}

# -------------------------------
# Run the app
# -------------------------------
shinyApp(ui, server)
