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
          choices = c("Constant")
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
    mu = input$moyenne
    sigma = sqrt(input$variance)
    L = input$pt_nbr
    T = 10
    N=1000
    t = seq(0,T,length.out = L)
    Z_k = rnorm(N)
    B = rep(0,L)
    for (i in 1:L){
      u = t[i]
      s <- 0
      for(j in 1:N){
        e_ju = sqrt(8*T)/((2*j+1)*pi)*sin(((2*j+1)*pi*u)/(2*T))
        s <- s+ Z_k[j]*e_ju
      }
      B[i] = s
    }
    X = mu*t + sigma*B
    plot(t,X,type= 'l')
  })
  
}

# -------------------------------
# Run the app
# -------------------------------
shinyApp(ui, server)
