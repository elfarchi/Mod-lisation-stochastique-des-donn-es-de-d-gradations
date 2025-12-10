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
        
        numericInput("pt_nbr", "Nombre points :", 100, min = -100, max = 100),
        numericInput("moyenne", "Moyenne :", 10, min = 0, max = 100),
        numericInput("variance", "Variance :", 10),
        numericInput("traj_nbr", "Nb trajectoires :", 7),
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
        numericInput("pt_nbr", "Nombre points :", 100, min = -100, max = 100),
        numericInput("shape","forme :",3),
        numericInput("traj_nbr", "Nb trajectoires :", 7),
        numericInput("rate","taux :",5)
      ),
      plotOutput("plot2")
    )
  ),
  
  # ---- PAGE 3 ----
  nav_panel(
    "CSV Plot",
    
    sidebarLayout(
      sidebarPanel(
        fileInput(
          inputId = "uploaded_file",
          label = "Upload a CSV file",
          accept = c(".csv")
        )
      ),
      
      mainPanel(
        plotOutput("csv_plot"),
        verbatimTextOutput("csv_debug")
      )
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
    traj_nbr = input$traj_nbr
    L = input$pt_nbr
    T = 10
    N=1000
    t = seq(0,T,length.out = L)
    
    simulate = function() {
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
      return(X)
    }
    
    results = replicate(traj_nbr, simulate())
    plot(t, results[, 1],type= 'l')
    for (i in 2:ncol(results)) {
      lines(t, results[, i], type='l')
    }
    abline(0, mu, col='red')
  })
  
  # ----- PAGE 2 -----
  output$plot2 <- renderPlot({
    traj_nbr = input$traj_nbr
    nbr_pts = input$pt_nbr
    forme = input$shape
    taux = input$rate
    T = 10
    N=1000
    t = seq(0,T,length.out = nbr_pts)
    
    simulate = function() {
      G = rep(0,nbr_pts)
      x = numeric(nbr_pts)
      x[1] = 0
      delta_x = rgamma(nbr_pts,shape = forme*T/nbr_pts,rate = taux)
      for (i in 2:nbr_pts){
        x[i] = delta_x[i] + x[i - 1]
      }
      return(x)
    }
    
    results = replicate(traj_nbr, simulate())
    plot(t, results[, 1],type= 'l')
    for (i in 2:ncol(results)) {
      lines(t, results[, i], type='l')
    }
  })
  
  # ----- PAGE 3 -----
  data_csv <- reactive({
    req(input$uploaded_file)
    
    path <- input$uploaded_file$datapath
    
    # 1 ere ligne
    first_line <- readLines(path, n = 1)
    sep_guess <- if (grepl(";", first_line)) ";" else ","
    
    df <- tryCatch({
      read.csv2(path, sep=";", dec=",")
      # read.csv(path, sep = sep_guess, dec = ifelse(sep_guess == ";", ",", "."))
    }, error = function(e) {
      cat("Error reading CSV:", e$message, "\n")
      return(NULL)
    })
    
    return(df)
  })
  
  output$csv_plot <- renderPlot({
    df <- data_csv()
    req(df)
    
    # le temps (colonne 1)
    t <- df[[1]]
    
    # Initialisation du plot avec la premiere courbe
    plot(
      t, df[[2]],
      type = "b",     # line + points
      pch = 16,       # solid dots
      main = "Toutes les courbes du CSV",
      xlab = "Temps",
      ylab = "Valeurs de degradation"
    )
    
    # les autres courbes
    for (i in 3:ncol(df)) {
      lines(t, df[[i]], type = "b", pch = 16)
    }
  })
}

# -------------------------------
# Run the app
# -------------------------------
shinyApp(ui, server)
