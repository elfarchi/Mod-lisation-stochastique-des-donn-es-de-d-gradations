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
          choices = c("Constant", "Aleatoire")
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
        selectInput(
          inputId = "model_choice",
          label = "La loi pour les dates de simulation :",
          choices = c("Constant", "Aleatoire")
        ),
        
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
    "CSV plot",
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Upload CSV File", accept = ".csv"),
        checkboxInput("log_it", "Log the dataset"),
        checkboxInput("show_stats", "Afficher statistiques"),
        checkboxInput("show_extra", "Afficher courbes supplémentaires"),
        hr(),
        verbatimTextOutput("csv_stats")   # <--- stats appear here
      ),
      mainPanel(
        plotOutput("plot3", height = "800px")
      )
    )
  )
)

# -------------------------------
# SERVER
# -------------------------------
server <- function(input, output, session) {
  
  # ----- PAGE 1 -----
  simulateWiener = function(N, L, mu, sigma, t) {
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
  
  output$plot1 <- renderPlot({
    model <- input$model_choice
    mu = input$moyenne
    sigma = sqrt(input$variance)
    traj_nbr = input$traj_nbr
    L = input$pt_nbr
    T = 10
    N=1000
    if (model == "Constant") {
      t = seq(0,T,length.out = L)
      
    } else if (model == "Aleatoire") {
      delta_t = runif(L - 1)
      t = numeric(L)
      t[1] = 0
      for (i in 2:L) {
        t[i] = t[i - 1] + delta_t[i - 1] 
      }
    }
    
    if (traj_nbr == 1){
      plot(t,simulateWiener(N, L, mu, sigma, t),type='l')
      abline(0, mu, col='red')
    }else{
      results = replicate(traj_nbr, simulateWiener(N, L, mu, sigma, t))
      plot(t, results[, 1],type= 'l')
      for (i in 2:ncol(results)) {
        lines(t, results[, i], type='l')
      }
      abline(0, mu, col='red')
    }
  })

  # ----- PAGE 2 -----
  
  simulateGamma = function(nbr_pts, forme, taux, t) {
    G = rep(0,nbr_pts)
    x = numeric(nbr_pts)
    x[1] = 0
    delta_x = numeric(nbr_pts)
    delta_x[1] = rgamma(1, shape = forme * t[1], rate = taux)
    for (i in 2:nbr_pts) {
      delta_x[i] = rgamma(1, shape = forme * (t[i] - t[i - 1]), rate = taux)
    }
    for (i in 2:nbr_pts){
      x[i] = delta_x[i] + x[i - 1]
    }
    return(x)
  }
  
  output$plot2 <- renderPlot({
    model <- input$model_choice
    traj_nbr = input$traj_nbr
    nbr_pts = input$pt_nbr
    forme = input$shape
    taux = input$rate
    T = 10
    N=1000
    if (model == "Constant") {
      t = seq(0,T,length.out = nbr_pts)
      
    } else if (model == "Aleatoire") {
      delta_t = runif(nbr_pts - 1)
      t = numeric(nbr_pts)
      t[1] = 0
      for (i in 2:nbr_pts) {
        t[i] = t[i - 1] + delta_t[i - 1] 
      }
    }
    
    if (traj_nbr == 1){
      plot(t,simulateGamma(nbr_pts, forme, taux, t),type='l')
    } else {
      results = replicate(traj_nbr, simulateGamma(nbr_pts, forme, taux, t))
      plot(t, results[, 1],type= 'l')
      for (i in 2:ncol(results)) {
        lines(t, results[, i], type='l')
      }
    }
    abline(0, forme/taux, col='red')
  })
  # ----- PAGE 3 -----
  data <- reactive({
    req(input$file)
    df <- read.csv2(input$file$datapath)
  })
  
  params <- reactive({
    df <- data()
    t = df[[1]]
    a = numeric(ncol(df) - 1)
    b = numeric(ncol(df) - 1)
    for (i in 1:length(a)) {
      X = df[[i + 1]]
      delta_X = numeric(nrow(df))
      delta_X[1] = X[1]
      for (j in 2:length(delta_X)) {
        delta_X[j] = X[j] - X[j - 1]
      }
      m = mean(delta_X)
      v = var(delta_X)
      a[i] = m/v
      b[i] = t[length(t)]/v
    }
    return (list(t = t, a = a, b = b))
  })
  
  output$csv_stats <- renderPrint({
    req(input$show_stats)
    p <- params()
    print(p$a)
    print(p$b)
  })
  
  output$plot3 <- renderPlot({
    df <- data()
    req(df)
    
    log_it <- input$log_it
    x <- if (log_it) log(df[[1]]) else df[[1]]
    y_col <- if (log_it) log(df[-1]) else df[-1]
    
    matplot(x, y_col, type = "b", pch = 16, lty = 1, xlab = "X", ylab = "Valeurs", main = "Courbes depuis CSV")
    
    if (input$show_extra) {
      p <- params()
      t = p$t
      a = p$a
      b = p$b
      for (i in 1:length(a)){
        lines(t, simulateGamma(length(t), a[i], b[i], t), col = "orange2", lwd = 2, lty = 2)
      }
    }
  })

}

# -------------------------------
# Run the app
# -------------------------------
shinyApp(ui, server)

