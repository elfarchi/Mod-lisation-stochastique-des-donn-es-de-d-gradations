library(shiny)
library(bslib)

# -------------------------------
# INTERFACE UTILISATEUR (UI)
# -------------------------------
ui <- page_navbar(
  title = "Étude des modèles de dégradation",
  
  # ---- PAGE 1 : Processus de Wiener ----
  nav_panel(
    "Processus Wiener",
    page_sidebar(
      sidebar = sidebar(
        
        # Choix du modèle de génération des temps
        selectInput(
          inputId = "model_choice",
          label = "La loi pour les dates de simulation :",
          choices = c("Constant", "Aleatoire")
        ),
        
        # Paramètres du modèle
        numericInput("pt_nbr", "Nombre points :", 100, min = -100, max = 100),
        numericInput("moyenne", "Moyenne :", 10, min = 0, max = 100),
        numericInput("variance", "Variance :", 10),
        numericInput("traj_nbr", "Nb trajectoires :", 7),
        numericInput("seuil", "Seuil :", 1)
      ),
      
      plotOutput("plot1")
    )
  ),
  
  # ---- PAGE 2 : Processus Gamma ----
  nav_panel(
    "Processus Gamma",
    page_sidebar(
      sidebar = sidebar(
        
        # Choix du modèle temporel
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
  
  # ---- PAGE 3 : Analyse d'un CSV ----
  nav_panel(
    "CSV plot",
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Upload CSV File", accept = ".csv"),
        checkboxInput("log_it", "Log the dataset"),
        checkboxInput("show_stats", "Afficher statistiques"),
        checkboxInput("show_Gamma_Moments", "Afficher les simulations de Gamma (Moments)"),
        checkboxInput("show_Wiener_Vrai", "Afficher les simulations de Wiener (Max.Vrai.)"),
        checkboxInput("takeoff_data", "Enlever les courbes reelles"),
        numericInput("seuil", "Seuil horizontal :", value = 20),
        hr(),
        verbatimTextOutput("csv_stats")   # Affichage statistiques
      ),
      
      mainPanel(
        plotOutput("plot3", height = "800px")
      )
    )
  )
)

# -------------------------------
# SERVEUR
# -------------------------------
server <- function(input, output, session) {
  
  # ============================================================
  # PAGE 1 : Simulation du processus de Wiener
  # ============================================================
  
  # Fonction qui simule une trajectoire de Wiener (décomposition KL)
  simulateWiener = function(N, L, mu, sigma, t) {
    Z_k = rnorm(N)          # Coefficients aléatoires
    B = rep(0,L)            # Stockage du bruit brownien approché
    
    for (i in 1:L){
      u = t[i]
      s <- 0
      # Décomposition de Karhunen-Loève tronquée
      for(j in 1:N){
        e_ju = sqrt(8*T)/((2*j+1)*pi)*sin(((2*j+1)*pi*u)/(2*T))
        s <- s+ Z_k[j]*e_ju
      }
      B[i] = s
    }
    
    # Processus avec dérive : X(t) = μt + σB(t)
    X = mu*t + sigma*B
    return(X)
  }
  
  # --- Plot du Wiener ---
  output$plot1 <- renderPlot({
    model <- input$model_choice
    mu = input$moyenne
    sigma = sqrt(input$variance)
    traj_nbr = input$traj_nbr
    L = input$pt_nbr
    T = 10
    N=1000
    
    # Construction de l'échelle temporelle
    if (model == "Constant") {
      t = seq(0,T,length.out = L)
    } else {
      delta_t = runif(L - 1)
      t = numeric(L)
      t[1] = 0
      for (i in 2:L) t[i] = t[i - 1] + delta_t[i - 1]
    }
    
    # Cas une seule trajectoire
    if (traj_nbr == 1){
      plot(t,simulateWiener(N, L, mu, sigma, t),type='l')
      abline(0, mu, col='red')
    } else {
      # Plusieurs trajectoires
      results = replicate(traj_nbr, simulateWiener(N, L, mu, sigma, t))
      plot(t, results[, 1],type= 'l')
      for (i in 2:ncol(results)) lines(t, results[, i])
      abline(0, mu, col='red')
    }
  })
  
  # ============================================================
  # PAGE 2 : Processus Gamma
  # ============================================================
  
  # Simule un processus Gamma (increments indépendants Gamma)
  simulateGamma = function(nbr_pts, forme, taux, t) {
    G = rep(0,nbr_pts)
    x = numeric(nbr_pts)
    x[1] = 0
    delta_x = numeric(nbr_pts)
    
    # Première incrémentation
    delta_x[1] = rgamma(1, shape = forme * t[1], rate = taux)
    
    # Incréments Gamma successifs
    for (i in 2:nbr_pts) {
      delta_x[i] = rgamma(1, shape = forme * (t[i] - t[i - 1]), rate = taux)
    }
    
    # Processus cumulé
    for (i in 2:nbr_pts) x[i] = delta_x[i] + x[i - 1]
    
    return(x)
  }
  
  output$plot2 <- renderPlot({
    model <- input$model_choice
    traj_nbr = input$traj_nbr
    nbr_pts = input$pt_nbr
    forme = input$shape
    taux = input$rate
    T = 10
    
    # Temps constant ou aléatoire
    if (model == "Constant") {
      t = seq(0,T,length.out = nbr_pts)
    } else {
      delta_t = runif(nbr_pts - 1)
      t = numeric(nbr_pts)
      t[1] = 0
      for (i in 2:nbr_pts) t[i] = t[i - 1] + delta_t[i - 1]
    }
    
    # Affichage
    if (traj_nbr == 1){
      plot(t,simulateGamma(nbr_pts, forme, taux, t),type='l')
    } else {
      results = replicate(traj_nbr, simulateGamma(nbr_pts, forme, taux, t))
      plot(t, results[, 1],type= 'l')
      for (i in 2:ncol(results)) lines(t, results[, i])
    }
    
    # Ligne horizontale = moyenne du processus (théorique)
    abline(0, forme/taux, col='red')
  })
  
  # ============================================================
  # PAGE 3 : Lecture CSV et estimation Gamma par moments
  # ============================================================
  
  # Lecture du fichier en réactif
  data <- reactive({
    req(input$file)
    df <- read.csv2(input$file$datapath)
  })
  
  # Estimation des paramètres Gamma par la méthode des moments
  paramsGammaMoments <- reactive({
    df <- data()
    t = df[[1]]
    
    a = numeric(ncol(df) - 1)
    b = numeric(ncol(df) - 1)
    
    # On calcule pour chaque colonne une estimation (a,b)
    for (i in 1:length(a)) {
      X = df[[i + 1]]
      
      # Incréments du processus
      delta_X = numeric(nrow(df))
      delta_X[1] = X[1]
      for (j in 2:length(delta_X)) delta_X[j] = X[j] - X[j - 1]
      
      m = mean(delta_X)
      v = var(delta_X)
      
      # Formules méthode des moments
      a[i] = (m^2)/(v * (t[i +1] - t[i]))
      b[i] = m/v
    }
    
    return (list(t = t, a = a, b = b))
  })
  
  paramsWienerMaxVrai <- reactive({
    df <- data()
    t = df[[1]]
    n = length(t)
    delta_t = numeric(n)
    delta_t[1] = t[1]
    for (i in 2:n) {
      delta_t[i] = t[i] - t[i-1]
    }
    mu = numeric(ncol(df) - 1)
    sigma = numeric(ncol(df) - 1)
    for (i in 1:length(mu)) {
      X = log(df[[i + 1]])
      delta_X = numeric(length(X))
      delta_X[1] = X[1]
      for (j in 2:n) {
        delta_X[j] = X[j] - X[j-1]
      }
      mu[i] = X[n]/t[n]
      sigma[i] = sqrt(sum((delta_X - mu[i] * delta_t)^2)/n)
    }
    return (list(t = t, mu = mu, sigma = sigma))
  })
  
  # Affichage des stats
  output$csv_stats <- renderPrint({
    req(input$show_stats)
    p <- paramsGammaMoments()
    print("parametres de Gamma par la methode de moments")
    print(p$a)
    print(p$b)
  })
  
  # ---- Plot CSV + simulations ----
  output$plot3 <- renderPlot({
    show_data <- !isTRUE(input$takeoff_data)
    show_gamma_Moments  <- isTRUE(input$show_Gamma_Moments)
    show_wiener_Vrai <- isTRUE(input$show_Wiener_Vrai)
    
    req(show_data || show_gamma_Moments || show_wiener_Vrai)
    
    df <- data()
    req(df)
    
    # Logging éventuel
    log_it <- isTRUE(input$log_it)
    x <- if (!log_it) df[[1]] else log(df[[1]])
    y_col <- if (!log_it) df[-1] else log(df[-1])
    
    # Extension du domaine temporel pour prévoir dans le futur
    t_max <- max(x, na.rm = TRUE)
    xlim <- c(0, 3 * t_max)
    t_ext <- seq(from = min(x), to = xlim[2], length.out = 10)
    
    seuil <- input$seuil
    ylim <- c(0, seuil * 1.2)
    
    
    # Plot vide
    plot(NA, xlim = xlim, ylim = ylim,
         xlab = "X", ylab = "Valeurs",
         main = "Courbes depuis CSV", type = "n")
    
    # Courbes réelles
    if (show_data) matlines(x, y_col, type = "b", pch = 16, lty = 1)
    
    # Simulations Gamma
    if (show_gamma_Moments) {
      p <- paramsGammaMoments()
      a <- p$a
      b <- p$b
      
      sim_mat <- sapply(seq_along(a), function(i) {
        simulateGamma(length(t_ext), a[i], b[i], t_ext)
      })
      
      # Moyenne + trajectoires
      # lines(t_ext, rowMeans(sim_mat), col = "blue", lwd = 3)
      matlines(t_ext, sim_mat, lty = 2, lwd = 2)
    }
    
    # Simulations Wiener
    if(show_wiener_Vrai) {
      p <- paramsWienerMaxVrai()
      mu = p$mu
      sigma = p$sigma
      
      sim_mat <- sapply(seq_along(mu), function(i) {
        simulateWiener(1000, length(t_ext), mu[i], sigma[i], t_ext)
      })
      matlines(t_ext, sim_mat, lty = 2, lwd = 2)
    }
    
    # Ligne horizontale seuil
    abline(h = seuil, col = "red", lwd = 2)
  })
}

# -------------------------------
# Lancement de l'application
# -------------------------------
shinyApp(ui, server)
