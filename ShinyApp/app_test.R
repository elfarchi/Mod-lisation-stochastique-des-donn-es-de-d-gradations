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
        numericInput("T","Durée",10),
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
        numericInput("traj_nbr", "Nb trajectoires :", 1),
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
        checkboxInput("show_Gamma_Simu", "Afficher les simulations de Gamma"),
        checkboxInput("takeoff_data", "Enlever les courbes reelles"),
        numericInput("seuil", "Seuil horizontal :", value = 10),
        hr(),
        verbatimTextOutput("csv_stats")   # Affichage statistiques
      ),
      
      mainPanel(
        plotOutput("plot3", height = "800px")
      )
    )
  ),
  # # ---- PAGE 4: Processus Weiner avec maintenance imparfaite ----
  nav_panel(
    "Processus Weiner avec maintenance imparfaite",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "model_maint",
          label = "Modèle de maintenance imparfaite",
          choices = c("Pas de maintenance","ARD fixe","ARD1","Changement de drift","kijima")
        ),
        numericInput("pt_nbr", "nombre de points ",1000),
        numericInput("mu", "paramètre de tendance :", 1),
        numericInput("sigma2", "paramètre de variabilité :", 5),
        numericInput("T","intervalle de temps :" ,50), 
        conditionalPanel(
          condition = "input.model_maint == 'ARD fixe'",
          numericInput("nbr_maint","Nombre de maintenances :", 4),
          numericInput("delta","constante de réduction :", 0)
          
        ),
        conditionalPanel(
          condition = "input.model_maint == 'ARD1'",
          numericInput("nbr_maint","Nombre de maintenances", 4),
          numericInput("rho","taux de restauration", 0)
  
        ),
        conditionalPanel(
          condition = "input.model_maint == 'Changement de drift'",
          numericInput("nbr_maint","Nombre de maintenances", 4),
          numericInput("alpha","facteur de multiplication", 1),
          numericInput("beta","facteur de modification alpha/beta", 1)
        ),
        conditionalPanel(
          condition = "input.model_maint == 'kijima'",
        ),
        conditionalPanel(condition = "input.model_maint == 'Pas de maintenance")
      ),
      mainPanel(
        plotOutput("plot4", height = "800px"),
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
  simulateWiener = function(N, L, mu, sigma, T) {
    Z_k = rnorm(N)          # Coefficients aléatoires
    t = seq(0,T,length.out = L)
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
      t = seq(0,input$T,length.out = L)
    } else {
      delta_t = runif(L - 1)
      t = numeric(L)
      t[1] = 0
      for (i in 2:L) t[i] = t[i - 1] + delta_t[i - 1]
    }
    
    # Cas une seule trajectoire
    if (traj_nbr == 1){
      plot(seq(0,input$T,length.out = L),simulateWiener(N, L, mu, sigma, input$T),type='l')
      abline(0, mu, col='red')
    } else {
      # Plusieurs trajectoires
      results = replicate(traj_nbr, simulateWiener(N, L, mu, sigma, input$T))
      plot(seq(0,input$T,length.out = L), results[, 1],type= 'l')
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
    show_sim  <- isTRUE(input$show_Gamma_Simu)
    
    req(show_data || show_sim)
    
    df <- data()
    req(df)
    
    # Logging éventuel
    log_it <- isTRUE(input$log_it)
    x <- if (log_it) log(df[[1]]) else df[[1]]
    y_col <- if (log_it) log(df[-1]) else df[-1]
    
    # Extension du domaine temporel pour prévoir dans le futur
    t_max <- max(x, na.rm = TRUE)
    xlim <- c(0, 3 * t_max)
    t_ext <- seq(from = min(x), to = xlim[2], length.out = 200)
    
    seuil <- input$seuil
    ylim <- c(0, seuil * 1.2)
    
    # Plot vide
    plot(NA, xlim = xlim, ylim = ylim,
         xlab = "X", ylab = "Valeurs",
         main = "Courbes depuis CSV", type = "n")
    
    # Courbes réelles
    if (show_data) matlines(x, y_col, type = "b", pch = 16, lty = 1)
    
    # Simulations Gamma
    if (show_sim) {
      p <- paramsGammaMoments()
      a <- p$a
      b <- p$b
      
      sim_mat <- sapply(seq_along(a), function(i) {
        simulateGamma(length(t_ext), a[i], b[i], t_ext)
      })
      
      # Moyenne + trajectoires
      lines(t_ext, rowMeans(sim_mat), col = "blue", lwd = 3)
      matlines(t_ext, sim_mat, lty = 2, lwd = 2)
    }
    
    # Ligne horizontale seuil
    abline(h = seuil, col = "red", lwd = 2)
  })
  
  # ============================================================
  # PAGE 4 : Processus Weiner avec maintenance imparfaite
  # ============================================================  
  ARD_1 <- function(X,X_maintenance,rho,nbr_maint,T,L){
    nbr_maint = nbr_maint+1
    idx_maint <- floor(seq(1, L, length.out = nbr_maint + 2))
    t <- seq(0, T, length.out = L)
    t_maintenance = rep(0,nbr_maint+1)
    
    for( i in 1:(nbr_maint+1)){
      t_maintenance[i]= t[idx_maint[i]]
    }
    X_maintenance[1:(idx_maint[2]-1)] = X[1: (idx_maint[2]-1)]
    delta <- 0
    for(i in 2:(length(idx_maint)-1)){
      X_maintenance[idx_maint[i]] = (1-rho)*X[idx_maint[i]]
      delta = X[idx_maint[i]]-X_maintenance[idx_maint[i]]
      X_maintenance[(idx_maint[i]+1):(idx_maint[i+1]-1)] = X[(idx_maint[i]+1):(idx_maint[i+1]-1)]-delta
    }
    abline(v = t_maintenance[2:(length(t_maintenance) - 1)],
           col = "red", lwd = 1)
    X_maintenance[L] =X[L]-delta
    lines(t,X_maintenance, type ='l', col="blue")
  }
  ARD_fixe <- function(X,X_maintenancefixe,petit_delta,nbr_maint,T,L){
    nbr_maint = nbr_maint+1
    idx_maint <- floor(seq(1, L, length.out = nbr_maint + 1))
    t <- seq(0, T, length.out = L)
    t_maintenance = rep(0,nbr_maint+1)
    for( i in (1:nbr_maint+1)){
      t_maintenance[i]= t[idx_maint[i]]
    }
    X_maintenancefixe[1:(idx_maint[2]-1)] = X[1: (idx_maint[2]-1)]
    for(i in 2:(length(idx_maint)-1)){
      X_maintenancefixe[idx_maint[i]:(idx_maint[i+1]-1)] = X[(idx_maint[i]):(idx_maint[i+1]-1)]-petit_delta*i
    }
    abline(v = t_maintenance[2:(length(t_maintenance) - 1)],
           col = "red", lwd = 1)
    X_maintenancefixe[L] =X[L]-petit_delta*(nbr_maint-1)
    lines(t,X_maintenancefixe, type ='l', col="green")
  }
  drift_change <- function(X,X_drift,mu,alpha,beta,nbr_maint,T,L){
    nbr_maint = nbr_maint+1
    idx_maint <- floor(seq(1, L, length.out = nbr_maint + 1))
    t <- seq(0, T, length.out = L)
    t_maintenance = rep(0,nbr_maint+1)
    for( i in 1:(nbr_maint+1)){
      t_maintenance[i]= t[idx_maint[i]]
    }
    X_drift[1:(idx_maint[2]-1)] = X[1: (idx_maint[2]-1)]
    X = X - mu*t
    for(i in 2:(length(idx_maint)-1)){
      alpha = alpha/beta
      X = X+mu*alpha*t
      X_drift[idx_maint[i]:(idx_maint[i+1]-1)] = X[(idx_maint[i]):(idx_maint[i+1]-1)]
      X= X-mu*alpha*t
    }
    abline(v = t_maintenance[2:(length(t_maintenance) - 1)],
           col = "red", lwd = 1)
    X_drift[L]=(X+mu*alpha*t)[L]
    lines(t,X_drift,type='l',col = "cyan")
  }
  output$plot4 <- renderPlot({
    L <- input$pt_nbr
    N = 1000
    sigma = sqrt(input$sigma2)
    X <- simulateWiener(N, L, input$mu, sigma, input$T)
    t <- seq(0, input$T, length.out = L)
    plot(t, X, type = 'l')
    X_res <- rep(0, L)
    if (input$model_maint == "ARD fixe") {
        ARD_fixe(X, X_res, input$delta, input$nbr_maint, input$T,L)
      
    } else if (input$model_maint == "ARD1") {
       ARD_1(X, X_res, input$rho, input$nbr_maint,input$T,L)
      
    } else if (input$model_maint == "Changement de drift") {
        drift_change(X, X_res, input$alpha,input$mu, input$beta, input$nbr_maint,input$T,L)
    } else if (input$model_maint == "kijima"){
        lines(h = 2,col ='orange')
    }
  })
  
  
  
}

# -------------------------------
# Lancement de l'application
# -------------------------------
shinyApp(ui, server)
