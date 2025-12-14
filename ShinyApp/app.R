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
        numericInput("traj_nbr", "Nb trajectoires :", 7),
        numericInput("rate","taux :",5)
      ),
      
      plotOutput("plot2")
    )
  ),
  
  # ---- PAGE 3 : Analyse d'un CSV ----
  nav_panel(
    "CSV plot",
    page_sidebar(
      sidebar = sidebar(
        fileInput("file", "Upload CSV File", accept = ".csv"),
        selectInput(
          inputId = "choix_du_modele",
          label = "Le choix du modele d'estimation",
          choices = c("maximum de vraisemblance", "moments")
        ),
        checkboxInput("log_it", "Log the dataset"),
        checkboxInput("takeoff_data", "Enlever les courbes reelles"),
        numericInput("seuil", "Seuil horizontal :", value = 10),
        verbatimTextOutput("csv_stats"), #Affichage statistiques
        conditionalPanel(
          condition = "input.choix_du_modele == 'maximum de vraisemblance'",
          checkboxInput("show_stats_mv", "Afficher statistiques"),
          selectInput(
            inputId = "one_or_more",
            label = "nombre de trajectoires",
            choices = c("1 seul trajectoire", "tous les trajectoires")
          )
        ),
        conditionalPanel(
          condition = "input.choix_du_modele == 'moments'",
          checkboxInput("show_stats_mom", "Afficher statistiques")
        ),
        conditionalPanel(
          condition = "input.choix_du_modele == ' '"
        )
        )
      ,
      mainPanel(
        plotOutput("plot3", height = "800px")
      )
  )),
  nav_panel(
    "Processus Weiner avec maintenance imparfaite",
    page_sidebar(
      sidebar = sidebar(
        selectInput(
          inputId = "model_maint",
          label = "Modèle de maintenance imparfaite",
          choices = c("Pas de maintenance","ARD fixe","ARD1","Changement de drift","kijima")
        ),
        numericInput("pt_nbr", "nombre de points ",1000),
        numericInput("mu", "paramètre de tendance :", 1),
        numericInput("sigma2", "paramètre de variabilité :", 5),
        numericInput("T","intervalle de temps :" ,50), 
        numericInput("S","seuil :", 40),
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
    Z_k = rnorm(N) # Coefficients aléatoires
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
    T = input$T
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
      plot(t,simulateWiener(N, L, mu, sigma, T),type='l')
      abline(0, mu, col='red')
    } else {
      # Plusieurs trajectoires
      results = replicate(traj_nbr, simulateWiener(N, L, mu, sigma, T))
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
    T = input$T
    
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
      plot(t,simulateGamma(nbr_pts, forme, taux, T),type='l')
    } else {
      results = replicate(traj_nbr, simulateGamma(nbr_pts, forme, taux, T))
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
  FILE <- reactiveVal(NULL)
  observeEvent(input$file,{
    req(input$file)
    
    filename <- input$file$name
    if(filename == "Laser.csv"){
      FILE("LASER")
    }
    else if (filename == "Semicond.csv"){
      FILE("SEMI_CONDUCTEUR")
    }
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
    log_t = log(t)
    n = length(t)
    delta_t = diff(log_t)
    mu = numeric(ncol(df) - 1)
    sigma = numeric(ncol(df) - 1)
    for (i in 1:length(df[-1])) {
      X = log(df[-1][,i])
      delta_X = diff(X)
      mu[i] = (X[n]-X[1])/(log_t[n]-log_t[1])
      sigma[i] = sqrt(1/n*sum((delta_X - mu[i]*delta_t)^2/delta_t))
    }
    return (list(t = t, mu = mu, sigma = sigma))
  })
  paramsWeinerplusieurstraj <- reactive({
    df <- data()
    t = df[[1]]
    x <- log(df[[1]])
    n = length(x)
    m = length(df[-1])
    y_mat = matrix(0,nrow=length(df[-1]), ncol = length(df[-1][,1]))
    for(i in 1:length(df[-1])){
      y_mat[i,] = log(df[-1][,i])
    }  
    y = y_mat[,2:n]-y_mat[,1:(n-1)]
    delta_x = sapply(1:m, function(i){
      y_mat[i,n]-y_mat[i,1]
    })
    delta_t = diff(x)
    d_tmat = matrix(delta_t, nrow = m, ncol = length(delta_t), byrow = TRUE)
    mu <- sum(delta_x)/((x[n]-x[1])*m)
    sigma <- sqrt(1/(n*m)*sum(((y-mu*d_tmat)^2)/d_tmat))
    res <- c(mu,sigma)
    return(res)
  })
  
  #Gamma par max de vraisemblance
  paramsGammaMV <- reactive({
    req(input$show_Gamma_Simu)
    df <- data()
    T <- max(df[[1]])
    df_sans_temps <- df[-1]
    nbr_pts <- nrow(df_sans_temps)
    
    ind <- seq(1, 200, length.out = 5000)
    x_n_vect <- numeric(ncol(df_sans_temps))
    
    s_vect <- sapply(1:ncol(df_sans_temps), function(i) {
      
      colonne_i <- df_sans_temps[, i]
      delta_X <- c(colonne_i[1], diff(colonne_i))
      Log_delta_X <- log(delta_X)
      
      x_n_vect[i] <<- colonne_i[length(colonne_i)]
      sum(Log_delta_X)
    })
    
    func <- function(b, s, x_n, nbr_pts) {
      nbr_pts * log(b) + s - nbr_pts * digamma(b * x_n / nbr_pts)
    }
    
    fct <- sapply(1:length(s_vect), function(j) {
      sapply(ind, function(b) func(b, s_vect[j], x_n_vect[j], nbr_pts))
    })
    
    b_ech <- numeric(ncol(df_sans_temps))
    a_ech <- numeric(ncol(df_sans_temps))
    
    for (j in 1:ncol(fct)) {
      idx <- which(fct[-1, j] * fct[-nrow(fct), j] < 0)
      b_ech[j] <- ind[idx[1]]
      a_ech[j] <- b_ech[j] * x_n_vect[j] / T
    }
    return(list(a = a_ech, b = b_ech))
  })
  
  # ---- Plot CSV + simulations ----
  output$plot3 <- renderPlot({
    show_data <- !isTRUE(input$takeoff_data)
    show_gamma_Moments  <- isTRUE(input$show_Gamma_Moments)
    show_wiener_Vrai <- isTRUE(input$show_Wiener_Vrai)
    
    req(show_data || show_gamma_Moments || show_wiener_Vrai)
    
    df <- data()
    req(df)
    t <- df[[1]]
    Y <- df[-1] 
    FILE_TYPE = FILE()
    if(FILE_TYPE == "SEMI_CONDUCTEUR"){
      t <- log(t)
      log_y_col <- log(Y)
      matplot(t,log_y_col, type = "b",pch = 16)
      if(input$choix_du_modele == "maximum de vraisemblance"){
        if(input$one_or_more == "1 seul trajectoire"){
          t_ext <- t
          delta_t <- diff(t_ext)
          p <- paramsWienerMaxVrai()
          mu = p$mu
          sigma = p$sigma
          sim_mat <- sapply(1:length(mu), function(i) {
            rnorm(length(t_ext),mean = mu[i]*delta_t, sd = sigma[i]*sqrt(delta_t))
            #simulateWiener(1000, length(t_ext), mu[i], sigma[i], t[length(t)])
          })
          sim_maty= sapply(1:ncol(sim_mat), function(j){
            cumsum(sim_mat[,j])
          })
          matplot(t_ext, sim_maty,,type = 'b',pch = 16)
        } else{#plusieurs trajectoires
          t_ext <- t
          delta_t <- diff(t_ext)
          p <- paramsWeinerplusieurstraj()
          mu = p[1]
          sigma = p[2]
          sim <- rnorm(length(t_ext), mean = mu*delta_t, sd = sigma*sqrt(delta_t))
          y_sim <- cumsum(sim)
          lines(t_ext,y_sim, col = 'gold',type = 'b',pch = 16)
        }
      }else{#moments
        
      }
      }else{#laser
        if(input$choix_du_modele == "maximum de vraisemblance"){
          
        }else{
          if(input$show_stats_mom){
            matplot(df[[1]],df[-1],type="b",pch = 16)
            p<-paramsGammaMoments()
            cat("Paramètres Gamma — méthode des moments\n")
            cat("a =\n"); print(p$a)
            cat("b =\n"); print(p$b)
          }
        }
    }
    #log_it <- isTRUE(input$log_it)
    #x <- if (log_it) log(df[[1]]) else df[[1]]
    #y_col <- if (log_it) log(df[-1]) else df[-1]
    
    # Extension du domaine temporel pour prévoir dans le futur
    #t_max <- max(x, na.rm = TRUE)
    #xlim <- c(0, 3 * t_max)
    #t_ext <- seq(from = min(x), to = xlim[2], length.out = 200)
    
    #ylim <- c(0, seuil * 1.2)
    
    # Plot vide
    #plot(NA, xlim = xlim, ylim = ylim,
    #     xlab = "X", ylab = "Valeurs",
    #     main = "Courbes depuis CSV", type = "n")
    
    # Courbes réelles
    #if (show_data) matlines(x, y_col, type = "b", pch = 16, lty = 1)
    
    # Simulations Gamma
      
      sim_mat <- sapply(seq_along(a), function(i) {
        simulateGamma(length(t_ext), a[i], b[i], t_ext)
      })
      
      # Moyenne + trajectoires
      lines(t_ext, rowMeans(sim_mat), col = "blue", lwd = 3)
      matlines(t_ext, sim_mat, lty = 2, lwd = 2)
    
    output$csv_stats <- renderPrint({
      req(input$show_stats)
      if (input$choix_du_modele == "moments") {
        p <- paramsGammaMoments()
        cat("Paramètres Gamma — méthode des moments\n")
        cat("a =\n"); print(p$a)
        cat("b =\n"); print(p$b)
      } else {
        p <- paramsGammaMV()
        cat("Paramètres Gamma — maximum de vraisemblance\n")
        cat("a =\n"); print(p$a)
        cat("b =\n"); print(p$b)
      }
    })
    
    
    # Ligne horizontale seuil
    seuil <- input$seuil
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
    idx_maint <- floor(seq(1, L, length.out = nbr_maint + 2))
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
    X_maintenancefixe[L] =X[L]-petit_delta*(length(idx_maint)-1)
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
  X_stored <- reactiveVal(NULL)
  t_stored <- reactiveVal(NULL)
  observeEvent(
    list(input$pt_nbr, input$mu, input$sigma2, input$T),
    {
      L <- input$pt_nbr
      sigma <- sqrt(input$sigma2)
      sim <- simulateWiener(N = 1000,L = L,mu = input$mu,sigma = sigma,T = input$T)
      
      t_stored(seq(0, input$T, length.out = L))
      X_stored(sim)
    },
    ignoreInit = TRUE
  )
  output$plot4 <- renderPlot({
    req(X_stored(), t_stored())
    L <- input$pt_nbr
    X <- X_stored()
    t <- t_stored()
    plot(t, X, type = 'l')
    abline(h = input$S, col ="purple")
    X_res <- rep(0, L)
    if (input$model_maint =="ARD fixe") {
      ARD_fixe(X, X_res, input$delta, input$nbr_maint, input$T,L)
      
    } else if (input$model_maint == "ARD1") {
      ARD_1(X, X_res, input$rho, input$nbr_maint,input$T,L)
      
    } else if (input$model_maint == "Changement de drift") {
      drift_change(X, X_res,input$mu, input$alpha, input$beta, input$nbr_maint,input$T,L)
    } else if (input$model_maint == "kijima"){
    }
    
  })
}

# -------------------------------
# Lancement de l'application
# -------------------------------
shinyApp(ui, server)
