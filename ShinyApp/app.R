library(shiny)
library(bslib)
library(statmod)

# -------------------------------
# INTERFACE UTILISATEUR (UI)
# -------------------------------
ui <- page_navbar(
  title = "Étude des modèles de dégradation",
  
  # ---- PAGE 1 : Processus de Wiener ----
  nav_panel(
    "Processus Wiener",
    
    tabsetPanel(
      # ===== TAB 1 : Trajectoires =====
      tabPanel(
        "Trajectoires",
        page_sidebar(
          sidebar = sidebar(
            numericInput("pt_nbr_1", "Nombre points :", 100),
            numericInput("moyenne_1", "Moyenne :", 10),
            numericInput("variance_1", "Variance :", 10),
            numericInput("traj_nbr_1", "Nb trajectoires :", 7),
            numericInput("T_1","Durée",10),
            numericInput("seuil_1", "Seuil :", 10)
          ),
          mainPanel(
            plotOutput("plot1",height = "800px")
          )
        )
      ),
      
      # ===== TAB 2 : Moyenne =====
      tabPanel(
        "simulation par max_de_vraisemblance",
        
        sidebarLayout(
          sidebarPanel(
            numericInput("pt_nbr_2", "Nombre points :", 100),
            numericInput("moyenne_2", "Moyenne :", 10),
            numericInput("variance_2", "Variance :", 10),
            numericInput("traj_nbr_2", "Nb trajectoires :", 7),
            numericInput("T_2","Durée_2",100),
            numericInput("seuil_2", "Seuil :", 1)
          ),
          mainPanel(
            plotOutput("plot1_mean",height = "800px")
          )
        )
      ),
      
      # ===== TAB 3 : Distribution =====
      tabPanel(
        "Distribution finale",
        sidebarLayout(
          sidebarPanel(
            numericInput("pt_nbr_3", "Nombre points :", 100),
            numericInput("moyenne_3", "Moyenne :", 8),
            numericInput("variance_3", "Variance :", 25),
            numericInput("traj_nbr_3", "Nb trajectoires :", 1000),
            numericInput("T_3","Durée",500),
            numericInput("seuil_3", "Seuil :", 80)
          ),
          mainPanel(
            plotOutput("plot1_hist",height = "800px")
          )
        )
      )
    )
  ),
  
  
  # ---- PAGE 2 : Processus Gamma ----
  nav_panel(
    "Processus Gamma",
    
    tabsetPanel(
      # ===== TAB 1 : Trajectoires =====
      tabPanel(
        "Trajectoires",
        page_sidebar(
          sidebar = sidebar(
            numericInput("pt_nbr_4", "Nombre points :", 100),
            numericInput("forme_4", "Forme :", 10),
            numericInput("taux_4", "Taux :", 10),
            numericInput("traj_nbr_4", "Nb trajectoires :", 7),
            numericInput("T_4","Durée",10),
          ),
          mainPanel(
            plotOutput("plot2",height = "800px")
          )
        )
      ),
      
      # ===== TAB 2 : Moyenne =====
      tabPanel(
        "simulation par max_de_vraisemblance et moments",
        
        sidebarLayout(
          sidebarPanel(
            numericInput("pt_nbr_5", "Nombre points :", 100),
            numericInput("forme_5", "Forme :", 10),
            numericInput("taux_5", "Taux :", 10),
            numericInput("traj_nbr_5", "Nb trajectoires :", 7),
            numericInput("T_5","Durée",10),
          ),
          mainPanel(
            plotOutput("plot2_mean",height = "800px")
          )
        )
      ),
      
      # ===== TAB 3 : Distribution =====
      tabPanel(
        "Distribution finale",
        sidebarLayout(
          sidebarPanel(
            numericInput("pt_nbr_6", "Nombre points :", 100),
            numericInput("forme_6", "Forme :", 10),
            numericInput("taux_6", "Taux :", 10),
            numericInput("traj_nbr_6", "Nb trajectoires :", 700),
            numericInput("T_6","Durée",90),
            numericInput("seuil_6", "Seuil :", 70)
          ),
          mainPanel(
            plotOutput("plot2_hist",height = "800px")
          )
        )
      )
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
          choices = c("pas d'estimation","maximum de vraisemblance", "moments")
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
          checkboxInput("show_stats_mom", "Afficher statistiques"),
          selectInput(
            inputId = "one_or_more",
            label = "nombre de trajectoires",
            choices = c("1 seul trajectoire", "tous les trajectoires")
          )
        ),
        conditionalPanel(
          condition = "input.choix_du_modele == 'pas d'estimation'"
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
          condition = "input.model_maint == 'kijima'"
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
    
    Z_k <- rnorm(N)
    t <- seq(0, T, length.out = L)
    
    j <- 1:N
    coef <- sqrt(8 * T) / ((2 * j + 1) * pi)
    
    B <- numeric(L)
    
    for (i in 1:L) {
      u <- t[i]
      e_ju <- coef * sin((2 * j + 1) * pi * u / (2 * T))
      B[i] <- sum(Z_k * e_ju)
    }
    
    mu * t + sigma * B
  }
  
  # --- Plot du Wiener ---
  output$plot1 <- renderPlot({
    mu = input$moyenne_1
    sigma = sqrt(input$variance_1)
    traj_nbr = input$traj_nbr_1
    L = input$pt_nbr_1
    T = input$T_1
    N=1000
    
    # Construction de l'échelle temporelle
    #if (model == "Constant") {
    #  t = seq(0,T,length.out = L)
    #} else {
    #  delta_t = runif(L - 1)
    #  t = numeric(L)
     # t[1] = 0
    #  for (i in 2:L) t[i] = t[i - 1] + delta_t[i - 1]
    #}
    t = seq(0,T,length.out = L)
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
  output$plot1_mean <- renderPlot({
    mu = input$moyenne_2
    sigma = sqrt(input$variance_2)
    traj_nbr = input$traj_nbr_2
    L = input$pt_nbr_2
    N = 1000
    T = input$T_2
    #max de vraisemblance à plusieurs trajectoires
    results = replicate(traj_nbr, simulateWiener(N, L, mu, sigma, T))
    t <- seq(0, T, length.out = L)
    delta_x <-apply(results,2,diff)
    delta_t = diff(t)
    d_tmat = matrix(delta_t, nrow = L-1, ncol = traj_nbr)
    mu_estimé <- sum(delta_x)/((t[L]-t[1])*traj_nbr)
    sigma_estimé <- sqrt(1/((L-1)*traj_nbr)*sum(((delta_x-mu_estimé*d_tmat)^2)/d_tmat))
    res <- c(mu_estimé,sigma_estimé)
    #momentsà plusieurs trajectoires
    S <- sapply(1:traj_nbr,function(i){
      sum(delta_x[,i])
    })
    mu_est <- mean(S)/T
    sigma_est = sqrt(var(S)/T)
    sim_1 <- simulateWiener(N,L,res[1],res[2],T)
    B <- (sim_1-res[1]*t)/res[2]
    plot(t,sim_1,type = 'l', col ="darkgreen")
    lines(t,mu_est*t+sigma_est*B,type = 'l', col ="hotpink")
    legend("topleft", col = c("orange","purple"), lty = 1, cex = .8,
           legend = c(paste("sigma estimé élevé au carré = ",res[2]^2),
                      paste("mu estimé =  ",res[1])))
  })
  output$plot1_hist <- renderPlot({
    mu = input$moyenne_3
    sigma = sqrt(input$variance_3)
    traj_nbr = input$traj_nbr_3
    L = input$pt_nbr_3
    N = 1000
    T = input$T_3
    h = input$seuil_3
    results = replicate(traj_nbr, simulateWiener(N, L, mu, sigma, T))
    t <- seq(0, T, length.out = L)
    liste_t <- apply(results,2,function(x){
      t[which(x>=h)[1]]
    })
    liste_t <- liste_t[!is.na(liste_t)]
    hist(liste_t, prob = TRUE, breaks = 30,
         main = "Sans maintenance", xlab = "Temps de traversée")
    xx <- seq(min(liste_t), max(liste_t), length.out = 2000)
    
    lines(xx, dinvgauss(xx, mean = h/mu, shape = h^2/(sigma^2)),
          col = "red", lwd = 2)
  })
  
  # ============================================================
  # PAGE 2 : Processus Gamma
  # ============================================================
  
  # Simule un processus Gamma (increments indépendants Gamma)
  simulateGamma = function(nbr_pts, forme, taux, T) {
    G = rep(0,nbr_pts)
    x = numeric(nbr_pts)
    x[1] = 0
    delta_x <- rgamma(nbr_pts, shape = forme * T/nbr_pts, rate = taux)
    
    # Processus cumulé
    for (i in 2:nbr_pts) x[i] = delta_x[i] + x[i - 1]
    
    return(x)
  }
  
  output$plot2 <- renderPlot({
    traj_nbr = input$traj_nbr_4
    nbr_pts = input$pt_nbr_4
    forme = input$forme_4
    taux = input$taux_4
    T = input$T_4
    
    t = seq(0,T,length.out = nbr_pts)
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
  output$plot2_mean <- renderPlot({
    a = input$forme_5
    b = input$taux_5
    traj_nbr = input$traj_nbr_5
    L = input$pt_nbr_5
    T = input$T_5
    results = replicate(traj_nbr, simulateGamma(L, a, b, T))
    t <- seq(0, T, length.out = L)
    nbr_pts <- L
    #max vraisemblance
    ind <- seq(1, 1000000, length.out = 500000)
    x_n_vect <- numeric(traj_nbr)
    s_vect <- sapply(1:traj_nbr, function(i) {
      
      traj_i <- results[, i]
      delta_Xi <- diff(traj_i)
      Log_delta_Xi <- log(delta_Xi)
      Log_delta_Xi <- Log_delta_Xi[!is.infinite(Log_delta_Xi)]
      s_vect[i] = sum(Log_delta_Xi)
    })
    s <- sum(s_vect)
    x_n_vect <- sapply(1:traj_nbr,function(i){
      results[,i][L]
    })
    func <- function(b, s, x_n, nbr_pts,traj_nbr) {
      nbr_pts * traj_nbr*log(b) + s - nbr_pts *traj_nbr* digamma(b * sum(x_n) /(nbr_pts*traj_nbr))
    }
    fct <- sapply(ind,function(b){
      func(b, s, x_n_vect, nbr_pts,traj_nbr)
    })
    idx <- which(fct[-1]* fct[-length(fct)] < 0)
    b_est = ind[idx[1]]
    a_est= (b_est*sum(x_n_vect))/(traj_nbr*T)
    #méthode des moments
    delta_results = apply(results,2,diff)
    S <- sapply(1:traj_nbr,function(i){
      sum(delta_results[,i])
    })
    a_estimé = mean(S)^2/(var(S)*T)
    b_estimé = mean(S)/var(S)
    plot(t,simulateGamma(L,a_est,b_est,T),col = "blue",type="l")
    lines(t,simulateGamma(L,a_estimé,b_estimé,T),col = "cyan",type = "l")
    legend("topleft", col = c("blue","cyan","orange","purple"), lty = 1, cex = .8,
           legend = c("max de vraisemblance","moments", paste("a = ",a_est),
                      paste("b =  ",b_est)))
    
  })
  output$plot2_hist <- renderPlot({
    forme = input$forme_6
    taux = input$taux_6
    traj_nbr = input$traj_nbr_6
    L = input$pt_nbr_6
    T = input$T_6
    h = input$seuil_6
    results = replicate(traj_nbr, simulateGamma(L, forme, taux,T))
    t <- seq(0, T, length.out = L)
    liste_t <- apply(results,2,function(x){
      t[which(x>=h)[1]]
    })
    liste_t <- liste_t[!is.na(liste_t)]
    hist(liste_t, prob = TRUE, breaks = 30,
         main = "Sans maintenance", xlab = "Temps de traversée")
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
  #Weiner -> Moments -> plusieurs trajectoires
  paramweinerMomentsplusieurstraj <- reactive({
    df <- data()
    t = df[[1]]
    log_t = log(t)
    Y = log(df[-1])
    n = length(t)
    delta_t = diff(log_t)
    delta_X = apply(Y,2,diff)
    s <- sapply(1:ncol(Y),function(i){
      sum(delta_X[,i])
    })
    mu <- mean(s)/sum(delta_t)
    sigma <- sqrt(var(s)/sum(delta_t))
    return (list(mu = mu, sigma = sigma))
  })
  #   Gamma -> Moments -> 1 trajectoire
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
  
  #Weiner -> Max vrai -> 1 trajectoire
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
  
  # Weiner -> Moments -> 1 seul trajectoire
  paramsweinerMoments <- reactive({
    df<- data()
    t = df[[1]]
    x <- log(df[[1]])
    n = length(x)
    m = length(df[-1])
    mu_vect = numeric(m)
    sigma_vect = numeric(m)
    log_t = log(t)
    Y = log(df[-1])
    for(i in 1:length(df[-1])){
      y_col <- log(df[-1][,i])
      y = diff(y_col) #1
      delta_t = diff(x) #2
      mu =  (y_col[n]-y_col[1])/(x[n]-x[1]) #3
      mu_vect[i] = mu
      sigma = sqrt(1/n*sum(((y-mu*delta_t)^2)/delta_t))
      sigma_vect[i] = sigma
    }
    sim_mat <- sapply(1:length(mu_vect), function(i) {
      rnorm(length(t),mean = mu_vect[i]*delta_t, sd = sigma_vect[i]*sqrt(delta_t))
    })
    sim_maty= sapply(1:ncol(sim_mat), function(j){
      cumsum(sim_mat[,j])
    })
    return(list(mu= mu_vect,sigma= sigma_vect))
  })
  
  # Weiner -> Max vrai -> plusieurs traj
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
  
  #Gamma par max de vraisemblance -> 1 trajectoire
  paramsGammaMV <- reactive({
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
  #Gamma -> Max vrai -> plusieurs trajectoires
  paramsGammaMVplusieurstraj <- reactive({
    df <- data()
    t <- df[[1]]
    T <- sum(diff(t))
    Y <- df[-1]
    nbr_pts <- nrow(Y)
    traj_nbr <- ncol(df[-1])
    ind <- seq(1, 2000, length.out = 5000)
    x_n_vect <- numeric(traj_nbr)
    s_vect <- sapply(1:traj_nbr, function(i) {
      traj_i <- Y[, i]
      delta_Xi <- diff(traj_i)
      Log_delta_Xi <- log(delta_Xi)
      Log_delta_Xi <- Log_delta_Xi[!is.infinite(Log_delta_Xi)]
      sum(Log_delta_Xi)
    })
    s <- sum(s_vect)
    x_n_vect <- sapply(1:traj_nbr,function(i){
      Y[,i][nbr_pts]
    })
    func <- function(b, s, x_n, nbr_pts,traj_nbr) {
      nbr_pts * traj_nbr*log(b) + s - nbr_pts *traj_nbr* digamma(b * sum(x_n) /(nbr_pts*traj_nbr))
    }
    fct <- sapply(ind,function(b){
      func(b, s, x_n_vect, nbr_pts,traj_nbr)
    })
    idx <- which(fct[-1]* fct[-length(fct)] < 0)
    b_est = ind[idx]
    a_est= (b_est*sum(x_n_vect))/(traj_nbr*T)
    res <- c(a_est,b_est)
    return(res)
  })
  #Gamma -> moments -> plusieurs traj 
  paramsGammaMomentsPlusieurstraj <- reactive({
    df <- data()
    t <- df[[1]]
    Y <- df[-1]
    delta_Y = apply(Y,2,diff)
    S <- sapply(1:ncol(Y),function(i){
      sum(delta_Y[,i])
    })
    a_estimé = mean(S)^2/(var(S)*T)
    b_estimé = mean(S)/var(S)
    return(list(a = a_estimé,b= b_estimé))
  })
  # ---- Plot CSV + simulations ----
  output$plot3 <- renderPlot({
    df <- data()
    req(df)
    t <- df[[1]]
    Y <- df[-1] 
    FILE_TYPE = FILE()
    if(FILE_TYPE == "SEMI_CONDUCTEUR"){
      t <- log(t)
      log_y_col <- log(Y)
      if(input$choix_du_modele == "pas d'estimation"){
        matplot(t,log_y_col, type = "b",pch = 16,lty = 1)
        
      }else if(input$choix_du_modele == "maximum de vraisemblance"){
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
          matplot(t_ext, sim_maty,type = 'b',pch = 16)
        } else{#plusieurs trajectoires
          t_ext <- t
          delta_t <- diff(t_ext)
          p <- paramsWeinerplusieurstraj()
          mu = p[1]
          sigma = p[2]
          sim <- rnorm(length(t_ext), mean = mu*delta_t, sd = sigma*sqrt(delta_t))
          y_sim <- cumsum(sim)
          matplot(t,log_y_col, type = "b",pch = 16,lty = 1)
          lines(t_ext,y_sim, col = 'gold',type = 'b',pch = 16)
        }
      }else{#moments
        if (input$one_or_more == "tous les trajectoires"){
        t_ext <- t
        delta_t <- diff(t_ext)
        p <- paramweinerMomentsplusieurstraj()
        mu<- p$mu
        sigma <- p$sigma
        sim <- rnorm(length(t_ext), mean = mu*delta_t, sd = sigma*sqrt(delta_t))
        y_sim <- cumsum(sim)
        matplot(t,log_y_col, type = "b",pch = 16,lty = 1)
        lines(t_ext,y_sim, col = 'gold',type = 'b',pch = 16,lty = 1)
        } else{
          p<-paramsweinerMoments()
          t_ext <- t
          mu_vect <- p$mu
          sigma_vect <- p$sigma
          sim <- sapply(1:length(mu_vect),function(i){
            tr<- rnorm(length(t_ext),mean = mu_vect[i]*diff(t_ext),sd = sigma_vect[i]*sqrt(diff(t_ext)))
            cumsum(tr)
          })
          matplot(t_ext,sim,type= "b",pch = 16,lty = 1)
        }
        }
      }else{#laser
        t <- df[[1]]
        Y <- df[-1] 
        if (input$choix_du_modele == "pas d'estimation"){
          matplot(t,Y, type = "b",pch = 16, lty = 1)
        }else if(input$choix_du_modele == "maximum de vraisemblance"){
          if (input$one_or_more == "tous les trajectoires"){
            p<- paramsGammaMVplusieurstraj()
            a<- p[1]
            b <- p[2]
            sim <- rgamma(length(t),shape = a*diff(t),rate = b)
            y_sim <- cumsum(sim)
            plot(t,y_sim,type = 'b',col= "aquamarine",pch = 16,lty = 1)
          }else{
            p<- paramsGammaMV()
            a_ech <- p$a
            b_ech <- p$b
            sim <- sapply(1:length(a_ech),function(i){
              tr<- rgamma(length(t),shape = a_ech[i]*diff(t)[1],rate = b_ech[i])
              cumsum(tr)
            })
            matplot(t,sim,type= "b",pch = 16,lty = 1)
          }
        }else{#moments
          if (input$one_or_more == "1 seul trajectoire"){
            p<-paramsGammaMoments()
            a_ech <- p$a
            b_ech <- p$b
            sim <- sapply(1:length(a_ech),function(i){
              tr<- rgamma(length(t),shape = a_ech[i]*diff(t)[1],rate = b_ech[i])
              cumsum(tr)
            })
            matplot(t,sim,type= "b",pch = 16,lty = 1)
          }else{
            p<- paramsGammaMomentsPlusieurstraj()
            a_est <- p$a
            b_est <- p$b
            sim <- rgamma(length(t),shape = a*diff(t),rate = b)
            y_sim <- cumsum(sim)
            plot(t,y_sim,type = 'b',col= "purple",pch = 16,lty = 1)
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
      
      #sim_mat <- sapply(seq_along(a), function(i) {
       # simulateGamma(length(t_ext), a[i], b[i], t_ext)
      #})
      
      # Moyenne + trajectoires
      #lines(t_ext, rowMeans(sim_mat), col = "blue", lwd = 3)
      #matlines(t_ext, sim_mat, lty = 2, lwd = 2)
    

    
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
