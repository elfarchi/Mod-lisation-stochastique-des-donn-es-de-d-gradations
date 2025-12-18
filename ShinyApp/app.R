library(shiny)
library(bslib)
library(statmod)

ui <- page_navbar(
  title = "Étude des modèles de dégradation",
  
  # PAGE 1 : Processus de Wiener
  nav_panel(
    "Processus Wiener",
    
    tabsetPanel(
      # TAB 1 : Trajectoires
      tabPanel(
        "Trajectoires",
        page_sidebar(
          sidebar = sidebar(
            numericInput("pt_nbr_1", "Nombre points :", 1000),
            numericInput("moyenne_1", "Drift :", 8),
            numericInput("variance_1", "Variabilité :", 10),
            numericInput("traj_nbr_1", "Nb trajectoires :", 1),
            numericInput("T_1","Durée",10),
            actionButton("run_weiner_sim","Exécuter" , class = "btn-primary")
          ),
          mainPanel(
            plotOutput("plot1",height = "800px")
          )
        )
      ),
      
      #TAB 2 : Moyenne 
      tabPanel(
        "simulation par max de vraisemblance et moments",
        
        sidebarLayout(
          sidebarPanel(
            numericInput("pt_nbr_2", "Nombre points :", 1000),
            numericInput("moyenne_2", "Drift :", 0.484),
            numericInput("variance_2", "Variabilité :", 0.2905*2),
            numericInput("traj_nbr_2", "Nb trajectoires :", 100),
            numericInput("T_2","Durée",15),
            checkboxInput("biais_1","Biais des estimateurs par méthode des moments "),
            verbatimTextOutput("biais_txt_1"),
            checkboxInput("biais_4","Biais des estimateurs par méthode des MV"),
            verbatimTextOutput("biais_txt_4"),
            actionButton("run_weiner_mean","Exécuter",class = "btn-primary")
          ),
          mainPanel(
            plotOutput("plot1_mean",height = "800px")
          )
        )
      ),

    )
  ),
  
  
  # PAGE 2 : Processus Gamma 
  nav_panel(
    "Processus Gamma",
    
    tabsetPanel(
      # TAB 1 : Trajectoires
      tabPanel(
        "Trajectoires",
        page_sidebar(
          sidebar = sidebar(
            numericInput("pt_nbr_4", "Nombre points :", 1000),
            numericInput("forme_4", "Forme :", 8),
            numericInput("taux_4", "Taux :", 10),
            numericInput("traj_nbr_4", "Nb trajectoires :", 1),
            numericInput("T_4","Durée",10),
            actionButton("run_sim_gamma","Exécuter",class = "btn-primary")
          ),
          mainPanel(
            plotOutput("plot2",height = "800px")
          )
        )
      ),
      
      # TAB 2 : Moyenne 
      tabPanel(
        "simulation par max de vraisemblance et moments",
        
        sidebarLayout(
          sidebarPanel(
            numericInput("pt_nbr_5", "Nombre points :", 1000),
            numericInput("forme_5", "Forme :", 0.0624),
            numericInput("taux_5", "Taux :", 39.972),
            numericInput("traj_nbr_5", "Nb trajectoires :", 100),
            numericInput("T_5","Durée",4000),
            checkboxInput("biais_2","Biais des estimateurs par méthode des moments"),
            verbatimTextOutput("biais_txt_2"),
            checkboxInput("biais_3","Biais des estimateurs par méthode de MV"),
            verbatimTextOutput("biais_txt_3"),
            actionButton("run_gamma_mean", "Exécuter", class = "btn-primary")
          ),
          mainPanel(
            plotOutput("plot2_mean",height = "800px")
          )
        )
      ),
  
    )
  ),
  
  # PAGE 3 : Analyse d'un CSV
  nav_panel(
    "Analyse des données",
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Upload CSV File", accept = ".csv"),
        selectInput(
          inputId = "choix_du_modele",
          label = "Le choix du modèle d'estimation",
          choices = c("pas d'estimation","maximum de vraisemblance", "moments")
        ),
        conditionalPanel(
          condition = "input.choix_du_modele == 'maximum de vraisemblance'",
          selectInput(
            inputId = "one_or_more_MV",
            label = "nombre de trajectoires",
            choices = c("1 seul trajectoire", "tous les trajectoires")
          ),
          numericInput("seuil", "Seuil horizontal :", value = 0),
          checkboxInput("list_t_2","afficher liste du temps de défaillance pour les lasers"),
          verbatimTextOutput("list_ttxt_2"),
          checkboxInput("list_t_1","afficher liste du temps de défaillance pour les semi-conducteurs"),
          verbatimTextOutput("list_ttxt_1"),
        ),
        conditionalPanel(
          condition = "input.choix_du_modele == 'moments'",
          selectInput(
            inputId = "one_or_more_moments",
            label = "nombre de trajectoires",
            choices = c("1 seul trajectoire", "tous les trajectoires")
          ),
          
        ),
        conditionalPanel(
          condition = "input.choix_du_modele == 'pas d'estimation'"
        )
        )
      ,
      mainPanel(
        plotOutput("plot3", height = "800px"),
        br(),
        hr(),
        h4("Estimations trouvées"),
        conditionalPanel(
          condition = "input.one_or_more_MV == '1 seul trajectoire'",
          verbatimTextOutput("estim_MV_txt")
        ),
        conditionalPanel(
          condition = "input.one_or_more_moments == '1 seul trajectoire'",
          verbatimTextOutput("estim_Moments_txt")
        )
      )
      
  )
  )
  
    )

server <- function(input, output, session) {

# PAGE 1 : Simulation du processus de Wiener
  
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
  
  output$plot1 <- renderPlot({
    req(input$run_weiner_sim)
    mu = isolate(input$moyenne_1)
    sigma = isolate(sqrt(input$variance_1))
    traj_nbr = isolate(input$traj_nbr_1)
    L = isolate(input$pt_nbr_1)
    T = isolate(input$T_1)
    N=1000
    t = seq(0,T,length.out = L)
    # Cas une seule trajectoire
    if (traj_nbr == 1){
      plot(t,simulateWiener(N, L, mu, sigma, T),type='l',xlab = "temps",
           ylab = "Dégradation Weiner",main = "Simulation d'une dégradation par le processus de Weiner")
      grid(col = "lightgray", lty = "dotted")
      abline(0, mu, col='red')
    } else {
      # Plusieurs trajectoires
      results = replicate(traj_nbr, simulateWiener(N, L, mu, sigma, T))
      plot(t, results[, 1],type= 'l',xlab = "temps",
           ylab = "Dégradation Weiner",main = "Simulation des dégradations par le processus de Weiner")
      grid(col = "lightgray", lty = "dotted")
      for (i in 2:ncol(results)) lines(t, results[, i])
      abline(0, mu, col='red')
    }
  })
  results_store <- reactiveVal(NULL)
  output$plot1_mean <- renderPlot({
    req(input$run_weiner_mean)
    mu = isolate(input$moyenne_2)
    sigma = isolate(sqrt(input$variance_2))
    traj_nbr = isolate(input$traj_nbr_2)
    L = isolate(input$pt_nbr_2)
    N = 1000
    T = isolate(input$T_2)
    
    #max de vraisemblance à plusieurs trajectoires
    
    results = replicate(traj_nbr, simulateWiener(N, L, mu, sigma, T))
    results_store(results)
    t <- seq(0, T, length.out = L)
    delta_x <-apply(results,2,diff)
    delta_t = diff(t)
    d_tmat = matrix(delta_t, nrow = L-1, ncol = traj_nbr)
    mu_estimé <- round(sum(delta_x)/((t[L]-t[1])*traj_nbr),4)
    sigma_estimé <-round( sqrt(1/(L*traj_nbr)*sum(((delta_x-mu_estimé*d_tmat)^2)/d_tmat)),4)
    res <- c(mu_estimé,sigma_estimé)
    
    #moments à plusieurs trajectoires
    
    S <- sapply(1:traj_nbr,function(i){
      sum(delta_x[,i])
    })
    mu_est <- round(mean(S)/sum(delta_t),4)
    Y = S/sqrt(T)
    sigma_est <- round(sqrt(
      mean((delta_x - mu_est * d_tmat)^2 / d_tmat)),4)
    sim_1 <- simulateWiener(N,L,res[1],res[2],T)
    B <- (sim_1-res[1]*t)/res[2]
    plot(t,sim_1,type = 'l', col ="darkgreen",
         main = "Simulation des trajectoires par méthodes de Maximum de vraisemblance et de moments",
         xlab = "temps",ylab= "Dégradation prédite")
    grid(col = "lightgray", lty = "dotted")
    lines(t,simulateWiener(N,L,mu_est,sigma_est,T),type = 'l', col ="hotpink")
    legend("topleft", col = c("darkgreen","hotpink"), lty = 1,lwd = 2,cex = 1.2,
           legend = c(paste0("MV : mu = ",mu_estimé,", sigma = ",sigma_estimé),
                      paste0("Moments : mu  = ",mu_est,", sigma = ", sigma_est)))
  })
  
  output$biais_txt_4 <- renderText({
    req(input$biais_4)
    results <- results_store()
    req(results)
    t <- seq(0, input$T_2, length.out = input$pt_nbr_2)
    delta_t <- diff(t)
    L <- input$pt_nbr_2
    traj_nbr <- input$traj_nbr_2
    mu_vect <- numeric(traj_nbr)
    sigma_vect <- numeric(traj_nbr)
    for (i in 1:traj_nbr) {
      X <- results[, i]
      delta_X <- diff(X)
      mu_vect[i] <- (X[L] - X[1]) / (t[L] - t[1])
      sigma_vect[i] <- sqrt(mean((delta_X - mu_vect[i] * delta_t)^2 / delta_t))
    }
    
    biais_mu <- mean(mu_vect) - input$moyenne_2
    biais_sigma <- mean(sigma_vect) - sqrt(input$variance_2)
    
    paste0(
      "Biais mu     = ", round(biais_mu, 6), "\n",
      "Biais sigma = ", round(biais_sigma, 6)
    )
  })
  
  
  output$biais_txt_1 <- renderText({
    req(input$biais_1)
    results <- results_store()
    req(results)
    t <- seq(0, input$T_2, length.out = input$pt_nbr_2)
    delta_t <- diff(t)
    L <- input$pt_nbr_2
    traj_nbr <- input$traj_nbr_2
    mu_vect <- numeric(traj_nbr)
    sigma_vect <- numeric(traj_nbr)
    for(i in 1:traj_nbr){
      y_col <- results[,i]
      y = diff(y_col)
      frac <- y/delta_t
      mu =  sum(frac)/length(y)
      mu_vect[i] = mu
      V = y/sqrt(delta_t)
      
      sigma = sqrt(1/input$pt_nbr_2*sum((V - mean(V))^2))
      sigma_vect[i] = sigma
    }
    
    biais_mu <- mean(mu_vect) - input$moyenne_2
    biais_sigma <- mean(sigma_vect) - sqrt(input$variance_2)
    
    paste0(
      "Biais mu     = ", round(biais_mu, 6), "\n",
      "Biais sigma = ", round(biais_sigma, 6)
    )
  })
  # PAGE 2 : Processus Gamma
  
  simulateGamma = function(nbr_pts, forme, taux, T) {
    x = numeric(nbr_pts)
    x[1] = 0
    if (!is.na(forme) && !is.na(taux) &&
        forme > 0 && taux > 0) {
      delta_x <- rgamma(nbr_pts, shape = forme * T / nbr_pts, rate = taux)
    for (i in 2:nbr_pts){
      x[i] = delta_x[i] + x[i - 1]
    }
    return(x)
    }}
  
  output$plot2 <- renderPlot({
    req(input$run_sim_gamma)
    traj_nbr = isolate(input$traj_nbr_4)
    nbr_pts = isolate(input$pt_nbr_4)
    forme = isolate(input$forme_4)
    taux = isolate(input$taux_4)
    T = isolate(input$T_4)
    t = seq(0,T,length.out = nbr_pts)
    
    if (traj_nbr == 1){
      plot(t,simulateGamma(nbr_pts, forme, taux, T),type='l',xlab = "temps",ylab = "Dégradation Gamma",
           main = "SImulation d'une dégradation par le processus Gamma")
      grid(col = "lightgray", lty = "dotted")
    } else {
      results = replicate(traj_nbr, simulateGamma(nbr_pts, forme, taux, T))
      plot(t, results[, 1],type= 'l',xlab = "temps",ylab = "Dégradation Gamma",
           main = "SImulation des dégradations par le processus Gamma")
      grid(col = "lightgray", lty = "dotted")
      for (i in 2:ncol(results)) lines(t, results[, i])
    }
    abline(0, forme/taux, col='red')
  })
  results_store_2 = reactiveVal(NULL)
  output$plot2_mean <- renderPlot({
    req(input$run_gamma_mean)
    a <- isolate(input$forme_5)
    b <- isolate(input$taux_5)
    traj_nbr <- isolate(input$traj_nbr_5)
    L <- isolate(input$pt_nbr_5)
    T <- isolate(input$T_5)
    results = replicate(traj_nbr, simulateGamma(L, a, b, T))
    results_store_2(results)
    nbr_pts <- L
    t <- seq(0, T, length.out = L)
    
    #max vraisemblance
    
    iind <- seq(1, 4000, length.out = 50000)
    s_vect <- as.vector(apply(results, 2, diff))
    log_svect <- log(s_vect)[!is.infinite(log(s_vect))]
    s <- sum(log_svect)
    func <- function(b, s, x_n, nbr_pts,traj_nbr) {
      (nbr_pts-1) * traj_nbr*log(b) + s - (nbr_pts-1) *traj_nbr* digamma(b * sum(x_n) /((nbr_pts-1)*traj_nbr))
    }
    fct <- sapply(ind,function(b){
      func(b, s, s_vect, nbr_pts,traj_nbr)
    })
    idx <- which(fct[-1]* fct[-length(fct)] < 0)
    b_est <- ind[idx[1]]
    a_est <- (b_est * sum(s_vect)) / (traj_nbr * T)
    
    #méthode des moments
    
    dt <- diff(t); dt <- dt[1]
    dX_all <- as.vector(apply(results, 2, diff))
    b_global <- mean(dX_all) / var(dX_all)
    a_global <- mean(dX_all)^2 / (var(dX_all) * dt)
    plot(t,simulateGamma(L,a_est,b_est,T),col = "blue",type="l",xlab = "instants de temps",
         main = "simulation des trajectoires Gamma avec les paramètres estimés à partir des simulations de 500 trajectoires",
         ylab = "simulation Gamma")
    grid(col = "lightgray", lty = "dotted")
    lines(t,simulateGamma(L,a_global,b_global,T),col = "cyan",type = "l")
    legend(
      "topleft",col = c("blue", "cyan"),lty = 1,lwd = 2,cex = 1,
      legend = c(paste0("MV : a = ", round(a_est, 5),", b = ", round(b_est, 5)),
        paste0( "Moments : a = ", round(a_global, 5),", b = ", round(b_global, 5)
        )
      )
    )
  })
  
    output$biais_txt_2 <- renderText({
      req(input$biais_2)
      results <- results_store_2()
      req(results)
      t <- seq(0, input$T_5, length.out = input$pt_nbr_5)
      delta_t <- diff(t)
      L <- input$pt_nbr_5
      traj_nbr <- input$traj_nbr_5
      a = numeric(traj_nbr)
      b = numeric(traj_nbr)
      for (i in 1:traj_nbr) {
        X = results[,i]
        delta_X = diff(X)
        m_1 = mean(delta_X/sqrt(delta_t))
        v = var(delta_X)
        m_2 = mean(delta_X)
        a[i] = (m_1^2)/v 
        b[i] = m_2/v
      }
      biais_a = mean(a)-input$forme_5
      biais_b = mean(b)-input$taux_5
      paste0(
        "Biais forme  = ", round(biais_a, 6), "\n",
        "Biais taux = ", round(biais_b, 6)
      )
    })
    output$biais_txt_3<- renderText({
      req(input$biais_3)
      results <- results_store_2()
      req(results)
      t <- seq(0, input$T_5, length.out = input$pt_nbr_5)
      delta_t <- diff(t)
      L <- input$pt_nbr_5
      traj_nbr <- input$traj_nbr_5
      ind <- seq(1, 2000, length.out = 5000)
      x_n_vect <- apply(results,2,diff)
      
      s_vect <- sapply(1:traj_nbr, function(i) {
        traj_i <- results[, i]
        delta_Xi <- diff(traj_i)
        Log_delta_Xi <- log(delta_Xi)
        Log_delta_Xi <- Log_delta_Xi[!is.infinite(Log_delta_Xi)]
        sum(Log_delta_Xi)
      })
      
      func <- function(b, s, x_n, nbr_pts) {
        (nbr_pts) * log(b) + s - (nbr_pts) * digamma(b * sum(x_n) / (nbr_pts))
      }
      
      fct <- sapply(1:length(s_vect), function(j) {
        sapply(ind, function(b) func(b, s_vect[j], x_n_vect[,j], L))
      })
      
      b_ech <- numeric(traj_nbr)
      a_ech <- numeric(traj_nbr)
      
      for (j in 1:ncol(fct)) {
        idx <- which(fct[-1, j] * fct[-nrow(fct), j] < 0)
        b_ech[j] <- ind[idx[1]]
        a_ech[j] <- b_ech[j] * sum(x_n_vect[,j]) / input$T_5
      }
      biais_a = mean(a_ech)-input$forme_5
      biais_b = mean(b_ech)-input$taux_5
      paste0(
        "Biais forme  = ", round(biais_a, 6), "\n",
        "Biais taux = ", round(biais_b, 6)
      )
    })
    
 
  # PAGE 3 : Lecture CSV et estimation Gamma par moments
  
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
    delta_t = diff(log_t)
    n = length(delta_t)
    sum_delta_t = sum(delta_t)
    delta_X = apply(Y,2,diff)
    s <- sapply(1:ncol(Y),function(i){
      sum(delta_X[,i])
    })
    mu_global <- mean(s/sum_delta_t)
    sigma_global <- sqrt(var(s)/sum(delta_t))
    return (list(mu = mu_global, sigma = sigma_global))
  })
  
  #   Gamma -> Moments -> 1 trajectoire
  paramsGammaMoments <- reactive({
    df <- data()
    t = df[[1]]
    a = numeric(ncol(df) - 1)
    b = numeric(ncol(df) - 1)
    for (i in 1:length(a)) {
      X = df[[i + 1]]
      delta_X = diff(X)
      delta_t = diff(t)
      m_1 = mean(delta_X/sqrt(delta_t))
      v = var(delta_X)
      m_2 = mean(delta_X)
      a[i] = (m_1^2)/v 
      b[i] = m_2/v
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
      frac <- y/delta_t
      mu =  sum(frac)/length(y)
      #3
      mu_vect[i] = mu
      V = y/sqrt(delta_t)
      
      sigma = sqrt(1/n*sum((V - mean(V))^2))
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
    s_vect <- as.vector(apply(Y, 2, diff))
    log_svect <- log(s_vect)[!is.infinite(log(s_vect))]
    s <- sum(log_svect)
    func <- function(b, s, x_n, nbr_pts,traj_nbr) {
      (nbr_pts-1) * traj_nbr*log(b) + s - (nbr_pts-1) *traj_nbr* digamma(b * sum(x_n) /((nbr_pts-1)*traj_nbr))
    }
    fct <- sapply(ind,function(b){
      func(b, s, s_vect, nbr_pts,traj_nbr)
    })
    idx <- which(fct[-1]* fct[-length(fct)] < 0)
    b_est <- ind[idx[1]]
    a_est <- (b_est * sum(s_vect)) / (traj_nbr * T)
    res <- c(a_est,b_est)
    return(res)
  })
  
  
  #Gamma -> moments -> plusieurs traj (pooled)
  paramsGammaMomentsPlusieurstraj <- reactive({
    df <- data()
    t <- df[[1]]
    Y <- df[-1]
    dt <- diff(t); dt <- dt[1]
    dX_all <- as.vector(apply(Y, 2, diff))
    
    b_global <- mean(dX_all) / var(dX_all)
    a_global <- mean(dX_all)^2 / (var(dX_all) * dt)
    return(list(a = a_global,b= b_global))
  })
  
#Plot CSV + simulations 
  
  hey_reactive <- reactiveVal(NULL)
  hey_reactive_2 <- reactiveVal(NULL)
  reactive_muest <- reactiveVal(NULL)
  reactive_sigmaest <- reactiveVal(NULL)
  reactive_aest <- reactiveVal(NULL)
  reactive_best <- reactiveVal(NULL)
  reactive_valeurs_deg <- reactiveVal(NULL)
    output$plot3 <- renderPlot({
    df <- data()
    req(df)
    t <- df[[1]]
    Y <- df[-1] 
    seuil <- input$seuil
    FILE_TYPE = FILE()
    if(FILE_TYPE == "SEMI_CONDUCTEUR"){
      t <- log(t)
      log_y_col <- log(Y)
      if(input$choix_du_modele == "pas d'estimation"){
        matplot(t,log_y_col, type = "b",pch = 16,lty = 1,main ="trajectoires réelles de dégradations des semi_conducteurs (en échelle logarithmique)",
                xlab = "log(temps)",ylab = "log(Dégradation)")
        grid(col = "lightgray", lty = "dotted")
      }else if(input$choix_du_modele == "maximum de vraisemblance"){
        if(input$one_or_more_MV == "1 seul trajectoire"){
          t_ext <- t
          delta_t <- diff(t_ext)
          p <- paramsWienerMaxVrai()
          mu = p$mu
          sigma = p$sigma
          reactive_muest(mu)
          reactive_sigmaest(sigma)
          sim_mat <- sapply(1:length(mu), function(i) {
            rnorm(length(t_ext),mean = mu[i]*delta_t, sd = sigma[i]*sqrt(delta_t))
          })
          sim_maty= sapply(1:ncol(sim_mat), function(j){
            cumsum(sim_mat[,j])
          })
          x_nvect = sim_maty[nrow(sim_maty),]
          BOOL <- x_nvect>= seuil
          while (FALSE %in% BOOL) {
            sim_again <- sapply(1:length(mu), function(i) {
              rnorm(length(delta_t),
                    mean = mu[i] * delta_t,
                    sd   = sigma[i] * sqrt(delta_t))
            })
            simy <- apply(sim_again, 2, cumsum)
            last_val <- sim_maty[nrow(sim_maty), ]
            for (j in 1:ncol(simy)) {
              simy[, j] <- simy[, j] + last_val[j]
            }
            sim_maty <- rbind(sim_maty, simy)
            x_nvect <- sim_maty[nrow(sim_maty), ]
            BOOL <- x_nvect >= seuil
          }
          dt <- mean(delta_t)
          t_full <- seq(from = t_ext[1],
                        by = dt,
                        length.out = nrow(sim_maty))
          listie_bestie <- sapply(1:length(mu),function(i){
            round(t_full[which(sim_maty[,i]>= seuil)[1]],3)
          })
          liste_ord_def <-sapply(1:length(mu),function(i){
            sim_maty[which(sim_maty[,i]>= seuil)[1]]
          })
          hey_reactive(listie_bestie)
          reactive_valeurs_deg(liste_ord_def)
          matplot(t_full, sim_maty,type = 'b',pch = 16,lty= 1,main ="Trajectoires simulées par le processus de Weiner (en échelle logarithmique)",
                  xlab = "log(temps)",ylab= "log(Dégradation)")
          grid(col = "lightgray", lty = "dotted")
          if (seuil > 0){
          abline(h = seuil, col = "red", lwd = 2,lty = 4)}
          sapply(1:length(listie_bestie),function(i){
            abline(v = listie_bestie[i],col = "blue",lwd = 1,lty = 2)
            points(listie_bestie[i], liste_ord_def[i],col = "purple")
          })
          axis(
            side = 1,
            at = listie_bestie,
            labels = round(listie_bestie, 2),
            las = 2,      
            cex.axis = 1
          )
        } else{#plusieurs trajectoires
          t_ext <- t
          delta_t <- diff(t_ext)
          p <- paramsWeinerplusieurstraj()
          mu = p[1]
          sigma = p[2]
          sim <- rnorm(length(t_ext), mean = mu*delta_t, sd = sigma*sqrt(delta_t))
          y_sim <- cumsum(sim)
          matplot(t,log_y_col, type = "b",pch = 16,lty = 1,main ="Trajectoires réelles de dégradations (en échelle logarithmique)",
                  xlab = "log(temps)",ylab= "log(Dégradation)")
          grid(col = "lightgray", lty = "dotted")
          lines(t_ext,y_sim, col = 'gold2',type = 'b',pch = 16,lwd = 5)
          legend("topleft",col = c("black","red","gold2"),lty = 1,lwd = 2,cex = 1.1,
                 legend = (c(paste0("paramètre de tendance (drift) estimé = ",round(mu,3))
                                    ,paste0("racine carrée du paramètre de variabilité estimé = ",round(sigma,3)),
                             "trajectoire prédite par le processus de Weiner à plusieurs trajectoires")))
        }
      }else{#moments
        if (input$one_or_more_moments == "tous les trajectoires"){
        t_ext <- t
        delta_t <- diff(t_ext)
        p <- paramweinerMomentsplusieurstraj()
        mu<- p$mu
        sigma <- p$sigma
        sim <- rnorm(length(t_ext), mean = mu*delta_t, sd = sigma*sqrt(delta_t))
        y_sim <- cumsum(sim)
        matplot(t,log_y_col, type = "b",pch = 16,lty = 1,main ="Trajectoires réelles de dégradations (en échelle logarithmique)",
                xlab = "log(temps)",ylab= "log(Dégradation)")
        grid(col = "lightgray", lty = "dotted")
        lines(t_ext,y_sim, col = 'gold2',type = 'b',pch = 16,lty = 1,lwd = 5)
        legend("topleft",col = c("black","red","gold2"),lty = 1,lwd = 2,cex = 1.1,
               legend = (c(paste0("paramètre de tendance (drift) estimé = ",round(mu,3))
                           ,paste0("racine carée du paramètre de variabilité estimé = ",round(sigma,3)),
                           "trajectoire prédite par le processus de Weiner à plusieurs trajectoires")))
        } else{
          p<-paramsweinerMoments()
          t_ext <- t
          mu_vect <- p$mu
          sigma_vect <- p$sigma
          reactive_muest(mu_vect)
          reactive_sigmaest(sigma_vect)
          sim <- sapply(1:length(mu_vect),function(i){
            tr<- rnorm(length(t_ext),mean = mu_vect[i]*diff(t_ext),sd = sigma_vect[i]*sqrt(diff(t_ext)))
            cumsum(tr)
          })
          matplot(t_ext,sim,type= "b",pch = 16,lty = 1,main ="Trajectoires simulées par le processus de Weiner (en échelle logarithmique)",
                  xlab = "log(temps)",ylab= "log(Dégradation)")
          grid(col = "lightgray", lty = "dotted")
        }
        }
      }else{#laser
        t <- df[[1]]
        Y <- df[-1] 
        if (input$choix_du_modele == "pas d'estimation"){
          matplot(t,Y, type = "b",pch = 16, lty = 1,xlab = "temps",ylab ="Dégradations du laser"
                  ,main = "Trajectoires des dégradations du laser en fonction du temps")
          grid(col = "lightgray", lty = "dotted")
        }else if(input$choix_du_modele == "maximum de vraisemblance"){
          if (input$one_or_more_MV == "tous les trajectoires"){
            Y <- df[-1]
            p<- paramsGammaMVplusieurstraj()
            a<- p[1]
            b <- p[2]
            sim <- rgamma(length(t),shape = a*diff(t),rate = b)
            y_sim <- cumsum(sim)
            matplot(t,Y, type = "b",pch = 16, lty = 1,xlab = "temps",ylab ="Dégradations du laser"
                    ,main = "Trajectoires des dégradations du laser en fonction du temps")
            grid(col = "lightgray", lty = "dotted")
            lines(t,y_sim,type = 'b',col= "red",lwd = 5,lty  =4,pch = 19)
            legend("topleft",col = c("blue","green3","red"),lty = 1,lwd = 2,cex = 1.1,
                   legend = (c(paste0("taux estimé = ",round(a,3))
                               ,paste0("forme estimé = ",round(b,3)),
                               "trajectoire prédite avec méthode de MV en utilisant les paramètres estimés")))
            
          }else{
            seuil <- input$seuil
            p <- paramsGammaMV()
            a_ech <- p$a
            b_ech <- p$b
            reactive_aest(a_ech)
            reactive_best(b_ech)
            n_traj <- length(a_ech)
            delta_t <- diff(t)
            dt <- mean(delta_t)
            sim_mat <- sapply(1:n_traj, function(i) {
              rgamma(length(delta_t),
                     shape = a_ech[i] * delta_t,
                     rate  = b_ech[i])
            })
            sim_maty <- apply(sim_mat, 2, cumsum)
            x_nvect <- sim_maty[nrow(sim_maty), ]
            BOOL <- x_nvect >= seuil
            iter <- 0
            
            while (any(!BOOL)) {
              
              sim_again <- sapply(1:n_traj, function(i) {
                rgamma(length(delta_t),
                       shape = a_ech[i] * delta_t,
                       rate  = b_ech[i])
              })
              
              simy <- apply(sim_again, 2, cumsum)
              last_val <- sim_maty[nrow(sim_maty), ]
              for (j in 1:ncol(simy)) {
                simy[, j] <- simy[, j] + last_val[j]
              }
              sim_maty <- rbind(sim_maty, simy)
              x_nvect <- sim_maty[nrow(sim_maty), ]
              BOOL <- x_nvect >= seuil
            }
            t_full <- seq(from = t[1],
                          by   = dt,
                          length.out = nrow(sim_maty))
            listie_bestie <- sapply(1:n_traj, function(i) {
              idx <- which(sim_maty[, i] >= seuil)[1]
              t_full[idx]
            })
            liste_ord_def <-sapply(1:n_traj,function(i){
              sim_maty[which(sim_maty[,i]>= seuil)[1]]
            })
            hey_reactive(listie_bestie)
            matplot(t_full,sim_maty,type= "b",pch = 16,lty = 1,main ="Trajectoires simulées le processus Gamma en utilisant la méthode de maximum de vraisemblance",
                    xlab = "temps",ylab = "Dégradations du laser",xaxt = "n")
            grid(col = "lightgray", lty = "dotted")
            if (seuil > 0) {
              abline(h = seuil, col = "red", lwd = 2,lty = 4)
            }
            sapply(1:length(listie_bestie),function(i){
              abline(v = listie_bestie[i],col = "blue",lwd = 1,lty = 2)
              points(listie_bestie[i], liste_ord_def[i], pch = 19,col = "purple")
            })
            axis(
              side = 1,
              at = listie_bestie,
              labels = round(listie_bestie, 2),
              las = 2,      
              cex.axis = 1.3
            )
          }
        }else{#moments
          if (input$one_or_more_moments == "1 seul trajectoire"){
            p<-paramsGammaMoments()
            a_ech <- p$a
            b_ech <- p$b
            reactive_aest(a_ech)
            reactive_best(b_ech)
            sim <- sapply(1:length(a_ech),function(i){
              tr<- rgamma(length(t),shape = a_ech[i]*diff(t)[1],rate = b_ech[i])
              cumsum(tr)
            })
            matplot(t,sim,type= "b",pch = 16,lty = 1,xlab = "temps",ylab = "Dégradations prédites du laser",
                    main = "Trajectoires prédites des dégradations du laser en utilisant la méthode des moments")
            grid(col = "lightgray", lty = "dotted")
          }else{
            Y <- df[-1]
            p<- paramsGammaMomentsPlusieurstraj()
            a_est <- p$a
            b_est <- p$b
            sim <- rgamma(length(t),shape = a_est*diff(t),rate = b_est)
            y_sim <- cumsum(sim)
            matplot(t,Y, type = "b",pch = 16, lty = 1,xlab = "temps",ylab = "Dégradations du laser",
                    main = "Trajectoires de dégradations du laser")
            grid(col = "lightgray", lty = "dotted")
            lines(t,y_sim,type = 'b',col= "purple",lwd = 5)
            legend("topleft",col = c("brown","red"),lty = 1,lwd = 2,cex = 1.1,
                   legend = (c(paste0("taux estimé = ",round(a_est,3))
                               ,paste0("forme estimé = ",round(b_est,3)))))
          }
          }
        }

  })
  
  output$list_ttxt_1 <- renderText({
    req(input$list_t_1)
    req(input$seuil)   
    list <- hey_reactive()
    req(list)
    paste0(round(exp(list),4))
  })
  output$survie_txt <- renderText({
    req(input$survie_prob)
    list<- hey_reactive()
    req(list)
    round(mean(list >=input$survie_prob),4)
  })
  output$list_ttxt_2 <- renderText({
    req(input$list_t_2)
    req(input$seuil)
    list <- hey_reactive()
    req(list)
    paste0(list)
  })
  output$estim_MV_txt <- renderText({
    mu <- reactive_muest()
    sigma <- reactive_sigmaest()
    req(mu)
    req(sigma)
    paste(
      paste0(
        "Trajectoire ",seq_along(mu),": ",
        "paramètre de dirft  estimé = ", round(mu,4),"\t",
        ",racine carrée du paramètre de variabilité = ",round(sigma,4)
      ),
      collapse = "\n"
    )
    
  })
  output$estim_Moments_txt <-renderText({
    a <- reactive_aest()
    b <- reactive_best()
    req(a)
    req(b)
    paste(
      paste0(
        "Trajectoire ",seq_along(a),": ",
        "paramètre de forme a estimé = ", round(a,4),"\t",
        ", Paramètre de taux b  = ",round(b,4)
      ),
      collapse = "\n"
    )
  })
}

shinyApp(ui, server)
