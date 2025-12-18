library(shiny)
library(bslib)
library(statmod)

ui <- page_navbar(
  title = "Étude des modèles de dégradation",
  
  # ---- PAGE 1 : Processus de Wiener ----
  nav_panel(
    "Processus Wiener",
    
    tabsetPanel(
      # TAB 1 : Trajectoires
      tabPanel(
        "Trajectoires",
        page_sidebar(
          sidebar = sidebar(
            numericInput("pt_nbr_1", "Nombre points :", 100),
            numericInput("moyenne_1", "Dérive :", 10),
            numericInput("variance_1", "Volatilité :", 10),
            numericInput("traj_nbr_1", "Nb trajectoires :", 7),
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
            numericInput("moyenne_2", "Dérive :", 10),
            numericInput("variance_2", "Volatilité :", 15),
            numericInput("traj_nbr_2", "Nb trajectoires :", 500),
            numericInput("T_2","Durée",100),
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
      
      #TAB 3 : Distribution
      tabPanel(
        "Etude des dates de défaillances",
        sidebarLayout(
          sidebarPanel(
            numericInput("pt_nbr_3", "Nombre points :", 100),
            numericInput("moyenne_3", "Dérive :", 8),
            numericInput("variance_3", "Volatilité :", 25),
            numericInput("traj_nbr_3", "Nb trajectoires :", 1000),
            numericInput("T_3","Durée",500),
            numericInput("seuil_3", "Seuil :", 80),
            actionButton("run_weiner_défaillance","Exécuter",class = "btn-primary")
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
      # TAB 1 : Trajectoires
      tabPanel(
        "Trajectoires",
        page_sidebar(
          sidebar = sidebar(
            numericInput("pt_nbr_4", "Nombre points :", 100),
            numericInput("forme_4", "Forme :", 10),
            numericInput("taux_4", "Taux :", 10),
            numericInput("traj_nbr_4", "Nb trajectoires :", 7),
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
            numericInput("forme_5", "Forme :", 10),
            numericInput("taux_5", "Taux :", 8),
            numericInput("traj_nbr_5", "Nb trajectoires :", 500),
            numericInput("T_5","Durée",100),
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
      
      # TAB 3 : Distribution
      tabPanel(
        "Etude des dates de défaillances",
        sidebarLayout(
          sidebarPanel(
            selectInput(inputId = "Modele_rep",
                        label = "Modèle de représentation choisi",
                        choices = c("histogramme de date de défaillance","fonction de survie")),
            numericInput("pt_nbr_6", "Nombre points :", 100),
            numericInput("forme_6", "Forme :", 10),
            numericInput("taux_6", "Taux :", 8),
            numericInput("traj_nbr_6", "Nb trajectoires :", 700),
            numericInput("T_6","Durée",90),
            numericInput("seuil_6", "Seuil :", 70),
            checkboxInput("median","moyenne des temps de défaillance"),
            verbatimTextOutput("median_text"),
            actionButton("run_gamma_défaillance","Exécuter",class = "btn-primary"),
            numericInput("maint_eff","date de maintenance effective", 0.95),
            verbatimTextOutput("maint_txt"),
            numericInput("prob_surv","probabilité de survie",30),
            verbatimTextOutput("prob_text")
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
    "Analyse des données",
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Upload CSV File", accept = ".csv"),
        selectInput(
          inputId = "choix_du_modele",
          label = "Le choix du modele d'estimation",
          choices = c("pas d'estimation","maximum de vraisemblance", "moments")
        ),
        numericInput("seuil", "Seuil horizontal :", value = 0),
        conditionalPanel(
          condition = "input.choix_du_modele == 'maximum de vraisemblance'",
          selectInput(
            inputId = "one_or_more",
            label = "nombre de trajectoires",
            choices = c("1 seul trajectoire", "tous les trajectoires")
          ),
          checkboxInput("list_t","afficher liste du temps de défaillance"),
          verbatimTextOutput("list_ttxt"),
          numericInput("survie_prob","Probabilité de survie",10),
          verbatimTextOutput("survie_txt")
        ),
        conditionalPanel(
          condition = "input.choix_du_modele == 'moments'",
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
        numericInput("nbr_maint", "Nombre de maintenances :", 4),
        conditionalPanel(
          condition = "input.model_maint == 'ARD fixe'",
          numericInput("delta","constante de réduction :", 0)
          
        ),
        conditionalPanel(
          condition = "input.model_maint == 'ARD1'",
          numericInput("rho","taux de restauration", 0)
          
        ),
        conditionalPanel(
          condition = "input.model_maint == 'Changement de drift'",
          numericInput("alpha","facteur de multiplication", 1),
          numericInput("beta","facteur de modification alpha/beta", 1)
        ),
        #conditionalPanel(
        #  condition = "input.model_maint == 'kijima'"
        #),
        conditionalPanel(condition = "input.model_maint == 'Pas de maintenance'")
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
    req(input$run_weiner_sim)
    mu = isolate(input$moyenne_1)
    sigma = isolate(sqrt(input$variance_1))
    traj_nbr = isolate(input$traj_nbr_1)
    L = isolate(input$pt_nbr_1)
    T = isolate(input$T_1)
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
  output$plot1_hist <- renderPlot({
    req(input$run_weiner_défaillance)
    mu = isolate(input$moyenne_3)
    sigma = isolate(sqrt(input$variance_3))
    traj_nbr = isolate(input$traj_nbr_3)
    L = isolate(input$pt_nbr_3)
    N = 1000
    T = isolate(input$T_3)
    h = isolate(input$seuil_3)
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
    ind <- seq(1, 10000, length.out = 50000)
    s_vect <- as.vector(apply(results, 2, diff))
    s <- sum(log(s_vect))
    func <- function(b, s, x_n, nbr_pts,traj_nbr) {
      (nbr_pts-1) * traj_nbr*log(b) + s - (nbr_pts-1) *traj_nbr* digamma(b * sum(x_n) /((nbr_pts-1)*traj_nbr))
    }
    fct <- sapply(ind,function(b){
      func(b, s, s_vect, nbr_pts,traj_nbr)
    })
    idx <- which(fct[-1]* fct[-length(fct)] < 0)
    b_est <- if (length(idx) >= 1) ind[idx[1]] else NA_real_
    a_est <- if (is.finite(b_est) && b_est > 0) (b_est * sum(s_vect)) / (traj_nbr * T) else NA_real_
    #méthode des moments
    dt <- diff(t); dt <- dt[1]
    dX_all <- as.vector(apply(results, 2, diff))
    b_global <- mean(dX_all) / var(dX_all)
    a_global <- mean(dX_all)^2 / (var(dX_all) * dt)
    plot(t,simulateGamma(L,a_est,b_est,T),col = "blue",type="l",xlab = "instants de temps",
         main = "simulation des trajectoires Gamma avec les paramètres estimés à partir des simulations de 500 trajectoires",
         ylab = "simulation Gamma")
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
        # Formules méthode des moments
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
    temps_défaillance = reactiveVal(NULL)
  output$plot2_hist <- renderPlot({
    req(input$run_gamma_défaillance)
    forme = isolate(input$forme_6)
    taux = isolate(input$taux_6)
    traj_nbr = isolate(input$traj_nbr_6)
    L = isolate(input$pt_nbr_6)
    T = isolate(input$T_6)
    h = isolate(input$seuil_6)
    results = replicate(traj_nbr, simulateGamma(L, forme, taux,T))
    t <- seq(0, T, length.out = L)
    liste_t <- apply(results,2,function(x){
      t[which(x>=h)[1]]
    })
    liste_t <- liste_t[!is.na(liste_t)]
    temps_défaillance(liste_t)
    if(input$Modele_rep == "histogramme de date de défaillance"){
    hist(liste_t, prob = FALSE, breaks = 30,
         main = "Sans maintenance", xlab = "Temps de traversée")
    }else{
      S <- ecdf(liste_t)
      t_grid <- seq(0, max(liste_t), length.out = 2000)
      plot(t_grid, 1 - S(t_grid),
           type = "s",
           main = "Fonction de survie empirique",
           xlab = "Temps",
           ylab = "S(t) = P(T > t)")
    }
    output$median_text <-renderText({
      req(input$median)
      t_deff <- temps_défaillance()
      req(t_deff)
      round(mean(t_deff),3)
    })
    output$maint_txt <- renderText({
      req(input$maint_eff)
      t_deff <- temps_défaillance()
      req(t_deff)
      paste0("date de maintenance effective à risque ", input$maint_eff," : ",round(quantile(t_deff,input$maint_eff),5))
    })
    output$prob_text <- renderText({
      req(input$prob_surv)
      t_deff <- temps_défaillance()
      req(t_deff)
      round(mean(t_deff >=input$prob_surv),4)
    })
    
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
    delta_t = diff(log_t)
    n = length(delta_t)
    sum_delta_t = sum(delta_t)
    delta_X = apply(Y,2,diff)
    s <- sapply(1:ncol(Y),function(i){
      sum(delta_X[,i])
    })
    mu_global <- mean(s/sum_delta_t)
    sigma_global <- sqrt(var(s)/sum(delta_t))
    #dt <- diff(log_t); dt <- dt[1]
    #dX_all <- as.vector(apply(Y, 2, diff))
    
    #mu_global <- mean(dX_all/sum(dt))
    #sigma_global <- sqrt(var(dX_all)/sum(dt))
    return (list(mu = mu_global, sigma = sigma_global))
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
      delta_X = diff(X)
      delta_t = diff(t)
      
      m_1 = mean(delta_X/sqrt(delta_t))
      v = var(delta_X)
      m_2 = mean(delta_X)
      # Formules méthode des moments
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
    s <- sum(log(s_vect))
    func <- function(b, s, x_n, nbr_pts,traj_nbr) {
      (nbr_pts-1) * traj_nbr*log(b) + s - (nbr_pts-1) *traj_nbr* digamma(b * sum(x_n) /((nbr_pts-1)*traj_nbr))
    }
    fct <- sapply(ind,function(b){
      func(b, s, s_vect, nbr_pts,traj_nbr)
    })
    idx <- which(fct[-1]* fct[-length(fct)] < 0)
    b_est <- if (length(idx) >= 1) ind[idx[1]] else NA_real_
    a_est <- if (is.finite(b_est) && b_est > 0) (b_est * sum(s_vect)) / (traj_nbr * T) else NA_real_
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
  # ---- Plot CSV + simulations ----
  hey_reactive <- reactiveVal(NULL)
  hey_reactive_2 <- reactiveVal(NULL)
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
          hey_reactive(listie_bestie)
          matplot(t_full, sim_maty,type = 'b',pch = 16,lty= 1,main ="Trajectoires simulées par le processus de Weiner (en échelle logarithmique)",
                  xlab = "log(temps)",ylab= "log(Dégradation)")
          if (seuil > 0){
          abline(h = seuil, col = "red", lwd = 2,lty = 4)}
          sapply(1:length(listie_bestie),function(i){
            abline(v = listie_bestie[i],col = "blue",lwd = 1,lty = 2)
            points(listie_bestie[i], seuil, pch = 30,col = "purple")
          })
          axis(
            side = 1,
            at = listie_bestie,
            labels = round(listie_bestie, 2),
            las = 2,      
            cex.axis = 1.3
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
          lines(t_ext,y_sim, col = 'gold2',type = 'b',pch = 16)
          legend("topleft",col = c("black","red","gold2"),lty = 1,lwd = 2,cex = 1.1,
                 legend = (c(paste0("paramètre de tendance (drift) estimé = ",round(mu,3))
                                    ,paste0("paramètre de variabilité estimé = ",round(sigma^2,3)),
                             "trajectoire prédite par le processus de Weiner à plusieurs trajectoires")))
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
        matplot(t,log_y_col, type = "b",pch = 16,lty = 1,main ="Trajectoires réelles de dégradations (en échelle logarithmique)",
                xlab = "log(temps)",ylab= "log(Dégradation)")
        lines(t_ext,y_sim, col = 'gold2',type = 'b',pch = 16,lty = 1)
        legend("topleft",col = c("black","red","gold2"),lty = 1,lwd = 2,cex = 1.1,
               legend = (c(paste0("paramètre de tendance (drift) estimé = ",round(mu,3))
                           ,paste0("paramètre de variabilité estimé = ",round(sigma^2,3)),
                           "trajectoire prédite par le processus de Weiner à plusieurs trajectoires")))
        } else{
          p<-paramsweinerMoments()
          t_ext <- t
          mu_vect <- p$mu
          sigma_vect <- p$sigma
          sim <- sapply(1:length(mu_vect),function(i){
            tr<- rnorm(length(t_ext),mean = mu_vect[i]*diff(t_ext),sd = sigma_vect[i]*sqrt(diff(t_ext)))
            cumsum(tr)
          })
          matplot(t_ext,sim,type= "b",pch = 16,lty = 1,main ="Trajectoires simulées par le processus de Weiner (en échelle logarithmique)",
                  xlab = "log(temps)",ylab= "log(Dégradation)")
        }
        }
      }else{#laser
        t <- df[[1]]
        Y <- df[-1] 
        if (input$choix_du_modele == "pas d'estimation"){
          matplot(t,Y, type = "b",pch = 16, lty = 1)
        }else if(input$choix_du_modele == "maximum de vraisemblance"){
          if (input$one_or_more == "tous les trajectoires"){
            Y <- df[-1]
            p<- paramsGammaMVplusieurstraj()
            a<- p[1]
            b <- p[2]
            sim <- rgamma(length(t),shape = a*diff(t),rate = b)
            y_sim <- cumsum(sim)
            matplot(t,Y, type = "b",pch = 16, lty = 1)
            lines(t,y_sim,type = 'l',col= "red",lwd = 5,lty  =4)
            legend("topleft",col = c("brown","red"),lty = 1,lwd = 2,cex = 1.1,
                   legend = (c(paste0("taux estimé = ",round(a,3))
                               ,paste0("forme estimé = ",round(b,3)))))
            
          }else{
            seuil <- input$seuil
            p <- paramsGammaMV()
            a_ech <- p$a
            b_ech <- p$b
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
            hey_reactive(listie_bestie)
            matplot(t_full,sim_maty,type= "b",pch = 16,lty = 1,main ="trace des trajectoires simulées par le processus Gamma",
                    xlab = "temps",ylab = "Dégradation",xaxt = "n")
            if (seuil > 0) {
              abline(h = seuil, col = "red", lwd = 2,lty = 4)
            }
            sapply(1:length(listie_bestie),function(i){
              abline(v = listie_bestie[i],col = "blue",lwd = 1,lty = 2)
              points(listie_bestie[i], seuil, pch = 19,col = "purple")
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
            Y <- df[-1]
            p<- paramsGammaMomentsPlusieurstraj()
            a_est <- p$a
            b_est <- p$b
            sim <- rgamma(length(t),shape = a_est*diff(t),rate = b_est)
            y_sim <- cumsum(sim)
            matplot(t,Y, type = "b",pch = 16, lty = 1)
            lines(t,y_sim,type = 'l',col= "purple",lwd = 5)
            legend("topleft",col = c("brown","red"),lty = 1,lwd = 2,cex = 1.1,
                   legend = (c(paste0("taux estimé = ",round(a_est,3))
                               ,paste0("forme estimé = ",round(b_est,3)))))
          }
          }
        }

  })
  output$list_ttxt <- renderText({
    req(input$list_t)
    list <- hey_reactive()
    req(list)
    paste0(list)
  }) 
  output$survie_txt <- renderText({
    req(input$survie_prob)
    list<- hey_reactive()
    req(list)
    round(mean(list >=input$survie_prob),4)
  })
  output$list_ttxt_2 <- renderText({
    req(input$list_t_2)
    list <- hey_reactive()
    req(list)
    paste0(list)
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
  X_stored <- reactiveVal(NULL)
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
    }
    
  })
}
shinyApp(ui, server)
