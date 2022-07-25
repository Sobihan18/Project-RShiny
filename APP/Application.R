library(shiny)
library(shinythemes)
library(mvtnorm)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(dplyr)

# Define UI
ui <- fluidPage(theme = shinytheme("superhero"),
                themeSelector(),
                navbarPage(
                  "K-Means",
                  tabPanel("Intro",
                           sidebarPanel(
                             tags$h3("Liste des paramètres:"),
                             
                             sliderInput(inputId = "points",
                                         label = "Nombre de points:",
                                         min = 10,
                                         max = 1000,
                                         value = 100),
                             
                             textInput('vec1', 'Entrer la proportion', "0.3,0.4,0.3"),
                             
                             textInput('vec2', 'Entrer la moyenne', "1,2,3,4,3,0"),
                             
                             numericInput("sigma", 
                                          label = "Sigma", 
                                          value = 0.1),
                             
                             
                             actionButton("submitbutton", "Submit", 
                                          class = "btn btn-primary")
                             
                             
                             
                             
                           ), # sidebarPanel
                           
                           
                           mainPanel(
                             br(),
                             br(),
                             h2("Générer les mélanges gaussiens" , align = "center"),
                             
                             
                             h3('Vous avez saisi'),
                             verbatimTextOutput("oid1"),
                             verbatimTextOutput("oid2"),
                             
                           
                             plotOutput(outputId = "Plot"),
                             
                             
                             br(),
                             
                             
                           ), # mainPanel
                           
                           h4("L’algorithme des K-means (K-moyennes) est un algorithme non supervisé très connu en matière de Clustering. Cet algorithme a été conçu en 1957 au sein des Laboratoires Bell par Stuart P.Lloyd comme technique de modulation par impulsion et codage (MIC)."),
                           br(),
                           tags$img(src = "1.png", height = 400, width = 820),
                           br(),
                           br(),
                           h4("Le clustering consiste à regrouper selon un critère de similarité, une grande quantité de données en plusieurs sous-ensembles appelés clusters. Les éléments contenus dans un cluster sont similaires les uns aux autres, mais différents des éléments des autres clusters."),
                           
                           h4("L’idée est assez simple et intuitive. La première étape consiste à définir K centroïdes aléatoirement auxquels on associe K étiquettes. Ensuite pour chaque point, on calcule leur distance euclidienne aux K centroïdes et on associe le point au centroïde le plus proche et l’étiquette correspondante. Puis on recalcule K nouveaux centroïdes à partir des points rattachés. On répète ces étapes jusqu’à ce que les nouveaux centroïdes ne bougent plus des précédents."),
                           br(),
                           tags$img(src = "algo.png", height = 500, width = 820),
                           br(),
                           br(),
                           h4("L'algorithme de K-means est sensible à l'initialisation des centroïdes. Pour surmonter cela, nous utilisons aussi K-means++. Cet algorithme assure une initialisation plus intelligente des centroïdes et améliore la qualité du clustering. Hormis l'initialisation, le reste de l'algorithme est identique à l'algorithme K-means standard."),
                           br(),
                           h4("Voici l’algorithme de K-Means ++ : "),
                           
                           h4("1) Sélectionnez au hasard le premier centroide à partir des points de données."),
                           h4("2) Pour chaque point, calculez sa distance par rapport au centroïde le plus proche précédemment choisi."),
                           h4("3) Sélectionnez le centroïde suivant à partir des points de données de sorte que la probabilité de choisir un point comme centroïde soit directement proportionnelle à sa distance par rapport au centroïde le plus proche précédemment choisi. (c'est-à-dire que le point ayant la distance maximale du centroïde le plus proche est le plus susceptible d'être sélectionné ensuite comme centroïde)."),
                           h4("4) Répétez les étapes 2 et 3 jusqu'à ce que k centroïdes aient été échantillonnés."),
                           br(),
                           
                           
                           sliderInput(inputId = "K",
                                       label = "Nombre de clusters:",
                                       min = 1,
                                       max = 10,
                                       value = 3),
                           
                           sliderInput(inputId = "NiterMax",
                                       label = "Nombre d'itération maximale:",
                                       min = 1,
                                       max = 20,
                                       value = 10),
                           
                           actionButton("go", "Update", 
                                        class = "btn btn-primary"),
                           checkboxInput("test", "K Means ++", value = FALSE),
                           
                           plotOutput(outputId = "Plot2")
                           
                           
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Illustration", 
                           
                           br(),
                           br(),
                           br(),
                           
                           h1("Illustration de K-Means itérartion par itération" , align = "center"),
                           
                           fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput(outputId = "g1"),plotOutput(outputId = "g2"))
                           ),
                           fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput(outputId = "g3"),plotOutput(outputId = "g4"))
                           ),
                           fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput(outputId = "g5"),plotOutput(outputId = "g6"))
                           ),
                           fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput(outputId = "g7"),plotOutput(outputId = "g8"))
                           ),
                           fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput(outputId = "g9"),plotOutput(outputId = "g10"))
                           ),
                           fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput(outputId = "g11"),plotOutput(outputId = "g12"))
                           )
                           
                           
                  ),
                  tabPanel("Iris",
                           br(),
                           br(),
                           br(),
                           tags$h1("Application de K-Means sur les données réelles : IRIS" , align = "center"),
                           
                           
                           h4("L'ensemble de données Iris se compose de 50 échantillons de chacune des trois espèces d'Iris (Iris setosa, Iris virginica et Iris versicolor). Quatre caractéristiques ont été mesurées sur chaque échantillon : la longueur et la largeur des sépales et des pétales, en centimètres."),
                           br(),
                           tags$img(src = "iris.png", height = 360, width = 850),
                           
                           br(),
                           br(),
                           br(),
                           sidebarPanel(
                            
                             #variable <- c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width" ),
                             selectInput('xcol', 'X Variable', c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width" ), selected = "Petal.Length"),
                             selectInput('ycol', 'Y Variable', c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width" ), selected = "Petal.Width"),
                             sliderInput(inputId = "K2",
                                         label = "Nombre de clusters:",
                                         min = 1,
                                         max = 10,
                                         value = 3),
                             sliderInput(inputId = "NiterMax2",
                                         label = "Nombre d'itération maximale:",
                                         min = 1,
                                         max = 10,
                                         value = 5),
                             checkboxInput("test2", "K Means ++", value = FALSE),
                             
                             actionButton("go2", "Update", 
                                          class = "btn btn-primary")
                             
                           ), # sidebarPanel
                           mainPanel(
                             plotOutput('plot3'),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                           ), # mainPanel
                           
                           tags$h1("Importer vos données" , align = "center"),
                           
                           
                           selectInput("data",
                                       "Select data input :",
                                       choices = c("My own data" = "upload")),
                           conditionalPanel(
                             condition = "input.data == 'upload'",
                             fileInput("file",
                                       "Choose CSV File",
                                       accept = ".csv")
                           ),
                          
                           sliderInput(inputId = "K3",
                                       label = "Nombre de clusters:",
                                       min = 1,
                                       max = 10,
                                       value = 3),
                           sliderInput(inputId = "NiterMax3",
                                       label = "Nombre d'itération maximale:",
                                       min = 1,
                                       max = 10,
                                       value = 5),
                           checkboxInput("test3", "K Means ++", value = FALSE),
                           
                           actionButton("go3", "Update", 
                                        class = "btn btn-primary"),
                           
                           plotOutput('plot4'),
                           
                           
                           br(),
                           br(),
                           br(),
                           br(),
                           br(),
                           
                           h1("Illustration de K-Means itérartion par itération" , align = "center"),
                           
                           fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput(outputId = "f1"),plotOutput(outputId = "f2"))
                           ),
                           fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput(outputId = "f3"),plotOutput(outputId = "f4"))
                           ),
                           fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput(outputId = "f5"),plotOutput(outputId = "f6"))
                           ),
                           fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput(outputId = "f7"),plotOutput(outputId = "f8"))
                           ),
                           fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput(outputId = "f9"),plotOutput(outputId = "f10"))
                           ),
                           fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput(outputId = "f11"),plotOutput(outputId = "f12"))
                           )
                           
                           
                  )
                  
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output, session) {
  datasetInput <- eventReactive(input$submitbutton, {
    n = input$points
    p <- as.numeric(unlist(strsplit(input$vec1,",")))
    nb = length(p)
    mu <- as.numeric(unlist(strsplit(input$vec2,",")))
    y <- sample(1:nb,size=n,prob=p,replace=TRUE)
    vec = sapply(1:nb,function(i){ return (sum(y==i))})
    Sigma <- diag(c(input$sigma,input$sigma))
    if(nb == 1){
      df = rmvnorm(n,mu[1:2],Sigma)
    }
    else{
    df = rmvnorm(vec[1],mu[1:2],Sigma)
    for(i in 2:nb){
      pos1 = 2*i - 1
      pos2 = 2*i
      df = rbind(df ,rmvnorm(vec[i],mu[pos1:pos2],Sigma))
    }
    }
    colnames(df) <- c("x","y") 
    return(df)
  })
  
  
  
  output$oid2<-renderPrint({
    x <- as.numeric(unlist(strsplit(input$vec1,",")))
    cat("Vecteur de proportion : ")
    if(sum(x) == 1){
      cat("(")
      for(i in 1:(length(x)-1)){
        cat(x[i])
        cat(", ")
      }
      cat(x[length(x)])
      cat(")")
    }
    cat("\n")
    
    y <- as.numeric(unlist(strsplit(input$vec2,",")))
    cat("Vecteur de moyenne : ")
    q = length(y)/2
    if(length(y)%%2 == 0){
      if(q == 1){
        cat("(")
        cat(y[1])
        cat(",")
        cat(y[2])
        cat(")")
        
      }
      else if(q == 2){
        cat("( (")
        cat(y[1])
        cat(",")
        cat(y[2])
        cat("); ")
        cat("(")
        cat(y[(2*q-1)])
        cat(",")
        cat(y[(2*q)])
        cat(") ) ")
      }
      else{
      cat("( (")
      cat(y[1])
      cat(",")
      cat(y[2])
      cat("); ")
      for(i in 2:(q-1)){
        cat("(")
        cat(y[(2*i-1)])
        cat(",")
        cat(y[(2*i)])
        cat("); ")
      }
        cat("(")
        cat(y[(2*q-1)])
        cat(",")
        cat(y[(2*q)])
        cat(") ) ")
    }
      
    }
  }
  )
  
  
  
  
  output$Plot <- renderPlot({
    df = datasetInput()
    data = data.frame(df)
    ggplot(data,aes(x =  x,y =  y)) + geom_point() + labs(title =  "Data") +
      theme(legend.position = "bottom" , plot.title = element_text(hjust = 0.5 , size=20)) 
  })
  
  
  
  
  generate <- eventReactive(input$go, {
    K = input$K
    NiterMax = input$NiterMax
    df = datasetInput()
    data = data.frame(df)
    
    create_centroids <- function(K,data){
      df <- data[sample(1:nrow(data),K),]      
      df <- cbind(df,1:K)                      
      df <- data.frame(df)
      colnames(df) <- c("x1", "y1", "n_centroid")
      return(df)
    }
    
    
    # Distance euclidienne
    
    euclidean_distance_carre <- function(p1, p2){
      return (sum((p1 - p2)^2))
    }
    
    
    create_centroids_advance <- function(K,data){
      df <- data[sample(1:nrow(data),1),]      # choisir un point aléatoire dans l'ensemble de données
      for(s in 1:(K-1)){
        tab <- c()
        for(i in 1:length(data$x)){
          # on calcule pour chaque point la distance euclidienne par rapport au centroïde le plus proche précédemment choisi
          x <- unlist(sapply(1:s , function(j){ return (euclidean_distance_carre(df[j,1:2], data[i,1:2])) }))
          m <- min(x)
          tab <- c(tab,m)
        }
        p <- which.max(tab)   # on stocke dans tab le point ayant la distance maximale du centroïde le plus proche
        df <- rbind(df,data[p,])
      }
      df <- df[,1:2]
      df <- cbind(df,1:K)
      df <- data.frame(df)
      colnames(df) <- c("x1", "y1", "n_centroid")
      return(df)
    }
    
    
    if (input$test == TRUE){
      centroids = create_centroids_advance(K, data)
    }
    else{
      centroids = create_centroids(K, data)
    }
    
    plot_list <- list()      
    plot_1 = ggplot() + 
      geom_point(aes(x,y, col = "bleu", size = 1), data = data) + 
      geom_point(aes(x1,y1), data = centroids, size = 10, shape=21) +
      labs(title = paste("Illustration de K-means : Initialisation")) +
      geom_text(aes(x1,y1, label = n_centroid), data = centroids, col = "black") +
      theme_minimal() + theme(legend.position = "bottom" , plot.title = element_text(hjust = 0.5 , size=12)) + guides(size = "none") 
    plot_list[[1]] = plot_1
    
    
    
    
    
    # Première attribution de chaque point a un cluster plus proche
    premiere_attribution <- function(data, centroids){
      data$cluster <- NA    
      data$error <- NA       
      for(i in 1:length(data$x)){
        z <- unlist(sapply(1:K , function(j){ return (euclidean_distance_carre(centroids[j,1:2], data[i,1:2])) }))
        data$cluster[i] <- which.min(z)    
        data$error[i] <- min(z)              
      }
      return (data)
    }
    
    data <- premiere_attribution(data, centroids)
    plot_2 <- ggplot() + 
      geom_point(aes(x,y, col = as.factor(cluster), size = 5), data = data) + 
      geom_point(aes(x1,y1), data = centroids, size = 10, shape=21) +
      labs(title = paste("Illustration de K-means : 1ère attribution")) +
      geom_text(aes(x1,y1, label = n_centroid), data = centroids, col = "black") +
      theme_minimal() + theme(legend.position = "bottom" , plot.title = element_text(hjust = 0.5 , size=12)) + guides(size = "none") 
    plot_list[[2]] = plot_2
    
    
    mean_by_cluster <- function(data,K){
      centroids <- data %>%
        group_by(cluster) %>%
        summarize(
          x = mean(x), 
          y = mean(y)
        ) 
      centroids <- data.frame(centroids)[,c(2,3,1)]
      colnames(centroids) <- c("x1", "y1", "n_centroid")
      return (centroids)
    }
    plot_3 <- ggplot() + 
      geom_point(aes(x,y, col = as.factor(cluster), size = 7), data = data) + 
      geom_point(aes(x1,y1), data = mean_by_cluster(data, K), size = 10, shape=21) +
      labs(title = paste("Illustration de K-means : 1ère itération")) +
      geom_text(aes(x1,y1, label = n_centroid), data = mean_by_cluster(data, K), col = "black") +
      theme_minimal() + theme(legend.position = "bottom" , plot.title = element_text(hjust = 0.5 , size=12)) + guides(size = "none") 
    plot_list[[3]] = plot_3
    
    
    iter = 0
    erreur_old = 0
    erreur_new = sum(data$error)     
    centroids <- mean_by_cluster(data, K)
    while( erreur_old != erreur_new && (iter <= NiterMax ) ){
      for(i in 1:length(data$x)){
        z = unlist(sapply(1:K , function(j){ return (euclidean_distance_carre(centroids[j,1:2], data[i,1:2])) }))
        data$error[i] = min(z)
        data$cluster[i] = which.min(z)
      }
      erreur_old   = erreur_new 
      erreur_new = sum(data$error)
      centroids <- mean_by_cluster(data,K)
      plot_list[[(iter+4)]] = ggplot(data = data) + 
        geom_point(aes(x,y, col = as.factor(cluster), size = 7)) + 
        geom_point(aes(x1,y1), data = centroids, size = 10, shape=21) +
        labs(title = paste("Illustration de K-means : " , iter + 2 , "e itération")) +
        geom_text(aes(x1,y1, label = n_centroid), data = centroids, col = "black") +
        theme_minimal() + theme(legend.position = "bottom" , plot.title = element_text(hjust = 0.5 , size=12)) + guides(size = "none")
      iter = iter + 1
    }
    
    return (c((iter - 1),plot_list,data,centroids))
  })
  
  output$Plot2 <- renderPlot({
    
    tab = generate()
    iter = tab[[1]]
    data = data.frame(x = tab$x, y = tab$y, cluster = tab$cluster)
    centroids = data.frame(x1 = tab$x1, y1 = tab$y1, n_centroid = tab$n_centroid)
    ggplot() + 
      geom_point(aes(x,y, col = as.factor(cluster), size = 7), data = data) + 
      geom_point(aes(x1,y1), data = centroids, size = 10, shape=21) +
      labs(title = paste("Illustration de K-means avec convergence en " , iter , " itérations")) +
      geom_text(aes(x1,y1, label = n_centroid), data = centroids, col = "black") +
      theme_minimal() + theme(legend.position = "bottom" , plot.title = element_text(hjust = 0.5 , size=12)) + guides(size = "none") 
    
  })
  
  
  output$g1 <- renderPlot({
    tab = generate()
    tab[[2]]
  })
  
  
  output$g2 <- renderPlot({
    tab = generate()
    tab[[3]]
  })
  
  output$g3 <- renderPlot({
    tab = generate()
    iter = tab[[1]]
    if( iter >= 1){
      tab[[4]]
    }
  })
  
  output$g4 <- renderPlot({
    tab = generate()
    iter = tab[[1]]
    if( iter >= 2){
      tab[[5]]
    }
  })
  
  output$g5 <- renderPlot({
    tab = generate()
    iter = tab[[1]]
    if( iter >= 3){
      tab[[6]]
    }
  })
  
  output$g6 <- renderPlot({
    tab = generate()
    iter = tab[[1]]
    if( iter >= 4){
      tab[[7]]
    }
  })
  
  output$g7 <- renderPlot({
    tab = generate()
    iter = tab[[1]]
    if( iter >= 5){
      tab[[8]]
    }
  })
  
  output$g8 <- renderPlot({
    tab = generate()
    iter = tab[[1]]
    if( iter >= 6){
      tab[[9]]
    }
  })
  
  output$g8 <- renderPlot({
    tab = generate()
    iter = tab[[1]]
    if( iter >= 6){
      tab[[9]]
    }
  })
  
  output$g9 <- renderPlot({
    tab = generate()
    iter = tab[[1]]
    if( iter >= 7){
      tab[[10]]
    }
  })
  
  output$g10 <- renderPlot({
    tab = generate()
    iter = tab[[1]]
    if( iter >= 8){
      tab[[11]]
    }
  })
  
  output$g11 <- renderPlot({
    tab = generate()
    iter = tab[[1]]
    if( iter >= 9){
      tab[[12]]
    }
  })
  
  output$g12 <- renderPlot({
    tab = generate()
    iter = tab[[1]]
    if( iter >= 10){
      tab[[13]]
    }
  })
  
  
  
  selectedData <- eventReactive(input$go2,{
    df = iris[, c(input$xcol, input$ycol)]
    K = input$K2
    NiterMax = input$NiterMax2
    data = data.frame(df)
    colnames(data) <- c("x","y")
    
    
    create_centroids <- function(K,data){
      df <- data[sample(1:nrow(data),K),]      
      df <- cbind(df,1:K)                      
      df <- data.frame(df)
      colnames(df) <- c("x1", "y1", "n_centroid")
      return(df)
    }
    
    
    # Distance euclidienne
    
    euclidean_distance_carre <- function(p1, p2){
      return (sum((p1 - p2)^2))
    }
    
    create_centroids_advance <- function(K,data){
      df <- data[sample(1:nrow(data),1),]      # choisir un point aléatoire dans l'ensemble de données
      for(s in 1:(K-1)){
        tab <- c()
        for(i in 1:length(data$x)){
          # on calcule pour chaque point la distance euclidienne par rapport au centroïde le plus proche précédemment choisi
          x <- unlist(sapply(1:s , function(j){ return (euclidean_distance_carre(df[j,1:2], data[i,1:2])) }))
          m <- min(x)
          tab <- c(tab,m)
        }
        p <- which.max(tab)   # on stocke dans tab le point ayant la distance maximale du centroïde le plus proche
        df <- rbind(df,data[p,])
      }
      df <- df[,1:2]
      df <- cbind(df,1:K)
      df <- data.frame(df)
      colnames(df) <- c("x1", "y1", "n_centroid")
      return(df)
    }
    
    
    if (input$test2 == TRUE){
      centroids = create_centroids_advance(K, data)
    }
    else{
      centroids = create_centroids(K, data)
    }
    
    
    
    
    mean_by_cluster <- function(data,K){
      centroids <- data %>%
        group_by(cluster) %>%
        summarize(
          x = mean(x), 
          y = mean(y)
        ) 
      centroids <- data.frame(centroids)[,c(2,3,1)]
      colnames(centroids) <- c("x1", "y1", "n_centroid")
      return (centroids)
    }
    
    
    
    data$cluster <- NA
    data$error <- NA
    iter = 0
    erreur_old = 0
    found = FALSE
    while( (found == FALSE) && (iter < NiterMax ) ){
      for(i in 1:length(data$x)){
        y = unlist(sapply(1:K , function(j){ return (euclidean_distance_carre(centroids[j,1:2], data[i,1:2])) }))
        data$error[i] = min(y)
        data$cluster[i] = which.min(y)
      }
      erreur_new = sum(data$error)
      found <- erreur_new == erreur_old
      erreur_old   = erreur_new 
      centroids <- mean_by_cluster(data,K)
      if(!found){
        iter = iter + 1
      }
    }
    
    return (c(iter,data,centroids))
    
    
  })
  
  
  
  
  output$plot3 <- renderPlot({
    
    tab = selectedData()
    iter = tab[[1]]
    data = data.frame(x = tab$x, y = tab$y, cluster = tab$cluster)
    centroids = data.frame(x1 = tab$x1, y1 = tab$y1, n_centroid = tab$n_centroid)
    ggplot() + 
      geom_point(aes(x,y, col = as.factor(cluster), size = 7), data = data) + 
      geom_point(aes(x1,y1), data = centroids, size = 10, shape=21) +
      labs(title = paste("Illustration de K-means sur IRIS avec convergence en " , iter , " itérations")) +
      geom_text(aes(x1,y1, label = n_centroid), data = centroids, col = "black") +
      theme_minimal() + theme(legend.position = "bottom" , plot.title = element_text(hjust = 0.5 , size=12)) + guides(size = "none") 
    
  })
  
  
  
  
  csvInput <- reactive({
    switch(input$data,
           "upload" = read.csv(input$file$datapath, header = FALSE)[,1:2])
  })
  
  selectedData3 <- eventReactive(input$go3,{
    validate(
      need(input$data != "upload" || input$file != "","Change data input or upload CSV file!")
    )
    
    df    <- csvInput()
    
    K = input$K3
    NiterMax = input$NiterMax3
    data = data.frame(df)
    colnames(data) <- c("x","y")
    
    
    create_centroids <- function(K,data){
      df <- data[sample(1:nrow(data),K),]      
      df <- cbind(df,1:K)                      
      df <- data.frame(df)
      colnames(df) <- c("x1", "y1", "n_centroid")
      return(df)
    }
    
    
    # Distance euclidienne
    
    euclidean_distance_carre <- function(p1, p2){
      return (sum((p1 - p2)^2))
    }
    
    
    create_centroids_advance <- function(K,data){
      df <- data[sample(1:nrow(data),1),]      # choisir un point aléatoire dans l'ensemble de données
      for(s in 1:(K-1)){
        tab <- c()
        for(i in 1:length(data$x)){
          # on calcule pour chaque point la distance euclidienne par rapport au centroïde le plus proche précédemment choisi
          z <- unlist(sapply(1:s , function(j){ return (euclidean_distance_carre(df[j,1:2], data[i,1:2])) }))
          m <- min(z)
          tab <- c(tab,m)
        }
        p <- which.max(tab)   # on stocke dans tab le point ayant la distance maximale du centroïde le plus proche
        df <- rbind(df,data[p,])
      }
      df <- df[,1:2]
      df <- cbind(df,1:K)
      df <- data.frame(df)
      colnames(df) <- c("x1", "y1", "n_centroid")
      return(df)
    }
    
    
    if (input$test3 == TRUE){
      centroids = create_centroids_advance(K, data)
    }
    else{
      centroids = create_centroids(K, data)
    }
    
    plot_list <- list()      
    plot_1 = ggplot() + 
      geom_point(aes(x,y, col = "bleu", size = 1), data = data) + 
      geom_point(aes(x1,y1), data = centroids, size = 10, shape=21) +
      labs(title = paste("Illustration de K-means : Initialisation")) +
      geom_text(aes(x1,y1, label = n_centroid), data = centroids, col = "black") +
      theme_minimal() + theme(legend.position = "bottom" , plot.title = element_text(hjust = 0.5 , size=12)) + guides(size = "none") 
    plot_list[[1]] = plot_1
    
    
    
    # Première attribution de chaque point a un cluster plus proche
    premiere_attribution <- function(data, centroids){
      data$cluster <- NA    
      data$error <- NA       
      for(i in 1:length(data$x)){
        z <- unlist(sapply(1:K , function(j){ return (euclidean_distance_carre(centroids[j,1:2], data[i,1:2])) }))
        data$cluster[i] <- which.min(z)   
        data$error[i] <- min(z)              
      }
      return (data)
    }
    
    data <- premiere_attribution(data, centroids)
    plot_2 <- ggplot() + 
      geom_point(aes(x,y, col = as.factor(cluster), size = 5), data = data) + 
      geom_point(aes(x1,y1), data = centroids, size = 10, shape=21) +
      labs(title = paste("Illustration de K-means : 1ère attribution")) +
      geom_text(aes(x1,y1, label = n_centroid), data = centroids, col = "black") +
      theme_minimal() + theme(legend.position = "bottom" , plot.title = element_text(hjust = 0.5 , size=12)) + guides(size = "none") 
    plot_list[[2]] = plot_2
    
    
    mean_by_cluster <- function(data,K){
      centroids <- data %>%
        group_by(cluster) %>%
        summarize(
          x = mean(x), 
          y = mean(y)
        ) 
      centroids <- data.frame(centroids)[,c(2,3,1)]
      colnames(centroids) <- c("x1", "y1", "n_centroid")
      return (centroids)
    }
    plot_3 <- ggplot() + 
      geom_point(aes(x,y, col = as.factor(cluster), size = 7), data = data) + 
      geom_point(aes(x1,y1), data = mean_by_cluster(data, K), size = 10, shape=21) +
      labs(title = paste("Illustration de K-means : 1ère itération")) +
      geom_text(aes(x1,y1, label = n_centroid), data = mean_by_cluster(data, K), col = "black") +
      theme_minimal() + theme(legend.position = "bottom" , plot.title = element_text(hjust = 0.5 , size=12)) + guides(size = "none") 
    plot_list[[3]] = plot_3
    
    
    iter = 0
    erreur_old = 0
    erreur_new = sum(data$error)     
    centroids <- mean_by_cluster(data, K)
    while( erreur_old != erreur_new && (iter <= NiterMax ) ){
      for(i in 1:length(data$x)){
        z = unlist(sapply(1:K , function(j){ return (euclidean_distance_carre(centroids[j,1:2], data[i,1:2])) }))
        data$error[i] = min(z)
        data$cluster[i] = which.min(z)
      }
      erreur_old   = erreur_new 
      erreur_new = sum(data$error)
      centroids <- mean_by_cluster(data,K)
      plot_list[[(iter+4)]] = ggplot(data = data) + 
        geom_point(aes(x,y, col = as.factor(cluster), size = 7)) + 
        geom_point(aes(x1,y1), data = centroids, size = 10, shape=21) +
        labs(title = paste("Illustration de K-means : " , iter + 2 , "e itération")) +
        geom_text(aes(x1,y1, label = n_centroid), data = centroids, col = "black") +
        theme_minimal() + theme(legend.position = "bottom" , plot.title = element_text(hjust = 0.5 , size=12)) + guides(size = "none")
      iter = iter + 1
    }
    
    return (c((iter - 1),plot_list,data,centroids))
    
  })
  
  output$plot4 <- renderPlot({
    tab = selectedData3()
    iter = tab[[1]]
    data = data.frame(x = tab$x, y = tab$y, cluster = tab$cluster)
    centroids = data.frame(x1 = tab$x1, y1 = tab$y1, n_centroid = tab$n_centroid)
    ggplot() + 
      geom_point(aes(x,y, col = as.factor(cluster), size = 7), data = data) + 
      geom_point(aes(x1,y1), data = centroids, size = 10, shape=21) +
      labs(title = paste("Illustration de K-means avec convergence en " , iter , " itérations")) +
      geom_text(aes(x1,y1, label = n_centroid), data = centroids, col = "black") +
      theme_minimal() + theme(legend.position = "bottom" , plot.title = element_text(hjust = 0.5 , size=12)) + guides(size = "none") 
    
    
  })
    
    
  
  
  output$f1 <- renderPlot({
    tab = selectedData3()
    tab[[2]]
  })
  
  
  output$f2 <- renderPlot({
    tab = selectedData3()
    tab[[3]]
  })
  
  output$f3 <- renderPlot({
    tab = selectedData3()
    iter = tab[[1]]
    if( iter >= 1){
      tab[[4]]
    }
  })
  
  output$f4 <- renderPlot({
    tab = selectedData3()
    iter = tab[[1]]
    if( iter >= 2){
      tab[[5]]
    }
  })
  
  output$f5 <- renderPlot({
    tab = selectedData3()
    iter = tab[[1]]
    if( iter >= 3){
      tab[[6]]
    }
  })
  
  output$f6 <- renderPlot({
    tab = selectedData3()
    iter = tab[[1]]
    if( iter >= 4){
      tab[[7]]
    }
  })
  
  output$f7 <- renderPlot({
    tab = selectedData3()
    iter = tab[[1]]
    if( iter >= 5){
      tab[[8]]
    }
  })
  
  output$f8 <- renderPlot({
    tab = selectedData3()
    iter = tab[[1]]
    if( iter >= 6){
      tab[[9]]
    }
  })
  
  
  output$f9 <- renderPlot({
    tab = selectedData3()
    iter = tab[[1]]
    if( iter >= 7){
      tab[[10]]
    }
  })
  
  output$f10 <- renderPlot({
    tab = selectedData3()
    iter = tab[[1]]
    if( iter >= 8){
      tab[[11]]
    }
  })
  
  output$f11 <- renderPlot({
    tab = selectedData3()
    iter = tab[[1]]
    if( iter >= 9){
      tab[[12]]
    }
  })
  
  output$f12 <- renderPlot({
    tab = selectedData3()
    iter = tab[[1]]
    if( iter >= 10){
      tab[[13]]
    }
  })
  
  
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)