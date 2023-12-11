library(shiny)
library(tidyr)
library(lubridate)
library(forecast)
library(tidyverse)
library(plotly)
library(ggplot2)
library(TSstudio)
library(Metrics)

library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
library(conflicted)
conflict_prefer("renderDataTable", "DT")



productdb <- readRDS("./data/productdb.rds")

productdb1 <- productdb %>%
  group_by(CHEMSUB, BNFNAME) %>%
  summarise(SUMQ = sum(QUANTITY)) %>%
  arrange(desc(SUMQ))

# fonction qui affiche pour chaque categorie les 5 meilleurs produits
fivebestproduct <- function(X) {
  res <- productdb1 %>%
    filter(CHEMSUB == X) %>%
    head(5) %>%
    pull(BNFNAME)
  return(res)
}

function(input, output, session) {
  
  # data frame contenant les 5 meilleurs produits
  getDataforsales <- reactive({
    if (input$Forecastbutton == "quantity") {
      productdb %>%
        filter(BNFNAME %in% fivebestproduct(input$categ)) %>%
        select(month, BNFNAME, QUANTITY) %>%
        rename(Month = month, Product = BNFNAME, Metric = QUANTITY)
    } else if (input$Forecastbutton == "Sales Revenue") {
      productdb %>%
        filter(BNFNAME %in% fivebestproduct(input$categ)) %>%
        select(month, BNFNAME, ACTCOST) %>%
        rename(Month = month, Product = BNFNAME, Metric = ACTCOST)
    }
  })
  

  output$fivebestproduct <- renderPlotly({
    plot_ly(getDataforsales(),
            x = ~Month,
            y = ~Metric,
            color = ~Product,
            type = "scatter",
            mode = "lines",
            text = ~paste("Product:", Product))%>%
      plotly::layout(title = paste("Top 5 produits dans la categorie", input$categ))
  })
  

  
  
  
  
  
  
  getDataforprod <- reactive({
    if (input$Forecastbutton == "quantity") {
      productdb %>%
        filter(BNFNAME == input$prods) %>%
        mutate(Date = ymd(paste(month, "20", sep = "-"))) %>%
        select(month, QUANTITY, Date) %>%
        rename(Metric = QUANTITY)
    } else if (input$Forecastbutton == "Sales Revenue") {
      productdb %>%
        filter(BNFNAME == input$prods) %>%
        mutate(Date = ymd(paste(month, "20", sep = "-"))) %>%
        select(month, ACTCOST, Date) %>%
        rename(Metric = ACTCOST)
    }
  })
  
  # creation du serie temporelle du produit selectionné par l'utilisateur
  ts_data <- reactive({
    ts(data = getDataforprod()$Metric,
       start = c(year(min(getDataforprod()$Date)), month(min(getDataforprod()$Date))),
       frequency = 12
    )
  })
  
  # affichage serie temporelle
  output$actual_ts <- renderPlotly({
    if (input$decompose) {
      TSstudio::ts_decompose(ts_data())
    } else {
      TSstudio::ts.plot(ts_data(), title = input$prods)
    }
  })
  
  # predictions en utilisant les modeles avec les series temporelles
  # creation de notre 1ere modele
  #predictions en utilisant les modekes avec les series temporelles
  #creation de notre 1ere modele 
  #modele Auto arima
  md1=reactive({
    forecast(auto.arima(ts_data()))
  })
  
  #modele2 Tbats
  md2=reactive({
    forecast(tbats(ts_data()))
  })
  #modele 3 Ets
  md3 =reactive({
    forecast((ts_data()))
  })
  
  #affichage des resultats de notre modele
  output$auplotforecast=renderPlot({
    if(input$forecastmodel=="AUTO"){
      autoplot(md3())
    }
    else if(input$forecastmodel=="tbats"){
      autoplot(md2())
    }
    else if(input$forecastmodel=="autoarima"){
      autoplot(md1())
    }
  })
  
  
  #affichage des données predictes dans un tableau
  output$forecastdata=renderDataTable({
    if(input$forecastmodel=="AUTO"){
      as.data.frame(forecast(md3(),h=12))
    }
    else if(input$forecastmodel=="tbats"){
      as.data.frame(forecast(md2(),h=12))
    }
    else if(input$forecastmodel=="autoarima"){
      autoplot(md1(),h=12)
    }
    
  })
  
  #comparaison modele
  # Calcul du RMSE pour chaque modèle
  rmse_autoarima <- reactive({
    actual_values <- getDataforprod()$Metric
    predicted_values <- as.vector(forecast(md1(), h =12)$mean)
    rmse(actual_values, predicted_values)
  })
  
  rmse_tbats <- reactive({
    actual_values <- getDataforprod()$Metric
    predicted_values <- as.vector(forecast(md2(), h = 12)$mean)
    rmse(actual_values, predicted_values)
  })
  
  rmse_ets <- reactive({
    actual_values <- getDataforprod()$Metric
    predicted_values <- as.vector(forecast(md3(), h =  12)$mean)
    rmse(actual_values, predicted_values)
  })
  
  # Création d'un graphique comparatif des RMSE
  output$compare_models <- renderPlot({
    models <- c("Auto ARIMA", "TBATS", "ETS")
    rmse_values <- c(rmse_autoarima(), rmse_tbats(), rmse_ets())
    
    data <- data.frame(Model = models, RMSE = rmse_values)
    
    ggplot(data, aes(x = Model, y = RMSE, fill = Model)) +
      geom_bar(stat = "identity", color = "black") +
      labs(title = "Comparaison des modèles", y = "RMSE") +
      theme_minimal() +
      scale_fill_manual(values = c("#FF9999", "#66B2FF", "#99FF99"))  # Couleurs personnalisées
  })
  
  #erreurs de predictions 
  # Calcul des erreurs

  errors <- reactive({
    actual_values <- getDataforprod()$Metric
    predicted_values <- as.vector(forecast(md1(), h = 12)$mean)
    
    # Utilisez la fonction head pour ajuster les longueurs
    actual_values <- head(actual_values, length(predicted_values))
    
    residuals <- actual_values - predicted_values
    return(data.frame(Actual = actual_values, Predicted = predicted_values, Residuals = residuals))
  })
  
  
  # Création d'un graphique de performance du modèle
  output$model_performance <- renderPlot({
    ggplot(errors(), aes(x = Actual, y = Residuals)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Performance du Modèle (Erreurs)", x = "Valeurs Réelles", y = "Erreurs") +
      theme_minimal()
  })
  
  
  
  
  
}
