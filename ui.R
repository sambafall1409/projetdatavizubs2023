library(shiny)
library(tidyr)
library(lubridate)
library(forecast)
library(tidyverse)
library(plotly)
library(DT)
library(shinydashboard)  
library(shinythemes)
library(shinyWidgets)

productdb$Date <- ymd(paste(productdb$month, "20", sep = "-"))
dashboardPage(
  dashboardHeader(
    title = "Pharmapredict"
  ),
  
  dashboardSidebar(
    width = 300,
    tags$style(HTML("
      /* Styles for dashboardSidebar */
      .sidebar {
        background-color: #000; /* Noir */
        color: #fff; /* Texte blanc */
      }
    ")),
    radioButtons("Forecastbutton", label = "Choisir une variable cible", choices = c("Chiffres d'affaires"="Sales Revenue", "Quantité"="quantity"), inline = TRUE),
    selectizeInput("categ", choices = unique(productdb$CHEMSUB), label = "Categorie"),
    selectizeInput("prods", label = "Nom Produit", choices = unique(productdb$BNFNAME)),
    checkboxInput("decompose", label = "Decompose Ets", value = TRUE),
    selectInput("forecastmodel", label = " Modele", 
                choices = c("ETS" = "AUTO", "TBATS" = "tbats", "AUTO ARIMA" = "autoarima")
    )
    
  ),
  
  
  
  dashboardBody(
    tags$style(HTML("
      /* Styles for the dashboardBody */
      .dashboardBody {
        background-color: #000; /* Noir */
      }
      
      /* Styles for boxes */
      .info-box {
        border: 1px solid #d6d8db;
        background-color: #000; /* Noir */
        border-radius: 4px;
        margin-bottom: 20px;
        padding: 10px;
        color: #fff; /* Texte blanc */
      }
      
      .about-section {
        font-size: 16px; /* Taille de la police augmentée */
        color: #000; /* Texte noir */
        font-family: 'Arial', sans-serif; /* POUR Changer la police de caractères */
      }
    ")),
    
    tags$div(
      class = "about-section",
      h2("À propos de l'application"),
      p(
        "Bienvenue dans l'application Forecaster Web, une plateforme puissante qui vous permet d'explorer et de prédire les performances de vente et de quantité pour chaque produit. ",
        "Cette application offre une analyse approfondie des produits en affichant les 5 meilleurs en termes de ventes et de quantités pour chaque catégorie. ",
        "Vous pouvez également examiner l'évolution temporelle des ventes pour chaque produit sélectionné, ce qui vous donne un aperçu détaillé de la performance passée."
      ),
      tags$a(href = "https://github.com/sambafall1409/projetdatavizubs2023", "lien vers mon dataset et codes"),
    ),
    
    tags$div(
      class = "about-section",
      h2("Fonctionnement de l'application"),
      p(
        "Utilisant des modèles de prévision avancés tels que l'Auto ARIMA et TBATS, l'application génère des prévisions fiables pour anticiper les futurs chiffres de vente et de quantité.
        Vous avez désormais également la possibilité de comparer les performances des différents modèles de prédiction, vous permettant de choisir celui qui correspond le 
        mieux à vos besoins. Les résultats de ces prévisions sont présentés de manière claire dans un tableau de données, facilitant ainsi l'interprétation et la prise de décision.
        Explorez les fonctionnalités de prédiction avancées et la convivialité de cette application pour optimiser votre compréhension des tendances du marché et améliorer la planification
        de votre inventaire."
      )
    ),
    
    fluidRow(
      
      
      shinydashboard::box(
        width = 12,
        title = "Evolution of Top 5 Products",
        plotlyOutput("fivebestproduct"),
        background = "black",  # Fond noir
        solidHeader = TRUE,
       
        
      )
      
      
      
    ),
    
    fluidRow(
      shinydashboard::box(
        width = 6,
        title = "Actual Time Series",
        plotlyOutput("actual_ts"),
        background = "black"  # Fond noir
      ),
      shinydashboard::box(
        width = 6,
        title = "Forecasted Data",
        plotOutput("auplotforecast"),
        background = "black"  # Fond noir
      )
    ),
    
    fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Forecasted Data Table",
        DTOutput("forecastdata")
        # Fond noir
      )
    ),
    fluidRow(
      shinydashboard::box(
        width = 6,
        title = "Comparaison des modèles",
        plotOutput("compare_models")  
      ),
      shinydashboard::box(
        width = 6,
        title = "Performance du Modèle (Erreurs)",
        plotOutput("model_performance")  
      )
    )
  )
)

