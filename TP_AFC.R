
# 1. Chargement des packages nécessaires
#_______________________________________
  
  # Chargement des librairies
  # Mais d'abord télécharger les packages s'ils ne sont pas encore installés
  library(readxl)       # Permet d'importer les données
  library(tidyverse)    # Permet de manipuler les matrices
  library(FactoMineR)   # Permet de faire l'analyse factorielle
  library(factoextra)   # Permet de les représentations de l'Analyse factorielle
  library(gplots)       # Permet une représentation graphique du tableau de contingence
  library(corrplot)
  
# 2. Importation de la base de données
# _____________________________________

  data("housetasks")
  View(housetasks)
  
  # Mise en oeuvre de l'ACP
  # install.packages("Factoshiny")
  library(Factoshiny)
  # CA_result_shiny<-CAshiny(housetasks)
  result_shiny<-Factoshiny(iris)
  # Pour revenir à la dernière étape avant de quitter l'application.
  Factoshiny(result_shiny)
  
# 3. Statistiques descriptives classiques
#________________________________________
  

  
# 4. L'analyse factorielle des coresspondances
#_____________________________________________
  
  # 4.1 Mise en oeuvre de l'AFC
  #----------------------------
  CA_result <- CA(housetasks, graph = FALSE)
  
  # 4.2 Analyse des valeurs propres
  #--------------------------------
  eig.value <- CA_result$eig
  eig.value = as.data.frame(eig.value)
  eig.value$dimension = seq.int(from=1, to=3, by=1)
  View(eig.value)
  ggplot(eig.value, aes(x=dimension, y=`percentage of variance`)) +
        geom_bar(fill="#FFA500",stat = "identity") +
        geom_point() +
        geom_line() + 
        scale_x_discrete(breaks=seq.int(from=1, to=10, by=1)) +
        ggtitle("Diagramme des valeurs propres") +
        xlab("Dimensions") +
        ylab("Valeurs propres")
  
  # 4.3 Analyse du nuage de la variable "Région"
  #---------------------------------------------
  row <- get_ca_row(CA_result)

  # Nuage des modalités
  fviz_ca_row(CA_result,repel = TRUE,axes = c(1, 2))
  
  #Contribution des modalités a la formation des axes
  corrplot(row$contrib[,1:3], is.corr=FALSE)
  fviz_contrib(CA_result, choice = "row", axes = 1:2)
  fviz_ca_row(CA_result, col.row = "contrib",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
  
  # Qualite de representation des modalités
  corrplot(row$cos2[,1:3], is.corr=FALSE)
  fviz_cos2(CA_result, choice = "row", axes = 1:2)
  fviz_ca_row(CA_result, col.row = "cos2", gradient.rows = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE) 
  
  # 4.4 Analyse du nuage de la variable "Age x Sexe"
  #-------------------------------------------------
  col <- get_ca_col(CA_result)
  
  # Nuage des modalités
  fviz_ca_col(CA_result,repel = TRUE,axes = c(1, 2))
  
  # On analyse puis caracterise le premier axe puis de meme le second axe.
  # On donne les ressemblances entre les groupes.
  # Le premier axe oppose A et B. Le second axe oppose C et D. 
  # Dans le rapport, on ne va interpreter que le tableau et non la contribution ni la qualité
  # En AFC, on ne parle que de modalités ni de variables, ni d'observations
  
  # Qualite de representation des modalités
  
  corrplot(col$cos2[,1:3], is.corr=FALSE)
  fviz_cos2(CA_result, choice = "col", axes = 1:2)
  fviz_ca_col(CA_result, col.col = "cos2", gradient.rows = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE) 
  
  #Contribution des modalités a la formation des axes
  corrplot(col$contrib[,1:3], is.corr=FALSE)
  fviz_contrib(CA_result, choice = "col", axes = 1:2)
  fviz_ca_col(CA_result, col.col = "contrib",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)

  # 4.5 Analyse des deux nuages
  #----------------------------
  fviz_ca_biplot(CA_result, repel = TRUE)
  
 # Ici, on fait une synthese des principaux de résultats 
  
  # Introduction
  # Montrer avec un ou deux auteurs pour montrer l'importance du sujet choisi.