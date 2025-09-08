
# 1. Définition du répertoire de travail
#_______________________________________

  getwd()
  setwd("F:/ISEP3/Semestre2/Analyse des données/Cours_2024/Projet_ADD_AFC")
  getwd()
  
# 2. Chargement des packages nécessaires
#_______________________________________
  
  # Chargement des librairies
  # Mais d'abord télécharger les packages s'ils ne sont pas encore installés
  library(readxl)       # Permet d'importer les données
  library(tidyverse)    # Permet de manipuler les matrices
  library(FactoMineR)   # Permet de faire l'analyse factorielle
  library(factoextra)   # Permet de faire les représentations de l'Analyse factorielle
  library(gplots)       # Permet une représentation graphique du tableau de contingence
  library(corrplot)

  
  # F:\ISEP3\Semestre2\Analyse des données\Cours_2024\Projet_ADD_AFC
# 3. Exportation de la base de données
#_____________________________________
  Data_CA <- read_excel("ehcvm_individu_NER.xlsx",
                        sheet = "Sheet1",
                        range = NULL,
                        col_names = TRUE,
                        col_types = NULL,
                        na = "")
  Data_CA  <- Data_CA %>% remove_rownames %>% column_to_rownames(var="Probleme sante")
  View(Data_CA)
  # La base contient deux variables
  # La variable "Région" admet 14 modalités
  # La variable "Age x Sexe" admet 34 modalités
  
  # Une visualisation graphique du tableau de contingence
  data <- as.table(as.matrix (Data_CA))  #pour les besoins de la fonction balloonplot
  balloonplot(t (data), main = "Probleme sante", xlab = "", ylab = "",
              label = FALSE, show.margins = FALSE)
  
# 3. Statistiques descriptives classiques
#________________________________________
  
  # La statistique du Chi-2
  X <- chisq.test(Data_CA)
  X
  
  # Le tableau de contingence de référence
  View(X$expected)
  
# 4. L'analyse factorielle des coresspondances
#_____________________________________________
  
  # 4.1 Mise en oeuvre de l'AFC
  #----------------------------
  CA_result <- CA (Data_CA, graph = FALSE)
  
  # 4.2 Analyse des valeurs propres
  #--------------------------------
  eig.value <- CA_result$eig
  eig.value = as.data.frame(eig.value)
  eig.value$dimension = seq.int(from=1, to=11, by=1)
  View(eig.value)
  ggplot(eig.value, aes(x=dimension, y=`percentage of variance`)) +
        geom_bar(fill="blue",stat = "identity") +
        geom_point() +
        geom_line() + 
        geom_text(aes(label=percentage_label), vjust=-0.5, color="black") +        scale_x_discrete(breaks=seq.int(from=1, to=10, by=1)) +
        ggtitle("Diagramme des valeurs propres") +
        xlab("Dimensions") +
        ylab("Valeurs propres")
  
  # deux criteres de choix des qxes factoriels
  
  # 4.3 Analyse du nuage de la variable "Région"
  #---------------------------------------------
  row <- get_ca_row(CA_result)
  
  # Nuage des modalités
  fviz_ca_row(CA_result,repel = TRUE,axes = c(1,2))
  # on regarde d'abord la qualité puis la contribution avant d'interpreter ce graphique
  
  # Qualite de representation des modalités
  corrplot(row$cos2[,1:4], is.corr=FALSE) # pour chaque dim
  fviz_cos2(CA_result, choice = "row", axes = 1:2) # pour les deux dim
  fviz_ca_row(CA_result, col.row = "cos2", gradient.rows = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)
  ################################
  # regarde la contribution en premier
  #Contribution des modalités a la formation des axes
  corrplot(row$contrib[,1:4], is.corr=FALSE)
  fviz_contrib(CA_result, choice = "row", axes = 1:2)
  fviz_ca_row(CA_result, col.row = "contrib",
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
  
  # 4.4 Analyse du nuage de la variable "Probleme sante x Branche d'activite"
  #-------------------------------------------------
  col <- get_ca_col(CA_result)
  
  # Nuage des modalités
  fviz_ca_col(CA_result,repel = TRUE,axes = c(1, 2))
  
  # Qualite de representation des modalités
  corrplot(col$cos2[,1:4], is.corr=FALSE)
  fviz_cos2(CA_result, choice = "col", axes = 1:2)
  fviz_ca_col(CA_result, col.col = "cos2", gradient.rows = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE) 
  
  #Contribution des modalités a la formation des axes
  corrplot(col$contrib[,1:4], is.corr=FALSE)
  fviz_contrib(CA_result, choice = "col", axes = 1:2)
  fviz_ca_col(CA_result, col.col = "contrib",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)

  # 4.5 Analyse des deux nuages
  #----------------------------
  fviz_ca_biplot(CA_result, repel = TRUE)
         
# 5. Mise en élèment supplémentaire
#__________________________________
  # Modalité mis en supplémentaire : "Dakar"
  CA_result_supp <- CA (Data_CA, row.sup = 1, graph = FALSE)
  fviz_ca_biplot(CA_result_supp, repel = TRUE)
  
  library(Factoshiny)   # Permet l'analyse factorielle sous forme d'application
  # Mise en oeuvre de l'ACP
  PCA_result_shiny<-CAshiny(Data_CA)
  