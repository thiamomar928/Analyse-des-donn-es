
# 1. Chargement des packages nécessaires
#_______________________________________

  # Mais d'abord télécharger les packages s'ils ne sont pas encore installés
  install.packages("tidyverse")
  install.packages("klaR")
  install.packages("psych")
  install.packages("MASS")
  install.packages("devtools")
  install.packages("caret")
  install.packages("GGally")
  
  library(tidyverse)   
  library(klaR)
  library(psych)
  library(MASS)
  library(devtools)
  library(caret)
  library(GGally)
  
# 2. Chargement de la base de données
#_____________________________________
  data(iris)
  iris$Species <- as.character(iris$Species)
  View(iris)
  
# 3. Statistiques descriptives 
#_____________________________
  
  # Les moyennes par variable
  apply(iris[1:4], 2, mean)
  
  # Les moyennes par variable et catégorie
  data_mean <- iris %>% 
    group_by(Species) %>% 
    summarise_all(mean)
  
  # Les médianes par variable
  apply(iris[1:4], 2, median)
  
  # Les médianes par variable et catégorie
  data_median <- iris %>% 
    group_by(Species) %>% 
    summarise_all(median)
  
  # L'écart type par variable
  apply(iris[1:4], 2, sd) 
  
  # L'écart type par variable et catégorie
  data_sd <- iris %>% 
    group_by(Species) %>% 
    summarise_all(sd)
  
  # Un résumé graphique
  graphics <- ggpairs(iris, aes(color = Species))
  
# 4. Réalisation de l'AFD 
#________________________
  
  # 4.1 Choix de l'échantilllon test
  #---------------------------------
  set.seed(0)
  samples <- sample(2, nrow(iris),
                replace = TRUE,
                prob = c(0.8, 0.2))
  training <- iris[samples==1,]
  testing <- iris[samples==2,]
  
  # 4.2 L'analyse discriminante
  #----------------------------
  data_lda <- lda(Species~., training)
  data_lda # Pour voir le résumé de l'AFD
  
  View(data_lda) # Pour voir les résultats de l'AFD
  
  # 4.3 L'analyse des résultats
  #----------------------------
     
      # Probabilités a priori du groupe : 
      #----------------------------------
      # Elles représentent les proportions de chaque espèce dans l'ensemble d'entraînement "training". 
      # Par exemple, 34,9 % de toutes les observations dans l'ensemble sont de l'espèce virginica.
      data_lda[["prior"]]
      
      # Moyennes des groupes : 
      #-----------------------
      # Elles représentent les valeurs moyennes de chaque variable par catégorie.
      data_lda[["means"]]
      
      # Coefficients des discriminants linéaires : 
      #-------------------------------------------
      # Ils affichent la combinaison linéaire des variables prédictives utilisée pour former la règle de décision de l'AFD.
      data_lda[["scaling"]]
      # Chaque axe discriminant est une fonction linéaire.
      # Il est la combinaison linéaire de quatre (04) variables.
      # Leurs équations s'écrivent alors ainsi : 
      # attach(data_lda)
      # LD1 = 0.701*Sepal.Length + 1.601*Sepal.Width - 2.054*Petal.Length - 2.974*Petal.Width
      # LD2 = 0.400*Sepal.Length - 2.340*Sepal.Width - 2.731*Petal.Length - 2.339*Petal.Width

      # Proportion de la trace :
      #--------------------------
      # Il s'agit des pourcentages de séparation obtenu par chaque fonction discriminante linéaire.
      # Le pourcentage de séparation ou de discrimination (équivalent de l'inertie expliquée) assuré par le premier axe discriminant est de 99,09%.
    
      # Les coordonnées discriminantes ou score des individus :
      #--------------------------------------------------------
      training$LD1 <- 1.123626*training$Sepal.Length + 1.201630*training$Sepal.Width -2.437765*training$Petal.Length -2.402852*training$Petal.Width
      training$LD2 <- 0.05957769*training$Sepal.Length - -2.12116156*training$Sepal.Width +0.97822029*training$Petal.Length -2.88098246*training$Petal.Width

  # 4.4 Quelques représentations graphiques
  #----------------------------------------
      # Nuages des individus :
      #-----------------------
      ggplot(training, aes(x = LD1, y = LD2, color = Species)) +
        geom_point(color = "blue", size = 3) +
        labs(title = "Nuage des individus", x = "Premier axe discriminant : 99,09%", y = "Second axe discriminant : 0,91%")
    
      ggplot(training, aes(x = LD1, y = LD2, color = Species)) +
        geom_point() +
        labs(title = "Nuage des individus", x = "Premier axe discriminant : 99,09%", y = "Second axe discriminant : 0,91%")
      
      ggplot(training, aes(x = LD1, y = LD2, color = Species)) +
        geom_point() +
        stat_ellipse(geom = "path", alpha = 0.2) +
        labs(title = "Nuage des individus", x = "Premier axe discriminant : 99,09%", y = "Second axe discriminant : 0,91%")
      
      # Distribution des classes suivant le premier facteur :
      #------------------------------------------------------
      ggplot(training, aes(x = LD1, fill = Species)) +
        geom_histogram(binwidth = 1, position = "dodge") +
        labs(title = "Histogramme suivant une variable de catégorie", x = "Valeurs", y = "Fréquence") +
        scale_fill_manual(values = c("red", "blue", "green"))
      
      ggplot(training, aes(x = LD1, color = Species, fill = Species)) +
        geom_density(alpha = 0.5) +
        labs(title = "Densité suivant une variable de catégorie", x = "Valeurs", y = "Densité") +
        scale_color_manual(values = c("red", "blue", "green")) +
        scale_fill_manual(values = c("red", "blue", "green"))
      
      # Distribution des classes suivant le second facteur :
      #-----------------------------------------------------
      
      ggplot(training, aes(x = LD2, fill = Species)) +
        geom_histogram(binwidth = 1, position = "dodge") +
        labs(title = "Histogramme suivant une variable de catégorie", x = "Valeurs", y = "Fréquence") +
        scale_fill_manual(values = c("red", "blue", "green"))
      
      ggplot(training, aes(x = LD2, color = Species, fill = Species)) +
        geom_density(alpha = 0.5) +
        labs(title = "Densité suivant une variable de catégorie", x = "Valeurs", y = "Densité") +
        scale_color_manual(values = c("red", "blue", "green")) +
        scale_fill_manual(values = c("red", "blue", "green"))
  
  # 4.5 Affectation et qualité de l'affectation
  #--------------------------------------------
      # Prédiction à partir de l'échantillon test
      #------------------------------------------
      predicted <- predict(data_lda, testing)
      names(predicted) 
      View(predicted) # Pour voir les résultats de la prédiction
      # Cette fonction -name(predicted) - renvoie une liste avec trois variables.
      # class : La catégorie prédite (ou le diagnostique)
      # x : Les scores des individus sur lesquels le test est fait
      score <- predicted$x
      classement <- data.frame(predicted$class)
      affectation <- cbind(testing, score, classement)
      
      # Classes prédites
      head(predicted$class)
      # Probabilités a posteriori
      head(predicted$posterior)
      # Coordonnées dans l'espace discriminant
      head(predicted$x)
      
      # Matrice de confusion et qualité de l'affectation
      #-------------------------------------------------
      vect <- predict(data_lda, testing)$class
      quality <- table(Actual = testing$Species, Predicted = vect)
      quality
      sum(diag(quality))/sum(quality)*100
      # Le pouvoir prédictif de l'AFD réalisée est de 95,8%
