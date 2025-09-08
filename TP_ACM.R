
# 1. Définition du répertoire de travail
#_______________________________________
    set.seed(01)
    getwd()
    setwd("F:/ISE2/1_Analyse des données/Cours_2024/Dossier ADD - ACM")
    getwd()
# 2. Chargement des packages nécessaires
#_______________________________________

    # Chargement des librairies
    # Mais d'abord télécharger les packages s'ils ne sont pas encore installés
    library(readxl)       # Permet d'importer les données
    library(tidyverse)    # Permet de manipuler les matrices
    library(FactoMineR)   # Permet de faire l'analyse factorielle
    library(factoextra)   # Permet de les représentations de l'Analyse factorielle
    library(Factoshiny)   # Permet l'analyse factorielle sous forme d'application
    library(gplots)       # Permet une représentation graphique du tableau de contingence
    library(corrplot)
    library(dplyr)
    library(sampling)
    
# 3. Exportation de la base de données
#_____________________________________
  Data_MCA <- read_excel("Data_MCA.xlsx",
                         sheet = "Sheet1",
                         range = NULL,
                         col_names = TRUE,
                         col_types = NULL,
                         na = "")
  # View(Data_MCA)
  Data_MCA  <- Data_MCA %>% remove_rownames %>% column_to_rownames(var="caseid")
  View(Data_MCA)
  # La base contient 11 variables et 18200 individus
  
# Visualiser les labels des variables et des modalités
Data_MCA %>% sjPlot::view_df()

# Mise en oeuvre de l'ACP
# ACM_result<-MCAshiny(Data_MCA)

# 4. Tirage d'un échantillon représentatif
#_________________________________________    
    # Définir les variables de stratification et la taille de l'échantillon
    strata_variables <- c("age", "gender")
    sample_size <- 75
    
    # Créer les strates
    Data_MCA$stratum <- interaction(Data_MCA$age, Data_MCA$gender)
    
    # Calculer la taille de chaque strate
    stratum_sizes <- Data_MCA %>%
    group_by(stratum) %>%
    summarise(n = n()) %>%
    mutate(sample_size = round(n / sum(n) * sample_size))
    
    # Créer un cadre de sondage
    survey_frame <- strata(Data_MCA, stratanames = c("age", "gender"), size = stratum_sizes$sample_size, method = "srswor")
    
    # Extraire l'échantillon
    sample_MCA <- getdata(Data_MCA, survey_frame)
    Data <- sample_MCA[,2:12] %>% select(-stratum) %>% select(-inputstate)
    # La base contient 75 variables et 15 individus
    
    # 5. L'analyse des correspondances multiples
    #___________________________________________
    
    # 5.1 Mise en oeuvre de l'ACM
    #----------------------------
    MCA_result <- MCA(Data, graph = FALSE)

    # ACM_result<-MCAshiny(Data)
    
    
    # 5.2 Analyse des valeurs propres
    #--------------------------------
    eig.value <- MCA_result$eig
    eig.value <- as.data.frame(eig.value)
    eig.value$dimension <- seq.int(from=1, to=nrow(eig.value))
    
    # Vérification du tableau des valeurs propres
    View(eig.value)
    
    # Visualisation des valeurs propres
    library(ggplot2)
    ggplot(eig.value, aes(x=dimension, y=`percentage of variance`)) +
    geom_bar(fill="#FFA500", stat="identity") +
    geom_point() + 
    geom_line() + 
    scale_x_discrete(breaks=seq.int(from=1, to=min(10, nrow(eig.value)), by=1)) +
    ggtitle("Diagramme des valeurs propres") +
    xlab("Dimensions") +
    ylab("Valeurs propres")

    # 5.3 Analyse du nuage des variables
    #-----------------------------------
    var <- get_mca_var(MCA_result)
    
# Pour voir les contributions des modalités avec leurs vraies valeures.    
# coord, cos2, contrib 
    
    # Nuage des modalités
    fviz_mca_var(MCA_result,repel = TRUE,axes = c(1, 2))
    fviz_mca_var(MCA_result,repel = TRUE,axes = c(1, 2), choice = "var")
    
    #Contribution des modalités a la formation des axes
    corrplot(var$contrib[,1:2], is.corr=FALSE)
    fviz_contrib(MCA_result, choice = "var", axes = 1)
    fviz_mca_var(MCA_result, col.var = "contrib",
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
    
    # Qualité de representation des modalités
    corrplot(var$cos2[,1:2], is.corr=FALSE)
    fviz_cos2(MCA_result, choice = "var", axes = 1:2)
    fviz_mca_var(MCA_result, col.var = "cos2", gradient.rows = c("#00AFBB", "#E7B800", "#FC4E07"),
                 repel = TRUE) 
    
    # 5.4 Analyse du nuage des individus
    #-----------------------------------
    ind <- get_mca_ind(MCA_result)
    
    # Nuage des individus
    fviz_mca_ind(MCA_result,repel = TRUE,axes = c(1, 2))
    
    #Contribution des individus a la formation des axes
    corrplot(ind$contrib[,1:4], is.corr=FALSE)
    fviz_contrib(MCA_result, choice = "ind", axes = 1:2)
    fviz_mca_ind(MCA_result, col.ind = "contrib",
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
    
    # Qualite de representation des individus
    corrplot(ind$cos2[,1:4], is.corr=FALSE)
    fviz_cos2(MCA_result, choice = "ind", axes = 1:2)
    fviz_mca_ind(MCA_result, col.ind = "cos2", gradient.rows = c("#00AFBB", "#E7B800", "#FC4E07"),
                 repel = TRUE) 
    
    # Nuage des individus par catégorie
    fviz_mca_ind (MCA_result,
                  label = "none",
                  habillage = "ideo5", 
                  addEllipses = TRUE, ellipse.type = "confidence")
    plotellipses(MCA_result)
    
# On fait dans cette partie une analyse par groupe. Ce ci  
    
# =====================IMPORTANT=======================================
    
# Liste des variables à tester
    vars <- c("race", "educ", "marstat", "employ","pid3","ideo5", "religpew", "age", "gender")
    # Boucle pour générer les graphiques pour chaque variable
    for (v in vars) {
      cat("\n==== Variable :", v, "====\n")
      
      # Transformation en facteur
      Data[[v]] <- as.factor(Data[[v]])
      
      # Graphique MCA
      print(
        fviz_mca_ind(MCA_result,
                     label = "none",
                     habillage = Data[[v]],
                     addEllipses = TRUE,
                     ellipse.type = "confidence",
                     palette = "jco",
                     repel = TRUE) + ggtitle(paste("Habillage par", v))
      )
    }

# =====================IMPORTANT=======================================  
    
    # 5.5 Analyse des deux nuages
    
    #----------------------------
    fviz_mca_biplot(MCA_result, repel = TRUE)
    
    
    
    
    # Définir les variables de stratification et la taille de l'échantillon
    strata_variables <- c("age", "gender")
    sample_size <- 75
    
    # Créer les strates
    Data_MCA$stratum <- interaction(Data_MCA$age, Data_MCA$gender)
    
    # Calculer la taille de chaque strate
    stratum_sizes <- Data_MCA %>%
      group_by(stratum) %>%
      summarise(n = n()) %>%
      mutate(sample_size = round(n / sum(n) * sample_size))
    
    # Créer un cadre de sondage
    survey_frame <- strata(Data_MCA, stratanames = c("age", "gender"), size = stratum_sizes$sample_size, method = "srswor")
    
    # Extraire l'échantillon
    sample_MCA <- getdata(Data_MCA, survey_frame)
    Data <- sample_MCA[,2:12] %>% select(-stratum) %>% select(-inputstate)
    # La base contient 11 variables et 9 individus
