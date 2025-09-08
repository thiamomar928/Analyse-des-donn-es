
# 1. Définition du répertoire de travail
#_______________________________________

  getwd()
  setwd("F:/ISE2/1_Analyse des données/Cours_2024/TP_ADD-ACP/TP_ACP")
  getwd()
# 2. Chargement des packages nécessaires
#_______________________________________
  
  # Mais d'abord télécharger les packages s'ils ne sont pas encore installés
  install.packages(c("readxl",
                     "tidyverse",
                     "FactoMineR",
                     "factoextra",
                     "Factoshiny",
                     "corrplot",
                     "REAT",
                     "EnvStats"
                     ,"psych"))

 
  library(readxl)       # Permet d'importer les données
  library(tidyverse)    # Permet de manipuler les matrices
  library(FactoMineR)   # Permet de faire l'analyse factorielle
  library(factoextra)   # Permet de les représentations de l'Analyse factorielle
  library(Factoshiny)   # Permet l'analyse factorielle sous forme d'application
  library(corrplot)     # Permet de représenter les matrices de correllation
  library(REAT)         # Permet de tracer la courbe de Lorentz
  library(EnvStats)     # Permet de calculer les paramètres de forme
  library(psych)        # Permet de réaliser l'ACP varimax
  
# 3. Exportation de la base de données
#_____________________________________
  
  Data_PCA <- read_excel("Data_PCA.xlsx")
  View(Data_PCA)
  # Choix des variables qui nous intéressent
  Data <-Data_PCA %>% select(Country_Code, Series_Code, YR_2018)
  View(Data)
  
# 4. Changement du format de la base
#___________________________________
  
  Data_PCA_wide <- Data %>% pivot_wider(names_from = "Series_Code",
                                        values_from = "YR_2018")
  dim(Data_PCA)
  # View(Data_PCA_wide)
  Data_PCA_wide  <- Data_PCA_wide %>% remove_rownames %>% column_to_rownames(var="Country_Code")
  View(Data_PCA_wide)
  
  # install.packages("writexl")
  # library(writexl)
  # write_xlsx(data, "Data_PCA_wide.xlsx")
  
  # La base contient n_obs individus et n_var variables 
  dim(Data_PCA_wide)
  
  n_obs=25 # nombre d'observations
  n_var=23 # nombre de variables
  
  
  
# 5. Statistique descriptive univariée
#_____________________________________
 
  # Statistiques globales pour chaque variable
  summary(Data_PCA_wide)
  
  # 5.1 Les paramètres de position
  #-------------------------------
  # La moyenne
  var_mean <- Data_PCA_wide %>% summarise_all(mean) #calcul la moyenne de chaue var
  var_mean <- as.data.frame(t(var_mean)) # transpose 
  var_mean <- var_mean %>% rename("Mean"="V1") # renomme
  # View(var_mean)
  
  # La médiane
  var_med <- Data_PCA_wide %>% summarise_all(median)
  var_med <- as.data.frame(t(var_med))
  var_med <- var_med %>% rename("Median"="V1")
  #View(var_med)
  
  # Le mode
  #Puisqu'ils s'agit de variables quantitatives continues, pas pertinent
  
  # 5.2 Les paramètres de dispersion
  #---------------------------------
  # L'écart-type
  var_sd <- Data_PCA_wide %>% summarise_all(sd)
  var_sd <- as.data.frame(t(var_sd))
  var_sd <- var_sd %>% rename("SD"="V1")
  View(var_sd)
  
  # La variance
  var_var <- var_sd^2 
  var_var <- var_var %>% rename("Variance"="SD")
  #View(var_var)
  
  # L'écart absolu médian
  var_mad <- Data_PCA_wide %>% summarise_all(mad)
  var_mad <- as.data.frame(t(var_mad))
  var_mad <- var_mad %>% rename("ECMédian"="V1")

    # L'Ã©cart absolu moyen
  values <- c()
  for (i in 1:23) {
    x <- 0
    for (j in 1:25) {
      y <- 1/25*abs(Data_PCA_wide[j,i]-mean(Data_PCA_wide[,i]))
      x <- x+y
    }
    values <-  c(values,x)
  }
  var_eac <- data.frame(series=colnames(Data_PCA_wide),ECMoyen=values)
  var_eac  <- var_eac %>% remove_rownames %>% column_to_rownames(var="series")
  View(var_eac)
  
  # L'Ã©cart interquartile
  values <- c()
  for (i in 1:23) {
    values <-  c(values,IQR(Data_PCA_wide[,i]))
  }
  var_IQR <- data.frame(series=colnames(Data_PCA_wide),IQR=values)
  var_IQR  <- var_IQR %>% remove_rownames %>% column_to_rownames(var="series")
  #View(var_IQR)
  
  # L'Ã©tendu
  values <- c()
  for (i in 1:23) {
    x <- max(Data_PCA_wide[,i])
    y <- min(Data_PCA_wide[,i])
    values <-  c(values,x-y)
  }
  var_eten <- data.frame(series=colnames(Data_PCA_wide),Etendu=values)
  var_eten  <- var_eten %>% remove_rownames %>% column_to_rownames(var="series")
  #View(var_eten)
  
  # Le coefficient de variation
  values <- c()
  for (i in 1:23) {
    x <- sd(Data_PCA_wide[,i])
    y <- mean(Data_PCA_wide[,i])
    values <-  c(values,x/y)
  }
  var_cv <- data.frame(series=colnames(Data_PCA_wide),CV=values)
  var_cv  <- var_cv %>% remove_rownames %>% column_to_rownames(var="series")
  #View(var_cv)
  
  # 5.3 Les paramÃ¨tres de forme
  #----------------------------
  # Coefficient d'asymÃ©trie de fisher
  values <- c()
  for (i in 1:23) {
    values <-  c(values,skewness(Data_PCA_wide[,i]))
  }
  var_skw <- data.frame(series=colnames(Data_PCA_wide),Skewness=values)
  var_skw  <- var_skw %>% remove_rownames %>% column_to_rownames(var="series")
  # View(var_skw)
  
  # Coefficient d'applatissement de Pearson
  values <- c()
  for (i in 1:23) {
    values <-  c(values,kurtosis(Data_PCA_wide[,i]))
  }
  var_kurt <- data.frame(series=colnames(Data_PCA_wide),Kurtosis=values)
  var_kurt  <- var_kurt %>% remove_rownames %>% column_to_rownames(var="series")
  #View(var_kurt)
  
  # Courbe de Lorentz et coefficient de Gini
  par(mfrow=c(5,6))
  for (i in 1:23) {
    lorenz(Data_PCA_wide[,i], lctitle = colnames(Data_PCA_wide)[i], lcx = "", lcy = "",lcgn = TRUE)
  }
  par(mfrow=c(1,1))
  
  
  
  # 5.4 Résumé des statistiques univarié
  #-------------------------------------
  stat_uni <- bind_cols(var_mean,var_med,var_mad,var_sd,var_var,var_eac,var_IQR,var_eten,var_cv,var_skw,var_kurt)
  View(stat_uni)
  print(stat_uni)
# 6. Statistique descriptive bivariée
#____________________________________
  
  # 6.1 Covariance et coefficient de correlation (Pearson)
  #-------------------------------------------------------
  # Matrice de covariance
  var_cor <- cor(Data_PCA_wide)
  # Matrice des coefficients de corrélation 
  corrplot(var_cor)
  
  # Matrice des nuages de points
  pairs(Data_PCA_wide)
  pairs(Data_PCA_wide %>% select(BDP, IND, AGR))
  
  # 6.2 Correlation de rang : Spearman 
  #-----------------------------------
  # Matrice de covariance
  var_cor_s <- cor(Data_PCA_wide, method = "spearman")
  # Matrice des coefficients de corrélation 
  corrplot(var_cor_s)
  
  # 6.3 Correlation de rang : Kendal 
  #-----------------------------------
  # Matrice de covariance
  var_cor_k <- cor(Data_PCA_wide, method = "kendal")
  # Matrice des coefficients de corrélation 
  corrplot(var_cor_k)
  
# 7. L'analyse en composantes principales
#________________________________________
  # 7.1 Normalisation des données
  #------------------------------
  Data_PCA_norm <- scale(Data_PCA_wide, center=T, scale=T)
  view(Data_PCA_norm)
  # 7.2 Mise en oeuvre de l'ACP
  #----------------------------
  PCA_result <- PCA(Data_PCA_norm, ncp = 10, scale.unit = TRUE, graph = FALSE)
  Factoshiny(Data_PCA_norm)
  # Valeurs propres
  e.scaled <-  eigen(cov(Data_PCA_norm))
  print(e.scaled$values) 
  
  # Vecteurs propres
  data.pc  <-  as.matrix(Data_PCA_norm) %*% e.scaled$vectors 
  View(data.pc)
  
  # 7.3 Analyse des valeurs propres
  #--------------------------------
  eig.value <- PCA_result$eig
  eig.value = as.data.frame(eig.value)
  eig.value$dimension = seq.int(from=1, to=23, by=1)
  View(eig.value)
  
  ggplot(eig.value, aes(x=dimension, y=`percentage of variance`)) +
        geom_bar(fill="#FFA500",stat = "identity") +
        geom_point() +
        geom_line() + 
        scale_x_discrete(breaks=seq.int(from=1, to=10, by=1)) +
        ggtitle("Diagramme des valeurs propres") +
        xlab("Dimensions") +
        ylab("Valeurs propres")
  
  # 7.4 Analyse des variables
  #--------------------------
  var <- PCA_result$var
  
  # Cercle de correlation des variables (nuage des variables)
  fviz_pca_var(PCA_result,repel = TRUE,axes = c(1, 2))
  View(var$coord)
  
  # Qualite de representation des variables
  corrplot(var$cos2[,1:5], is.corr=FALSE)
  fviz_cos2(PCA_result, choice = "var", axes = 1:2)
  fviz_pca_var(PCA_result, col.var = "cos2",
               repel = TRUE) 
  
  #Contribution des variables a la formation des axes
  corrplot(var$contrib[,1:3], is.corr=FALSE)
  fviz_contrib(PCA_result, choice = "var", axes = 1)
  fviz_pca_var(PCA_result, col.var = "contrib",
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
  
# ================================================
  # Contributions sur les 3 premiers axes (matrice complète)
  corrplot(var$contrib[,1:3], is.corr = FALSE)
  
  # Seulement les 6 variables les plus contributives sur l'axe 1
  fviz_contrib(PCA_result, choice = "var", axes = 1, top = 6)
  
  # Visualisation des variables (colorées selon leur contribution)
  fviz_pca_var(PCA_result, col.var = "contrib",
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE)
  
  head(sort(var$contrib[,1], decreasing = TRUE), 6)
  
  # IDH, POP1, ESP,SCO, POPc et AGR contribuent le plus à la formation de l'axe 1.
  # Interprétation de l'axe 1 : Axe du développement humain et économique
  head(sort(var$contrib[,2], decreasing = TRUE), 6)
  # INV, EPG, DCF, MAN, GDPc et HLT contribuent le plus à la formation de l'axe 2.
  # Interprétation de l'axe 2 : Axe de l'investissement dans le capital humain et économique.
  # Donne les significations des variables abregées c'est-à-dire  IDH= indice de de
# ================================================
  
  
  # 7.5 Analyse des individus
  #--------------------------
  ind <- PCA_result$ind
  
  # Nuage des individus 
  fviz_pca_ind (PCA_result, repel = TRUE)
  View(ind$coord)

  # Qualité de representation des individus
  corrplot(ind$cos2[,1:5], is.corr=FALSE)  
  fviz_cos2(PCA_result, choice = "ind", axes = 1:2)
  fviz_pca_ind(PCA_result, col.ind = "cos2",
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE) 
  
  # Contribution des individus à la formation des axes
  corrplot(ind$contrib[,1:2], is.corr=FALSE)
  fviz_contrib(PCA_result, choice = "ind", axes = 1:2)
  fviz_pca_ind(PCA_result, col.ind = "contrib",
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
  
  # 7.6 Analyse simultanée des nuages
  #----------------------------------
  fviz_pca_biplot(PCA_result, repel = TRUE)
         
# 8. Mise en élèment supplémentaire
#__________________________________
  
  # Ajout d'une variable qualitative
  LOC <- c("OCE","AFR","AFR","AME","AME","ASI","AFR","AFR","EUR","EUR","AFR","ASI","ASI","ASI","AME","AFR","ASI","AFR","EUR","ASI","AFR","ASI","AFR","AME","AFR")
  Data_PCA_supp <- data.frame(Data_PCA_norm,LOC)
  
  # Mise en element supplementaire d'un individu //(ind.sup=1)
  PCA_result_supp1 <- PCA(Data_PCA_supp[,1:n_var],scale.unit = T, ncp = 10,ind.sup=1, graph = FALSE) 
  graph <- fviz_pca_ind(PCA_result_supp1)
  fviz_pca_ind(PCA_result_supp1, invisible="ind.sup")
  fviz_add(graph,PCA_result_supp1$ind.sup$coord, color = "red")
  
  # Mise en element suppementaire d'une variable qualitative
  PCA_result_supp2 <- PCA(Data_PCA_supp,scale.unit = T, ncp = 10, quali.sup=24 ,graph = FALSE) 
  graph <- fviz_pca_ind(PCA_result_supp2)
  fviz_pca_ind(PCA_result_supp2, invisible="quali.sup")
  fviz_add(graph,PCA_result_supp2$quali.sup$coord, color = "red")
  
  # Mise en element suppementaire d'une variable quantitative
  PCA_result_supp3 <- PCA(Data_PCA_supp[,1:n_var],scale.unit = T, ncp = 10,quanti.sup=n_var,graph = FALSE) 
  graph <- fviz_pca_var(PCA_result_supp3)
  fviz_pca_var(PCA_result_supp3, invisible="quanti.sup")
  fviz_add(graph,PCA_result_supp3$quanti.sup$coord, color = "red")
  
# 9. Raccourci mais attention !
#______________________________
  # Mise en oeuvre de l'ACP
  PCA_result_shiny<-PCAshiny(Data_PCA_norm)

# 10. Présence d'effet taille
#____________________________
  
  # 10.1 Exportation d'une autre base de données
  #---------------------------------------------
  Data_SE<- read_excel("Data_size_effect.xlsx",
                         range = NULL,
                         col_names = TRUE,
                         col_types = NULL,
                         na = "")
  Data_SE  <- Data_SE %>% remove_rownames %>% column_to_rownames(var="Région")
  
  # 10.2 Observation de l'effet taille
  #-----------------------------------
  Data_SE_norm <- scale(Data_SE, center=T, scale=T)
  PCA_SE_result <- PCA(Data_SE_norm, ncp = 10, scale.unit = TRUE, graph = FALSE)
  var <- PCA_SE_result$var
  fviz_pca_var(PCA_SE_result,repel = TRUE,axes = c(1, 2))
  
  # 10.3 Traitement des données (en utlisant les pourcentages)
  #-----------------------------------------------------------
  Data_SE_per <- Data_SE %>% rowwise() %>% mutate(total=sum(C1,C2,C3,C4,H0,H1,H2,H3,H4)) %>% ungroup() %>% mutate(C1p=C1/total*100, C2p=C2/total*100, C3p=C3/total*100, C4p=C4/total*100, H0p=H0/total*100,H1p=H1/total*100,H2p=H2/total*100,H3p=H3/total*100,H4p=H4/total*100) %>% select(C1p,C2p,C3p,C4p,H0p,H1p,H2p,H3p,H4p)
  Data_SE_norm <- scale(Data_SE_per, center=T, scale=T)
  PCA_SE_result <- PCA(Data_SE_norm, ncp = 10, scale.unit = TRUE, graph = FALSE)
  var <- PCA_SE_result$var
  fviz_pca_var(PCA_SE_result,repel = TRUE,axes = c(1, 2))
  