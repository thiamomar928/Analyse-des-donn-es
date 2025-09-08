
# 1. Définition du répertoire de travail
#_______________________________________

getwd()
setwd("F:/ISE2/1_Analyse des données/Cours_2024/Complément cours - Effet taille")
getwd()
# F:\ISEP3\Semestre2\1_Analyse des données\Cours_2024\Complément cours - Effet taille
# 2. Chargement des packages nécessaires
#_______________________________________

# Mais d'abord télécharger les packages s'ils ne sont pas encore installés
#install.packages("readxl")
#install.packages("tidyverse")
#install.packages("FactoMineR")
#install.packages("factoextra")
#install.packages("Factoshiny")
#install.packages("corrplot")
#install.packages("REAT")
#install.packages("EnvStats")
#install.packages("psych")

library(readxl)       # Permet d'importer les données
library(tidyverse)    # Permet de manipuler les matrices
library(FactoMineR)   # Permet de faire l'analyse factorielle
library(factoextra)   # Permet de les représentations de l'Analyse factorielle
library(Factoshiny)   # Permet l'analyse factorielle sous forme d'application
library(corrplot)     # Permet de représenter les matrices de correllation
library(REAT)         # Permet de tracer la courbe de Lorentz
library(EnvStats)     # Permet de calculer les paramètres de forme
library(psych)        # Permet de réaliser l'ACP varimax


# 3. Observation de l'effet taille
#________________________________

# 10.1 Exportation d'une autre base de données
#---------------------------------------------
Data_SE<- read_excel("Data_size_effect.xlsx",
                     range = NULL,
                     col_names = TRUE,
                     col_types = NULL,
                     na = "")
View(Data_SE)
Data_SE  <- Data_SE %>% remove_rownames %>% column_to_rownames(var="Département")
View(Data_SE)
# 10.2 Observation de l'effet taille
#-----------------------------------
PCA_SE_result <- PCA(Data_SE, ncp = 10, scale.unit = TRUE, graph = FALSE)
var <- PCA_SE_result$var
fviz_pca_var(PCA_SE_result,repel = TRUE,axes = c(1, 2))
fviz_pca_biplot(PCA_SE_result, repel = TRUE)

# 10.3 Traitement des données (en délaissant le premier axe)
#-----------------------------------------------------------
fviz_pca_var(PCA_SE_result,repel = TRUE,axes = c(2, 3))

# 10.4 Traitement des données (en utlisant les pourcentages)
#-----------------------------------------------------------
Data_SE_per <- Data_SE %>% rowwise() %>% mutate(total=sum(Enseignants,	Professeurs,	Ecoles,	Universités,	Hôpitaux,	Medecins,	Sages_femmes,	Pharmaciens,	Dentistes)) %>% ungroup() %>% mutate(En=Enseignants/total*100, Pr=Professeurs/total*100, Eco=Ecoles/total*100, Uni=Universités/total*100, Hop=Hôpitaux/total*100,Med=Medecins/total*100,Sg=Sages_femmes/total*100,Phar=Pharmaciens/total*100,Dent=Dentistes/total*100) %>% select(En,Pr,Eco,Uni,Hop,Med,Sg,Phar,Dent)
departements <- rownames(Data_SE)
Data_SE_per <- Data_SE_per %>% mutate(Identifier = departements)
Data_SE_per <- Data_SE_per %>% select(Identifier, everything())
Data_SE_per  <- Data_SE_per %>% remove_rownames %>% column_to_rownames(var="Identifier")
PCA_SE_per_result <- PCA(Data_SE_per, ncp = 10, scale.unit = TRUE, graph = FALSE)
var <- PCA_SE_per_result$var
fviz_pca_var(PCA_SE_per_result,repel = TRUE,axes = c(1, 2))
fviz_pca_biplot(PCA_SE_per_result, repel = TRUE)

# 10.5 Traitement des données (en utlisant l'ACP sur les rangs)
#-------------------------------------------------------------
# Rang sur les variables (colonnes)
Data_SE_rk <- apply(Data_SE, 2, rank)
PCA_SE_rk_result <- PCA(Data_SE_rk, ncp = 10, scale.unit = TRUE, graph = FALSE)
var <- PCA_SE_rk_result$var
fviz_pca_var(PCA_SE_rk_result,repel = TRUE,axes = c(1, 2))
fviz_pca_biplot(PCA_SE_rk_result, repel = TRUE)  
# Rang sur les individus (lignes)
Data_SE_rkw <- t(apply(Data_SE, 1, rank))
PCA_SE_rkw_result <- PCA(Data_SE_rkw, ncp = 10, scale.unit = TRUE, graph = FALSE)
var <- PCA_SE_rkw_result$var
fviz_pca_var(PCA_SE_rkw_result,repel = TRUE,axes = c(1, 2))
fviz_pca_biplot(PCA_SE_rkw_result, repel = TRUE)  

