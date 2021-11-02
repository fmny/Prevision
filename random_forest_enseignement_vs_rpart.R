################################################################
#Utilisation de RandomForest en vue de faire de la prevision:
###############################################################

#lien
#https://thinkr.fr/premiers-pas-en-machine-learning-avec-r-volume-4-random-forest/


#import du fichier CSv sans accent 

#Chemin d acces
setwd("C:\\Users\\Francis\\R_new\\random forest\\data-enseignement")
#verification
#getwd()
educ <- read.csv(file=".\\fr-en-effectifs-des-personnels-des-ecoles-et-etablissements-du-2nd-degre-ss-accent.csv",sep=';')

#install.packages("Rtools")
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("tidy")
#install.packages("caret")


library(dplyr)      # pour mutate_at et %>%
library(tidyverse)
library(tidyr)      # pour unnest et separate
library(caret)
#dplyr contient les operateurs %>% qui permettent le data wrangling (operation sur la BDD)


#attention select doit etre remplace par dplyr::select car un autre package utilise la fonction select
educ_small <- educ %>%  filter(Academie == "RENNES") %>% 
  dplyr::select(Type.etablissement, Secteur.enseignement,Groupe.de.personnels, Titulaire, Sexe, Borne.inferieure.de.la.tranche.age, Nombre.agents,Code.region)
##selectionne les colonnes que l on veut conserver


#faire tourner ces operations 1 par 1 pour comprendre le data wrangling

#educ_small_test <- educ_small %>% 
#  mutate(Idx = 1:n()) %>%
#  group_by(Idx) %>% #ne sert ? rien
#  mutate( Agent = list(rep( Sexe, Nombre.agents) ) ) %>%
#  unnest() %>% # explose la base de maniere ? avoir 87123 ligne (cad le nb d agent)
#  ungroup() %>% #ne sert a rien
#  dplyr::select(-Idx) #retire la colonne Idx


sum(educ_small$Nombre.agents)
#[1] 43510    #sur le site data ancienne
#[1] 87123    #resultat FM

nrow(educ_small)
#[1] 16248    #sur le site data ancienne
#[1] 32107    #resultat FM



#on remplace Nombre.agents par 1 dans la colonne
#educ_small_test$Nombre.agents<-1


#Mise en facteur des variables pour random forest


educ_small2 <- educ_small
##avec ou sans IDX a la fin
educ_small2 <- educ_small %>% mutate(Idx = 1:n()) %>%group_by(Idx) %>%mutate( Agent = list(rep( Sexe, Nombre.agents) ) )%>%unnest() %>%ungroup()
##educ_small2 <- educ_small2 %>% mutate(Idx = 1:n()) %>%group_by(Idx) %>%mutate( Agent = list(rep( Sexe, Nombre.agents) ) )%>%unnest() %>%ungroup()%>%select(-Idx)


#Var quanti et var quali
educ_small2$Type.etablissement   =  factor(educ_small2$Type.etablissement)
educ_small2$Secteur.enseignement =  factor(educ_small2$Secteur.enseignement)
educ_small2$Groupe.de.personnels =  factor(educ_small2$Groupe.de.personnels)
educ_small2$Titulaire =  factor(educ_small2$Titulaire)
educ_small2$Sexe      =  factor(educ_small2$Sexe)
educ_small2$Borne.inferieure.de.la.tranche.age = factor(educ_small2$Borne.inferieure.de.la.tranche.age)
educ_small2$Code.region=factor(educ_small2$Code.region)
educ_small2$Agent=factor(educ_small2$Agent)

#attention ici, le fait d avoir d avoir demultiplie la base pour avoir un enregistrement
#par enseignant a cree des doublons dans la colonne Idx
#de sortes que anti_join ne fonctionnait pas

educ_small2$Idx<-1:nrow(educ_small2)
educ_small2$Nombre.agents<-1

set.seed(2811)

train <- educ_small2 %>% sample_frac(0.8) #prend 80% de la base pour le training
nrow(train)
#69698
test <- anti_join(educ_small2, train) #prend les 20% restant
nrow(test)
#17425/87123=0.2000046

#on retire la colonne 8 (region qui possede des NA et n a qu une seule valeur donc sans interet)
train<-train[-8]
test<-test[-8]

library(randomForest)
set.seed(2811)
model <- randomForest(Titulaire ~ ., data = train, ntree = 100, na.action = na.omit)

#500 est un peu long, je prends 100 arbres

hist(model$oob.times)
model$votes[1:10,]
model$importance
varImpPlot(model)

############################################################
#Utilisation de randomForest pour faire une prediction
############################################################

set.seed(2811)
test$predicted <- predict(model, test)

table(test$predicted, test$Titulaire)


summary(test$predicted)
summary(test$Titulaire)
nrow(test)


library(caret)
conf <- confusionMatrix(data = test$predicted, reference = test$Titulaire)
conf

#Confusion Matrix and Statistics

#Reference
#Prediction    Contractuel Titulaire
#Contractuel        7057       203
#Titulaire           812      9353

#Accuracy : 0.9418    taux de bonnes prevision

#lecture du tableau
# on a donc 7057 Contractuel bien predit et 203 mal predit (etaient des titulaires)
#les bonnes predictions sont sur la diagonale


conf$byClass["Sensitivity"]
#0.8968103

conf$byClass["Specificity"]
# 0.9787568 


#Fichier des résulats avec la colonne de resultat des previsions titulaire et contractuel
write.csv(test,file=".\\test.csv")
#write.csv(test$predicted,file=".\\temp.csv")
#revoir les resultats sous excel tout a bougé

########################################
#Prog fait sous Rpart 
#########################################

#Installation des packages
#install.packages("Rtools")
#install.packages("rpart")
#install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

lapply(educ_small2,class)


#educ_small2_Tree <- rpart(Titulaire~.,data=educ_small2)

set.seed(2811)
educ_Tree <- rpart(Titulaire~.,data=train,control=rpart.control(minsplit=5,cp=0),na.action=na.pass)
#on retire les NA 


#prp(educ_Tree,extra=1) #graphe complet mais illisible
#summary(educ_Tree)

#Rem: minsplit =5, signifie que l'on ne redecoupe une branche 
#en plusieurs feuilles que si celle-ci possede au moins 5 obs
#par defaut minsplit=20 si l'on ne le precise pas

#plotcp(educ_Tree) 
educ_Tree$cptable
#write.csv(educ_Tree$cptable,file=".\\educ_tree_cptable.csv")



#Simplification
#Comme attendu, les performances s ameliorent dans un premier temps quand on augmente 
#le nombre de feuilles puis se degradent en raison du sur-apprentissage. On choisit en general la complexite qui minimise 
#l erreur estimee

#educ_Simple <- prune(educ_Tree,cp=0.0000316)

#Simplification de l arbre en faisant des test-le min de la stderror generant du surrapprentissage
educ_Simple <- prune(educ_Tree,cp=0.0002)

plotcp(educ_Simple) #le graphe optimal a beaucoup de feuilles
prp(educ_Simple,extra=1)

set.seed(2811)
#educ_Tree_optimal <- prune(educ_Tree,cp=educ_Tree$cptable[which.min(educ_Tree$cptable[,4]),1])
#ptitanicOptimal <- prune(ptitanicTree,cp=ptitanicTree$cptable[which.min(ptitanicTree$cptable[,4]),1])
#print(educ_Tree$cptable[which.min(educ_Tree$cptable[,4]),1])

#plotcp(educ_Tree_optimal) #le graphe optimal a beaucoup de feuilles
#prp(educ_Tree_optimal,extra=1)
#l arbre semble etre trop touffu, cependant les previsions sont plutôt bonnes


#Par defaut, la fonction estime les probabilites d appartenance aux classes pour chaque observation 
#(simplement par le ratio dans la feuille correspondante). Par exemple le code suivant
set.seed(2811)
predict(educ_Simple, test, type="class")

#table(test$Titulaire, predict(educ_Tree, test, type="class"))

#result rpart
#               Contractuel Titulaire
#Contractuel        7639       208
#Titulaire           223      9355
#semble correct en terme de resultats


tree<-educ_Simple
data<- test
#Pour obtenir la classe predite, il suffit d ajouter 
#le parametre type avec la bonne valeur, soit :


#predict(ptitanicOptimal, type="class")
set.seed(2811)
pred<-predict(tree, test, type="class",na.action = na.omit)


#on observe des resultats sensiblement identiques avec rpart que random forest

library(caret)
#conf <- confusionMatrix(data = test$predicted, reference = test$Titulaire)
conf_rpart <- confusionMatrix(data = predict(educ_Simple, test, type="class"), reference = test$Titulaire)
conf_rpart

#Confusion Matrix and Statistics

#Reference
#Prediction    Contractuel Titulaire
#Contractuel        7090       204
#Titulaire           779      9352

#Accuracy : 0.9436 

write.csv(pred,file=".\\temp_rpart.csv")

conf_rpart$byClass["Sensitivity"] 
# 0.9010039   taux de bonnes predictions pour les contractuel 

conf_rpart$byClass["Specificity"] 
# 0.9786522  taux de bonnes predictions pour les titulaires


###################################################################################
#anciens resultat avec educ_tree_optimal qui je crois effectue du surrapprentissage
#Confusion Matrix and Statistics

#Reference
#Prediction    Contractuel Titulaire
#Contractuel        7639       223
#Titulaire           208      9355

#Accuracy : 0.9753 j obtiens un resultat meilleur qu avec random forest (?)

#conf_rpart$byClass["Sensitivity"] 

#result random forest
#0.9734931 taux de bonnes predictions pour les contractuel 

#conf_rpart$byClass["Specificity"] 
#Specificity  0.9767175
###########################################################################


######################################
#utilisation d un reseau de neuronnes
####################################

library(nnet)
library(MASS)


#nombre agent et Idx ne sont pas des factors

#attention a cette syntaxe la repetition du code peut poser probleme
train2<-train[-7] #je retire la col nb agent
train2<-train2[-8] #je retire la col IDX
#train2<-train2[-7] #je retire la col code.region 
#(car 1 seule val ce qui pose probleme pour la fonction nnet)
colnames(train2)

#la colonne code.region pose problem car elle ne contient qu une val =53 et NA

#train2$Titulaire<-factor(train2$Titulaire)



#model.dis <- nnet(train2$Titulaire~.,data = train2, size = 2, decay = 0.001,na.action=na.omit)

#colnames(train2)
#class(train2$Type.etablissement)
#class(train2$Secteur.enseignement)
#class(train2$Groupe.de.personnels)
#class(train2$Titulaire)
#class(train2$Sexe)
#class(train2$Borne.inferieure.de.la.tranche.age)
#class(train2$Code.region)
#class(train2$Agent)

#levels(train$Titulaire)

#attention a cette syntaxe 
new_test<-test[-7] #je retire la col nb agent
new_test<-new_test[-8] #je retire la col IDX

#colnames(new_test)

#on retire les NA du code region qui empeche le nnet de fonctionner
new_test <- new_test %>%
  filter(new_test$Code.region == 53) %>%##filtre par academie
  select(Type.etablissement, Secteur.enseignement,Groupe.de.personnels, Titulaire, Sexe, Borne.inferieure.de.la.tranche.age,Code.region,Agent)##selectionne les colonnes qu'on veut

#new_test<-new_test[-7] #je retire la col code.region (car 1 seule val)

colnames(new_test)

#pred2<-predict(model.dis, newdata = new_test,type ="class",na.action=na.omit)
#length(pred2)
#summary(model.dis)

#mat = table(pred2, new_test$Titulaire)

set.seed(2811)
model.dis2 <- nnet(train2$Titulaire~.,data = train2, size = 5, decay = 0.001,na.action=na.omit)
pred3<-predict(model.dis2, newdata = new_test,type ="class",na.action=na.omit)
mat2 = table(pred3, new_test$Titulaire)
mat2

#Size=5 est obtenu a la main 
#(size=5 dans nnet empiriquement le meilleur resultat, les resultats ne changent pratiquement pas
#si j'accrois le nombre de couches)

#result avec Percepteur Multi Couches (PMC) (reseau de neuronnes)
#Confusion Matrix and Statistics

conf_nnet <- confusionMatrix(data = as.factor(pred3), reference = test$Titulaire)
conf_nnet

#Confusion Matrix and Statistics

#Confusion Matrix and Statistics

#Reference
#Prediction    Contractuel Titulaire
#Contractuel        7075       230
#Titulaire           794      9326

#Accuracy : 0.9412 

conf_nnet$byClass["Sensitivity"] 
#0.8990977        taux de bonnes predictions pour les contractuel 

conf_nnet$byClass["Specificity"] 
#0.9759314 

write.csv(pred3,file=".\\temp_nnet.csv")




