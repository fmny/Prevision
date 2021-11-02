################################################################
#Utilisation de RandomForest en vue de faire de la prevision:
###############################################################

#lien
#https://thinkr.fr/premiers-pas-en-machine-learning-avec-r-volume-4-random-forest/


#import du fichier CSv sans accent 

#Chemin ? modifier
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
#dplyr contient les op?rateurs %>% qui permettent le data wrangling (operation sur la BDD)


educ_small <- educ %>%
  filter(Academie == "RENNES") %>%##filtre par academie
  select(Type.etablissement, Secteur.enseignement,Groupe.de.personnels, Titulaire, Sexe, Borne.inferieure.de.la.tranche.age, Nombre.agents,Code.region)##selectionne les colonnes qu'on veut


#faire tourner ces operations 1 par 1 pour comprendre le data wrangling

educ_small_test <- educ_small %>% 
  mutate(Idx = 1:n()) %>%
  group_by(Idx) %>% #ne sert ? rien
  mutate( Agent = list(rep( Sexe, Nombre.agents) ) ) %>%
  unnest() %>% # explose la base de maniere ? avoir 87123 ligne (cad le nb d agent)
  ungroup() %>% #ne sert a rien
  select(-Idx) #retire la colonne Idx


sum(educ_small$Nombre.agents)
#[1] 43510    #sur le site data ancienne
#[1] 87123    #resultat FM

nrow(educ_small)
#[1] 16248    #sur le site data ancienne
#[1] 32107    #resultat FM



sum(educ_small_test$Nombre.agents)
#611857 (ici, il faut modifier cette colonne )

#on remplace Nombre.agents par 1 dans la colonne
educ_small_test$Nombre.agents<-1


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


set.seed(2811)

train <- educ_small2 %>% sample_frac(0.8) #prend 80% de la base pour le training
nrow(train)
#69698
test <- anti_join(educ_small2, train) #prend les 20% restant
nrow(test)
#17425/87123=0.2000046


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

#summary(test$predicted)
#Contractuel   Titulaire        NA's 
#       7776        9485         164 
# summary(test$Titulaire)
#Contractuel   Titulaire 
#       7847        9578 


summary(test$predicted)
summary(test$Titulaire)
nrow(test)

#Fichier des résulats avec la colonne de resultat des previsions titulaire et contractuel
#write.csv(test$predicted,file=".\\temp.csv")
write.csv(test,file=".\\test.csv")

library(caret)
conf <- confusionMatrix(data = test$predicted, reference = test$Titulaire)

conf$byClass["Sensitivity"] #taux de vrais positifs



#resultats refaits 164 NA dans la colonne region
#nrow(test)-164 = 17261


#conf$byClass["Sensitivity"] #taux de vrais positifs
#Sensitivity 
#0.9518874      #7439/7815
#conf$byClass["Specificity"] #taux de vrais negatifs
#Specificity 
#0.9643235      #9109/9446

#conf
#              Reference
#Prediction    Contractuel Titulaire
#Contractuel        7439       337
#Titulaire           376      9109

#lecture du tableau
# on a donc 7439 Contractuel bien predit et 337 mal predit (etaient des titulaires)
#les bonnes predictions sont sur la diagonale
#Accuracy : 0.9587  taux de bonnes prevision

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


#ptitanicTree <- rpart(survived~.,data=ptitanic)
#educ_small2_Tree <- rpart(Titulaire~.,data=educ_small2)


educ_Tree <- rpart(Titulaire~.,data=train,control=rpart.control(minsplit=5,cp=0),na.action=na.pass)
#on retire les NA 


#prp(educ_Tree,extra=1) #graphe complet mais illisible
#summary(educ_Tree)

#Rem: minsplit =5, signifie que l'on ne redecoupe une branche 
#en plusieurs feuilles que si celle-ci possede au moins 5 obs
#par defaut minsplit=20 si l'on ne le precise pas

#plotcp(educ_Tree) 
educ_Tree$cptable
write.csv(educ_Tree$cptable,file=".\\educ_tree_cptable.csv")


#(reprendre ici)


#Simplification
#Comme attendu, les performances s ameliorent dans un premier temps quand on augmente 
#le nombre de feuilles puis se degradent en raison du sur-apprentissage. On choisit en general la complexite qui minimise 
#l erreur estimee

educ_Simple <- prune(educ_Tree,cp=0.0000316)

#test
#educ_Simple <- prune(educ_Tree,cp=0.001)

plotcp(educ_Simple) #le graphe optimal a ? feuilles
prp(educ_Simple,extra=1)


educ_Tree_optimal <- prune(educ_Tree,cp=educ_Tree$cptable[which.min(educ_Tree$cptable[,4]),1])
#ptitanicOptimal <- prune(ptitanicTree,cp=ptitanicTree$cptable[which.min(ptitanicTree$cptable[,4]),1])
print(educ_Tree$cptable[which.min(educ_Tree$cptable[,4]),1])

plotcp(educ_Tree_optimal) #le graphe optimal a ? feuilles
prp(educ_Tree_optimal,extra=1)
#l arbre semble etre trop touffu, cependant les previsions sont plutôt bonnes


#Par defaut, la fonction estime les probabilites d appartenance aux classes pour chaque observation 
#(simplement par le ratio dans la feuille correspondante). Par exemple le code suivant
predict(educ_Tree, test, type="class")

table(test$Titulaire, predict(educ_Tree, test, type="class"))

#result rpart
#               Contractuel Titulaire
#Contractuel        7639       208
#Titulaire           223      9355
#ca a l air bon 


tree<-educ_Tree
data<- test
#Pour obtenir la classe predite, il suffit d ajouter 
#le parametre type avec la bonne valeur, soit :


#predict(ptitanicOptimal, type="class")

pred<-predict(tree, test, type="class",na.action = na.omit)

write.csv(pred,file=".\\temp_rpart.csv")

#on observe des resultats sensiblement identiques avec rpart que random forest

library(caret)
#conf <- confusionMatrix(data = test$predicted, reference = test$Titulaire)
conf_rpart <- confusionMatrix(data = predict(educ_Tree, test, type="class"), reference = test$Titulaire)
conf_rpart

#Confusion Matrix and Statistics

#Reference
#Prediction    Contractuel Titulaire
#Contractuel        7639       223
#Titulaire           208      9355

#Accuracy : 0.9753 j obtiens un resultat meilleur qu avec random forest (?)


conf_rpart$byClass["Sensitivity"] #taux de vrais positifs

#result random forest
#0.9734931 taux de bonnes predictions pour les contractuel 

conf_rpart$byClass["Specificity"] #taux de vrais negatifs
#Specificity  0.9767175



######################################
#utilisation d un reseau de neuronnes
####################################

library(nnet)
library(MASS)


#nombre agent et Idx ne sont pas des factors

#attention ? cette syntaxe la repetition du code peut poser probleme
train2<-train[-7] #je retire la col nb agent
train2<-train2[-8] #je retire la col IDX
train2<-train2[-7] #je retire la col code.region (car 1 seule val)
colnames(train2)

#la colonne code.region pose problem car elle ne contient qu une val =53 et NA

#train2$Titulaire<-factor(train2$Titulaire)



model.dis <- nnet(train2$Titulaire~.,data = train2, size = 2, decay = 0.001,na.action=na.omit)

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

#attention ? cette syntaxe 
new_test<-test[-7] #je retire la col nb agent
new_test<-new_test[-8] #je retire la col IDX

#colnames(new_test)

#on retire les NA du code region qui empeche le nnet de fonctionner
new_test <- new_test %>%
  filter(new_test$Code.region == 53) %>%##filtre par academie
  select(Type.etablissement, Secteur.enseignement,Groupe.de.personnels, Titulaire, Sexe, Borne.inferieure.de.la.tranche.age,Code.region,Agent)##selectionne les colonnes qu'on veut

new_test<-new_test[-7] #je retire la col code.region (car 1 seule val)

colnames(new_test)

pred2<-predict(model.dis, newdata = new_test,type ="class",na.action=na.omit)
length(pred2)
#summary(model.dis)


mat = table(pred2, new_test$Titulaire)

model.dis2 <- nnet(train2$Titulaire~.,data = train2, size = 10, decay = 0.001,na.action=na.omit)
pred3<-predict(model.dis2, newdata = new_test,type ="class",na.action=na.omit)
mat2 = table(pred3, new_test$Titulaire)
mat2

#result avec Percepteur Multi Couches (PMC) (reseau de neuronnes)
#pred3         Contractuel Titulaire
#Contractuel        7056       210
#Titulaire           791      9368

#revoir
#conf_nnet <- confusionMatrix(data = pred3, reference = test$Titulaire)
#conf_nnet


write.csv(pred3,file=".\\temp_nnet.csv")




