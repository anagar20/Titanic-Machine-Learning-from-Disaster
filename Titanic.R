train = read.csv("train.csv",na.strings = c(""))
test = read.csv("test.csv",na.strings = c(""))
str(train)
summary(train)
csv= train
train = train[,-2]

combi = rbind(train,test)
combi$Name = as.character(combi$Name)
combi$Title = sapply(combi$Name, FUN=function(x) {strsplit(x,split='[,.]')[[1]][2]})
combi$Title = sub(' ','',combi$Title)
combi$Title[combi$Title %in% c('Mme','Mlle')]='Mlle'
combi$Title[combi$Title %in% c('Capt','Don','Major','Sir')]='Sir'
combi$Title[combi$Title %in% c('Dona','Lady','the Countless','Johnkheer')]='Lady'
combi$Title = factor(combi$Title)
combi$FamilySize = combi$SibSp+combi$Parch+1
combi$Surname = sapply(combi$Name,FUN =function(x){strsplit(x,split='[,.]')[[1]][1]})
combi$FamilyID = paste(as.character(combi$FamilySize),combi$Surname,sep="")
combi$FamilyID[combi$FamilySize<=2] = 'Small'
famIDs = data.frame(table(combi$FamilyID))
famIDs = famIDs[famIDs$Freq<=2, ]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1]='Small'
combi$FamilyID=factor(combi$FamilyID)

combi$Embarked[which(is.na(combi$Embarked))] = 'S'

na_index1_1 =  which(combi$Pclass==1)
na_index1 = which(combi$Pclass==1 & is.na(combi$Age)) 
median_age_1=median(combi$Age[na_index1_1],na.rm=TRUE)
combi$Age[na_index1] = median_age_1

na_index1_2 =  which(combi$Pclass==2)
na_index2 = which(combi$Pclass==2 & is.na(combi$Age)) 
median_age_2=median(combi$Age[na_index1_2],na.rm=TRUE)
combi$Age[na_index2] = median_age_2

na_index1_3 =  which(combi$Pclass==3)
na_index3 = which(combi$Pclass==3 & is.na(combi$Age)) 
median_age_3=median(combi$Age[na_index1_3],na.rm=TRUE)
combi$Age[na_index3] = median_age_3

ind1 = which(combi$Fare>500)
combi$Fare[ind1] = NA
ind1_1 = which(is.na(combi$Fare))
median_Fare=median(combi$Fare,na.rm=TRUE)
combi$Fare[ind1_1] = median_Fare

ind2 = which(combi$Parch==9)
combi$Parch[ind2] = NA
median_Parch=median(combi$Parch,na.rm=TRUE)
combi$Fare[ind2] = median_Parch

combi
train = combi[1:891, ]
test = combi[892:1309, ]
train$Survived = csv$Survived

library(party)
library(caret)
fit = cforest(as.factor(Survived)~Pclass+Age+Sex+SibSp+Parch+Fare+
                Embarked+Title+FamilySize+FamilyID,
              data=train,
              controls = cforest_unbiased(ntree=2000,mtry=c(3,4,5)))
pp = predict(fit,test,OOB=TRUE,type="response")


submission <- data.frame(PassengerID = test$PassengerId, Survived = pp)
write.csv(submission, file = 'Submission.csv', row.names = F)
