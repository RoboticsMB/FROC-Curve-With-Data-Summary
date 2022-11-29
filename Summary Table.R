library(readxl)
library(data.table)
library(dplyr)
df <- read_excel("D:/User Files/Matthew/R Package For Dad/Database.xlsx")
Subjects <- unique(df$case)
Techniques <- unique(df$modality)
Tumor <- 0
NoTumor <- 0
TruePositive <- c()
TrueNegative <- c()
FalsePositive <- c()
FalseNegative <- c()

#and adding a comment here too to test github
FROCDataFindings <- data.frame('Condition' = character(), 'Min' = double(), 'Max' = double(), 'Average' = double(), 'Standard Deviation' = double())

for (modal in Techniques){
  dfModal <- df[df$modality == modal,]
  for (Subject in Subjects){
    OneSubject <- dfModal[dfModal$case == Subject,]
    HasTumor <- nrow(OneSubject[OneSubject$target == 1,]) != 0
    if (HasTumor){
     Tumor  = Tumor+1
    }else{
     NoTumor = NoTumor+1
    }
    P <- OneSubject[OneSubject$rating != 0,]
   TruePositive = append(TruePositive, nrow(P[P$target == 1,]))
   FalsePositive = append(FalsePositive, nrow(P[P$target == 0,]))
   N <- OneSubject[OneSubject$rating == 0,]
   FalseNegative = append(FalseNegative, nrow(N[N$target == 1,]))
   TrueNegative = append(TrueNegative, nrow(N[N$target == 0,]))
  }
  print(paste("Module", modal, sep=" "))
  FROCDataFindings[nrow(FROCDataFindings) + 1,] <- c('TruePositive', min(TruePositive), max(TruePositive), (sum(TruePositive)/length(TruePositive)), sd(TruePositive))
  FROCDataFindings[nrow(FROCDataFindings) + 1,] <- c('TrueNegative', min(TrueNegative), max(TrueNegative), (sum(TrueNegative)/length(TrueNegative)), sd(TrueNegative))
  FROCDataFindings[nrow(FROCDataFindings) + 1,] <- c('FalsePositive', min(FalsePositive), max(FalsePositive), (sum(FalsePositive)/length(FalsePositive)), sd(FalsePositive))
  FROCDataFindings[nrow(FROCDataFindings) + 1,] <- c('FalseNegative', min(FalseNegative), max(FalseNegative), (sum(FalseNegative)/length(FalseNegative)), sd(FalseNegative))
  FROCDataFindings[nrow(FROCDataFindings) + 1,] <- c('Ratings', min(df$rating), max(df$rating),(sum(df$rating)/length(df$rating)), sd(df$rating))
  print(FROCDataFindings)
  FROCDataFindings = FROCDataFindings[0,]
  TruePositive=c()
  TrueNegative=c()
  FalsePositive=c()
  FalseNegative=c()
}




#if there is rating and target then TP
#if there is rating but no target then FP
#if there is no rating but there is target then FN
#if there is no rating and target then TN
  
  
