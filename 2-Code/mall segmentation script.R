##Loading packages

library(tidyverse)
library(magrittr)
library(recipes)
library(psycho)
library(psych)
library(lsr)
library(descr)
library(ggplot2)
library(waffle)
library(here)
#loading the data
df = read_csv(here("1-Input Files","Mall segm.csv"))
#testing
#Preliminary Data exploration
summary(df)
sd(df$Age)
par(mfrow= c(2,2))
boxplot(df$Age, main = "Edad")
boxplot(df$`Annual Income (k$)`, main = "Salario anual (k$)")
boxplot(df$`Spending Score (1-100)`, main = "Spending Score")

#There are no NAs
#There are some outliers, but they'r not too far from the highest wages, so we'll leave them here

#We eliminate the sex variable from the cluster analysis. We'll use it as an illustrative variable later.

#Data Standarization


names(df) = c("CustomerID","Gender","Age","annual_income","score")

z_df = df %>% 
  select(-Gender) %>% 
  select(-CustomerID) %>% 
  scale() %>% 
  as.tibble() 

z_df = df %>% 
  select(-Gender) %>% 
  select(-CustomerID) %>% 
  psycho::standardize() %>% 
  mutate(annual_income = `Annual Income (k$)`) %>% 
  mutate(score = `Spending Score (1-100)`) %>% 
  select(-(`Annual Income (k$)`)) %>% 
  select(-(`Spending Score (1-100)`))


plot(z_df)

dd = dist(z_df, method = "euclidean")

hc = hclust(dd, method = "ward.D2")
par(mfrow = c(1,1))
plot(hc, hang = -1, cex = 0.5)

#Deciding the number of clusters

k_3 = cutree(hc, k =3)
k_4 = cutree(hc, k =4)
k_5 = cutree(hc, k =5)
k_6 = cutree(hc, k =6)

prop.table(table(k_3))
prop.table(table(k_4))
prop.table(table(k_5))
prop.table(table(k_6))

z_df$k_4 = k_4
z_df$k_5 = k_5
z_df$k_6 = k_6


sqrt(etaSquared(aov(Age~k_4, z_df)))
sqrt(etaSquared(aov(annual_income~k_4, z_df)))
sqrt(etaSquared(aov(score~k_4, z_df)))

sqrt(etaSquared(aov(Age~k_5, z_df)))
sqrt(etaSquared(aov(annual_income~k_5, z_df)))
sqrt(etaSquared(aov(score~k_5, z_df)))

sqrt(etaSquared(aov(Age~k_6, z_df)))
sqrt(etaSquared(aov(annual_income~k_6, z_df)))
sqrt(etaSquared(aov(score~k_6, z_df)))

#5 groups seem to be the best fit

##Profile graphs

summaries_k4 = describeBy(z_df[,1:3],z_df$k_4)
summaries_k5 = describeBy(z_df[,1:3],z_df$k_5)
summaries_k6 = describeBy(z_df[,1:3],z_df$k_6)

##Graphs on Averages: k = 5
plot(summaries_k5$'1'$vars,summaries_k5$'1'$mean,type="l",xaxt='n', ylim = c(-2,2),xlab="item",ylab="mitjana de grup", col = 1)
lines(summaries_k5$'2'$vars,summaries_k5$'2'$mean,type="l", col=2)
lines(summaries_k5$'3'$vars,summaries_k5$'3'$mean,type="l",col=3)
lines(summaries_k5$'4'$vars,summaries_k5$'4'$mean,type="l", col=4)
lines(summaries_k5$'5'$vars,summaries_k5$'5'$mean,type="l",col=5)
axis(1, at = c(1,2,3), labels = c("Age", "Annual_income", "Score"))
abline(h=0)
par(xpd = T)
legend(3,2, c("G1", "G2", "G3", "G4", "G5"), col = c(1,2,3,4,5), lty = c(1,1,1,1,1), cex = 0.6)

summaries_k5

#We validate the 5-group cluster

cdg = aggregate(as.data.frame(z_df),list(k_5),mean)[,2:4]
cdg
set.seed(1)
k_means = kmeans(z_df[,1:3], center = cdg)
table(k_means$cluster, z_df$k_5)

#Stability Index = 95%

## Illustrative analysis: Sex as an illustrative variable

z_df$gender = df$Gender
CrossTable(z_df$gender,z_df$k_5, chisq=T, expected = T, sresid = T)


library(readxl)
df.results = read_excel(here("1-Input Files","Malltble.xlsx")) %>% 

library(readxl)
df.results = read_excel("Malltble.xlsx") %>% 


library(readxl)
df.results = read_excel(here("1-Input Files","Malltble.xlsx")) %>% 

###Esto ya es otra cosa

library(readxl)
df.results = read_excel("Malltble.xlsx") %>% 
  na.omit %>% 
  mutate(lab.ypos = cumsum(prop)-0.5*(prop)) 

names(df.results) = (c("Grupo","Proporcion","Edad","Salario","Score","lab.ypos"))

#Graphs for the report
mycols <- c("Grupo 1" = "#4D4D4D","Grupo 2" ="#5DA5DA","Grupo 3" =  "#FAA43A","Grupo 4"= "#60BD68", "Grupo 5" = "#F17CB0")

df.results$Grupo = factor(df.results$Grupo, levels = c("Grupo 1", "Grupo 2", "Grupo 3", "Grupo 4", "Grupo 5"))

#Score Graphs
ggplot(data = df.results, aes(x=Grupo,y= Score))+
  geom_col(fill = mycols)+
  labs(title = "Spending Score and Group Size")+
  labs(title = "Spending Score y Peso de los Grupos   11%  33%   23%   19%   14%")+
  labs(title = "Spending Score and Group Size")+
  labs(title = "Spending Score y Peso de los Grupos   11%  33%   23%   19%   14%")+

  scale_fill_manual(values = mycols)+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank(), panel.background = element_blank())

##Proportion waffle chart

vals = c(11,33,23,19,14)
val_names = c("Grupo 1","Grupo 2","Grupo 3","Grupo 4","Grupo 5")
names(vals) = val_names

waffle(vals, title = "Group Sizes") +

vals = c(22,66,45,39,28)
val_names = c("Grupo 1","Grupo 2","Grupo 3","Grupo 4","Grupo 5")
names(vals) = val_names

waffle(vals) +
  scale_fill_manual(values = mycols)

summaries_k5

##Age and Wage graphs 
ggplot(data = df.results, aes(x=Salario, y=Edad, fill = Grupo))+
  geom_point(show.legend = F, size = 5, shape = 24)+
  labs(title = "¿Qué perfil tienen nuestros clientes? Edad y Salario de los mismos")+
  scale_fill_manual(values = mycols)+
  theme(panel.background = element_blank())

# It seems that sex is irrelevant  


