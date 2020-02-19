##ALEXA CANAAN
##ACTIVITY 4


#use built in iris dataset
#petal measurements of iris plants
#take a look at it 
head(iris)
#load in some tidyverse packages
#install.packages("dplyr")
#install.packages("ggplot2")
library(dplyr)
library(ggplot2)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

#subset the data to only include iris versicolor
versicolor <- iris[iris$Species=="versicolor", ]

#regressions can be run in different ways
lm.out <- lm(versicolor$Sepal.Width~versicolor$Sepal.Length)
lm.out <- lm(versicolor[ ,"Sepal.Width"]~ versicolor[,"Sepal.Length"])
#regression output in a list
str(lm.out)
#summary stats of the regression
summary(lm.out)

#y ~ x
#dependent variables is a function of independent variable

#create vectors that will be iterated through
x <- c("Sepal.Length", "Petal.Length", "Sepal.Length")
y <- c("Sepal.Width", "Petal.Width", "Petal.Length")
#create a blank list that will store the information from the for loop
lm.out <- list()

#produces regression table for the 3 relationships above
for (i in 1:3){
  lm.out[[i]] <- lm(versicolor[ ,y[i]] ~ versicolor[ ,x[i]])
}

#check the output
lm.out[[1]]
lm.out[[2]]
lm.out[[3]]

#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))

#iris left
#height right
#join tables keeping all info from iris and adding height info
iris2<-left_join(iris, height, by="Species")
#normalize petal width and height
iris2$Petal.Width/iris2$Height.cm

#####################################
##### Part 3: plots in ggplot2  #####
#####################################
#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
ggplot(data=iris, aes(Sepal.Length, Sepal.Width))+
  geom_point()+
  theme_bw()
  

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
ggplot(data=iris, aes(Sepal.Length, Sepal.Width))+
  geom_point()+
  theme_classic()

#3c.make a scatter plot with ggplot and get rid of grid lines
#and show species by color increasing the point size
ggplot(data=iris, aes(Sepal.Length, Sepal.Width, color=Species))+
  geom_point(size=4)+
  theme_classic()

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################
#Overall, there is a change in syntax between plot and ggplot. 
#For example, in ggplot to specify color you would say "color",
#but in plot you would say col