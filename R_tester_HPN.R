gapminder.base <- read.csv(file = "datasets/gapminder.txt", header=TRUE, sep = "\t", stringsAsFactors = FALSE)
getwd()

a=5
a==5
a=9
b=7
a==b
a=b

plot(gapminder$lifeExp, gapminder$gdpPercap)
plot(gapminder $gdpPercap,gapminder $lifeExp)
plot(gapminder$pop,gapminder$gdpPercap)

gapminder$life_month <- gapminder$lifeExp*12

avg.life = mean(gapminder$lifeExp)
is.above.avg <- gapminder$lifeExp > avg.life
gapminder[is.above.avg,]

boxplot(lifeExp ~continent, data = gapminder)

library(readr)
gapminder <- read_delim(file = "datasets/gapminder.txt", 
                        delim = "\t", escape_double = FALSE, trim_ws = TRUE)

gapminder[gapminder$pop == min(gapminder$pop),]

library('dplyr')

class(gapminder)

mat = matrix(data = 1:25, ncol = 5, byrow = TRUE)
mat

mat2 = matrix(data = 0, ncol = 5, nrow =5, byrow = TRUE)
mat2

mat3 = matrix(data = runif(100), ncol = 5, nrow =5, byrow = TRUE)
mat3

gapminder$year = factor(gapminder$year, ordered = TRUE)

#exercise

test.frame <- data.frame(Day=1:5, Magnification=c(2,10,5,2,5), Observation=c('Growth', 'Death', 'No Change', 'Death', 'Growth'))
test.frame$Magnification = factor(test.frame$Magnification, ordered = TRUE)

Mag = test.frame$Magnification
class(Mag)

Filter <- test.frame[test.frame$Magnification == 5,]
Filter
nrow(Filter)

test.frame$hours = test.frame$Day*24
test.frame

#loops

# unique() only pulls contigous repeating values ... sort first!
# Base R unique creates vectors, dplyer creates a daa frame

continents <- sort(gapminder$continent)
#continents <- sort(gapminder$country)
for(cont in unique(sort(gapminder$continent))){
     life.min <- min(gapminder[gapminder$continent == cont, 'lifeExp'])
     life.max <- max(gapminder[gapminder$continent == cont, 'lifeExp'])
     life.med <- mean(gapminder[gapminder$continent == cont, 'lifeExp'])
     #print(paste0('The life expectancy in ',cont,' is ',life.min,' to ',life.max,'. The median life expectancy is ',life.med,'.'))
}

y = 0
z=0
for (x in gapminder$lifeExp){
     y = y+x
     z = z+1
     m = y/x
     t = x/z

     if (class(x)!='numeric'){
          print(paste0(x,' at row ',z, ' is not number'))
     }
}

countries <- sort(gapminder$country)
for(cont in unique(countries)){
     life.min <- min(gapminder[gapminder$country == cont, 'lifeExp'])
     life.max <- max(gapminder[gapminder$country == cont, 'lifeExp'])
     life.med <- median(gapminder[gapminder$country == cont, 'lifeExp'])
     print(paste0('The life expectancy in ',cont,' is ',life.min,' to ',life.max,'. The median life expectancy is ',life.med,'.'))
}

for(cont in unique(sort(gapminder$country))){
     life.min <- min(gapminder[gapminder$country == cont, 'lifeExp'])
     life.max <- max(gapminder[gapminder$country == cont, 'lifeExp'])
     a= subset(gapminder[gapminder$country == cont, 'lifeExp'])
     life.med <- median(a$lifeExp)
     print(paste0('The life expectancy in ',cont,' is ',life.min,' to ',life.max,'. The median life expectancy is ',life.med,'.'))
}

cont = 'Germany'
a= subset(gapminder[gapminder$country == cont, 'lifeExp'])
structure(a)
class(a)
mean(a$lifeExp)
structure(gapminder[gapminder$country == cont, 'lifeExp'])

loop.vector <- c(1, 4, 9, 16, 25, 36, 49, 64, 81, 100)
structure(loop.vector)
class(loop.vector)
mean(loop.vector)

for (x in 1:10){
     y = x*10
     print(paste0('X ',x, ' multiplied by 10 is ',y))
}

par(bg = 'white')
plot(gapminder$year,gapminder$pop, col  = rainbow(15))

colors()

# Apply FUnctions
loop.vector <- c(1, 4, 9, 16, 25, 36, 49, 64, 81, 100)
root.vector <- sapply(loop.vector, function(x) sqrt(x))
root.vector

#iF ELSE Statements



# Plotting
iris <-read.csv(file = 'datasets/iris.txt', header = T, sep = '\t')

# elements get added to plot until next plot command
# plot (x~y)
plot(iris$Sepal.Length, iris$Petal.Length, xlab ='Sepal Length', ylab = 'Petal Length')

#lm (y~x)
abline(lm(iris$Petal.Length~iris$Sepal.Length))

#boxplot (y~x)
boxplot (iris$Sepal.Length ~ iris$Species, col=rainbow(3))

#ggplot2
library(ggplot2)
ggplot(data = iris, aes(x=Petal.Length,  y=Petal.Width))+geom_point()

ggplot(iris, aes(x=Petal.Length,  y=Petal.Width))

# geom_point for scatter plots
ggplot(iris, aes(x=Sepal.Length,  y=Sepal.Width))+geom_point()+
     geom_smooth(method ="lm")
                                                                                  
ggplot(gapminder, aes(x=pop))+geom_histogram()
ggplot(gapminder, aes(x=pop, y=lifeExp))+geom_point()+geom_smooth(method ="lm")

ggplot(iris, aes(x=Sepal.Length, fill=Species))+geom_histogram(position = "identity", alpha =0.7)
ggplot(iris, aes(x=Petal.Length,  y=Petal.Width, color=Species, shape = Species)) + geom_point()

median(c(1,2,3,4,5))

#Boxplots

cont = c('Syria', 'Spain', 'Thailand')
gap.select = gapminder[gapminder$country %in% cont, ]
gap.select = gapminder

ggplot(gap.select, aes(x=continent, y=pop, fill = continent))+geom_boxplot()+
     labs(x = 'Country', y = 'Population')+
     facet_grid(.~year, scales = 'free')


