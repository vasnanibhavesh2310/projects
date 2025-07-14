movies <- read.csv(file.choose())

head(movies)

colnames(movies) <- c('Film', 'Genre', 'CriticRating', 'AudienceRating', 'BudgetMillions', 'Year')

head(movies)
tail(movies)

str(movies)
summary(movies)

movies$Film <- factor(movies$Film)
movies$Genre <- factor(movies$Genre)
movies$Year <- factor(movies$Year)

summary(movies)
str(movies)


#------------------------------------------------- Aesthetics

library(ggplot2)

ggplot(data = movies, aes(x=CriticRating, y=AudienceRating))

# add geometry

ggplot(data = movies, aes(x=CriticRating, y=AudienceRating)) +
  geom_point()

# add colour

ggplot(data = movies, aes(x=CriticRating, y=AudienceRating,
                          colour=Genre)) +
  geom_point()

# add size

ggplot(data = movies, aes(x=CriticRating, y=AudienceRating,
                          colour=Genre, size=Genre)) +
  geom_point()

# add size - better way

ggplot(data = movies, aes(x=CriticRating, y=AudienceRating,
                          colour=Genre, size=BudgetMillions)) +
  geom_point()

#----------------------------------------------------- Plotting with layers

p <- ggplot(data = movies, aes(x=CriticRating, y=AudienceRating,
                               colour=Genre, size=BudgetMillions)) 
  
p + geom_point()

p + geom_line()

# plotting multiple layers

p + geom_point() + geom_line()
p + geom_line() + geom_point()


#----------------------------------------------------- Overriding aesthetics

q <- ggplot(data = movies, aes(x=CriticRating, y=AudienceRating,
                               colour=Genre, size=BudgetMillions)) 

q + geom_point()

#overriding aesthetics

#ex1
q + geom_point(aes(size = CriticRating))

#ex2
q + geom_point(aes(colour = BudgetMillions))

# q remains the same
q + geom_point()

#ex3
q + geom_point(aes(x=BudgetMillions)) +
  xlab('Budget Millions $$$')

#ex4
q + geom_line(size=1) + geom_point()


#----------------------------------------------------- Mapping vs Setting


r <- ggplot(data = movies, aes(x=CriticRating, y=AudienceRating))

r + geom_point()

#add colour
#1. Mapping
r + geom_point(aes(colour=Genre))

#2. Setting
r + geom_point(colour='DarkGreen')

#1. Mapping
r + geom_point(aes(size=BudgetMillions))

#2. Setting
r + geom_point(size=10)

#------------------------------------------ Histogram and density charts

s <- ggplot(data = movies, aes(x=BudgetMillions))
s + geom_histogram(binwidth=10)

# add colour
s + geom_histogram(binwidth=10, aes(fill=Genre))

# add border
s + geom_histogram(binwidth=10, aes(fill=Genre), colour='Black')


# density charts
s + geom_density(aes(fill=Genre))
s + geom_density(aes(fill=Genre), position='stack')

#------------------------------------------ Starting Layer Tips

t <- ggplot(data=movies, aes(x=AudienceRating))
t + geom_histogram(binwidth = 10, 
                   fill = 'white', colour = 'blue')

# another way

t <- ggplot(data=movies)
t + geom_histogram(binwidth = 10,
                   aes(x=AudienceRating),
                   fill = 'white', colour = 'blue')

# 4 
t + geom_histogram(binwidth = 10,
                   aes(x=CriticRating),
                   fill = 'white', colour = 'blue')

#5

t <- ggplot()

#------------------------------------------ Statistical Transformations

?geom_smooth

u <- ggplot(data = movies, aes(x=CriticRating, y=AudienceRating,
                               colour=Genre)) 
u + geom_point() + geom_smooth(fill = NA)

#boxplots

u <- ggplot(data = movies, aes(x=Genre, y=AudienceRating,
                               colour=Genre))
u + geom_boxplot(size=1.2)

u + geom_boxplot(size=1.2) + geom_point()

#tip
u + geom_boxplot(size=1.2) + geom_jitter()

#another way:
u + geom_jitter() + geom_boxplot(size=1.2, alpha=0.5)

#------------------------------------------ Using Facets

#facets
v <- ggplot(data = movies, aes(x=BudgetMillions))

v + geom_histogram(binwidth=10, aes(fill=Genre),
                   colour='Black') +
  facet_grid(Genre~., scales='free')

#scatterplots
w <- ggplot(data = movies, aes(x=CriticRating, y=AudienceRating,
                               colour=Genre)) 

w + geom_point(size=3) +
facet_grid(Genre~.)

w + geom_point(size=3) +
  facet_grid(.~Year)

w + geom_point(size=3) +
  facet_grid(Genre~Year)

w + geom_point(size=3) +
  geom_smooth() +
  facet_grid(Genre~Year)

w + geom_point(aes(size=BudgetMillions)) +
  geom_smooth() +
  facet_grid(Genre~Year)

#------------------------------------------ Coordinates

#Today
#limits
#zoom

m <- ggplot(data = movies, aes(x=CriticRating, y=AudienceRating,
                               size=BudgetMillions,
                               colour=Genre)) 

m + geom_point()

m + geom_point() +
  xlim(50,100) +
  ylim(50,100)

# won't work well always

n <- ggplot(data = movies, aes(x=BudgetMillions))

n + geom_histogram(binwidth = 10, aes(fill = Genre), colour = 'Black')

n + geom_histogram(binwidth = 10, aes(fill = Genre), colour = 'Black') +
  ylim(0,50)

#instead zoom in 

n + geom_histogram(binwidth = 10, aes(fill = Genre), colour = 'Black') +
  coord_cartesian(ylim=c(0,50))

#improving #1

w + geom_point(aes(size=BudgetMillions)) +
  geom_smooth() +
  facet_grid(Genre~Year) +
  coord_cartesian(ylim = c(0,100))

#------------------------------------------ Theme

o <- ggplot(data = movies, aes(x=BudgetMillions))
h <- o + geom_histogram(binwidth=10, aes(fill = Genre), colour = 'Black')

# axes label
h +
  xlab('Money Axis') +
  ylab('Number of Movies') +
  theme(axis.title.x = element_text(colour = 'DarkGreen', size = 30),
        axis.title.y = element_text(colour = 'Red', size = 30))

# tick formatting

h +
  xlab('Money Axis') +
  ylab('Number of Movies') +
  theme(axis.title.x = element_text(colour = 'DarkGreen', size = 30),
        axis.title.y = element_text(colour = 'Red', size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

?theme

# legend formatting

h +
  xlab('Money Axis') +
  ylab('Number of Movies') +
  ggtitle('Movie Budget Distribution') +
  theme(axis.title.x = element_text(colour = 'DarkGreen', size = 30),
        axis.title.y = element_text(colour = 'Red', size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        
        legend.title = element_text(size=30),
        legend.text = element_text(size=20),
        legend.position = c(1,1),
        legend.justification = c(1,1),
        
        plot.title = element_text(colour = 'DarkBlue',
                                  size = 40,
                                  family = 'Courier'))
