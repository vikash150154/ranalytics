#Data Summarisation using Dplyr 
#dataset - mtcars

#dplyr - mtcars
#install.packages('dplyr')
library(dplyr) #install this library
mtcars
head(mtcars)
tail(mtcars)
names(mtcars)
summary(mtcars)
hist(mtcars$mpg)
# summary
mtcars %>% filter(mpg>25 & gear ==5)
mtcars %>% group_by(gear) %>% summarise(n=n(), mean(mpg)) # this is used to (count n=n())
                                          

#Plan how would you like to summarise this dataset

#dplyr - mtcars
library(dplyr)
df = mtcars
df$carnames = rownames(mtcars)
df = cbind(cars=rownames(mtcars), mtcars)
df %>% mutate(add_c)
head(df)
head(df)
#Filter----
filter(df, cyl == 8) %>% select(cars, mpg,cyl)

filter(mtcars, cyl == 8)
filter(mtcars, cyl < 6)

# Multiple criteria
filter(mtcars, cyl < 6 & vs == 1)
filter(mtcars, cyl < 6 | vs == 1)

# Multiple arguments are equivalent to and
filter(mtcars, cyl < 6, vs == 1)

#Select rows 
#by rownumber
filter(mtcars, row_number() == 1L)
filter(mtcars, row_number() == n())
filter(mtcars, between(row_number(), 5, n()))  # 5 th to the last row

#slice-----
slice(mtcars, 1L) # first row , because it is 
slice(mtcars, n())
slice(mtcars, 5:n())
slice(mtcars, c(2,4,5,10))
# aletranate rows
slice(mtcars, seq(1, nrow(mtcars),2))
dim(slice(mtcars, seq(1, nrow(mtcars),2)))

#mutate----
#create new columns based on existing columns
# increase the mlae by 30%
mutate(mtcars, mpg_new=mpg*1.3)
mutate(mtcars, displ_l = disp / 61.0237) #keeps other col

#GroupBy summary

mtcars %>% group_by(am) %>% summarise(mean(mpg), max(wt))
mtcars %>% group_by(am) %>% summarise(MEANMPG = mean(mpg), MAXWT= max(wt))
mtcars %>% group_by(am, gear) %>% summarise_all(mean)
mtcars %>% group_by(am, gear)%>% summarise_all(c("min", "max"))

#specific columns
mtcars %>% summarise_at(c("mpg", "wt"), mean, na.rm = TRUE) # removing missing values na.rm
(x=c(5,6,7,NA))
mean(x, na.rm= TRUE)

#select rows on the basis of sample

sample_frac(mtcars, 0.2, replace=T) # 20% OF THE ROWS ON RANDOM BASIS 
sample_n(mtcars, 60, replace=T) %>% select(mpg)
sample_n(mtcars, 5, replace=F) %>% select(mpg)
mtcars %>% sample_n(5, replace=F) %>% select(mpg, wt, gear)
mtcars %>% sample_n(10, replace=F) %>% select(mpg, wt, gear,hp) %>% group_by(gear) %>% summarise(mean(mpg), max_wt =max(wt), min(hp)) %>% arrange(max_wt)
#Rows having least mpg (last 2)
top_n(mtcars,-3, mpg)
mtcars %>% arrange(-mpg)
select(mtcars, mpg) %>% arrange(desc(mpg))
mtcars %>% select (mpg) %>% hist
library


ggplot(data=mtcars, aes(x=gear)) +
  geom_bar(stat="count", color='red', fill='green')

ggplot(data=mtcars, aes(x=gear, fill=factor(gear))) +
  geom_bar(stat="count", color='red', fill='green')


ggplot(data=mtcars, aes(x=gear, y=mpg, group=1)) +
  geom_line()+
  geom_point()
women
?women
head(women)
dim(women)
plot(women)
cor(women) # strenght(strong) and direction(positve or negative) of realtion -1 to 0 to +1
cov(mtcars$mpg, mtcars$wt)
# is height the cause of change in weight - do linera regression
fit=lm(weight~height, data=women)
summary(fit)
predict(fit, 71.5)

# multiple liner regression
head (mtcars)
df <- mtcars %>% select(mpg, wt, hp)
head(df)
fit2=lm(mpg~wt+hp, data=df)
summary(fit2)
fitted(fit2)
cbind(df$mpg, fitted(fit2),resid(fit2))
summary(fit2)
resid(fit2)
df2=women
df3=rbind(df2,c(73,500))
fit2b=lm(weight~height,data=df3)
summary(fit2b)
plot(fit2b)

