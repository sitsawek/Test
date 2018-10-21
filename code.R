library(dplyr)
dat <- read.csv("femaleMiceWeights.csv")
controls <- filter(dat,Diet == "chow")%>%select(Bodyweight)%>%unlist
treatment <- filter(dat,Diet == "hf")%>%select(Bodyweight)%>%unlist
obs <- mean(treatment)-mean(controls)
population <- unlist(population)
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )
set.seed(1)
n <- 1000
nulls <- vector("numeric",n)
for (i in 1:n){
    c2 <- sample(x,50)
    b <- mean(control)
    #b <- mean(x)
    #treatment <- sample(x,5)
    #nulls[i] <- a-b 
}
mean(nulls > 1) 
sum(nulls-obs)/n
mean(abs(nulls)>1)
a = subset(g,year == 1952)
prop = function(q) {
    mean(x<= q)
}
prop(40)
qs = seq(from=min(x), to=max(x), length=20)
props = sapply(qs, prop)
props = sapply(qs, function(q) mean(x <= q))
x <- filter(dat,Diet=="chow"&Sex == "F")%>%select(Bodyweight)%>%unlist
y <- filter(dat,Diet == "hf"& Sex =="F")%>%select(Bodyweight)%>%unlist
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
mean( abs(z) <=1 )
mypar(2,2)
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
