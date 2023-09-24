install.packages("gapminder")
library(gapminder)
library(dplyr)
library(ggplot2)
str(gapminder)
China <- gapminder %>% filter(country == "China") %>% head(10)
View(China)
ggplot(data = China, aes(x=year,y=lifeExp)) + geom_point(color = 'red', size = 3) + xlab('Year') +ylab('Life Expectancy')+ggtitle("Life Expectancy in China")+ theme_bw(base_size = 18)

ggplot(data = gapminder, aes(x= year, y=lifeExp, group =country,color =continent)) + geom_line() +xlab('Year')+ylab('Life Expectancy')+ggtitle("Life Expectancy in Countries")+theme_bw()
ggplot(data = gapminder, aes(x= year, y=lifeExp, group=country,color=continent))+geom_line()+theme_bw()+facet_wrap(~continent)+ xlab('Year')+ylab('Life Expectancy')+ggtitle("Life Expectancy in Countries")

ggplot(data = China, aes(x=year,y=gdpPercap))+geom_line()+scale_y_log10(breaks=c(1000,2000,3000,4000,5000),labels=scales::dollar)+xlim(1940,2010)+theme_gray(base_size = 20)
