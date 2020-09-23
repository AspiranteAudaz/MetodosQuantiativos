#install.packages("dplyr")
library(dplyr)

# Adicionando o diretorio atual
setwd("../");

dataset <- read.csv(file="./summer-products-with-rating-and-performance_2020-08.csv", header=TRUE, sep=",")
dataset <- as.data.frame(dataset)

base <- select(dataset, price, units_sold, rating, rating_count, retail_price)

cor(base)

modelo1var <- lm(base$units_sold ~ base$price)
summary(modelo1var)

anova(modelo1var)

as.numeric(rownames(base$price))
#baseMod = subset(base, base$price<200 & 0 < base$units_sold & base$units_sold < 200)
baseMod = base

applylmgraph = function(xval, yval, xlab="X", ylab="Y")
{
    modelo2var <- lm(yval ~ xval)
    fitline <- predict(modelo2var);
    lin0 = c(min(xval), max(xval))
    lin1 = c(min(fitline), max(fitline));
    print(lin0)
    plot(x=xval, y=yval, ylab=ylab, xlab=xlab);
    lines(x = lin0, y = lin1, col = "red");
}

logap = function(x){log(x, 10)}

# Valores de teste, pra garantir que ta safe a funcao 
#vals = 1:10
#obs  = c(3, 4, 7, 6, 9, 10, 14, 7, 15, 19)
#applylmgraph(obs, vals)

applylmgraph(t(as.data.frame(lapply(baseMod$price, logap))), t(as.data.frame(lapply(baseMod$price, logap))))
#applylmgraph(baseMod$price, baseMod$units_sold)
#applylmgraph(baseMod$retail_price, baseMod$rating)


for(i in (1: ncol(base)))
{
    for(j in (1: ncol(base)))
        applylmgraph(base[,i], base[,j])
}


modelo2var <- lm(base$units_sold ~ base$price)
summary(modelo2var)

fitline <- predict(modelo2var)

lin0 = c(min(base$price), max(base$price))
lin1 = c(min(fitline), max(fitline));
plot(y=base$price, x=base$units_sold, ylab="Qtd de itens vendidos", xlab="Preço");
lines(y = lin0, x = lin1, col = "red");

modelo2var <- lm(base$units_sold ~ base$price + base$rating)
summary(modelo2var)
