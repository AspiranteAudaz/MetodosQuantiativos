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
    print(summary(modelo2var))
    fitline <- predict(modelo2var)
    lin0 = c(min(xval), max(xval))
    lin1 = c(min(fitline), max(fitline));
    plot(x=xval, y=yval, ylab=ylab, xlab=xlab);
    lines(x = lin0, y = lin1, col = "red");
}

logap = function(x){log(x, 10)}

# Valores de teste, pra garantir que ta safe a funcao 
#vals = 1:10
#obs  = c(3, 4, 7, 6, 9, 10, 14, 7, 15, 19)
#applylmgraph(obs, vals)

applylmgraph(t(as.data.frame(lapply(baseMod$units_sold, logap))), t(as.data.frame(lapply(baseMod$price, logap))), xlab="units_sold", ylab="price")
#applylmgraph(baseMod$price, baseMod$units_sold)
#applylmgraph(baseMod$retail_price, baseMod$rating)

# Regressao linear simples em todas as combinaçoes
for(i in (1: ncol(base)))
{
    for(j in (1: ncol(base)))
        if(i != j)
            applylmgraph(base[,i], base[,j], xlab=colnames(base)[i], ylab=colnames(base)[j])
}
# Conclusao: muito outlier, filtrar


modelo2var <- lm(base$units_sold ~ base$price + base$rating)
summary(modelo2var)
