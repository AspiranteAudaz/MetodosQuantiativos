#install.packages("dplyr")
#install.packages("ggplot2") 
library("dplyr")
library("ggplot2")  

# Adicionando o diretorio atual
setwd("../");

# Le o dataset com as colunas desejadas
dataset <- read.csv(file="./summer-products-with-rating-and-performance_2020-08.csv", header=TRUE, sep=",")
dataset <- as.data.frame(dataset)
base    <- select(dataset, price, units_sold, rating, rating_count, retail_price,  merchant_rating, merchant_rating_count)

# Nomes alternativos
colnames(base) <- c("Price", "Units Sold", "Rating", "Rating C", "Retail Price", "Merch R", "Merch C")

## Seçao de outliers
# Quartis
Q1 = 0.25
Q2 = 0.5
Q3 = 0.75
Q4 = 1.0

# Calcula Inter Quartile Range (IQR) da lista
iqrange <- function(arr)
{
    quantile(arr, Q3) - quantile(arr, Q1)
}

# Limpa outliers se baseando em uma coluna
limpaOutlier <- function(df, col)
{
    IQR = iqrange(unlist(df[[col]]))
    lower = Q1 - 1.5*IQR
    upper = Q3 + 1.5*IQR
    subset(df, df[[col]] > lower & df[[col]] < upper)
}

# Mapa de calor de correlacao
cor_map = data.frame(rows = rep(colnames(base), each = ncol(base)), cols = rep(colnames(base), each = 1, times=ncol(base)), Corr = c(cor(base)), stringsAsFactors=FALSE)

cor_heat = ggplot(cor_map, aes(rows, cols)) + geom_tile(aes(fill = Corr)) 
cor_heat = cor_heat + scale_fill_gradient(low = "#0000FF", high = "#EE6600") + theme(axis.title.y=element_blank(), axis.title.x=element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=16, face="bold"), axis.text.y = element_text(vjust = 0.5, hjust=1, size=16, face="bold"))
cor_heat

# Eliminares os valores com correlacao quase perfeita e muita baixa
cor_map = subset(cor_map, (abs(Corr) < 0.99 & abs(Corr) > 0.2))

# Calcula novo cor_map aplicando remocao de outliers 2 a 2

cor_map$Corr_New <- NA
rows = cor_map$"rows"
cols = cor_map$"cols"

for(i in 1:(nrow(cor_map)))
{
    sub_base = limpaOutlier(base, rows[[i]])
    sub_base = limpaOutlier(sub_base, cols[[i]])
    sub_base = select(sub_base, rows[i], cols[[i]])
    cor_map[i, "Corr_New"] = cor(sub_base)[2]
}
cor_map_new <- subset(cor_map, abs(cor_map$Corr_New) > abs(cor_map$Corr))
cor_map_new

# Regressao Linear
lmGraph <- function(df, fit, xcol, ycol, xcol_name="x", ycol_name="y")
{
    lmplot = ggplot(df, aes(x=xcol, y=ycol)) + geom_point()
    lmplot = lmplot + theme(axis.title.y=element_text(size=16, face="bold"), axis.title.x=element_text(size=16, face="bold"))
    lmplot = lmplot + labs(title="Regressão Linear Simples",x=xcol_name, y=ycol_name)
    lmplot = lmplot + theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size=16, face="bold"))
    lmplot = lmplot + theme(axis.text.y = element_text(vjust = 0.5, hjust=1, size=16, face="bold"))
    lmplot = lmplot + geom_smooth(method=lm, se=FALSE, col="red")
    print(lmplot)
    res <- summary(lm(xcol~ycol))
    res
}

##Algumas analises simples com o novo cor_map
xc = "Price"
yc = "Retail Price"

# Normal
lmGraph(base, NULL, base[[xc]], base[[yc]], xcol_name=xc, ycol_name=yc)

# Sem outlier
sub_base = limpaOutlier(base, xc)
sub_base = limpaOutlier(sub_base, yc)
lmGraph(sub_base, NULL, sub_base[[xc]], sub_base[[yc]], xcol_name=xc, ycol_name=yc)

xc = "Rating C"
yc = "Merch C"

# Normal
lmGraph(base, NULL, base[[xc]], base[[yc]], xcol_name=xc, ycol_name=yc)

# Sem outlier
sub_base = limpaOutlier(base, xc)
sub_base = limpaOutlier(sub_base, yc)
lmGraph(sub_base, NULL, sub_base[[xc]], sub_base[[yc]], xcol_name=xc, ycol_name=yc)
