# Adicionando o diretorio atual
setwd("./")

# Importando dados
data <- read.csv(
  file="./summer-products-with-rating-and-performance_2020-08.csv",
  header=TRUE,
  sep=","
)

# Construção do DataFrame
df <- data.frame(data)
View(df)

# Seleção das variáveis para estudo
df <- df[c(
  "price",
  "retail_price",
  "units_sold", 
  "rating",
  "countries_shipped_to"
)]

# traduzindo colunas
trad = c(
  "Preco_do_Produto",
  "Preco_de_Varejo",
  "Unidades_Vendidas",
  "Media_da_Nota_do_Produto",
  "Numero_de_Paises_Para_Envio"
)
# colocando nomes traduzidos nas colunas
for(i in 1:dim(df)[2]){
  colnames(df)[i] <- trad[i]
}

# nova variavel
df$desconto = df$Preco_de_Varejo - df$Preco_do_Produto
colnames(df)[dim(df)[2]] = "desconto"

# Descrição das notas para decidir categorização
boxplot(
  df$Media_da_Nota_do_Produto,
  main = "Distribuição de Médias de Notas dos Produtos",
  col = "cadetblue4",
  pch = 16
)
summary(df$Media_da_Nota_do_Produto)

# Categorização das avaliações
df$avaliacao = ifelse(df$Media_da_Nota_do_Produto >= 3.85, "DESTAQUE", "COMUM")
df$avaliacao = as.factor(df$avaliacao)
df$avaliacao_bi = ifelse(df$avaliacao == "DESTAQUE", 1, 0)
View(df)

# Regressão Logística
model1<- glm(
  avaliacao ~ desconto + Unidades_Vendidas ,
  data=df,
  family = binomial
)
summary(model1)


# tetativa de predição
prev.df = predict(
  model1,
  df,
  type='response'
)
df$Predicao = prev.df

# visualização
plot(prev.df, df$avaliacao_bi)

