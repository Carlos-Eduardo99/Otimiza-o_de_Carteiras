#-----------------------------------------------------------------------------#
#                       Análise Exploratória de Dados                         #
#                    Otimização de Carteira de Criptomoedas                   #
#                                                                             #
# Carlos Eduardo de Sousa                                                     #
# carloscadu1811999@gmail.com                                                 #
# github.com/Carlos-Eduardo99                                                 #
# V1.0  em 17/10/2023                                                         #
#-----------------------------------------------------------------------------#

# Ambiente----------------------------------------------------------------------
library(ggplot2)
library(GGally)
library(dplyr)
library(zoo)
library(tidyr)

# IMPORTANTE!!
# É necessário carregar o arquivo .RData do experimento 1 (data1.RData)
# A análise nesse script foi feita levando em consideração o experimento 1

# Análise-----------------------------------------------------------------------
# Lendo o arquivo CSV
setwd("/home/carlos/Área de Trabalho/Faculdade/Scripts")
data <- read.csv("dataset/experimento1/treino.csv") 

# Especificando os ativos
moedas <- colnames(data)[-1]

# Extrai os preços de fechamento de cada ativo
precos <- data[, moedas]

# Converte a coluna 'Data' para o formato de data
data$Data <- as.Date(data$Data)

# Divide em 3 grupos com preços semelhantes
data1 <- subset(data, select = c(ETH, BCH, BSV, LTC, BNB, XMR, Data))
data2 <- subset(data, select = c(XRP, USDT, EOS, XLM, TRX, ADA, XTZ, LEO, Data))
data3 <- subset(data, select = c(BTC, Data))

data_long1 <- gather(data1, key = "Criptomoeda", value = "Preço", -Data)
data_long2 <- gather(data2, key = "Criptomoeda", value = "Preço", -Data)
data_long3 <- gather(data3, key = "Criptomoeda", value = "Preço", -Data)

# Gráfico de série temporal ----------------------------------------------------
print(ggplot(data = data_long1, aes(x = Data, y = `Preço`, color = Criptomoeda)) +
  geom_line() +
  labs(
       x = "Período",
       y = "Preço (BRL)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma))

print(ggplot(data = data_long2, aes(x = Data, y = `Preço`, color = Criptomoeda)) +
        geom_line() +
        labs(
          x = "Período",
          y = "Preço (BRL)") +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma))

print(ggplot(data = data_long3, aes(x = Data, y = `Preço`, color = Criptomoeda)) +
        geom_line() +
        labs(
          x = "Período",
          y = "Preço (BRL)") +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma))

# Desvio-padrão e Média dos retornos -------------------------------------------

# Matriz de correlação
print(ggcorr(precos, label=T, method = c("pairwise", "pearson")))

# Extrai as variâncias (diagonal principal da matriz de covariância)
variancias <- diag(cov_matrix)

# Criar um data frame
variancias_df <- data.frame(Ativo = colnames(cov_matrix), Variancia = 
                              variancias, Media_Retornos = media_retornos)

cores <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", 
           "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", "#aec7e8", "#ffbb78", 
           "#98df8a", "#ff9896", "#c5b0d5")


# Gráfico do retorno médio e desvio-padrão
print(ggplot(data = variancias_df, aes(x = Ativo)) +
        geom_point(aes(y = Media_Retornos, color = Ativo), size = 3) +
        geom_errorbar(aes(ymin = Media_Retornos - sqrt(Variancia), 
                          ymax = Media_Retornos + sqrt(Variancia), 
                          color = Ativo), width = 0.2) +
        geom_text(aes(y = Media_Retornos, label = sprintf("%.4f", Media_Retornos)), 
                  nudge_x = 0.35, nudge_y = -0.009, size = 1.95) +
        labs(x = "Criptomoedas",
             y = "Retorno médio") +
        theme_minimal() +
        scale_color_manual(values = cores) +
        guides(color = 'none')
)

# retorno percentual diário ----------------------------------------------------

pdf("graficos.pdf", width = 11, height = 7)

carteira <- data[1:367, moedas]
carteira <- as.matrix(carteira)

retorno_percentual <- diff(carteira) / head(carteira, -1)

retorno_BSV <- retorno_percentual[, "BSV"]
retorno_USDT <- retorno_percentual[, "USDT"]
retorno_LEO <- retorno_percentual[, "LEO"]
retorno_XLM <- retorno_percentual[, "XLM"]

datas <- seq(as.Date("2020-01-01"), by = "days", length.out = length(retorno_BSV))

# Crie o gráfico para BSV
par(mfrow = c(2, 2))  # Define a disposição dos gráficos
plot(datas, retorno_BSV, type = "l", col = "#d62728", xlab = "Data", ylab = "Retorno diário", main = "BSV")

plot(datas, retorno_XLM, type = "l", col = "#ffbb78", xlab = "Data", ylab = "Retorno diário", main = "XLM")

plot(datas, retorno_LEO, type = "l", col = "#7f7f7f", xlab = "Data", ylab = "Retorno diário", main = "LEO")

plot(datas, retorno_USDT, type = "l", col = "#aec7e8", xlab = "Data", ylab = "Retorno diário", main = "USDT")

# Salvar os gráficos no arquivo PDF
dev.off()