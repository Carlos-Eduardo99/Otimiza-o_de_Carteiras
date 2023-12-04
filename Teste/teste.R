#-----------------------------------------------------------------------------#
#                        Execução do Conjunto de Teste                        #
#                    Otimização de Carteira de Criptomoedas                   #
#                                                                             #
# Carlos Eduardo de Sousa                                                     #
# carloscadu1811999@gmail.com                                                 #
# github.com/Carlos-Eduardo99                                                 #
# V1.0  em 04/10/2023                                                         #
#-----------------------------------------------------------------------------#

library("ggplot2")
library("pheatmap")
library("dplyr")

# Extração de dados ------------------------------------------------------------

# Setando o repositorio
setwd("/home/carlos/Área de Trabalho/Faculdade/Scripts")
data <- read.csv("dataset/experimento3/teste.csv")

# Especificando os ativos que irão compor a carteira
criptomoedas <- colnames(data)[-1]

# Número de criptomoedas na carteira
n <- length(criptomoedas)

# Extraindo os preços de fechamento de cada ativo
carteira <- data[, criptomoedas]

carteira <- as.matrix(carteira)

#-------------------------------------------------------------------------------

# Processamento ----------------------------------------------------------------

retornos <- diff(carteira) / head(carteira, -1) #Calculo do retorno percentual simples [r[i]]
media_retornos <- colMeans(retornos) # Cálculo das médias dos retornos E[R]

resultado_multiplicacao <- pesos_carteiras * media_retornos

# Soma as colunas para obter o retorno de cada carteira
retorno_carteira <- rowSums(resultado_multiplicacao)

# Crie um dataframe com os retornos
dados_grafico <- data.frame(Carteira = 1:length(retorno_carteira), Retorno = retorno_carteira)

# Ordene o dataframe em ordem decrescente de Retorno
dados_grafico <- dados_grafico[order(-dados_grafico$Retorno), ]

# Selecione as 100 primeiras linhas
top_1000 <- head(dados_grafico, 1000)

classificacao <- seq(1, 1000)

# Adicione a classificação como rótulos do eixo X
top_1000$Classificacao <- classificacao

print(ggplot(top_1000, aes(x = Classificacao, y = Retorno, fill = ifelse(Carteira == indice_max_sharpe, "Maior Sharpe", "Outras Carteiras"))) +
        geom_bar(stat = "identity") +
        labs(x = "Posição", y = "Retorno(%)", title = "Experimento 3 - Retornos das Carteiras Otimizadas") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_fill_manual(values = c("Maior Sharpe" = "red", "Outras Carteiras" = "#eeeeee")) +
        guides(fill = guide_legend(title = "Legenda")))


# Selecione as 100 primeiras linhas
top_100 <- head(dados_grafico, 100)

classificacao <- seq(1, 100)

# Adicione a classificação como rótulos do eixo X
top_100$Classificacao <- classificacao

print(ggplot(top_100, aes(x = Classificacao, y = Retorno, fill = ifelse(Carteira == indice_max_sharpe, "Maior Sharpe", "Outras Carteiras"))) +
        geom_bar(stat = "identity") +
        labs(x = "Posição", y = "Retorno(%)", title = "Experimento 3 - Retornos das 100 Melhores Carteiras") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_fill_manual(values = c("Maior Sharpe" = "red", "Outras Carteiras" = "#eeeeee")) +
        guides(fill = guide_legend(title = "Legenda"))
)