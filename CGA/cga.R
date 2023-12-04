#-----------------------------------------------------------------------------#
#                        Compact Genetic Algorithm                            #
#                    Otimização de Carteira de Criptomoedas                   #
#                                                                             #
# Carlos Eduardo de Sousa                                                     #
# carloscadu1811999@gmail.com                                                 #
# github.com/Carlos-Eduardo99                                                 #
# V1.0  em 20/09/2023                                                         #
#-----------------------------------------------------------------------------#

library("ggplot2")
library("pheatmap")

#rm(list = ls())
#cat("\014")  # clear console
# Extração de dados ------------------------------------------------------------

# Setando o repositorio
setwd("/home/carlos/Área de Trabalho/Faculdade/Scripts")
data <- read.csv("dataset/experimento1/treino.csv")
 
# Especificando os ativos que irão compor a carteira
criptomoedas <- colnames(data)[-1]
 
# Número de criptomoedas na carteira
n <- length(criptomoedas)
 
#Extraindo os preços de fechamento de cada ativo
carteira <- data[, criptomoedas]
 
carteira <- as.matrix(carteira)
 
#-------------------------------------------------------------------------------
 
# Processamento ----------------------------------------------------------------
 
retornos <- diff(carteira) / head(carteira, -1) #Calculo do retorno percentual simples [r[i]]
cov_matrix <- cov(retornos) # Cálculo da matriz de covariância
media_retornos <- colMeans(retornos) # Cálculo das médias dos retornos E[R]
 
# Obtendo a carteira ótima -----------------------------------------------------
 
# Vetor de pesos
get_pesos <- function(best){
   
  # Contagem de "uns" do cromossomo
  num_uns <- sum(best)
   
  # Inicialização do vetor com pesos igual a 0
  vetor_pesos <- rep(0, length(best))
   
  # Verifica se há ao menos um 1
  if (num_uns > 0) {
    peso_uns <- 1 / num_uns # Divide os pesos igualmente
    vetor_pesos[best == 1] <- peso_uns # Atribui o peso nos elementos iguais a 1
  }
   
  return(vetor_pesos)
}

# Retorna o risco e o retorno da carteira
get_retorno_risco <- function(U, cov_matrix, media_retornos) {
  retorno_carteira <- sum(media_retornos * U)
  risco_carteira <- sqrt(t(U) %*% cov_matrix %*% U)
  
  return(list(retorno = retorno_carteira, risco = risco_carteira))
}
 
#Rerotna a fitness do indivíduo
#Índice Sharpe
fitness <- function(c){

  risk_free <- 0
  retorno <- c$retorno
  risco <- c$risco

  f = ((retorno - risk_free)/risco)

  return(f)
}

#-------------------------------------------------------------------------------
 
# Funções do CGA ---------------------------------------------------------------

generate <- function(probvector,n_ativos) {
  
  candidate <- rep(0, length(probvector))
  
  while (sum(candidate)<n_ativos){ #Restrição de cardinalidade
    i <- 1
    for (p in probvector) {
      if (runif(1) < p)
        candidate[i] <- 1
      else
        candidate[i] <- 0
      
      i <- i + 1
    }
 }
  
  return(candidate)
}
 
tournament <- function(cromo1, cromo2) {
  fit_cromo1 <- fitness(get_retorno_risco(get_pesos(cromo1),cov_matrix,media_retornos))
  fit_cromo2 <- fitness(get_retorno_risco(get_pesos(cromo2),cov_matrix,media_retornos))
  
  winner <- NA
  loser <- NA
  
  if (!is.na(fit_cromo1) && !is.na(fit_cromo2)) {
    if (fit_cromo1 > fit_cromo2) {
      winner <- list(cromo1, fit_cromo1)
      loser <- list(cromo2, fit_cromo2)
    } else {
      winner <- list(cromo2, fit_cromo2)
      loser <- list(cromo1, fit_cromo1)
    }
  } else {
    print("Valor ausente")
    print(fit_cromo1)
    print(get_pesos(cromo1))
    print(fit_cromo2)
    print(get_pesos(cromo2))
  }
  
  return (list(winner, loser))
}
 
update <- function(probvector, winner, loser, npop) {
  for (i in 1:length(probvector)) {
    if ((winner[[1]][i] != loser[[1]][i])){
      if (winner[[1]][i] == 1) {
        probvector[i] <- probvector[i] + (1/npop)
      } else {
        probvector[i] <- probvector[i] - (1/npop)
      }
    }
  }
  
  return(round(probvector,2))
}
 
convergence <- function(probvector) {
  conv <- TRUE
  for (p in probvector) {
    if (p > 0 & p < 1) {
      conv <- FALSE
      break
    }
  }
  
  return(conv)
}
 
#-----------------------------------------------------------------------------#
#                               ALGORITMO                                     #
#                                                                             #
#                         Carlos Eduardo de Sousa                             #
#-----------------------------------------------------------------------------#
 
num_carteiras <- 1000
maxgen <- 3000
npop <- 100
 
# Restrição na quantidade de ativos na carteira (restrição de cardinalidade)
n_ativos <- 2
 
contador <- 0
carteiras_otimizadas <- data.frame(Iteracao = integer(),
                                  Retorno = numeric(),
                                  Risco = numeric(),
                                  Fitness = numeric())
 
pesos_carteiras <- data.frame(matrix(nrow = num_carteiras, ncol = n))
 
melhor_fitness <- 0
 
while (contador < num_carteiras) {
       
      # Inicialização do vetor de probabilidade
      probvector <- rep(0.5, n)
       
      # gera um indivíduo inicial
      best <- generate(probvector, n_ativos)
       
      pesos <- get_pesos(best)
       
      # Obtém o vetor convertido
      c <- get_retorno_risco(pesos,cov_matrix,media_retornos)
       
      fit_best <- fitness(c)
      best <- list(best, fit_best)
       
      # armazena a melhor fitness por iteração
      bestfitness <- rep(NA, maxgen)
      matprob <- probvector
       
      for (iter in 1:maxgen) {
         
        cromo1 <- generate(probvector,n_ativos)
        cromo2 <- generate(probvector,n_ativos)
         
         
        result <- tournament(cromo1, cromo2)
        winner <- result[[1]]
        loser <- result[[2]]
         
        
        if (winner[[2]] < best[[2]]) {
           best <- winner
        }
        bestfitness[iter] <- best[[2]]
         
        #atualiza o vetor de probabilidade
        probvector <- update(probvector, winner, loser, npop)
        matprob <- rbind(matprob, probvector)
         
        #verifica se o vetor de probabilidade convergiu
        if (convergence(probvector) == TRUE)
           break
        }
       
        f <- best[[2]]
        carteira <- get_retorno_risco(get_pesos(best[[1]]),cov_matrix, media_retornos)
       
        # Atualize as variáveis de melhor resultado se o resultado atual for melhor
       if (f > melhor_fitness) {
         iteracao <- iter
         melhor_fitness <- f
         melhor_retorno <- carteira$retorno
         melhor_risco <- carteira$risco
         melhor_pesos <- get_pesos(best[[1]])
         Vetor_B <- best[[1]]
         Vetor_P <- probvector
         melhor_bestfitness <- bestfitness
         melhor_matprob <- matprob
       }
 
       carteiras_otimizadas <- rbind(carteiras_otimizadas, 
                                     data.frame (Iteracao = iter,
                                          Retorno = carteira$retorno,
                                          Risco = carteira$risco,
                                          Fitness = best[[2]]
                               ))
       
       
       
       # Adicione os vetores de pesos como novas colunas no dataframe pesos_carteiras
       pesos_carteiras[contador + 1, ] <- get_pesos(best[[1]])
       
       contador <- contador + 1
}
 
# ------------------------------------------------------------------------------
 
print("CARTEURA COM OS MELHORES PESOS:")
print(melhor_pesos)
print("MELHOR PESOS (BINARIO):")
print(Vetor_B)
print("FIT DA MELHOR SOLUÇÃO:")
print(melhor_fitness)
print("VETOR DE PROBABILIDADE:")
print(Vetor_P)
 
print("RETORNO DA CARTEIRA: ")
print(melhor_retorno)
print("RISCO DA CARTEIRA: ")
print(melhor_risco)

# Plotagens --------------------------------------------------------------------

plotGraphs <- TRUE

if (plotGraphs == TRUE) {
  
  # Gráfico de convergência
  plot(melhor_bestfitness, type = "l", xlab = "Iterações",
       ylab = "Melhor fitness",
       main = "Convergência Evolutiva", panel.first = grid(), lwd = 2,
       xlim = c(0, iteracao), col = "black")
  abline(h = 0, col = "gray", lty = 2)

  # Gráfico de calor
  rownames(melhor_matprob) <- as.character(seq(1:dim(melhor_matprob)[1]))
  pheatmap(melhor_matprob, cluster_rows = FALSE, cluster_cols = FALSE, scale = "none",
  color = colorRampPalette(c("white", "orange", "red"))(100),
  border_color = "grey60", breaks = seq(0,1,0.01), legend_breaks = seq(0,1,0.1),
  show_colnames = FALSE, show_rownames = FALSE, main = "Experimento 1")
  
  
  # Índice da carteira com o maior índice Sharpe
  indice_max_sharpe <- which.max(carteiras_otimizadas$Fitness)
  X_axis <- carteiras_otimizadas$Risco[indice_max_sharpe]
  y_axis <- carteiras_otimizadas$Retorno[indice_max_sharpe]*100
  
  # Fronteira Eficiente
  print(ggplot(data = carteiras_otimizadas, aes(x = Risco, y = Retorno*100, color = Fitness)) +
    geom_point() +
    scale_color_gradient(low = "blue", high = "red") +
    geom_point(data = data.frame(Risco = X_axis, Retorno = y_axis), 
               aes(x = Risco, y = Retorno), color = "orange", size = 4) +
    labs(x = "Risco", y = "Retorno Esperado (%)", color = "Índice Sharpe",
         title="Experimento 1") +
    theme_minimal())
  
}