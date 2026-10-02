# ======================================================================
# Primeiros passos no R
# R Lab - Laboratório de Ecologia e Dendrologia Tropical (LEDT/UFAM)
# https://ledtufam.github.io/extensao/rlab/rotinas/primeiros-passos.html
# ======================================================================


# ---- 1. Instale o R e o RStudio ----

# ---- 2. Trabalhe com projetos ----

# ---- 3. O básico da linguagem ----
# Comentários começam com #
x <- c(12.5, 30.2, 18.7, 45.0)   # vetor com DAP (cm) de 4 árvores
mean(x)                          # média
sd(x)                            # desvio padrão

# Área basal (m²) de cada árvore a partir do DAP em cm
g <- pi * x^2 / 40000
round(g, 4)


# ---- 4. Instale e carregue pacotes ----
install.packages(c("readxl", "ggplot2", "dplyr"))

library(readxl)
library(ggplot2)


# ---- 5. Importe uma planilha ----
# CSV com ; e vírgula decimal (padrão do Excel em português)
dados <- read.csv2("dados/inventario.csv")

# Ou o arquivo do Excel
dados <- readxl::read_excel("dados/inventario.xlsx", sheet = 1)

head(dados)     # primeiras linhas
str(dados)      # tipo de cada coluna
summary(dados)  # resumo


# ---- 6. Seu primeiro gráfico ----
set.seed(42)
dap <- rlnorm(200, meanlog = log(20), sdlog = 0.5)
hist(dap, breaks = 20, col = "#78c2ad", border = "white",
     main = "", xlab = "DAP (cm)", ylab = "Número de árvores")


# ---- Para continuar estudando ----
