# ======================================================================
# Padrão espacial: índices de agregação e distribuições de probabilidade
# R Lab - Laboratório de Ecologia e Dendrologia Tropical (LEDT/UFAM)
# https://ledtufam.github.io/extensao/rlab/rotinas/indices-agregacao.html
# ======================================================================


# ---- Roteiro da aula prática ----

# ---- 0. Antes de começar ----

# ---- 1. Dados: os caixetais do Vale do Ribeira ----
cx <- read.csv("dados/caixeta.csv")   # no seu projeto: read.csv("dados/caixeta.csv")
str(cx)
head(cx)

table(cx$local)                       # quantos fustes em cada caixetal?
length(unique(cx$especie))            # quantos nomes de espécies?
sort(table(cx$especie), decreasing = TRUE)[1:5]   # as 5 espécies com mais fustes


# ---- Um fuste não é uma árvore ----
# corrige uma grafia duplicada (veja a rotina de nomes científicos!)
cx$especie[cx$especie == "Callophyllum brasiliensis"] <- "Calophyllum brasiliensis"

arv <- unique(cx[, c("local", "parcela", "arvore", "especie")])
arv$parc <- paste(arv$local, arv$parcela, sep = "_")

c(fustes = nrow(cx), arvores = nrow(arv), parcelas = length(unique(arv$parc)))


# ---- Tabela espécie × parcela ----
tab <- unclass(table(arv$especie, arv$parc))
UT  <- ncol(tab)                       # 15 parcelas
tab <- tab[order(rowSums(tab), decreasing = TRUE), ]
tab[1:6, ]


# ---- 2. Passo a passo com uma espécie ----

# ---- Os dados da espécie ----
n_ij <- tab["Myrcia sulfiflora", ]   # nº de árvores da espécie em cada parcela
n_ij


# ---- Quantidades básicas ----
n_i <- sum(n_ij)
U_i <- sum(n_ij > 0)
M_i <- mean(n_ij)
S2  <- var(n_ij)          # o R já usa (UT - 1) no denominador
c(UT = UT, n_i = n_i, U_i = U_i, M_i = M_i, S2 = S2)


# ---- Índice de MacGuinnes ($IGA_i$) ----
D_i <- n_i / UT
d_i <- -log(1 - U_i / UT)      # log() no R é o logaritmo natural (ln)
IGA <- D_i / d_i
c(D_i = D_i, d_i = d_i, IGA = IGA)


# ---- Índice de Fracker e Brischle ($K_i$) ----
K <- (D_i - d_i) / d_i^2
K


# ---- Índice de Payandeh ($P_i$) ----
P <- S2 / M_i
P


# ---- Índice não aleatorizado ($R_i$) e índice de Hazen ($IH_i$) ----
IH <- P * (UT - 1)
IH

gl <- UT - 1
c(chi2_75 = qchisq(0.75, gl), chi2_95 = qchisq(0.95, gl), chi2_99 = qchisq(0.99, gl))


# ---- Conferindo na tabela impressa ----
IH > qchisq(0.95, gl)      # não aleatorizado: agregada?
IH > qchisq(0.99, gl)      # Hazen: agrupada?


# ---- Índice de Morisita ($IM_i$) e teste F ----
IM  <- UT * sum(n_ij * (n_ij - 1)) / (n_i * (n_i - 1))
F_i <- (IM * (n_i - 1) + UT - n_i) / (UT - 1)
F_tab <- qf(0.95, df1 = UT - 1, df2 = Inf)     # F tabelado: UT-1 e infinito gl
c(IM = IM, F = F_i, F_tabelado = F_tab)


# ---- Juntando tudo ----
data.frame(
  indice = c("MacGuinnes", "Fracker e Brischle", "Payandeh", "Não aleatorizado (χ²)", "Hazen", "Morisita"),
  valor  = round(c(IGA, K, P, IH, IH, IM), 2)
)


# ---- Sua vez! ----

# ---- Exercício ----

# ---- Resposta ----
n_ij <- tab["Calophyllum brasiliensis", ]
n_i <- sum(n_ij); U_i <- sum(n_ij > 0); M_i <- mean(n_ij); S2 <- var(n_ij)
D_i <- n_i / UT; d_i <- -log(1 - U_i / UT)
IGA <- D_i / d_i
K   <- (D_i - d_i) / d_i^2
P   <- S2 / M_i
IH  <- P * (UT - 1)
IM  <- UT * sum(n_ij * (n_ij - 1)) / (n_i * (n_i - 1))
F_i <- (IM * (n_i - 1) + UT - n_i) / (UT - 1)
round(c(n_i = n_i, U_i = U_i, IGA = IGA, K = K, P = P, IH = IH, IM = IM, F = F_i), 2)


# ---- 3. Resumo das fórmulas ----

# ---- 4. Uma função para calcular todas as espécies ----
indices_agregacao <- function(tab, UT = ncol(tab)) {
  n_i <- rowSums(tab)                       # total de árvores
  U_i <- rowSums(tab > 0)                   # parcelas com a espécie
  M_i <- n_i / UT                           # média por parcela
  S2  <- apply(tab, 1, function(x) sum((x - mean(x))^2) / (UT - 1))  # variância

  # MacGuinnes e Fracker & Brischle (indefinidos se a espécie ocorre em todas as parcelas)
  D_i <- n_i / UT
  d_i <- ifelse(U_i < UT, -log(1 - U_i / UT), NA)
  IGA <- D_i / d_i
  K   <- (D_i - d_i) / d_i^2

  # Payandeh / não aleatorizado / Hazen
  P   <- S2 / M_i
  qui <- P * (UT - 1)                       # = IH (índice de Hazen)

  # Morisita e teste F
  IM  <- UT * rowSums(tab * (tab - 1)) / (n_i * (n_i - 1))
  F_i <- (IM * (n_i - 1) + UT - n_i) / (UT - 1)

  # Valores críticos
  chi95 <- qchisq(0.95, UT - 1)
  chi75 <- qchisq(0.75, UT - 1)
  chi99 <- qchisq(0.99, UT - 1)
  F95   <- qf(0.95, UT - 1, Inf)

  classe <- function(x, cortes, rotulos) as.character(cut(x, cortes, rotulos, right = TRUE))

  data.frame(
    n_i, U_i, D_i = round(D_i, 2), d_i = round(d_i, 2),
    S2 = round(S2, 2), M_i = round(M_i, 2),
    IGA = round(IGA, 2), K = round(K, 2), P = round(P, 2),
    IH = round(qui, 2), IM = round(IM, 2), F = round(F_i, 2),
    IGA_cl = ifelse(is.na(IGA), "-", ifelse(abs(IGA - 1) < 1e-9, "Aleat.",
             classe(IGA, c(-Inf, 1, 2, Inf), c("Unif.", "T.Agr.", "Agreg.")))),
    K_cl   = ifelse(is.na(K), "-", classe(K, c(-Inf, 0.15, 1, Inf), c("Aleat.", "T.Agr.", "Agreg."))),
    P_cl   = classe(P, c(-Inf, 1, 1.5, Inf), c("Aleat.", "T.Agr.", "Agreg.")),
    R_cl   = ifelse(qui > chi95, "Agreg.", "Aleat."),
    IH_cl  = ifelse(qui < chi75, "Não agr.", ifelse(qui > chi99, "Agreg.", "T.Agr.")),
    IM_cl  = ifelse(F_i > F95, "Agreg.", ifelse(IM < 1, "Unif.", "Aleat.")),
    check.names = FALSE
  )
}


# ---- 5. Resultados ----
res <- indices_agregacao(tab[rowSums(tab) >= 5, ], UT = UT)
knitr::kable(res[, 1:12], caption = "Estimativas dos índices por espécie (15 parcelas de 10 × 20 m).")

knitr::kable(res[, c("IGA_cl", "K_cl", "P_cl", "R_cl", "IH_cl", "IM_cl")],
             col.names = c("MacGuinnes", "Fracker-Brischle", "Payandeh",
                           "Não aleat.", "Hazen", "Morisita"),
             caption = "Classificação do padrão espacial (Agreg. = agregado; T.Agr. = tendência ao agrupamento; Aleat. = aleatório; Unif. = uniforme).")


# ---- A escala importa: a caixeta dentro de cada local ----
caixeta_local <- t(sapply(split(arv, arv$local), function(d) {
  parcelas <- unique(d$parc)                               # 5 parcelas do local
  n <- sapply(parcelas, function(p) sum(d$parc == p & d$especie == "Tabebuia cassinoides"))
  r <- indices_agregacao(matrix(n, nrow = 1), UT = length(parcelas))
  unlist(r[, c("n_i", "M_i", "S2", "P", "IM", "P_cl", "R_cl", "IM_cl")])
}))
knitr::kable(caixeta_local, caption = "Índices para *Tabebuia cassinoides* em cada caixetal (5 parcelas por local).")


# ---- Para pensar ----

# ---- 6. Exercícios ----

# ---- Para entregar ----

# ---- 7. Para saber mais: conferindo com o livro ----
livro <- data.frame(
  especie = c("Anadenanthera peregrina", "Cecropia hololeuca", "Ocotea pretiosa"),
  n_i = c(76, 9, 41), U_i = c(8, 6, 9), S2 = c(53.60, 0.77, 10.54)
)
UT_livro <- 10
D <- livro$n_i / UT_livro
d <- -log(1 - livro$U_i / UT_livro)
data.frame(livro["especie"],
           IGA = round(D / d, 2),
           K   = round((D - d) / d^2, 2),
           P   = round(livro$S2 / D, 2))


# ---- 8. Distribuições de probabilidade ----
n_ind     <- 0:4                        # nº de indivíduos por parcela
n_parcelas <- c(73, 18, 3, 4, 2)        # nº de parcelas (Tabela 1.5)
N  <- sum(n_parcelas)                   # 100 parcelas
mu <- sum(n_ind * n_parcelas) / N       # média = 0,44 indivíduo/parcela
mu


# ---- Poisson ----
p_pois <- dpois(n_ind, lambda = mu)
p_pois <- c(p_pois, 1 - sum(p_pois))    # inclui a classe "> 4"
esp_pois <- N * p_pois
data.frame(x = c(n_ind, ">4"), P = round(p_pois, 4), Esperado = round(esp_pois, 2))


# ---- Binomial negativa ----
S2 <- (sum(n_ind^2 * n_parcelas) - sum(n_ind * n_parcelas)^2 / N) / N   # como no livro (denominador n)
k  <- mu^2 / (S2 - mu)
c(S2 = S2, k = k)

p_bn <- dnbinom(n_ind, size = k, mu = mu)   # mesmo que as fórmulas P(0), P(1)... do livro
p_bn <- c(p_bn, 1 - sum(p_bn))
esp_bn <- N * p_bn
data.frame(x = c(n_ind, ">4"), P = round(p_bn, 4), Esperado = round(esp_bn, 2))


# ---- Teste de aderência (qui-quadrado) ----
obs <- c(n_parcelas, 0)                 # nenhuma parcela com mais de 4 indivíduos

qui_pois <- sum((obs - esp_pois)^2 / esp_pois)
qui_bn   <- sum((obs - esp_bn)^2 / esp_bn)
n_cl     <- length(obs)                 # 6 classes

data.frame(
  Modelo     = c("Poisson", "Binomial negativa"),
  qui2_obs   = round(c(qui_pois, qui_bn), 2),
  gl         = c(n_cl - 2, n_cl - 3),
  qui2_tab   = round(c(qchisq(0.95, n_cl - 2), qchisq(0.95, n_cl - 3)), 3),
  Ajuste     = ifelse(c(qui_pois < qchisq(0.95, n_cl - 2),
                        qui_bn   < qchisq(0.95, n_cl - 3)), "Aceito", "Rejeitado")
)

m <- rbind(Observado = obs, Poisson = esp_pois, `Binomial negativa` = esp_bn)
colnames(m) <- c(n_ind, ">4")
barplot(m, beside = TRUE, col = c("grey60", "#6cc3d5", "#78c2ad"), border = NA,
        xlab = "Número de indivíduos por parcela", ylab = "Número de parcelas",
        legend.text = TRUE, args.legend = list(bty = "n"))


# ---- 9. E se eu tiver as coordenadas das árvores? A função K de Ripley ----

# ---- Os dados: *Beilschmiedia pendula* em Barro Colorado ----
library(spatstat)
data(bei)
bei                      # um "padrão de pontos" (ppp): coordenadas + janela da parcela

plot(bei, pch = 16, cex = 0.3, main = "")


# ---- Ponte com os índices: dividindo a parcela em subparcelas ----
q <- quadratcount(bei, nx = 20, ny = 10)
n_ij <- as.vector(q)             # nº de árvores em cada uma das 200 subparcelas
summary(n_ij)

res_bei <- indices_agregacao(matrix(n_ij, nrow = 1, dimnames = list("B. pendula", NULL)),
                             UT = length(n_ij))
res_bei[, c("n_i", "U_i", "M_i", "S2", "P", "IM", "P_cl", "R_cl", "IM_cl")]

tamanhos <- list("100 × 100 m (1 ha)"      = c(10, 5),
                 "50 × 50 m (0,25 ha)"     = c(20, 10),
                 "20 × 20 m (0,04 ha)"     = c(50, 25),
                 "10 × 10 m (0,01 ha)"     = c(100, 50))

escala <- t(sapply(tamanhos, function(g) {
  n <- as.vector(quadratcount(bei, nx = g[1], ny = g[2]))
  r <- indices_agregacao(matrix(n, nrow = 1), UT = length(n))
  c(subparcelas = length(n), media = round(mean(n), 2), Payandeh = r$P, Morisita = r$IM)
}))
escala


# ---- A função K de Ripley e a função L ----
set.seed(123)
env <- envelope(bei, Lest, nsim = 39, rmax = 100, correction = "border", verbose = FALSE)

plot(env, . - r ~ r, main = "", legend = FALSE,
     xlab = "Distância r (m)", ylab = "L(r) − r", ylim = c(-3, 24))
legend("topright", bty = "n", lty = c(1, 2, NA), pch = c(NA, NA, 15),
       col = c("black", "red", "grey75"), pt.cex = 2,
       legend = c("Observado", "Esperado ao acaso (CSR)", "Envelope (39 simulações)"))


# ---- Para pensar ----

# ---- Referências ----
