# ======================================================================
# Associação entre espécies e similaridade entre áreas
# R Lab - Laboratório de Ecologia e Dendrologia Tropical (LEDT/UFAM)
# https://ledtufam.github.io/extensao/rlab/rotinas/associacao-similaridade.html
# ======================================================================


# ---- Roteiro da aula prática (16/10 · EE5) ----

# ---- 1. Dados ----
cx <- read.csv("dados/caixeta.csv")   # no seu projeto: read.csv("dados/caixeta.csv")
cx$especie[cx$especie == "Callophyllum brasiliensis"] <- "Calophyllum brasiliensis"

arv <- unique(cx[, c("local", "parcela", "arvore", "especie")])   # uma linha por árvore
arv$parc <- paste(arv$local, arv$parcela, sep = "_")

ab <- unclass(table(arv$especie, arv$parc))   # abundância: espécie × parcela
pa <- (ab > 0) * 1                            # presença (1) e ausência (0)
dim(pa)


# ---- 2. Associação entre duas espécies ----
A <- pa["Calophyllum brasiliensis", ]
B <- pa["Psidium sp", ]

a <- sum(A == 1 & B == 1); b <- sum(A == 1 & B == 0)
c <- sum(A == 0 & B == 1); d <- sum(A == 0 & B == 0)
N <- a + b + c + d
c(a = a, b = b, c = c, d = d, N = N)

Ea  <- (a + b) * (a + c) / N
qui <- N * (abs(a * d - b * c) - N / 2)^2 / ((a + b) * (c + d) * (a + c) * (b + d))
c(a_observado = a, a_esperado = round(Ea, 2), qui2 = round(qui, 3),
  qui2_critico = round(qchisq(0.95, 1), 3))


# ---- Quando as frequências esperadas são pequenas ----
tab22 <- matrix(c(a, c, b, d), 2, dimnames = list(Calophyllum = c("pres", "aus"),
                                                  Psidium = c("pres", "aus")))
tab22
chisq.test(tab22)$p.value        # mesma conta da fórmula acima (com Yates)
fisher.test(tab22)$p.value


# ---- Coeficientes de associação ----
c(Jaccard = a / (a + b + c), Ochiai = a / sqrt((a + b) * (a + c)))


# ---- 3. Todos os pares de espécies ----
sp  <- names(which(rowSums(ab) >= 5))
par_sp <- t(combn(sp, 2))

assoc <- data.frame(A = par_sp[, 1], B = par_sp[, 2])
assoc[, c("a", "b", "c", "d", "Jaccard", "sinal", "p_Fisher")] <- NA
for (k in seq_len(nrow(assoc))) {
  x <- pa[assoc$A[k], ]; y <- pa[assoc$B[k], ]
  a <- sum(x & y); b <- sum(x & !y); c <- sum(!x & y); d <- sum(!x & !y)
  Ea <- (a + b) * (a + c) / (a + b + c + d)
  assoc[k, c("a", "b", "c", "d")] <- c(a, b, c, d)
  assoc$Jaccard[k]  <- round(a / (a + b + c), 2)
  assoc$sinal[k]    <- ifelse(a > Ea, "+", ifelse(a < Ea, "−", "0"))
  assoc$p_Fisher[k] <- round(fisher.test(matrix(c(a, c, b, d), 2))$p.value, 4)
}
knitr::kable(assoc[assoc$p_Fisher < 0.05, ], row.names = FALSE,
             caption = "Pares de espécies com associação significativa (Fisher, p < 0,05).")

M <- matrix(NA, length(sp), length(sp), dimnames = list(sp, sp))
for (k in seq_len(nrow(assoc))) {
  v <- ifelse(assoc$sinal[k] == "+", 1, -1) * (1 - assoc$p_Fisher[k])
  M[assoc$A[k], assoc$B[k]] <- M[assoc$B[k], assoc$A[k]] <- v
}
par(mar = c(10, 12, 1, 1))
image(1:length(sp), 1:length(sp), M, col = hcl.colors(20, "Blue-Red 2", rev = TRUE),
      zlim = c(-1, 1), axes = FALSE, xlab = "", ylab = "")
axis(1, 1:length(sp), sp, las = 2, font = 3, cex.axis = 0.8)
axis(2, 1:length(sp), sp, las = 1, font = 3, cex.axis = 0.8)
for (k in seq_len(nrow(assoc))) {
  i <- match(assoc$A[k], sp); j <- match(assoc$B[k], sp)
  rot <- paste0(assoc$Jaccard[k], ifelse(assoc$p_Fisher[k] < 0.05, "*", ""))
  text(i, j, rot, cex = 0.7); text(j, i, rot, cex = 0.7)
}


# ---- 4. Similaridade entre os caixetais ----
ab_local <- unclass(table(arv$local, arv$especie))   # local × espécie
pa_local <- (ab_local > 0) * 1

similaridade <- function(x, y) {
  a <- sum(x > 0 & y > 0); b <- sum(x > 0 & y == 0); c <- sum(x == 0 & y > 0)
  c(Jaccard  = a / (a + b + c),
    Sorensen = 2 * a / (2 * a + b + c),
    BrayCurtis = 2 * sum(pmin(x, y)) / sum(x + y),
    comuns = a)
}

pares <- combn(rownames(ab_local), 2)
sim_local <- t(apply(pares, 2, function(p) similaridade(ab_local[p[1], ], ab_local[p[2], ])))
rownames(sim_local) <- apply(pares, 2, paste, collapse = " × ")
knitr::kable(round(sim_local, 2), caption = "Similaridade entre os caixetais.")


# ---- 5. Similaridade entre parcelas e dendrograma ----
dist_matriz <- function(m, indice) {
  n <- nrow(m); D <- matrix(0, n, n, dimnames = list(rownames(m), rownames(m)))
  for (i in 1:n) for (j in 1:n) D[i, j] <- 1 - similaridade(m[i, ], m[j, ])[indice]
  as.dist(D)
}

ab_parc <- t(ab)                                       # parcela × espécie
d_jac <- dist_matriz(ab_parc, "Jaccard")
d_bc  <- dist_matriz(ab_parc, "BrayCurtis")

par(mfrow = c(1, 2), mar = c(7, 4, 2, 1))
distancias <- list(Jaccard = d_jac, `Bray-Curtis` = d_bc)
for (nm in names(distancias)) {
  h <- hclust(distancias[[nm]], method = "average")
  plot(h, hang = -1, main = nm, xlab = "", sub = "", ylab = "Distância", cex = 0.8)
}

c(Jaccard     = cor(d_jac, cophenetic(hclust(d_jac, "average"))),
  Bray_Curtis = cor(d_bc,  cophenetic(hclust(d_bc,  "average"))))


# ---- Conferindo com pacotes ----

# ---- 6. Exercícios ----

# ---- Para entregar (EE5) ----

# ---- Para o relatório (formato de artigo) ----

# ---- Referências ----
