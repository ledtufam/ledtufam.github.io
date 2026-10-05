# ======================================================================
# Diversidade da vegetação: rarefação, estimadores e diversidade beta
# R Lab - Laboratório de Ecologia e Dendrologia Tropical (LEDT/UFAM)
# https://ledtufam.github.io/extensao/rlab/rotinas/diversidade.html
# ======================================================================


# ---- Roteiro da aula prática (30/10 · EE7) ----

# ---- 1. Dados ----
cx <- read.csv("dados/caixeta.csv")   # no seu projeto: read.csv("dados/caixeta.csv")
cx$especie[cx$especie == "Callophyllum brasiliensis"] <- "Calophyllum brasiliensis"

arv <- unique(cx[, c("local", "parcela", "arvore", "especie")])   # uma linha por árvore
arv$parc <- paste(arv$local, arv$parcela, sep = "_")

ab_parc  <- unclass(table(arv$parc, arv$especie))    # parcela × espécie
ab_local <- unclass(table(arv$local, arv$especie))   # local × espécie
rowSums(ab_local)                                    # árvores por caixetal


# ---- 2. Curva do coletor (acumulação de espécies) ----
coletor <- function(m, n_perm = 200) {
  m <- m > 0
  res <- replicate(n_perm, {
    ordem <- sample(nrow(m))
    sapply(seq_along(ordem), function(k) sum(colSums(m[ordem[1:k], , drop = FALSE]) > 0))
  })
  data.frame(parcelas = 1:nrow(m), media = rowMeans(res),
             inf = apply(res, 1, quantile, 0.025), sup = apply(res, 1, quantile, 0.975))
}

set.seed(2026)
cc <- coletor(ab_parc)
cc

plot(cc$parcelas, cc$media, type = "n", ylim = c(0, max(cc$sup) + 5),
     xlab = "Número de parcelas (200 m² cada)", ylab = "Número de espécies")
polygon(c(cc$parcelas, rev(cc$parcelas)), c(cc$inf, rev(cc$sup)), col = "#78c2ad55", border = NA)
lines(cc$parcelas, cc$media, lwd = 2, col = "#2c7a5e")
points(cc$parcelas, cc$media, pch = 19, col = "#2c7a5e")


# ---- 3. Rarefação: comparar a riqueza com o mesmo número de indivíduos ----
rarefacao <- function(x, n) {
  x <- x[x > 0]; N <- sum(x)
  sapply(n, function(k) sum(1 - exp(lchoose(N - x, k) - lchoose(N, k))))
}

n_min <- min(rowSums(ab_local))          # o menor caixetal (jureia, 178 árvores)
sapply(rownames(ab_local), function(l) round(rarefacao(ab_local[l, ], n_min), 1))

cores <- c(chauas = "#1b9e77", jureia = "#d95f02", retiro = "#7570b3")
plot(NA, xlim = c(1, max(rowSums(ab_local))), ylim = c(0, 25),
     xlab = "Número de árvores", ylab = "Número esperado de espécies")
for (l in rownames(ab_local)) {
  n <- 1:sum(ab_local[l, ])
  lines(n, rarefacao(ab_local[l, ], n), lwd = 2, col = cores[l])
}
abline(v = n_min, lty = 2, col = "grey50")
legend("bottomright", legend = names(cores), col = cores, lwd = 2, bty = "n")


# ---- 4. Quantas espécies faltam? Estimadores de riqueza ----
estimadores <- function(m) {          # m = parcela × espécie (abundâncias)
  x  <- colSums(m); x <- x[x > 0]
  Sobs <- length(x); F1 <- sum(x == 1); F2 <- sum(x == 2)
  inc <- colSums(m > 0); inc <- inc[inc > 0]
  Q1 <- sum(inc == 1); k <- nrow(m)
  c(S_obs = Sobs, F1 = F1, F2 = F2, Chao1 = Sobs + F1 * (F1 - 1) / (2 * (F2 + 1)),
    Q1 = Q1, Jack1 = Sobs + Q1 * (k - 1) / k)
}

est <- rbind(
  t(sapply(split(as.data.frame(ab_parc), sub("_.*", "", rownames(ab_parc))),
           function(d) estimadores(as.matrix(d)))),
  total = estimadores(ab_parc))
knitr::kable(est, digits = 1, caption = "Riqueza observada e estimada por caixetal e no total.")


# ---- 5. Diversidade alfa, beta e gama ----
particao <- function(m) {
  alfa <- mean(rowSums(m > 0)); gama <- sum(colSums(m) > 0)
  c(alfa = alfa, gama = gama, beta_mult = gama / alfa, beta_adit = gama - alfa)
}

rbind(
  `parcelas dentro de chauas` = particao(ab_parc[grepl("chauas", rownames(ab_parc)), ]),
  `parcelas dentro de jureia` = particao(ab_parc[grepl("jureia", rownames(ab_parc)), ]),
  `parcelas dentro de retiro` = particao(ab_parc[grepl("retiro", rownames(ab_parc)), ]),
  `entre os caixetais`        = particao(ab_local)
) |> round(2)


# ---- 6. Exercícios ----

# ---- Para entregar (EE7) ----

# ---- Para o relatório (formato de artigo) ----

# ---- Referências ----
