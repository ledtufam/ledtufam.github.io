# ======================================================================
# Índices de heterogeneidade: Shannon, Simpson e Pielou
# R Lab - Laboratório de Ecologia e Dendrologia Tropical (LEDT/UFAM)
# https://ledtufam.github.io/extensao/rlab/rotinas/heterogeneidade.html
# ======================================================================


# ---- Roteiro da aula prática (23/10 · EE6) ----

# ---- 1. Dados ----
cx <- read.csv("dados/caixeta.csv")   # no seu projeto: read.csv("dados/caixeta.csv")
cx$especie[cx$especie == "Callophyllum brasiliensis"] <- "Calophyllum brasiliensis"

arv <- unique(cx[, c("local", "parcela", "arvore", "especie")])   # uma linha por árvore
ab  <- unclass(table(arv$local, arv$especie))                      # local × espécie
ab[, 1:5]


# ---- 2. Passo a passo: o caixetal da Jureia ----
n <- ab["jureia", ]
n <- n[n > 0]                  # só as espécies presentes
N <- sum(n); S <- length(n)
p <- n / N

H <- -sum(p * log(p))
D <- sum(n * (n - 1)) / (N * (N - 1))
J <- H / log(S)

c(N = N, S = S, Shannon = round(H, 3), Simpson_D = round(D, 3),
  Gini_Simpson = round(1 - D, 3), Simpson_inv = round(1 / D, 2), Pielou = round(J, 3))


# ---- 3. Os três caixetais ----
heterogeneidade <- function(n) {
  n <- n[n > 0]; N <- sum(n); S <- length(n); p <- n / N
  H <- -sum(p * log(p)); D <- sum(n * (n - 1)) / (N * (N - 1))
  c(N = N, S = S, Shannon = H, Simpson_D = D, Gini_Simpson = 1 - D,
    Simpson_inv = 1 / D, Pielou = H / log(S))
}

het <- t(apply(ab, 1, heterogeneidade))
het <- rbind(het, total = heterogeneidade(colSums(ab)))
knitr::kable(het, digits = 3, caption = "Índices de heterogeneidade por caixetal.")

cores <- c(chauas = "#1b9e77", jureia = "#d95f02", retiro = "#7570b3")
plot(NA, xlim = c(1, max(het[1:3, "S"])), ylim = c(0.1, 100), log = "y",
     xlab = "Posição da espécie (da mais para a menos abundante)",
     ylab = "Abundância relativa (%)")
for (l in rownames(ab)) {
  pr <- sort(ab[l, ab[l, ] > 0], decreasing = TRUE) / sum(ab[l, ]) * 100
  lines(seq_along(pr), pr, type = "b", pch = 19, col = cores[l])
}
legend("topright", legend = names(cores), col = cores, pch = 19, lty = 1, bty = "n")


# ---- 4. Os índices de Shannon diferem? Teste t de Hutcheson ----
hutcheson <- function(n1, n2) {
  v <- function(n) {
    n <- n[n > 0]; N <- sum(n); S <- length(n); p <- n / N
    H <- -sum(p * log(p))
    c(H = H, var = (sum(p * log(p)^2) - sum(p * log(p))^2) / N + (S - 1) / (2 * N^2), N = N)
  }
  a <- v(n1); b <- v(n2)
  t  <- (a["H"] - b["H"]) / sqrt(a["var"] + b["var"])
  gl <- (a["var"] + b["var"])^2 / (a["var"]^2 / a["N"] + b["var"]^2 / b["N"])
  c(H1 = a[["H"]], H2 = b[["H"]], t = t[[1]], gl = gl[[1]], p = 2 * pt(-abs(t[[1]]), gl[[1]]))
}

pares <- combn(rownames(ab), 2)
teste <- t(apply(pares, 2, function(x) hutcheson(ab[x[1], ], ab[x[2], ])))
rownames(teste) <- apply(pares, 2, paste, collapse = " × ")
knitr::kable(teste, digits = 4, caption = "Teste t de Hutcheson entre os caixetais.")


# ---- 5. Números de Hill: uma família de índices ----
hill <- function(n, q) {
  p <- n[n > 0] / sum(n)
  if (q == 1) exp(-sum(p * log(p))) else sum(p^q)^(1 / (1 - q))
}
qs <- seq(0, 3, by = 0.1)
perfis <- sapply(rownames(ab), function(l) sapply(qs, function(q) hill(ab[l, ], q)))

matplot(qs, perfis, type = "l", lty = 1, lwd = 2, col = cores[colnames(perfis)],
        xlab = "Ordem q", ylab = "Número efetivo de espécies")
legend("topright", legend = colnames(perfis), col = cores[colnames(perfis)], lwd = 2, bty = "n")
round(perfis[qs %in% c(0, 1, 2), ], 2)


# ---- 6. O peso da espécie dominante ----
sem_caixeta <- ab[, colnames(ab) != "Tabebuia cassinoides"]
knitr::kable(cbind(com = het[1:3, c("Shannon", "Pielou")],
                   sem = t(apply(sem_caixeta, 1, heterogeneidade))[, c("Shannon", "Pielou")]),
             digits = 3, col.names = c("H' (com)", "J' (com)", "H' (sem)", "J' (sem)"),
             caption = "Shannon e Pielou com e sem a caixeta.")


# ---- 7. Exercícios ----

# ---- Para entregar (EE6) ----

# ---- Para o relatório (formato de artigo) ----

# ---- Referências ----
