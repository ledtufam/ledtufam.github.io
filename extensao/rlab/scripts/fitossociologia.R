# ======================================================================
# Parâmetros fitossociológicos e estrutura
# R Lab - Laboratório de Ecologia e Dendrologia Tropical (LEDT/UFAM)
# https://ledtufam.github.io/extensao/rlab/rotinas/fitossociologia.html
# ======================================================================


# ---- Roteiro da aula prática (09/10 · EE4) ----

# ---- 1. Dados ----
cx <- read.csv("dados/caixeta.csv")   # no seu projeto: read.csv("dados/caixeta.csv")
cx$especie[cx$especie == "Callophyllum brasiliensis"] <- "Calophyllum brasiliensis"
str(cx)

fator_cap <- 0.1    # mm  -> cm
fator_h   <- 0.1    # dm  -> m
cx$cap_cm <- cx$cap * fator_cap
cx$h_m    <- cx$h * fator_h
cx$dap    <- cx$cap_cm / pi                     # DAP (cm)
cx$g      <- pi * cx$dap^2 / 40000              # área basal do fuste (m²) = CAP²/(4π)/10000
summary(cx[, c("dap", "h_m", "g")])


# ---- 2. De fustes para árvores ----
cx$parc <- paste(cx$local, cx$parcela, sep = "_")
chave   <- c("local", "parc", "arvore", "especie")

arv <- aggregate(g ~ local + parc + arvore + especie, data = cx, FUN = sum)
arv$h_m     <- aggregate(h_m ~ local + parc + arvore + especie, data = cx, FUN = max)$h_m
arv$fustes  <- aggregate(fuste ~ local + parc + arvore + especie, data = cx, FUN = length)$fuste
arv$dap_eq  <- sqrt(arv$g * 40000 / pi)        # DAP equivalente (cm) de uma árvore com vários fustes

c(fustes = nrow(cx), arvores = nrow(arv), parcelas = length(unique(arv$parc)))
head(arv)


# ---- 3. Passo a passo com uma espécie ----
A  <- 15 * 10 * 20 / 10000           # 0,3 ha
UT <- 15
m  <- arv[arv$especie == "Myrcia sulfiflora", ]

n_i <- nrow(m);                  N <- nrow(arv)
u_i <- length(unique(m$parc))
G_i <- sum(m$g);                 G <- sum(arv$g)

c(DA = n_i / A, DR = 100 * n_i / N, FA = 100 * u_i / UT,
  DoA = G_i / A, DoR = 100 * G_i / G)


# ---- No Excel ----

# ---- 4. Uma função para todas as espécies ----
fitossociologia <- function(d, A, UT) {
  n_i <- tapply(d$g, d$especie, length)
  u_i <- tapply(d$parc, d$especie, function(x) length(unique(x)))
  G_i <- tapply(d$g, d$especie, sum)
  FA  <- 100 * u_i / UT
  f <- data.frame(
    especie = names(n_i), n = as.integer(n_i),
    DA  = n_i / A,  DR  = 100 * n_i / sum(n_i),
    FA  = FA,       FR  = 100 * FA / sum(FA),
    DoA = G_i / A,  DoR = 100 * G_i / sum(G_i)
  )
  f$VI <- f$DR + f$FR + f$DoR
  f$VC <- f$DR + f$DoR
  f <- f[order(f$VI, decreasing = TRUE), ]
  rownames(f) <- NULL
  f
}

geral <- fitossociologia(arv, A = 0.3, UT = 15)
knitr::kable(head(geral, 10), digits = 2,
             caption = "Parâmetros fitossociológicos das 10 espécies de maior VI (15 parcelas, 0,3 ha).")

c(arvores_ha = nrow(arv) / 0.3, area_basal_m2_ha = sum(arv$g) / 0.3,
  riqueza = length(unique(arv$especie)))


# ---- 5. Comparando os três caixetais ----
por_local <- lapply(split(arv, arv$local), fitossociologia, A = 0.1, UT = 5)

resumo <- t(sapply(split(arv, arv$local), function(d) c(
  arvores_ha = nrow(d) / 0.1,
  G_m2_ha    = round(sum(d$g) / 0.1, 1),
  riqueza    = length(unique(d$especie)),
  VI_caixeta = round(subset(fitossociologia(d, 0.1, 5), especie == "Tabebuia cassinoides")$VI, 1)
)))
knitr::kable(resumo, caption = "Resumo da estrutura por caixetal.")

cores <- c("#78c2ad", "#f3969a", "#6cc3d5")
par(mfrow = c(3, 1), mar = c(4, 14, 2, 1))
for (l in names(por_local)) {
  f <- head(por_local[[l]], 5)
  comp <- t(as.matrix(f[, c("DR", "FR", "DoR")]))
  colnames(comp) <- f$especie
  barplot(comp[, ncol(comp):1], horiz = TRUE, las = 1, font.axis = 3, col = cores,
          border = NA, xlim = c(0, 300), xlab = "VI", main = l, cex.names = 0.9)
}
legend("bottomright", legend = c("DR", "FR", "DoR"), fill = cores, border = NA, bty = "n")


# ---- 6. Estrutura diamétrica e vertical ----
classes_d <- seq(0, ceiling(max(arv$dap_eq) / 5) * 5, by = 5)
classes_h <- seq(0, ceiling(max(arv$h_m) / 2) * 2, by = 2)

par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))
for (l in sort(unique(arv$local)))
  hist(arv$dap_eq[arv$local == l], breaks = classes_d, col = "#78c2ad", border = "white",
       main = l, xlab = "DAP equivalente (cm)", ylab = "Árvores")
for (l in sort(unique(arv$local)))
  hist(arv$h_m[arv$local == l], breaks = classes_h, col = "#6cc3d5", border = "white",
       main = l, xlab = "Altura (m)", ylab = "Árvores")

tab_d <- table(arv$local, cut(arv$dap_eq, classes_d))
tab_d[, colSums(tab_d) > 0]

arv$grupo <- ifelse(arv$especie == "Tabebuia cassinoides", "caixeta", "outras")
aggregate(fustes ~ grupo, data = arv, FUN = function(x) round(c(media = mean(x), max = max(x)), 2))


# ---- 7. Exercícios ----

# ---- Para entregar (EE4) ----

# ---- Para o relatório (formato de artigo) ----

# ---- Exporte os resultados ----
dir.create("resultados", showWarnings = FALSE)
write.csv2(geral, "resultados/fitossociologia_geral.csv", row.names = FALSE)
for (l in names(por_local))
  write.csv2(por_local[[l]], paste0("resultados/fitossociologia_", l, ".csv"), row.names = FALSE)


# ---- Referências ----
