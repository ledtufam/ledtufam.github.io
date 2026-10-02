# ======================================================================
# Espectros NIR: do arquivo bruto à discriminação de espécies
# R Lab - Laboratório de Ecologia e Dendrologia Tropical (LEDT/UFAM)
# https://ledtufam.github.io/extensao/rlab/rotinas/nir-especies.html
# ======================================================================


# ---- Dados e scripts originais ----

# ---- Os dados ----

# ---- 1. Gerar a matriz espectral no ISC Reader App ----

# ---- 2. Importar a matriz no R ----
m    <- read.csv("dados/matriz_isc_handroanthus.csv", check.names = FALSE)   # no seu projeto: "dados/..."
meta <- read.csv("dados/handroanthus_metadados.csv")

dim(m)
m[1:3, 1:6]


# ---- Informações no nome do arquivo ----
partes <- do.call(rbind, strsplit(sub("_r\\.csv$", "", m$File), "_"))
info <- data.frame(
  barcode = partes[, 1],     # código de barras da exsicata
  face    = partes[, 2],     # ab = abaxial; ad = adaxial
  config  = partes[, 3],
  data    = partes[, 4],
  hora    = partes[, 5]
)
info$leitura <- paste(info$barcode, info$face, info$hora, sep = "_")
head(info, 3)

table(info$face)
head(table(info$barcode, info$face))


# ---- 3. Ajustar os comprimentos de onda ----
wl_bruto <- as.numeric(colnames(m)[-1])
R_bruto  <- as.matrix(m[, -1])
table(rowSums(!is.na(R_bruto)))

wl_ref <- seq(900, 1700, by = 3.52)
length(wl_ref)

X <- t(apply(R_bruto, 1, function(r) r[!is.na(r)]))   # 228 valores de cada leitura, em ordem
colnames(X) <- wl_ref
dim(X)


# ---- 4. Associar as espécies ----
info$especie <- meta$especie[match(info$barcode, meta$barcode)]
table(info$especie, info$face)


# ---- 5. Visualizar os espectros ----
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
for (f in c("ab", "ad")) {
  sel <- info$face == f
  matplot(wl_ref, t(X[sel, ]), type = "l", lty = 1, col = adjustcolor("firebrick", 0.5),
          xlab = "Comprimento de onda (nm)", ylab = "Reflectância", ylim = c(0, 1.05),
          main = ifelse(f == "ab", "Abaxial", "Adaxial"))
}


# ---- Recorte da faixa espectral ----
manter <- wl_ref >= 950 & wl_ref <= 1650
X <- X[, manter]
wl <- wl_ref[manter]
ncol(X)       # 199 comprimentos de onda


# ---- Controle de qualidade: leituras problemáticas ----
library(plotly)
longo <- data.frame(leitura = rep(info$leitura, ncol(X)),
                    wl = rep(wl, each = nrow(X)), refl = as.vector(X))
plot_ly(longo, x = ~wl, y = ~refl, split = ~leitura, type = "scatter", mode = "lines",
        line = list(width = 1), showlegend = FALSE)

remover <- c("INPA0206017_ab_102412", "INPA0206014_ad_112909",
             "INPA0206027_ad_105717", "INPA0206027_ad_105558",
             "INPA0206027_ad_105646", "INPA0206027_ad_105620")

# como essas leituras se comparam às demais?
round(rowMeans(X[info$leitura %in% remover, ]), 3)
summary(rowMeans(X[!info$leitura %in% remover, ]))

ok <- !info$leitura %in% remover
X <- X[ok, ]
info <- info[ok, ]
nrow(X)       # 468 leituras

par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
for (f in c("ab", "ad")) {
  sel <- info$face == f
  matplot(wl, t(X[sel, ]), type = "l", lty = 1, col = adjustcolor("firebrick", 0.5),
          xlab = "Comprimento de onda (nm)", ylab = "Reflectância", ylim = c(0.2, 0.85),
          main = ifelse(f == "ab", "Abaxial", "Adaxial"))
}


# ---- 6. Médias por exsicata: três conjuntos de dados ----
media_exsicata <- function(sel) {
  g <- factor(info$barcode[sel])
  M <- rowsum(X[sel, ], g) / as.vector(table(g))
  data.frame(especie = meta$especie[match(rownames(M), meta$barcode)],
             barcode = rownames(M), M, check.names = FALSE, row.names = NULL)
}

dados <- list(
  "Abaxial + Adaxial" = media_exsicata(rep(TRUE, nrow(X))),
  "Abaxial"           = media_exsicata(info$face == "ab"),
  "Adaxial"           = media_exsicata(info$face == "ad")
)
sapply(dados, nrow)                       # nº de exsicatas em cada conjunto
table(dados[["Abaxial + Adaxial"]]$especie)

especies <- sort(unique(info$especie))
cores <- setNames(c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e"), especies)

par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))
for (nm in names(dados)) {
  d <- dados[[nm]]
  medias <- rowsum(as.matrix(d[, -(1:2)]), d$especie) / as.vector(table(d$especie))
  matplot(wl, t(medias), type = "l", lty = 1:5, lwd = 2, col = cores[rownames(medias)],
          xlab = "Comprimento de onda (nm)", ylab = "Reflectância", main = nm, ylim = c(0.45, 0.7))
}
legend("bottomright", legend = especies, col = cores, lty = 1:5, lwd = 2, bty = "n",
       text.font = 3, cex = 0.8)


# ---- 7. LDA com validação leave-one-out ----
library(MASS)

lda_loo <- function(d) {
  Xd <- as.matrix(d[, -(1:2)])               # só as colunas espectrais
  y  <- factor(d$especie)
  res <- data.frame(barcode = d$barcode, observada = y, prevista = NA, prob = NA)
  post <- matrix(NA, nrow(d), nlevels(y), dimnames = list(NULL, levels(y)))

  for (i in seq_len(nrow(d))) {
    modelo   <- suppressWarnings(lda(Xd[-i, ], grouping = y[-i]))   # ajusta sem a exsicata i
    previsao <- predict(modelo, Xd[i, , drop = FALSE])              # classifica a exsicata i
    res$prevista[i] <- as.character(previsao$class)
    res$prob[i]     <- max(previsao$posterior)
    post[i, ]       <- previsao$posterior
  }

  res$tipo <- paste0(ifelse(res$prevista == res$observada, "Correta", "Incorreta"),
                     ifelse(res$prob > 0.95, " (significativa)", " (não significativa)"))
  res$prevista <- factor(res$prevista, levels = levels(y))
  cbind(res, round(post, 4))
}


# ---- Abaxial + adaxial ----
loo <- lda_loo(dados[["Abaxial + Adaxial"]])
head(loo[, c("barcode", "observada", "prevista", "prob", "tipo")])

acuracia <- function(loo) {
  acertos <- sum(loo$prevista == loo$observada)
  ic <- binom.test(acertos, nrow(loo))$conf.int
  c(acuracia = 100 * acertos / nrow(loo), IC_inf = 100 * ic[1], IC_sup = 100 * ic[2])
}

conf <- table(Observada = loo$observada, Prevista = loo$prevista)
dimnames(conf) <- lapply(dimnames(conf), function(x) sub("Handroanthus", "H.", x))
conf
round(acuracia(loo), 2)
table(loo$tipo)

plot_confusao <- function(conf, titulo = "") {
  k <- nrow(conf)
  par(mar = c(8, 9, 3, 1))
  image(1:k, 1:k, t(conf[k:1, ]), col = colorRampPalette(c("white", "firebrick"))(20),
        axes = FALSE, xlab = "", ylab = "", main = titulo, cex.main = 0.95)
  axis(1, 1:k, colnames(conf), las = 2, font = 3, cex.axis = 0.85)
  axis(2, 1:k, rev(rownames(conf)), las = 1, font = 3, cex.axis = 0.85)
  mtext("Prevista", side = 1, line = 6.5); mtext("Observada", side = 2, line = 7.5)
  for (i in 1:k) for (j in 1:k) text(j, k - i + 1, conf[i, j], cex = 1.2)
  box()
}
ac <- acuracia(loo)
plot_confusao(conf, sprintf("Abaxial + Adaxial | %.1f%% (IC 95%%: %.1f–%.1f)", ac[1], ac[2], ac[3]))


# ---- Comparando as faces da folha ----
resultados <- lapply(dados, lda_loo)

tabela <- t(sapply(resultados, function(r) {
  a <- acuracia(r)
  c(Exsicatas = nrow(r),
    `Acurácia (%)` = round(a[[1]], 1),
    `IC 95% (%)` = paste0(round(a[[2]], 1), "–", round(a[[3]], 1)),
    `Corretas p > 0,95` = sum(r$tipo == "Correta (significativa)"),
    `Incorretas p > 0,95` = sum(r$tipo == "Incorreta (significativa)"))
}))
knitr::kable(tabela)

par(mfrow = c(1, 2))
for (nm in c("Abaxial", "Adaxial")) {
  r <- resultados[[nm]]
  cf <- table(r$observada, r$prevista)
  dimnames(cf) <- lapply(dimnames(cf), function(x) sub("Handroanthus", "H.", x))
  ac <- acuracia(r)
  plot_confusao(cf, sprintf("%s | %.1f%%", nm, ac[1]))
}


# ---- Interpretação ----

# ---- Exportar os resultados ----
write.csv(resultados[["Abaxial + Adaxial"]], "resultados/loo_abaxial_adaxial.csv", row.names = FALSE)
write.csv(resultados[["Abaxial"]],           "resultados/loo_abaxial.csv",         row.names = FALSE)
write.csv(resultados[["Adaxial"]],           "resultados/loo_adaxial.csv",         row.names = FALSE)


# ---- Cuidados ----

# ---- Referências ----
