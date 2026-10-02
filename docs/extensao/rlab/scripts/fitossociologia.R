# ======================================================================
# Parâmetros fitossociológicos e diversidade
# R Lab - Laboratório de Ecologia e Dendrologia Tropical (LEDT/UFAM)
# https://ledtufam.github.io/extensao/rlab/rotinas/fitossociologia.html
# ======================================================================


# ---- 1. Dados ----
set.seed(2026)
especies <- c("Eschweilera coriacea", "Protium hebetatum", "Pouteria guianensis",
              "Ecclinusa guianensis", "Licania micrantha", "Micropholis guyanensis",
              "Scleronema micranthum", "Oenocarpus bataua", "Goupia glabra",
              "Dinizia excelsa")
prob <- c(0.22, 0.18, 0.12, 0.10, 0.09, 0.08, 0.07, 0.07, 0.05, 0.02)

n_arv <- 600
inv <- data.frame(
  parcela = sample(paste0("P", 1:10), n_arv, replace = TRUE),
  especie = sample(especies, n_arv, replace = TRUE, prob = prob),
  dap     = round(10 + rlnorm(n_arv, meanlog = 2.3, sdlog = 0.7), 1)  # DAP >= 10 cm
)
head(inv)

# Para seus dados, use por exemplo:
# inv <- read.csv2("dados/inventario.csv")


# ---- 2. Definições da amostragem ----
area_parcela <- 0.1                          # ha
n_parcelas   <- length(unique(inv$parcela))
area_total   <- area_parcela * n_parcelas    # ha


# ---- 3. Cálculo dos parâmetros ----
inv$g <- pi * inv$dap^2 / 40000      # área basal de cada árvore (m²)

n_i <- tapply(inv$dap, inv$especie, length)                         # nº de árvores
p_i <- tapply(inv$parcela, inv$especie, function(x) length(unique(x)))  # nº de parcelas
G_i <- tapply(inv$g, inv$especie, sum)                              # área basal

fito <- data.frame(
  especie = names(n_i),
  N   = as.integer(n_i),
  DA  = n_i / area_total,
  DR  = 100 * n_i / sum(n_i),
  FA  = 100 * p_i / n_parcelas,
  DoA = G_i / area_total,
  DoR = 100 * G_i / sum(G_i)
)
fito$FR  <- 100 * fito$FA / sum(fito$FA)
fito$IVI <- fito$DR + fito$FR + fito$DoR
fito$IVC <- fito$DR + fito$DoR

fito <- fito[order(fito$IVI, decreasing = TRUE), ]
rownames(fito) <- NULL
knitr::kable(fito, digits = 2)


# ---- 4. Gráfico do IVI ----
comp <- t(as.matrix(fito[, c("DR", "FR", "DoR")]))
colnames(comp) <- fito$especie
par(mar = c(4, 12, 1, 1))
barplot(comp[, ncol(comp):1], horiz = TRUE, las = 1, font.axis = 3,
        col = c("#78c2ad", "#f3969a", "#6cc3d5"), border = NA,
        xlab = "IVI", cex.names = 0.8)
legend("bottomright", legend = c("DR", "FR", "DoR"),
       fill = c("#78c2ad", "#f3969a", "#6cc3d5"), border = NA, bty = "n")


# ---- 5. Diversidade ----
p <- fito$N / sum(fito$N)
S <- nrow(fito)

H <- -sum(p * log(p))
simpson <- 1 - sum(p^2)
J <- H / log(S)

data.frame(Riqueza = S, Shannon = round(H, 3),
           Simpson = round(simpson, 3), Pielou = round(J, 3))


# ---- Conferindo com pacotes ----

# ---- 6. Exporte os resultados ----
write.csv2(fito, "resultados/fitossociologia.csv", row.names = FALSE)


# ---- Referências ----
