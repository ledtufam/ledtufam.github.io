# ======================================================================
# Calendário fenológico a partir de registros de herbário (speciesLink)
# R Lab - Laboratório de Ecologia e Dendrologia Tropical (LEDT/UFAM)
# https://ledtufam.github.io/extensao/rlab/rotinas/fenologia-specieslink.html
# ======================================================================


# ---- 1. Baixar os registros do speciesLink ----
library(jsonlite)

baixar_splink <- function(especie, chave = Sys.getenv("SPLINK_KEY"), limite = 5000) {
  url <- paste0("https://specieslink.net/ws/1.0/search?",
                "scientificname=", URLencode(especie),
                "&scope=p&limit=", limite,
                "&apikey=", chave)
  res <- fromJSON(url)
  res$features$properties          # tabela com um registro por linha
}

reg <- baixar_splink("Handroanthus serratifolius")
nrow(reg)
names(reg)


# ---- Prefere baixar pelo site? ----

# ---- 2. Dados de exemplo ----
set.seed(921)
n <- 240
mes_fl <- sample(1:12, n, replace = TRUE, prob = c(1, 1, 1, 1, 2, 4, 9, 12, 8, 3, 1, 1))
fen <- sample(c("flor", "fruto", "esteril", "ambos"), n, replace = TRUE, prob = c(.40, .25, .25, .10))
mes <- ifelse(fen == "fruto", (mes_fl + 1) %% 12 + 1, mes_fl)   # frutos ~2 meses depois

modelos <- list(
  flor    = c("Árvore 15 m, flores amarelas.", "fl. amarelas", "Com flores.", "Botões e flores abertas.", "Floração intensa, sem folhas."),
  fruto   = c("Frutos imaturos.", "fr. verdes", "Árvore com frutos maduros, deiscentes.", "Frutificando."),
  ambos   = c("Flores e frutos.", "fl. e fr. jovens"),
  esteril = c("Material estéril.", "Árvore 20 m, DAP 40 cm.", "", "Ramo estéril, folhas compostas.")
)
reg <- data.frame(
  scientificName    = "Espécie hipotética",
  monthCollected    = mes,
  stateProvince     = sample(c("Amazonas", "Pará", "Acre", "Rondônia", "Mato Grosso"), n, replace = TRUE),
  occurrenceRemarks = vapply(fen, function(f) sample(modelos[[f]], 1), "")
)
head(reg)


# ---- 3. Identificar flor e fruto nas etiquetas ----
txt <- tolower(reg$occurrenceRemarks)

termos_flor  <- c("\\bflor", "\\bfl\\.", "\\bflower", "\\bbot[aã]o", "\\bbot[oõ]es", "\\bflora[cç][aã]o", "\\bantese")
termos_fruto <- c("\\bfrut", "\\bfr\\.", "\\bfruit")

reg$flor  <- grepl(paste(termos_flor,  collapse = "|"), txt)
reg$fruto <- grepl(paste(termos_fruto, collapse = "|"), txt)

# registros sem mês ou sem informação de fenologia não entram no calendário
reg <- subset(reg, !is.na(monthCollected) & monthCollected %in% 1:12)
c(total = nrow(reg), com_flor = sum(reg$flor), com_fruto = sum(reg$fruto),
  sem_info = sum(!reg$flor & !reg$fruto))


# ---- 4. Contar registros por mês ----
meses <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")

calendario <- data.frame(
  mes   = factor(meses, levels = meses),
  flor  = tabulate(reg$monthCollected[reg$flor],  nbins = 12),
  fruto = tabulate(reg$monthCollected[reg$fruto], nbins = 12),
  total = tabulate(reg$monthCollected,            nbins = 12)
)
calendario

calendario$prop_flor  <- round(calendario$flor  / calendario$total, 2)
calendario$prop_fruto <- round(calendario$fruto / calendario$total, 2)
calendario[, c("mes", "prop_flor", "prop_fruto")]


# ---- 5. Calendário fenológico ----
cores <- c(flor = "#ffce67", fruto = "#78c2ad")
barplot(t(as.matrix(calendario[, c("flor", "fruto")])), beside = TRUE,
        names.arg = meses, col = cores, border = NA, las = 1,
        ylab = "Número de registros", legend.text = c("Flor", "Fruto"),
        args.legend = list(x = "topright", bty = "n"))


# ---- Versão circular ----
calendario_circular <- function(contagem, cor, titulo = "") {
  ang <- pi / 2 - (0:11) * 2 * pi / 12            # janeiro no topo, sentido horário
  r   <- contagem / max(contagem)
  plot(NA, xlim = c(-1.25, 1.25), ylim = c(-1.25, 1.25), asp = 1, axes = FALSE,
       xlab = "", ylab = "", main = titulo)
  for (k in c(.25, .5, .75, 1)) symbols(0, 0, circles = k, inches = FALSE, add = TRUE, fg = "grey85")
  for (i in 1:12) {
    a <- seq(ang[i] + pi / 12, ang[i] - pi / 12, length.out = 20)
    polygon(c(0, r[i] * cos(a), 0), c(0, r[i] * sin(a), 0), col = cor, border = "white")
  }
  text(1.15 * cos(ang), 1.15 * sin(ang), meses, cex = 0.8)
}

par(mfrow = c(1, 2), mar = c(0, 0, 2, 0))
calendario_circular(calendario$flor,  cores["flor"],  "Floração")
calendario_circular(calendario$fruto, cores["fruto"], "Frutificação")


# ---- 6. Mês médio e concentração (estatística circular) ----
media_circular <- function(m) {
  ang <- (m - 0.5) * 2 * pi / 12
  C <- mean(cos(ang)); S <- mean(sin(ang))
  ang_medio <- atan2(S, C) %% (2 * pi)
  c(mes_medio = round(ang_medio * 12 / (2 * pi) + 0.5, 1), r = round(sqrt(C^2 + S^2), 2))
}

rbind(Floração     = media_circular(reg$monthCollected[reg$flor]),
      Frutificação = media_circular(reg$monthCollected[reg$fruto]))


# ---- Para discutir na prática ----

# ---- Referências ----
