# =====================================================================
#  CALENDÁRIO FENOLÓGICO A PARTIR DE REGISTROS DE HERBÁRIO (speciesLink)
#  Atividade prática - FGD921 Dendrologia Tropical (UFAM)
#  Laboratório de Ecologia e Dendrologia Tropical (LEDT) - ledtufam.github.io
#
#  COMO USAR
#  1. Baixe um dos datasets (.csv) na página da atividade:
#     https://ledtufam.github.io/extensao/rlab/rotinas/fenologia-specieslink.html
#  2. Abra este script no RStudio.
#  3. Aperte Ctrl + Shift + Enter (ou clique na setinha ao lado de
#     "Source" e escolha "Source with Echo"). Uma janela vai abrir:
#     escolha o arquivo .csv do dataset.
#     (Se a janela não aparecer, procure-a atrás do RStudio.)
#  4. Os resultados (tabela e figuras) são salvos numa pasta nova
#     "resultados_<espécie>", no mesmo lugar do arquivo .csv.
#
#  Este script usa só o R básico: não é preciso instalar pacotes.
# =====================================================================


# ---------------------------------------------------------------------
# 1. LER OS DADOS
# ---------------------------------------------------------------------
arquivo <- file.choose()          # abre a janela para escolher o .csv

reg <- read.csv(arquivo, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
especie <- reg$especie[1]

cat("\nEspécie:", especie, "\n")
cat("Número de registros no arquivo:", nrow(reg), "\n")
head(reg)


# ---------------------------------------------------------------------
# 2. IDENTIFICAR FLOR E FRUTO NAS ANOTAÇÕES DAS ETIQUETAS
#    As etiquetas não têm um campo padronizado de fenologia. Por isso,
#    procuramos palavras-chave (em português, inglês e espanhol).
# ---------------------------------------------------------------------
txt <- tolower(paste(" ", reg$anotacoes, " "))
txt[is.na(reg$anotacoes)] <- ""

# 2a. apagar expressões negativas, para não contar como flor/fruto
negativos <- c("sem flor(es)?", "sem fruto?s?", "sem fl\\.", "sem fr\\.",
               "without (flowers?|fruits?)", "sin (flor(es)?|frutos?)",
               "flor-de-[a-z]+", "florest[a-z]*")   # "floresta" não é flor!
for (n in negativos) txt <- gsub(n, " ", txt)

# 2b. palavras que indicam flor ou fruto
termos_flor  <- c("\\bflor", "\\bfl\\.", "\\bfls?\\b", "\\bflower", "\\bfloral",
                  "\\bbot[aã]o", "\\bbot[oõ]es", "\\bbud", "\\bflora[cç][aã]o",
                  "\\bfloresc", "\\bflorid", "\\bantese", "\\bp[eé]talas?",
                  "\\binflorescen")
termos_fruto <- c("\\bfrut", "\\bfr\\.", "\\bfrs?\\b", "\\bfruit", "\\bvagem",
                  "\\bvagens", "\\blegume", "\\bsementes?\\b", "\\bseeds?\\b",
                  "\\bc[aá]psula", "\\bdrupa", "\\bbaga\\b")

reg$flor  <- grepl(paste(termos_flor,  collapse = "|"), txt)
reg$fruto <- grepl(paste(termos_fruto, collapse = "|"), txt)

# 2c. só entram registros com mês de coleta válido (1 a 12)
reg$mes <- suppressWarnings(as.integer(reg$mes))
reg <- reg[!is.na(reg$mes) & reg$mes %in% 1:12, ]

resumo <- c(registros_com_mes = nrow(reg),
            com_flor  = sum(reg$flor),
            com_fruto = sum(reg$fruto),
            sem_informacao = sum(!reg$flor & !reg$fruto))
print(resumo)

# CONFIRA UMA AMOSTRA À MÃO! As linhas abaixo mostram 15 etiquetas
# classificadas como "flor" e 15 como "fruto". A busca acertou?
set.seed(1)
cat("\n--- Exemplos classificados como FLOR ---\n")
print(head(reg$anotacoes[reg$flor][sample(sum(reg$flor))], 15))
cat("\n--- Exemplos classificados como FRUTO ---\n")
print(head(reg$anotacoes[reg$fruto][sample(sum(reg$fruto))], 15))


# ---------------------------------------------------------------------
# 3. CONTAR REGISTROS POR MÊS
# ---------------------------------------------------------------------
meses <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
           "Jul", "Ago", "Set", "Out", "Nov", "Dez")

calendario <- data.frame(
  mes   = meses,
  total = tabulate(reg$mes, nbins = 12),            # todas as coletas
  flor  = tabulate(reg$mes[reg$flor],  nbins = 12), # coletas com flor
  fruto = tabulate(reg$mes[reg$fruto], nbins = 12)  # coletas com fruto
)
# proporção de coletas férteis em cada mês (controla o esforço de coleta)
calendario$prop_flor  <- round(calendario$flor  / pmax(calendario$total, 1), 2)
calendario$prop_fruto <- round(calendario$fruto / pmax(calendario$total, 1), 2)
print(calendario)


# ---------------------------------------------------------------------
# 4. MÊS MÉDIO E CONCENTRAÇÃO (ESTATÍSTICA CIRCULAR)
#    Cada mês vira um ângulo. r vai de 0 (espalhado no ano todo)
#    a 1 (todos os registros no mesmo mês).
# ---------------------------------------------------------------------
media_circular <- function(m) {
  if (length(m) < 3) return(c(mes_medio = NA, r = NA, n = length(m)))
  ang <- (m - 0.5) * 2 * pi / 12
  C <- mean(cos(ang)); S <- mean(sin(ang))
  ang_medio <- atan2(S, C) %% (2 * pi)
  c(mes_medio = round(ang_medio * 12 / (2 * pi) + 0.5, 1),
    r = round(sqrt(C^2 + S^2), 2), n = length(m))
}
circular <- rbind(Floracao     = media_circular(reg$mes[reg$flor]),
                  Frutificacao = media_circular(reg$mes[reg$fruto]))
print(circular)
# Dica: mes_medio = 8,5 significa segunda quinzena de agosto.


# ---------------------------------------------------------------------
# 5. GRÁFICOS
# ---------------------------------------------------------------------
cores <- c(flor = "#ffce67", fruto = "#78c2ad")

grafico_barras <- function() {
  par(mar = c(4, 4.5, 3, 1))
  barplot(t(as.matrix(calendario[, c("flor", "fruto")])), beside = TRUE,
          names.arg = meses, col = cores, border = NA, las = 1,
          ylab = "Número de registros", main = especie, font.main = 3,
          legend.text = c("Com flor", "Com fruto"),
          args.legend = list(x = "topright", bty = "n"))
}

calendario_circular <- function(contagem, cor, titulo = "") {
  ang <- pi / 2 - (0:11) * 2 * pi / 12          # janeiro no topo, sentido horário
  r   <- if (max(contagem) > 0) contagem / max(contagem) else contagem
  plot(NA, xlim = c(-1.25, 1.25), ylim = c(-1.25, 1.25), asp = 1, axes = FALSE,
       xlab = "", ylab = "", main = titulo)
  for (k in c(.25, .5, .75, 1)) symbols(0, 0, circles = k, inches = FALSE, add = TRUE, fg = "grey85")
  for (i in 1:12) {
    a <- seq(ang[i] + pi / 12, ang[i] - pi / 12, length.out = 20)
    polygon(c(0, r[i] * cos(a), 0), c(0, r[i] * sin(a), 0), col = cor, border = "white")
  }
  text(1.15 * cos(ang), 1.15 * sin(ang), meses, cex = 0.8)
}

grafico_circular <- function() {
  par(mfrow = c(1, 2), mar = c(0, 0, 2, 0))
  calendario_circular(calendario$flor,  cores["flor"],  "Floração")
  calendario_circular(calendario$fruto, cores["fruto"], "Frutificação")
  par(mfrow = c(1, 1))
}

grafico_barras()     # aparece na aba "Plots" do RStudio
grafico_circular()


# ---------------------------------------------------------------------
# 6. SALVAR OS RESULTADOS (para colar no relatório em Word)
# ---------------------------------------------------------------------
nome <- gsub("[^A-Za-z]+", "_", especie)
pasta <- file.path(dirname(arquivo), paste0("resultados_", nome))
dir.create(pasta, showWarnings = FALSE)

write.csv(calendario, file.path(pasta, "tabela_calendario.csv"), row.names = FALSE)
write.csv(as.data.frame(circular), file.path(pasta, "estatistica_circular.csv"))

png(file.path(pasta, "figura1_barras.png"), width = 2000, height = 1200, res = 250)
grafico_barras(); invisible(dev.off())
png(file.path(pasta, "figura2_circular.png"), width = 2200, height = 1150, res = 250)
grafico_circular(); invisible(dev.off())

cat("\n=====================================================\n")
cat("Pronto! Resultados salvos em:\n", pasta, "\n")
cat("Arquivos: tabela_calendario.csv, estatistica_circular.csv,\n")
cat("          figura1_barras.png, figura2_circular.png\n")
cat("=====================================================\n")
