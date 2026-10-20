# =====================================================================
#  PREPARAR OS DATASETS DA ATIVIDADE DE FENOLOGIA (uso da professora)
#
#  Lê os arquivos baixados do speciesLink que estão em "brutos/" e gera
#  um CSV limpo por espécie nesta pasta (dados/fenologia/), sempre com
#  as mesmas colunas e em UTF-8. São esses CSVs que os alunos baixam.
#
#  Como usar: abra o projeto do site (ledt.Rproj) e rode
#     source("extensao/rlab/dados/fenologia/_preparar-datasets.R")
#  Só usa o R básico.
# =====================================================================

pasta  <- "extensao/rlab/dados/fenologia"
brutos <- list.files(file.path(pasta, "brutos"), full.names = TRUE,
                     pattern = "\\.(txt|csv|tsv)$", ignore.case = TRUE)
if (length(brutos) == 0) stop("Coloque os arquivos do speciesLink em ", file.path(pasta, "brutos"))

# sinônimos que devem ser juntados ao nome aceito
sinonimos <- c("Tabebuia serratifolia" = "Handroanthus serratifolius")

# ---------------------------------------------------------------------
ler_bruto <- function(f) {
  # codificação: UTF-8 se o texto for válido, senão latin1
  linhas <- readLines(f, n = 2000, warn = FALSE, encoding = "UTF-8")
  enc <- if (all(validUTF8(linhas))) "UTF-8" else "latin1"
  cab <- readLines(f, n = 1, warn = FALSE)
  sep <- c("\t", ";", ",")[which.max(c(lengths(regmatches(cab, gregexpr("\t", cab))),
                                       lengths(regmatches(cab, gregexpr(";",  cab))),
                                       lengths(regmatches(cab, gregexpr(",",  cab)))))]
  d <- read.table(f, sep = sep, header = TRUE, quote = if (sep == "\t") "" else "\"",
                  fill = TRUE, comment.char = "", stringsAsFactors = FALSE,
                  colClasses = "character", encoding = enc, check.names = FALSE)
  d[] <- lapply(d, enc2utf8)
  # nomes de colunas simplificados: minúsculas, sem acento, sem símbolos
  nm <- tolower(iconv(enc2utf8(names(d)), from = "UTF-8", to = "ASCII//TRANSLIT"))
  names(d) <- gsub("[^a-z0-9]", "", nm)
  message(basename(f), ": ", nrow(d), " linhas, separador '", sep, "', ", enc)
  d
}

pega <- function(d, opcoes) {          # primeira coluna que existir
  achou <- intersect(opcoes, names(d))
  if (length(achou)) d[[achou[1]]] else rep(NA_character_, nrow(d))
}

padronizar <- function(d) {
  mes <- suppressWarnings(as.integer(pega(d, c("monthcollected", "month", "mes", "mescoleta"))))
  if (all(is.na(mes))) {               # tenta tirar o mês de uma coluna de data
    data <- pega(d, c("eventdate", "datecollected", "data", "datacoleta"))
    mes <- suppressWarnings(as.integer(sub("^\\d{4}-(\\d{1,2}).*$", "\\1", data)))
  }
  # todas as colunas de anotações/observações viram um texto só
  cols_txt <- grep("note|notas|remark|observ|descri|phenolog|fenolog|reproductive", names(d), value = TRUE)
  anot <- if (length(cols_txt)) apply(d[cols_txt], 1, function(x) paste(x[!is.na(x) & x != ""], collapse = " | ")) else ""
  esp <- trimws(pega(d, c("scientificname", "nomecientifico", "species")))
  esp <- sub("^(\\S+\\s+\\S+).*$", "\\1", esp)        # só gênero + epíteto
  esp <- ifelse(esp %in% names(sinonimos), sinonimos[esp], esp)
  data.frame(
    especie   = esp,
    herbario  = pega(d, c("collectioncode", "institutioncode", "acronimo")),
    catalogo  = pega(d, c("catalognumber", "barcode")),
    coletor   = pega(d, c("collector", "recordedby", "coletor")),
    numero    = pega(d, c("collectornumber", "recordnumber", "fieldnumber", "numero")),
    ano       = suppressWarnings(as.integer(pega(d, c("yearcollected", "year", "ano")))),
    mes       = mes,
    estado    = pega(d, c("stateprovince", "estado")),
    municipio = pega(d, c("county", "municipality", "municipio")),
    anotacoes = anot,
    stringsAsFactors = FALSE
  )
}

tudo <- do.call(rbind, lapply(brutos, function(f) padronizar(ler_bruto(f))))
tudo <- unique(tudo)                                     # remove linhas repetidas
tudo$anotacoes <- gsub("[\r\n\t]+", " ", tudo$anotacoes)

cat("\nRegistros por espécie:\n"); print(sort(table(tudo$especie), decreasing = TRUE))
alvo <- names(which(table(tudo$especie) >= 100))         # ignora nomes raros/erros

for (sp in alvo) {
  d <- tudo[tudo$especie == sp, ]
  arq <- file.path(pasta, paste0(gsub(" ", "_", tolower(sp)), ".csv"))
  con <- file(arq, open = "w", encoding = "UTF-8")
  cat("\ufeff", file = con)                              # BOM: acentos certos no Excel
  write.csv(d, con, row.names = FALSE, na = "")
  close(con)
  cat(sprintf("%-30s %5d registros | com mês: %5d | com anotação: %5d -> %s\n", sp, nrow(d),
              sum(!is.na(d$mes)), sum(nchar(d$anotacoes) > 0), basename(arq)))
}
