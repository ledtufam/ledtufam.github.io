# =====================================================================
# Pós-renderização: gera as versões em inglês (docs/en) e espanhol (docs/es)
#
# Este script roda SOZINHO ao final de uma renderização completa do site
# (botão "Render Website" na aba Build do RStudio, ou `quarto render`
# no Terminal). Você não precisa chamá-lo.
#
# Ao renderizar só uma página (botão "Render" de um arquivo), ele não faz
# nada, para não deixar a pré-visualização lenta.
# =====================================================================

idiomas <- c("en", "es")
saida   <- Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR", "docs")
perfil  <- Sys.getenv("QUARTO_PROFILE")

# Só roda na renderização completa do perfil português
arquivos <- strsplit(Sys.getenv("QUARTO_PROJECT_OUTPUT_FILES"), "\n")[[1]]
completa <- Sys.getenv("QUARTO_PROJECT_RENDER_ALL") == "1" && length(arquivos) > 5
if (perfil %in% idiomas || !completa) quit(save = "no")

# Executável do Quarto (o mesmo que está rodando agora)
bin <- Sys.getenv("QUARTO_BIN_PATH")
quarto <- file.path(bin, if (.Platform$OS.type == "windows") "quarto.exe" else "quarto")
if (!file.exists(quarto)) quarto <- file.path(bin, "quarto")
if (!file.exists(quarto)) quarto <- Sys.which("quarto")
# no Windows, usa o caminho curto (sem espaços, ex.: C:/PROGRA~1/...)
if (.Platform$OS.type == "windows") quarto <- utils::shortPathName(quarto)

copiar_novos <- function(de, para) {
  for (a in list.files(de, recursive = TRUE, all.files = TRUE, no.. = TRUE)) {
    destino <- file.path(para, a)
    if (!file.exists(destino)) {
      dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
      file.copy(file.path(de, a), destino)
    }
  }
}

busca_arq <- file.path(saida, "search.json")
busca <- if (file.exists(busca_arq)) jsonlite::fromJSON(busca_arq, simplifyVector = FALSE) else list()

for (l in idiomas) {
  message("\n==> Gerando versão '", l, "'...")
  tmp <- file.path("_idiomas", l)
  unlink(tmp, recursive = TRUE)
  status <- system2(quarto, c("render", "--profile", l))
  if (!identical(as.integer(status), 0L)) stop("Erro ao gerar a versão '", l, "'")

  destino <- file.path(saida, l)
  unlink(destino, recursive = TRUE)
  dir.create(destino, recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(file.path(tmp, l), full.names = TRUE), destino, recursive = TRUE)

  extras <- setdiff(list.files(tmp), c(l, "search.json", "sitemap.xml", "listings.json", "index.html"))
  for (e in extras) {
    if (dir.exists(file.path(tmp, e))) copiar_novos(file.path(tmp, e), file.path(saida, e))
    else if (!file.exists(file.path(saida, e))) file.copy(file.path(tmp, e), file.path(saida, e))
  }
  sj <- file.path(tmp, "search.json")
  if (file.exists(sj)) busca <- c(busca, jsonlite::fromJSON(sj, simplifyVector = FALSE))
}

if (length(busca)) jsonlite::write_json(busca, busca_arq, auto_unbox = TRUE)
unlink("_idiomas", recursive = TRUE)
message("\n==> Versões em inglês e espanhol prontas em ", saida, "/en e ", saida, "/es")
