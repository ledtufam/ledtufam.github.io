# =====================================================================
# Lista de publicações do LEDT agrupada por ano, a partir do publicacoes.bib
# Usado em publicacoes.qmd (e nas versões en/ e es/). Só usa R básico.
#
# Para incluir uma publicação: acrescente a referência no publicacoes.bib
# (exportada do Zotero/Mendeley ou copiada do "Cite" da revista) e
# renderize o site. Tipos reconhecidos: @article, @book, @software,
# @misc, @incollection, @inproceedings, @techreport.
# =====================================================================

# Membros do LEDT destacados em negrito (sobrenome + inicial)
# (último sobrenome, primeira inicial). Atualize quando entrar alguém novo.
ledt_membros <- c("Vasconcelos, C", "Durgante, F", "Cunha, K", "Chaves, S",
                  "Guabiraba-Ribeiro, G", "Ribeiro, G", "Arruda, Y",
                  "Ferro, M", "Mayo, A", "Sá, A", "Melo, A", "Neto, H",
                  "Ferreira, E", "Guedes, Y", "Ataide, C")

.textos <- list(
  pt = list(total = "publicações", preprint = "Preprint", article = "Artigo",
            book = "Livro/Relatório", software = "Software/Dados", guide = "Guia de campo",
            other = "Outro", all = "Todas", link = "Acessar"),
  en = list(total = "publications", preprint = "Preprint", article = "Article",
            book = "Book/Report", software = "Software/Data", guide = "Field guide",
            other = "Other", all = "All", link = "View"),
  es = list(total = "publicaciones", preprint = "Preprint", article = "Artículo",
            book = "Libro/Informe", software = "Software/Datos", guide = "Guía de campo",
            other = "Otro", all = "Todas", link = "Ver")
)

# --- leitura do .bib (parser simples, com chaves balanceadas) ----------
.ler_valor <- function(s, i) {
  ch <- substr(s, i, i)
  if (ch == "{") {
    nivel <- 0; j <- i
    repeat {
      c2 <- substr(s, j, j)
      if (c2 == "{") nivel <- nivel + 1
      if (c2 == "}") { nivel <- nivel - 1; if (nivel == 0) break }
      j <- j + 1
    }
    list(valor = substr(s, i + 1, j - 1), fim = j)
  } else if (ch == '"') {
    j <- regexpr('"', substr(s, i + 1, nchar(s)))[1] + i
    list(valor = substr(s, i + 1, j - 1), fim = j)
  } else {
    m <- regexpr("^[^,}]+", substr(s, i, nchar(s)))
    list(valor = trimws(regmatches(substr(s, i, nchar(s)), m)), fim = i + attr(m, "match.length") - 1)
  }
}

.limpar_latex <- function(x) {
  acentos <- c("\\\\'\\{?a\\}?" = "á", "\\\\'\\{?e\\}?" = "é", "\\\\'\\{?i\\}?" = "í",
               "\\\\'\\{?o\\}?" = "ó", "\\\\'\\{?u\\}?" = "ú", "\\\\'\\{?A\\}?" = "Á",
               "\\\\'\\{?E\\}?" = "É", "\\\\~\\{?a\\}?" = "ã", "\\\\~\\{?o\\}?" = "õ",
               "\\\\~\\{?n\\}?" = "ñ", '\\\\"\\{?o\\}?' = "ö", '\\\\"\\{?a\\}?' = "ä",
               '\\\\"\\{?e\\}?' = "ë", "\\\\\\^\\{?e\\}?" = "ê", "\\\\c\\{?c\\}?" = "ç",
               "\\\\aa" = "å", "\\\\o" = "ø", "\\\\L" = "Ł", "\\\\v\\{?c\\}?" = "č")
  for (k in names(acentos)) x <- gsub(k, acentos[[k]], x)
  x <- gsub("[{}]", "", x)
  gsub("\\s+", " ", trimws(x))
}

ler_bib <- function(arquivo) {
  s <- paste(readLines(arquivo, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  inicios <- gregexpr("@[A-Za-z]+\\s*\\{", s)[[1]]
  lapply(inicios, function(i0) {
    tipo <- tolower(sub("@([A-Za-z]+).*", "\\1", substr(s, i0, i0 + 20)))
    abre <- regexpr("\\{", substr(s, i0, nchar(s)))[1] + i0 - 1
    corpo <- .ler_valor(s, abre)$valor
    chave <- trimws(sub(",.*", "", corpo))
    resto <- sub("^[^,]*,", "", corpo)
    campos <- list(); i <- 1
    while (TRUE) {
      m <- regexpr("[A-Za-z_-]+\\s*=\\s*", substr(resto, i, nchar(resto)))
      if (m[1] < 0) break
      nome <- tolower(trimws(sub("\\s*=.*", "", regmatches(substr(resto, i, nchar(resto)), m))))
      pos <- i + m[1] - 1 + attr(m, "match.length")
      v <- .ler_valor(resto, pos)
      campos[[nome]] <- v$valor
      i <- v$fim + 1
    }
    c(list(tipo = tipo, chave = chave), campos)
  })
}

# --- formatação --------------------------------------------------------
.autor <- function(a) {
  a <- .limpar_latex(a)
  if (grepl(",", a)) {
    fam <- trimws(sub(",.*", "", a)); dado <- trimws(sub("^[^,]*,", "", a))
  } else {
    p <- strsplit(a, " ")[[1]]
    k <- length(p)
    while (k > 2 && grepl("^(da|de|do|dos|das|van|von|del|la)$", p[k - 1])) k <- k - 1
    fam <- paste(p[k:length(p)], collapse = " "); dado <- paste(p[seq_len(k - 1)], collapse = " ")
  }
  ini <- paste0(substr(strsplit(gsub("[.-]", " ", dado), " +")[[1]], 1, 1), collapse = ".")
  nome <- if (nzchar(ini)) paste0(fam, ", ", ini, ".") else fam
  ultimo <- tail(strsplit(fam, " ")[[1]], 1)
  membro <- paste0(ultimo, ", ", substr(ini, 1, 1)) %in% ledt_membros
  if (membro) paste0("**", nome, "**") else nome
}

.autores <- function(x) {
  if (is.null(x)) return("")
  aa <- trimws(strsplit(gsub("\\s+", " ", x), " and ")[[1]])
  f <- vapply(aa, .autor, "")
  if (length(f) > 10) {
    membros <- f[-(1:3)][grepl("^\\*\\*", f[-(1:3)])]
    f <- c(f[1:3], if (length(membros)) c("…", membros), "et al.")
  }
  paste(f, collapse = "; ")
}

.categoria <- function(e) {
  rev <- tolower(paste(e$journal, e$publisher))
  if (grepl("rxiv|preprint|preprints|research square|ssrn", rev)) return("preprint")
  if (e$tipo %in% c("software", "dataset", "data")) return("software")
  if (grepl("field museum|field guide|guia", rev)) return("guide")
  if (e$tipo == "article") return("article")
  if (e$tipo %in% c("book", "techreport", "incollection", "inbook")) return("book")
  "other"
}

publicacoes_html <- function(arquivo = "publicacoes/publicacoes.bib", lang = "pt") {
  tx <- .textos[[lang]]
  bib <- ler_bib(arquivo)
  anos <- vapply(bib, function(e) as.integer(.limpar_latex(e$year %||% "0")), 1L)
  bib <- bib[order(-anos, vapply(bib, function(e) .limpar_latex(e$title %||% ""), ""))]
  anos <- sort(anos, decreasing = TRUE)
  cats <- vapply(bib, .categoria, "")

  out <- c(sprintf("<p class='pub-total'>%d %s</p>", length(bib), tx$total),
           "<div class='pub-filtros'>",
           sprintf("<button class='btn btn-sm btn-success' data-cat='all'>%s</button>", tx$all))
  for (cc in intersect(c("article", "preprint", "book", "guide", "software", "other"), cats))
    out <- c(out, sprintf("<button class='btn btn-sm btn-outline-success' data-cat='%s'>%s (%d)</button>",
                          cc, tx[[cc]], sum(cats == cc)))
  out <- c(out, "</div>", "")

  for (a in unique(anos)) {
    out <- c(out, sprintf("## %d {.pub-ano}", a), "")
    for (k in which(anos == a)) {
      e <- bib[[k]]; cat_k <- cats[k]
      titulo <- .limpar_latex(e$title %||% "")
      veiculo <- .limpar_latex(e$journal %||% e$booktitle %||% e$publisher %||% "")
      detalhes <- c(if (!is.null(e$volume)) paste0(e$volume, if (!is.null(e$number)) paste0("(", e$number, ")")),
                    if (!is.null(e$pages)) e$pages, if (!is.null(e$version)) paste0("v", e$version))
      doi <- if (!is.null(e$doi)) .limpar_latex(e$doi) else NULL
      url <- if (!is.null(doi)) paste0("https://doi.org/", doi) else if (!is.null(e$url)) gsub("\\s", "", e$url) else NULL
      ref <- paste0(.autores(e$author), " (", a, "). ", titulo, ". *", veiculo, "*",
                    if (length(detalhes)) paste0(", ", paste(detalhes, collapse = ", ")), ".")
      out <- c(out,
        sprintf("::: {.pub-item data-cat='%s'}", cat_k),
        sprintf("[%s]{.badge .pub-tipo .pub-%s} %s", tx[[cat_k]], cat_k, ref),
        "",
        paste0("[",
          if (!is.null(url)) sprintf("<a href='%s' target='_blank' class='btn btn-outline-success btn-sm'><i class='bi bi-box-arrow-up-right'></i> %s</a> ", url, if (!is.null(doi)) paste0("DOI: ", doi) else tx$link),
          if (!is.null(doi)) sprintf("<span class='altmetric-embed' data-badge-type='2' data-doi='%s' data-hide-no-mentions='true' data-badge-popover='left'></span> <span class='__dimensions_badge_embed__' data-doi='%s' data-style='small_rectangle' data-hide-zero-citations='true'></span>", doi, doi),
          "]{.pub-links}"),
        ":::", "")
    }
  }
  out <- c(out,
    "<script src='https://d1bxh8uas1mnw7.cloudfront.net/assets/embed.js'></script>",
    "<script async src='https://badge.dimensions.ai/badge.js' charset='utf-8'></script>",
    "<script>",
    "document.querySelectorAll('.pub-filtros button').forEach(function(b){b.addEventListener('click',function(){",
    "  var c=b.dataset.cat; document.querySelectorAll('.pub-filtros button').forEach(function(x){x.className='btn btn-sm '+(x===b?'btn-success':'btn-outline-success');});",
    "  document.querySelectorAll('.pub-item').forEach(function(i){i.style.display=(c==='all'||i.dataset.cat===c)?'':'none';});",
    "  document.querySelectorAll('h2.pub-ano').forEach(function(h){var s=h.closest('section')||h.parentElement; var vis=s.querySelectorAll('.pub-item:not([style*=\"none\"])').length; s.style.display=vis?'':'none';});",
    "});});",
    "</script>")
  cat(out, sep = "\n")
}

`%||%` <- function(a, b) if (is.null(a)) b else a
