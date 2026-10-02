# ======================================================================
# Padronização de nomes científicos
# R Lab - Laboratório de Ecologia e Dendrologia Tropical (LEDT/UFAM)
# https://ledtufam.github.io/extensao/rlab/rotinas/nomes-cientificos.html
# ======================================================================


# ---- 1. Os dados ----
sp <- read.csv("dados/especies_oliveira_amaral2004.csv")
str(sp)
head(sp)


# ---- 2. Limpeza básica ----
sum(grepl("^\\s|\\s$", sp$nome_cientifico))      # nomes com espaço no início ou no fim

sp$nome_cientifico <- trimws(gsub("\\s+", " ", sp$nome_cientifico))
sp$status[is.na(sp$status)] <- ""

sp$nome <- ifelse(sp$rank %in% c("gênero", "família", "indeterminada"),
                  paste(sp$nome_cientifico, sp$status), sp$nome_cientifico)
sp$genero <- ifelse(sp$rank %in% c("espécie", "gênero"), sub(" .*", "", sp$nome_cientifico), NA)
exemplos <- c(which(sp$rank == "gênero")[1:2], which(sp$status == "cf.")[1],
              which(sp$rank %in% c("família", "indeterminada")))
sp[exemplos, c("familia", "nome", "rank", "status")]


# ---- Nomes repetidos ----
dup <- sp$nome[duplicated(sp$nome)]
sp[sp$nome %in% dup, c("familia", "nome")]


# ---- 3. Respondendo às perguntas da prática (lista original) ----
c(
  familias           = length(unique(sp$familia[sp$familia != "Indeterminada"])),
  generos            = length(unique(na.omit(sp$genero))),
  especies           = sum(sp$rank == "espécie"),
  ate_genero         = sum(sp$rank == "gênero"),
  ate_familia        = sum(sp$rank == "família"),
  indeterminadas     = sum(sp$rank == "indeterminada"),
  com_cf             = sum(sp$status == "cf."),
  com_aff            = sum(sp$status == "aff.")
)

riqueza <- sort(table(sp$familia), decreasing = TRUE)[1:15]
par(mar = c(4, 10, 1, 1))
barplot(rev(riqueza), horiz = TRUE, las = 1, col = "#78c2ad", border = NA,
        xlab = "Número de espécies")


# ---- 4. Validar os nomes no TNRS ----
install.packages("TNRS")
library(TNRS)

sp$busca <- gsub("\\s(cf|aff)\\.\\s", " ", sp$nome_cientifico)
nomes_especie <- unique(sp$busca[sp$rank == "espécie"])

res <- TNRS(nomes_especie, sources = c("wfo", "wcvp"))

res[, c("Name_submitted", "Name_matched", "Overall_score",
        "Taxonomic_status", "Accepted_name", "Accepted_family")]


# ---- Comparando com a lista original ----
comp <- merge(sp[sp$rank == "espécie", c("familia", "nome_cientifico", "busca")],
              res[, c("Name_submitted", "Name_matched", "Overall_score",
                      "Taxonomic_status", "Accepted_name", "Accepted_family")],
              by.x = "busca", by.y = "Name_submitted", all.x = TRUE)

# Quantas espécies tiveram o nome alterado desde 2004 (sinônimos)?
sinonimos <- subset(comp, Taxonomic_status == "Synonym", c(nome_cientifico, Accepted_name))
nrow(sinonimos); sinonimos

# Quais estavam escritas incorretamente?
subset(comp, Overall_score < 1, c(nome_cientifico, Name_matched, Overall_score))

# Quais não foram encontradas?
subset(comp, is.na(Accepted_name) | Accepted_name == "", nome_cientifico)

# Alguma espécie mudou de família?
mudou_familia <- subset(comp, familia != Accepted_family,
                        c(nome_cientifico, familia, Accepted_family))
table(paste(mudou_familia$familia, "→", mudou_familia$Accepted_family))

# Alguma mudou de gênero?
subset(comp, sub(" .*", "", busca) != sub(" .*", "", Accepted_name),
       c(nome_cientifico, Accepted_name))

write.csv(comp, "resultados/lista_padronizada.csv", row.names = FALSE)


# ---- 5. Conferir na Flora e Funga do Brasil ----
install.packages("flora")
library(flora)

ffb <- get.taxa(nomes_especie, life.form = TRUE, habitat = TRUE, domain = TRUE)
ffb[, c("original.search", "search.str", "family", "taxon.status", "notes")]


# ---- Boas práticas na escrita de nomes científicos ----

# ---- Referências ----
