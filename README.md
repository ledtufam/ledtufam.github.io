Website oficial do **Laboratório de Ecologia e Dendrologia Tropical (LEDT)**, vinculado à Universidade Federal do Amazonas (UFAM) e coordenado pela Profa. Caroline C. Vasconcelos.

## Como atualizar o site

- **Português:** páginas `.qmd` na raiz e nas pastas `ensino/`, `extensao/`, `divulgacao/`.
- **Inglês / Espanhol:** páginas traduzidas nas pastas `en/` e `es/` (mesmo nome do arquivo em português).
- **Menu de cada idioma:** `_quarto-pt.yml`, `_quarto-en.yml`, `_quarto-es.yml`. Configurações comuns: `_quarto.yml`.
- **Nova notícia:** crie `divulgacao/posts/AAAA-MM-DD-titulo/index.qmd`.
- **Nova rotina do R Lab:** copie `extensao/rlab/_modelo-rotina.qmd` para `extensao/rlab/rotinas/` e coloque o script `.R` em `extensao/rlab/scripts/`.

- **Nova publicação:** acrescente a referência em `publicacoes/publicacoes.bib` (a página se organiza sozinha por ano).

**Para publicar:** no RStudio, aba **Build → Render Website** (ou `source("render_site.R")`). O script `_idiomas.R` gera sozinho as versões em `docs/en` e `docs/es`. Depois faça commit e push.

O botão **Render** de uma página só serve para pré-visualizar; ele não atualiza as versões em inglês e espanhol.
