# =====================================================================
# Gera o site do LEDT completo (português + inglês + espanhol) em docs/
#
# Forma mais simples: no RStudio, aba "Build" -> "Render Website".
# Ou rode no Console:  source("render_site.R")
#
# As versões em inglês e espanhol são geradas automaticamente pelo
# script _idiomas.R ao final de toda renderização completa.
# =====================================================================
if (requireNamespace("quarto", quietly = TRUE)) {
  quarto::quarto_render(as_job = FALSE)
} else {
  q <- Sys.getenv("RSTUDIO_QUARTO", Sys.which("quarto"))
  if (.Platform$OS.type == "windows") q <- utils::shortPathName(q)
  system2(q, "render")
}
