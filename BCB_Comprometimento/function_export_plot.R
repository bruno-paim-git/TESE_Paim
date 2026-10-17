Sys.setenv(
  NO_PROXY = "127.0.0.1,localhost",
  no_proxy = "127.0.0.1,localhost"
)

options(chromote.timeout = 60)

exportar_plotly_png <- function(
    plot,
    file,
    width = 1600,
    height = 800,
    delay = 5
) {
  
  # Diretório de destino
  dir.create(
    dirname(file),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  # HTML temporário
  tmp_html <- tempfile(fileext = ".html")
  
  htmlwidgets::saveWidget(
    plot,
    tmp_html,
    selfcontained = TRUE
  )
  
  # Sessão Chrome
  b <- chromote::ChromoteSession$new()
  
  # Define tamanho da viewport
  b$Emulation$setDeviceMetricsOverride(
    width = width,
    height = height,
    deviceScaleFactor = 1,
    mobile = FALSE
  )
  
  # Abre HTML
  b$go_to(
    paste0(
      "file:///",
      normalizePath(tmp_html, winslash = "/")
    )
  )
  
  # Aguarda renderização
  Sys.sleep(delay)
  
  # Captura
  b$screenshot(
    file = file
  )
  
  # Fecha sessão
  b$close()
  
  # Remove HTML temporário
  unlink(tmp_html)
  
  invisible(file)
}
