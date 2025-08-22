
library(shiny)
library(bslib)
library(DT)
library(readr)
library(readxl)
library(shinycssloaders)
library(samplesimil)

read_any <- function(path, delim = ",", sheet = NULL) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("csv")) {
    out <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  } else if (ext %in% c("txt")) {
    out <- readr::read_delim(path, delim = delim, show_col_types = FALSE, progress = FALSE)
  } else if (ext %in% c("xlsx")) {
    sh <- if (is.null(sheet)) 1 else sheet
    out <- readxl::read_excel(path, sheet = sh)
  } else {
    stop("Formato no soportado: ", ext)
  }
  as.data.frame(out, stringsAsFactors = FALSE)
}

sanitize_num <- function(x, lower = -Inf, upper = Inf, default = NA_real_) {
  if (!is.numeric(x) || length(x) == 0 || !is.finite(x)) return(default)
  x <- as.numeric(x)[1]
  x <- max(min(x, upper), lower)
  x
}

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  titlePanel("samplesimil — Censo con cobertura incompleta: aleatoriedad y precisión"),
  sidebarLayout(
    sidebarPanel(
      h4("Modo de datos"),
      radioButtons("modo", NULL, choices = c("Cargar archivos"="cargar", "Simular ejemplo"="simular"),
                   inline = TRUE, selected="simular"),
      conditionalPanel('input.modo=="cargar"',
        fileInput("file_marco",   "Marco (id)", accept = c(".csv",".txt",".xlsx")),
        fileInput("file_piloto",  "Piloto (id, score opcional)", accept = c(".csv",".txt",".xlsx")),
        fileInput("file_muestra", "Muestra (id, score opcional)", accept = c(".csv",".txt",".xlsx")),
        textInput("id_col", "Nombre columna ID", value = "id"),
        textInput("score_col", "Nombre columna score (opcional)", value = "score"),
        radioButtons("delim", "Separador TXT", choices = c(",",";","\\t"), inline = TRUE, selected = ","),
        numericInput("sheet", "Hoja (XLSX)", value = 1, min = 1, step = 1)
      ),
      conditionalPanel('input.modo=="simular"',
        numericInput("N", "N del marco", value = 115, min = 10, step = 5),
        numericInput("n_pil", "n piloto", value = 30, min = 5, step = 5),
        numericInput("n_mue", "n muestra", value = 60, min = 10, step = 5),
        checkboxInput("incluye_score", "Incluir score sintético (0..100)", value = TRUE)
      ),
      hr(),
      h4("Parámetros de análisis"),
      helpText("B mayor = nulas más suaves (más lentas). K define bloques en chi²."),
      numericInput("K", "K bloques (chi²)", value = 10, min = 4, step = 1),
      numericInput("B", "Réplicas Monte Carlo", value = 5000, min = 1000, step = 500),
      hr(),
      h4("Precisión (media, CPF)"),
      helpText("Si no hay 'score' en piloto, se usa S²_ref = (U−L)²/4."),
      numericInput("alpha", "Nivel (α)", value = 0.05, min = 0.001, max = 0.49, step = 0.01),
      numericInput("EFD", "Efecto de diseño (≥1)", value = 1.3, min = 1, step = 0.05),
      numericInput("TR", "Tasa de respuesta TR (0-1)", value = 0.80, min = 0.01, max = 0.99, step = 0.01),
      numericInput("EA", "Semiancho objetivo (EA)", value = 2.0, min = 0.0001, step = 0.1),
      numericInput("L", "Límite inferior Y (si no hay piloto)", value = 0, step = 1),
      numericInput("U", "Límite superior Y (si no hay piloto)", value = 100, step = 1),
      numericInput("semilla", "Semilla", value = 20250813, step = 1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Resumen",
          h5("Planteo"),
          p("Escenario: Censo con cobertura incompleta. Se audita el ", strong("mecanismo de selección"),
            " sobre el orden del marco (IDs) y se planifica/evalúa la ", strong("precisión de la media"),
            " con CPF + EFD + TR, usando S² de piloto o acotamientos."),
          fluidRow(
            column(6, wellPanel(h5("Aleatoriedad — p-valores (Monte Carlo)"),
              tableOutput("tabla_p"),
              p(em("Regla:")," p ≥ 0.05 ⇒ patrón compatible con SRSWOR en el orden del marco.")
            )),
            column(6, wellPanel(h5("Plan de precisión (EA)"),
              tableOutput("tabla_plan"),
              tableOutput("tabla_ci"),
              textOutput("texto_precision")
            ))
          )
        ),
        tabPanel("Aleatoriedad",
          h5("Cobertura, bloques, nulas, ECDF y QQ de posiciones"),
          fluidRow(
            column(6, shinycssloaders::withSpinner(plotOutput("plot_cover_pil", height = "240px"))),
            column(6, shinycssloaders::withSpinner(plotOutput("plot_cover_mue", height = "240px")))
          ),
          fluidRow(
            column(6, shinycssloaders::withSpinner(plotOutput("plot_blocks_pil", height = "240px"))),
            column(6, shinycssloaders::withSpinner(plotOutput("plot_blocks_mue", height = "240px")))
          ),
          fluidRow(
            column(6, shinycssloaders::withSpinner(plotOutput("plot_runs_mue", height = "240px"))),
            column(6, shinycssloaders::withSpinner(plotOutput("plot_gap_mue", height = "240px")))
          ),
          fluidRow(
            column(6, shinycssloaders::withSpinner(plotOutput("plot_ecdf_pil", height = "240px"))),
            column(6, shinycssloaders::withSpinner(plotOutput("plot_ecdf_mue", height = "240px")))
          ),
          fluidRow(
            column(6, shinycssloaders::withSpinner(plotOutput("plot_qq_pil", height = "240px"))),
            column(6, shinycssloaders::withSpinner(plotOutput("plot_qq_mue", height = "240px")))
          )
        ),
        tabPanel("Precisión",
          h5("Curva semiancho IC95% (media) vs n"),
          p("Líneas: ", strong("n_actual"), " (muestra) y ", strong("n_objetivo"),
            ". Línea roja horizontal: ", strong("EA"), "."),
          shinycssloaders::withSpinner(plotOutput("plot_precision", height = "400px"))
        ),
        tabPanel("Calculadoras",
          h5("Invertir fórmulas (CPF)"),
          fluidRow(
            column(6, wellPanel(
              h6("1) Dado n y α ⇒ error alcanzable"),
              numericInput("calc_n1", "n (muestra)", value = 60, min = 2, step = 1),
              numericInput("calc_alpha1", "α (nivel)", value = 0.05, min = 0.001, max = 0.49, step = 0.01),
              textInput("calc_mu1", "μ_ref (para error relativo, opcional)", value = ""),
              actionButton("btn_calc1", "Calcular"),
              tableOutput("out_calc1")
            )),
            column(6, wellPanel(
              h6("2) Dado n y E_abs / d ⇒ confianza alcanzable"),
              numericInput("calc_n2", "n (muestra)", value = 60, min = 2, step = 1),
              textInput("calc_eabs2", "E_abs (unidades) — opcional si usa d", value = ""),
              textInput("calc_d2", "d (error relativo, p.ej. 0.1) — opcional", value = ""),
              textInput("calc_mu2", "μ_ref (si usa d)", value = ""),
              actionButton("btn_calc2", "Calcular"),
              tableOutput("out_calc2")
            ))
          ),
          fluidRow(
            column(12, wellPanel(
              h6("3) Dado EA / d y α ⇒ n objetivo y n a contactar"),
              numericInput("calc_alpha3", "α (nivel)", value = 0.05, min = 0.001, max = 0.49, step = 0.01),
              textInput("calc_eabs3", "EA (unidades) — opcional si usa d", value = ""),
              textInput("calc_d3", "d (error relativo, p.ej. 0.1) — opcional", value = ""),
              textInput("calc_mu3", "μ_ref (si usa d)", value = ""),
              numericInput("calc_TR3", "TR (tasa de respuesta)", value = 0.80, min = 0.01, max = 0.99, step = 0.01),
              actionButton("btn_calc3", "Calcular"),
              tableOutput("out_calc3")
            ))
          ),
          helpText("Nota: Todas las calculadoras usan N, S², EFD del panel izquierdo y, si corresponde, μ_ref.")
        ),
        tabPanel("Datos",
          h5("Piloto"), DTOutput("tbl_pil"),
          h5("Muestra"), DTOutput("tbl_mue")
        ),
        tabPanel("Reporte",
          h5("Descarga de reporte (HTML)"),
          downloadButton("dl_report_html", "Descargar reporte (HTML)"),
          br(), br(),
          textOutput("help_pandoc")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  read_delim_choice <- function(sym) if (sym == "\\t") "\t" else sym

  datos <- reactive({
    set.seed(input$semilla)
    if (input$modo == "simular") {
      N <- as.integer(input$N)
      ids_marco <- sprintf("ESC-%04d", 1:N)
      id_pil <- sort(sample.int(N, size = as.integer(input$n_pil), replace = FALSE))
      id_mue <- sort(sample.int(N, size = as.integer(input$n_mue), replace = FALSE))
      if (isTRUE(input$incluye_score)) {
        y_all <- round(100 * stats::rbeta(N, 2.2, 2.8), 2)
        piloto <- data.frame(id = ids_marco[id_pil],  score = y_all[id_pil])
        muestra <- data.frame(id = ids_marco[id_mue], score = y_all[id_mue])
      } else {
        piloto <- data.frame(id = ids_marco[id_pil])
        muestra <- data.frame(id = ids_marco[id_mue])
      }
      marco <- data.frame(id = ids_marco)
      list(marco=marco, piloto=piloto, muestra=muestra, id_col="id", score_col="score")
    } else {
      validate(
        need(!is.null(input$file_marco),   "Suba el archivo del MARCO"),
        need(!is.null(input$file_piloto),  "Suba el archivo de la PILOTO"),
        need(!is.null(input$file_muestra), "Suba el archivo de la MUESTRA")
      )
      d <- read_delim_choice(input$delim)
      marco   <- read_any(input$file_marco$datapath, delim = d, sheet = input$sheet)
      piloto  <- read_any(input$file_piloto$datapath, delim = d, sheet = input$sheet)
      muestra <- read_any(input$file_muestra$datapath, delim = d, sheet = input$sheet)
      list(marco=as.data.frame(marco), piloto=as.data.frame(piloto), muestra=as.data.frame(muestra),
           id_col=input$id_col, score_col=input$score_col)
    }
  })

  analisis <- reactive({
    d <- datos(); req(d)
    marco <- d$marco; piloto <- d$piloto; muestra <- d$muestra
    id_col <- d$id_col; score_col <- d$score_col
    K <- as.integer(input$K); B <- as.integer(input$B)

    stopifnot(id_col %in% names(marco), id_col %in% names(piloto), id_col %in% names(muestra))
    ids_marco   <- as.character(marco[[id_col]])
    ids_piloto  <- as.character(piloto[[id_col]])
    ids_muestra <- as.character(muestra[[id_col]])
    N <- length(ids_marco)

    pos_pil <- samplesimil::map_a_pos(ids_piloto, ids_marco)
    pos_mue <- samplesimil::map_a_pos(ids_muestra, ids_marco)

    diag_pil <- samplesimil::diag_posiciones(N, pos_pil, K = K, B = B, semilla = input$semilla)
    diag_mue <- samplesimil::diag_posiciones(N, pos_mue, K = K, B = B, semilla = input$semilla)

    has_score <- (!is.null(score_col)) && (score_col %in% names(piloto)) && (score_col %in% names(muestra))
    if (has_score) {
      Y_pil <- suppressWarnings(as.numeric(piloto[[score_col]])); Y_pil <- Y_pil[is.finite(Y_pil)]
      S2_ref <- tryCatch(stats::var(Y_pil), error=function(e) NA_real_)
      mu_ref <- tryCatch(mean(Y_pil), error=function(e) NA_real_)
      fuente_S2 <- "Varianza y media de la prueba piloto"
    } else {
      S2_ref <- NA_real_; mu_ref <- NA_real_; fuente_S2 <- "Acotamiento [L,U]"
    }
    if (!is.finite(S2_ref) || S2_ref <= 0) {
      L <- sanitize_num(input$L, lower=-1e9, upper=1e9, default=0)
      U <- sanitize_num(input$U, lower=-1e9, upper=1e9, default=100)
      if (!is.finite(L) || !is.finite(U) || U <= L) {
        S2_ref <- 1.0; fuente_S2 <- "Fallback: S2_ref=1 (parámetros inválidos)"
      } else {
        S2_ref <- (U - L)^2 / 4
        if (!is.finite(mu_ref)) mu_ref <- (L + U)/2
        fuente_S2 <- sprintf("Acotamiento [%.3g, %.3g] (μ_ref ≈ media del intervalo)", L, U)
      }
    }

    alpha <- sanitize_num(input$alpha, lower=1e-6, upper=0.49, default=0.05)
    EFD   <- sanitize_num(input$EFD,   lower=1.0,  upper=100,  default=1.3)
    TR    <- sanitize_num(input$TR,    lower=1e-6, upper=0.999999, default=0.8)
    EA    <- sanitize_num(input$EA,    lower=1e-6, upper=1e9,  default=2.0)

    plan <- samplesimil::plan_n(N, S2 = S2_ref, EA = EA, alpha = alpha, EFD = EFD, TR = TR)
    n_actual <- length(pos_mue)
    semiancho_actual <- samplesimil::semiancho_media(N, S2_ref, n_actual, alpha, EFD)

    list(N=N, pos_pil=pos_pil, pos_mue=pos_mue,
         diag_pil=diag_pil, diag_mue=diag_mue,
         has_score = has_score, piloto=piloto, muestra=muestra, score_col=score_col,
         S2_ref=S2_ref, mu_ref=mu_ref, fuente_S2=fuente_S2, plan=plan,
         alpha=alpha, EFD=EFD, TR=TR, EA=EA,
         n_actual=n_actual, semiancho_actual=semiancho_actual)
  })

  output$tabla_p <- renderTable({
    ap <- analisis(); req(ap)
    rbind(
      cbind(Conjunto="Piloto",  t(round(ap$diag_pil$pvals,4)), p_Fisher = round(ap$diag_pil$p_fisher,4)),
      cbind(Conjunto="Muestra", t(round(ap$diag_mue$pvals,4)), p_Fisher = round(ap$diag_mue$p_fisher,4))
    )
  }, rownames = FALSE)

  output$tabla_plan <- renderTable({
    ap <- analisis(); req(ap)
    data.frame(
      N = ap$N,
      S2_referencia = round(ap$S2_ref,3),
      mu_ref = round(ap$mu_ref,3),
      fuente_S2 = ap$fuente_S2,
      alpha = ap$alpha, EFD = ap$EFD, TR = ap$TR, EA = ap$EA,
      n_actual = ap$n_actual,
      n_objetivo = ap$plan$n_objetivo,
      n_contacto = ap$plan$n_contacto,
      semiancho_en_n_actual = round(ap$semiancho_actual,3)
    )
  }, rownames = FALSE)

  output$tabla_ci <- renderTable({
    ap <- analisis(); req(ap)
    sem_obj <- samplesimil::semiancho_media(ap$N, ap$S2_ref, ap$plan$n_objetivo, ap$alpha, ap$EFD)
    delta <- ap$semiancho_actual - sem_obj
    ratio <- ap$semiancho_actual / sem_obj
    data.frame(
      Indicador = c("Semiancho actual", "Semiancho en n_obj", "Diferencia (actual - objetivo)", "Razón (actual/objetivo)"),
      Valor = round(c(ap$semiancho_actual, sem_obj, delta, ratio), 3)
    )
  }, rownames = FALSE)

  output$texto_precision <- renderText({
    ap <- analisis(); req(ap)
    ok <- ap$semiancho_actual <= ap$EA
    paste0(
      "Con n_actual = ", ap$n_actual, 
      ", el semiancho del IC ", 100*(1-ap$alpha), "% de la media es ", round(ap$semiancho_actual, 3),
      " (EA objetivo = ", ap$EA, "). ",
      if (ok) "Ya se cumple el criterio de precisión." else "Aún no se cumple; aumentar n o revisar EFD/TR/S²."
    )
  })

  # Gráficos Aleatoriedad
  output$plot_cover_pil <- renderPlot({
    ap <- analisis(); req(ap)
    plot(1:ap$N, rep(0, ap$N), type="n", yaxt="n", xlab="ID (1..N)", ylab="", main="Cobertura PILOTO")
    points(ap$pos_pil, rep(0, length(ap$pos_pil)), pch=15, col="red")
  })
  output$plot_cover_mue <- renderPlot({
    ap <- analisis(); req(ap)
    plot(1:ap$N, rep(0, ap$N), type="n", yaxt="n", xlab="ID (1..N)", ylab="", main="Cobertura MUESTRA")
    points(ap$pos_mue, rep(0, length(ap$pos_mue)), pch=15, col="red")
  })
  output$plot_blocks_pil <- renderPlot({
    ap <- analisis(); req(ap)
    barplot(ap$diag_pil$bloques_obs, main="Bloques PILOTO", xlab="Bloque", ylab="Frecuencia")
    abline(h = ap$diag_pil$n / ap$diag_pil$K, col="blue", lty=2, lwd=2)
  })
  output$plot_blocks_mue <- renderPlot({
    ap <- analisis(); req(ap)
    barplot(ap$diag_mue$bloques_obs, main="Bloques MUESTRA", xlab="Bloque", ylab="Frecuencia")
    abline(h = ap$diag_mue$n / ap$diag_mue$K, col="blue", lty=2, lwd=2)
  })
  output$plot_runs_mue <- renderPlot({
    ap <- analisis(); req(ap)
    hist(ap$diag_mue$null$runs, breaks=40, col="gray85", border="white",
         main="Nula de rachas (MUESTRA)", xlab="N° rachas")
    abline(v = ap$diag_mue$T_obs["runs"], col="red", lwd=2)
  })
  output$plot_gap_mue <- renderPlot({
    ap <- analisis(); req(ap)
    hist(ap$diag_mue$null$gap, breaks=40, col="gray85", border="white",
         main="Nula de brecha (MUESTRA)", xlab="Máxima brecha")
    abline(v = ap$diag_mue$T_obs["gap"], col="red", lwd=2)
  })
  output$plot_ecdf_pil <- renderPlot({
    ap <- analisis(); req(ap)
    plot(ecdf(ap$pos_pil/ap$N), verticals = TRUE, do.points = FALSE, col = "black", lwd = 2,
         main = "ECDF PILOTO vs Uniforme", xlab = "u = pos/N", ylab = "F(u)")
    abline(0,1,col="red",lty=2,lwd=2)
  })
  output$plot_ecdf_mue <- renderPlot({
    ap <- analisis(); req(ap)
    plot(ecdf(ap$pos_mue/ap$N), verticals = TRUE, do.points = FALSE, col = "black", lwd = 2,
         main = "ECDF MUESTRA vs Uniforme", xlab = "u = pos/N", ylab = "F(u)")
    abline(0,1,col="red",lty=2,lwd=2)
  })
  output$plot_qq_pil <- renderPlot({
    ap <- analisis(); req(ap)
    k <- seq_along(ap$pos_pil); exp_pos <- (k * (ap$N + 1)) / (length(ap$pos_pil) + 1)
    plot(exp_pos, ap$pos_pil, pch=19, cex=.6, xlab="Posición esperada", ylab="Observada",
         main="QQ posiciones (PILOTO)")
    abline(0,1,col="blue",lty=2,lwd=2)
  })
  output$plot_qq_mue <- renderPlot({
    ap <- analisis(); req(ap)
    k <- seq_along(ap$pos_mue); exp_pos <- (k * (ap$N + 1)) / (length(ap$pos_mue) + 1)
    plot(exp_pos, ap$pos_mue, pch=19, cex=.6, xlab="Posición esperada", ylab="Observada",
         main="QQ posiciones (MUESTRA)")
    abline(0,1,col="blue",lty=2,lwd=2)
  })

  # Precisión plot
  output$plot_precision <- renderPlot({
    ap <- analisis(); req(ap)
    df <- samplesimil::curva_semiancho(ap$N, ap$S2_ref, ap$alpha, ap$EFD, n_max = ap$N-1)
    plot(df$n, df$semiancho, type="l", lwd=2, xlab="n", ylab="Semiancho IC (media)",
         main = sprintf("Curva precisión–n — S2_ref: %s", ap$fuente_S2))
    abline(h = input$EA, col="red", lty=2, lwd=2)
    abline(v = ap$plan$n_objetivo, col="blue", lty=3, lwd=2)
    abline(v = ap$n_actual, col="darkgreen", lty=3, lwd=2)
    points(ap$plan$n_objetivo, samplesimil::semiancho_media(ap$N, ap$S2_ref, ap$plan$n_objetivo, ap$alpha, ap$EFD),
           pch=19, cex=1.1)
    points(ap$n_actual, ap$semiancho_actual, pch=19, cex=1.1, col="darkgreen")
    legend("topright",
           legend=c("Semiancho", "EA objetivo", "n objetivo", "n actual"),
           col=c("black","red","blue","darkgreen"),
           lty=c(1,2,3,3), lwd=2, pch=c(NA,NA,19,19), bty="n")
  })

  # Calculadora 1
  observeEvent(input$btn_calc1, {
    ap <- analisis(); req(ap)
    n1 <- as.integer(input$calc_n1)
    a1 <- sanitize_num(input$calc_alpha1, lower=1e-6, upper=0.49, default=0.05)
    eabs <- samplesimil::error_abs_para_n(ap$N, ap$S2_ref, n1, a1, ap$EFD)
    mu1 <- suppressWarnings(as.numeric(input$calc_mu1))
    if (!is.finite(mu1)) {
      out <- data.frame(Indicador=c("E_abs alcanzable"), Valor=round(eabs,3))
    } else {
      out <- data.frame(Indicador=c("E_abs alcanzable","d alcanzable"),
                        Valor=c(round(eabs,3), round(eabs/abs(mu1),4)))
    }
    output$out_calc1 <- renderTable(out, rownames = FALSE)
  })

  # Calculadora 2
  observeEvent(input$btn_calc2, {
    ap <- analisis(); req(ap)
    n2 <- as.integer(input$calc_n2)
    eabs_str <- trimws(input$calc_eabs2); d_str <- trimws(input$calc_d2); mu2 <- suppressWarnings(as.numeric(input$calc_mu2))
    has_eabs <- nchar(eabs_str) > 0; has_d <- nchar(d_str) > 0
    if (has_eabs) {
      eabs <- as.numeric(eabs_str); res <- samplesimil::confianza_para_n_y_error_abs(ap$N, ap$S2_ref, n2, eabs, ap$EFD)
    } else if (has_d && is.finite(mu2)) {
      dval <- as.numeric(d_str); res <- samplesimil::confianza_para_n_y_error_rel(ap$N, ap$S2_ref, n2, dval, mu2, ap$EFD)
    } else {
      res <- list(alpha=NA_real_, conf=NA_real_, z=NA_real_)
    }
    output$out_calc2 <- renderTable({
      data.frame(Indicador=c("z","alpha","confianza (1-α)"),
                 Valor=round(c(res$z, res$alpha, res$conf), 4))
    }, rownames = FALSE)
  })

  # Calculadora 3
  observeEvent(input$btn_calc3, {
    ap <- analisis(); req(ap)
    a3 <- sanitize_num(input$calc_alpha3, lower=1e-6, upper=0.49, default=0.05)
    eabs_str <- trimws(input$calc_eabs3); d_str <- trimws(input$calc_d3); mu3 <- suppressWarnings(as.numeric(input$calc_mu3))
    TR3 <- sanitize_num(input$calc_TR3, lower=1e-6, upper=0.999999, default=0.8)
    has_eabs <- nchar(eabs_str) > 0; has_d <- nchar(d_str) > 0
    if (has_eabs) {
      eabs <- as.numeric(eabs_str)
      res <- samplesimil::n_para_error_abs(ap$N, ap$S2_ref, eabs, a3, ap$EFD, TR3)
    } else if (has_d && is.finite(mu3)) {
      dval <- as.numeric(d_str)
      res <- samplesimil::n_para_error_rel(ap$N, ap$S2_ref, mu3, dval, a3, ap$EFD, TR3)
    } else {
      res <- list(n_objetivo=NA_integer_, n_contacto=NA_integer_)
    }
    output$out_calc3 <- renderTable({
      data.frame(Indicador=c("n objetivo","n a contactar (ajuste TR)"),
                 Valor=c(res$n_objetivo, res$n_contacto))
    }, rownames = FALSE)
  })

  # Datos
  output$tbl_pil <- renderDT({
    d <- datos(); req(d); datatable(d$piloto, options = list(pageLength = 5))
  })
  output$tbl_mue <- renderDT({
    d <- datos(); req(d); datatable(d$muestra, options = list(pageLength = 5))
  })

  output$help_pandoc <- renderText({
    if (!rmarkdown::pandoc_available()) {
      return("Aviso: pandoc no está disponible. Se usará un HTML simple como alternativa.")
    } else return("")
  })

  output$dl_report_html <- downloadHandler(
    filename = function() sprintf("samplesimil_reporte_%s.html", as.character(Sys.Date())),
    content = function(file) {
      ap <- analisis()
      if (rmarkdown::pandoc_available()) {
        rmarkdown::render(
          system.file("report/positions_report.Rmd", package = "samplesimil"),
          output_file = file,
          params = list(resumen = list(
            N = ap$N, K = input$K, B = input$B,
            diag_pil = ap$diag_pil, diag_mue = ap$diag_mue,
            n_actual = ap$n_actual, n_objetivo = ap$plan$n_objetivo,
            EA = ap$EA, alpha = ap$alpha, EFD = ap$EFD, TR = ap$TR,
            S2_ref = ap$S2_ref, mu_ref = ap$mu_ref, fuente_S2 = ap$fuente_S2,
            semiancho_actual = ap$semiancho_actual,
            semiancho_obj = samplesimil::semiancho_media(ap$N, ap$S2_ref, ap$plan$n_objetivo, ap$alpha, ap$EFD)
          )),
          envir = new.env(parent = globalenv())
        )
      } else {
        html <- paste0("<html><head><meta charset='utf-8'><title>samplesimil reporte</title></head><body>",
          "<h2>Censo incompleto — Aleatoriedad por posiciones y precisión de la media</h2>",
          "<p><b>N:</b> ", ap$N, " | <b>n_actual:</b> ", ap$n_actual, " | <b>n_objetivo:</b> ", ap$plan$n_objetivo, "</p>",
          "<p><b>EA:</b> ", ap$EA, " | <b>alpha:</b> ", ap$alpha, " | <b>EFD:</b> ", ap$EFD, " | <b>TR:</b> ", ap$TR, "</p>",
          "<p><b>S2_ref:</b> ", round(ap$S2_ref,3), " (", ap$fuente_S2, "), <b>mu_ref:</b> ", round(ap$mu_ref,3), "</p>",
          "<h3>Aleatoriedad (p-valores)</h3>",
          "<p><b>Piloto:</b> ", paste(names(ap$diag_pil$pvals), round(ap$diag_pil$pvals,4), collapse=' | '), "</p>",
          "<p><b>Muestra:</b> ", paste(names(ap$diag_mue$pvals), round(ap$diag_mue$pvals,4), collapse=' | '), "</p>",
          "<h3>Precisión</h3>",
          "<p><b>Semiancho actual:</b> ", round(ap$semiancho_actual,3), 
          " | <b>Semiancho (n_obj):</b> ", round(samplesimil::semiancho_media(ap$N, ap$S2_ref, ap$plan$n_objetivo, ap$alpha, ap$EFD),3), "</p>",
          "</body></html>")
        con <- file(file, "w", encoding="utf-8"); writeLines(html, con); close(con)
      }
    }
  )
}

shinyApp(ui, server)
