# -------- Utilidades de posiciones --------
map_a_pos <- function(ids_sel, ids_marco) {
  pos <- match(ids_sel, ids_marco)
  pos <- pos[!is.na(pos) & pos >= 1 & pos <= length(ids_marco)]
  sort(unique(pos))
}

.runs_count <- function(idx, N) {
  z <- integer(N); z[idx] <- 1L
  sum(c(1L, diff(z)) != 0L)
}

.max_gap <- function(idx, N) {
  gaps <- diff(c(0, sort(idx), N+1L)) - 1L
  max(gaps)
}

.ks_pos_stat <- function(pos, N) {
  u <- sort(pos) / N
  n <- length(u)
  max( max(abs((1:n)/n - u)), max(abs(((1:n)-1)/n - u)) )
}

.chi_blocks_stat <- function(pos, N, K) {
  n <- length(pos)
  bloques <- cut(1:N, breaks = K, labels = FALSE)
  obs <- tabulate(bloques[pos], nbins = K)
  esp <- rep(n/K, K)
  sum((obs - esp)^2 / esp)
}

.p_fisher <- function(pvals) {
  X <- -2 * sum(log(pvals))
  1 - pchisq(X, df = 2*length(pvals))
}

#' Diagnóstico de aleatoriedad por posiciones (Monte Carlo)
diag_posiciones <- function(N, pos, K = 10L, B = 5000L, semilla = NULL) {
  if (!is.null(semilla)) set.seed(semilla)
  n <- length(pos); pos <- sort(pos)
  T_chi <- .chi_blocks_stat(pos, N, K)
  T_ks  <- .ks_pos_stat(pos, N)
  T_run <- .runs_count(pos, N)
  T_gap <- .max_gap(pos, N)

  chi_b <- numeric(B); ks_b <- numeric(B); run_b <- numeric(B); gap_b <- numeric(B)
  for (b in seq_len(B)) {
    pb <- sort(sample.int(N, n, replace = FALSE))
    chi_b[b] <- .chi_blocks_stat(pb, N, K)
    ks_b[b]  <- .ks_pos_stat(pb, N)
    run_b[b] <- .runs_count(pb, N)
    gap_b[b] <- .max_gap(pb, N)
  }

  p_chi <- mean(chi_b >= T_chi)
  p_ks  <- mean(ks_b  >= T_ks)
  p_run <- 2*min(mean(run_b <= T_run), mean(run_b >= T_run)); p_run <- min(1, p_run)
  p_gap <- mean(gap_b >= T_gap)

  list(
    n = n, N = N, K = K,
    T_obs = c(chi = T_chi, ks = T_ks, runs = T_run, gap = T_gap),
    pvals = c(p_chi = p_chi, p_ks = p_ks, p_runs = p_run, p_gap = p_gap),
    p_fisher = .p_fisher(c(p_chi, p_ks, p_run, p_gap)),
    null = list(chi = chi_b, ks = ks_b, runs = run_b, gap = gap_b),
    bloques_obs = tabulate(cut(1:N, K, labels = FALSE)[pos], nbins = K),
    pos = pos
  )
}

# -------- Calculadoras de precisión para la media (CPF) --------
#' Semiancho del IC de la media con CPF
semiancho_media <- function(N, S2, n, alpha = 0.05, EFD = 1) {
  z <- stats::qnorm(1 - alpha/2)
  z * sqrt( (1 - n/N) * (EFD * S2) / n )
}

#' Error absoluto alcanzable con n y alpha
error_abs_para_n <- function(N, S2, n, alpha = 0.05, EFD = 1) {
  semiancho_media(N, S2, n, alpha, EFD)
}

#' Error relativo alcanzable con n y alpha (respecto a mu_ref)
error_rel_para_n <- function(N, S2, mu_ref, n, alpha = 0.05, EFD = 1) {
  if (!is.finite(mu_ref) || mu_ref == 0) stop("mu_ref debe ser finita y no nula.")
  error_abs_para_n(N, S2, n, alpha, EFD) / abs(mu_ref)
}

#' Tamaño muestral para error absoluto objetivo
n_para_error_abs <- function(N, S2, E_abs, alpha = 0.05, EFD = 1, TR = 1) {
  z  <- stats::qnorm(1 - alpha/2)
  num <- N * EFD * (z^2) * S2
  den <- (E_abs^2) * N + EFD * (z^2) * S2
  n   <- num / den
  n_obj <- ceiling(n); n_ctc <- ceiling(n / TR)
  list(n_objetivo = n_obj, n_contacto = n_ctc)
}

#' Tamaño muestral para error relativo objetivo d (proporción de mu_ref)
n_para_error_rel <- function(N, S2, mu_ref, d, alpha = 0.05, EFD = 1, TR = 1) {
  if (!is.finite(mu_ref) || mu_ref == 0) stop("mu_ref debe ser finita y no nula.")
  if (!is.finite(d) || d <= 0) stop("d debe ser positivo.")
  E_abs <- d * abs(mu_ref)
  n_para_error_abs(N, S2, E_abs, alpha, EFD, TR)
}

#' Nivel de confianza alcanzable con n y error absoluto objetivo
confianza_para_n_y_error_abs <- function(N, S2, n, E_abs, EFD = 1) {
  if (!is.finite(E_abs) || E_abs <= 0) stop("E_abs debe ser positivo.")
  denom <- sqrt( (1 - n/N) * (EFD * S2) / n )
  if (!is.finite(denom) || denom <= 0) stop("Parámetros inválidos: ver N, n, S2, EFD.")
  z <- E_abs / denom
  alpha <- 2 * (1 - stats::pnorm(z))
  list(alpha = alpha, conf = 1 - alpha, z = z)
}

#' Nivel de confianza alcanzable con n y error relativo objetivo d
confianza_para_n_y_error_rel <- function(N, S2, n, d, mu_ref, EFD = 1) {
  if (!is.finite(mu_ref) || mu_ref == 0) stop("mu_ref debe ser finita y no nula.")
  if (!is.finite(d) || d <= 0) stop("d debe ser positivo.")
  E_abs <- d * abs(mu_ref)
  confianza_para_n_y_error_abs(N, S2, n, E_abs, EFD)
}

#' Alias histórico para compatibilidad
plan_n <- function(N, S2, EA, alpha = 0.05, EFD = 1, TR = 1) {
  n_para_error_abs(N, S2, E_abs = EA, alpha = alpha, EFD = EFD, TR = TR)
}

curva_semiancho <- function(N, S2, alpha = 0.05, EFD = 1, n_max = N-1) {
  z  <- stats::qnorm(1 - alpha/2)
  ns <- unique(c(seq(20, min(600, n_max), by=10), seq(min(620, n_max), n_max, by=20)))
  sem <- z * sqrt( (1 - ns/N) * (EFD * S2) / ns )
  data.frame(n = ns, semiancho = sem)
}

run_app <- function() {
  app_dir <- system.file("app", package = "samplesimil")
  if (app_dir == "") stop("No se encontró la carpeta de la app.", call. = FALSE)
  shiny::runApp(app_dir, launch.browser = TRUE, display.mode = "normal")
}

.onAttach <- function(libname, pkgname) {
  auto <- getOption("samplesimil.autorun", TRUE)
  if (isTRUE(auto) && interactive()) {
    packageStartupMessage("samplesimil: iniciando interfaz Shiny (options(samplesimil.autorun=FALSE) para desactivar).")
    try(run_app(), silent = TRUE)
  } else {
    packageStartupMessage("samplesimil cargado. Ejecuta samplesimil::run_app() para abrir la interfaz.")
  }
}
