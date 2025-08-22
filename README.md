# samplesimil 0.4.0

**Caso de uso**: Censo con cobertura incompleta (participación voluntaria). Preguntas:
1) ¿Puedo generalizar? (auditoría del mecanismo de selección sobre el orden del marco).
2) ¿Qué precisión logro o necesito? (tamaño muestral, error absoluto/relativo o nivel de confianza).

**Módulos**:
- **Aleatoriedad por posiciones** (Monte Carlo): chi² por bloques, KS de posiciones, rachas y brecha máxima.
- **Precisión de la media** (CPF + EFD + TR): calculadoras para n objetivo, error alcanzable o confianza alcanzable,
  con objetivos en error **absoluto** o **relativo a la media**.

## Instalación desde GitHub
```r
install.packages("remotes")
remotes::install_github("diegomezapy/samplesimil", ref = "main",
                        upgrade = "never", dependencies = TRUE)
library(samplesimil)
run_app()
```

## Instalación local (zip descargado)
```r
install.packages("remotes")
remotes::install_local("C:/ruta/a/samplesimil", upgrade = "never", dependencies = TRUE)
library(samplesimil)
run_app()
```

## Ejemplo conceptual (N = 115, n = 60)
- Objetivo en **error relativo** `d = 0.10` de la media `\bar{Y}` con `S²` de piloto:
  `n_para_error_rel(N, S2, mu_ref, d, alpha, EFD, TR)`.
- Para **n fijo = 60** y un `E_abs` objetivo, calcule `confianza_para_n_y_error_abs(N, S2, n, E_abs, EFD)`
  para el `1 - α` alcanzable.
- Para **n fijo = 60** y `α` fijo, use `error_abs_para_n(N, S2, n, alpha, EFD)` y divida por `mu_ref`
  para obtener el error relativo.

## Datos de ejemplo
- `inst/extdata/marco_ids.csv`
- `inst/extdata/piloto.csv`
- `inst/extdata/muestra.csv`
