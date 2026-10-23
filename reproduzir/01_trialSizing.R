# =============================================================================
# Reprodução | trialSizing
# -----------------------------------------------------------------------------
# Refaz, na mesma ordem, todas as análises do trialSizing mostradas na
# apresentação: diagnóstico do campo, CV por formato de parcela, os três
# modelos de tamanho ótimo, a comparação entre métodos e o número de
# repetições.
# Rodar a partir da pasta do projeto (abrir o arquivo .Rproj).
# Requer: install.packages(c("trialSizing", "ggplot2"))
# =============================================================================

library(trialSizing)
source(file.path("reproduzir", "00_tema.R"))

## ---- ts-dados ----
# Três ensaios de uniformidade simulados, cada um com 8 x 12 unidades de 1 m²
grid1 <- as.matrix(uniformity_trial[uniformity_trial$trial == "T1",
                                    grep("^col", names(uniformity_trial))])
grids <- lapply(split(uniformity_trial, uniformity_trial$trial),
                function(d) as.matrix(d[, grep("^col", names(d))]))

## ---- ts-diagnostico ----
chk <- check_trial(grids)
plot(chk)

## ---- ts-cv ----
tab <- calc_cv_shapes(list(T1 = grid1))

## ---- ts-cv-grafico ----
ggplot(tab, aes(x, cv)) +
  geom_point(aes(size = n), colour = cores[["azul_medio"]], alpha = 0.7) +
  scale_size_area(max_size = 9) +
  labs(x = "Tamanho de parcela (m²)", y = "CV (%)", size = "Parcelas")

## ---- ts-lrp ----
f_lrp <- fit_lrp(tab, x = "x", cv = "cv", step = 0.05)
plot(f_lrp)

## ---- ts-mcm ----
f_mcm <- fit_mcm(tab, x = "x", cv = "cv")
plot(f_mcm)

## ---- ts-qrp ----
f_qrp <- fit_qrp(tab, x = "x", cv = "cv", step = 0.05)
plot(f_qrp)

## ---- ts-comparacao ----
set.seed(26)
cmp <- compare_methods(grid1, step = 0.05, bootstrap = TRUE, n_boot = 200)

## ---- ts-comparacao-grafico ----
s <- cmp$summary
s$method <- factor(toupper(s$method), levels = rev(toupper(s$method)))
ggplot(s, aes(Xo, method)) +
  geom_errorbar(aes(xmin = Xo_lwr, xmax = Xo_upr), width = 0.25, orientation = "y",
                colour = cores[["cinza"]], na.rm = TRUE) +
  geom_point(size = 5, colour = cores[["azul_medio"]]) +
  geom_text(aes(label = paste0(format(round(Xo, 1), decimal.mark = ","), " m²")),
            vjust = -1.1, size = 6, colour = cores[["azul"]]) +
  labs(x = "Tamanho ótimo de parcela (m²)", y = NULL)

## ---- ts-repeticoes ----
cv_otimo <- f_lrp$parameters[["Breakpoint_Response"]]
rep <- calc_replicates(treatments = 3:20, cv_percent = cv_otimo,
                       lsd_percent = c(10, 20, 30))
plot(rep)
