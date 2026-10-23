# =============================================================================
# Reprodução | Gráficos antes e depois do ggplot2
# -----------------------------------------------------------------------------
# Os mesmos dados desenhados com os gráficos básicos do R, como se fazia antes
# do ggplot2 (2007), e com o ggplot2, que segue a gramática dos gráficos
# (Wickham, 2010).
# Rodar a partir da pasta do projeto (abrir o arquivo .Rproj).
# Requer: install.packages(c("EstimateBreed", "ggplot2"))
# Dados: ptn (proteína e força de glúten de trigo) e dados simulados.
# =============================================================================

library(EstimateBreed)
source(file.path("reproduzir", "00_tema.R"))

## ---- gg-base-facetas ----
par(mfrow = c(1, 3))
for (cv in sort(unique(ptn$Cult))) {
  d <- ptn[ptn$Cult == cv, ]
  plot(d$PTN, d$W, main = cv)
  abline(lm(W ~ PTN, data = d))
}

## ---- gg-ggplot-facetas ----
ggplot(ptn, aes(PTN, W)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Cult)

## ---- gg-barras-dados ----
# Quatro grupos com a mesma média (50) e o mesmo desvio padrão (10), mas com
# distribuições diferentes (dados simulados, como em Weissgerber et al., 2015)
set.seed(2015)
padroniza <- function(x) (x - mean(x)) / sd(x) * 10 + 50
grupos <- list(
  "Simétrico"        = rnorm(20),
  "Valor\nextremo"   = c(rnorm(19, 0, 0.3), 4),
  "Dois\ngrupos"     = c(rnorm(10, -1, 0.25), rnorm(10, 1, 0.25)),
  "Quatro\namostras" = c(-1, -0.4, 0.3, 1.1)
)
barras <- do.call(rbind, lapply(names(grupos), function(g)
  data.frame(grupo = g, valor = padroniza(grupos[[g]]))))
barras$grupo <- factor(barras$grupo, levels = names(grupos))
aggregate(valor ~ grupo, data = barras, FUN = function(v) c(media = mean(v), dp = sd(v)))

## ---- gg-barras-base ----
medias <- tapply(barras$valor, barras$grupo, mean)
dps    <- tapply(barras$valor, barras$grupo, sd)
x <- barplot(medias, ylim = c(0, 80), cex.names = 0.85)
arrows(x, medias, x, medias + dps, angle = 90, length = 0.1)

## ---- gg-barras-ggplot ----
ggplot(barras, aes(grupo, valor)) +
  geom_boxplot(outlier.shape = NA, width = 0.5, colour = cores[["cinza"]]) +
  geom_jitter(width = 0.12, size = 2.6, alpha = 0.8, colour = cores[["azul_medio"]]) +
  labs(x = NULL, y = "Valor")
