# =============================================================================
# Fitopatologia | Curva de progresso da doença e controle químico
# -----------------------------------------------------------------------------
# Dados simulados: severidade (%) avaliada a cada 7 dias, em 3 tratamentos
# com 4 repetições. Pergunta: os fungicidas reduzem o progresso da doença?
#   1. área abaixo da curva de progresso da doença (AACPD) e ANOVA
#   2. ajuste do modelo logístico e comparação das taxas de progresso
# Requer apenas o pacote ggplot2.
# =============================================================================

library(ggplot2)

## ---- fito-dados ----
set.seed(2026)
dias  <- seq(0, 42, by = 7)
y0    <- 1                                 # severidade inicial (%)
taxa  <- c("Testemunha" = 0.20, "Fungicida A" = 0.13,
           "Fungicida B" = 0.09)                # taxa de progresso

dados <- expand.grid(dia = dias, rep = 1:4, tratamento = names(taxa))
r     <- taxa[as.character(dados$tratamento)]
dados$sev <- 100 / (1 + (100 / y0 - 1) * exp(-r * dados$dia))
dados$sev <- pmin(pmax(dados$sev + rnorm(nrow(dados), 0, 2), 0), 100)
head(dados)

## ---- fito-aacpd ----
aacpd <- function(t, y) sum(diff(t) * (head(y, -1) + tail(y, -1)) / 2)

res <- do.call(rbind, lapply(split(dados, ~ tratamento + rep), function(d)
  data.frame(tratamento = d$tratamento[1], rep = d$rep[1],
             aacpd = aacpd(d$dia, d$sev))))

aggregate(aacpd ~ tratamento, data = res, FUN = mean)
summary(aov(aacpd ~ tratamento, data = res))

## ---- fito-logistico ----
logistico <- function(d) {
  m <- nls(sev ~ 100 / (1 + (100 / y0 - 1) * exp(-r * dia)),
           data = d, start = list(y0 = 1, r = 0.1))
  coef(m)
}

taxas <- t(sapply(split(dados, dados$tratamento), logistico))
round(taxas, 3)

## ---- fito-grafico ----
medias <- aggregate(sev ~ dia + tratamento, data = dados, FUN = mean)
curvas <- do.call(rbind, lapply(rownames(taxas), function(tr) {
  t <- seq(0, 42, by = 0.5)
  y0 <- taxas[tr, "y0"]
  r  <- taxas[tr, "r"]
  data.frame(tratamento = tr, dia = t,
             sev = 100 / (1 + (100 / y0 - 1) * exp(-r * t)))
}))
curvas$tratamento <- factor(curvas$tratamento, levels = names(taxa))

ggplot(medias, aes(dia, sev, colour = tratamento)) +
  geom_line(data = curvas, linewidth = 1) +
  geom_point(size = 3) +
  labs(x = "Dias após a inoculação", y = "Severidade (%)", colour = NULL)

## ---- fito-salvar ----
ggsave("progresso_da_doenca.png", width = 16, height = 10,
       units = "cm", dpi = 300)
