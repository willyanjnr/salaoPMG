# =============================================================================
# Reprodução | EstimateBreed
# -----------------------------------------------------------------------------
# Refaz, na mesma ordem, as funções do EstimateBreed mostradas na
# apresentação: ISGR, índice fototermal, risco de ferrugem, condição de
# aplicação (delta T), plastocrono, temperaturas cardinais e seleção de
# transgressivos. Os dados são os de exemplo do pacote.
# Rodar a partir da pasta do projeto (abrir o arquivo .Rproj).
# Requer: install.packages(c("EstimateBreed", "ggplot2", "patchwork"))
# A previsão do delta T (tdelta) precisa de internet e roda no final.
# =============================================================================

library(EstimateBreed)
library(patchwork)
source(file.path("reproduzir", "00_tema.R"))

## ---- eb-isgr ----
DPclim <- with(desvamb, desv_clim(ENV, TMED, PREC))
indice <- with(genot, isgr(GEN, ENV, NG, MG, CICLO))

## ---- eb-isgr-grafico ----
ind <- as.data.frame(indice)
ind$Gen <- reorder(ind$Gen, ind$ISGR)
ggplot(ind, aes(Gen, ISGR, fill = Env)) +
  geom_col(width = 0.75) +
  labs(x = "Linhagem", y = "ISGR", fill = "Ambiente") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 12),
        panel.grid.major.x = element_blank())

## ---- eb-ptermal ----
foto <- with(termaldata, ptermal(Day, Temperature, Radiation, Period))

## ---- eb-ptermal-grafico ----
foto$Periodo <- ifelse(grepl("^V", foto$PER), "Vegetativo", "Reprodutivo")
pal_per <- c(Vegetativo = cores[["secundaria"]], Reprodutivo = cores[["azul"]])
h1 <- ggplot(foto, aes(DAY, Q, fill = Periodo)) + geom_col(width = 1) +
  scale_fill_manual(values = pal_per) +
  labs(x = "Dias do ciclo", y = "Quociente diário", fill = NULL) +
  theme(legend.position = "top")
h2 <- ggplot(foto, aes(DAY, Qac_final, colour = Periodo)) + geom_line(linewidth = 1.6) +
  scale_colour_manual(values = pal_per) +
  labs(x = "Dias do ciclo", y = "Acumulado", colour = NULL) +
  theme(legend.position = "top")
h1 | h2

## ---- eb-risco ----
risco <- with(clima, risk(DY, MO, TMED, RH, disease = "rust"))

## ---- eb-risco-grafico ----
meses <- c("jan", "fev", "mar", "abr", "mai", "jun",
           "jul", "ago", "set", "out", "nov", "dez")
risco$Mes <- factor(meses[risco$Month], levels = meses)
ggplot(risco, aes(Mes, RELrisk, fill = RELrisk)) +
  geom_col(width = 0.7) +
  scale_fill_gradient(low = "#FBDDB0", high = cores[["laranja"]], guide = "none") +
  labs(x = NULL, y = "Risco relativo (%)") +
  theme(panel.grid.major.x = element_blank())

## ---- eb-plastocrono ----
p <- with(pheno, plast(GEN, TMED, EST, NN, habit = "ind", plot = TRUE))
fases <- split(p$data, p$data$Class)
round(sapply(fases, function(d) 1 / coef(lm(NN ~ STA, data = d))[["STA"]]), 1)

## ---- eb-plastocrono-grafico ----
nomes <- c(Early = "V1 a V10", Intermediate = "R1 e R2", Late = "R3 a R5")
plasto <- sapply(fases, function(d) 1 / coef(lm(NN ~ STA, data = d))[["STA"]])
rotulos <- paste0(nomes[names(fases)], ", ", format(round(plasto, 0)), " °C dia por nó")
d <- unique(p$data[, c("STA", "NN", "Class")])
ggplot(d, aes(STA, NN, colour = Class)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 1) +
  geom_point(size = 2.8) +
  scale_colour_manual(values = unname(paleta3), labels = rotulos) +
  labs(x = "Soma térmica acumulada (°C dia)", y = "Número de nós", colour = NULL) +
  theme(legend.position = "right")

## ---- eb-cardinais ----
diario <- aggregate(TMED ~ MO + DY, data = clima, FUN = mean)
diario <- diario[order(diario$MO, diario$DY), ]
cardinais <- with(diario[1:120, ], optemp(TMED, crop = "soybean"))

## ---- eb-transgressivos ----
set.seed(26)
ensaio <- data.frame(Gen = paste0("G", 1:20), Var = round(rnorm(20, 3.5, 0.8), 2))
selecao <- with(ensaio, transg(Gen, Var, Control = rep(3.8, 20), plot = TRUE))

## ---- eb-tdelta ----
prev <- tdelta(LON = -53.6969, LAT = -28.0638, type = 1, days = 5, verbose = FALSE)

## ---- eb-tdelta-grafico ----
prev$Momento <- as.POSIXct(paste(prev$Day, prev$Hour), tz = "America/Sao_Paulo")
prev$Ideal <- with(prev, DELTAT >= 2 & DELTAT <= 8 & WindS < 10 & Prec < 2)
ggplot(prev, aes(Momento, DELTAT)) +
  annotate("rect", xmin = min(prev$Momento), xmax = max(prev$Momento),
           ymin = 2, ymax = 8, fill = cores[["secundaria"]], alpha = 0.2) +
  geom_line(colour = cores[["cinza"]]) +
  geom_point(aes(colour = Ideal), size = 2.2) +
  scale_colour_manual(values = c(`TRUE` = cores[["azul_medio"]], `FALSE` = cores[["laranja"]]),
                      labels = c(`TRUE` = "Adequado", `FALSE` = "Inadequado")) +
  scale_x_datetime(date_breaks = "12 hours", date_labels = "%d/%m %Hh") +
  labs(x = NULL, y = "Delta T (°C)", colour = NULL) +
  theme(legend.position = "top", axis.text.x = element_text(size = 13))
