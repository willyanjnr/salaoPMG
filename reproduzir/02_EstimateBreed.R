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
# Gráfico previsto no código da função risk(): risco por temperatura, por
# umidade e risco total, mês a mês. Na versão atual do CRAN esse bloco fica
# depois do return() e não é desenhado, então ele é refeito aqui com os
# mesmos cálculos da função.
alfa <- log(2) / log(2.30508474576)
umidade <- aggregate(RH ~ DY + MO, data = clima, FUN = function(x) sum(x > 85))
names(umidade) <- c("Day", "Month", "W")
umidade$RHrisk <- pmin(1 / 1 + 2.72^(umidade$W - 12), 100)
temp <- aggregate(TMED ~ DY + MO, data = subset(clima, RH > 85), FUN = mean)
names(temp) <- c("Day", "Month", "TMed")
temp$TEMPrisk <- (2 * (temp$TMed - 8) * alfa * (22.75 - 8) * alfa -
                  (temp$TMed - 8) * 2 * alfa) / (22.75 - 8) * 2 * alfa
diario_risco <- merge(umidade, temp)
diario_risco$TOTALrisk <- pmin(diario_risco$RHrisk * diario_risco$TEMPrisk, 100)

layout(matrix(c(1, 3, 2, 3), 2, 2, byrow = TRUE))
cor_caixa <- rgb(0.3, 0.5, 0.4, 0.6)
boxplot(TEMPrisk ~ Month, data = temp, col = cor_caixa, outline = FALSE,
        ylab = "Temperature Risk (%)", xlab = "Month")
boxplot(RHrisk ~ Month, data = umidade, col = cor_caixa, outline = FALSE,
        ylab = "Relative Humidity Risk (%)", xlab = "Month")
boxplot(TOTALrisk ~ Month, data = diario_risco, col = cor_caixa, outline = FALSE,
        ylab = "Total Risk (%)", xlab = "Month")

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

## ---- eb-deltat-conceito ----
# Delta T = temperatura do ar menos temperatura de bulbo úmido.
# Bulbo úmido pela fórmula de Stull (2011), a partir da temperatura e da umidade.
bulbo_umido <- function(t, ur) {
  t * atan(0.151977 * sqrt(ur + 8.313659)) + atan(t + ur) - atan(ur - 1.676331) +
    0.00391838 * ur^1.5 * atan(0.023101 * ur) - 4.686035
}
grade <- expand.grid(temp = seq(10, 40, by = 0.2), ur = seq(20, 100, by = 0.5))
grade$delta_t <- grade$temp - bulbo_umido(grade$temp, grade$ur)
grade$faixa <- cut(grade$delta_t, c(-Inf, 2, 8, Inf),
                   labels = c("Abaixo de 2", "De 2 a 8 (adequado)", "Acima de 8"))
ggplot(grade, aes(temp, ur)) +
  geom_raster(aes(fill = faixa)) +
  geom_contour(aes(z = delta_t), breaks = c(2, 8), colour = "white", linewidth = 0.8) +
  scale_fill_manual(values = c("#8FA9C4", cores[["secundaria"]], cores[["laranja"]])) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Temperatura do ar (°C)", y = "Umidade relativa (%)", fill = "Delta T (°C)")

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
