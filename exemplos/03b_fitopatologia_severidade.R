# =============================================================================
# Fitopatologia | Severidade de doença em folha de soja a partir de uma foto
# -----------------------------------------------------------------------------
# A imagem é uma matriz de pixels com três camadas de cor (vermelho, verde e
# azul). Cada pixel é classificado pela cor em fundo, tecido sadio, clorose ou
# necrose, e a severidade é a proporção da folha com sintomas.
# Imagem: exemplo do pacote pliman (Olivoto, 2022), folha de soja sobre fundo
# azul. Requer apenas o pacote jpeg.
# =============================================================================

library(jpeg)

# Os caminhos são relativos à pasta exemplos/. Aberto pelo projeto, o script entra nela.
if (!dir.exists("dados") && dir.exists("exemplos")) setwd("exemplos")

## ---- sev-imagem ----
img <- readJPEG("dados/folha_soja.jpg")
dim(img)                        # linhas, colunas e 3 camadas de cor

R <- c(img[, , 1])              # cada camada vira um vetor de pixels
G <- c(img[, , 2])
B <- c(img[, , 3])

## ---- sev-classes ----
fundo <- B > R & B > G          # fundo azul

# matiz (hue) de cada pixel: verde perto de 0,33, amarelo perto de 0,16,
# marrom abaixo de 0,12
matiz <- rgb2hsv(R, G, B, maxColorValue = 1)[1, ]

classe <- ifelse(fundo, "fundo",
          ifelse(matiz > 0.20, "sadia",
          ifelse(matiz > 0.12, "clorose", "necrose")))
table(classe)

## ---- sev-severidade ----
folha <- classe != "fundo"
area  <- prop.table(table(classe[folha])) * 100
round(area, 1)

severidade <- sum(area[c("clorose", "necrose")])
cat("Severidade:", round(severidade, 1), "% da área foliar\n")

## ---- sev-mapa ----
cores <- c(fundo = "white", sadia = "#3E7D3A",
           clorose = "#E3B505", necrose = "#6B3E26")
mapa  <- matrix(cores[classe], nrow = nrow(img), ncol = ncol(img))

par(mfrow = c(1, 2), mar = c(0, 0, 2, 0))
plot(as.raster(img));  title("Foto original")
plot(as.raster(mapa)); title("Pixels classificados")

## ---- sev-salvar ----
png("severidade_folha.png", width = 16, height = 9, units = "cm", res = 300)
par(mfrow = c(1, 2), mar = c(0, 0, 2, 0))
plot(as.raster(img));  title("Foto original")
plot(as.raster(mapa)); title("Pixels classificados")
dev.off()
