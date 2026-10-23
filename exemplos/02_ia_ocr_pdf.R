# =============================================================================
# Inteligência artificial | Leitura de texto em PDF digitalizado (OCR)
# -----------------------------------------------------------------------------
# A página de exemplo é só uma imagem, sem camada de texto: não dá para
# selecionar nem copiar nada. O Tesseract reconhece os caracteres com uma rede
# neural recorrente (LSTM) treinada para o português, e o R transforma o texto
# reconhecido em uma tabela pronta para análise.
# Requer os pacotes pdftools, tesseract, png e ggplot2.
# =============================================================================

library(pdftools)
library(tesseract)
library(ggplot2)

# Os caminhos são relativos à pasta exemplos/. Aberto pelo projeto, o script entra nela.
if (!dir.exists("dados") && dir.exists("exemplos")) setwd("exemplos")

## ---- ocr-pdf ----
arquivo <- "dados/boletim_digitalizado.pdf"

pdf_info(arquivo)$pages       # uma página
nchar(pdf_text(arquivo))      # nenhum caractere: a página é só uma imagem

## ---- ocr-imagem ----
# a página vira uma matriz de pixels, como a foto da folha
pagina <- pdf_render_page(arquivo, dpi = 300, numeric = TRUE)
dim(pagina)

# binarização: tons de cinza viram preto ou branco, o que reduz os erros
cinza <- apply(pagina[, , 1:3], c(1, 2), mean)
pb    <- (cinza > 0.6) * 1
png::writePNG(pb, "pagina_pb.png")

## ---- ocr-leitura ----
# modelo de português (baixado na primeira vez)
if (!"por" %in% tesseract_info()$available) tesseract_download("por")

modelo <- tesseract("por", options = list(tessedit_pageseg_mode = 6))
texto  <- ocr("pagina_pb.png", engine = modelo)
cat(texto)

## ---- ocr-tabela ----
meses <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
           "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")

# linhas da tabela: um nome seguido de quatro números
linhas <- strsplit(texto, "\n")[[1]]
linhas <- grep("^[^ ]+( [0-9]+(,[0-9]+)?){4}$", linhas, value = TRUE)

clima <- read.table(text = linhas, dec = ",",
                    col.names = c("mes", "chuva", "tmax", "tmin", "dias"))
clima$mes <- factor(clima$mes, levels = meses)

# conferência: a IA também erra, então o script verifica o que leu
stopifnot(nrow(clima) == 12,
          !anyNA(clima$mes),
          all(clima$tmin < clima$tmax),
          all(clima$dias %in% 0:31))
clima

## ---- ocr-grafico ----
fator <- max(clima$chuva) / max(clima$tmax)

ggplot(clima, aes(mes)) +
  geom_col(aes(y = chuva), fill = "#C9D3E6", width = 0.7) +
  geom_line(aes(y = tmax * fator, group = 1, colour = "Máxima"),
            linewidth = 1.2) +
  geom_line(aes(y = tmin * fator, group = 1, colour = "Mínima"),
            linewidth = 1.2) +
  scale_y_continuous("Chuva (mm)",
                     sec.axis = sec_axis(~ . / fator, name = "Temperatura (°C)")) +
  scale_x_discrete(labels = substr(meses, 1, 1)) +
  labs(x = NULL, colour = NULL)

## ---- ocr-salvar ----
write.csv2(clima, "boletim_extraido.csv", row.names = FALSE)
ggsave("climograma.png", width = 16, height = 10, units = "cm", dpi = 300)
