# =============================================================================
# Raízes | Comprimento de raízes a partir de uma imagem
# -----------------------------------------------------------------------------
# Faz em R o cálculo central de programas como o WinRHIZO. A imagem vira uma
# matriz de pixels, as raízes são separadas do fundo, afinadas até a espessura
# de um pixel (esqueleto) e o comprimento é a soma das ligações entre os
# pixels do esqueleto.
# As imagens são simuladas, com comprimento conhecido, para mostrar como se
# valida o método. Com imagens reais, a validação é feita contra o método de
# referência (WinRHIZO, régua ou método das interseções) nas mesmas amostras.
# Requer os pacotes png e ggplot2.
# =============================================================================

library(png)
library(ggplot2)

## ---- raiz-simular ----
# Desenha um sistema radicular e devolve o comprimento total desenhado (px).
# Raiz principal descendo do topo e laterais saindo dela em ângulos aleatórios.
simular_raiz <- function(arquivo, n_laterais = 20, semente = 1, lado = 800) {
  set.seed(semente)
  passo <- 12
  dentro <- function(x, y) x > 10 & x < lado - 10 & y > 10 & y < lado - 10

  segmento <- function(x0, y0, angulo, n, lwd) {
    x <- x0; y <- y0; total <- 0
    for (i in seq_len(n)) {
      angulo <- angulo + rnorm(1, 0, 0.18)
      x1 <- x + passo * cos(angulo); y1 <- y + passo * sin(angulo)
      if (!dentro(x1, y1)) break
      segments(x, y, x1, y1, lwd = lwd, col = "#5B4636", lend = "round")
      total <- total + passo
      x <- x1; y <- y1
    }
    total
  }

  png(arquivo, width = lado, height = lado, bg = "#F4F1EA")
  par(mar = c(0, 0, 0, 0))
  plot.new(); plot.window(c(0, lado), c(0, lado), xaxs = "i", yaxs = "i")

  # raiz principal, guardando a trajetória para inserir as laterais
  x <- lado / 2; y <- lado - 20; ang <- -pi / 2; caminho <- NULL; total <- 0
  for (i in 1:55) {
    ang <- -pi / 2 + 0.7 * (ang + pi / 2) + rnorm(1, 0, 0.12)
    x1 <- x + passo * cos(ang); y1 <- y + passo * sin(ang)
    if (!dentro(x1, y1)) break
    segments(x, y, x1, y1, lwd = 7, col = "#5B4636", lend = "round")
    total <- total + passo
    caminho <- rbind(caminho, c(x1, y1, ang))
    x <- x1; y <- y1
  }
  for (k in seq_len(n_laterais)) {
    p <- caminho[sample(nrow(caminho), 1), ]
    lado_ramo <- sample(c(-1, 1), 1)
    total <- total + segmento(p[1], p[2], p[3] + lado_ramo * runif(1, 0.6, 1.3),
                              n = sample(6:22, 1), lwd = 4)
  }
  dev.off()
  total
}

## ---- raiz-funcoes ----
# Desloca a matriz uma linha ou coluna, preenchendo a borda com zero
desloca <- function(m, di, dj) {
  out <- matrix(0L, nrow(m), ncol(m))
  li <- max(1, 1 - di):min(nrow(m), nrow(m) - di)
  lj <- max(1, 1 - dj):min(ncol(m), ncol(m) - dj)
  out[li, lj] <- m[li + di, lj + dj]
  out
}

# Afinamento de Zhang e Suen (1984): remove pixels da borda até sobrar uma
# linha de um pixel no centro de cada raiz
afinar <- function(raiz) {
  m <- raiz * 1L
  direcoes <- list(c(-1, 0), c(-1, 1), c(0, 1), c(1, 1),
                   c(1, 0), c(1, -1), c(0, -1), c(-1, -1))
  repeat {
    mudou <- FALSE
    for (etapa in 1:2) {
      p <- lapply(direcoes, function(d) desloca(m, d[1], d[2]))
      vizinhos <- Reduce(`+`, p)
      p9 <- c(p, p[1])
      transicoes <- Reduce(`+`, lapply(1:8, function(k) p9[[k]] == 0 & p9[[k + 1]] == 1))
      if (etapa == 1) {
        c1 <- p[[1]] * p[[3]] * p[[5]]; c2 <- p[[3]] * p[[5]] * p[[7]]
      } else {
        c1 <- p[[1]] * p[[3]] * p[[7]]; c2 <- p[[1]] * p[[5]] * p[[7]]
      }
      apagar <- m == 1 & vizinhos >= 2 & vizinhos <= 6 & transicoes == 1 &
        c1 == 0 & c2 == 0
      if (any(apagar)) { m[apagar] <- 0L; mudou <- TRUE }
    }
    if (!mudou) break
  }
  m == 1
}

# O afinamento deixa "degraus" em raízes inclinadas. Os pixels de canto que
# não fazem falta para a conexão são retirados, e o esqueleto passa a usar
# ligações diagonais nesses trechos
tirar_degraus <- function(esqueleto) {
  m <- esqueleto * 1L
  direcoes <- list(c(-1, 0), c(-1, 1), c(0, 1), c(1, 1),
                   c(1, 0), c(1, -1), c(0, -1), c(-1, -1))
  # cada canto é formado por dois vizinhos retos (N e L, L e S, S e O, O e N);
  # o pixel sai se o vizinho na diagonal oposta estiver vazio
  cantos <- list(c(1, 3, 6), c(3, 5, 8), c(5, 7, 2), c(7, 1, 4))
  for (k in cantos) {
    p <- lapply(direcoes[k], function(d) desloca(m, d[1], d[2]))
    m[m == 1 & p[[1]] == 1 & p[[2]] == 1 & p[[3]] == 0] <- 0L
  }
  m == 1
}

# Comprimento do esqueleto: conta as ligações entre pixels vizinhos e usa os
# pesos de Kulpa (1977), 0,948 para ligações retas e 1,343 para diagonais,
# que corrigem o efeito da grade de pixels em raízes inclinadas
comprimento_px <- function(esqueleto) {
  e  <- tirar_degraus(esqueleto) * 1L
  retas <- sum(e * desloca(e, 0, 1)) + sum(e * desloca(e, 1, 0))
  diag  <- sum(e * desloca(e, 1, 1)  * (1 - desloca(e, 0, 1))  * (1 - desloca(e, 1, 0))) +
           sum(e * desloca(e, 1, -1) * (1 - desloca(e, 0, -1)) * (1 - desloca(e, 1, 0)))
  0.948 * retas + 1.343 * diag
}

## ---- raiz-imagem ----
verdadeiro <- simular_raiz("raiz_01.png", semente = 1)

img  <- readPNG("raiz_01.png")
raiz <- apply(img[, , 1:3], c(1, 2), mean) < 0.5   # TRUE onde há raiz
dim(raiz)
mean(raiz) * 100                                   # % da imagem com raiz

## ---- raiz-esqueleto ----
esqueleto <- afinar(raiz)
c(raiz = sum(raiz), esqueleto = sum(esqueleto))    # pixels antes e depois

## ---- raiz-comprimento ----
cm_por_px <- 2.54 / 200                            # imagem de 200 dpi
estimado  <- comprimento_px(esqueleto) * cm_por_px
round(c(estimado = estimado, verdadeiro = verdadeiro * cm_por_px), 1)

## ---- raiz-mapa ----
par(mfrow = c(1, 2), mar = c(0, 0, 2, 0))
plot(as.raster(img)); title("Imagem")
mapa <- matrix("#F4F1EA", nrow(raiz), ncol(raiz))
mapa[raiz] <- "#D9CBB8"; mapa[esqueleto] <- "#10116F"
plot(as.raster(mapa)); title("Esqueleto")

## ---- raiz-validacao ----
# 12 imagens com comprimento conhecido, do mais simples ao mais denso
validacao <- do.call(rbind, lapply(1:12, function(i) {
  arq <- sprintf("raiz_val_%02d.png", i)
  real <- simular_raiz(arq, n_laterais = 5 + 4 * i, semente = 100 + i)
  r <- apply(readPNG(arq)[, , 1:3], c(1, 2), mean) < 0.5
  data.frame(imagem = i, real = real * cm_por_px,
             estimado = comprimento_px(afinar(r)) * cm_por_px)
}))

# concordância de Lin (CCC): 1 é concordância perfeita
ccc <- with(validacao, 2 * cov(real, estimado) /
              (var(real) + var(estimado) + (mean(real) - mean(estimado))^2))
erro <- with(validacao, mean((estimado - real) / real) * 100)
round(c(CCC = ccc, erro_medio_pct = erro), 3)

## ---- raiz-grafico ----
ggplot(validacao, aes(real, estimado)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey50") +
  geom_point(size = 3.5, colour = "#2F3BA6") +
  labs(x = "Comprimento real (cm)", y = "Comprimento estimado (cm)")

## ---- raiz-salvar ----
write.csv2(validacao, "validacao_raizes.csv", row.names = FALSE)
ggsave("validacao_raizes.png", width = 12, height = 10, units = "cm", dpi = 300)
