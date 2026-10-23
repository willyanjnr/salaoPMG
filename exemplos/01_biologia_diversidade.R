# =============================================================================
# Biologia | Diversidade de espécies em três ambientes
# -----------------------------------------------------------------------------
# Dados simulados: contagem de 12 espécies em 5 parcelas por ambiente.
# Perguntas: a diversidade difere entre mata nativa, borda e lavoura?
# As parcelas se agrupam pelo ambiente de origem?
# Usa apenas o R básico.
# =============================================================================

## ---- bio-dados ----
set.seed(2026)
especies  <- paste0("sp", 1:12)
ambientes <- c("Mata nativa", "Borda", "Lavoura")

# abundância média esperada de cada espécie em cada ambiente
lambda <- rbind(c(20, 18, 15, 12, 10, 9, 8, 6, 5, 4, 3, 2),
                c(30, 20, 12,  8,  5, 4, 3, 2, 1, 1, 0.5, 0.5),
                c(60, 15,  5,  2,  1, 0.5, 0.2, 0.2, 0.1, 0, 0, 0))
dimnames(lambda) <- list(ambientes, especies)

dados <- expand.grid(parcela = 1:5, ambiente = ambientes,
                     especie = especies)
media <- lambda[cbind(as.character(dados$ambiente),
                      as.character(dados$especie))]
dados$n <- rpois(nrow(dados), media)
head(dados)

## ---- bio-indices ----
shannon <- function(n) { p <- n[n > 0] / sum(n); -sum(p * log(p)) }
simpson <- function(n) { p <- n / sum(n); 1 - sum(p^2) }

parcelas <- split(dados, ~ ambiente + parcela)
indices <- do.call(rbind, lapply(parcelas, function(d)
  data.frame(ambiente = d$ambiente[1], parcela = d$parcela[1],
             riqueza  = sum(d$n > 0),
             shannon  = shannon(d$n),
             simpson  = simpson(d$n))))

aggregate(cbind(riqueza, shannon, simpson) ~ ambiente,
          data = indices, FUN = mean)

## ---- bio-teste ----
modelo <- aov(shannon ~ ambiente, data = indices)
summary(modelo)

## ---- bio-tukey ----
TukeyHSD(modelo)

## ---- bio-matriz ----
# matriz parcelas x espécies
curto  <- c("Mata nativa" = "Mata", "Borda" = "Borda", "Lavoura" = "Lavoura")
rotulo <- paste(curto[as.character(dados$ambiente)], dados$parcela)
matriz <- xtabs(n ~ rotulo + especie, data = dados)

bray_curtis <- function(m) {
  d <- outer(seq_len(nrow(m)), seq_len(nrow(m)), Vectorize(function(i, j)
    sum(abs(m[i, ] - m[j, ])) / sum(m[i, ] + m[j, ])))
  dimnames(d) <- list(rownames(m), rownames(m))
  as.dist(d)
}
dist_bc <- bray_curtis(matriz)

## ---- bio-dendrograma ----
arvore <- hclust(dist_bc, method = "average")   # UPGMA
cor(cophenetic(arvore), dist_bc)                # correlação cofenética

par(mar = c(1, 5, 1, 1))
plot(arvore, hang = -1, main = "", sub = "", xlab = "",
     ylab = "Dissimilaridade de Bray-Curtis")
rect.hclust(arvore, k = 3, border = "#4F7F7A")

## ---- bio-salvar ----
png("dendrograma.png", width = 16, height = 10, units = "cm", res = 300)
par(mar = c(1, 5, 1, 1))
plot(arvore, hang = -1, main = "", sub = "", xlab = "",
     ylab = "Dissimilaridade de Bray-Curtis")
rect.hclust(arvore, k = 3, border = "#4F7F7A")
dev.off()
