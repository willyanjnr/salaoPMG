# =============================================================================
# Análises básicas para publicação, do início ao fim, com um mesmo ensaio
# -----------------------------------------------------------------------------
# Ensaio simulado de soja em blocos casualizados (DBC), fatorial 4 x 4:
#   4 cultivares x 4 densidades de semeadura (20, 30, 40 e 50 plantas m-2)
#   4 blocos, 64 parcelas
# Variáveis: produtividade (PROD, kg ha-1), estande final (PLANTAS, plantas m-2),
# vagens por planta (NVP), grãos por vagem (GPV), massa de mil grãos (MMG, g),
# altura de planta (ALT, cm), proteína (PTN, %) e óleo (OLEO, %).
#
# Roteiro
#   1. Dados
#   2. Estatística descritiva
#   3. ANOVA fatorial e pressupostos
#   4. Teste de médias (Tukey)
#   5. Regressão (resposta à densidade)
#   6. Correlação
#   7. Regressão múltipla stepwise
#   8. Componentes principais (PCA)
#   9. Salvar tabelas e figuras
#
# Os dados são simulados. Troque a seção 1 pela leitura da sua planilha, com
# as mesmas colunas, e o restante do script roda igual.
# =============================================================================

# --- Pacotes (instala só os que faltarem) -------------------------------------
pacotes <- c("ggplot2", "car", "emmeans", "multcomp", "multcompView",
             "MASS", "corrplot", "factoextra", "patchwork")
faltam <- setdiff(pacotes, rownames(installed.packages()))
if (length(faltam) > 0) install.packages(faltam)
invisible(lapply(pacotes, library, character.only = TRUE))

theme_set(theme_minimal(base_size = 14))
cores4 <- c("#10116F", "#4F7F7A", "#C0661A", "#7A6FB0")


# =============================================================================
# 1. Dados
# =============================================================================
set.seed(2026)

ensaio <- expand.grid(BLOCO = paste0("B", 1:4),
                      CULTIVAR = c("C1", "C2", "C3", "C4"),
                      DENS = c(20, 30, 40, 50))

# efeitos de cultivar e de bloco
ef_nvp <- c(C1 = 5, C2 = 1, C3 = -4, C4 = -2)     # vagens por planta
ef_mmg <- c(C1 = 6, C2 = 12, C3 = -10, C4 = -8)   # massa de mil grãos (g)
ef_bl  <- c(B1 = 0.8, B2 = 0, B3 = -0.5, B4 = -0.3)

n <- nrow(ensaio)
cv <- as.character(ensaio$CULTIVAR); bl <- as.character(ensaio$BLOCO)

# estande final: plantas que sobreviveram até a colheita
ensaio$PLANTAS <- round(ensaio$DENS * runif(n, 0.85, 0.97), 1)

# componentes do rendimento: com mais plantas, cada planta produz menos vagens
ensaio$NVP <- round(62 * exp(-0.03 * (ensaio$PLANTAS - 18)) + ef_nvp[cv] +
                      ef_bl[bl] + rnorm(n, 0, 2), 1)
ensaio$GPV <- round(2.3 + rnorm(n, 0, 0.08), 2)
ensaio$MMG <- round(150 + ef_mmg[cv] - 0.15 * (ensaio$PLANTAS - 18) +
                      rnorm(n, 0, 4), 1)
ensaio$ALT <- round(68 + 0.35 * ensaio$PLANTAS + c(C1 = -4, C2 = 2, C3 = 5, C4 = 0)[cv] +
                      rnorm(n, 0, 3), 1)

# produtividade (kg ha-1) = plantas m-2 x vagens x grãos x massa do grão x 10
ensaio$PROD <- round(ensaio$PLANTAS * ensaio$NVP * ensaio$GPV * ensaio$MMG / 1000 * 10 *
                       (1 + rnorm(n, 0, 0.03)))

# qualidade: proteína e óleo variam em sentidos opostos
ensaio$PTN  <- round(38 + c(C1 = 1.2, C2 = 0, C3 = -0.8, C4 = 0.4)[cv] -
                       0.0006 * (ensaio$PROD - 4000) + rnorm(n, 0, 0.6), 2)
ensaio$OLEO <- round(41.5 - 0.55 * ensaio$PTN + rnorm(n, 0, 0.35), 2)

# fatores para a ANOVA; DENS também fica numérica para a regressão
ensaio$BLOCO    <- factor(ensaio$BLOCO)
ensaio$CULTIVAR <- factor(ensaio$CULTIVAR)
ensaio$DENS_F   <- factor(ensaio$DENS)

head(ensaio)
str(ensaio)

# Com dados reais:
# ensaio <- read.csv2("meu_ensaio.csv")   # ou readxl::read_excel("meu_ensaio.xlsx")


# =============================================================================
# 2. Estatística descritiva
# =============================================================================
variaveis <- c("PROD", "PLANTAS", "NVP", "GPV", "MMG", "ALT", "PTN", "OLEO")

descritiva <- data.frame(
  variavel = variaveis,
  media  = sapply(ensaio[variaveis], mean),
  dp     = sapply(ensaio[variaveis], sd),
  minimo = sapply(ensaio[variaveis], min),
  maximo = sapply(ensaio[variaveis], max)
)
descritiva$cv_pct <- descritiva$dp / descritiva$media * 100
print(descritiva, digits = 3, row.names = FALSE)

# médias por cultivar e densidade
aggregate(PROD ~ CULTIVAR + DENS, data = ensaio, FUN = mean)

g_box <- ggplot(ensaio, aes(DENS_F, PROD, fill = CULTIVAR)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = cores4) +
  labs(x = "Densidade (plantas m-2)", y = "Produtividade (kg ha-1)", fill = "Cultivar")
g_box


# =============================================================================
# 3. ANOVA fatorial em blocos casualizados e pressupostos
# =============================================================================
modelo <- aov(PROD ~ BLOCO + CULTIVAR * DENS_F, data = ensaio)
summary(modelo)

# coeficiente de variação experimental
qm_res <- summary(modelo)[[1]]["Residuals", "Mean Sq"]
cv_exp <- sqrt(qm_res) / mean(ensaio$PROD) * 100
cat("CV experimental:", round(cv_exp, 2), "%\n")

# pressupostos (p > 0,05 indica que o pressuposto não foi rejeitado)
# Se a normalidade falhar, olhe o gráfico QQ antes de decidir. Desvios leves
# pouco afetam a ANOVA. Desvios fortes pedem transformação ou modelo
# generalizado (glm).
shapiro.test(residuals(modelo))                                 # normalidade
leveneTest(PROD ~ CULTIVAR * DENS_F, data = ensaio)             # homogeneidade

par(mfrow = c(1, 2))
plot(modelo, which = 1:2)                                       # resíduos e QQ
par(mfrow = c(1, 1))

# a mesma ANOVA para todas as variáveis, com o valor-p de cada efeito
anova_todas <- do.call(rbind, lapply(variaveis, function(v) {
  m <- aov(reformulate("BLOCO + CULTIVAR * DENS_F", v), data = ensaio)
  tab <- summary(m)[[1]]
  rownames(tab) <- trimws(rownames(tab))
  data.frame(variavel = v,
             p_cultivar   = tab["CULTIVAR", "Pr(>F)"],
             p_densidade  = tab["DENS_F", "Pr(>F)"],
             p_interacao  = tab["CULTIVAR:DENS_F", "Pr(>F)"],
             cv_pct = sqrt(tab["Residuals", "Mean Sq"]) / mean(ensaio[[v]]) * 100)
}))
print(anova_todas, digits = 3, row.names = FALSE)


# =============================================================================
# 4. Teste de médias (Tukey)
# =============================================================================
# Se a interação não for significativa, compara-se cada fator isoladamente.
# Se for, comparam-se as cultivares dentro de cada densidade (by = "DENS_F").
medias_cv <- emmeans(modelo, ~ CULTIVAR)
letras_cv <- cld(medias_cv, adjust = "tukey", Letters = letters, decreasing = TRUE)
letras_cv

# letras sem espaços, para o gráfico
letras_cv$.group <- trimws(letras_cv$.group)

g_tukey <- ggplot(letras_cv, aes(reorder(CULTIVAR, -emmean), emmean)) +
  geom_col(fill = "#4F7F7A", width = 0.6) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.15) +
  geom_text(aes(y = emmean + SE, label = .group), vjust = -0.6, size = 5) +
  labs(x = "Cultivar", y = "Produtividade média (kg ha-1)",
       caption = "Médias seguidas da mesma letra não diferem pelo teste de Tukey (5%).")
g_tukey

# desdobramento, caso a interação seja significativa
cld(emmeans(modelo, ~ CULTIVAR | DENS_F), adjust = "tukey",
    Letters = letters, decreasing = TRUE)


# =============================================================================
# 5. Regressão: resposta da produtividade à densidade
# =============================================================================
# Densidade é quantitativa, então o mais indicado é a regressão, e não Tukey.
reg_lin  <- lm(PROD ~ DENS, data = ensaio)
reg_quad <- lm(PROD ~ DENS + I(DENS^2), data = ensaio)
anova(reg_lin, reg_quad)          # o termo quadrático melhora o ajuste?
summary(reg_quad)

b <- coef(reg_quad)
dens_max <- -b[2] / (2 * b[3])                      # ponto de máxima
prod_max <- b[1] + b[2] * dens_max + b[3] * dens_max^2
cat("Densidade de máxima produtividade:", round(dens_max, 1), "plantas m-2\n")
cat("Produtividade estimada no ponto de máxima:", round(prod_max), "kg ha-1\n")

r2 <- summary(reg_quad)$r.squared
equacao <- sprintf("y = %.0f + %.1fx %.3fx²   R² = %.2f", b[1], b[2], b[3], r2)
equacao <- gsub(".", ",", gsub("+ -", "- ", equacao, fixed = TRUE), fixed = TRUE)

g_reg <- ggplot(ensaio, aes(DENS, PROD)) +
  geom_point(colour = "grey55", alpha = 0.6,
             position = position_jitter(width = 0.6, height = 0)) +
  stat_summary(fun = mean, geom = "point", size = 3.5, colour = "#10116F") +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), colour = "#C0661A", se = TRUE) +
  geom_vline(xintercept = dens_max, linetype = 2, colour = "grey40") +
  annotate("text", x = 20, y = max(ensaio$PROD) * 1.03, label = equacao,
           hjust = 0, size = 4.5) +
  labs(x = "Densidade de semeadura (plantas m-2)", y = "Produtividade (kg ha-1)")
g_reg


# =============================================================================
# 6. Correlação de Pearson
# =============================================================================
matriz_cor <- cor(ensaio[variaveis])
round(matriz_cor, 2)

# valor-p de cada par
p_cor <- outer(variaveis, variaveis, Vectorize(function(a, b)
  cor.test(ensaio[[a]], ensaio[[b]])$p.value))
dimnames(p_cor) <- list(variaveis, variaveis)
round(p_cor, 4)

corrplot(matriz_cor, method = "color", type = "lower", addCoef.col = "black",
         tl.col = "black", number.cex = 0.8, p.mat = p_cor, sig.level = 0.05,
         insig = "blank", col = colorRampPalette(c("#C0661A", "white", "#10116F"))(200))
# células em branco: correlação não significativa (p > 0,05)


# =============================================================================
# 7. Regressão múltipla stepwise
# =============================================================================
# Quais caracteres explicam a produtividade?
completo <- lm(PROD ~ PLANTAS + NVP + GPV + MMG + ALT + PTN + OLEO, data = ensaio)
vif(completo)                    # multicolinearidade: valores acima de 10 preocupam

passo <- stepAIC(completo, direction = "both", trace = FALSE)
summary(passo)
passo$anova                      # variáveis retiradas em cada passo

# coeficientes padronizados, para comparar a importância das variáveis
padronizado <- lm(reformulate(attr(terms(passo), "term.labels"), "PROD"),
                  data = as.data.frame(scale(ensaio[variaveis])))
round(coef(padronizado)[-1], 3)

# Cuidado: o stepwise escolhe variáveis pelos dados e tende a superestimar o
# ajuste. Use para explorar e confirme com conhecimento agronômico.


# =============================================================================
# 8. Componentes principais (PCA)
# =============================================================================
pca <- prcomp(ensaio[variaveis], scale. = TRUE)   # escala: variáveis em unidades diferentes
summary(pca)                                       # variância explicada
round(pca$rotation[, 1:2], 3)                      # cargas em PC1 e PC2

g_var <- fviz_eig(pca, addlabels = TRUE, barfill = "#4F7F7A", barcolor = "#4F7F7A") +
  labs(title = NULL, x = "Componente", y = "Variância explicada (%)")
g_biplot <- fviz_pca_biplot(pca, habillage = ensaio$CULTIVAR, palette = cores4,
                            col.var = "black", repel = TRUE, addEllipses = TRUE,
                            ellipse.level = 0.68, label = "var") +
  labs(title = NULL, colour = "Cultivar", fill = "Cultivar", shape = "Cultivar") +
  coord_cartesian(clip = "off")
g_var + g_biplot + plot_layout(widths = c(1, 2))


# =============================================================================
# 9. Salvar tabelas e figuras
# =============================================================================
dir.create("resultados", showWarnings = FALSE)
write.csv2(descritiva,   "resultados/descritiva.csv",    row.names = FALSE)
write.csv2(anova_todas,  "resultados/anova_todas.csv",   row.names = FALSE)
write.csv2(as.data.frame(letras_cv), "resultados/tukey_cultivar.csv", row.names = FALSE)
write.csv2(round(matriz_cor, 3), "resultados/correlacao.csv")
write.csv2(round(pca$rotation, 3), "resultados/pca_cargas.csv")

ggsave("resultados/boxplot.png",   g_box,   width = 16, height = 10, units = "cm", dpi = 300, bg = "white")
ggsave("resultados/tukey.png",     g_tukey, width = 14, height = 10, units = "cm", dpi = 300, bg = "white")
ggsave("resultados/regressao.png", g_reg,   width = 16, height = 11, units = "cm", dpi = 300, bg = "white")
ggsave("resultados/pca.png", g_var + g_biplot + plot_layout(widths = c(1, 2)),
       width = 26, height = 11, units = "cm", dpi = 300, bg = "white")

png("resultados/correlacao.png", width = 16, height = 16, units = "cm", res = 300)
corrplot(matriz_cor, method = "color", type = "lower", addCoef.col = "black",
         tl.col = "black", number.cex = 0.8, p.mat = p_cor, sig.level = 0.05,
         insig = "blank", col = colorRampPalette(c("#C0661A", "white", "#10116F"))(200))
dev.off()

cat("Tabelas e figuras salvas na pasta 'resultados'.\n")
