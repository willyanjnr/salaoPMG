# =============================================================================
# Cores e tema dos gráficos usados na apresentação e nos scripts de reprodução
# =============================================================================

library(ggplot2)

# Azul do Salão, secundária sóbria e laranja para alertas
cores <- c(azul = "#10116F", azul_medio = "#2F3BA6", secundaria = "#4F7F7A",
           laranja = "#C0661A", claro = "#ECEEFA", cinza = "#5F676C")

tema <- theme_minimal(base_size = 18) +
  theme(text = element_text(colour = "#20252B"),
        axis.text = element_text(colour = cores[["cinza"]]),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#E6E8E7", linewidth = 0.4),
        plot.title = element_text(face = "bold", colour = cores[["azul"]]),
        plot.caption = element_text(colour = cores[["cinza"]], size = 13))
theme_set(tema)

paleta3 <- c(cores[["azul"]], cores[["secundaria"]], cores[["laranja"]])
options(ggplot2.discrete.colour = paleta3, ggplot2.discrete.fill = paleta3)
