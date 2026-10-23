# Ferramentas em pacote R para pesquisa em processos biológicos e ambientais

Oficina do Salão do Conhecimento UNIJUÍ 2026 (PPGSAS), com Willyan Júnior Adorian
Bandeira, Ivan Ricardo Carvalho e José Antonio Gonzalez da Silva.

**Apresentação:** <https://willyanjnr.github.io/salaoPMG/>

## Reproduzir o que está na apresentação

Baixe este repositório (Code > Download ZIP), abra `salaoPMG.Rproj` no RStudio e
rode qualquer script da pasta `reproduzir/`:

| Script | Conteúdo |
|---|---|
| `01_trialSizing.R` | diagnóstico do campo, CV por formato de parcela, LRP, MCM, QRP, comparação entre métodos e número de repetições |
| `02_EstimateBreed.R` | ISGR, índice fototermal, risco de ferrugem, plastocrono, temperaturas cardinais, transgressivos e delta T |
| `03_graficos_ggplot2.R` | os mesmos dados com os gráficos básicos do R e com o ggplot2 |

```r
install.packages(c("trialSizing", "EstimateBreed", "ggplot2", "patchwork"))
```

## O R no lugar de programas pagos

Os scripts da pasta `exemplos/` rodam do início ao fim:

| Script | Tarefa | Programa pago comparado |
|---|---|---|
| `04_raizes_comprimento.R` | comprimento de raízes por imagem, com validação | WinRHIZO |
| `03b_fitopatologia_severidade.R` | área sadia, clorose e necrose em folha | WinDIAS |
| `03_fitopatologia_progresso.R` | AACPD, ANOVA e modelo logístico | SAS, Statistica |
| `02_ia_ocr_pdf.R` | texto e tabela de um PDF digitalizado | ABBYY FineReader |
| `01_biologia_diversidade.R` | diversidade e dendrograma de Bray-Curtis | PC-ORD, PRIMER |

```r
install.packages(c("png", "jpeg", "pdftools", "tesseract"))
```

Um script só substitui um programa pago depois de uma validação séria, com as
mesmas amostras medidas pelo script e pelo método de referência.

Os exemplos de OCR e de severidade entram na pasta `exemplos/` para ler os dados.
Para voltar a rodar os scripts de `reproduzir/`, reabra o projeto.

## Pacotes e aplicativos

* trialSizing: <https://willyanjnr.github.io/trialSizing/>
* EstimateBreed: <https://cran.r-project.org/package=EstimateBreed>
* GrainVision: <https://ivan10000.shinyapps.io/GrainVisionV3/>
* AmbientômicaRS: <https://ivan10000.shinyapps.io/AmbientomicaRS/>

## Créditos

A imagem `exemplos/dados/folha_soja.jpg` vem do pacote pliman (Olivoto, 2022,
licença GPL). O documento `exemplos/dados/boletim_digitalizado.pdf` é simulado,
com valores fictícios.
