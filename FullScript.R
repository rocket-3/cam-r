library(dplyr)
library(corrr)

data <- tibble(read.csv("C:/w/all-minuses.csv"))
eoData <- tibble(read.csv("C:/w/eo-minuses.csv"))

withoutMinusRows <- function(df) {
  filter(df, MMAC != '-')
}

withMetricsMadeNumeric <- function(df) {
  errors <- c("CAMC", "CAMC.cvc.", "LCOM5", "LCOM5.cvc.", "MMAC", "MMAC.cvc.", "NHD", "NHD.cvc.", "SCOM", "SCOM.cvc.")
  cp <- df
  cp[errors] <- lapply(
    cp[errors],
    function(x) as.numeric(as.character(x))
  )
  cp
}

nrow(withMetricsMadeNumeric(
  withoutMinusRows(eoData)
))

nrow(na.omit(
  withMetricsMadeNumeric(
    withoutMinusRows(eoData)
  )
))

nrow(withMetricsMadeNumeric(
  withoutMinusRows(data)
))

nrow(na.omit(
  withMetricsMadeNumeric(
    withoutMinusRows(data)
  )
))

withInitialDataFixes <- function(df) {
  na.omit(
    withMetricsMadeNumeric(
      withoutMinusRows(df)
    )
  )
}

withoutMetricsErrors <- function(df) {
  cp <- df
  keys <- c("MMAC", "MMAC.cvc.", "CAMC", "CAMC.cvc.", "NHD", "NHD.cvc.", "SCOM", "SCOM.cvc.", "LCOM5", "LCOM5.cvc.")
  for (key in keys) {
    cp <- cp[!cp[key] > 1.0,]
    cp <- cp[!cp[key] < 0.0,]
  }
  cp
}

outliers <- function(x) {
  Q1 <- quantile(x, probs = .1)
  Q3 <- quantile(x, probs = .9)
  iqr <- Q3 - Q1
  upper_limit <- Q3 + (iqr * 1.5)
  lower_limit <- Q1 - (iqr * 1.5)
  x > upper_limit | x < lower_limit
}

withoutOutliers <- function(df, cols = names(df)) {
  cp <- df
  for (col in cols) {
    cp <- cp[!outliers(cp[[col]]),]
  }
  cp
}

withoutGroupColumn <- function(df) {
  df[c(-1)]
}

grouppedByProjectWithAllMetrics <- function(df) {
  df %>%
    group_by(repository) %>%
    summarise(
      project_loc = sum(ncss, na.rm = TRUE),
      file_loc = mean(ncss, na.rm = TRUE),
      branches = mean(cc, na.rm = TRUE),
      types = mean(coc, na.rm = TRUE),
      mtypes = mean(acoc, na.rm = TRUE),
      static = mean(smethods, na.rm = TRUE) + mean(sattributes, na.rm = TRUE),
      CAMC = mean(CAMC, na.rm = TRUE),
      LCOM5 = mean(LCOM5, na.rm = TRUE),
      MMAC = mean(MMAC, na.rm = TRUE),
      NHD = mean(NHD, na.rm = TRUE),
      SCOM = mean(SCOM, na.rm = TRUE),
      CAMC_A = mean(CAMC.cvc., na.rm = TRUE),
      LCOM5_A = mean(LCOM5.cvc., na.rm = TRUE),
      MMAC_A = mean(MMAC.cvc., na.rm = TRUE),
      NHD_A = mean(NHD.cvc., na.rm = TRUE),
      SCOM_A = mean(SCOM.cvc., na.rm = TRUE),
    ) %>%
    withoutGroupColumn()
}

reposAll <- grouppedByProjectWithAllMetrics(
  withoutMetricsErrors(
    withInitialDataFixes(data)
  )
)

reposEo <- grouppedByProjectWithAllMetrics(
  withoutMetricsErrors(
    withInitialDataFixes(eoData)
  )
)

groupped <- function(df) {
  df %>%
    group_by(file) %>%
    summarise(
    # project_loc = sum(ncss, na.rm = TRUE),
    file_loc = mean(ncss, na.rm = TRUE),
    branches = mean(cc, na.rm = TRUE),
    types = mean(coc, na.rm = TRUE),
    mtypes = mean(acoc, na.rm = TRUE),
    static = mean(smethods, na.rm = TRUE) + mean(sattributes, na.rm = TRUE),
    CAMC = mean(CAMC, na.rm = TRUE),
    LCOM5 = mean(LCOM5, na.rm = TRUE),
    MMAC = mean(MMAC, na.rm = TRUE),
    NHD = mean(NHD, na.rm = TRUE),
    SCOM = mean(SCOM, na.rm = TRUE),
    ) %>%
    withoutGroupColumn()
}

grouppedWithAltMetrics <- function(df) {
  df %>%
    group_by(file) %>%
    summarise(
    # project_loc = sum(ncss, na.rm = TRUE),
    file_loc = mean(ncss, na.rm = TRUE),
    branches = mean(cc, na.rm = TRUE),
    types = mean(coc, na.rm = TRUE),
    mtypes = mean(acoc, na.rm = TRUE),
    static = mean(smethods, na.rm = TRUE) + mean(sattributes, na.rm = TRUE),
    CAMC = mean(CAMC.cvc., na.rm = TRUE),
    LCOM5 = mean(LCOM5.cvc., na.rm = TRUE),
    MMAC = mean(MMAC.cvc., na.rm = TRUE),
    NHD = mean(NHD.cvc., na.rm = TRUE),
    SCOM = mean(SCOM.cvc., na.rm = TRUE),
    ) %>%
    withoutGroupColumn()
}

corChart <- function(df) {
  library(PerformanceAnalytics)
  chart.Correlation(
    df,
    histogram = TRUE
  )
}

netplot <- function(df) {
  df %>%
    correlate(method = "pearson") %>%
    network_plot(min_cor = 0.3)
}

chartPipelineDefault <- function(df) {
  corChart(
    groupped(
      withInitialDataFixes(df)
    )
  )
}

chartPipelineAlternative <- function(df) {
  corChart(
    grouppedWithAltMetrics(
      withInitialDataFixes(df)
    )
  )
}

chartPipelineDefaultFixed <- function(df) {
  corChart(
    groupped(
      withoutMetricsErrors(
        withInitialDataFixes(df)
      )
    )
  )
}

chartPipelineWithAltMetricsFixed <- function(df) {
  corChart(
    grouppedWithAltMetrics(
      withoutMetricsErrors(
        withInitialDataFixes(df)
      )
    )
  )
}


netplotPipeline <- function(df) {
  netplot(
    groupped(
      withInitialDataFixes(df)
    )
  )
}

netplotPipelineWithAltMetrics <- function(df) {
  netplot(
    grouppedWithAltMetrics(
      withInitialDataFixes(df)
    )
  )
}


netplotPipelineDefaultFixed <- function(df) {
  netplot(
    groupped(
      withoutMetricsErrors(
        withInitialDataFixes(df)
      )
    )
  )
}

nrow(
  na.omit(
    withMetricsMadeNumeric(
      withoutMinusRows(data)
    )
  )
)


nrow(
  withoutMetricsErrors(
    na.omit(
      withMetricsMadeNumeric(
        withoutMinusRows(data)
      )
    )
  )
)

netplotPipelineAltWithApprox <- function(df) {
  netplot(
    grouppedWithAltMetrics(
      withoutMetricsErrors(
        withInitialDataFixes(df)
      )
    )
  )
}

pdfDrawing <- function(name, drawing, bg = "transparent") {
  path <- file.path(name)
  pdf(file = path, bg = bg, useDingbats = TRUE, paper = "a4r", width = 1000)
  par(mfrow = c(1, 1))
  drawing()
  dev.off()
}

chartsDrawing <- function() {
  chartPipelineDefault(data)
  mtext("100", side = 3, padj = -5)

  chartPipelineAlternative(data)
  mtext("100 CVC", side = 3, padj = -5)

  chartPipelineDefaultFixed(data)
  mtext("100 FIXED", side = 3, padj = -5)

  chartPipelineWithAltMetricsFixed(data)
  mtext("100 CVC FIXED", side = 3, padj = -5)

  chartPipelineDefault(eoData)
  mtext("EO", side = 3, padj = -5)

  chartPipelineAlternative(eoData)
  mtext("EO CVC", side = 3, padj = -5)

  chartPipelineDefaultFixed(eoData)
  mtext("EO FIXED", side = 3, padj = -5)

  chartPipelineWithAltMetricsFixed(eoData)
  mtext("EO CVC FIXED", side = 3, padj = -5)
}

netplotsDrawing <- function() {
  plot.new()
  dev.flush()
  netplotPipeline(data)
  dev.flush()
  title(main = "100")

  netplotPipelineWithAltMetrics(data)
  title(main = "100 CVC")

  netplotPipelineDefaultFixed(data)
  title(main = "100 FIXED")

  netplotPipelineAltWithApprox(data)
  title("100 CVC FIXED")
}

boxplotsDrawing <- function() {

  par(
    mfrow = c(2, 6),
    mgp = c(4, 1.5, 0),
    mar = c(4, 5, 3, 2) + 0
  )

  i <- 1
  labs <- function(i, key) {
    val <- c(
      "Project lines", "File lines", "Branches",
      "Class types", "Method types", "Static elements"
    )[i]
    if (is.na(val))val <- key
    val
  }
  types <- function(i) {
    val <- c(
      "lines", "lines", "units",
      "units", "units", "units"
    )[i]
    if (is.na(val))val <- "value"
    val
  }

  for (key in names(reposAll)) {
    a <- reposAll %>% pull(key)
    b <- reposEo %>% pull(key)
    C <- list(a, b)

    names(C) <- c(
      paste0("Github\n n=", length(a)),
      paste0("EO\n n=", length(b))
    )

    boxplot(C, col = "white", main = labs(i, key), ylab=types(i),  yaxt = "n")
    axis(2, at = NULL, labels = T, las = 2)
    i <- i+1
  }
}

pdfDrawing("C:/w/repos-boxplots.pdf", boxplotsDrawing)
pdfDrawing("C:/w/files-charts.pdf", chartsDrawing)
pdfDrawing("C:/w/files-netplots.pdf", netplotsDrawing, "lightgrey")

