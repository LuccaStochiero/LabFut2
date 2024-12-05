#' Plota um mapa de calor no campo de futebol
#'
#' Esta função cria um mapa de calor (zonal ou baseado em densidade de Kernel) sobre o campo de futebol, utilizando coordenadas de localização no campo.
#'
#' @param df DataFrame com as coordenadas dos eventos no campo, com as colunas `x` e `y` representando as posições.
#' @param lengthPitch Comprimento do campo de futebol (padrão 105 metros).
#' @param widthPitch Largura do campo de futebol (padrão 68 metros).
#' @param xBins Número de bins horizontais para o mapa de calor (padrão 10).
#' @param yBins Número de bins verticais para o mapa de calor. Se `NULL`, é igual a `xBins`.
#' @param kde Se `TRUE`, plota o mapa de calor utilizando uma estimativa de densidade de Kernel.
#' @param arrow Controla a direção das setas no campo, opções: "none", "r", "l" (padrão "none").
#' @param colLow Cor do valor mais baixo do mapa de calor (padrão "white").
#' @param colHigh Cor do valor mais alto do mapa de calor (padrão "red").
#' @param title Título do gráfico.
#' @param subtitle Subtítulo do gráfico.
#' @param x Nome da coluna que representa a coordenada `x` (padrão "x").
#' @param y Nome da coluna que representa a coordenada `y` (padrão "y").
#'
#' @return Um gráfico do mapa de calor sobre o campo de futebol.
#' @export
soccerHeatmap <- function(df, lengthPitch = 105, widthPitch = 68, xBins = 10, yBins = NULL, kde = FALSE, arrow = c("none", "r", "l"), colLow = "white", colHigh = "red", title = NULL, subtitle = NULL, x = "x", y = "y") {

  # Certifica-se de que a entrada seja um DataFrame
  df <- as.data.frame(df)

  # Renomeia as variáveis conforme os parâmetros x e y
  df$x <- df[,x]
  df$y <- df[,y]

  # Mapa de calor zonal
  if (!kde) {
    # Verifica se yBins é NULL e define seu valor igual a xBins
    if (is.null(yBins)) yBins <- xBins

    # Filtra valores inválidos fora dos limites do campo
    df <- df[df$x > 0 & df$x < lengthPitch & df$y > 0 & df$y < widthPitch,]

    # Define os intervalos dos bins
    x.range <- seq(0, lengthPitch, length.out = xBins + 1)
    y.range <- seq(0, widthPitch, length.out = yBins + 1)

    # Cria o gráfico com o campo de futebol e o mapa de calor zonal
    p <- soccerPitch::soccerPitch(lengthPitch, widthPitch, arrow = arrow, title = title, subtitle = subtitle, theme = "grass") +
      ggplot2::geom_bin2d(data = df, ggplot2::aes(x, y, alpha = ..count..), binwidth = c(diff(x.range)[1], diff(y.range)[1])) +
      ggplot2::scale_fill_gradient(low = colLow, high = colHigh) +
      ggplot2::scale_alpha_continuous(range = c(0.95, 0.95)) +
      ggplot2::guides(fill = "none") +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = 'none') +
      ggplot2::scale_y_reverse()

    # Desenha as linhas do campo
    p <- soccerPitch::soccerPitchFG(p, title = !is.null(subtitle), subtitle = !is.null(title), colPitch = NA)

    # Mapa de calor baseado em estimativa de densidade de Kernel
  } else {
    dens <- MASS::kde2d(df$x, df$y, n = 200, lims = c(c(0.25, lengthPitch - 0.25), c(0.25, widthPitch - 0.25)))
    dens_df <- data.frame(expand.grid(x = dens$x, y = dens$y), z = as.vector(dens$z))

    # Cria o gráfico com o campo de futebol e o mapa de calor de Kernel
    p <- soccerPitch::soccerPitch(lengthPitch, widthPitch, arrow = arrow, title = title, subtitle = subtitle, theme = "grass") +
      ggplot2::geom_tile(data = dens_df, ggplot2::aes(x = x, y = y, fill = z)) +
      ggplot2::scale_fill_distiller(palette = "Spectral", na.value = "white") +
      ggplot2::guides(fill = "none") +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = 'none') +
      ggplot2::scale_y_reverse()

    p <- soccerPitch::soccerPitchFG(p, title = !is.null(subtitle), subtitle = !is.null(title))
  }

  return(p)
}
