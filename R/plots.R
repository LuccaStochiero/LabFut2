#' Generate a Soccer Heatmap
#'
#' Esta função gera um mapa de calor de futebol baseado nos eventos de jogo.
#'
#' @param data Dados do jogo contendo informações sobre as jogadas.
#' @param x Nome da coluna no dataframe que contém as coordenadas x dos eventos.
#' @param y Nome da coluna no dataframe que contém as coordenadas y dos eventos.
#' @param tipo Tipo de evento para o mapa de calor. Pode ser "Chute", "Passe" ou "Tudo".
#' @param jogo ID do jogo a ser filtrado (opcional).
#' @param time Nome do time a ser filtrado (opcional).
#' @param jogador Nome do jogador a ser filtrado (opcional).
#' @param xBins Número de divisões (bins) no eixo x para o mapa de calor.
#' @param yBins Número de divisões (bins) no eixo y para o mapa de calor.
#' @param arrow Direção das setas a serem usadas no gráfico (opcional).
#' @param colLow Cor para os valores baixos de densidade.
#' @param colHigh Cor para os valores altos de densidade.
#'
#' @return Um gráfico de mapa de calor gerado pela função `soccerHeatmap`.
#'
#' @export
generateSoccerHeatmap <- function(data, x, y, tipo = "Tudo", jogo = NULL, time = NULL, jogador = NULL,
                                  xBins = 18, yBins = 18, arrow = c("r"), colLow = "grey", colHigh = "darkred") {

  # Verifica se o tipo é válido
  if (!tipo %in% c("Chute", "Passe", "Tudo")) {
    stop("O argumento 'tipo' deve ser 'Chute', 'Passe' ou 'Tudo'.")
  }

  # Seleciona as colunas necessárias do dataframe
  filtered_data <- data %>%
    dplyr::select(game_id, team, player, dplyr::all_of(x), dplyr::all_of(y), type)

  # Filtra os dados com base no tipo de evento
  if (tipo == "Chute") {
    filtered_data <- filtered_data %>%
      dplyr::filter(type %in% c('SavedShot', 'MissedShot', 'Goal', 'ShotOnPost'))
  } else if (tipo == "Passe") {
    filtered_data <- filtered_data %>%
      dplyr::filter(type == 'Pass')
  }

  # Filtra os dados com base no jogo, time e jogador, se fornecido
  if (!is.null(jogo)) {
    filtered_data <- filtered_data %>%
      dplyr::filter(game_id == jogo)
  }

  if (!is.null(time)) {
    filtered_data <- filtered_data %>%
      dplyr::filter(team == time)
  }

  if (!is.null(jogador)) {
    filtered_data <- filtered_data %>%
      dplyr::filter(player == jogador)
  }

  # Gera o mapa de calor usando a função soccerHeatmap
  soccerHeatmap::soccerHeatmap(df = filtered_data,
                               lengthPitch = 105,
                               widthPitch = 68,
                               xBins = xBins,
                               yBins = yBins,
                               kde = FALSE,
                               arrow = arrow,
                               colLow = colLow,
                               colHigh = colHigh,
                               x = x,
                               y = y)
}


#' Generate a Soccer Map
#'
#' Esta função gera um gráfico com os eventos de futebol no campo, incluindo passes e chutes.
#'
#' @param data Dados do jogo contendo informações sobre as jogadas.
#' @param x Nome da coluna no dataframe que contém as coordenadas x dos eventos.
#' @param y Nome da coluna no dataframe que contém as coordenadas y dos eventos.
#' @param endx Nome da coluna no dataframe que contém as coordenadas x do destino (para passes).
#' @param endy Nome da coluna no dataframe que contém as coordenadas y do destino (para passes).
#' @param tipo Tipo de evento a ser plotado. Pode ser "Passe", "Chute" ou "Defensivo".
#' @param jogo ID do jogo a ser filtrado (opcional).
#' @param time Nome do time a ser filtrado (opcional).
#' @param jogador Nome do jogador a ser filtrado (opcional).
#'
#' @return Um gráfico de eventos de futebol no campo gerado pela função `ggplot2::ggplot`.
#'
#' @export
generateSoccerMap <- function(data, x, y, endx = NULL, endy = NULL,
                              tipo = "Pass", jogo = NULL, time = NULL, jogador = NULL) {

  # Verifica se o tipo é válido
  if (!tipo %in% c("Passe", "Chute", "Defensivo")) {
    stop("Tipo inválido. Escolha 'Passe', 'Chute' ou 'Defensivo'.")
  }

  # Verifica se endx e endy são fornecidos para o tipo "Passe"
  if (tipo == "Passe" && (is.null(endx) || is.null(endy))) {
    stop("Para passes, você deve especificar as colunas 'endx' e 'endy'.")
  }

  # Filtra os dados conforme os parâmetros fornecidos
  filtered_data <- data %>%
    dplyr::select(game_id, team, player, dplyr::all_of(x), dplyr::all_of(y),
                  dplyr::all_of(endx), dplyr::all_of(endy), type, outcome_type)

  if (!is.null(jogo)) {
    filtered_data <- filtered_data %>%
      dplyr::filter(game_id == jogo)
  }

  if (!is.null(time)) {
    filtered_data <- filtered_data %>%
      dplyr::filter(team == time)
  }

  if (!is.null(jogador)) {
    filtered_data <- filtered_data %>%
      dplyr::filter(player == jogador)
  }

  # Verifica se há dados após o filtro
  if (nrow(filtered_data) == 0) {
    stop("Nenhum dado disponível após os filtros aplicados.")
  }

  # Cria o gráfico base com o campo de futebol
  plot <- soccerPitch::soccerPitch(lengthPitch = 105, widthPitch = 68, arrow = "r", theme = "outlier")

  # Adiciona os eventos ao gráfico com base no tipo
  if (tipo == "Passe") {
    plot <- plot +
      ggplot2::geom_segment(data = filtered_data,
                            ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y),
                                         xend = !!rlang::sym(endx), yend = !!rlang::sym(endy),
                                         color = factor(outcome_type)),
                            arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "cm")),
                            size = 1) +
      ggplot2::geom_point(data = filtered_data,
                          ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y),
                                       fill = factor(outcome_type)),
                          color = "white",
                          pch = 21,
                          size = 3,
                          stroke = 1.5)
  } else {
    plot <- plot +
      ggplot2::geom_point(data = filtered_data,
                          ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y),
                                       color = factor(outcome_type)),
                          size = 3)
  }

  return(plot)
}
