#' Correção de um valor pela TLm - Taxa Legal
#'
#' @description
#' \code{correct_TLm()} usa dados do IPEADATA para calcular o valor atualizado pela TLm.
#'
#' @param valor Valor a ser reajustado (default é 1000.00).
#' @param dtInicio Data de início da atualização.
#' @param dtFim Data final da atualização.
#'
#' @references Os índeces podem ser acessados através do acesso ao sítio do IPEA Data
#' \url{http://www.ipeadata.gov.br/}.
#'
#' @return A \code{tibble}, com os valor corrigido, e os índices.
#'
#' @examples
#' # Exemplo, corrigindo pela TLm:
#' # df <- correct_interest(valor = 1000000, dtInicio = "01/01/2003",dtFim = Sys.Date())
#'
#' @importFrom lubridate %m-%
#' @export
correct_TLm <- function(valor = 1000000, dt_inicio = "02/01/2008", dt_fim = "30/08/2024") {
  library(lubridate)
  library(dplyr)
  library(httr)
  library(tibble)
  
  ajustar_data <- function(date) {
    if (!grepl("^\\d{2}/\\d{2}/\\d{4}$", date)) stop("'inicio' tem que estar no formato DD/MM/YYYY.")
    dmy(date)
  }
  
  baixar_dados_ipea <- function(url, series_code) {
    res <- GET(url)
    stop_for_status(res, task = paste("Buscando os dados da série", series_code, "da API do IPEA."))
    data <- content(res)[[2]]
    df <- bind_rows(data)[, 2:3]
    df$VALDATA <- as_date(df$VALDATA)
    return(df)
  }
  
  # Ajustar datas de início e fim
  dt_inicio <- ajustar_data(dt_inicio)
  dt_fim <- ajustar_data(dt_fim)
  
  message("\nDownloading os dados do IPEA data API\n...\n")
  
  # Baixar dados do IPCA-15 e SELIC
  ipca <- baixar_dados_ipea("http://ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='PRECOS12_IPCA15G12')", "IPCA-15") |> 
    select(fim_mes = VALDATA, ipca_15 = VALVALOR) |> 
    mutate(fim_mes = ceiling_date(fim_mes, "month") - days(1))
  
  selic <- baixar_dados_ipea("http://ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='GM366_TJOVER366')", "SELIC") |> 
    select(database = VALDATA, selic = VALVALOR) |> 
    filter(!is.na(selic)) |> 
    mutate(fator_diario = (1 + selic / 100)^(1 / 252),
           fator_acumulado = cumprod(fator_diario))
   
  # Calcular SELIC mensal
    selic_mensal <- selic |> 
      dplyr::mutate(fim_mes = lubridate::ceiling_date(database, "month")-lubridate::days(1)) |> 
      dplyr::group_by(fim_mes) |> 
      dplyr::mutate(selic_mes = cumprod(fator_diario)-1, selic_mes = selic_mes*100) |> 
      dplyr::filter(max(database)==database) |>
      dplyr::mutate(fim_mes = lubridate::ceiling_date(fim_mes, "month")-lubridate::days(1)) |>
      dplyr::select(fim_mes, selic_mes)

  # Ajustar meses inicial e final
  mes_inicial <- if_else(day(dt_inicio) == 1, ceiling_date(dt_inicio, "month") - days(1), 
                         ceiling_date(ceiling_date(dt_inicio, "month"), "month") - days(1))
  mes_final <- if_else(dt_fim == ceiling_date(dt_fim, "month") - days(1), dt_fim,
                       floor_date(dt_fim, "month") - days(1))
  
  # Criar base TLm
  base_tlm <- ipca |> 
    left_join(selic_mensal, by = "fim_mes") |> 
    mutate(TLm = ((selic_mes / 100 + 1) / (ipca_15 / 100 + 1) - 1) * 100,
           mes_referencia = ceiling_date(fim_mes + 1, "month") - days(1),
           TLm_dia = TLm / day(mes_referencia)) |> 
    rename(mes_base = fim_mes)
  
  # base_tlm |> FGLUtils::to_excel()
  # Criar base mensal
  base_mensal <- tibble(mes_base = seq(mes_inicial, mes_final, by = "month")) |> 
    mutate(mes_base = ceiling_date(mes_base, "month") - days(1)) |> 
    left_join(base_tlm, by = "mes_base") |> 
    mutate(observacao = "mês completo")
  
  # Função para gerar base diária
  gerar_base_diaria <- function(data_inicio, data_fim) {
    seq(data_inicio, data_fim, by = "day") |> 
      as_tibble_col("data_base") |> 
      mutate(observacao = paste("pro-rata dia:", data_base),
             mes_referencia = ceiling_date(data_fim, "month") - days(1)) |> 
      left_join(base_tlm, by = c("mes_referencia" = "mes_base")) |> 
      select(mes_base = mes_referencia, TLm = TLm_dia, observacao)
  }
  
  dias_iniciais <- if (day(dt_inicio) > 1) gerar_base_diaria(dt_inicio, ceiling_date(dt_inicio, "month") - days(1)) else tibble()
  dias_finais <- if (dt_fim != ceiling_date(dt_fim, "month") - days(1)) gerar_base_diaria(floor_date(dt_fim, "month"), dt_fim) else tibble()
  
  df <- bind_rows(dias_iniciais, base_mensal, dias_finais)
  
  return(df)
}
