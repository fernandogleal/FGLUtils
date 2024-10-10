#' Criar um Data Frame de Calendário
#'
#' Esta função gera um data frame de calendário com informações detalhadas de datas 
#' de uma data inicial especificada até uma data final. O calendário inclui várias 
#' colunas relacionadas a datas, como ano, mês, dia, dia da semana e dias úteis 
#' em um mês de acordo com o calendário "Brazil/ANBIMA".
#'
#' @param data_inicial Uma string de caracteres representando a data inicial no formato "YYYY/MM/DD". O padrão é "2001/01/01".
#' @param data_final Uma string de caracteres representando a data final no formato "YYYY/MM/DD". O padrão é "2040/12/31".
#'
#' @return Um tibble contendo os dados do calendário com as seguintes colunas:
#' \describe{
#'   \item{data}{Data no formato "YYYY-MM-DD".}
#'   \item{ano}{Ano extraído da data.}
#'   \item{mes}{Mês extraído da data.}
#'   \item{mes_nome_abr}{Nome abreviado do mês em português.}
#'   \item{des_mes_ano}{Nome abreviado do mês e os dois últimos dígitos do ano.}
#'   \item{mes_nome}{Nome completo do mês em português.}
#'   \item{mes_ano}{Mês e ano concatenados com um sublinhado.}
#'   \item{tri}{Trimestre do ano.}
#'   \item{dia}{Dia do mês.}
#'   \item{dia_semana}{Dia da semana como um número (1 = Domingo, 7 = Sábado).}
#'   \item{dia_semana_nome}{Nome completo do dia da semana em português.}
#'   \item{dia_semana_nome_abr}{Nome abreviado do dia da semana em português.}
#'   \item{du_mes}{Dia útil do mês de acordo com o calendário "Brazil/ANBIMA".}
#'   \item{ordem_mes_nome}{Número de ordem do grupo para cada combinação mês-ano.}
#' }
#'
#' @importFrom tibble as_tibble_col
#' @importFrom dplyr mutate
#' @importFrom lubridate year month quarter day wday floor_date ceiling_date
#' @importFrom stringr str_c str_sub str_pad
#' @importFrom janitor clean_names
#' @importFrom data.table data.table
#' @importFrom bizdays bizseq
#'
#' @examples
#' # Criar um calendário de 2021 a 2022
#' calendario <- create_dCalendario("2021/01/01", "2022/12/31")
#' head(calendario)
#'
#' @export
create_dCalendario <- function(data_inicial = "2001/01/01", data_final = "2040/12/31"){

# Create vectors with month and weekday names in Portuguese
month_names_pt <- c("janeiro", "fevereiro", "março", "abril", "maio", "junho", 
                    "julho", "agosto", "setembro", "outubro", "novembro", "dezembro")
month_names_abr_pt <- c("jan", "fev", "mar", "abr", "mai", "jun", 
                        "jul", "ago", "set", "out", "nov", "dez")
weekday_names_pt <- c("domingo", "segunda-feira", "terça-feira", "quarta-feira", 
                      "quinta-feira", "sexta-feira", "sábado")
weekday_names_abr_pt <- c("dom", "seg", "ter", "qua", "qui", "sex", "sáb")

# Generate the calendar data
dcalendario <- seq(as.Date(data_inicial), as.Date(data_final), "days") |> 
  tibble::as_tibble_col(column_name = "data") |> 
  dplyr::mutate(
    ano = lubridate::year(data), 
    mes = lubridate::month(data),
    mes_nome_abr = month_names_abr_pt[mes], 
    des_mes_ano = stringr::str_c(mes_nome_abr, stringr::str_sub(ano, 3, 4), sep = "/"),
    mes_nome = month_names_pt[mes], 
    mes_ano = stringr::str_pad(mes, 2, pad = "0") |> stringr::str_c("_", ano),
    tri = stringr::str_c("tri", lubridate::quarter(data), sep = " "),
    dia = lubridate::day(data),
    dia_semana = lubridate::wday(data),
    dia_semana_nome = weekday_names_pt[dia_semana],
    dia_semana_nome_abr = weekday_names_abr_pt[dia_semana]
  ) |> 
  janitor::clean_names() |> 
  dplyr::mutate(dplyr::across(c(mes_nome_abr,des_mes_ano,mes_nome,tri,dia_semana_nome,dia_semana_nome_abr), stringr::str_to_title))

dt <- data.table::data.table(dcalendario)

library(bizdays)

dt[, du_mes := {
  bizdays_in_month <- bizdays::bizseq(lubridate::floor_date(data, "month"), lubridate::ceiling_date(data, "month") - 1, cal = "Brazil/ANBIMA")
  ifelse(data %in% bizdays_in_month, match(data, bizdays_in_month), 0)
}, by = .(mes_ano)]  

dt[, ordem_mes_nome := .GRP, by = .(mes_ano)]

dCalendario <- tibble::as_tibble(dt)

return(dCalendario)
}