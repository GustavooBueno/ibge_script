library(magrittr, include.only = '%>%')

#WEBSCRAPING----
link_dados_brasil <- "https://cidades.ibge.gov.br/brasil/panorama"
page_dados_brasil <- rvest::read_html(link_dados_brasil)

info_brasil <- page_dados_brasil %>%
  rvest::html_nodes(".lista__nome") %>%
  rvest::html_text()

dados_brasil <- page_dados_brasil %>%
  rvest::html_nodes(".lista__valor div") %>%
  rvest::html_text()



#CRIANDO DATAFRAME----
df_brasil <- data.frame(info_brasil, dados_brasil)
tabela_brasil <- df_brasil


#TRATANDO OS DADOS DA COLUNA INFO_BRASIL----
#TIRANDO ESPAÇOS
tabela_brasil$info_brasil <- stringr::str_trim(tabela_brasil$info_brasil)

#CRIANDO COLUNA ANO
tabela_brasil$ano <- stringr::str_sub(tabela_brasil$info_brasil,
                                      start = -5)
#TIRANDO O ] DA COLUNA ANO
tabela_brasil$ano <- stringr::str_remove(tabela_brasil$ano,
                                         pattern = '\\]')

#REMOVENDO ANO DA COLUNA 'info_brasil'
tabela_brasil$info_brasil <- stringr::str_remove_all(tabela_brasil$info_brasil,
                                                     '\\[|\\]|2015|2019|[1000-3000]|
                                          |janeiro| fevereiro|março|abril|maio|
                                          |junho|julho|agosto|setembro|outubro|
                                          |novembro|dezembro|
                                          |º trimestre|º semestre')

#TRATANDO OS DADOS DA COLUNA DADOS_BRASIL----
#TIRANDO ESPAÇOS
tabela_brasil$dados_brasil <- stringr::str_trim(tabela_brasil$dados_brasil)

#CRIANDO COLUNA UNIDADE
tabela_brasil$unidade <- stringr::str_extract_all(tabela_brasil$dados_brasil,
                                                  pattern =
                                                    '%|pessoas|filhos por mulher|
                                                  |óbitos por mil nascidos vivos|
                                                  |matrículas|docentes|
                                                  |escolas|R\\$')

#REMOVENDO UNIDADES DA COLUNA 'dados_brasil'
tabela_brasil$dados_brasil <- stringr::str_remove_all(tabela_brasil$dados_brasil,
                                                      pattern =
                                                        '[:alpha:]|[:symbol:]|%')

#MELHORANDO AS LINHAS DA COLUNA 'unidade'
tabela_brasil <- tabela_brasil %>%
  dplyr::mutate(unidade = dplyr::case_when(unidade == '%' ~ 'porcento',
                                           unidade == 'R$' ~ 'reais',
                                           unidade == 'character(0)' ~ '-',
                                           TRUE ~ as.character(unidade)))
