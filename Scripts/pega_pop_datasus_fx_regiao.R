#' Tabnet_pop
#' @author Oswaldo G Cruz (ogcruz(at)gmail.com)
#' 
#' @param anoq  ano de 2000 a 2020
#' @param qsexo sexo na forma T = todos, M = Masculino, F= Feminino
#' @param qnivel nivel de agregação geografica, que pode ser:
#'                muni,municipio, município
#'                uf,estados,
#'                macro,macroregiao,macroregião
#'                regional,regional_saude,regional_saúde,cir
#'
#' @return tabela população por faixa etária
#' @export
#'
#' @examples 
#'           #retorna todos os anos de 2000 a 2020 total por município
#'           tabela_toda_fx_2000_2020 <- 2000:2020 %>% map_df(tabnet_pop,qsexo='T')
#'           
#'           # retorna a população de mulheres somente o ano de 2018 por UF
#'           tab <- tabnet_pop(2018,'F','UF')
#'           
#'           # população total por macro regiao de 2015 a 2020
#'           tabela-macro <- 2015:2020 %>% map(tabnet_pop,qnivel='macro')
#'           
#'           # população total por microregião IBGE de 2000 a 2020
#'           # tabela_micro <- 2000:2020 %>% map(tabnet_pop, qnivel='micro)
#'           # NÃO IMPLEMENTADO!
#' 

tabnet_pop <- function(anoq ='2020',qsexo='Todos',qnivel='muni') {
  
  require(tidyverse)
  require(httr)
  require(rvest) 
  
  if(anoq < '2000' | anoq > '2020' ) stop('Ano  de 2000 a 2020')
  
  #all <- 'TODAS_AS_CATEGORIAS__'
  
  all <-   case_when( qsexo == 'M' | qsexo == 'Masculino' ~ '1',
                      qsexo == 'F' | qsexo == 'Feminino' ~ '2' ,
                      qsexo == 'T' | qsexo == 'Todos' ~  'TODAS_AS_CATEGORIAS__',
                      TRUE ~  'ERRO' 
  )
  
  if(all == 'ERRO') stop('Sexo deve ser M (Masculino), F (Feminino) ou T (Todos)')
  
  
  nivel <- c("Munic%EDpio",
             "Unidade_da_Federa%E7%E3o",
             "Macrorregi%E3o_de_Sa%FAde",
             "Regi%E3o_de_Sa%FAde_%28CIR%29")
  ntam <- c(6,2,4,5)
  
  nreg <- case_when(
    tolower(qnivel) %in% c('muni','municipio','município') ~ 1 ,
    tolower(qnivel) %in% c('uf','estados') ~ 2 ,
    tolower(qnivel) %in% c('macro','macroregiao','macroregião') ~ 3,
    tolower(qnivel) %in% c('regional','regional_saude','regional_saúde','cir') ~ 4,
    # tolower(qnivel) %in% c('micro','microregiao','microregião') ~ 5,
    TRUE ~ 99
  )
  
  if(nreg == 99) stop('nome da região deve ser muni,uf,macro,regional  (ver documentação da função)')
  
  
  url <- "http://tabnet.datasus.gov.br/cgi/tabcgi.exe?popsvs/cnv/popbr.def"
  
  par <- "Linha=NREGZZZZ&Coluna=Faixa_Et%E1ria_1&Incremento=Popula%E7%E3o_residente&Arquivos=popXXXX.dbf&SRegi%E3o=TODAS_AS_CATEGORIAS__&pesqmes2=Digite+o+texto+e+ache+f%E1cil&SUnidade_da_Federa%E7%E3o=TODAS_AS_CATEGORIAS__&pesqmes3=Digite+o+texto+e+ache+f%E1cil&SMunic%EDpio=TODAS_AS_CATEGORIAS__&pesqmes4=Digite+o+texto+e+ache+f%E1cil&SCapital=TODAS_AS_CATEGORIAS__&pesqmes5=Digite+o+texto+e+ache+f%E1cil&SRegi%E3o_de_Sa%FAde_%28CIR%29=TODAS_AS_CATEGORIAS__&pesqmes6=Digite+o+texto+e+ache+f%E1cil&SMacrorregi%E3o_de_Sa%FAde=TODAS_AS_CATEGORIAS__&pesqmes7=Digite+o+texto+e+ache+f%E1cil&SMicrorregi%E3o_IBGE=TODAS_AS_CATEGORIAS__&pesqmes8=Digite+o+texto+e+ache+f%E1cil&SRegi%E3o_Metropolitana_-_RIDE=TODAS_AS_CATEGORIAS__&pesqmes9=Digite+o+texto+e+ache+f%E1cil&STerrit%F3rio_da_Cidadania=TODAS_AS_CATEGORIAS__&pesqmes10=Digite+o+texto+e+ache+f%E1cil&SMacrorregi%E3o_PNDR=TODAS_AS_CATEGORIAS__&SAmaz%F4nia_Legal=TODAS_AS_CATEGORIAS__&SSemi%E1rido=TODAS_AS_CATEGORIAS__&SFaixa_de_Fronteira=TODAS_AS_CATEGORIAS__&SZona_de_Fronteira=TODAS_AS_CATEGORIAS__&SMunic%EDpio_de_extrema_pobreza=TODAS_AS_CATEGORIAS__&SSexo=SEXOYYYY&pesqmes17=Digite+o+texto+e+ache+f%E1cil&SFaixa_Et%E1ria_1=TODAS_AS_CATEGORIAS__&pesqmes18=Digite+o+texto+e+ache+f%E1cil&SFaixa_Et%E1ria_2=TODAS_AS_CATEGORIAS__&formato=table&mostre=Mostra"
  
  
  ano_query <- str_sub(anoq,3,4)
  newpar <- str_replace(par,'XXXX',ano_query)
  newpar <- str_replace(newpar,'SEXOYYYY',all)
  newpar <- str_replace(newpar,'NREGZZZZ',nivel[nreg])
  
  res <- POST(url,body = newpar,encode = "form")
  
  html_doc <- res %>% 
    content('text', encoding = 'latin1') %>% 
    read_html()
  
  tabela <- html_node(html_doc,'table') %>% 
    html_table()
  
  
  varnames <- str_replace_all(tabela[2,],' ','_') 
  varnames <- str_replace(varnames,'^([0-9]{1,2})','FX_\\1')
  varnames <- str_replace(varnames,'_anos','')
  varnames[1] <- 'Area'
  
  tabela <- tabela[-1:-4,]
  names(tabela) <- varnames
  
  regex1 <- paste0('^[0-9]{',ntam[nreg],'}\\s')
  regex2 <- paste0('^[0-9]{',ntam[nreg],'}')
  
  tabela %>% 
    mutate (nome_regiao  = str_replace(Area,regex1,''),
            codreg = str_extract(Area,regex2),
            ano = anoq,
            sexo = qsexo
    ) %>% 
    mutate(across(c(starts_with('FX_'),Total), function(x) { as.numeric(str_replace_all(x,'\\.','')) } ) ) %>% 
    dplyr::select(codreg,nome_regiao,ano,sexo,starts_with('FX_'),Total) %>%   
    as_tibble()
  
}
