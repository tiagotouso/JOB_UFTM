# SCRIPT PARA GERAR GRÁFICOS DO FLUXO DA EDUCAÇÃO SUPERIOR

# BASE DE DADOS
# https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/indicadores-educacionais/indicadores-de-fluxo-da-educacao-superior

library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggthemes)

theme_set(theme_economist())

# FUNÇÕES BEGIN ----

criarGraficoGGP <- function(dados, instituto, curso, ano, pasta){
  # FUNÇÃO PARA CRIAR O GRÁFICO
  
  anomin <- min(dados$ANO)
  
  titulo <- paste('Fluxo da Educação Superior - ', curso)
  
  dados %>% ggplot(aes(ANO, VALOR, colour = factor(TIPO))) +
    geom_line() +
    geom_point() +
    geom_text_repel(aes(label = format(round(VALOR, 2), nsmall = 2)), size=3.0, show_guide = F) + 
    labs(title = titulo, caption = 'Fonte: INEP', x = '', y = '', colour = NULL)+
    theme(plot.title = element_text(hjust = 0.5),
          legend.position="bottom",
          color  = NULL, legend.text=element_text(size=8)) +
    coord_cartesian(ylim = c(0, 105), xlim = c(anomin, 2019)) +
    scale_x_continuous(breaks=seq(anomin, 2019)) +
    guides(col = guide_legend(nrow = 1))
  
  narquivo <- paste('_GRAFICOS_R/', pasta, '/I-F-E-S', sep = '')
  narquivo <- paste(narquivo,instituto, curso, ano, '.jpeg')
  ggsave(narquivo, scale = 1, dpi = 300, width = 26, height = 13, units = "cm")
}


buscasValores <- function(dadosf, co_municipio, no_curso){
  # FUNÇÃO PARA FILTRA OS VALORES DO CURSO POR CIDADE
  
  curso <- dadosf %>% filter(CO_MUNICIPIO == co_municipio & NO_CURSO == no_curso)
  
  if(dim(curso)[1] > 0){
    curso <- curso %>% select('ANO', 'TAP', 'TCA', 'TDA', 'TCAN', 'TADA')
    
    ddt <- data.frame()
    # for(cl in c('TAP', 'TCA', 'TDA', 'TCAN', 'TADA')){
    for(cl in c('TAP', 'TCA', 'TDA')){
      ax <- curso %>% select(ANO, cl)
      ax <- rename(ax, VALOR=cl)
      ax['TIPO'] <- cl
      
      ddt <- rbind(ddt, ax)
    }
    
    ddt <- ddt %>% mutate(TIPO = case_when(TIPO == 'TAP' ~ 'Taxa de Permanência',
                                           TIPO == 'TCA' ~ 'Taxa de Conclusão Acumulada',
                                           TIPO == 'TDA' ~ 'Taxa de Desistência Acumulada',
                                           TIPO == 'TCAN' ~ 'Taxa de Conclusão Anual',
                                           TIPO == 'TADA' ~ 'Taxa de Desistência Anual'))
    
    ddt <- ddt %>% mutate(VALOR = round(VALOR, 2))
    
    # RETORNA 1 + DADOS SE O DATASET FOR <> 0
    retorno <- list(1, ddt)
  }else{
    # RETORNA 0 SE O DATASET FOR == 0
    retorno <- list(0, list(0))
  }

  return(retorno)
}


filtraDadosIES <- function(dados){
  # FUNÇÃO PARA FILTRAR DADOS DA IES
  
  # OS NOMES DAS COLUNAS ESTÃO NA LINHA 8
  colunas <- c(dados[8, ])
  
  # CORRIGIR OS NOMES DAS COLUNAS
  colnames(dados) <- colunas
  
  # FILTRA OS DADOS DA UFTM
  dadosIES <- dados %>% filter(CO_IES == 597)
  
  # CORRIGIR OS VALORES NUMÉRICOS
  dadosIES <- dadosIES %>% 
    mutate(ANO=as.integer(dadosIES$NU_ANO_REFERENCIA),
           TAP=as.numeric(dadosIES$TAP),
           TCA=as.numeric(dadosIES$TCA),
           TDA=as.numeric(dadosIES$TDA),
           TCAN=as.numeric(dadosIES$TCAN),
           TADA=as.numeric(dadosIES$TADA))
  
  return(dadosIES)
}


filtrarCursosIES <- function(dadosIES){
  # CRIA UMA LISTA DE CIDADE E CURSO DO DATASET
  
  cursosIES <- dadosIES %>% select(CO_MUNICIPIO, NO_CURSO)
  cursosIES <- cursosIES[duplicated(cursosIES)== FALSE, ]
  
  return(cursosIES)
}


addCaracteres <- function(numero){
  # FUNÇÃO PARA ADD ZEROS A ESQUERDA
  
  valor <- ''
  for (n in seq(1, 5 - nchar(as.character(numero)))){
    valor <- paste(valor,  '0', sep='')
  }
  valor <- paste(valor,  numero, sep='')
  
  return(valor)
}

# FUNÇÕES END ----

# SCRIPT BEGIN ----

# DIRETÓRIO DOS ARQUIVOS
diretorio <- 'C:/_BIG_DATA/INDICADORES DE FLUXO DA EDUCAÇÃO DA EDUCAÇÃO SUPERIOR/'

# NOME DOS ARQUIVOS
arquivos <- c('indicadores_trajetoria_educacao_superior_2015_2019.xlsx',
    'indicadores_trajetoria_educacao_superior_2014_2019.xlsx',
    'indicadores_trajetoria_educacao_superior_2013_2019.xlsx',
    'indicadores_trajetoria_educacao_superior_2012_2019.xlsx',
    'indicadores_trajetoria_educacao_superior_2011_2019.xlsx',
    'indicadores_trajetoria_educacao_superior_2010_2019.xlsx')

nomelista <- c('2015_2019', '2014_2019', '2013_2019',
                    '2012_2019', '2011_2019', '2010_2019')

# CARREGAR OS DATASET
# VARIÁVEL PARA GUARDAR TODOS OS DATASET CARREGADOS
dadoslista <- list()
for (num in c(1:6)){
  narquivo <- paste(diretorio, arquivos[num], sep='')
  dados <- read_excel(narquivo)
  dadoslista[[nomelista[num]]] <- dados
}

# FILTRAR DADOS DA IES
# TRATAR DADOS
# CRIAR DATASET DOS CURSOS

# VARIÁVEL PARA GUARDAR DOS OS CURSOS E ANOS DOS DATASET
dadoscursos <- data.frame()
# VARIÁVEL PARA GUARDAR TODOS OS DATASET DA INSTITUIÇÃO
dadoslistaIES <- list()

for(ano in nomelista){
  print(ano)
  dados <- dadoslista[[ano]]
  dadosIES <- filtraDadosIES(dados)
  dadoslistaIES[[ano]] <- dadosIES
  cursos <- filtrarCursosIES(dadosIES)
  cursos['ANO'] <- ano

  dadoscursos <- rbind(dadoscursos, cursos)
}

# CARREGAR DADOS DOS CURSOS DE UBERABA
# COM A PLANILHA INTERNA COM DADOS DOS CURSOS
planilhacursos <- read_excel('_DADOS/DADOS DO CURSO PARA CÁLCULO DO ALUNO EQUIVALENTE GRADUAÇÃO.xlsx')
planilhacursos <- planilhacursos %>% select(INSTITUTO, MUNICIPIO, CURSO, DURAÇÃO)

planilhacursos <- planilhacursos %>% mutate(CO_MUNICIPIO=case_when(MUNICIPIO == 'UBERABA' ~ 3170107,
                                       MUNICIPIO == 'ITURAMA' ~ 3134400))
planilhacursos['ARQUIVO'] <- planilhacursos[,'DURAÇÃO'] + 1 -4
planilhacursos <- planilhacursos %>% arrange(INSTITUTO, CURSO)


# CRIAR OS GRÁFICOS PARCIAIS POR INSTITUTO, CIDADE, CURSO
for (num in seq(1, dim(planilhacursos)[1])){
  
  cdmunicipio = as.character(planilhacursos[num, 'CO_MUNICIPIO'])
  curso = as.character(planilhacursos[num, 'CURSO'])
  instituto = as.character(planilhacursos[num, 'INSTITUTO'])
  
  print(curso)

  idarquivo = as.numeric(planilhacursos[num, 'ARQUIVO'])
  ano <- nomelista[idarquivo]
  dadostemp <- dadoslistaIES[[ano]]

  resultado <- buscasValores(dadostemp, cdmunicipio, curso)

  if (resultado[[1]] == 1){
    contador <- contador + 1
    dadosgrafico <- resultado[[2]]
    criarGraficoGGP(dadosgrafico, instituto, curso, ano, 'PARCIAL')
  }else{
    print('--- ERRO ---')
    print(paste('  ', 'CURSO NÃO ENCONTRADO NO ARQUIVO'))
    print(paste('  ', curso, ' - ', ano))
  }
}

# CRIAR OS GRÁFICOS TOTAIS POR INSTITUTO, CIDADE, CURSO
for (num in seq(1, dim(planilhacursos)[1])){
  
  cdmunicipio = as.character(planilhacursos[num, 'CO_MUNICIPIO'])
  curso = as.character(planilhacursos[num, 'CURSO'])
  instituto = as.character(planilhacursos[num, 'INSTITUTO'])
 
  filtro <- dadoscursos %>% filter(CO_MUNICIPIO == cdmunicipio
                                   & NO_CURSO == curso)
  print(curso)
  if (dim(filtro)[1] > 0){
    
    anos <- filtro[,'ANO']$ANO
    
    for (ano in anos){
      print(paste('  ', ano))
      dadostemp <- dadoslistaIES[[ano]]
      resultado <- buscasValores(dadostemp, cdmunicipio, curso)
      
      if (resultado[[1]] == 1){
        contador <- contador + 1
        dadosgrafico <- resultado[[2]]
        criarGraficoGGP(dadosgrafico, instituto, curso, ano, 'TOTAL')
      }
    }
  }else{
    print('--- ERRO ---')
    print(paste('  ', 'CURSO NÃO ENCONTRADO NO ARQUIVO'))
    print(paste('  ', curso, ' - ', ano))
  }
}

