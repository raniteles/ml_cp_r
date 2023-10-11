getwd()

#pacotes
pacman::p_load(
  # ETL
  data.table, dplyr,
  # MACHINE LEARNING
  caret,
  # REGRAS DE ASSOCIAÇÃO
  arules, arulesCBA, arulesViz,
  # TABELAS
  reactablefmtr
)

#carregar database
candidatos_pe_2022 <- fread('E:/Machine Learning_CP/ml_cp_r/database_cand/consulta_cand_2022_PE.csv', encoding = 'Latin-1', stringsAsFactors = T)

# filtrar apenas deputados federais e variáveis de perfil
federais_pe_2022 <- candidatos_pe_2022 %>% filter(DS_CARGO == 'DEPUTADO FEDERAL') %>% select(TP_AGREMIACAO, NM_MUNICIPIO_NASCIMENTO, NR_IDADE_DATA_POSSE, DS_GENERO, DS_GRAU_INSTRUCAO, DS_ESTADO_CIVIL, DS_COR_RACA, DS_OCUPACAO)

#observar se os dados estão com as classes certas
glimpse(federais_pe_2022)

#modificar nomes para facilitar análise e plot
names(federais_pe_2022) <- c(
  'agremiacao', 'municipio', 'idade', 'genero', 'instrucao', 'estado_civil', 'raca', 'ocupacao'
)

#discretizar variável numérica
federais_pe_2022[ , 3] <- discretizeDF(federais_pe_2022[ , 3]) # transforma variáveis numéricas em fatores

##### MINERAÇÃO #####
# mineração com a priori
regras_federais <- apriori(
  federais_pe_2022, 
  parameter = list(supp = 0.2, conf = 0.5, minlen = 2, maxlen = 5))

##limpar e organizar regras
# três casas decimais
quality(regras_federais) <- round(quality(regras_federais), digits = 3) 
# organizar por lift
regras_federais <- sort(regras_federais, by="lift") 
# remover regras redundantes
regras_federais_res <- regras_federais[!is.redundant(regras_federais, measure="lift")]

#inspecionar regras
inspect(regras_federais_res)

regras_federais_df = data.frame(
  lhs = labels(lhs(regras_federais_res)),
  rhs = labels(rhs(regras_federais_res)), 
  regras_federais_res@quality)

reactable(
  regras_federais_df, 
  defaultColDef = colDef(cell = data_bars(regras_federais_df ,text_position = 'outside-base')),
  pagination = F
)

# gráfico de coordenadas
plot(regras_federais_res, method="paracoord", control=list(reorder=T), measure=c("lift"), lty = "dotted")

#gráfico de relações agrupadas
plot(regras_federais_res, method="grouped", measure=c("lift"))

### APENAS GENERO FEMININO ###

regras_gen_fem <- apriori(federais_pe_2022, control=list(verbose=F), parameter = list(minlen=2, supp=0.1, conf=0.3), appearance = list(lhs="genero=FEMININO", default="rhs"))

quality(regras_gen_fem)<-round(quality(regras_gen_fem),digits = 3)
regras_gen_fem<-sort(regras_gen_fem,by="lift")

regras_gen_fem_df = data.frame(
  lhs = labels(lhs(regras_gen_fem)),
  rhs = labels(rhs(regras_gen_fem)), 
  regras_gen_fem@quality)

reactable(regras_gen_fem_df, defaultColDef = colDef( cell = data_bars(regras_gen_fem_df,text_position = 'outside-base')
))

plot(regras_gen_fem,method="paracoord",control=list(reorder=T),measure=c("lift"),lty = "dotted")
