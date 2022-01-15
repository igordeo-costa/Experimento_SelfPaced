# Função para limpeza dos dados
read.pcibex <- function(filepath, auto.colnames=TRUE, fun.col=function(col,cols){cols[cols==col]<-paste(col,"Ibex",sep=".");return(cols)}) {
  n.cols <- max(count.fields(filepath,sep=",",quote=NULL),na.rm=TRUE)
  if (auto.colnames){
    cols <- c()
    con <- file(filepath, "r")
    while ( TRUE ) {
      line <- readLines(con, n = 1, warn=FALSE)
      if ( length(line) == 0) {
        break
      }
      m <- regmatches(line,regexec("^# (\\d+)\\. (.+)\\.$",line))[[1]]
      if (length(m) == 3) {
        index <- as.numeric(m[2])
        value <- m[3]
        if (is.function(fun.col)){
          cols <- fun.col(value,cols)
        }
        cols[index] <- value
        if (index == n.cols){
          break
        }
      }
    }
    close(con)
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=cols))
  }
  else{
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=seq(1:n.cols)))
  }
}

# Pacotes necessários
require(dplyr)
require(tidyr)
require(stringr)
require(ggplot2)
require(scales)
require(forcats)
require(RColorBrewer)
require(gridExtra)
require(lme4)

dados <- read.pcibex("~/Downloads/results_112suj.csv")

# Número de participantes:
length(unique(dados$Results.reception.time))

x <- dados %>%
  select(-c("Newline.", "Sentence..or.sentence.MD5.", "Inner.element.number",
            "Latin.Square.Group", "Comments", "Controller.name"))

colnames(x) <- c("Participante", "ID_Partic", "Ordem", "Label",
                 "ElemType", "ElemName", "Parametro",
                 "Position", "EventTime", "ID", "quant",
                 "item", "frase", "lista", "idade",
                 "gender", "escol", "nativo", "RT")

#-------------------------------------------------------------------------------
# Analisando os critérios de exclusão
#-------------------------------------------------------------------------------
# Falante nativo
x %>%
  filter(nativo %in% c("Sim", "Não")) %>%
  group_by(Participante, nativo) %>%
  tally() %>%
  group_by(Participante, nativo) %>%
  summarise(n = n/n) %>%
  group_by(nativo) %>%
  tally()

# Exclusão de não nativos
clean_data <- x %>%
  filter(nativo == "Sim") %>% # 1 participante excluído
  filter(!Participante %in% c("1638920395", "1638474825")) # 2 participantes excluídos por inconsistência nas distratoras (ver abaixo)

# Dados de escolaridade (exclusão)
clean_data %>%
  group_by(Participante, escol) %>%
  tally() %>%
  group_by(Participante, escol) %>%
  summarise(n = n/n) %>%
  group_by(escol) %>%
  tally()

# Exclusão dos participantes que não atenderam ao critério de escolaridade: 1 participante excluído
clean_data <- clean_data %>%
  filter(!escol %in% c("Ensino Médio incompleto"))

# Análise das declarações de gênero
clean_data %>%
  group_by(Participante, gender) %>%
  tally() %>%
  group_by(Participante, gender) %>%
  summarise(n = n/n) %>%
  group_by(gender) %>%
  tally()

# Idade média dos participantes
clean_data %>%
  group_by(Participante) %>%
  summarise(idade = unique(idade)) %>%
  ungroup() %>%
  summarise(idadeMedia = mean(idade),
            idadeMin = min(idade),
            idadeMax = max(idade))

  
#------------------------------------------------------
# Análise da escolha forçada e participantes por lista
#------------------------------------------------------
escolha <- clean_data %>%
  filter(Label == "experiment") %>%
  filter(Parametro == "Choice")

# Total de participantes por lista
escolha %>%
  group_by(lista) %>%
  tally() %>%
  mutate(partic_por_lista = n/12) # frases vistas por participante = 12

choice_data <- escolha %>%
  mutate(Position = str_replace_all(Position, c("%09" = ""))) %>%
  mutate(Position = str_replace_all(Position, c("uma" = "um")))

# Total de respostas à escolha forçada perdidas devido ao tempo esgotado
choice_data %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.integer, as.factor) %>%
  group_by(quant) %>%
  tally(is.na(Position)) %>% # TOTAL = !is.na(); perdidas = is.na
  summarise(perdidas = sum(n))

round(65/(1219+65)*100, 2) # cerca de 5% perdidas



#-------------------------------------------------------------------------------
# Análise das sentenças distratoras
#-------------------------------------------------------------------------------
dist <- clean_data %>%
  filter(Label == "distrat")

# Carregando a tabela com a resposta esperada por cada frase direto do GitHub
expected_answers <- read.csv("/home/dados/Acadêmicos/Doutorado/EXPERIMENTOS_2021/SelfPacedReading/Distratoras.csv")

# Uma frase distratora foi catalogada erroneamente, de modo que havia 17 (e não 16) com resposta esperada + de um
# O código abaixo conserta isso
expected_answers <- expected_answers %>%
  mutate(resposta = ifelse(frase == "Os pedreiros moveram alguns tijolos depois da obra.", "+um", resposta))

expected_answers <- edit(expected_answers) # mudar 'posicao' para 'lista'

# Mesclando as duas tabelas e filtrando apenas respostas à escolha forçada
dist <- expected_answers %>%
  select(c(lista, resposta, frase)) %>%
  inner_join(dist, by = c("frase", "lista")) %>%
  filter(Parametro == "Choice") %>%
  mutate(Position = str_replace_all(Position, c("uma" = "um"))) %>%
  filter(Position %in% c("Apenas um", "Mais de um"))

# Análise por sujeitos
dist$Participante <- as.factor(dist$Participante)

# Participante respondeu 'Mais de um' apenas 50% das vezes quando a resposta era '+um'
# 1638920395
# Participante com SE_inf a 50% para resposta "Apenas um" quando o esperado era 'um'
# 1638474825

cores <- brewer.pal(name = "Set1", n = 3)

dist %>%
  group_by(resposta, Participante, Position) %>%
  tally() %>%
  mutate(porc = n/sum(n, na.rm = T)) %>%
  mutate(SE=sqrt((porc*(1-porc))/n)) %>% # Calcula o erro padrão (Standad Error ou SE) da proporção
  mutate(SE_inf=porc-SE) %>%
  mutate(SE_sup=porc+SE) %>%
  mutate(SE_inf=if_else(SE_inf<0, 0, SE_inf)) %>%
  filter(Position == "Mais de um") %>% # Ou "Mais de um" aqui... Tanto faz...
  filter(!Participante %in% c("1638920395", "1638474825")) %>%
  ggplot(aes(x = Participante, y = porc, color = resposta)) +
  geom_hline(aes(yintercept = 0.5), linetype = "dashed", color = "grey60") +
  geom_errorbar(aes(ymin = SE_inf, ymax = SE_sup), width = 0.3, size = 0.7, color = "grey") +
  geom_point(size = 2, stroke = 1, fill = "white", shape = 21) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L)) +
  scale_color_manual(values = cores, name = "Resposta Esperada:",
                     labels = c("Mais de um", "Apenas um")) +
  labs(x = "", y = "") +
  theme(text = element_text(size=14),
        legend.position = "top",
        legend.text = element_text(size=14),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 

# Análise global das distratoras
dist %>%
  filter(!Participante %in% c("1638920395", "1638474825")) %>%
  group_by(resposta, Position) %>%
  tally() %>%
  mutate(porc = n/sum(n))

# Porcentagem de respostas corretas nas distratoras
# 98.1% (Mais de um) e  98.3% (Apenas um)

dist %>%
  filter(!Participante %in% c("1638920395", "1638474825")) %>%
  group_by(resposta, frase, Position) %>%
  tally() %>%
  mutate(porc = n/sum(n, na.rm = T)) %>%
  mutate(SE=sqrt((porc*(1-porc))/n)) %>% # Calcula o erro padrão (Standad Error ou SE) da proporção
  mutate(SE_inf=porc-SE) %>%
  mutate(SE_sup=porc+SE) %>%
  mutate(SE_inf=if_else(SE_inf<0, 0, SE_inf)) %>%
  filter(Position == "Mais de um") %>% # Ou "Mais de um" aqui... Tanto faz...
  ggplot(aes(x = frase, y = porc, color = resposta)) +
  geom_errorbar(aes(ymin = SE_inf, ymax = SE_sup), width = 0.3, size = 0.7, color = "grey") +
  geom_point(size = 2, stroke = 1, fill = "white", shape = 21) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L)) +
  scale_color_manual(values = cores, name = "Resposta Esperada:",
                     labels = c("Mais de um", "Apenas um")) +
  labs(x = "", y = "") +
  theme(text = element_text(size=14),
        legend.position = "top",
        legend.text = element_text(size=14)) 

#-------------------------------------------------------------------------------
# Análise dos dados de fato
#-------------------------------------------------------------------------------
# Respostas 'Apenas um'

apenas_um <- choice_data %>%
  filter(!is.na(Position)) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.integer, as.factor) %>%
  group_by(quant, Position) %>%
  tally() %>%
  mutate(prop = n/sum(n, na.rm = T)) %>%
  filter(Position == "Apenas um") %>%
  mutate(SE=sqrt((prop*(1-prop))/n)) %>% # Calcula o erro padrão (Standad Error ou SE) da proporção
  mutate(SE_inf=prop-SE) %>%
  mutate(SE_sup=prop+SE) %>%
  mutate(SE_inf=if_else(SE_inf<0, 0, SE_inf)) %>% # Limita os valores inferiores a zero
  mutate(across(3:6, round, 4)) %>%
  mutate_if(is.numeric, funs(.*100)) %>%
  mutate(n = n/100) # Apenas para corrigir a multiplicação acima, que foi também para o 'inteiro'

setwd("/home/dados/Acadêmicos/Doutorado/Qualificação/Texto/Tabelas/")
write.csv(apenas_um, "selfpaced_apenasUm.csv")

cor <- brewer.pal("Set1", n = 3)

g <- apenas_um %>%
  ggplot(aes(x = fct_reorder(quant, prop), y = prop, color = quant)) +
  geom_errorbar(aes(ymin = SE_inf, ymax = SE_sup), width = 0.08, size = 0.7, color = "grey60") +
  geom_point(fill="white", size = 1.5, shape = 21, stroke = 1) +
  theme_bw() +
  ylab("") + xlab("") +
  coord_flip() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L),
                     limits = c(.1, .7),
                     breaks = seq(from = 0, to = 1, by = .1)) +
  scale_color_manual(values = cor, name = NULL,
                     labels = c("Cada", "Todo", "Todos os")) +
  theme(legend.position = "top", 
        text = element_text(size=12),
        legend.text = element_text(size=10),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size=12),
        legend.title=element_blank()) +
  ggtitle("Resposta dada: 'Apenas um'")

setwd("/home/dados/Acadêmicos/Doutorado/Qualificação/Texto/Imagens/")
g <- grid.arrange(g, ncol = 1)
ggsave("Selfpaced_VisaoGeral.png", g, dpi = 300, width = 115, height = 85, units = "mm")

# os predicados originais eram altamente enviesados para uma leitura "Apenas um" (todas com mais de 90% para leituras 'Apenas um');
# O objetivo, aqui, era verificar o quanto os quantificadores influenciavam nesse panorama;
# CADA mudou completamente, levando para mais de 80% de leituras "Mais de um";
# 'TODO' mudou para mais de 60%, indicando alguma tendência distributiva?
# 'TODOS OS' mudou para cerca de 40%, ou seja, diminui pouco a tendência original

# Modelo de regressão aos dados, usando 'cada' como base
# Desnecessário nesse caso!
choice <- choice_data %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.integer, as.factor)

model <- glmer(Position ~ quant + (1|Participante) + (1|item), data = choice, family = binomial)
summary(model)

#-------------------------------------------------------------------------------
# Análise dos tempos de reação
#-------------------------------------------------------------------------------
exper <- clean_data %>%
  filter(Label == "experiment") %>%
  filter(RT != "NULL")

exper$RT <- as.numeric(exper$RT)
exper$item <- as.factor(exper$item)
exper$quant <- as.factor(exper$quant)
exper$Position <- as.factor(exper$Position)
exper$Parametro <- as.integer(exper$Parametro)

teste <- exper %>%
  mutate(Parametro = ifelse(quant != "todos os" & Parametro>1, Parametro+1, Parametro))

g_rt <- teste %>%
  filter(RT <= 5000 & RT >= 150) %>% # Critério de exclusão!!! (ver abaixo)
  filter(!is.na(Parametro)) %>%
  group_by(quant, Parametro) %>%
  summarise(meanRT = mean(RT, na.rm = T), # Com log RT fica melhor, creio
            SD = sd(RT, na.rm = T),
            n = n(),
            SE = SD/sqrt(n),
            CI = SE*1.96) %>%
  ggplot(aes(x = as.factor(Parametro), y = meanRT, group = quant, color = quant, linetype = quant)) +
  geom_line(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(x = as.factor(Parametro), ymin = meanRT - SE, ymax = meanRT + SE),
                color = "grey40", position = position_dodge(width = 0.4), width = 0.2,
                linetype = "solid", alpha = 0.6) +
  geom_point(size = 2, shape = 21, stroke = 1, fill = "white",
             position = position_dodge(width = 0.4)) +
  theme_classic() +
  scale_x_discrete(label = c("1\n\nTodos\n\nCada\n\nTodo", "2\n\nos", "3\n\natletas\n\natleta\n\natleta",
                             "4\n\npintaram\n\npintou\n\npintou", "5\n\numa", "6\n\nquadra", "7\n\ndurante",
                             "8\n\na", "9\n\nreforma", "10\n\ndo", "11\n\nginásio.")) +
  labs(x = "", y = "Tempo de Reação (RT)")  +
  theme(legend.title = element_blank(),
        legend.position = c(0.07, 0.9)) +
#  ggtitle("Médias e erros padrão da média") +
  annotate("rect", xmin=4.7, xmax=6.3, ymin=430, ymax=650, alpha=0.1, fill="red") +
  annotate("rect", xmin=6.5, xmax=7.5, ymin=430, ymax=650, alpha=0.1, fill="orange") +
  annotate("rect", xmin=7.7, xmax=9.3, ymin=430, ymax=650, alpha=0.1, fill="green")

setwd("/home/dados/Acadêmicos/Doutorado/Qualificação/Texto/Imagens/")
g_rt <- grid.arrange(g_rt, ncol = 1)
ggsave("SelfPaced_RTs.png", g_rt, dpi = 300, width = 230, height = 100, units = "mm")



# OBS para não esquecer: as propriedades lexicais de "todo" são incompatíveis com um verbo flexionado no passado.
# "todo" requer uma interpretação "genérica" ('todo atleta é saudável'). O evento denotado pelo verbo, portanto,
# deve ter essas propriedades.

#---------------------------------------------------
# INVESTIGAÇÃO EXPLORATÓRIA DA DISTRIBUIÇÃO DOS RTs
#---------------------------------------------------
# Todas as posições
# Ver tb com filtragem e logaritmo
teste %>%
  filter(!is.na(Parametro)) %>%
  filter(RT <= 5000 & RT >= 150) %>%
  ggplot(aes(x = as.factor(Parametro), y = log(RT), color = quant)) +
  geom_jitter(shape = 21) +
  facet_wrap(~quant) +
  scale_x_discrete(label = c("Todos\n\nCada\n\nTodo", "os", "atletas\n\natleta\n\natleta",
                             "pintaram\n\npintou\n\npintou", "uma", "quadra", "durante",
                             "a", "reforma", "do", "ginásio."))

# Apenas a posição 4, que parece indicar uma diferença
teste %>%
  filter(Parametro == 4) %>%
  ggplot(aes(x = as.factor(Parametro), y = RT, color = quant)) +
  geom_jitter(shape = 19) +
  facet_wrap(~quant)

# Média de todo x cada: excluindo RTs menores que 150ms
# Apenas na posição do verbo
med <- teste %>%
  filter(!is.na(Parametro)) %>%
  filter(RT <= 5000 & RT >= 150) %>%
  filter(Parametro == 4) %>%
  filter(quant != "todo") %>%
  group_by(quant) %>%
  summarise(medias = mean(log(RT), na.rm = T))

# Distribuição de todo x cada
teste %>%
  filter(!is.na(Parametro)) %>%
  filter(RT <= 5000 & RT >= 150) %>%
  filter(Parametro == 4) %>%
  filter(quant != "todos os") %>%
  ggplot(aes(x = log(RT), fill = quant)) +
  geom_density(alpha = 0, aes(color = quant)) +
  geom_vline(data = med,
             aes(xintercept = medias, color = quant, linetype = quant),
             size = 1) +
  theme_classic() #+
  #facet_wrap(~quant)

# O quanto dessa diferença de média é causada pelo componente gaussiano da distribuição (mu)
# ou pelo componente exponencial da distribuição (tau)?
# install.packages("/home/igor/Downloads/retimes_0.1-2.tar.gz", repos = NULL, type = "source")

teste %>%
  filter(!is.na(Parametro)) %>%
  filter(RT >= 150) %>%
  ggplot(aes(x = log(RT), y = quant, color = quant)) +
  geom_jitter(size = 0.2) +
  theme_classic() +
  facet_wrap(~Parametro)


teste %>%
  filter(!is.na(Parametro)) %>%
  filter(RT >= 150) %>%
  ggplot() +
  geom_density(aes(x = log(RT), color = quant), alpha = 1) +
  theme_classic() +
  facet_wrap(~Parametro)

cor <- c("#E41A1C", "#4DAF4A", "#377EB8")

g1 <- teste %>%
  filter(!is.na(Parametro)) %>%
  filter(RT <= 5000 & RT >= 150) %>%
  filter(Parametro == 4) %>% # Ver também posição 9
  ggplot(aes(log(RT), ..scaled..)) +
  geom_density(aes(x = log(RT), color = quant, fill = quant), alpha = .4) +
  geom_segment(aes(x = 7.7, y = 0.25, xend = 7.7, yend = 0.1),
               arrow =  arrow(length = unit(0.1, "inches")),
               color = "grey") +
  geom_text(aes(label = "Efeito\nrelacionado \nà cauda da\ndistribuição",
                x = 7.7, y = 0.4), size = 3) +
  scale_fill_manual(values = cor, name = NULL) +
  scale_color_manual(values = cor, name = NULL) +
  theme_classic() +
  facet_wrap(~Parametro) +
  theme(legend.position = 'none') +
  labs(y = "Densidade")

# Fazendo o mesmo para a posição 5

g2 <- teste %>%
  filter(!is.na(Parametro)) %>%
  filter(RT <= 5000 & RT >= 150) %>%
  filter(Parametro == 5) %>% # Ver também posição 9
  ggplot(aes(log(RT), ..scaled..)) +
  geom_density(aes(x = log(RT), color = quant, fill = quant), alpha = .4) +
  geom_segment(aes(x = 7.7, y = 0.3, xend = 7.7, yend = 0.1),
               arrow =  arrow(length = unit(0.2, "inches")),
               color = "grey") +
  geom_text(aes(label = "Efeito\nrelacionado \nà cauda da\ndistribuição",
                x = 7.7, y = 0.4)) +
  scale_fill_manual(values = cor, name = NULL) +
  scale_color_manual(values = cor, name = NULL) +
  theme_classic() +
  facet_wrap(~Parametro) +
  theme(legend.position = 'none') +
  labs(y = "Densidade")

# Fazendo o mesmo para a posição 9

g3 <- teste %>%
  filter(!is.na(Parametro)) %>%
  filter(RT <= 5000 & RT >= 150) %>%
  filter(Parametro == 9) %>% # Ver também posição 9
  ggplot(aes(log(RT), ..scaled..)) +
  geom_density(aes(x = log(RT), color = quant, fill = quant), alpha = .4) +
  geom_segment(aes(x = 7.4, y = 0.39, xend = 6.9, yend = 0.3),
               arrow =  arrow(length = unit(0.1, "inches")),
               color = "grey") +
  geom_text(aes(label = "Efeito\nrelacionado\nao corpo da\ndistribuição",
                x = 7.7, y = 0.4), size = 3) +
  scale_fill_manual(values = cor, name = NULL) +
  scale_color_manual(values = cor, name = NULL) +
  theme_classic() +
  facet_wrap(~Parametro) +
  theme(legend.position = c(0.13, 0.8),
        legend.text = element_text(size = 7)) +
  labs(y = "")

setwd("/home/dados/Acadêmicos/Doutorado/Qualificação/Texto/Imagens/")
g_dist <- grid.arrange(g1, g3, ncol = 2)
ggsave("SelfPaced_Distrib.png", g_dist, dpi = 300, width = 230, height = 80, units = "mm")


90-17

#-------------------------------------------------------------------------------
# Investigar apenas se necessário!
#-------------------------------------------------------------------------------
require(retimes)

y <- teste %>%
  filter(Parametro == 4) %>%
  filter(RT >= 150)

par <- y %>%
  filter(RT <= 3000) %>%
  group_by(quant) %>%
  summarise(parametros = mexgauss(RT))

par$tags <- rep(c("mu", "sigma", "tau"), 3)

# De fato, o componente relevante na composição da média de "todo" é tau
par %>%
  pivot_wider(names_from = tags,
              values_from = parametros)

# Lembrando: a média é composta por mu + tau
# A variância por sigma^2 + tau^2

#-------------------------------------------------------------------------------
# Aplicando EXPLORATORIAMENTE um modelo misto aos dados
#-------------------------------------------------------------------------------
require(lme4)  

# Posição 4:
# Diferença entre todo x cada, mas não entre todos os x cada

p4 <- teste %>%
  filter(Parametro == 4) %>%
  filter(RT <= 5000 & RT >= 150)

model4 <- lmer(log(RT) ~ quant + (1|Participante) + (1|item), data = p4) # singular com slopes para participantes ou itens
summary(model4)

# Diferença entre todo x cada, mas não entre todos os x cada

p5 <- teste %>%
  filter(Parametro == 5) %>%
  filter(RT <= 5000 & RT >= 150)

model5 <- lmer(log(RT) ~ quant + (1|Participante) + (1|item), data = p5) # singular com slopes para participantes ou itens
summary(model5)

# Posição 5:
# Diferença entre todo x cada, mas não entre todos os x cada
# Efeito de espraiamento da posição 4, já que não perdura na posição 6

p6 <- teste %>%
  filter(Parametro == 6) %>%
  filter(RT <= 5000 & RT >= 150)

model6 <- lmer(log(RT) ~ quant + (1|Participante) + (1|item), data = p6) # singular com slopes para participantes ou itens
summary(model6)

# Não há efeitos!

p7 <- teste %>%
  filter(Parametro == 7) %>%
  filter(RT <= 5000 & RT >= 150)

model7 <- lmer(log(RT) ~ quant + (1|Participante) + (1|item), data = p7) # singular com slopes para participantes ou itens
summary(model7)

# Não há efeitos!

p8 <- teste %>%
  filter(Parametro == 8) %>%
  filter(RT <= 5000 & RT >= 150)

model8 <- lmer(log(RT) ~ quant + (1|Participante) + (1|item), data = p8) # singular com slopes para participantes ou itens
summary(model8)

# Não há efeitos!

p9 <- teste %>%
  filter(Parametro == 9) %>%
  filter(RT <= 5000 & RT >= 150)

model9 <- lmer(log(RT) ~ quant + (1|Participante) + (1|item), data = p9) # singular com slopes para participantes ou itens
summary(model9)

# diferença entre todo x cada
# diferença entre todos os x cada


# RESUMO:
# Não há efeitos: p6, p7, p8
# Há efeitos: p5 (apenas para 'todo', herdado de p4, não crítica) e p9 (para 'todo' e 'todos os')







# Rascunho! Ainda a revisar!

cis <- confint(model)

cis <- cis[5:6,]

summary(model)$coefficients[2:3]

final <- data.frame(fixef = c(0.06967179, 0.02140354),
ciinf = c(0.02117232, -0.02708855),
cisup = c(0.11818679, 0.06985144),
quant = c("todo", "todos os"))

final %>%
  ggplot(aes(x = quant, y = fixef)) +
  geom_hline(aes(yintercept = 0), color = "grey", size = 1, linetype = "dashed") +
  geom_errorbar(aes(x = quant, ymin = ciinf, ymax = cisup), width = 0.1, size = 1) +
  geom_point(color = "orange", size = 6, shape = 19) +
  scale_y_continuous(breaks = seq(from = -0.03, to = 0.12, by = 0.03)) +
  coord_flip() +
  theme_classic() +
  theme(text = element_text(size = 16)) +
  labs(y = "\nEstimativa para os efeitos fixos em log(RT)", x = "")

# Cada x todos os: diferença de 11 milisegundos
exp(6.27157 + 0.02140) - exp(6.27157)

# Cada x todo: 38 milisegundos
exp(6.27157 + 0.06967179) - exp(6.27157)


y %>%
  filter(!is.na(Parametro)) %>%
  filter(RT >= 150) %>%
  filter(Parametro == 4) %>%
  group_by(quant) %>%
  summarise(medias = mean(RT, na.rm = T))

699-596 # diferença na amostra entre cada e todo
608-596 # O modelo "recuperou bem esse", mas não o anterior (devido à distribuição da amostra, ao componente tau?)
