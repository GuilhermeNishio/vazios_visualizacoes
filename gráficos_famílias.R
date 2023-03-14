# Definindo diretório de trabalho
setwd("Z:/Guilherme/R/Vazios")

# Carregando os pacotes
library(readr)
library(dplyr)
library(readxl)
library(ggplot2)
library(janitor)


# Lendo os arquivos e definindo os dataframes
df <- read_excel("4. Ranking Famílias - Rankeado.xlsx", sheet = "Estudo Familias")

# Fazendo nomes de colunas legíveis
df <- as.data.frame(slice(df, 1:(n() - 15)))
names(df) = make.names(names(df))


# Inserindo nome de linha para Cidade
df$DISTRITOS[is.na(df$DISTRITOS)] <- "Cidade"

colunataxa = df$Taxa.de.oferta.de.emprego.formal.a.cada.100.pessoas.da.População.em.Idade.Ativa..PIA...Estimativa.SEADE.2022.
#Criando valor da cidade pra taxa de oferta de emprego
colunataxa[is.na(colunataxa)] <- mean(colunataxa, na.rm = TRUE)
View(df)

# Gráfico 6 Oferta de emprego formal (colorido)
df$Taxa.de.oferta.de.emprego.formal.a.cada.100.pessoas.da.População.em.Idade.Ativa..PIA...Estimativa.SEADE.2022.[is.na(df$Taxa.de.oferta.de.emprego.formal.a.cada.100.pessoas.da.População.em.Idade.Ativa..PIA...Estimativa.SEADE.2022.)] <- mean(df$Taxa.de.oferta.de.emprego.formal.a.cada.100.pessoas.da.População.em.Idade.Ativa..PIA...Estimativa.SEADE.2022., na.rm=TRUE)


# Arredondando valores percentuais para centésimos
df <- adorn_rounding(df, digits = 2)

# Inserindo coluna com zonas dos distritos
df <- df %>% 
  mutate(zonas = case_when(DISTRITOS %in% c('Cachoeirinha', 'Casa Verde', 'Limão', 'Brasilândia','Freguesia do Ó','Jaçanã','Tremembé','Anhanguera','Perus','Jaraguá','Pirituba',
                                            'São Domingos','Mandaqui','Santana','Tucuruvi','Vila Guilherme','Vila Maria','Vila Medeiros') ~ 'Norte',
                           DISTRITOS %in% c('Campo Limpo','Capão Redondo','Vila Andrade','Cidade Dutra','Grajaú','Socorro','Cidade Ademar','Pedreira','Cursino','Ipiranga',
                                            'Sacomã','Jabaquara','Jardim Ângela','Jardim São Luis','Marsilac','Parelheiros','Campo Belo','Campo Grande','Santo Amaro','Moema',
                                            'Saúde','Vila Mariana') ~ 'Sul',
                           DISTRITOS %in% c("Aricanduva","Carrão","Vila Formosa","Cidade Tiradentes","Ermelino Matarazzo","Ponte Rasa","Guaianases","Lajeado",
                                            "Itaim Paulista","Vila Curuçá", "Cidade Líder", "Itaquera",'José Bonifácio','Parque do Carmo','Água Rasa',
                                            'Belém', 'Brás','Mooca','Pari','Tatuapé','Artur Alvim','Cangaíba','Penha','Vila Matilde','Iguatemi',
                                            'São Mateus','São Rafael','Jardim Helena','São Miguel','Vila Jacuí','São Lucas','Sapopemba','Vila Prudente') ~ 'Leste',
                           DISTRITOS %in% c('Butantã', 'Morumbi','Raposo Tavares','Rio Pequeno','Vila Sônia','Barra Funda','Jaguara','Jaguaré','Lapa',
                                            'Perdizes','Vila Leopoldina','Alto de Pinheiros','Itaim Bibi','Jardim Paulista','Pinheiros') ~ 'Oeste',
                           DISTRITOS %in% c("Bela Vista","Bom Retiro","Cambuci","Consolação", "Liberdade","República","Santa Cecília","Sé") ~ "Centro",
                           TRUE ~ 'Cidade')) 

# Definindo uma paleta de cores para os gráficos
paleta = c("#1B2CC1", "#d02d5c", "#533745", "#92c9b1", "#e9c46a", "purple")

# Gráfico de barras % CadÚnico
plt1 <- ggplot(df, 
               aes(x = reorder(DISTRITOS , Proporção.de.famílias.cadastradas.no.CadÚnico), 
                   y = Proporção.de.famílias.cadastradas.no.CadÚnico, 
                   fill = factor(zonas, levels = c("Centro", "Norte","Sul", "Leste", "Oeste", "Cidade")), width = 0.75)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = df$zonas, values=paleta) +
  labs(fill = "Região", title = "Porcentagem de famílias cadastradas no CadÚnico em relação ao total \nde famílias no distrito (%)", x = "Distritos", y = "Porcentagem (%)", size = 5.5) +
  geom_text(aes(label = Proporção.de.famílias.cadastradas.no.CadÚnico), colour = "black", size = 2, hjust = -0.5) +
  theme(axis.text.y = element_text(size = 5.5)) +
  scale_y_continuous(limits = c(0,101), expand = c(0, 0)) +
  guides(fill=guide_legend(title="Região")) +
  facet_wrap(~zonas, ncol = 2) +
  coord_flip()
plt1

# Gráfico de barras % CadÚnico facet_wrap

df2 <- select(df, -zonas)
plt1c <- ggplot(df, aes(x = reorder(DISTRITOS , Proporção.de.famílias.cadastradas.no.CadÚnico), 
                       y = Proporção.de.famílias.cadastradas.no.CadÚnico, 
                       fill = factor(zonas, levels = c("Centro", "Norte","Sul", "Leste", "Oeste", "Cidade")), 
                       width = 0.75)) +
  geom_bar(data = df2, stat = "identity", fill = "grey70") +
  geom_bar(data = df, stat = "identity") +
  scale_fill_manual(name = df$zonas, values=paleta) +
  facet_wrap(~zonas, ncol = 2) +
  theme(axis.text.y = element_blank()) +
  labs(fill = "Região", title = "Porcentagem de famílias cadastradas no CadÚnico em relação ao total \nde famílias no distrito (%)", x = "Distritos", y = "Porcentagem (%)", size = 5.5) +
  geom_text(aes(label = Proporção.de.famílias.cadastradas.no.CadÚnico), colour = "black", size = 2, hjust = -0.5) +
  scale_y_continuous(limits = c(0,101), expand = c(0, 0)) +
  guides(fill=guide_legend(title="Região")) +
  coord_flip()
plt1c

plt1b <- ggplot(df, aes(x = reorder(DISTRITOS , Proporção.de.famílias.cadastradas.no.CadÚnico), 
                       y = Proporção.de.famílias.cadastradas.no.CadÚnico, fill = factor(zonas, levels = c("Centro", "Norte","Sul", "Leste", "Oeste", "Cidade")), width = 0.75)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = df$zonas, values = paleta) +
  labs(title = "Porcentagem de famílias cadastradas no CadÚnico em relação ao total \nde famílias no distrito (%)", x = "Distritos", y = "Porcentagem (%)", size = 5.5) +
  geom_text(aes(label = Proporção.de.famílias.cadastradas.no.CadÚnico), colour = "black", size = 2, hjust = -0.5) +
  theme(axis.text.y = element_text(size = 5.5)) +
  scale_y_continuous(limits = c(0,101), expand = c(0, 0)) +
  coord_flip()
plt1b

#Gráfico de barras % PAB
plt2 <- ggplot(df, aes(x = reorder(DISTRITOS , Proporção.das.famílias.que.recebem.Auxílio.Brasil.em.relação.às.cadastradas.no.CadÚnico), 
                       y = Proporção.das.famílias.que.recebem.Auxílio.Brasil.em.relação.às.cadastradas.no.CadÚnico, 
                       fill = factor(zonas, levels = c("Centro", "Norte","Sul", "Leste", "Oeste", "Cidade"))))+
  geom_bar(stat = "identity", width = 0.75) +
  scale_fill_manual(name = "zonas", values = paleta) +
  labs(title = "Porcentagem das famílias que recebem Auxílio Brasil em relação \nàs cadastradas no CadÚnico (%)", 
       x = "Distritos", y = "Porcentagem (%)", size = 5.5) +
  geom_text(aes(label = Proporção.de.famílias.cadastradas.no.CadÚnico), color = "black", size = 2, hjust = -0.5) +
  theme(axis.text.y = element_text(size = 5.5)) +
  scale_y_continuous(limits = c(0,101), expand = c(0, 0)) +
  guides(fill=guide_legend(title="Região")) +
  coord_flip()
plt2


plt2b <- ggplot(df, aes(x = reorder(DISTRITOS , Proporção.das.famílias.que.recebem.Auxílio.Brasil.em.relação.às.cadastradas.no.CadÚnico), 
                       y = Proporção.das.famílias.que.recebem.Auxílio.Brasil.em.relação.às.cadastradas.no.CadÚnico, 
                       fill = factor(zonas, levels = c("Centro", "Norte","Sul", "Leste", "Oeste", "Cidade"))))+
  geom_bar(data = df2, stat = "identity", fill = "grey70", width = 0.70) +
  geom_bar(data = df, stat = "identity") +
  facet_grid(~factor(zonas, levels = c("Leste", "Centro", "Norte","Sul", "Oeste", "Cidade"))) +
  scale_fill_manual(name = "zonas", values = paleta) +
  labs(title = "Porcentagem das famílias que recebem Auxílio Brasil em relação às cadastradas no CadÚnico (%)", 
       x = "Distritos", y = "Porcentagem (%)", size = 5.5) +
  geom_text(aes(label = Proporção.de.famílias.cadastradas.no.CadÚnico), color = "black", size = 2, hjust = -0.3) +
  theme(axis.text.y = element_text(size = 7)) +
  scale_y_continuous(limits = c(0,70), expand = c(0, 0)) +
  guides(fill=guide_legend(title="Região")) +
  coord_flip()
plt2b

# Gráfico de barras SASF
plt3 <- ggplot(df, aes(x = reorder(DISTRITOS , Média.mensal.de.famílias.acompanhadas.no.SASF...1º.quadrimestre.2022), 
                       y = Média.mensal.de.famílias.acompanhadas.no.SASF...1º.quadrimestre.2022, 
                       fill = factor(zonas, levels = c("Centro", "Norte","Sul", "Leste", "Oeste", "Cidade"))))+
  geom_bar(stat = "identity", width = 0.75) +
  scale_fill_manual(name = "zonas", values = paleta, limits = c("Centro", "Norte","Sul", "Leste", "Oeste")) +
  labs(title = "Média mensal de famílias acompanhadas pelo SASF - 1º.quadrimestre.2022", 
       x = "Distritos", y = "Número de famílias", size = 6) +
  geom_text(aes(label = Média.mensal.de.famílias.acompanhadas.no.SASF...1º.quadrimestre.2022), color = "black", size = 2.5, hjust = -0.5) +
  theme(axis.text.y = element_text(size = 7)) +
  scale_y_continuous(limits = c(0,5000), expand = c(0, 0)) +
  guides(fill=guide_legend(title="Região")) + 
  coord_flip()
plt3

# Gráfico de barras SASF (facet_wrap)
plt3b <- ggplot(df, aes(x = reorder(DISTRITOS , Média.mensal.de.famílias.acompanhadas.no.SASF...1º.quadrimestre.2022), 
                       y = Média.mensal.de.famílias.acompanhadas.no.SASF...1º.quadrimestre.2022, 
                       fill = factor(zonas, levels = c("Centro", "Norte","Sul", "Leste", "Oeste", "Cidade"))))+
  geom_bar(data = df2, stat = "identity", fill = "grey70") +
  geom_bar(data = df, stat = "identity") +
  facet_wrap(~zonas, ncol = 2) +
  theme(axis.text.y = element_blank()) +
  scale_fill_manual(name = "zonas", values = paleta, limits = c("Centro", "Norte","Sul", "Leste", "Oeste")) +
  labs(title = "Média mensal de famílias acompanhadas pelo SASF - 1º.quadrimestre.2022", 
       x = "Distritos", y = "Número de famílias", size = 6) +
  geom_text(aes(label = Média.mensal.de.famílias.acompanhadas.no.SASF...1º.quadrimestre.2022), color = "black", size = 2.5, hjust = -0.5) +
  theme(axis.text.y = element_text(size = 7)) +
  scale_y_continuous(limits = c(0,5000), expand = c(0, 0)) +
  guides(fill=guide_legend(title="Região")) + 
  coord_flip()
plt3b


#Gráfico de barras favelas
plt4 <- ggplot(df, aes(x = reorder(DISTRITOS , Proporção.....de.domicílios.em.favelas.em.relação.ao.total.de.domicílios..por.distrito), 
                       y = Proporção.....de.domicílios.em.favelas.em.relação.ao.total.de.domicílios..por.distrito, 
                       fill = factor(zonas, levels = c("Centro", "Norte","Sul", "Leste", "Oeste", "Cidade"))))+
  geom_bar(stat = "identity", width = 0.75, show.legend = FALSE) +
  scale_fill_manual(name = "zonas", values = c("#145C9E","#145C9E","#145C9E","#145C9E","#145C9E","#D16666")) +
  labs(title = "Proporção de domicílios em favelas em relação ao total de domicílios por distrito", 
       x = "Distritos", y = "Número de famílias", size = 6) +
  geom_text(aes(label = Proporção.....de.domicílios.em.favelas.em.relação.ao.total.de.domicílios..por.distrito), color = "black", size = 2.5, hjust = -0.5) +
  theme(axis.text.y = element_text(size = 7)) +
  scale_y_continuous(limits = c(0,101), expand = c(0, 0)) +
  coord_flip()
plt4

# Favelas colorido
plt4b <- ggplot(df, aes(x = reorder(DISTRITOS , Proporção.....de.domicílios.em.favelas.em.relação.ao.total.de.domicílios..por.distrito), 
                       y = Proporção.....de.domicílios.em.favelas.em.relação.ao.total.de.domicílios..por.distrito, 
                       fill = factor(zonas, levels = c("Centro", "Norte","Sul", "Leste", "Oeste", "Cidade"))))+
  geom_bar(stat = "identity", width = 0.75) +
  scale_fill_manual(name = "zonas", values = paleta) +
  labs(title = "Proporção de domicílios em favelas em relação ao total de domicílios por distrito", 
       x = "Distritos", y = "Número de famílias", size = 6) +
  geom_text(aes(label = Proporção.....de.domicílios.em.favelas.em.relação.ao.total.de.domicílios..por.distrito), color = "black", size = 2.5, hjust = -0.5) +
  theme(axis.text.y = element_text(size = 7)) +
  scale_y_continuous(limits = c(0,101), expand = c(0, 0)) +
  guides(fill=guide_legend(title="Região")) +
  coord_flip()
plt4b

# Gráfico 5 Oferta de emprego formal

plt5 <- ggplot(df, aes(x = reorder(DISTRITOS , Taxa.de.oferta.de.emprego.formal.a.cada.100.pessoas.da.População.em.Idade.Ativa..PIA...Estimativa.SEADE.2022.), 
                       y = Taxa.de.oferta.de.emprego.formal.a.cada.100.pessoas.da.População.em.Idade.Ativa..PIA...Estimativa.SEADE.2022., 
                       fill = factor(zonas, levels = c("Centro", "Norte","Sul", "Leste", "Oeste", "Cidade"))))+
  geom_bar(stat = "identity", width = 0.75, show.legend = FALSE) +
  scale_fill_manual(name = "zonas", values = c("#145C9E","#145C9E","#145C9E","#145C9E","#145C9E","#D16666")) +
  labs(title = "Taxa de oferta de emprego formal a cada 100 pessoas da Populaçã e Idade Ativa (PIA)", 
       x = "Distritos", y = "Número de famílias", size = 6) +
  geom_text(aes(label = Taxa.de.oferta.de.emprego.formal.a.cada.100.pessoas.da.População.em.Idade.Ativa..PIA...Estimativa.SEADE.2022.), 
            color = "black", size = 2.5, hjust = -0.5) +
  theme(axis.text.y = element_text(size = 7)) +
  scale_y_continuous(limits = c(0,1150), expand = c(0, 0)) +
  coord_flip()
plt5

# Gráfico 5 Oferta de emprego formal

plt6 <- ggplot(df, aes(x = reorder(DISTRITOS, Taxa.de.oferta.de.emprego.formal.a.cada.100.pessoas.da.População.em.Idade.Ativa..PIA...Estimativa.SEADE.2022.), 
                       y = Taxa.de.oferta.de.emprego.formal.a.cada.100.pessoas.da.População.em.Idade.Ativa..PIA...Estimativa.SEADE.2022., 
                       fill = factor(zonas, levels = c("Centro", "Norte","Sul", "Leste", "Oeste", "Cidade"))))+
  geom_bar(stat = "identity", width = 0.75) +
  scale_fill_manual(values = paleta, name = "zonas") +
  labs(title = "Taxa de oferta de emprego formal a cada 100 pessoas da População em Idade Ativa (PIA)", 
       x = "Distritos", y = "Número de famílias", size = 6) +
  geom_text(aes(label = Taxa.de.oferta.de.emprego.formal.a.cada.100.pessoas.da.População.em.Idade.Ativa..PIA...Estimativa.SEADE.2022.), 
            color = "black", size = 2.5, hjust = -0.5) +
  theme(axis.text.y = element_text(size = 7)) +
  scale_y_continuous(limits = c(0,1150), expand = c(0, 0)) +
    guides(fill=guide_legend(title="Região")) +
  coord_flip()
plt6
View(df)

# Gráfico média de pessoas atendidas no CRAS (PAIF)
plt7 <- ggplot(df, 
               aes(x = reorder(DISTRITOS, Média.do.1º.quadrimestre.de.pessoas.atendidas.em.2022..CRAS.),
                   y =  Média.do.1º.quadrimestre.de.pessoas.atendidas.em.2022..CRAS.,
                   fill = factor(zonas, levels = c("Centro", "Norte","Sul", "Leste", "Oeste", "Cidade")))) +
  geom_bar(stat = "identity", width = 0.75) +
  scale_fill_manual(values = paleta, name = "zonas") +
  labs(title = "Média de pessoas atendidas no 1º quadrimestre de 2022 em CRAS (PAIF)", 
       x = "Distritos", y = "Número de pessoas atendidas", size = 6) +
  geom_text(aes(label = Média.do.1º.quadrimestre.de.pessoas.atendidas.em.2022..CRAS.), 
            color = "black", size = 2.5, hjust = -0.5) +
  theme(axis.text.y = element_text(size = 7)) +
  scale_y_continuous(limits = c(0,3100), expand = c(0, 0)) +
  guides(fill=guide_legend(title="Região")) +
  coord_flip()
plt7

# Média de pessoas atendidas no CREAS (PAEF)
plt8 <- ggplot(df, 
               aes(x = reorder(DISTRITOS, Média.do.1º.quadrimestre.de.pessoas.atendidas.em.2022..CREAS.),
                   y =  Média.do.1º.quadrimestre.de.pessoas.atendidas.em.2022..CREAS.,
                   fill = factor(zonas, levels = c("Centro", "Norte","Sul", "Leste", "Oeste", "Cidade")))) +
  geom_bar(stat = "identity", width = 0.75) +
  scale_fill_manual(values = paleta, name = "zonas") +
  labs(title = "Média de pessoas atendidas no 1º quadrimestre de 2022 em CREAS (PAEFI)", 
       x = "Distritos", y = "Número de pessoas atendidas", size = 6) +
  geom_text(aes(label = Média.do.1º.quadrimestre.de.pessoas.atendidas.em.2022..CREAS.), 
            color = "black", size = 2.5, hjust = -0.5) +
  theme(axis.text.y = element_text(size = 7)) +
  scale_y_continuous(limits = c(0,300), expand = c(0, 0)) +
  guides(fill=guide_legend(title="Região")) +
  coord_flip()
plt8
