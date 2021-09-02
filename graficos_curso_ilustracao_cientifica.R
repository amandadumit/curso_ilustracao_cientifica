#' ---
#' title: "Curso de Ilustração Científica para Artigos"
#' author: "Ana Letycia Basso Garcia e Patricia Sanae Sujii"
#' date: "02 de Setembro de 2021"
#' output: 
#'   rmdformats::robobook:
#'     self_contained: true
#'     thumbnails: true
#'     lightbox: true
#'     gallery: false
#'     highlight: tango
#' ---

#' # Introdução 
#' Neste documento você poderá refazer alguns dos gráficos apresentados durante o **Curso Rápido de 
#' Ilustração Científica para Artigos**. Os gráficos foram feitos utilizando o conjunto de dados
#' [*World Happiness Report 2021*](https://www.kaggle.com/ajaypalsinghlo/world-happiness-report-2021) 
#' disponível no Kaggle. Fique a vontade para adaptar e utilizar os códigos para uso pessoal, mas
#' lembre-se de fazer as devidas alterações de acordo com o seu público e a sua mensagem. Este documento
#' é um complemento à apostila do curso.
#'      
#' O objetivo deste material é apresentar alguns modelos de gráficos simples feitos no *R* utilizando 
#' pacote *ggplot2*. Esses gráficos são úteis na representação de distribuição de dados, relação entre variáveis e 
#' quantidades, conforme especificado nos tópicos deste documento. Note que há varias outras 
#' possibilidades de representação gráfica para atender esses objetivos. A escolha do tipo 
#' de gráfico e dos elementos a serem priorizados depende do contexto da comunicação visual,
#' em que quatro perguntas essenciais devem ser consideradas:
#'   
#' * Por quê?
#' * O quê?
#' * Para quem?
#' * Como?  
#' 
#' # Gráficos
#' 
#+ echo=FALSE, results='hide'
#' ## Carregando pacotes necessários
library(ggridges)
library(ggplot2)
library(RColorBrewer)
library(dplyr)

#' ## Carregando o conjunto de dados "World Happiness Report" 

df = read.csv("world-happiness-report-2021.csv", head=T, sep = ",", na.strings = NA, encoding = "UTF-8", dec = ",")

#' Transformando varíaveis
df[3:ncol(df)] = data.frame(sapply(df[3:ncol(df)], as.numeric))

#' Filtandro apenas colunas que nos interessam

df = df[,c(1:3, 7:13)]
#' Alterando o nome das colunas

colnames(df) = c("País", "Região", "NotaGeral", "RendaPerCapita", "SuporteSocial", 
                 "ExpectativaDeVidaSaudável", "LiberdadeParaTomarDecisões", 
                 "Generosidade", "PercepçãoDeCorrupção",
                 "NotaDeDistopia")
#' ## Distribuições

#' Observando a distribuição dos dados:  

#' **Distribuição de generosidade por região** - Densidades
ggplot(df, aes(x = Generosidade, y = Região)) +
  geom_density_ridges(fill = "lightblue") +
  scale_x_continuous(name= "Índice de generosidade")+
  scale_y_discrete(name = "Região")+
  theme_classic(base_size = 12)

#' **Histograma dos dados de Suporte Social**
d1 = ggplot(df, aes(x = SuporteSocial)) +
  geom_histogram(binwidth = 0.03, fill = "steelblue", color = "darkblue") +
  guides(color=guide_legend(title="Região")) +
  scale_x_continuous(name= "Suporte social")+
  scale_y_continuous(name = "Frequência") +
  theme_classic(base_size = 12) 
d1

#' **Gráfico de densidade da distribuição dos dados de Suporte Social**   
#' Mesma informação do histograma, representada de outra forma.

d2 = ggplot(df, aes(x = SuporteSocial)) +
  geom_density(fill = "steelblue") +
  guides(color=guide_legend(title="Região")) +
  scale_x_continuous(name= "Suporte social")+
  scale_y_continuous(name = "Frequência") +
  theme_classic(base_size = 12) 
d2

#' **Gráfico de violino da distribuição dos dados de Suporte Social para cada categoria de Região**   
d3 = ggplot(df, aes(x = Região, y = SuporteSocial)) +
  geom_violin(fill = "steelblue") +
  scale_x_discrete(name= "")+
  scale_y_continuous(name = "Suporte social") +
  theme_classic(base_size = 12) +
  guides(x = guide_axis(angle = 45))
d3

#' **Boxplot da distribuição dos dados de Suporte social para cada categoria de Região**   
d4 = ggplot(df, aes(x = Região, y = SuporteSocial)) +
  geom_boxplot(fill = "steelblue") +
  guides(color=guide_legend(title="Região")) +
  scale_x_discrete(name= "")+
  scale_y_continuous(name = "Suporte social") +
  theme_classic(base_size = 12) +
  guides(x = guide_axis(angle = 45))
d4

#' **Combinando os graficos de distribuição dos dados de Suporte social em um painel**

d = cowplot::plot_grid(d1, d2, d3, d4,  ncol = 2, nrow = 2)
d

#' ## Relações

#' **Gráfico de pontos relacionando as variáveis Renda per capita (eixo x) e Suporte social (eixo y)**
#' As regiões são separadas por cores 
ggplot(df, aes(x = SuporteSocial, y = RendaPerCapita, color = Região)) +
  geom_point() +
  guides(color=guide_legend(title="Região")) +
  scale_x_continuous(name= "Renda per capita")+
  scale_y_continuous(name= "Suporte social") +
  theme_classic()

#' **Énfase na média e nos valores em torno da média**  
#' Gráfico de pontos relacionando as variáveis Renda per capita (eixo x) e Suporte social (eixo y)
#' e resumo estatístico enfatizando o intervalo de confiança (sombreado) e a média (linha)
#' para as duas categorias selecionadas: América Latina e Sudeste Asiático. Veja também
#' que é possível fazer alguma comparação entre as duas regiões para essas duas variáveis.  

df %>% 
  filter(.$Região == "Latin America and Caribbean" | .$Região == "Southeast Asia") %>% 
  ggplot(., aes(x = SuporteSocial, y = RendaPerCapita, color = Região)) +
  geom_point() +
  geom_smooth(aes(color = Região, fill =  Região), method = "lm") +
  guides(color=guide_legend(title="Região"), fill=guide_legend(title="Região")) +
  scale_color_manual(values = c('#af8dc3','#7fbf7b')) +
  scale_fill_manual(values = c('#af8dc3','#7fbf7b')) +
  scale_x_continuous(name= "Renda per capita")+
  scale_y_continuous(name= "Suporte social") +
  theme_classic()

#' *Outro exemplo*  
#' Combinado mais de duas varíaveis no gráfico para observar relação
#' **Gráfico de pontos América Latina**
df %>% 
  filter(.$Região == "Latin America and Caribbean") %>% 
  arrange(., NotaGeral) %>% 
  ggplot(., aes(x = SuporteSocial, y = RendaPerCapita, color = NotaGeral, size = Generosidade)) +
  geom_point() +
  scale_x_continuous(name= "Suporte social") +
  scale_y_continuous(name = "Renda per capita") +
  guides(color=guide_legend(title= "Índice de felicidade")) +
  guides(size=guide_legend(title= "Índice de generosidade")) +
  theme_classic(base_size = 12)
  
#' ## Quantidades

#' Quantidade de paises por região, que estão participando da pesquisa
#' **Gráfico de barras**
df %>% 
  group_by(., Região) %>% 
  count() %>% 
  ggplot(., aes(x = Região, y = n)) +
  scale_y_continuous(name = "Número de países avaliados") +
  geom_col(fill = "#456573" ) +
  scale_x_discrete(name= "") +
  guides(x = guide_axis(angle = 45)) +
  theme_classic(base_size = 12)

#' Outra opção, neste caso, menos eficaz para comunicar o número de paises por região para
#' este conjunto de dados.
#' **Gráfico de pontos com escala de cor para possível impressão em escala de cinza**
#' 
df %>% 
  group_by(., Região) %>% 
  count(., Região) %>% 
  ggplot(., aes(x = Região, y = n)) +
    geom_point(aes(colour = n)) +
    scale_x_discrete(name= "") +
    scale_y_continuous(name = "Número de países avaliados") +
    guides(x = guide_axis(angle = 45)) +
    guides(color=guide_legend(title= "Países")) +
    guides(size=guide_legend(title= "")) +
    theme_classic(base_size = 12)

#' Para conhecer mais sobre gráficos, recomendamos a leitura do livro [*Fundamentals of Data Visualization*](https://clauswilke.com/dataviz/)
#' de Claus O. Wilke.  
#' 
#' Para aprender sobre sobre o *ggplot2*, há muito material de qualidade disponível na internet,
#' mas nós recomendamos o [guia oficial do pacote](https://ggplot2.tidyverse.org/) para conhecer
#' as diversas possibilidades de uso.
#' 
#' Os dois materiais são gratuitos para acesso online. 



