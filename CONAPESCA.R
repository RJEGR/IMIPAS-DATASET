# OPEN XLSX TIME-BOUND DATA
# GET SPECIES AQUACULTURE/FISHERING BY STATE
# SUBSET SPECIES FROM B.C

library(readxl)

library(tidyverse)
library(readxl)

path <- 'D:/CONAPESCA/XLS/'

setwd(path)

list.files(path = path)
           
# 


f <- list.files(path = path, pattern = '.xlsx', full.names = T)

read_excel_f(f[2])

sapply(strsplit(my_custom_name_repair(f), "_"), `[`, 7)
my_custom_name_repair(f)

my_custom_name_repair <- function(x) tolower(gsub(".xlsx", " ", basename(x)))

read_excel_f <- function(x){
  Year <- my_custom_name_repair(x)
  df <- read_excel(x) %>% mutate(Year = Year)
  return(df)
}

df <- lapply(f, read_excel_f ) %>%
  bind_rows()

df %>% count(Year)

names(df)


SPECIE <- c("ENTIDAD", "ORIGEN") # "NOMBREPRINCIPALESPECIE",	
VALOR <- c("PESO VIVO (KG)",	"VALOR (PESOS)")
TIEMPO <- c("ANO") # "MES"

# PESCA VS ACUICULTURA 
df %>%
  group_by_at(c("ORIGEN", "ANO")) %>%
  summarise_at(VALOR, sum) %>%
  ggplot(aes(x = ANO, y = `PESO VIVO (KG)`, fill = ORIGEN)) +
  geom_col(position = position_dodge2())

# ACUICULTURA BY STATE
df %>%
  group_by_at(c(SPECIE, TIEMPO)) %>%
  summarise_at(VALOR, sum) %>%
  group_by(ORIGEN) %>%
  mutate(Frac = `PESO VIVO (KG)` / sum(`PESO VIVO (KG)`)) %>%
  arrange(desc(`PESO VIVO (KG)`)) %>%
  mutate(ENTIDAD = factor(ENTIDAD, levels = unique(ENTIDAD))) %>%
  ggplot(aes(x = ENTIDAD, y = Frac)) +
  geom_col() +
  facet_grid(ORIGEN ~ ., scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent)

# PRECIO VS CANTIDAD
library(rstatix)

df %>%
  group_by_at(c("ORIGEN", "ENTIDAD")) %>%
  summarise_at(VALOR, sum) %>%
  # cor_test(vars = `PESO VIVO (KG)`, vars2 = `VALOR (PESOS)`) %>%
  ggplot(aes(x = `PESO VIVO (KG)`, y = `VALOR (PESOS)`, color = ORIGEN)) +
  geom_point() +
  geom_smooth(se = T, method = lm) +
  ggpubr::stat_cor(aes( group = ORIGEN), cor.coef.name = "R", 
                   p.accuracy = 0.001, color = 'black')
  
# ACUICULTURA VS CAPTURA CORRELACION? 

df %>%
  group_by_at(c("ORIGEN", "ENTIDAD")) %>%
  summarise_at("PESO VIVO (KG)", sum) %>%
  pivot_wider(names_from = ORIGEN, values_from = `PESO VIVO (KG)`) %>%
  cor_test(vars = ACUACULTURA, vars2 = CAPTURA)


# IN BC ?
# ya que hay una relacion R positiva significativa > 0.7, usamos cualquier Valor (peso o valor)

df %>% 
  # filter(ANO > 2019) %>%
  distinct(NOMBREPRINCIPALESPECIE, ENTIDAD) %>% 
  count(ENTIDAD, sort = T) %>%
  arrange(-desc(n)) %>%
  mutate(ENTIDAD = factor(ENTIDAD, levels = unique(ENTIDAD))) %>%
  ggplot(aes(x = n, y = ENTIDAD)) +
  geom_col() 

# BC alberga el mayor numero de sp acuicolas junto a veracruz, aunque no representa 
# una fraccion de la produccion total acuicuola prioritaria, 
# con 9% y 2% para la CAPTURA y ACUICULTURA

# QUE ESPECIES EN BC?
# ATUN, OSTION Y LOBINA FIGURAN DENTRO DE LAS SP MAS PRODUCIDAS POR ACUICULTURA,
# el abulon tmb entra dentro de las primeras 5 sp
# LA SARDINA, ANCHOVETA, CALAMAR, Y ERIZO FIGURAN DENTRO DE LAS MAS PRODUCIDAS VIA CAPTURA 
df %>%
  filter(ENTIDAD %in% "BAJA CALIFORNIA") %>%
  group_by_at(c("ORIGEN","NOMBREPRINCIPALESPECIE")) %>%
  summarise_at(VALOR, sum) %>%
  # Convertir a fraccion del total
  group_by(ORIGEN) %>%
  mutate(Frac = `PESO VIVO (KG)` / sum(`PESO VIVO (KG)`)) %>%
  arrange(desc(Frac)) %>%
  mutate(NOMBREPRINCIPALESPECIE = factor(NOMBREPRINCIPALESPECIE, levels = unique(NOMBREPRINCIPALESPECIE))) %>%
  ggplot(aes(x = Frac, y = NOMBREPRINCIPALESPECIE, fill = ORIGEN)) +
  geom_col() +
  facet_grid(ORIGEN ~ ., scales = "free_y", space = "free") +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(labels = scales::percent)
  

# CUANTO DINERO REPRESENTA anualmente ESTO?

df %>%
  filter(ENTIDAD %in% "BAJA CALIFORNIA") %>%
  group_by_at(c("ORIGEN", "ANO")) %>%
  summarise_at(VALOR, sum) %>% view()
  ggplot(aes(y = `VALOR (PESOS)`, x = ANO, fill = ORIGEN)) +
  geom_col() +
  scale_y_continuous(labels = scales::dollar)
