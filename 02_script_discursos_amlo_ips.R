Sys.setlocale("LC_TIME", "es_ES")

# Paquetes ----
if(!require("rvest")) install.packages("rvest") & require("rvest")
if(!require("tm")) install.packages("tm") & require("tm")
if(!require("tidytext")) install.packages("tidytext") & require("tidytext")
if(!require("haven")) install.packages("haven") & require("haven")
if(!require("foreign")) install.packages("foreign") & require("foreign")
if(!require("wordcloud2")) install.packages("wordcloud2") & require("wordcloud2")
if(!require("gganimate")) install.packages("gganimate") & require("gganimate")
if(!require("ggcorrplot")) install.packages("ggcorrplot") & require("ggcorrplot")
if(!require("gridExtra")) install.packages("gridExtra") & require("gridExtra")
if(!require("ggthemes")) install.packages("ggthemes") & require("ggthemes")
if(!require("hrbrthemes")) install.packages("hrbrthemes") & require("hrbrthemes")
if(!require("magick")) install.packages("magick") & require("magick")
if(!require("scales")) install.packages("scales") & require("scales")
if(!require("RColorBrewer")) install.packages("RColorBrewer") & require("RColorBrewer")
if(!require("lubridate")) install.packages("lubridate")  & require("lubridate")
if(!require("zoo")) install.packages("zoo")  & require("zoo")
if(!require("smooth")) install.packages("smooth") & require("smooth")
if(!require("forecast")) install.packages("forecast") & require("forecast")
if(!require("Mcomp")) install.packages("Mcomp") & require("Mcomp")
if(!require("quanteda")) install.packages("quanteda") & require("quanteda")
if(!require("quanteda.corpora")) install.packages("quanteda.corpora") & require("quanteda.corpora")
if(!require("seededlda")) install.packages("seededlda") & require("seededlda")
if(!require("yaml")) install.packages("yaml") & require("yaml")
require(tidyverse)

# Funciones ----
# Negar
`%notin%` <- negate(`%in%`) 
# Quitar acentos
rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str) 
  
  return(str)
}


# Directorios ----
paste_inp <- function(x){paste0("MCV/discursos_amlo/01_datos/", x)}
paste_out <- function(x){paste0("MCV/discursos_amlo/03_productos/", x)}

# Leyenda de todas las gráficas ----
fiuffi <- "Elaboración de @MexicoComoVamos con información pública de la Oficina de la Presidencia y el Índice de Progreso Social | @guzmart_"

# Colores ----
ips_discrete <- c(
  '#00B7CE', '#F7943F', '#ACCC4A', '#5A494E', '#C64736'
)

# Scrapping ----
# Sólo seguir este paso si se desea replicar la extracción de datos.
# Los discursos utilizados se encuentran en la carpeta '01_datos'

amlo_url <- "https://presidente.gob.mx/secciones/version-estenografica/"

drop <- read_html(amlo_url) %>% 
  html_nodes("div.pagenavi") %>% 
  html_nodes("a") %>% 
  html_attr("href")

# DF que contendrá todas las observaciones
final <- data.frame()

# x = número de página y loop
for(x in 1:as.numeric(str_remove_all(str_sub(drop[length(drop)],-3), "/"))){ 
  # urls de las páginas en dónde se guardan las versiones estenográficas
  amlos <- read_html(paste0(amlo_url, "page/", as.character(x),"/"))
  
  # urls de las versiones estenográficas
  urls <- amlos %>% 
    html_nodes("header.entry-header") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  # títulos
  titles <- amlos %>% 
    html_nodes("h2.entry-title") %>% 
    html_text()
  
  # fechas
  fechas <- 
    as.Date.character(
      str_replace_all(
        paste0(str_sub(titles,1,6),"20",str_sub(titles,7,8)),
        "\\.","-"
      ), format = "%d-%m-%Y"
    )
  
  
  
  # base temporal por página
  tempo <- data.frame(
    url = urls,
    título = titles,
    fecha = fechas
  )
  
  bodies <- c()
  # for para extraer títulos y textos
  for(i in tempo$url){
    
    wbpg <- read_html(i)
    
    body <- wbpg %>%
      html_nodes("p") %>%
      html_text()
    one_body <- paste(body, collapse=" ")
    bodies <- append(bodies, one_body)
    
  }
  
  tempo$texto <- bodies
  tempo$loop <- as.character(x)
  
  # se guarda cada loop en final
  final <- bind_rows(final, tempo)
  rm(tempo,urls,fechas,titles,bodies,wbpg,body,one_body)
}
beepr::beep(1)

rm(drop)

final <- final %>% 
  mutate(
    texto = str_replace_all(texto,"PRESIDENTE ANDRÉS MANUEL LÓPEZ OBRADOR:", "~~~PRESIDENTE ANDRÉS MANUEL LÓPEZ OBRADOR"),
    texto = str_replace_all(texto,"MODERADOR:", "~~~INTERLOCUTORA"),
    texto = str_replace_all(texto,"INTERLOCUTOR:", "~~~INTERLOCUTORA"),
    texto = str_replace_all(texto,"INTERLOCUTORA:", "~~~INTERLOCUTORA"),
    texto = str_replace_all(texto,"PREGUNTA:", "~~~PREGUNTA"),
    texto = paste0("~~~PRESIDENTE ANDRÉS MANUEL LÓPEZ OBRADOR",
                   sub(".*? ~~~PRESIDENTE ANDRÉS MANUEL LÓPEZ OBRADOR","",texto))
  )

# write_csv(final, path = paste0(inp,"discursos_amlo_20200818_20181204.csv"))

# Data ----
# Se recopilaron 830 discursos
d <- read_csv(paste_inp("discursos_amlo_20201204_20181204.csv"))

# Semi-automatización de actualizaciones ----
url_scrapeados <- unique(d$url)
tit_scrapeados <- unique(d$título)

amlo_url <- "https://presidente.gob.mx/secciones/version-estenografica/"

drop <- read_html(amlo_url) %>% 
  html_nodes("div.pagenavi") %>% 
  html_nodes("a") %>% 
  html_attr("href")


x <- 1

final <- data.frame()

while (x <= as.numeric(str_remove_all(str_sub(drop[length(drop)],-3), "/"))){
  amlos <- read_html(paste0(amlo_url, "page/", as.character(x),"/"))
  
  # urls de las versiones estenográficas
  urls <- amlos %>% 
    html_nodes("header.entry-header") %>% 
    html_nodes("a") %>% 
    html_attr("href") 
  
  urls <- urls[urls%notin%url_scrapeados]
  
  if (length(urls)>0){
    titles <- amlos %>% 
      html_nodes("h2.entry-title") %>% 
      html_text()
    
    titles <- titles[titles%notin%tit_scrapeados]
    
    # fechas
    fechas <- 
      as.Date.character(
        str_replace_all(
          paste0(str_sub(titles,1,6),"20",str_sub(titles,7,8)),
          "\\.","-"
        ), format = "%d-%m-%Y"
      )
    
    
    
    # base temporal por página
    tempo <- data.frame(
      url = urls,
      título = titles,
      fecha = fechas
    )
    
    bodies <- c()
    # for para extraer títulos y textos
    for(i in tempo$url){
      
      wbpg <- read_html(i)
      
      body <- wbpg %>%
        html_nodes("p") %>%
        html_text()
      one_body <- paste(body, collapse=" ")
      bodies <- append(bodies, one_body)
      
    }
    
    tempo$texto <- bodies
    tempo$loop <- as.character(x)
    
    # se guarda cada loop en final
    final <- bind_rows(final, tempo)
    rm(tempo,urls,fechas,titles,bodies,wbpg,body,one_body)
    
  } else {
    print(paste0("No se encontraron nuevas apariciones públicas en loop ", as.character(x)))   
  }
  x <- x +1
}


beepr::beep(1)

rm(drop)

final <- final %>% 
  mutate(
    texto = str_replace_all(texto,"PRESIDENTE ANDRÉS MANUEL LÓPEZ OBRADOR:", "~~~PRESIDENTE ANDRÉS MANUEL LÓPEZ OBRADOR"),
    texto = str_replace_all(texto,"MODERADOR:", "~~~INTERLOCUTORA"),
    texto = str_replace_all(texto,"INTERLOCUTOR:", "~~~INTERLOCUTORA"),
    texto = str_replace_all(texto,"INTERLOCUTORA:", "~~~INTERLOCUTORA"),
    texto = str_replace_all(texto,"PREGUNTA:", "~~~PREGUNTA"),
    texto = paste0("~~~PRESIDENTE ANDRÉS MANUEL LÓPEZ OBRADOR",
                   sub(".*? ~~~PRESIDENTE ANDRÉS MANUEL LÓPEZ OBRADOR","",texto))
  )

d <- bind_rows(final %>% select(-loop), d)

write_csv(d, path = paste_inp("discursos_amlo_20201204_20181204.csv"))


# Limpiar texto ----
# Reemplazar 'Presidente...' por 'AMLO'
d <- read_csv(paste_inp("discursos_amlo_20201204_20181204.csv"))
data_amlo <- d %>% 
  mutate(
    texto = gsub("PRESIDENTE ANDRÉS MANUEL LÓPEZ OBRADOR", "AMLO", texto),
    texto = gsub("\\---.*","",texto),
    texto = str_remove_all(texto, "[[:digit:]]+")
      ) %>% 
  # Mantener sólo intrevenciones de AMLO
  unnest_tokens("texto", input = texto, token = stringr::str_split, pattern = "~~~") %>% 
  mutate(amlo = ifelse(str_detect(texto, "amlo"),1,0),
         texto = str_remove_all(texto, "amlo"),
         texto = str_trim(texto)) %>% 
  # Existen 48 entradas con errores en fecha
  
  filter(fecha > "2018-11-30", 
         # CAMBIAR FECHA !!!!
         fecha < "2020-12-05") %>% 
  subset(amlo==1) %>% select(-amlo) %>% 
  mutate(id_amlo = paste(row_number(), str_remove_all(as.character(fecha), "-"), sep = "_"))

# Stopwords ----
custom_stop_words <- as_data_frame(tm::stopwords("es")) %>% 
  bind_rows(as_data_frame(c(
    "si","dijo","así","sólo", "dice", "pues","entonces",
    "ahí","digo","creo","que","en","la","ah","bueno", "bla","tan",
    "te", "iba", "he", "él", "t", "+", "señor"
  ))) %>% 
  rename(palabra = value)

stopwords_regex = paste(custom_stop_words$palabra, collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')

# Seeded LDA ----
# * Diccionario
dict_ips <- dictionary(
  list(
    nutrición_y_cuidados_médicos_básicos =	c("alimentación",	"alimento",	"alimentos",	"comida",	"desnutrición",	"infantil",	"materna",	"maternidad",	"mortalidad",	"nutrición"),
    agua_y_saneamiento =	c("agua",	"digno",	"drenaje",	"saneamiento",	"sanitario",	"servicio",	"servicios"),
    vivienda =	c("concreto",	"digna",	"fogón",	"hacinamiento",	"hogar",	"hogares",	"construcción",	"piso",	"tierra",	"vivienda"),
    seguridad_personal =	c("accidentes",	"crimen",	"delictiva",	"feminicidio",	"feminicidios",	"homicidio",	"homicidios",	"inseguridad",	"seguridad",	"violencia"),
    acceso_a_conocimientos_básicos =	c("alumno",	"alumnos",	"analfabetismo",	"educación",	"matriculación",	"primaria",	"secundaria"),
    acceso_a_información_y_comunicaciones =	c("celular",	"información",	"internet",	"libertad", "prensa",	"telefonía",	"medios",	"teléfono",	"prensa"),
    salud_y_bienestar =	c("circulatorias",	"diabetes",	"etiquetado",	"hipertensión",	"mortalidad",	"obesidad",	"salud",	"suicidio",	"suicidios",	"vida"),
    calidad_medioambiental =	c("agua",	"ambiental",	"ambiente",	"basura",	"contaminación",	"desecho",	"ecología",	"ecológico",	"energía",	"medioambiente",	"parques"),
    derechos_personales =	c("ciudadana",	"ciudadano",	"ciudadanía",	"contratos",	"democracia",	"derechos",	"elecciones",	"electoral",	"negocios",	"participación",	"propiedad"),
    libertad_personal_y_de_elección =	c("adolescente",	"burocracia",	"corrupción",	"embarazo",	"informal",	"informalidad",	"jóvenes",	"juventud",	"juventudes",	"laboral",	"ninis",	"transporte"),
    inclusión =	c("discapacidad",	"discapacidades",	"discriminación",	"diversidad",	"homofobia",	"inclusión",	"indígena",	"indígenas",	"paridad"),
    acceso_a_educación_superior =	c("licenciatura",	"licenciaturas",	"posgrado",	"posgrados",	"universidad")
  )
)

# * A lo largo del tiempo ----
fechas_loop <- c(
  "2018-12-01", 
  
  "2019-01-01",
  "2019-02-01",
  "2019-03-01",
  "2019-04-01",
  "2019-05-01",
  "2019-06-01",
  "2019-07-01",
  "2019-08-01",
  "2019-09-01",
  "2019-10-01",
  "2019-11-01",
  "2019-12-01",
  
  "2020-01-01",
  "2020-02-01",
  "2020-03-01",
  "2020-04-01",
  "2020-05-01",
  "2020-06-01",
  "2020-07-01",
  "2020-08-01",
  "2020-09-01",
  "2020-10-01",
  "2020-11-01",
  "2020-12-01",
  
  "2021-01-01"
)

temas_final <- data.frame()
for(i in 2:(length(fechas_loop))){
  print(
    paste0(
      "Proceso para periodo ", fechas_loop[i-1], " - ", fechas_loop[i]
    )
  )
  
  ttt_tempo <- data_amlo$texto[data_amlo$fecha>=fechas_loop[i-1] & data_amlo$fecha<fechas_loop[i]]
  dfm_tempo <- dfm(corpus(ttt_tempo), remove_punct = TRUE, remove = custom_stop_words$palabra) %>% 
    dfm_trim(min_termfreq = 0.8, termfreq_type = "quantile",
             max_docfreq = 0.1, docfreq_type = "prop")
  
  tmod_slda_tempo <- textmodel_seededlda(dfm_tempo, dictionary = dict_ips)
  
  dfm_tempo$topic2 <- topics(tmod_slda_tempo)
  
  # cross-table of the topic frequency
  temas_tempo <- as.data.frame(table(dfm_tempo$topic2))
  names(temas_tempo) <- c("tema", "n")
  
  temas_tempo <- temas_tempo %>% 
    mutate(
      dim = case_when(
        tema == "acceso_a_conocimientos_básicos" ~ "Fundamentos del Bienestar",
        tema == "acceso_a_educación_superior" ~ "Oportunidades",
        tema == "acceso_a_información_y_comunicaciones" ~ "Fundamentos del Bienestar",
        tema == "agua_y_saneamiento" ~ "Necesidades Humanas Básicas",
        tema == "calidad_medioambiental" ~ "Fundamentos del Bienestar",
        tema == "derechos_personales" ~ "Oportunidades",
        tema == "inclusión" ~ "Oportunidades",
        tema == "libertad_personal_y_de_elección" ~ "Oportunidades",
        tema == "nutrición_y_cuidados_médicos_básicos" ~ "Necesidades Humanas Básicas",
        tema == "salud_y_bienestar" ~ "Fundamentos del Bienestar",
        tema == "seguridad_personal" ~ "Necesidades Humanas Básicas",
        tema == "vivienda" ~ "Necesidades Humanas Básicas",
        T ~ "99 CHECK"
      ),
      periodo_inicio = fechas_loop[i-1],
      periodo_fin = fechas_loop[i]
    )
  
  temas_final <- bind_rows(temas_final, temas_tempo)
  rm(ttt_tempo, dfm_tempo, tmod_slda_tempo, temas_tempo)
}
beepr::beep(5)

# * Ejes temáticos -----
ttt <- data_amlo$texto
dfm <- dfm(corpus(ttt), remove_punct = TRUE, remove = custom_stop_words$palabra) %>% 
  dfm_trim(min_termfreq = 0.8, termfreq_type = "quantile",
           max_docfreq = 0.1, docfreq_type = "prop")

tmod_slda <- textmodel_seededlda(dfm, dictionary = dict_ips)


dfm$topic2 <- topics(tmod_slda)

# cross-table of the topic frequency
temas <- as.data.frame(table(dfm$topic2))
names(temas) <- c("tema", "n")

temas <- temas %>% 
  mutate(
    dim = case_when(
      tema == "acceso_a_conocimientos_básicos" ~ "Fundamentos del Bienestar",
      tema == "acceso_a_educación_superior" ~ "Oportunidades",
      tema == "acceso_a_información_y_comunicaciones" ~ "Fundamentos del Bienestar",
      tema == "agua_y_saneamiento" ~ "Necesidades Humanas Básicas",
      tema == "calidad_medioambiental" ~ "Fundamentos del Bienestar",
      tema == "derechos_personales" ~ "Oportunidades",
      tema == "inclusión" ~ "Oportunidades",
      tema == "libertad_personal_y_de_elección" ~ "Oportunidades",
      tema == "nutrición_y_cuidados_médicos_básicos" ~ "Necesidades Humanas Básicas",
      tema == "salud_y_bienestar" ~ "Fundamentos del Bienestar",
      tema == "seguridad_personal" ~ "Necesidades Humanas Básicas",
      tema == "vivienda" ~ "Necesidades Humanas Básicas",
      T ~ "99 CHECK"
    )
  )
beepr::beep(1)

# Productos ----
# 1. Temas más mencionados ----
fiuf <- "Ejes temáticos del IPS con mayor número de referencias"
fiuff <- "en las apariciones públicas de AMLO"
ggplot(
  temas %>% 
    left_join(
      temas %>% 
        group_by(dim) %>% 
        summarise(tot = sum(n))
    ),
  aes(
    y = reorder(str_to_sentence(str_replace_all(tema, "_", " ")), n),
    x = n,
    fill = paste0(dim, "\n", prettyNum(tot, big.mark = ",")),
    label = prettyNum(n, big.mark = ",")
  )
) + 
  geom_col() + geom_text(show.legend = F, size = 6, hjust = -0.2) +
  scale_fill_manual("", values = c(ips_discrete[2],ips_discrete[1], ips_discrete[3])) +
  scale_x_continuous(labels = scales::comma, limits = c(0,3000), breaks = seq(1000,3000,500)) +
  labs(title= str_wrap(fiuf, width = 75),
       subtitle = fiuff,
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")

ggsave(filename = paste_out("01_temas_freq.png"), width = 18, height = 12, dpi = 100, bg = "transparent")


# 2. Dimensiones a lo largo del tiempo ----
fiuf <- "Referencias a las dimensiones del IPS"
fiuff <- "en las apariciones públicas de AMLO, a lo pargo del tiempo\nPromedio móvil por 4 meses"
ggplot(
  temas_final %>% 
    group_by(dim, periodo_inicio) %>% 
    summarise(tot = sum(n)) %>% 
    mutate(mv_av = round(rollapply(tot, 4, mean, align='right',fill=NA), 1),
           periodo_inicio = as.Date(periodo_inicio)),
  aes(
    x = periodo_inicio,
    y = mv_av,
    group = dim,
    col = dim
  )
) + 
  geom_line(size = 2) + geom_point(size = 4) +
  scale_color_manual("", values = c(ips_discrete[2],ips_discrete[1], ips_discrete[3])) +
  scale_x_date(labels = date_format("%b-%Y"), date_breaks = "3 month",limits = c(as.Date("2019-02-01"),as.Date("2021-01-01"))) +
  scale_y_continuous(limits = c(100,300), breaks = seq(100,300,50)) +
  labs(title= str_wrap(fiuf, width = 75),
       subtitle = fiuff,
       caption = fiuffi) +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold" , hjust = 0.5),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Arial Narrow"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")
ggsave(filename = paste_out("02_dim_tiempo.png"), width = 17, height = 12, dpi = 100, bg = "transparent")


openxlsx::write.xlsx(as.data.frame(terms(tmod_slda, 50)), paste_out("01_seeded_lda.xlsx"))
write_yaml(dict_ips, paste_inp("00_diccionarios_ips.yml"))

# 3. Palabras relacionadas con ejes temáticos
terms_1000 <- terms(tmod_slda, 1000) %>% 
  as_data_frame()
openxlsx::write.xlsx(
  terms_1000 %>% 
    mutate(
      nutrición_y_cuidados_médicos_básicos = str_remove_all(nutrición_y_cuidados_médicos_básicos, paste(dict_ips$nutrición_y_cuidados_médicos_básicos, collapse = "|")),
      agua_y_saneamiento = str_remove_all(agua_y_saneamiento, paste(dict_ips$agua_y_saneamiento, collapse = "|")),
      vivienda = str_remove_all(vivienda, paste(dict_ips$vivienda, collapse = "|")),
      seguridad_personal = str_remove_all(seguridad_personal, paste(dict_ips$seguridad_personal, collapse = "|")),
      acceso_a_conocimientos_básicos = str_remove_all(acceso_a_conocimientos_básicos, paste(dict_ips$acceso_a_conocimientos_básicos, collapse = "|")),
      acceso_a_información_y_comunicaciones = str_remove_all(acceso_a_información_y_comunicaciones, paste(dict_ips$acceso_a_información_y_comunicaciones, collapse = "|")),
      salud_y_bienestar = str_remove_all(salud_y_bienestar, paste(dict_ips$salud_y_bienestar, collapse = "|")),
      calidad_medioambiental = str_remove_all(calidad_medioambiental, paste(dict_ips$calidad_medioambiental, collapse = "|")),
      derechos_personales = str_remove_all(derechos_personales, paste(dict_ips$derechos_personales, collapse = "|")),
      libertad_personal_y_de_elección = str_remove_all(libertad_personal_y_de_elección, paste(dict_ips$libertad_personal_y_de_elección, collapse = "|")),
      inclusión = str_remove_all(inclusión, paste(dict_ips$inclusión, collapse = "|")),
      acceso_a_educación_superior = str_remove_all(acceso_a_educación_superior, paste(dict_ips$acceso_a_educación_superior, collapse = "|"))
    ),
  paste_out("03_seeded_lda_terms.xlsx")
)



