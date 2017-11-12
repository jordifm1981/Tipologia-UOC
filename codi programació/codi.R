#Carrego la llibreria rvest 
library('rvest')

#url de la web de la qual vull extreure les dades. Analitzant la url, poso els registres que m'interessen manualment
url <- 'http://www.atpworldtour.com/en/rankings/singles/?rankDate=2017-11-6&countryCode=all&rankRange=1-1000'

#Carrego el codi html de la web
webpage <- read_html(url)

# Agafem els nodes que són taules
sb_table <- html_nodes(webpage, 'table')

# Mirem quantes taultes hi ha
length(sb_table)

# Només n'hi ha una que és la que ens interessa
players <- html_table(sb_table)[[1]]
head(players)

#El país es mostra amb una imatge de la bandera. Busco l'element i amb quedo amb l'atribut "alt" de la imatge on hi ha el país en text
pais_players_html <- html_nodes(webpage,'.country-cell img')
pais_players_html
pais_players <- html_attr(pais_players_html, "alt")
pais_players

#Poso la informació del païs en text a la columna "Country".
players["Country"] <- pais_players

#De forma semblant, busco la url de la pàgina de cada jugador a l'atribut "href" del nom del jugador.
url_players_html <- html_nodes(webpage,'.player-cell a')
url_players <- html_attr(url_players_html, "href")
url_players

#Completo les url, ja que a l'atribut "href" apareixien contextualitzades
url_players <- paste0('http://www.atpworldtour.com',url_players)
url_players

#Creo un data frame per fer el bucle i guardar la informació de cada jugador
n <- length(url_players)
df <- data.frame(Characters=character(),
                 Characters=character(),
                 Characters=character(),
		 Characters=character(),
		 Characters=character(),
                 stringsAsFactors=FALSE)

colnames(df)<-c("WL","TITOLS","GUANYS", "RANKINGDOBLES", "GUANYSDOBLES")

# Faig el bucle per agafar les dades dels jugadors recorrent totes les url corresponents
# Agafem els nodes que són taules
# Com que tenim vaires taules, em quedo la que hi apareix la informació que vull.
# Poso la informació que m'interessa de cada jugador per files al data frame creat
# Busco informació que no surt a la taula, la de la modalitat de dobles, que està guardada als atributs de la div de la taula
# Guardo aquesta informació al data frame
library(xml2)
for (i in 1:n){
	url2 <- url_players[i]
	webpage2 <- read_html(url2)
	sb_table2 <- html_nodes(webpage2, 'table')
	players2 <- html_table(sb_table2[[2]], fill=TRUE)
	df[i,]<-players2[1,5:7]
	infodobles <- read_html(url2) %>% html_nodes(xpath = '//*[@class="stat-value"]') %>% xml_attr("data-doubles")
	df[i,4:5]<-infodobles[c(1,5)]
	}

head(df)

# Netejo les dades
df$WL<-gsub("([ \t\n\r\f\v]+).*", "", df$WL)
df$TITOLS<-gsub("([ \t\n\r\f\v]+).*", "", df$TITOLS)
df$GUANYS<-gsub('[$]([0-9]*.+)([0-9]+)[ \t\n\r\f\v].*','\\1\\2\\3', df$GUANYS)
df$GUANYS<-gsub(",", "", df$GUANYS)
df$GUANYSDOBLES<-gsub("[$]", "", df$GUANYSDOBLES)
df$GUANYSDOBLES<-gsub(",", "", df$GUANYSDOBLES)

df$GUANYS<-replace(df$GUANYS,df$GUANYS=="Prize Money","0")
df$GUANYSDOBLES<-replace(df$GUANYSDOBLES,df$GUANYSDOBLES=="","0")
head(df)

# Fem la suma dels guanys
df$GUANYS<-as.integer(df$GUANYS)
df$GUANYSDOBLES<-as.integer(df$GUANYSDOBLES)
totals <- df$GUANYS + df$GUANYSDOBLES

# Adjuntem els totals al data frame
df<-cbind(df,totals)
head(df)


# Elimino els atributs que no m'interessen de la primera base de dades
players <- players[-c(2, 8, 9)]
# Adjunto els nous atributs que hem buscat i creat
players <- cbind(players, df)

# Cambio l'ordre d'alguna columna
aux<-players[,10]
players[,10]<-players[,9]
players[,9]<-aux

# Canvio el nom de les columnes
colnames(players)<-c("RANKING", "PAIS", "JUGADOR", "EDAT", "PUNTS", "TORNEJOS", "WL", "TITOLS", "RANKING_DOB", "GUANYS_IND", "GUANYS_DOB", "GUANYS_TOT")

head(players)
# Ara només ens quedaria crear l'arxiu .csv del dataset, però...
# Repassant les dades obtingudes es veuen alguns resultats estranys. Per exemple, el jugador 951 del rànking, ha jugat un torneig i n'ha guanyat 3.
# Investigant aquests pocs casos (3 o 4 de 1000), he vist que la pàgina de detall del jugador apareix directament amb la informació dels dobles, no la individual.
# Repassem la primera taula de la qual hem tret la informació i és totalment correcte i coherent. Només hem de rectificar aquests casos.
# Així, cal tornar a buscar aquesta informació, assegurant que és la correcte. Ho fem mirant la informació dels atributs de la div, no la informació que es mostra a la taula,
# i així tenim segur la informació que volem referent a la categoria individuals o dobles, independentment de la que es mostra en pantalla.

for (i in 1:n){
	url2 <- url_players[i]
	infosingles <- read_html(url2) %>% html_nodes(xpath = '//*[@class="stat-value"]') %>% xml_attr("data-singles")
	df[i,1:3]<-infosingles[c(3,4,5)]
	}

df$GUANYS<-gsub("[$]", "", df$GUANYS)
df$GUANYS<-gsub(",", "", df$GUANYS)
df$GUANYS<-replace(df$GUANYS,df$GUANYS=="","0")

# Fem la suma dels guanys i posem les dades correctes al conjunt de dades final
players$GUANYS_IND<-as.integer(df$GUANYS)
players$GUANYS_TOT <- players$GUANYS_IND + players$GUANYS_DOB
players$TITOLS <- df$TITOLS
players$WL <- df$WL

head(players)

# Creo el dataset en format csv per guardar el resultat final de les dades obtingudes
write.csv(players, file = "ATP_jugadors_guanys_2017.csv")


# Evidentment, hagués pogut substituir la part de codi per anar a buscar les dades correctes directament,
# però he cregut interessant mostrar el caràcter no lineal d'aquests tipus de mètodes oberts a redefinicions.

