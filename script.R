df <- read.csv("./figure1.csv", stringsAsFactors = FALSE)
df$Date <- as.POSIXct(df$Date)
library(ggplot2)
png("./figure1.png", width = 800, height = 500)
pdf("./figure1.pdf", width = 10, height = 7)
ggplot(df, aes(x=Date,y=FN)) + 
  geom_point(aes(shape=type), size=3) + 
  geom_line(aes(group=type, colour=type)) + 
  theme_bw() + 
  scale_shape_discrete(name="",labels=c("Cantonales", "Européennes", "Législatives", "Présidentielles", "Régionales")) + 
  scale_colour_discrete(name="",labels=c("Cantonales", "Européennes", "Législatives", "Présidentielles", "Régionales")) + 
  scale_y_continuous(limits=c(0,17.5)) + 
  theme(legend.position="bottom") + 
  xlab("") + 
  ylab("Vote FN, en % des inscrits")
dev.off()
dev.off()



for (i in 5:nrow(df)) {
  if (df[i, "type"] %in% "Pre") {
    tmp <- df[i, "FN"]
  }
  df[i, "pres"] <- tmp
  df[i, "ratio"] <- df[i, "FN"] / df[i, "pres"]
}

png("./figure2.png", width = 800, height = 500)
pdf("./figure2.pdf", width = 10, height = 7)
ggplot(df[!df$type %in% "Pre", ], aes(x = Date, y = ratio, group = type)) +
  geom_point(aes(shape=type), size=3) + 
  geom_line(aes(group=type, colour=type)) + 
  theme_bw() + 
  scale_shape_discrete(name="",labels=c("Cantonales", "Européennes", "Législatives", "Régionales")) + 
  scale_colour_discrete(name="",labels=c("Cantonales", "Européennes", "Législatives","Régionales")) + 
  theme(legend.position="bottom") + 
  xlab("") + 
  ylab("Rapport au score lors de l'élection présidentielle précédente")
dev.off()
dev.off()

write.csv(df, "./figure2.csv", row.names = FALSE)

library(dplyr)
library(tidyr)
load("./LARGEDF.Rdata")

# nombre de communes où le FN fait plus, en inscrits, en 2015 qu'en 2012
LARGEDF %>%
  filter(election %in% c("Pre12", "Can15")) %>%
  select(CodeInsee, election, FN) %>%
  spread(election, FN) %>%
  filter(Can15 >= Pre12) %>%
  tally()

library(tmap)

load("./communesOSM.Rdata")

tmp <- LARGEDF %>%
  select(CodeInsee, election, FN) %>%
  spread(election, FN) %>%
  as.data.frame
library(bigpca)
acp <- big.PCA(as.matrix(tmp[,-1]), pcs.to.keep = 5, center = FALSE, save.pcs = TRUE, verbose = TRUE, use.bigalgebra = TRUE)
tmp$acp <- acp$PCs[,1]

moyennes <- LARGEDF %>%
  group_by(CodeInsee) %>%
  summarise(moyenne = mu.FN + mean(FN, na.rm = TRUE))

library(lme4)
model <- lmer(FN ~ 1 + (1 | CodeInsee) + (1 | election), data = LARGEDF)
pred <- predict(model, re.form = ~(1 | CodeInsee))
moyennes2 <- data.frame(
  CodeInsee = LARGEDF[LARGEDF$election %in% "Eur14", "CodeInsee"], 
  multiniveau = pred[LARGEDF$election %in% "Eur14"]
)
moyennes$multiniveau <- moyennes2[match(moyennes$CodeInsee, moyennes2$CodeInsee), "multiniveau"] + mu.FN
moyennes[which(moyennes$multiniveau < 0), "multiniveau"] <- 0

moyennes$acp <- tmp[match(moyennes$CodeInsee, tmp$CodeInsee), "acp"]

library(maptools)
france <- unionSpatialPolygons(communes, IDs = substr(communes@data$insee, 1, 2))

png("./figure3.png", width = 600, height = 800)
pdf("./figure3.pdf", width = 8, height = 10)
print(
  communes[substr(communes@data$insee, 1, 2) %in% c(sprintf("%02.0f", 1:95), "2A", "2B"), ] %>% 
    append_data(moyennes, "insee", "CodeInsee") %>%
    tm_shape() +
      tm_fill(col = "multiniveau", palette = "Greys", style = "quantile", n = 6, textNA = NA, text_separator = "à", title = "Ordonnée à l'origine\npour chaque commune d'un modèle multiniveau") +
    tm_layout(title = "Structure du vote FN entre 1992 et 2015", bg.color = "white", scale = 1.1, asp = 0) +
    tm_shape(france) +
      tm_borders()
)
dev.off()
dev.off()

write.csv(moyennes[,c("CodeInsee", "multiniveau")], file = "figure3.csv", row.names = FALSE)

model2 <- lmer(FN ~ 1 + CS1 + CS2 + CS3 + CS4 + CS5 + CS6 + CS7 + CS8 + type + (1 | CodeInsee) + (1 | election), data = LARGEDF)
newdata <- expand.grid(CodeInsee = unique(LARGEDF$CodeInsee), election = unique(LARGEDF$election))
newdata$type <- LARGEDF[match(newdata$election, LARGEDF$election), "type"]
tmp <- LARGEDF[LARGEDF$election %in% "Leg93", c("CodeInsee", paste0("CS", 1:8))]
newdata[, paste0("CS", 1:8)] <- tmp[match(newdata$CodeInsee, tmp$CodeInsee), paste0("CS", 1:8)]
newdata$predict <- predict(model2, newdata)
newdata$temps <- LARGEDF[match(newdata$election, LARGEDF$election), "temps"]

tmp <- LARGEDF %>%
  filter(temps %in% c(3:7, 22:25), 
         !(election %in% "Can94")) %>%
  mutate(t = ifelse(temps < 10, "avant", "après")) %>%
  group_by(CodeInsee, t) %>%
  summarise(FN = mean(FN)) %>%
  spread(t, FN) %>%
  mutate(diff = après - avant) %>%
  append_data(communes[substr(communes@data$insee, 1, 2) %in% c(sprintf("%02.0f", 1:95), "2A", "2B"), ], 
              ., 
              "insee",
              "CodeInsee")
quantiles <- round(quantile(tmp@data$diff, c(0, 0.166, 0.333, 0.5, 0.667, 0.833, 1), na.rm = TRUE), 2)


png("./figure4.png", width = 600, height = 800)
pdf("./figure4.pdf", width = 8, height = 10)
print(
  LARGEDF %>%
    filter(temps %in% c(3:7, 22:25), 
           !(election %in% "Can94")) %>%
    mutate(t = ifelse(temps < 10, "avant", "après")) %>%
    group_by(CodeInsee, t) %>%
    summarise(FN = mean(FN)) %>%
    spread(t, FN) %>%
    mutate(diff = après - avant) %>%
    append_data(communes[substr(communes@data$insee, 1, 2) %in% c(sprintf("%02.0f", 1:95), "2A", "2B"), ], 
                ., 
                "insee",
                "CodeInsee") %>%
    tm_shape() +
      tm_fill(col = "diff", palette = "-RdBu", style = "fixed", breaks = quantiles, textNA = NA, text_separator = "à", auto.palette.mapping = FALSE) +
    tm_layout(title = "Différence entre le vote moyen (aux inscrits) sur la période 1993-1997\net sur la période 2012-2015", bg.color = "white", asp = 0) +
    tm_shape(france) +
    tm_borders()
)
dev.off()
dev.off()

png("./figure4nb.png", width = 600, height = 800)
pdf("./figure4nb.pdf", width = 8, height = 10)
print(
  LARGEDF %>%
    filter(temps %in% c(3:7, 22:25), 
           !(election %in% "Can94")) %>%
    mutate(t = ifelse(temps < 10, "avant", "après")) %>%
    group_by(CodeInsee, t) %>%
    summarise(FN = mean(FN)) %>%
    spread(t, FN) %>%
    mutate(diff = après - avant) %>%
    append_data(communes[substr(communes@data$insee, 1, 2) %in% c(sprintf("%02.0f", 1:95), "2A", "2B"), ], 
                ., 
                "insee",
                "CodeInsee") %>%
    tm_shape() +
    tm_fill(col = "diff", palette = "Greys", style = "fixed", breaks = quantiles, textNA = NA, text_separator = "à", auto.palette.mapping = FALSE, title = "Différence entre le vote moyen (aux inscrits) sur la période 1993-1997\net sur la période 2012-2015") +
    tm_layout(bg.color = "white", asp = 0) +
    tm_shape(france) +
    tm_borders()
)
dev.off()
dev.off()



LARGEDF %>%
  filter(temps %in% c(3:7, 22:25), 
         !(election %in% "Can94")) %>%
  mutate(t = ifelse(temps < 10, "avant", "après")) %>%
  group_by(CodeInsee, t) %>%
  summarise(FN = mean(FN)) %>%
  spread(t, FN) %>%
  mutate(diff = après - avant) %>%
  select(CodeInsee, diff) %>%
  write.csv(file = "figure4.csv", row.names = FALSE)

# png("./figure5.png", width = 600, height = 800)
# pdf("./figure5.pdf", width = 8, height = 10)
# print(
#   newdata %>%
#     filter(temps %in% c(3:7, 22:25), 
#            !(election %in% "Can94")) %>%
#     mutate(t = ifelse(temps < 10, "avant", "après")) %>%
#     group_by(CodeInsee, t) %>%
#     summarise(FN = mean(predict)) %>%
#     spread(t, FN) %>%
#     mutate(diff = après - avant) %>%
#     append_data(communes[substr(communes@data$insee, 1, 2) %in% c(sprintf("%02.0f", 1:95), "2A", "2B"), ], 
#                 ., 
#                 "insee",
#                 "CodeInsee") %>%
#     tm_shape() +
#     tm_fill(col = "diff", palette = "RdBu", style = "quantile", n = 6, textNA = NA, text_separator = "à", auto.palette.mapping = FALSE) +
#     tm_layout(title = "Différence entre le vote moyen sur la période 1993-1997\net sur la période 2012-2015", bg.color = "white", asp = 0) +
#     tm_shape(france) +
#     tm_borders()
# )
# dev.off()
# dev.off()

tmp <- LARGEDF %>%
  filter(temps %in% c(3:7, 22:25), 
         !(election %in% "Can94")) %>%
  mutate(t = ifelse(temps < 10, "avant", "après")) %>%
  group_by(CodeInsee, t) %>%
  summarise(FN = mean(FN)) %>%
  spread(t, FN) %>%
  mutate(diff = après - avant)

tmp2 <- LARGEDF %>%
          filter(election %in% "Pre12")

tmp$CS6 <- tmp2[match(tmp$CodeInsee, tmp2$CodeInsee), "CS6"]
tmp$CS3 <- tmp2[match(tmp$CodeInsee, tmp2$CodeInsee), "CS3"]
cor(tmp[, c("diff", "CS3", "CS6")], use = "complete.obs")

religion <- read.csv("./Religion en 2012 par départements.csv", sep = ";", dec = ",")
religion <- religion %>%
  mutate(CodeDepartement = sprintf("%02.0f", Departement))

religion2 <- read.csv("./religion.csv", sep = ";", na.strings = c("//", "||"))
religion2 <- religion2 %>%
  mutate(CodeDpt = ifelse(nchar(as.character(DEP)) < 2, paste0("0", as.character(DEP)), as.character(DEP)))
religion2 <- religion2 %>%
  group_by(CodeDpt) %>%
  summarise(catho = weighted.mean(Messalisants, w = Catholiques, na.rm = TRUE))

LARGEDF$catho <- religion[match(LARGEDF$CodeDpt, religion$CodeDepartement), "Catholique"]
LARGEDF$catho2 <- as.data.frame(religion2)[match(LARGEDF$CodeDpt, religion2$CodeDpt), "catho"]
LARGEDF$autre <- religion[match(LARGEDF$CodeDpt, religion$CodeDepartement), "Autres"]

tmp <- LARGEDF %>%
  filter(temps %in% c(3:7, 22:25), 
         !(election %in% "Can94")) %>%
  mutate(t = ifelse(temps < 10, "avant", "après")) %>%
  group_by(CodeDpt, t) %>%
  summarise(FN = weighted.mean(FN, w = population), catho = mean(catho), catho2 = mean(catho2), autres = mean(autre)) %>%
  spread(t, FN) %>%
  mutate(diff = après - avant)

cor(tmp[, c("diff", "catho", "catho2", "autres")], use = "pairwise.complete.obs")

load("./communes_ident.Rdata")

LARGEDF$AU <- communes_ident[match(LARGEDF$CodeInsee, communes_ident$CodeInsee), "CodeAU10"]

load("./Gini11AUR_DU.Rdata")

LARGEDF$Gini2011 <- Gini11AUR_DU[match(LARGEDF$AU, Gini11AUR_DU$AU2010), "RFUCGI11"]

tmp <- LARGEDF %>%
  filter(temps %in% c(3:7, 22:25), 
         !(election %in% "Can94")) %>%
  mutate(t = ifelse(temps < 10, "avant", "après")) %>%
  group_by(AU, t) %>%
  summarise(FN = weighted.mean(FN, w = population), Gini = mean(Gini2011)) %>%
  spread(t, FN) %>%
  mutate(diff = après - avant)

cor(tmp[, -1], use = "pairwise")

load("./RDL_INEGALITES_DEP11.Rdata")
LARGEDF$GRD <- RDL_INEGALITES_DEP11[match(LARGEDF$CodeDpt, RDL_INEGALITES_DEP11$DEP), "GRD"]

tmp <- LARGEDF %>%
  filter(temps %in% c(3:7, 22:25), 
         !(election %in% "Can94")) %>%
  mutate(t = ifelse(temps < 10, "avant", "après")) %>%
  group_by(CodeDpt, t) %>%
  summarise(FN = weighted.mean(FN, w = population), catho = mean(catho), catho2 = mean(catho2), autres = mean(autre), GRD = mean(GRD)) %>%
  spread(t, FN) %>%
  mutate(diff = après - avant)


LARGEDF %>%
  filter(election %in% "Pre12") %>%
  group_by(AU) %>%
  summarise(FN = weighted.mean(FN / 100, w = population), Gini = mean(Gini2011)) %>%
  mutate(FN = ifelse(FN <= 0, 0.001, FN)) %>%
  ggplot(aes(x = Gini, y = FN)) +
  geom_point() +
  geom_smooth()

library(grid)


load("./carto.communesPLM.Rdata")

png("./figure5.png", width = 600, height = 800)
pdf("./figure5.pdf", width = 8, height = 10)
print(
  LARGEDF %>%
    filter(temps %in% c(3:7, 22:25), 
           !(election %in% "Can94")) %>%
    mutate(t = ifelse(temps < 10, "avant", "après")) %>%
    group_by(CodeInsee, t) %>%
    summarise(FN = mean(FN)) %>%
    spread(t, FN) %>%
    mutate(diff = après - avant) %>%
    append_data(carto.communesPLM[substr(carto.communesPLM@data$INSEE_COM, 1, 2) %in% c(sprintf("%02.0f", 1:95), "2A", "2B"), ], 
                ., 
                "INSEE_COM",
                "CodeInsee") %>%
    tm_shape() +
    tm_fill(col = "diff", palette = "-RdBu", style = "fixed", breaks = quantiles, textNA = NA, text_separator = "à", auto.palette.mapping = FALSE) +
    tm_layout(title = "Différence entre le vote moyen (aux inscrits) sur la période 1993-1997\net sur la période 2012-2015", bg.color = "white", asp = 0)
)
dev.off()
dev.off()

png("./figure6.png", width = 600, height = 800)
pdf("./figure6.pdf", width = 8, height = 10)
LARGEDF %>%
  group_by(ZAUER99, election) %>%
  summarise(FN = weighted.mean(FN, w = population) + mu.FN) %>%
  ungroup() %>%
  mutate(ZAUER99 = factor(ZAUER99, levels = unique(LARGEDF$ZAUER99)[c(4, 1, 5, 3, 6, 2)])) %>%
  ggplot(aes(x = ZAUER99, y = FN)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ election) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), plot.margin = unit(c(0.5, 0.5, 0, 1.5), "cm")) +
  xlab("") +
  ylab("en % des inscrits")
dev.off()
dev.off()

tmp <- LARGEDF %>%
  group_by(ZAUER99, election) %>%
  summarise(FN = weighted.mean(FN, w = population) + mu.FN) %>%
  ungroup() %>%
  mutate(ZAUER99 = factor(ZAUER99, levels = unique(LARGEDF$ZAUER99)[c(4, 1, 5, 3, 6, 2)]))
write.csv(tmp, file = "figure6.csv")

model3 <- lmer(FN ~ 1 + ZAUER99 + (1 | CodeInsee) + (1 | election), data = LARGEDF)
model4 <- lmer(FN ~ 1 + CS1 + CS2 + CS3 + CS4 + CS5 + CS6 + CS7 + CS8 + ZAUER99 + (1 | CodeInsee) + (1 | election), data = LARGEDF)
model5 <- lmer(FN ~ 1 + CS1 + CS2 + CS3 + CS4 + CS5 + CS6 + CS7 + CS8 + ZAUER99*temps + (1 | CodeInsee) + (1 | election), data = LARGEDF)
model6 <- lmer(FN ~ 1 + CS1*temps + CS2*temps + CS3*temps + CS4*temps + CS5*temps + CS6*temps + CS7*temps + CS8*temps + ZAUER99*temps + (1 | CodeInsee) + (1 | election), data = LARGEDF)

LARGEDF %>%
  group_by(election, ZAUER99) %>%
  summarise(FN = sum(FN))