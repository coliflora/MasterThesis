########################################################################
###                          Master's Thesis                         ###
###     Deep learning-based methods for individual recognition of    ###
###                        Calotriton Aspter                         ###
########################################################################
require(tidyverse)
require(readxl)
require(MLmetrics)

### Descriptive graphics ###

df <- readxl::read_excel("TritonesCorrespondencias.xlsx", "Todas")
df$Location[grepl("Deu", df$Location)] <- "Torrent Joan Déu"
df %>% group_by(Triton) %>% summarise(NumberImagesPerIndividual=n()) %>%
  group_by(NumberImagesPerIndividual) %>%
  summarise(FrequenciesNImagesPerIndividual=n()) %>%
  ggplot(aes(x=NumberImagesPerIndividual, y=FrequenciesNImagesPerIndividual)) +
  geom_bar(stat="identity", fill="#fc7c0e") +
  geom_text(aes(label=FrequenciesNImagesPerIndividual), vjust=-0.35) +
  theme_bw() +
  ylim(c(0, 80.25)) +
  scale_x_continuous(breaks=c(seq(0, 16, by=2))) +
  xlab("Number of images per individual") +
  ylab("Frequency")
ggsave("BarplotFrequencyOfFrequency.png", height=3, width=7, dpi=600)

df %>% group_by(Location, Year) %>% 
  summarise(NumberImages=n(),
            NumberIndividuals=length(unique(Triton))) %>%
  pivot_longer(NumberImages:NumberIndividuals, names_to="Number", values_to="Values") %>%
  ggplot(aes(x=Values, y=Location, fill=Number)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual("", c("Number of Images", "Number of Individuals"), values=c("#fc7c0e", "#1f74b1")) +
  geom_text(aes(label=Values), hjust=-0.35, position = position_dodge(width = 1)) +
  facet_grid(~Year) +
  xlim(c(0, 380)) +
  theme_bw() +
  xlab("Frequency") +
  theme(axis.title.y=element_blank(),
        legend.position="bottom")
ggsave("BarplotImagesIndividuals.png", height=3.5, width=10, dpi=600)


### Survey Analysis ###

survey <- read_excel("~/Tritones/Survey/Survey_responses.xlsx")
names <- survey[c(1, 2), 7:66]
colnames(survey)[7:66] <- paste0("P", names[1, ], "_", names[2, ])
survey$punc <- as.numeric(gsub(" ^\ ", "", survey$Puntuación))
summary(survey$punc)
survey$time <- as.numeric(gsub("\\(", "", stringr::str_match(survey$`Form Timer ⏱️`, "In time (\\s*(.*?)\\s*min)")[, 3]))
summary(survey$time)
colnames(survey)[6] <- "Expert"
survey$Expert <- as.factor(substr(survey$Expert, 1, 1))
table(survey$Expert)

survey <- survey[-c(1, 2), ]
survey %>% group_by(Expert) %>%
  summarise(Mean_punc=mean(punc),
            Median_punc=median(punc),
            Min_punc=min(punc), 
            Max_punc=max(punc), 
            Mean_time=mean(time),
            Median_time=median(time),
            Min_time=min(time), 
            Max_time=max(time))
actual_values <- as.vector(ifelse(names[2, ]=="Different", 0, 1))
obs_values <- data.frame(ifelse(survey[7:66]=="Different", 0, 1))

F1_per_individual <- apply(obs_values, 1, function(x) F1_Score(actual_values, x))
obs_values_all <- as.vector(obs_values)
actual_values_all <- rep(actual_values, each=nrow(obs_values))

F1_data <- data.frame("Expert"=survey$Expert,
                      "F1"=F1_per_individual)

survey_interest <- survey[, c("Expert", "punc", "time")]
survey_interest$F1 <- F1_per_individual
levels(survey_interest$Expert) <- c("Non-Expert", "Expert")

boxplot_punctuation <- ggplot(survey_interest, aes(y=Expert, x=punc, fill=Expert)) +
  geom_boxplot(alpha=0.65) +
  geom_jitter(aes(color=Expert), alpha=0.75, height=0.1) +
  scale_fill_manual("", values=c("#1f74b1", "#fc7c0e")) +
  scale_color_manual("", values=c("#1f74b1", "#fc7c0e")) +
  theme_bw() +
  xlab("Nº correct answers") +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_text(size=15),
        axis.text=element_text(size=12),
        legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

boxplot_time <- ggplot(survey_interest, aes(y=Expert, x=time, fill=Expert)) +
  geom_boxplot(alpha=0.65) +
  geom_jitter(aes(color=Expert), alpha=0.75, height=0.1) +
  scale_fill_manual("", values=c("#1f74b1", "#fc7c0e")) +
  scale_color_manual("", values=c("#1f74b1", "#fc7c0e")) +
  theme_bw() +
  xlab("Time (minutes)") +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_text(size=15),
        legend.position="none",
        axis.text=element_text(size=12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

boxplot_F1 <- ggplot(survey_interest, aes(y=Expert, x=F1, fill=Expert)) +
  geom_boxplot(alpha=0.65) +
  geom_jitter(aes(color=Expert), alpha=0.75, height=0.1) +
  scale_fill_manual("", values=c("#1f74b1", "#fc7c0e")) +
  scale_color_manual("", values=c("#1f74b1", "#fc7c0e")) +
  theme_bw() +
  xlab("F1 score") +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_text(size=15),
        legend.position="none",
        axis.text=element_text(size=12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggpubr::ggarrange(boxplot_punctuation, boxplot_time, boxplot_F1, ncol=3,
                  widths=c(1.02, 0.8, 0.8))

ggsave("SurveyResults.png", height=3.5, width=10, dpi=600)

# boxplot_punctuation <- ggplot(survey_interest, aes(x=Expert, y=punc, fill=Expert)) +
#   geom_boxplot(alpha=0.65) +
#   geom_jitter(aes(color=Expert), alpha=0.75, width=0.1) +
#   scale_fill_manual("", values=c("#1f74b1", "#fc7c0e")) +
#   scale_color_manual("", values=c("#1f74b1", "#fc7c0e")) +
#   theme_bw() +
#   ylab("Nº correct answers") +
#   theme(axis.title.x=element_blank(),
#         legend.position="none",
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())
# 
# boxplot_time <- ggplot(survey_interest, aes(x=Expert, y=time, fill=Expert)) +
#   geom_boxplot(alpha=0.65) +
#   geom_jitter(aes(color=Expert), alpha=0.75, width=0.1) +
#   scale_fill_manual("", values=c("#1f74b1", "#fc7c0e")) +
#   scale_color_manual("", values=c("#1f74b1", "#fc7c0e")) +
#   theme_bw() +
#   ylab("Time (minutes)") +
#   theme(axis.title.x=element_blank(),
#         legend.position="none",
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())
# 
# boxplot_F1 <- ggplot(survey_interest, aes(x=Expert, y=F1, fill=Expert)) +
#   geom_boxplot(alpha=0.65) +
#   geom_jitter(aes(color=Expert), alpha=0.75, width=0.1) +
#   scale_fill_manual("", values=c("#1f74b1", "#fc7c0e")) +
#   scale_color_manual("", values=c("#1f74b1", "#fc7c0e")) +
#   theme_bw() +
#   ylab("F1 score") +
#   theme(axis.title.x=element_blank(),
#         legend.position="none",
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())
# 
# ggpubr::ggarrange(boxplot_punctuation, boxplot_time, boxplot_F1, ncol=3)


