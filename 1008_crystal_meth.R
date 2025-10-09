library(dplyr)
library(ggplot2)
library(waffle)

drugs_df <- readr::read_csv("data/Drug_Consumption.csv")

str(drugs_df)
summary(drugs_df)
drugs <- colnames(drugs_df)[14:32]

factor_answer <- function(x) {
  factor(x = x, 
         levels = c("CL0",  "CL1",  "CL2",  "CL3",  "CL4",  "CL5",  "CL6" ), 
         labels = c("Never Used", "Used over a Decade Ago", "Used in Last Decade",
                    "Used in Last Year", "Used in Last Month", "Used in Last Week", "Used in Last Day"),
         ordered = TRUE)
}

drugs_df[colnames(drugs_df) %in% drugs] <- lapply(drugs_df[colnames(drugs_df) %in% drugs], factor_answer)


drugs_df[2:6] <- lapply(drugs_df[2:6] , as.factor)
drugs_df$Education <- factor(drugs_df$Education, 
                                levels = c("Left school before 16 years",
                                           "Left school at 16 years",
                                           "Left school at 17 years",
                                           "Left school at 18 years",
                                           "Some college or university - no certificate or degree",
                                           "Professional certificate/ diploma",
                                           "University degree",
                                           "Masters degree",
                                           "Doctorate degree" ),
                             labels = c("Left school",
                                        "Left school",
                                        "Left school",
                                        "Left school",
                                        "Some college or university - no certificate or degree",
                                        "Professional certificate/ diploma",
                                        "University degree",
                                        "University degree",
                                        "Doctorate degree" ),
                                ordered = TRUE)


str(drugs_df)
summary(drugs_df)


meth_age <- table(drugs_df$Age, drugs_df$Meth)
chisq.test(meth_age)

meth_sex <- table(drugs_df$Gender, drugs_df$Meth)
chisq.test(meth_sex)

meth_education <- table(drugs_df$Gender, drugs_df$Education)
chisq.test(meth_sex)

drugs_df %>% 
  group_by(Age, Meth) %>% 
  summarise(nb = n()) %>% 
ggplot(aes(fill=Meth, values=nb)) +
  geom_waffle(n_rows = 10,
              size = 0.33, 
              colour = "white") +
  facet_wrap(~Age, ncol=1) +
  coord_equal() +
  theme_void() 



# stackbarplot age x meth -------------------------------------------------

drugs_df %>% 
  group_by(Age, Meth) %>% 
  summarise(nb = n()) %>% 
ggplot(aes(fill=(Meth), y=nb, x=Age)) + 
  geom_bar(position="fill", stat="identity", color = "white") +
  geom_hline(yintercept = c(0.25, 0.5), colour = "white", linetype = "dashed") +
  scale_y_continuous(expand = c(0,0), breaks = c(0.25, 0.5), minor_breaks = NULL, labels = c("25%", "50%")) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  labs(title = "Crystal Meth Consumption by Age",
       caption = "The Five Factor Model of personality and evaluation of drug consumption risk") +
  theme(plot.background   = element_rect(fill = "#084594"),
        plot.title.position = "plot",
        text =  element_text(colour = "white"),
        plot.caption.position = "plot",
        panel.background  = element_rect(fill = "#084594"),
        legend.background = element_rect(fill = "#084594"),
        axis.title = element_blank(),
        axis.text = element_text(colour = "white"))



