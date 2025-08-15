library(tidyverse)
library(quantreg)
library(regressinator)
library(splines)
library(ggeffects)
library(broom)
library(lme4)
library(maps)

weather <- read_csv("chicago/weather.csv")
qual <- read_csv("chicago/qual_times.csv")
chicago <- read_csv("chicago/results.csv") %>%
  group_by(Year, Gender) %>%
  arrange(Finish, .by_group = T) %>%
  mutate(Gender_place = row_number(Finish)) %>%
  ungroup() %>%
  mutate(international = case_when(Country != "USA" ~ 1, TRUE ~0), 
         super_era = case_when(Year >= 2019 ~ "Yes", TRUE ~ "No"),
         super_era_elite = case_when(Year >= 2017 ~ "Yes", TRUE ~ "No"),
         time_min = Finish/60, 
         id = row_number(), 
         Age = as.factor(Age)) %>%
  left_join(weather %>% select(Year, temp9 = `9AM Temp`, dew9= `9AM Dew`, wind9 = `9AM Wind`), by = "Year")

# remove times over 6:30 finishing mark
chicago <- chicago %>% 
  filter(Finish <= 23400)

# getting the elites  -------------------
dups <- chicago %>% filter((Name == "Ryan Hall" & Year == 2011) | (Name == "Jesse Davis" & Year <=2014) |(Name == "Michael Eaton" & Year == 2019) |
                     (Name == "Carlos Trujillo" & Year %in% c(2012, 2014)) | (Name == "Michael Shelley" & Year %in% c(2013, 2017)) | 
                     (Name == "Sean Quigley" & Overall < 100) | (Name == "Mike Morgan" & Overall < 100) | 
                     (Name == "Amy Hastings" & Year == 2014) | (Name == "Melissa White" & Year %in% c(2013, 2014)) |
                     (Name == "Sara Hall" & Year %in% c(2015, 2021)) | (Name == "Diego Estrada" & Country == "USA")|
                     (Name == "Tom Anderson" & Year == 2016) | (Name == "Caitlin Chrisman" & Year %in% c(2016, 2017)) | 
                     (Name == "Aaron Braun" & Year %in% c(2017, 2018)) | (Name == "Scott Smith" & Year == 2019) | 
                     (Name == "Lindsey Anderson" & Year %in% c(2018, 2019)) | (Name == "Matt McDonald" & Overall < 100)|
                     (Name == "Paul Hogan" & Year == 2022) | (Name == "Oscar Medina" & Overall < 100) |
                     (Name == "Hiroaki Sano" & Year == 2013) | (Name == "Rob Watson" & Overall < 100))

elite <- chicago %>%
  filter(Name %in% c("Evans Cheruiyot", "Dickson Chumba", "Bekana Daba", "Bernard Kipyego", "Wesley Korir", 
                     "Moses Mosop", "Takayuki Matsumiya", "Takashi Horiguchi", "Kouji Gokaya", "Joseph Chirlee",
                     "Hironori Arai", "Masaki Shimoju", "Yuuki Moriwaki", "Tyler McCandless",  "Michael McKeeman", 
                     "Mario Macias", "Shawn Forrest", "Levy Matebo", "Tsegaye Kebede", "Feyisa Lilesa", "Dadi Yami",
                     "Shami Dawit", "Laban Korir", "Michael Kipyego", "Samuel Ndungu", "Sammy Ndungu", "Dathan Ritzenhein",
                     "Takeshi Kumamoto", "Yuki Moriwaki", "Hiroki Kadota", "Jimmy Grabow", "Naoki Okamoto",
                     "Koji Kobayashi", "Jeffrey Eggleston", "Patrick Rizzo", "Drew Polley", "Patrick Smyth",
                     "Craig Leon", "Malcolm Richards", "Tim Tollefson", "Stephen Muturi", "Thomas Frazer",
                     "Sammy Kitwara", "Tilahun Regassa", "Stephen Pifer", "Dennis Kimetto", "Ayele Abshero",
                     "Emannuel Mutai", "Yoshinori Oda", "Zersenay Tadese", "Michael Shelley", "Kenji Higashino",
                     "Yoshiki Otsuka", "Hiroki Tanaka", "Norihide Fujimori", "Matt Dewald",
                     "Christopher Pannone", "Eric Loeffler", "Chris Siemers", "Brandon Mull", "Dan Kremske",
                     "Matt Tegenkamp", "Eliud Kipchoge", "Bernard Koech", "Kenenisa Bekele", "Satoru Sasaki",
                     "Ryosuke Fukuyama", "Rui Yonezawa", "Bobby Curtis", "Luke Humphrey", "Christo Landry",
                     "Tim Young", "Jameson Mora", "Gabe Proctor", "Bayron Piedra", "Bayron Piedra Aviles", 
                     "Matt Llano", "Brendan Gregg", "Jake Riley", "Lucas Rotich", "Abera Kuma", "Satoshi Yoshii", 
                     "Liam Adams", "Fernando Cabada", "Abel Kirui", "Micah Kogo", "Paul Lonyangata",
                     "Gideon Kipketer", "Koji Gokaya", "Takuya Fukatsu", "Luke Puskedra", "Elkanah Kibet", "Kazuya Ishida",
                     "Ryoichi Matsuo", "Tim Young", "David Nilsson", "Jose Madera", "Tony Migliozzi", "Jonathan Mott", 
                     "Kevin Havel", "Dustin Emerick", "Andrew Epperson", "Stephen Sambu", "Kiya Dandena", "Daniel Wallis",
                     "Kohei Matsumura", "Galen Rupp", "Chihiro Miyawaki", "Andrew Bumbalough", "Sam Chelanga",
                     "Chris Derrick", "Noah Droddy", "Mosinet Geremew Bayih", "Birhanu Legese", "Kenneth Kipkemoi",
                     "Mo Farah", "Geoffrey Kirui", "Suguru Osako", "Bedan Karoki", "Yuki Kawauchi", "Mohamed Reda El Aaraby",
                     "Tyler McCandless", "Kiya Dandena", "Yohei Suzuki", "Taku Fujimoto", "Johnny Crain", "Jonas Hampton",
                     "Pardon Ndhlovu", "Andrew Epperson", "Lawrence Cherono", "Minato Oishi", "Ryoma Takeuchi", 
                     "Tsubasa Hayakawa", "Chris Derrick", "Jacob Riley", "Brendan Gregg", "Brian Shrader", "Nico Montanez",
                     "Parker Stinson", "Ryan Root", "Alan Peterson", "Jackson Neff", "Dan Kremske", "Reed Fischer",
                     "Jerrell Mock", "Seifu Tura", "Seifu Tura Abdiwak", "Shifera Tamru Aredo", "Eric Kiptanui",
                     "Eric Kiprono Kiptanui", "Colin Mickow", "Wilkerson Given", "Chris Derrick", "Josh Izewski",
                     "Mike Sayenko", "Nico Montanez", "Alan Peterson", "Dan Kremske", "Brett Lustgarten", "Kurt Roeser",
                     "Clayton Young", "Abdisamed Abdi", "Ben Kendell", "Zachery Panning", "Dawit Wolde", "Stephen Kissa", 
                     "Benson Kipruto", "Kyohei Hosoya", "Hamza Sahli", "Hiroto Fujimagari", "Kiyoshi Koga", "Riki Nakanishi",
                     "John Korir", "Frank Lara", "Reid Buchanan", "Turner Wiley", "Chase Weaverling", "John Dressel", 
                     "Jp Flavin", "Conner Mantz", "Kelvin Kiptum", "Bashir Abdi", "Yuki Matsumura", "Takashi Ichida", 
                     "Masashi Nonaka", "Masaki Tsuda", "Mizuki Higashi", "Andrew Colley", "Kevin Salvano", "Joel Reichow",
                     "Brian Shrader", "Wesley Kiptoo", "Connor Winter", "Alec Sandusky",
                     "Askale Tafa", "Christelle Daunay", "Belainesh Zemedkun Gebre", "CHallissey", "Jeannette Faber", 
                     "Leah Thorvilson", "Kayoko Fukushi", "Ejegayehu Dibaba", "Malika Mejdoub", "Cruz Nonata da Silvab",
                     "Lucy Kabuu", "Atsede Baysa", "Rita Jeptoo", "Caroline Rotich", "Fatuma Sado", "Dot McMahan", "Laurie Knowles",
                     "Deanna Ardrey", "Addie Bracy", "Stephanie Pezzullo", "Jemima Sumgong Jelegat", "Ehitu Kiros Reda",
                     "Abebech Afework", "Yukiko Akaba", "Clara Santucci", "Amanda Rice", "Gabriela Trana", "Gina Slaby",
                     "Florence Kiplagat", "Mare Dibaba", "Birhane Dibaba", "Gelete Burka", "Wendy Thomas", "Laura Portis", "Sarah Cummings",
                     "Lauren Jimison", "Yuri Yoshizumi", "Whitney Bevins-Lazzara", "Heidi Greenwood",
                     "Sarah Crouch", "Lindsey Scherf", "Lisa Uhl", "Mulu Seboka", "Fionnuala Britton", "Susan Partridge", "Diane Nukuri",
                     "Jessica Draskau Petersson", "Deena Kastor", "Tera Moody", "Valentine Kipketer", "Yebrgual Melese",
                     "Purity Rionoripo", "Meskerem Assefa", "Serena Burla", "Freya Ross", "Agnieszka Mierzejewska", "Sarah Cummings",
                     "Alia Gray", "Julia Roman-Duval", "Rachel Hyland", "Emma Polley", "Tirunesh Dibaba",
                     "Madai Perez", "Jordan Hasay", "Lisa Weightman", "Becky Wade", "Michelle Lilienthal", "Danna Herrick", "Kristen Heckert",
                     "Roza Dereje", "Brigid Kosgei", "Shure Demise", "Laura Thweatt",
                     "Alyson Dixon", "Veronicah Nyaruai", "Taylor Ward", "Katie Matthews", "Melanie Myrand", "Gwen Jorgensen", 
                     "Alexi Pappas", "Betsy Saina", "Fionnuala McCormack", "Anke Esser", "Emma Bates", "Stephanie Bruce", "Lindsay Flanagan",
                     "Taylor Ward", "Maegan Krifchin", "Christina Vergara-Aleshire", "Sarah Sellers", "Alyssa Schneider",
                     "Ruth Chepngetich", "Keira D'Amato", "Bria Wetsch", "Daiana Ocampo", "Jane Bareikis", "Georgia Porter", 
                     "Anne-Marie Blaney", "Megan O'Neil", "Olivia Pratt", "Carrie Verdon", "Jessica Watychowicz", "Vivian Jerono Kiplagat",
                     "Emily Sisson", "Sara Vaughn", "Susanna Sullivan", "Krista DuChene", "Maggie Montoya", "Rachel Hannah",
                     "Brittney Feivor", "Meriah Earle", "Marie-Ange Brumelot", "Jessie Cardin", "Makena Morley",
                     "Tadu Teshome Nare", "Joyciline Jepkosgei", "Genzebe Dibaba Keneni", "Sutume Asefa Kebede", "Sifan Hassan",
                     "Desiree Linden", "Molly Seidel", "Dakotah Lindwurm", "Tristin Van Ord", "Gabriella Rooker", "Dominique Scott",
                     "Annmarie Tuxbury", "Stacy Ndiwa", "Lindsey Scherf", "Ghirmay Ghebreslassie", "Lani Rutto", "Girmay Birhanu Gebru", 
                     "Edna Kiplagat", "Abayneh Ayele", "Sisay Lemma", "Ezekiel Chebii", "Kate Landau", "Marci Klimek", "Ababel Yeshaneh", 
                     "Fionnuala Mccormack", "Dejene Debela", "Asefa Mengstu", "Sarah Pagano", "Meseret Belete Tola", 
                     "Kengo Suzuki", "Reuben Kiprop Kipyego", "Ruti Aga Sora", "Waganesh Mekasha Amare", "Sarah Inglis", 
                     "Dong Guojian", "Megertu Alemu", "Rose Harvey", "Pat Tiernan", "Matthew Mcdonald", "Nicolas Montanez", 
                     "Abayneh Degu Tsehay", "Amanuel Mesel Tikue")) %>%
  mutate(Name = case_when(Name == "Yuki Moriwaki" ~ "Yuuki Moriwaki", 
                          Name == "Sammy Ndungu" ~ "Samuel Ndungu",
                          Name == "Seifu Tura" ~ "Seifu Tura Abdiwak",
                          Name == "Bayron Piedra Aviles" ~ "Bayron Piedra", 
                          Name == "Eric Kiprono Kiptanui" ~ "Eric Kiptanui", 
                          Name == "Matthew Mcdonald" ~ "Matt McDonald",
                          TRUE ~ Name)) %>%
  rbind(dups) %>%
  filter(Year >= 2011, Age %in% c("24", "29", "34", "39")) %>% # remove 19, 44, 49 (10 total)
  distinct_all()

# eda --------------------
# make a map showing where runners come from!

chicago %>%
  filter(elite == 1) %>%
  group_by(Year, Gender) %>%
  summarize(avg_time = mean(Finish)/60, se_time = sd(Finish)/60, .groups = "drop") %>%
  ggplot(aes(x = Year, y = avg_time, color = Gender, fill = Gender)) +
  geom_line(size = 1) +
  geom_point()+
  geom_ribbon(aes(ymin = avg_time - se_time, ymax = avg_time + se_time), alpha = 0.2, color = NA) +
  labs(title = "Elite Marathon Finish Times by Year and Gender",
       y = "Average Finish Time (min)",
       x = "Year") +
  theme_minimal()


# talk about qualifying times decreasing in 2024 and 2025
# participation down in 2021 and 2022
# https://www.runnersworld.com/races-places/a42200126/chicago-marathon-field-2023/
# https://www.nytimes.com/athletic/5834953/2024/10/12/shoe-doping-marathon-times-kipchoge/
# https://www.runnersworld.com/gear/a42723316/super-shoes-performance-effect/
chicago %>%
  group_by(Year) %>%
  count() %>%
  ggplot(aes(x = Year, y = n)) +
  geom_line() +
  geom_point()+
  theme_minimal()

# proportion of international participants down in 2021
chicago %>%
  group_by(Year, international) %>%
  count() %>%
  group_by(Year) %>%
  mutate(prop = n/sum(n)) %>%
  filter(international == 1) %>%
  ggplot(aes(x = Year, y = prop)) +
  geom_line() +
  geom_point()+
  theme_minimal()

# supershoes: 2017 for elites
# 2019 for general public 

chicago_sub <- chicago %>% 
  filter(!id %in% elite$id) %>% 
  filter(Year >= 2014)

# Modeling elites -------------------

elite %>% group_by(super_era, Gender) %>% summarize(mean(time_min))
elite %>% count(Name) %>% ggplot(aes(x=n))+geom_bar()

model <- lmer(time_min ~ super_era_elite + Age + Gender + temp9 +
                (1 | Name), data = elite, REML=F)
model2 <- lmer(time_min ~ Age + Gender + temp9 +
                (1 | Name), data = elite, REML=F)
summary(model)
confint(model)
anova(model2, model)

newdata_pre <- data.frame(
  super_era_elite = "No",
  Age = as.factor("24"),
  Gender = "M",  # or adjust as needed
  temp9 = 50
)

newdata_post <- newdata_pre
newdata_post$super_era_elite <- "Yes"

# Predict average finishing time 
pred_pre <- predict(model, newdata = newdata_pre, re.form = NA)
pred_post <- predict(model, newdata = newdata_post, re.form = NA)

resid_sd <- sigma(model)

prob_pre <- pnorm(120, mean = pred_pre, sd = resid_sd)
prob_post <- pnorm(120, mean = pred_post, sd = resid_sd)
prob_post / prob_pre

newdata_pre <- data.frame(
  super_era_elite = "No",
  Age = as.factor("24"),
  Gender = "F",  # or adjust as needed
  temp9 = 50
)

# for fastest runner pre-super shoe era
runner_effect <- ranef(model)$Name["Dennis Kimetto", "(Intercept)"]

dennis <- elite[elite$Name == "Dennis Kimetto", ]

new_pre <- data.frame(
  super_era_elite = "No",
  Age = dennis$Age,
  Gender = "M",
  temp9 = dennis$temp9
)

new_post <- new_pre
new_post$super_era_elite <- "Yes"

pred_pre_fixed <- predict(model, newdata = new_pre, re.form = NA)
pred_post_fixed <- predict(model, newdata = new_post, re.form = NA)

pred_pre <- pred_pre_fixed + runner_effect
pred_post <- pred_post_fixed + runner_effect

resid_sd <- sigma(model)

prob_pre <- pnorm(120, mean = pred_pre, sd = resid_sd)
prob_post <- pnorm(120, mean = pred_post, sd = resid_sd)
prob_post/prob_pre # The probability of Dennis breaking 2:00 increased 4.74-fold in the super shoe era, 
# based on his historical performance trend—though the overall chance remains small.
  
  
# Modeling everydays ----------------

# Issue: selection bias or compositional shift: 
# If more slower runners joined in the super shoe era (because marathons became more popular)
# the average time could go up, even if individuals with the same age, sex, etc. got faster due to shoes.
# we are correctly adjusting for age, sex, and temperature, but not for unobserved changes in the composition of runners.

chicago_sub %>%
  mutate(sub4 = time_min <= 240) %>%
  group_by(Year) %>%
  summarize(prop_sub4 = mean(sub4))


# quantile regression: models the effect of predictors on a specific percentile 
# of finishing time, not just the mean. Doesn't assume uniform effects across all runners
# more robust to skew or changes in population size
# lets us ask Did runners at a certain performance level get faster in the super shoe era?

rq_median <- rq(time_min ~ super_era + Age + Gender + temp9, tau = 0.5, data = chicago_sub)

summary(rq_median)

rq25 <- rq(time_min ~ super_era + Age + Gender + temp9, tau = 0.25, data = chicago_sub)  
summary(rq25)
rq75 <- rq(time_min ~ super_era + Age + Gender + temp9, tau = 0.75, data = chicago_sub)
summary(rq75)

library(quantreg)

taus <- seq(0.1, 0.9, by = 0.05)  # percentiles
qr_models <- lapply(taus, function(tau) {
  rq(time_min ~ super_era + Age + Gender + temp9, tau = tau, data = chicago_sub)
})
qr_summaries <- lapply(qr_models, function(model) summary(model))
coef_super_shoe <- sapply(qr_summaries, function(s) s$coefficients["super_eraYes", 1])
lower <- sapply(qr_summaries, function(s) {
  est <- s$coefficients["super_eraYes", 1]
  se <- s$coefficients["super_eraYes", 2]
  est - 1.96 * se
})
upper <- sapply(qr_summaries, function(s) {
  est <- s$coefficients["super_eraYes", 1]
  se <- s$coefficients["super_eraYes", 2]
  est + 1.96 * se
})

plot_data <- data.frame(
  quantile = taus,
  coef = coef_super_shoe,
  lower = lower,
  upper = upper
)

qr_summaries[[17]]

write.csv(plot_data, "chicago/everyday_output.csv")

ggplot(plot_data, aes(x = quantile, y = abs(coef))) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = abs(lower), ymax = abs(upper)), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Effect of Super Shoe Era Across Quantiles",
       x = "Quantile of Finish Time",
       y = "Effect on Finish Time (minutes)") +
  theme_minimal()


