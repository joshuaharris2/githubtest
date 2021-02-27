library(tidyverse)
library(gapminder)

str(gapminder)

log(mean(gapminder$pop))

gapminder$pop %>% mean() %>% log()

gapminder %>% 
  filter(country == "Oman" &
           year > 1980 &
           year < 2000)

China <- gapminder %>% 
  filter(country == "China")
head(China, 4)


# Example Base R Plot
plot(lifeExp ~ year,
     data = China,
     xlab = "Year",
     ylab = "Life expectancy",
     main = "Life expectancy in China",
     col = "red",
     cex.lab = 1.5,
     cex.main = 1.5,
     pch = 16)


# Simple ggplot will be used
ggplot(data = China,
       aes(x = year, y = lifeExp))+
  geom_point()

# Adding more options to ggplot
ggplot(data = China,
       aes(x = year, y = lifeExp))+
  geom_point(color = "red", size = 3)

# Changing labels
ggplot(data = China,
       aes(x = year, y = lifeExp))+
  geom_point(color = "red", size = 3)+
  xlab("Year")+
  ylab("Life expectancy in China")+
  ggtitle("Life expectancy in China")+
  theme_bw(base_size = 18)

# Plot by countries basic
ggplot(data = gapminder,
       aes(x = year, y = lifeExp))+
  geom_point(color = "red", size = 3)+
  xlab("Year")+
  ylab("Life expectancy")+
  ggtitle("Life expectancy across the globe")+
  theme_bw(base_size = 18)

# Plot by countries more enhanced --but ugly
ggplot(data = gapminder,
       aes(x = year, y = lifeExp,
           group = country))+
  geom_line(color = "red", size = 3)+
  xlab("Year")+
  ylab("Life expectancy")+
  ggtitle("Life expectancy across the globe")+
  theme_bw(base_size = 18)

# Plot by countries more enhanced --getting closer
ggplot(data = gapminder,
       aes(x = year, y = lifeExp,
           group = country))+
  geom_line(color = "red")+
  xlab("Year")+
  ylab("Life expectancy")+
  ggtitle("Life expectancy across the globe")+
  theme_bw(base_size = 18)

# Plot by countries more enhanced --almost there
ggplot(data = gapminder,
       aes(x = year, y = lifeExp,
           group = country,
           color = continent))+
  geom_line()+
  xlab("Year")+
  ylab("Life expectancy")+
  ggtitle("Life expectancy across the globe")+
  theme_bw(base_size = 18)


# Plot by countries more enhanced -- so close now
ggplot(data = gapminder,
       aes(x = year, y = lifeExp,
           group = country,
           color = continent))+
  geom_line()+
  xlab("Year")+
  ylab("Life expectancy")+
  ggtitle("Life expectancy across the globe")+
  theme_bw(base_size = 18)+
  facet_wrap(~ continent)

# Plot by countries more enhanced -- re-position legend
ggplot(data = gapminder,
       aes(x = year, y = lifeExp,
           group = country,
           color = continent))+
  geom_line()+
  xlab("Year")+
  ylab("Life expectancy")+
  ggtitle("Life expectancy across the globe")+
  theme_bw()+
  facet_wrap(~ continent)+
  theme(legend.position = c(0.8, 0.25))

# Plot by countries more enhanced -- remove legend
ggplot(data = gapminder,
       aes(x = year, y = lifeExp,
           group = country,
           color = continent))+
  geom_line()+
  xlab("Year")+
  ylab("Life expectancy")+
  ggtitle("Life expectancy across the globe")+
  theme_bw()+
  facet_wrap(~ continent)+
  theme(legend.position = "none")

# Plot by countries more enhanced -- center title
ggplot(data = gapminder,
       aes(x = year, y = lifeExp,
           group = country,
           color = continent))+
  geom_line()+
  xlab("Year")+
  ylab("Life expectancy")+
  ggtitle("Life expectancy across the globe")+
  theme_bw()+
  facet_wrap(~ continent)+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

# Create a variable for the ggplot
lifeExp_by_year<-
  ggplot(data = gapminder,
       aes(x = year, y = lifeExp,
           group = country,
           color = continent))+
  geom_line()+
  xlab("Year")+
  ylab("Life expectancy")+
  ggtitle("Life expectancy across the globe")+
  theme_bw()+
  facet_wrap(~ continent)+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

# Adding a layer to existing variable ggplot
lifeExp_by_year + 
  theme(legend.position = "bottom")

###### New section of work on ggplot

# Correcting an overplot problem
ggplot(data = gapminder, 
       aes(x = continent, 
           y = year,
           color = continent))+
  geom_point()

# Correcting an overplot problem -- fix with jitter
ggplot(data = gapminder, 
       aes(x = continent, 
           y = year,
           color = continent))+
  geom_point(position = position_jitter(width = 0.5, height = 2))

# Axis changes
ggplot(data = China, aes(x = year, y = gdpPercap))+
  geom_line()+
  scale_y_log10(breaks = c(1000, 2000, 3000, 4000, 5000),
                labels = scales::dollar)+
  xlim(1940, 2010) + 
  ggtitle("Chinese GDP per capita")
  
# Axis changes non-logrithmic scale
gapminder %>% filter(country == "China") %>% 
  ggplot(aes(x = year, y = gdpPercap))+
  geom_line()+
  scale_y_continuous(breaks = c(1000, 2000, 3000, 4000, 5000),
                labels = scales::dollar)+
  xlim(1940, 2010) + 
  ggtitle("Chinese GDP per capita")

# Facet grid example
mtcars %>% 
  ggplot(aes(x = hp, y = mpg))+
  geom_point()+
  facet_grid(gear ~ am)

# Theme change options
ggplot(data = China, aes(x = year, y = lifeExp))+
  geom_line()+
  theme_gray(base_size = 20)

# Legend name and manual colors
lifeExp_by_year +
  theme(legend.position = c(0.8, 0.2))+
  scale_color_manual(
    name = "Which continent are\nwe looking at?", # \n adds a line break
    values = c("Africa" = "seagreen", "Americas" = "turquoise1",
               "Asia" = "royalblue", "Europe" = "violetred1",
               "Oceania" = "yellow"))
 
# Manual Legend example code
ggplot(data = gapminder, aes(x = year, y = lifeExp, group = country))+
  geom_line(alpha = 0.5, aes(color = "Country", size = "Country"))+
  geom_line(stat = "smooth", method = "loess",
            aes(group = continent, color = "Continent", size = "Continent"), alpha = 0.5)+
  facet_wrap(~ continent, nrow = 2)+
  scale_color_manual(name = "Life Exp. for:",
                     values = c("Country" = "black", "Continent" = "blue"))+
  scale_size_manual(name = "Life Exp. for:",
                    values = c("Country" = 0.25, "Continent" = 3))+
  theme_minimal(base_size = 14)+
  ylab("Years")+
  xlab("")+
  ggtitle("Life Expectancy, 1952-2007", subtitle = "By continent and country")+
  theme(legend.position = c(0.82,0.15), axis.text.x = element_text(angle = 45))
