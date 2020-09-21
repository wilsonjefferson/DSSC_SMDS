library("magrittr")
library("ggplot2")
library("ggpubr")

# Dataframe
Owners <- c("Katy Perry", "Justin Bieber", "Taylor Swift", "Cristiano Ronaldo",
             "Kim Kardashian", "Ariana Grande", "Selena Gomez", "Demi Lovato")
Instagram <- c(69, 98, 107, 123, 110, 118, 135, 67)
Twitter <- c(109, 106, 86, 72, 59, 57, 56, 56)

# Plot
plot(Instagram, Twitter, pch=21, bg=2, xlim=c(60, 150), ylim=c(40, 120))
text(Instagram[-6], Twitter[-6]+5, Owners[-6], cex=0.8)
text(Instagram[6], Twitter[6]-5, Owners[6], cex=0.8)

# Create a more suitable dataframe
all <- data.frame(Owners = Owners, Instagram = Instagram, Twitter = Twitter)

# Plot correlation using ggpubr library
ggpubr::ggscatter(all, x = 'Instagram', y = 'Twitter',
          color = 'red',                                          # for the points
          add = "reg.line",
          add.params = list(color = "blue", fill = "lightgray"),  # for the line
          conf.int = TRUE,
          cor.coef = TRUE,
          cor.method = "pearson",
          xlab = "Instagram", ylab = "Twitter",
          xlim=c(60, 150), ylim=c(40, 120))

# Pearson test
pearson <- cor.test(Instagram, Twitter,
                    method = "pearson")

# Print the result
pearson
