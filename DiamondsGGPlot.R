######################################################
# Exploration of the Diamonds dataset, which has
# close to 53000 rows of data!
######################################################

# We shall use the ggplot2 library to flesh out the
# visualisation of a dataset in stages
library(ggplot2)

# 1. Start with a scatter plot of price vs carat (weight),
# where each point is defaulted to a solid circle
ggplot(diamonds, aes(carat, price)) +
  geom_point()

# 2. Transform both the axes and watch what happens
ggplot(diamonds, aes(carat, price)) +
  geom_point() +
  stat_smooth(method = lm) +
  scale_x_log10() +
  scale_y_log10()

# You could also achieve the same effect with an 
# alternate shorter function form
# qplot(carat, price, data = diamonds,
#     geom = c("point", "smooth"),
#     method = "lm", log = "xy")

# Define a color scheme via a palette
myPalette <- c("blue", "orange", "maroon",
               "darkgreen", "yellow")

# 3. How about a histogram on the prices?
ggplot(diamonds, aes(price, fill=cut)) +
  geom_histogram(binwidth=1000) +
  ggtitle(paste("Price Distribution of",
                nrow(diamonds), "Diamonds\n",
                "Organised by Cut")) +
  scale_fill_manual(values=myPalette)


# 4. Create a boxplot
ggplot(data=diamonds) +
  geom_boxplot(mapping = aes(x=cut, y=depth),color="blue")

boxplot(depth~cut, data=diamonds, col="blue")


