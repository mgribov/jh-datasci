require(stats)
require(graphics) # for mosaicplot

# https://vincentarelbundock.github.io/Rdatasets/doc/datasets/esoph.html
# standard drink ~14g of alc: https://www.rethinkingdrinking.niaaa.nih.gov/How-much-is-too-much/What-counts-as-a-drink/Whats-A-Standard-Drink.aspx
# 1 cigarette ~1g of tobacco: http://stats.oecd.org/fileview2.aspx?IDFile=8e59b835-8196-426e-9297-15bc1dab652c

summary(esoph)


## effects of alcohol, tobacco and interaction, age-adjusted
model1 <- glm(cbind(ncases, ncontrols) ~ agegp + tobgp * alcgp,
              data = esoph, family = binomial())

anova(model1)


## Try a linear effect of alcohol and tobacco
model2 <- glm(cbind(ncases, ncontrols) ~ agegp + unclass(tobgp)
              + unclass(alcgp),
              data = esoph, family = binomial())


summary(model2)

# what if we use the instances as a rate
esoph$percentage <- esoph$ncases / esoph$ncontrols
model3 <- lm(percentage ~ agegp + tobgp * alcgp, data=esoph)

model4 <- glm(percentage ~ agegp + tobgp * alcgp,
              data = esoph, family = binomial())

# predict based on my data
my <- data.frame(agegp=double(), alcgp = double(), tobgp = double(), ncases=double(), ncontrols=double())
my[nrow(my) + 1, ] = c('35-44', '0-39g/day', '0-9g/day', 0, 0)
my[nrow(my) + 1, ] = c('55-64', '0-39g/day', '0-9g/day', 0, 0)
my[nrow(my) + 1, ] = c('55-64', '120+', '30+', 0, 0)
predict(model3, my)







## Re-arrange data for a mosaic plot
ttt <- table(esoph$agegp, esoph$alcgp, esoph$tobgp)
o <- with(esoph, order(tobgp, alcgp, agegp))
ttt[ttt == 1] <- esoph$ncases[o]
tt1 <- table(esoph$agegp, esoph$alcgp, esoph$tobgp)
tt1[tt1 == 1] <- esoph$ncontrols[o]
tt <- array(c(ttt, tt1), c(dim(ttt),2),
            c(dimnames(ttt), list(c("Cancer", "control"))))
mosaicplot(tt, main = "esoph data set", color = TRUE)