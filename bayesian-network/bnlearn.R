library(bnlearn)

data(learning.test)
df <- learning.test

tibble::glimpse(df)
# Observations: 5,000
# Variables: 6
# $ A <fctr> b, b, a, a, a, c, c, b, b, b, b, c, c, c, b, a, a, c, b, b, ...
# $ B <fctr> c, a, a, a, a, c, c, b, b, a, a, c, c, c, c, a, a, c, a, c, ...
# $ C <fctr> b, c, a, a, b, a, b, a, b, b, a, b, b, a, a, b, b, a, b, a, ...
# $ D <fctr> a, a, a, a, c, c, c, b, a, a, b, b, a, c, b, c, a, b, a, b, ...
# $ E <fctr> b, b, a, b, a, c, c, b, c, a, c, c, c, c, c, a, b, b, c, a, ...
# $ F <fctr> b, b, a, b, a, a, a, b, a, a, a, a, b, a, a, a, b, b, a, b, ...

# first try the Grow-Shrink algorithm
res <- gs(df)
plot(res, main = "the Grow-Shrink (GS)")

# now try the Incremental Association algorithm.
res2 <- iamb(df)
plot(res2, main = "the Incremental Association")

# the network structures seem to be identical, don't they?
all.equal(res, res2)

# how many tests each of the two algorithms used?
ntests(res)
ntests(res2)

# the hill-climbing (HC)
res3 <- hc(df)
plot(res3, main = "the hill-climbing (HC)")

fitted <- bn.fit(res3, data = df, method = "bayes")

particles <- cpdist(fitted,
                   nodes = "A",
                   evidence = (B == "C"))

prop.table(table(particles))

cpquery(fitted,
        event = (A == "b"),
        evidence = (D == "c"))
