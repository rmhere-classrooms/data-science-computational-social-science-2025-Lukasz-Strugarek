library(igraph)

g <- erdos.renyi.game(p.or.m=0.05, n=100)

summary(g)
#graf nie jest ważony na co wskazuje podsumowanie grafu (brak litery W)

#lista wierzchołków
V(g)

#lista krawędzi
E(g)

#dodanie losowych wag do krawędzi
E(g)$weight <- runif(length(E(g)), 0.01, 1)

summary(g)
#pojawił się parametr W w podsumawaniu grafu co wskazuje na to że jest już ważony

#stopnie węzłów
degree(g)

#histogram stopnie węzłów
hist(degree(g))

#liczba klastrów
clusters(g)$no

#wizualizacja grafu (rozmiar wg pageRank)
pr <- page.rank(g)$vector

plot(g, vertex.size=pr*500,
     vertex.label=NA)
