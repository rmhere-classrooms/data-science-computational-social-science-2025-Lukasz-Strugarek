library(igraph)

g <- barabasi.game(1000)

#wizualizacja layout Fruchterman & Reingold
layout <- layout.fruchterman.reingold(g)
plot(g, layout=layout, vertex.size=2,
     vertex.label=NA, edge.arrow.size=.2)

#poszukiwanie węzła o największej wartości miary betweenness
which.max(betweenness(g))

#średnica grafu
diameter(g)


#w grafach ER każde połączenie ma identyczne szanse na powstanie, w grafie BA węzły dodawane są iteracyjnie i preferują połączenia z węzłami mającymi już dużo połączeń