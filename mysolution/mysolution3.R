library(shiny)
library(bslib)
library(igraph)


prepare_graph <- function() {
  dane <- read.table(url("https://bergplace.org/share/out.radoslaw_email_email"), skip = 2)
  dane <- dane[, 1:2]
  colnames(dane) <- c("from", "to")
  
  graf <- graph_from_data_frame(dane, directed = TRUE)
  graf <- simplify(graf, remove.multiple = TRUE, remove.loops = TRUE)
  
  cnt_matrix <- table(dane$from, dane$to)
  total_sent <- rowSums(cnt_matrix)
  
  edges <- as_edgelist(graf)
  weights <- numeric(nrow(edges))
  
  for (i in 1:nrow(edges)) {
    from_node <- as.character(edges[i, 1])
    to_node <- as.character(edges[i, 2])
    weights[i] <- cnt_matrix[from_node, to_node] / total_sent[from_node]
  }
  
  E(graf)$weight <- weights
  return(graf)
}

select_seed_nodes <- function(graf) {
  n_seeds <- ceiling(vcount(graf) * 0.05)
  
  list(
    outdegree = order(degree(graf, mode = "out"), decreasing = TRUE)[1:n_seeds],
    betweenness = order(betweenness(graf, directed = TRUE), decreasing = TRUE)[1:n_seeds],
    closeness = order(closeness(graf, mode = "out"), decreasing = TRUE)[1:n_seeds],
    random = sample(1:vcount(graf), n_seeds),
    pagerank = order(page_rank(graf, directed = TRUE)$vector, decreasing = TRUE)[1:n_seeds]
  )
}

simulate_cascade <- function(graf, seed_nodes, weight_mult, max_iter) {
  n <- vcount(graf)
  activated <- rep(FALSE, n)
  activated[seed_nodes] <- TRUE
  attempted <- matrix(FALSE, nrow = n, ncol = n)
  
  history <- length(seed_nodes)
  newly_activated <- seed_nodes
  iteration <- 0
  
  while (length(newly_activated) > 0 && iteration < max_iter) {
    iteration <- iteration + 1
    next_wave <- integer(0)
    
    for (node in newly_activated) {
      neighbors <- neighbors(graf, node, mode = "out")
      if (length(neighbors) == 0) next
      
      for (neighbor in neighbors) {
        nb_id <- as.integer(neighbor)
        
        if (!activated[nb_id] && !attempted[node, nb_id]) {
          attempted[node, nb_id] <- TRUE
          
          edge_id <- get.edge.ids(graf, c(node, neighbor))
          weight <- E(graf)$weight[edge_id] * (weight_mult / 100)
          
          if (weight >= 1 || runif(1) < weight) {
            activated[nb_id] <- TRUE
            next_wave <- c(next_wave, nb_id)
          }
        }
      }
    }
    
    newly_activated <- unique(next_wave)
    history <- c(history, length(newly_activated))
  }
  
  if (length(history) < (max_iter + 1)) {
    history <- c(history, rep(0, max_iter + 1 - length(history)))
  }
  
  return(history)
}

run_experiment <- function(graf, method, weight_mult, n_iter) {
  seeds <- select_seed_nodes(graf)[[method]]
  
  #można dodać wiele symulacji ale to znacząco wydłuży czas
  all_results <- matrix(0, nrow = n_iter + 1, ncol = 1)
  
  for (i in 1:1) {
    all_results[, i] <- simulate_cascade(graf, seeds, weight_mult, n_iter)
  }
  
  rowMeans(all_results)
}


ui <- page_sidebar(
  title = "Zadanie 3 - danologia",
  
  sidebar = sidebar(
    sliderInput("weight_multiplier",
                "Weight multiplier (%):",
                min = 10, max = 200, value = 100, step = 10),
    
    sliderInput("max_iterations",
                "Max iterations:",
                min = 1, max = 50, value = 10, step = 1),
    
    actionButton("run_simulation", "Run simulation", class = "btn-primary"),
    
    hr(),
    
    textOutput("status")
  ),
  
  plotOutput("diffusion_plot", height = "600px")
)


server <- function(input, output, session) {
  
  graf <- reactiveVal(NULL)
  results <- reactiveVal(NULL)
  
  observe({
    g <- prepare_graph()
    graf(g)
  })
  
  output$status <- renderText({
    if (is.null(graf())) {
      "Loading graph..."
    } else {
      paste("Ready:", vcount(graf()), "nodes,", ecount(graf()), "edges")
    }
  })
  
  observeEvent(input$run_simulation, {
    g <- graf()
    if (is.null(g)) return(NULL)
    
    weight_mult <- input$weight_multiplier
    max_iter <- input$max_iterations
    
    withProgress(message = 'Running simulation...', value = 0, {
      incProgress(0.2, detail = "Outdegree (1/5)")
      outdegree_res <- run_experiment(g, "outdegree", weight_mult, max_iter)
      
      incProgress(0.2, detail = "Betweenness (2/5)")
      betweenness_res <- run_experiment(g, "betweenness", weight_mult, max_iter)
      
      incProgress(0.2, detail = "Closeness (3/5)")
      closeness_res <- run_experiment(g, "closeness", weight_mult, max_iter)
      
      incProgress(0.2, detail = "Random (4/5)")
      random_res <- run_experiment(g, "random", weight_mult, max_iter)
      
      incProgress(0.2, detail = "PageRank (5/5)")
      pagerank_res <- run_experiment(g, "pagerank", weight_mult, max_iter)
      
      res <- list(
        outdegree = outdegree_res,
        betweenness = betweenness_res,
        closeness = closeness_res,
        random = random_res,
        pagerank = pagerank_res
      )
      
      results(res)
    })
  })
  
  output$diffusion_plot <- renderPlot({
    res <- results()
    if (is.null(res)) {
      plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE)
      text(0, 0, "Click 'Run simulation' to start", cex = 1.5, col = "gray")
      return(NULL)
    }
    
    max_iter <- input$max_iterations
    
    colors <- c("#E74C3C", "#3498DB", "#2ECC71", "#95A5A6", "#9B59B6")
    labels <- c("Out-degree", "Betweenness", "Closeness", "Random", "PageRank")
    
    plot(NULL, xlim = c(0, max_iter), ylim = c(0, max(sapply(res, max))),
         xlab = "Iteration", ylab = "Newly activated",
         main = "Diffusion process")
    
    grid(col = "gray90")
    
    for (i in 1:5) {
      lines(0:max_iter, res[[i]], col = colors[i], lwd = 3, type = "b", pch = 19)
    }
    
    legend("bottomright", legend = labels, col = colors, lwd = 3, pch = 19)
  })
}

shinyApp(ui = ui, server = server)