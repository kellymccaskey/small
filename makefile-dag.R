# R script to make DAG from Makefile
# Source: https://github.com/leeper/make-example
# Copyright Thomas J. Leeper (2018), licensed MIT
requireNamespace("ggraph", quietly = TRUE)

parse_makefile <-
  function(
    file = "makefile",
    ...
  ) {
    con <- file(file, open = "rb")
    on.exit(close(con))
    m <- character()
    # handle continuation characters
    while (length(con)) {
      this_line <- readLines(con, n = 1L, warn = FALSE)
      if (!length(this_line)) {
        break
      }
      m[length(m) + 1] <- this_line
      while (grepl("\\\\$", m[length(m)]) && length(con)) {
        m[length(m)] <- sub("\\\\$", " ", m[length(m)])
        m[length(m)] <- paste0(m[length(m)], readLines(con, n = 1L))
      }
    }
    return(m)
  }

makefile_to_network <-
  function(
    lines = parse_makefile(),
    exclude = NULL,
    ...
  ) {
    rules <- lines[grepl("[ A-Za-z0-9./]+ ?[:] ?[ A-Za-z0-9./]+", lines)]
    rules_list <- strsplit(rules, " ?: ?")
    edges <- lapply(rules_list, function(x) {
      # cleanup target
      target <- x[1L]
      if (target %in% exclude) {
        return(NULL)
      }
      ## handle targets with multiple outputs
      targets <- strsplit(target, " ")[[1L]]
      
      # cleanup prereqs
      prereqs <- strsplit(x[2L], "[ \t]+")[[1L]]
      prereqs <- prereqs[!is.na(prereqs)]
      
      # return
      if (!length(prereqs) || all(prereqs == "")) {
        return(NULL)
      }
      as.vector(t(as.matrix((expand.grid(prereqs, targets, stringsAsFactors = FALSE)))))
    })
    igraph::make_graph(unlist(edges))
  }

net <- makefile_to_network(parse_makefile("Makefile"), exclude = c("all", "paper", "dag", "makefile-dag.png", "cleanALL"))
ggraph::ggraph(net, layout = "kk") + 
  ggraph::geom_edge_link(ggplot2::aes(start_cap = ggraph::label_rect(node1.name),
                                      end_cap = ggraph::label_rect(node2.name)), 
                         arrow = grid::arrow(length = grid::unit(2, 'mm'))) + 
  ggraph::geom_node_text(ggplot2::aes(label = name), size = 2) +
  ggplot2::theme_void()
ggplot2::ggsave("makefile-dag.png")