plotSuperimposedEdges = function(graph, P, mst = NULL, percentage = TRUE, ...) {
  graphcpp  = mcMST:::grapheratorToGraph(graph)

  coords = as.data.frame(graph$coordinates)
  colnames(coords) = c("x1", "x2")

  # point cloud
  pl = ggplot(coords, aes(x = x1, y = x2))
  pl = pl + geom_point()

  # highlight optimal mst edges
  if (!is.null(mst)) {
    edges.mst = t(mst$toEdgeList())
    mst.segments = cbind(coords[edges.mst[, 1L], , drop = FALSE], coords[edges.mst[, 2L], , drop = FALSE])
    colnames(mst.segments) = c("x1", "y1", "x2", "y2")
    pl = pl + geom_segment(data = mst.segments, aes(x = x1, y = y1, xend = x2, yend = y2, color = NULL), color = "blue", alpha = 0.4, linetype = "solid", size = 1.4)
  }

  # convert to graphs
  if (is.matrix(P[[1L]])) {
    P = lapply(P, function(el) {
      mcMST::edgeListToGraph(el, graphcpp)
    })
  }
  P = lapply(P, function(tree) graphcpp$toBitstring(tree))
  P = colSums(do.call(rbind, P))

  edges = graphcpp$toEdgeList()
  idx.inany = which(P != 0)
  edge.count = P[idx.inany]
  if (percentage) {
    edge.count = round((edge.count / max(edge.count)) * 100, 2)
  }
  edges = t(edges[, idx.inany, drop = FALSE])

  segments.start = coords[edges[, 1L], , drop = FALSE]
  segments.end   = coords[edges[, 2L], , drop = FALSE]
  segments = cbind(segments.start, segments.end)
  colnames(segments) = c("x1", "y1", "x2", "y2")
  segments$count = edge.count
  pl = pl + geom_segment(data = segments, aes(x = x1, y = y1, xend = x2, yend = y2, color = count))
  pl = pl + scale_color_gradient(low = "#0000000F", high = '#000000FF')

  pl = pl + theme_void()
  pl = pl + theme(legend.position = "top")
  pl = pl + labs(color = "Edge count")
  return(pl)
}

getProperties = function(s) {
  list(
    degrees = s$getMaximumDegree(),
    leafs = s$getNumberOfLeafs(),
    diams = s$getDiameter()
  )
}

plotSolutions = function(graph, P, mst, captions = NULL) {
  mu = length(P)
  n = getNumberOfNodes(graph)
  msts = lapply(P, function(s) {
    s$toEdgeList()
  })

  print(msts)

  coords = as.data.frame(graph$coordinates)
  colnames(coords) = c("x1", "x2")
  gg = ggplot(coords, aes(x = x1, y = x2))
  gg = gg + geom_point()
  gg = gg + theme_void()
  print(gg)

  ggs = vector(mode = "list", length = mu)

  # count number of occurences
  freqmatrix = matrix(0, nrow = n, ncol = n)
  for (i in seq_len(mu)) {
    for (j in seq_len(n-1)) {
      the.mst = msts[[i]]
      freqmatrix[the.mst[1, j], the.mst[2, j]] = freqmatrix[the.mst[1, j], the.mst[2, j]] + 1L
      freqmatrix[the.mst[2, j], the.mst[1, j]] = freqmatrix[the.mst[2, j], the.mst[1, j]] + 1L
    }
  }

  for (i in seq_len(mu)) {
    ggtmp = gg
    # highlight optimal mst edges
    if (!is.null(mst)) {
      edges.mst = t(mst$toEdgeList())
      mst.segments = cbind(coords[edges.mst[, 1L], , drop = FALSE], coords[edges.mst[, 2L], , drop = FALSE])
      colnames(mst.segments) = c("x1", "y1", "x2", "y2")
      ggtmp = ggtmp + geom_segment(data = mst.segments, aes(x = x1, y = y1, xend = x2, yend = y2, color = NULL), color = "blue", alpha = 0.4, linetype = "solid", size = 1.4)
    }

    the.mst = msts[[i]]
    df.from = coords[the.mst[1, ], , drop = FALSE]
    df.to   = coords[the.mst[2, ], , drop = FALSE]
    uniq = sapply(1:ncol(the.mst), function(i) {
      if (freqmatrix[the.mst[1, i], the.mst[2, i]] == 1) "unique" else "shared"
    })
    the.mst = cbind(df.from, df.to, uniq)
    colnames(the.mst) = c("from.x", "from.y", "to.x", "to.y", "unique")
    ggtmp = ggtmp + geom_segment(data = the.mst, aes_string(x = "from.x", y = "from.y", xend = "to.x", yend = "to.y", color = "unique"), size = 1.4, alpha = 0.4)
    ggtmp = ggtmp + scale_color_manual(values = c("unique" = "red", "shared" = "blue"))
    ggtmp = ggtmp + geom_point()

    degrees = sapply(1:n, function(j) P[[i]]$getDegree(j))
    print(degrees)
    idx.leafs = which(degrees == 1)
    ggtmp = ggtmp + geom_point(data = coords[idx.leafs, , drop = FALSE], size = 3)

    ggtmp = ggtmp + theme(legend.position = "none")
    if (!is.null(captions)) {
      ggtmp = ggtmp + labs(caption = captions[i])
    }

    ggs[[i]] = ggtmp
  }
  ggs$nrow = 1L
  do.call(gridExtra::grid.arrange, ggs)
}
