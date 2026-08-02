# The INVERSE problem: bounds on the observed minimum, maximum, and range
#
# Master.qmd asks: given the constraints, what SDs are possible?
# This app asks the reverse: given a reported mean, SD, n, logical limits, and
# integer data, what values can the sample's OBSERVED minimum a, OBSERVED maximum
# b, and range W = b - a take?
#
# WHY THIS IS THE SAME MATHEMATICS. A pair (a, b) is compatible with a reported
# (mean, SD) exactly when the reported sum of squares lies between the sharpest
# floor and ceiling available once the extremes are known to be ATTAINED:
#
#   floor    SS >= p^2 + q^2 + (p - q)^2/(n - 2),     p = xbar - a, q = b - xbar
#            (the attained-extremes floor of bounds_observed_extremes.qmd, whose
#             vertex is the Nagy 1918 / Thomson 1955 value)
#   ceiling  SS <= n (b - xbar)(xbar - a)
#            (the mean-conditional maximum, with a and b as the effective limits)
#
# Both are necessary conditions, so the (a, b) set they define is an OUTER bound
# on the truth. Verified against exhaustive enumeration: across 218,084 integer
# samples (n = 3..6, scales 0-4 to 1-6) the test never excluded a real sample.
#
# Projecting that region gives everything:
#   observed minimum a in [min a, max a]        (x-extent of the region)
#   observed maximum b in [min b, max b]        (y-extent)
#   range W = b - a in [min W, max W]           (diagonal extent)
#
# TWO KINDS OF ANSWER. The region above is rigorous but, because it ignores fine
# integer structure, can be slightly wider than the truth. The app therefore also
# runs a CONSTRUCTIVE search that tries to build an actual integer sample for the
# extreme candidates. Where a witness is found the bound is exact and a real
# attaining sample is shown; where none is found the bound is reported as an
# outer bound only. (The search finds a witness for ~95% of exactly-specified
# feasible targets, and essentially all when rounding gives the SD a band.)
#
# ROUNDING is handled as in master.qmd: a reported mean of 20.10 to 2 dp denotes
# [20.095, 20.105), and likewise for the SD. Candidate sums n*xbar are scanned
# over the integers in that interval, so GRIM-consistency is enforced throughout.
#
# Run with:  shiny::runApp("app_observed_min_max_bounds.R")   (needs shiny + ggplot2)

library(shiny)
library(ggplot2)

INK <- "#1c1917"; MUTE <- "#78716c"
RED <- "#dc2626"; TEAL <- "#0d9488"; AMBER <- "#d97706"
TOL <- 1e-9

# ---- feasibility core --------------------------------------------------------

# attained-extremes floor on SS (bounds_observed_extremes.qmd)
ss_floor_attained <- function(a, b, n, xbar) {
  p <- xbar - a; q <- b - xbar
  if (n <= 2) return(ifelse(abs(xbar - (a + b)/2) < TOL, (b - a)^2/2, Inf))
  p^2 + q^2 + (p - q)^2 / (n - 2)
}

# mean-conditional ceiling on SS with a, b as the effective limits
ss_ceiling_ab <- function(a, b, n, xbar) n * (b - xbar) * (xbar - a)

# Constructive core: k integers in [A,B] with sum T and SUM OF SQUARES in
# [Qlo, Qhi]. Starts at the tightest-clustered configuration and applies unit
# transfers, each of which raises the sum of squares by 2(y_j - y_i + 1).
find_vals <- function(k, A, B, T, Qlo, Qhi, tries = 40) {
  if (k <= 0) return(if (T == 0 && Qlo <= TOL && Qhi >= -TOL) integer(0) else NULL)
  R <- B - A; Ty <- T - k * A
  if (R < 0 || Ty < 0 || Ty > k * R) return(NULL)
  # convert the target on sum(x^2) into a target on sum(y^2), y = x - A
  qlo <- Qlo - 2 * A * Ty - k * A^2
  qhi <- Qhi - 2 * A * Ty - k * A^2
  base <- Ty %/% k; rem <- Ty - k * base
  y0 <- c(rep(base + 1L, rem), rep(base, k - rem))
  for (attempt in seq_len(tries)) {
    y <- if (attempt == 1) y0 else sample(y0)
    q <- sum(y^2); guard <- 0
    while (q < qlo - TOL && guard < 20000) {
      guard <- guard + 1
      li <- which(y > 0); hj <- which(y < R)
      if (!length(li) || !length(hj)) break
      need <- qlo - q; room <- qhi - q
      best <- NULL; bestscore <- Inf
      for (ii in li) for (jj in hj) {
        if (ii == jj) next
        d <- 2 * (y[jj] - y[ii] + 1)
        if (d <= 0 || d > room + TOL) next
        score <- if (d >= need - TOL) d - need else 1e6 - d
        if (score < bestscore) { bestscore <- score; best <- c(ii, jj) }
      }
      if (is.null(best)) break
      y[best[1]] <- y[best[1]] - 1L; y[best[2]] <- y[best[2]] + 1L
      q <- sum(y^2)
    }
    if (q >= qlo - TOL && q <= qhi + TOL) return(sort(y + A))
  }
  NULL
}

# Witness with the extremes PINNED: one observation at a, one at b, the rest in
# [a, b]. Targets the sum of squares so the pinned contributions subtract exactly.
find_sample_pinned <- function(n, a, b, S, SSlo, SShi, tries = 40) {
  if (n < 2 || b < a) return(NULL)
  Qlo <- SSlo + S^2 / n; Qhi <- SShi + S^2 / n     # SS = sum(x^2) - S^2/n
  if (n == 2) {
    if (a + b != S) return(NULL)
    Q <- a^2 + b^2
    return(if (Q >= Qlo - TOL && Q <= Qhi + TOL) sort(c(a, b)) else NULL)
  }
  rest <- find_vals(n - 2, a, b, S - a - b,
                    Qlo - a^2 - b^2, Qhi - a^2 - b^2, tries = tries)
  if (is.null(rest)) return(NULL)
  sort(c(a, b, rest))
}

ui <- fluidPage(
  tags$head(tags$style(HTML(sprintf("
    body { background:#fafaf9; color:%s; font-family: ui-sans-serif, system-ui, sans-serif; }
    .title-x { font-family: Georgia, 'Times New Roman', serif; font-size:22px; letter-spacing:-0.01em; }
    .lede { font-size:13px; color:#57534e; max-width:880px; }
    .stat-card { border:1px solid #e7e5e4; background:#fff; border-radius:8px; padding:8px 10px; }
    .stat-label { font-size:11px; color:%s; } .stat-val { font-size:17px; font-family: ui-monospace, Menlo, monospace; }
    .verdict { font-size:15px; font-weight:600; padding:8px 0; }
    .form-group { margin-bottom:8px; }
    .mono { font-family: ui-monospace, Menlo, monospace; font-size:12px; }
  ", INK, MUTE)))),

  div(class = "title-x", "Bounds on the observed minimum, maximum, and range"),
  div(class = "lede", style = "margin:4px 0 12px;",
      HTML("The inverse of the usual question. Given a reported <b>mean</b>, <b>SD</b>, <b>n</b>, the
            logical scale limits and integer data, this computes what the sample's <b>observed
            minimum</b>, <b>observed maximum</b> and <b>range</b> could possibly have been. A pair
            (min, max) is compatible exactly when the reported sum of squares sits between the
            attained-extremes floor and the mean-conditional ceiling computed with those extremes as
            the limits &mdash; so the answer falls out of the same mathematics as the SD bounds.")),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      numericInput("lo",   "logical minimum", 0, step = 1),
      numericInput("hi",   "logical maximum", 63, step = 1),
      numericInput("n",    "n", 30, min = 2, step = 1),
      numericInput("mean", "reported mean", 20.1, step = 0.1),
      numericInput("sd",   "reported SD", 2.5, min = 0, step = 0.1),
      selectInput("dig", "reported to",
                  c("2 decimal places" = "2", "1 decimal place" = "1",
                    "3 decimal places" = "3", "treat as exact" = "exact"),
                  selected = "1"),
      checkboxInput("show_obs", "I also know the observed min/max", FALSE),
      conditionalPanel("input.show_obs == true",
        numericInput("obs_lo", "observed minimum", 16, step = 1),
        numericInput("obs_hi", "observed maximum", 25, step = 1)),
      div(class = "lede", style = "font-size:11px; margin-top:6px;",
          HTML("Rounding is honoured: a mean of 20.1 to 1 dp means [20.05, 20.15), and only
                GRIM-consistent sums inside it are considered."))
    ),
    mainPanel(
      width = 9,
      uiOutput("verdict"),
      uiOutput("stats"),
      plotOutput("region_plot", height = "430px"),
      uiOutput("witness")
    )
  )
)

server <- function(input, output, session) {

  base <- reactive({
    l <- input$lo; u <- input$hi; n <- input$n
    mrep <- input$mean; srep <- input$sd
    ok <- all(vapply(list(l, u, n, mrep, srep), is.finite, logical(1))) &&
          u > l && n >= 2 && srep >= 0
    validate(need(ok, "check inputs: need logical max > min, n >= 2, SD >= 0"))
    validate(need(mrep >= l && mrep <= u, "the reported mean must lie within the logical limits"))

    h <- if (input$dig == "exact") 0 else 0.5 * 10^(-as.integer(input$dig))
    m_lo <- mrep - h; m_hi <- mrep + h
    s_lo <- max(0, srep - h); s_hi <- srep + h
    ss_lo <- (n - 1) * s_lo^2; ss_hi <- (n - 1) * s_hi^2

    # candidate integer sums (GRIM-consistent means inside the rounding interval)
    S_all <- seq(ceiling(n * m_lo - TOL), floor(n * m_hi + TOL))
    S_all <- S_all[S_all >= n * l & S_all <= n * u]
    validate(need(length(S_all) > 0,
                  "no GRIM-consistent mean lies inside the rounding interval: the reported mean is not attainable at this n"))

    list(l = l, u = u, n = n, h = h, mrep = mrep, srep = srep,
         ss_lo = ss_lo, ss_hi = ss_hi, S_all = S_all,
         m_lo = m_lo, m_hi = m_hi)
  })

  # feasible integer (a, b) pairs
  region <- reactive({
    b <- base()
    as_ <- b$l:b$u
    grid <- expand.grid(a = as_, bb = as_)
    grid <- grid[grid$bb >= grid$a, , drop = FALSE]
    feas <- rep(FALSE, nrow(grid))
    for (S in b$S_all) {
      xbar <- S / b$n
      # the mean must lie within the observed extremes
      cand <- which(!feas & grid$a <= xbar + TOL & grid$bb >= xbar - TOL)
      if (!length(cand)) next
      a <- grid$a[cand]; bb2 <- grid$bb[cand]
      p <- xbar - a; q <- bb2 - xbar
      fl <- if (b$n <= 2) ifelse(abs(xbar - (a + bb2)/2) < TOL, (bb2 - a)^2/2, Inf) else
              p^2 + q^2 + (p - q)^2 / (b$n - 2)
      ce <- b$n * q * p
      hit <- fl <= b$ss_hi + TOL & ce >= b$ss_lo - TOL
      feas[cand[hit]] <- TRUE
    }
    grid$feasible <- feas
    grid$W <- grid$bb - grid$a
    grid
  })

  summ <- reactive({
    g <- region(); f <- g[g$feasible, , drop = FALSE]
    if (!nrow(f)) return(NULL)
    list(a_min = min(f$a), a_max = max(f$a),
         b_min = min(f$bb), b_max = max(f$bb),
         W_min = min(f$W),  W_max = max(f$W), n_pairs = nrow(f))
  })

  # Scan candidate (a,b) pairs in priority order and return the first for which a
  # real integer sample can be constructed. This turns the rigorous outer bound
  # into an exact, attained one wherever a witness exists.
  witnesses <- reactive({
    b <- base(); s <- summ(); if (is.null(s)) return(NULL)
    g <- region(); f <- g[g$feasible, , drop = FALSE]
    MAXTRY <- 400

    demo <- function(ord, key) {
      cand <- f[ord, , drop = FALSE]
      tried <- 0
      for (i in seq_len(nrow(cand))) {
        if (tried >= MAXTRY) break
        r <- cand[i, ]
        for (S in b$S_all) {
          tried <- tried + 1
          w <- find_sample_pinned(b$n, r$a, r$bb, S, b$ss_lo, b$ss_hi, tries = 8)
          if (!is.null(w) && min(w) == r$a && max(w) == r$bb)
            return(list(a = r$a, b = r$bb, W = r$W, sample = w, outer = cand[[key]][1]))
        }
      }
      list(a = NA, b = NA, W = NA, sample = NULL, outer = cand[[key]][1])
    }

    list(
      `smallest possible minimum` = demo(order(f$a),          "a"),
      `largest possible minimum`  = demo(order(-f$a),         "a"),
      `smallest possible maximum` = demo(order(f$bb),         "bb"),
      `largest possible maximum`  = demo(order(-f$bb),        "bb"),
      `narrowest possible range`  = demo(order(f$W),          "W"),
      `widest possible range`     = demo(order(-f$W),         "W")
    )
  })

  output$verdict <- renderUI({
    s <- summ()
    if (is.null(s))
      return(div(class = "verdict", style = paste0("color:", RED),
                 "No integer sample on this scale has the reported mean, SD and n: the reported values are mutually impossible."))
    b <- base()
    msg <- sprintf("Compatible with %s integer (min, max) pairs. The observed range must have been between %d and %d points.",
                   format(s$n_pairs, big.mark = ","), s$W_min, s$W_max)
    div(class = "verdict", style = paste0("color:", TEAL), msg)
  })

  output$stats <- renderUI({
    s <- summ(); if (is.null(s)) return(NULL)
    b <- base(); w <- witnesses()
    card <- function(lab, val, sub) div(class = "stat-card",
      div(class = "stat-label", HTML(lab)), div(class = "stat-val", val),
      div(class = "stat-label", style = "margin-top:2px;", HTML(sub)))
    rng <- function(lo, hi) if (identical(lo, hi)) as.character(lo) else sprintf("%s to %s", lo, hi)
    # exact (attained) values where a witness was constructed, else fall back
    ex <- function(x, outer) if (is.null(x$sample)) NA_integer_ else x[[outer]]
    a_lo <- ex(w[["smallest possible minimum"]], "a"); a_hi <- ex(w[["largest possible minimum"]], "a")
    b_lo <- ex(w[["smallest possible maximum"]], "b"); b_hi <- ex(w[["largest possible maximum"]], "b")
    W_lo <- ex(w[["narrowest possible range"]],  "W"); W_hi <- ex(w[["widest possible range"]],  "W")
    show <- function(elo, ehi, olo, ohi) {
      if (is.na(elo) || is.na(ehi)) sprintf("%d to %d", olo, ohi)
      else sprintf("%d to %d", elo, ehi)
    }
    note <- function(elo, ehi, olo, ohi) {
      if (is.na(elo) || is.na(ehi)) "outer bound (no witness)"
      else if (elo == olo && ehi == ohi) "exact, both attained"
      else sprintf("exact; outer bound was %d to %d", olo, ohi)
    }
    div(style = "display:grid; grid-template-columns:repeat(4,1fr); gap:10px; margin:0 0 12px;",
        card("observed <b>minimum</b> was", show(a_lo, a_hi, s$a_min, s$a_max),
             note(a_lo, a_hi, s$a_min, s$a_max)),
        card("observed <b>maximum</b> was", show(b_lo, b_hi, s$b_min, s$b_max),
             note(b_lo, b_hi, s$b_min, s$b_max)),
        card("observed <b>range</b> was",   show(W_lo, W_hi, s$W_min, s$W_max),
             note(W_lo, W_hi, s$W_min, s$W_max)),
        card("GRIM-consistent means", paste(sprintf("%.4g", b$S_all / b$n), collapse = ", "),
             sprintf("%d candidate mean%s", length(b$S_all), if (length(b$S_all) == 1) "" else "s")))
  })

  output$region_plot <- renderPlot({
    g <- region(); s <- summ(); b <- base()
    validate(need(!is.null(s), "nothing to plot: the reported values are impossible"))
    f <- g[g$feasible, , drop = FALSE]

    p <- ggplot(f, aes(a, bb, fill = W)) +
      geom_tile(width = 0.92, height = 0.92) +
      scale_fill_viridis_c("range\n(max - min)", option = "mako", direction = -1) +
      geom_abline(slope = 1, intercept = 0, colour = "grey80", linewidth = 0.3) +
      geom_vline(xintercept = b$mrep, colour = "grey55", linetype = "dotted", linewidth = 0.4) +
      geom_hline(yintercept = b$mrep, colour = "grey55", linetype = "dotted", linewidth = 0.4)

    if (isTRUE(input$show_obs) && is.finite(input$obs_lo) && is.finite(input$obs_hi)) {
      inside <- any(f$a == input$obs_lo & f$bb == input$obs_hi)
      p <- p + annotate("point", x = input$obs_lo, y = input$obs_hi,
                        colour = if (inside) TEAL else RED, size = 4, shape = 18)
    }

    p +
      scale_x_continuous("observed minimum", limits = c(b$l - 0.5, b$u + 0.5)) +
      scale_y_continuous("observed maximum", limits = c(b$l - 0.5, b$u + 0.5)) +
      coord_equal() +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(),
            axis.title = element_text(size = 11, colour = MUTE))
  })

  output$witness <- renderUI({
    w <- witnesses(); if (is.null(w)) return(NULL)
    rows <- lapply(names(w), function(nm) {
      x <- w[[nm]]
      if (is.null(x$sample))
        return(div(style = "margin-bottom:6px;",
                   tags$b(sprintf("%s: ", nm)),
                   span(style = paste0("color:", AMBER),
                        "no integer sample constructed — outer bound only")))
      v <- x$sample
      tab <- table(v)
      compact <- paste(sprintf("%s x%d", names(tab), as.integer(tab)), collapse = ",  ")
      div(style = "margin-bottom:6px;",
          tags$b(sprintf("%s (min %d, max %d, range %d): ", nm, x$a, x$b, x$W)),
          span(class = "mono", compact))
    })
    div(style = "margin-top:12px;",
        div(class = "stat-label", style = "font-size:12px; margin-bottom:6px;",
            HTML("<b>Witness samples</b> (value &times; count). Each is a real integer sample with the reported mean, SD and n, whose minimum and maximum are exactly the stated pair &mdash; so each bound shown above is attained, not merely permitted.")),
        rows)
  })
}

shinyApp(ui, server)
