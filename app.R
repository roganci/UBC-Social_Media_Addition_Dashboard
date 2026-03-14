library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(tidyr)

# ── DATA ──────────────────────────────────────────────────────────────────────

df <- tryCatch(
  read.csv("data/raw/Students-Social-Media-Addiction.csv", stringsAsFactors = FALSE),
  error = function(e) {
    set.seed(42)
    n <- 500
    countries <- c("USA","India","UK","Canada","Australia","Brazil","Germany","Japan","France","South Korea")
    platforms <- c("Instagram","TikTok","YouTube","Facebook","Twitter","Snapchat","WhatsApp","LinkedIn")
    genders   <- c("Male","Female")
    levels    <- c("Undergraduate","Graduate")
    perf      <- c("Yes","No")
    data.frame(
      Student_ID            = seq_len(n),
      Age                   = sample(18:28, n, replace = TRUE),
      Gender                = sample(genders, n, replace = TRUE),
      Academic_Level        = sample(levels, n, replace = TRUE, prob = c(0.7, 0.3)),
      Country               = sample(countries, n, replace = TRUE),
      Most_Used_Platform    = sample(platforms, n, replace = TRUE),
      Avg_Daily_Usage_Hours = round(runif(n, 1, 10), 1),
      Sleep_Hours_Per_Night = round(rnorm(n, 6.5, 1.2), 1),
      Addicted_Score        = round(runif(n, 1, 10), 1),
      Mental_Health_Score   = round(rnorm(n, 5, 2), 1),
      Affects_Academic_Performance = sample(perf, n, replace = TRUE, prob = c(0.6, 0.4)),
      stringsAsFactors = FALSE
    )
  }
)

df <- df[df$Academic_Level %in% c("Undergraduate", "Graduate"), ]
df$Sleep_Hours_Per_Night  <- as.numeric(df$Sleep_Hours_Per_Night)
df$Addicted_Score         <- as.numeric(df$Addicted_Score)
df$Mental_Health_Score    <- as.numeric(df$Mental_Health_Score)
df$Avg_Daily_Usage_Hours  <- as.numeric(df$Avg_Daily_Usage_Hours)

AGE_MIN   <- min(df$Age, na.rm = TRUE)
AGE_MAX   <- max(df$Age, na.rm = TRUE)
COUNTRIES <- sort(unique(df$Country))
PLATFORMS <- sort(unique(df$Most_Used_Platform))

# ── SOCIAL MEDIA BRAND COLOURS ────────────────────────────────────────────────
platform_colors <- c(
  "Facebook"  = "#1877F2",
  "Instagram" = "#E1306C",
  "Twitter"   = "#1DA1F2",
  "Snapchat"  = "#FFFC00",
  "TikTok"    = "#EE1D52",
  "LinkedIn"  = "#0077B5",
  "YouTube"   = "#FF0000",
  "WhatsApp"  = "#25D366",
  "WeChat"    = "#07C160",
  "KakaoTalk" = "#FAE100",
  "LINE"      = "#06C755",
  "VKontakte" = "#4C75A3",
  "Other"     = "#94A3B8"
)

get_platform_color <- function(p) {
  col <- platform_colors[p]
  if (is.na(col)) "#94A3B8" else unname(col)
}

# ── CSS ───────────────────────────────────────────────────────────────────────

custom_css <- "
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&family=Space+Grotesk:wght@400;500;600;700&display=swap');

:root {
  --bg:      #0D0D0D;
  --surface: #1A1A2E;
  --card:    #16213E;
  --accent:  #E1306C;
  --accent2: #1877F2;
  --text:    #F1F5F9;
  --muted:   #94A3B8;
  --green:   #25D366;
  --border:  rgba(255,255,255,0.08);
}

* { box-sizing: border-box; }

html, body {
  background-color: var(--bg) !important;
  color: var(--text) !important;
  font-family: 'Inter', sans-serif !important;
  min-height: 100vh;
}

/* ── Hero ── */
.dash-hero {
  background: linear-gradient(135deg, #1A1A2E 0%, #16213E 55%, #0F3460 100%);
  border-radius: 20px;
  padding: 38px 44px 30px;
  margin: 14px 14px 22px;
  position: relative;
  overflow: hidden;
  border: 1px solid var(--border);
}
.dash-hero::before {
  content: '';
  position: absolute; top: -60px; right: -60px;
  width: 280px; height: 280px; border-radius: 50%;
  background: radial-gradient(circle, rgba(225,48,108,0.18) 0%, transparent 70%);
  pointer-events: none;
}
.dash-hero::after {
  content: '';
  position: absolute; bottom: -40px; left: 25%;
  width: 200px; height: 200px; border-radius: 50%;
  background: radial-gradient(circle, rgba(24,119,242,0.13) 0%, transparent 70%);
  pointer-events: none;
}
.dash-hero h1 {
  font-family: 'Space Grotesk', sans-serif !important;
  font-size: 2.3rem !important;
  font-weight: 700 !important;
  color: #ffffff !important;
  margin: 0 0 8px !important;
  letter-spacing: -0.8px;
  line-height: 1.2;
}
.dash-hero h1 .accent-word {
  background: linear-gradient(90deg, #E1306C, #F77737);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  background-clip: text;
}
.dash-hero p {
  color: rgba(241,245,249,0.62) !important;
  font-size: 0.96rem; margin: 0 0 16px; font-weight: 300;
}
.platform-tags { display: flex; gap: 8px; flex-wrap: wrap; }
.platform-tag {
  display: inline-flex; align-items: center; gap: 5px;
  border-radius: 20px; padding: 4px 12px;
  font-size: 0.74rem; font-weight: 600; letter-spacing: 0.3px;
  border: 1px solid rgba(255,255,255,0.14);
}

/* ── Sidebar ── */
.bslib-sidebar-layout > .sidebar {
  background: var(--surface) !important;
  border-right: 1px solid var(--border) !important;
  padding: 20px 18px !important;
}
.sidebar h5 {
  font-family: 'Space Grotesk', sans-serif !important;
  color: var(--text) !important;
  font-size: 1rem !important;
  font-weight: 600 !important;
  margin-bottom: 18px;
  padding-bottom: 10px;
  border-bottom: 2px solid var(--accent);
}
.sidebar label, .sidebar .control-label, .sidebar .form-label {
  color: var(--muted) !important;
  font-size: 0.74rem !important;
  font-weight: 600 !important;
  text-transform: uppercase !important;
  letter-spacing: 0.8px !important;
}
.sidebar .radio label {
  color: var(--text) !important;
  font-size: 0.88rem !important;
  font-weight: 400 !important;
  text-transform: none !important;
  letter-spacing: 0 !important;
}
.filter-divider { height: 1px; background: var(--border); margin: 14px 0; }

/* slider accent */
.irs--shiny .irs-bar,
.irs--shiny .irs-bar--single { background: var(--accent) !important; border-color: var(--accent) !important; }
.irs--shiny .irs-handle > i:first-child { background: var(--accent) !important; }
.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background: var(--accent) !important; }

/* ── Value boxes ── */
.bslib-value-box {
  border: 1px solid var(--border) !important;
  border-radius: 16px !important;
  background: var(--surface) !important;
  box-shadow: 0 0 24px rgba(225,48,108,0.1) !important;
  transition: transform 0.2s, box-shadow 0.2s;
  min-height: 110px !important;
}
.bslib-value-box:hover {
  transform: translateY(-3px);
  box-shadow: 0 8px 32px rgba(225,48,108,0.22) !important;
}
.bslib-value-box .value-box-value {
  font-family: 'Space Grotesk', sans-serif !important;
  font-size: 1.85rem !important;
  font-weight: 700 !important;
  color: var(--text) !important;
  line-height: 1.2 !important;
}
.bslib-value-box .value-box-title {
  font-size: 0.7rem !important;
  font-weight: 600 !important;
  letter-spacing: 1px !important;
  text-transform: uppercase !important;
  color: var(--muted) !important;
}

/* ── Cards ── */
.card {
  background: var(--card) !important;
  border: 1px solid var(--border) !important;
  border-radius: 16px !important;
  box-shadow: 0 4px 24px rgba(0,0,0,0.35) !important;
  color: var(--text) !important;
}
.card-header {
  background: rgba(255,255,255,0.04) !important;
  border-bottom: 1px solid var(--border) !important;
  border-radius: 16px 16px 0 0 !important;
  color: var(--text) !important;
  font-family: 'Space Grotesk', sans-serif !important;
  font-weight: 600 !important;
  font-size: 0.88rem !important;
  padding: 12px 18px !important;
}

/* ── Nav tabs ── */
.nav-tabs {
  border-bottom: 1px solid var(--border) !important;
  padding: 0 14px;
  background: var(--surface) !important;
}
.nav-tabs .nav-link {
  color: var(--muted) !important;
  font-weight: 500 !important;
  font-size: 0.86rem !important;
  border: none !important;
  border-bottom: 2px solid transparent !important;
  border-radius: 0 !important;
  padding: 12px 20px !important;
  background: transparent !important;
  transition: color 0.15s, border-color 0.15s;
}
.nav-tabs .nav-link.active {
  color: var(--text) !important;
  border-bottom: 2px solid var(--accent) !important;
  background: transparent !important;
}
.nav-tabs .nav-link:hover:not(.active) {
  color: var(--text) !important;
  border-bottom: 2px solid rgba(225,48,108,0.4) !important;
}
.tab-content, .tab-pane { background: var(--bg) !important; }
.bslib-sidebar-layout > .main { background: var(--bg) !important; padding: 20px !important; }

/* ── Country Story ── */
.country-story-header {
  font-family: 'Space Grotesk', sans-serif !important;
  color: var(--text); font-size: 1.55rem; font-weight: 700; margin-bottom: 4px;
}
.country-story-sub { color: var(--muted); font-size: 0.87rem; margin-bottom: 16px; }
.stat-pill {
  display: inline-flex; align-items: center; gap: 8px;
  background: rgba(255,255,255,0.05);
  border: 1px solid var(--border);
  border-radius: 12px; padding: 8px 16px;
  font-size: 0.8rem; font-weight: 500; color: var(--muted); margin: 4px;
}
.stat-pill .pill-val {
  font-family: 'Space Grotesk', sans-serif;
  font-size: 1.1rem; font-weight: 700; color: var(--text);
}

/* ── DataTable dark ── */
table.dataTable thead th {
  background: rgba(225,48,108,0.14) !important;
  color: var(--text) !important;
  border-bottom: 1px solid var(--border) !important;
  font-size: 0.78rem; font-weight: 600; letter-spacing: 0.5px;
}
table.dataTable tbody td {
  color: var(--muted) !important;
  border-bottom: 1px solid var(--border) !important;
  font-size: 0.84rem;
}
table.dataTable.stripe tbody tr.odd { background: rgba(255,255,255,0.025) !important; }
table.dataTable tbody tr:hover { background: rgba(225,48,108,0.07) !important; }
.dataTables_wrapper, .dataTables_wrapper label,
.dataTables_wrapper .dataTables_info { color: var(--muted) !important; }
.dataTables_wrapper input, .dataTables_wrapper select {
  background: rgba(255,255,255,0.05) !important;
  border: 1px solid var(--border) !important;
  color: var(--text) !important; border-radius: 6px;
}
.dataTables_wrapper .paginate_button.current {
  background: var(--accent) !important; color: white !important;
  border-radius: 6px; border: none !important;
}

/* ── Scrollbar ── */
::-webkit-scrollbar { width: 5px; height: 5px; }
::-webkit-scrollbar-track { background: var(--bg); }
::-webkit-scrollbar-thumb { background: rgba(255,255,255,0.1); border-radius: 3px; }
"

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bg = "#0D0D0D", fg = "#F1F5F9",
    primary = "#E1306C",
    base_font    = font_google("Inter"),
    heading_font = font_google("Space Grotesk")
  ),
  tags$head(
    tags$style(HTML(custom_css)),
    tags$link(rel = "stylesheet",
              href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.0/css/all.min.css")
  ),
  
  # Hero
  div(class = "dash-hero",
      h1(HTML('Social <span class="accent-word">Media</span> & Student Wellbeing')),
      p("Exploring addiction patterns, academic impact, and mental health across the globe"),
      div(class = "platform-tags",
          span(class="platform-tag", style="background:rgba(24,119,242,0.18); color:#1877F2;",
               tags$i(class="fa-brands fa-facebook me-1"), "Facebook"),
          span(class="platform-tag", style="background:rgba(225,48,108,0.18); color:#E1306C;",
               tags$i(class="fa-brands fa-instagram me-1"), "Instagram"),
          span(class="platform-tag", style="background:rgba(238,29,82,0.18); color:#EE1D52;",
               tags$i(class="fa-brands fa-tiktok me-1"), "TikTok"),
          span(class="platform-tag", style="background:rgba(255,0,0,0.18); color:#FF6B6B;",
               tags$i(class="fa-brands fa-youtube me-1"), "YouTube"),
          span(class="platform-tag", style="background:rgba(29,161,242,0.18); color:#1DA1F2;",
               tags$i(class="fa-brands fa-twitter me-1"), "Twitter"),
          span(class="platform-tag", style="background:rgba(37,211,102,0.18); color:#25D366;",
               tags$i(class="fa-brands fa-whatsapp me-1"), "WhatsApp")
      )
  ),
  
  navset_tab(
    
    # ══ TAB 1: GLOBAL OVERVIEW ══════════════════════════════════════════
    nav_panel(
      title = tagList(tags$i(class="fa-solid fa-globe me-1"), "Global Overview"),
      
      layout_sidebar(
        sidebar = sidebar(
          h5("Filters"),
          radioButtons("f_gender", "Gender",
                       choices = c("All","Male","Female"), selected = "All"),
          div(class = "filter-divider"),
          sliderInput("f_age", "Age Range",
                      min = AGE_MIN, max = AGE_MAX, value = c(AGE_MIN, AGE_MAX), step = 1),
          div(class = "filter-divider"),
          selectInput("f_level", "Academic Level",
                      choices = c("All","Undergraduate","Graduate"), selected = "All"),
          div(class = "filter-divider"),
          selectizeInput("f_platform", "Platform(s)",
                         choices = PLATFORMS, selected = NULL, multiple = TRUE,
                         options = list(placeholder = "All platforms")),
          width = 250, bg = "#1A1A2E"
        ),
        
        # KPI tiles
        layout_columns(
          value_box(
            title    = "Total Students",
            value    = textOutput("tile_students", inline = TRUE),
            showcase = tags$span(style = "font-size:1.9rem; background:linear-gradient(135deg,#E1306C,#F77737); -webkit-background-clip:text; -webkit-text-fill-color:transparent;",
                                 tags$i(class = "fa-solid fa-graduation-cap")),
            theme = value_box_theme(bg = "#1A1A2E", fg = "#F1F5F9"),
            height = 115
          ),
          value_box(
            title    = "Avg Daily Usage",
            value    = textOutput("tile_usage", inline = TRUE),
            showcase = tags$span(style = "font-size:1.9rem; color:#1DA1F2;",
                                 tags$i(class = "fa-solid fa-mobile-screen")),
            theme = value_box_theme(bg = "#1A1A2E", fg = "#F1F5F9"),
            height = 115
          ),
          value_box(
            title    = "Avg Sleep",
            value    = textOutput("tile_sleep", inline = TRUE),
            showcase = tags$span(style = "font-size:1.9rem; color:#69C9D0;",
                                 tags$i(class = "fa-solid fa-bed")),
            theme = value_box_theme(bg = "#1A1A2E", fg = "#F1F5F9"),
            height = 115
          ),
          value_box(
            title    = "Avg Addiction Score",
            value    = textOutput("tile_addiction", inline = TRUE),
            showcase = tags$span(style = "font-size:1.9rem; color:#EE1D52;",
                                 tags$i(class = "fa-solid fa-circle-exclamation")),
            theme = value_box_theme(bg = "#1A1A2E", fg = "#F1F5F9"),
            height = 115
          ),
          col_widths = c(3,3,3,3), fill = FALSE
        ),
        
        br(),
        
        layout_columns(
          card(
            card_header(tagList(tags$i(class="fa-solid fa-chart-bar me-2"), "Academic Performance Impact")),
            plotlyOutput("plot_aap", height = "270px")
          ),
          card(
            card_header(tagList(tags$i(class="fa-solid fa-users me-2"), "Gender × Academic Level")),
            plotlyOutput("plot_gender_level", height = "270px")
          ),
          card(
            card_header(tagList(tags$i(class="fa-solid fa-chart-pie me-2"), "Platform Share")),
            plotlyOutput("plot_platform_pie", height = "270px")
          ),
          col_widths = c(4,4,4)
        ),
        
        br(),
        
        # Equal width + equal explicit height on both containers
        layout_columns(
          card(
            card_header(tagList(tags$i(class="fa-solid fa-brain me-2"), "Addiction vs Mental Health  •  coloured by Sleep")),
            plotlyOutput("scatter_main", height = "300px")
          ),
          card(
            card_header(tagList(tags$i(class="fa-solid fa-clock me-2"), "Daily Usage Distribution by Platform")),
            plotlyOutput("plot_usage_box", height = "300px")
          ),
          col_widths = c(6,6)
        )
      )
    ),
    
    # ══ TAB 2: COUNTRY STORY ════════════════════════════════════════════
    nav_panel(
      title = tagList(tags$i(class="fa-solid fa-flag me-1"), "Country Story"),
      
      layout_sidebar(
        sidebar = sidebar(
          h5("Select Country"),
          selectInput("s_country", label = NULL,
                      choices = COUNTRIES, selected = COUNTRIES[1]),
          hr(style = "border-color:rgba(255,255,255,0.08);"),
          p("Dive into a single country's social media profile — compare it against the global average.",
            style = "font-size:0.79rem; color:#94A3B8; line-height:1.6;"),
          width = 240, bg = "#1A1A2E"
        ),
        
        uiOutput("country_headline"),
        br(),
        
        layout_columns(
          card(card_header("Platform Preference"),
               plotlyOutput("country_platform", height = "270px")),
          card(card_header("Addiction Score  •  vs Global"),
               plotlyOutput("country_addiction_hist", height = "270px")),
          col_widths = c(6,6)
        ),
        br(),
        layout_columns(
          card(card_header("Radar: Country vs Global Average"),
               plotlyOutput("country_radar", height = "300px")),
          card(card_header("Academic Impact by Gender"),
               plotlyOutput("country_impact_gender", height = "300px")),
          col_widths = c(6,6)
        )
      )
    ),
    
    # ══ TAB 3: DATA EXPLORER ════════════════════════════════════════════
    nav_panel(
      title = tagList(tags$i(class="fa-solid fa-table me-1"), "Data Explorer"),
      br(),
      card(card_header("Filtered Dataset"), DTOutput("data_table"))
    )
  )
)

# ── SERVER ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  # ── Reactive filtered data ───────────────────────────────────────────
  filtered <- reactive({
    d <- df
    if (input$f_gender != "All") d <- d[d$Gender == input$f_gender, ]
    d <- d[d$Age >= input$f_age[1] & d$Age <= input$f_age[2], ]
    if (input$f_level != "All") d <- d[d$Academic_Level == input$f_level, ]
    if (length(input$f_platform) > 0) d <- d[d$Most_Used_Platform %in% input$f_platform, ]
    d
  })
  
  # ── KPI tiles ─────────────────────────────────────────────────────────
  # Use isolate-safe pattern: render a character string directly
  output$tile_students <- renderText({
    format(nrow(filtered()), big.mark = ",")
  })
  output$tile_usage <- renderText({
    d <- filtered()
    if (nrow(d) == 0) return("—")
    paste0(round(mean(d$Avg_Daily_Usage_Hours, na.rm = TRUE), 1), "h")
  })
  output$tile_sleep <- renderText({
    d <- filtered()
    if (nrow(d) == 0) return("—")
    paste0(round(mean(d$Sleep_Hours_Per_Night, na.rm = TRUE), 1), "h")
  })
  output$tile_addiction <- renderText({
    d <- filtered()
    if (nrow(d) == 0) return("—")
    as.character(round(mean(d$Addicted_Score, na.rm = TRUE), 1))
  })
  
  # Helper: shared dark Plotly layout pieces
  dl <- function(fig, extra_x = list(), extra_y = list(), extra = list()) {
    base_axis <- list(
      gridcolor   = "rgba(255,255,255,0.06)",
      zerolinecolor = "rgba(255,255,255,0.1)",
      tickfont    = list(color = "#94A3B8"),
      titlefont   = list(color = "#F1F5F9")
    )
    args <- c(
      list(p = fig),
      list(
        plot_bgcolor  = "transparent",
        paper_bgcolor = "transparent",
        font   = list(family = "Inter, sans-serif", color = "#94A3B8", size = 11),
        xaxis  = modifyList(base_axis, extra_x),
        yaxis  = modifyList(base_axis, extra_y),
        margin = list(l = 10, r = 10, t = 10, b = 50),
        legend = list(bgcolor = "rgba(26,26,46,0.85)", bordercolor = "rgba(255,255,255,0.08)",
                      borderwidth = 1, font = list(color = "#F1F5F9"))
      ),
      extra
    )
    do.call(layout, args)
  }
  
  # ── Academic Performance bar ─────────────────────────────────────────
  output$plot_aap <- renderPlotly({
    d <- filtered(); req(nrow(d) > 0)
    pct <- d %>% count(Affects_Academic_Performance) %>%
      mutate(pct = round(n / sum(n) * 100, 1))
    fig <- plot_ly(pct,
                   y = ~Affects_Academic_Performance, x = ~pct,
                   type = "bar", orientation = "h",
                   color = ~Affects_Academic_Performance,
                   colors = c("No" = "#0077B5", "Yes" = "#EE1D52"),
                   text = ~paste0(pct, "%"), textposition = "outside",
                   hovertemplate = "<b>%{y}</b>: %{x:.1f}%<extra></extra>"
    )
    dl(fig, extra_x = list(title = "% of Students", ticksuffix = "%", range = c(0, 110)),
       extra_y = list(title = ""),
       extra   = list(showlegend = FALSE))
  })
  
  # ── Gender × Level ───────────────────────────────────────────────────
  output$plot_gender_level <- renderPlotly({
    d <- filtered(); req(nrow(d) > 0)
    gd <- d %>% count(Academic_Level, Gender)
    fig <- plot_ly(gd, x = ~Academic_Level, y = ~n, color = ~Gender, type = "bar",
                   colors = c("Female" = "#E1306C", "Male" = "#1877F2"),
                   hovertemplate = "<b>%{x}</b> — %{data.name}<br>%{y} students<extra></extra>")
    dl(fig,
       extra_x = list(title = ""),
       extra_y = list(title = "Count"),
       extra   = list(barmode = "group",
                      legend  = list(orientation = "h", y = -0.22,
                                     bgcolor = "rgba(26,26,46,0.85)",
                                     font = list(color = "#F1F5F9"))))
  })
  
  # ── Platform donut with brand colours ───────────────────────────────
  output$plot_platform_pie <- renderPlotly({
    d <- filtered(); req(nrow(d) > 0)
    plat <- d %>% count(Most_Used_Platform) %>% arrange(desc(n))
    cols <- sapply(plat$Most_Used_Platform, get_platform_color)
    plot_ly(plat, labels = ~Most_Used_Platform, values = ~n,
            type = "pie", hole = 0.46,
            marker = list(colors = cols, line = list(color = "#0D0D0D", width = 2)),
            textinfo = "label+percent",
            textfont = list(color = "#ffffff", size = 10),
            hovertemplate = "<b>%{label}</b><br>%{value} students (%{percent})<extra></extra>"
    ) %>% layout(
      showlegend = FALSE,
      plot_bgcolor = "transparent", paper_bgcolor = "transparent",
      font = list(family = "Inter", color = "#94A3B8"),
      margin = list(l = 10, r = 10, t = 10, b = 10)
    )
  })
  
  # ── Scatter: Addiction vs Mental Health ──────────────────────────────
  output$scatter_main <- renderPlotly({
    d <- filtered(); req(nrow(d) > 0)
    set.seed(1)
    d$jx <- d$Addicted_Score      + runif(nrow(d), -0.2, 0.2)
    d$jy <- d$Mental_Health_Score + runif(nrow(d), -0.2, 0.2)
    fig <- plot_ly(d, x = ~jx, y = ~jy,
                   type = "scatter", mode = "markers",
                   marker = list(
                     size = 7, opacity = 0.72,
                     color = ~Sleep_Hours_Per_Night,
                     colorscale = list(c(0,"#EE1D52"), c(0.5,"#0077B5"), c(1,"#69C9D0")),
                     colorbar = list(title = "Sleep (h)",
                                     titlefont = list(color="#94A3B8"),
                                     tickfont  = list(color="#94A3B8"),
                                     bgcolor   = "rgba(26,26,46,0.85)",
                                     bordercolor = "rgba(255,255,255,0.08)", len = 0.6),
                     line = list(width = 0)
                   ),
                   text = ~paste0("Addiction: ", Addicted_Score,
                                  "<br>Mental health: ", Mental_Health_Score,
                                  "<br>Sleep: ", Sleep_Hours_Per_Night, "h",
                                  "<br>Platform: ", Most_Used_Platform,
                                  "<br>Country: ", Country),
                   hovertemplate = "%{text}<extra></extra>"
    )
    dl(fig,
       extra_x = list(title = "Addiction Score"),
       extra_y = list(title = "Mental Health Score"))
  })
  
  # ── Box: Daily usage by platform with brand colours ──────────────────
  output$plot_usage_box <- renderPlotly({
    d <- filtered(); req(nrow(d) > 0)
    ord <- d %>% group_by(Most_Used_Platform) %>%
      summarise(med = median(Avg_Daily_Usage_Hours, na.rm = TRUE)) %>%
      arrange(desc(med)) %>% pull(Most_Used_Platform)
    d$Most_Used_Platform <- factor(d$Most_Used_Platform, levels = ord)
    
    fig <- plot_ly()
    for (p in ord) {
      col <- get_platform_color(p)
      sub <- d[d$Most_Used_Platform == p, ]
      fig <- fig %>% add_trace(
        data = sub, x = ~Most_Used_Platform, y = ~Avg_Daily_Usage_Hours,
        type = "box", name = p,
        fillcolor = paste0(substr(col, 1, 7), "55"),
        line   = list(color = col),
        marker = list(color = col, size = 3, opacity = 0.5),
        hovertemplate = paste0("<b>", p, "</b><br>%{y:.1f}h<extra></extra>")
      )
    }
    fig %>% layout(
      xaxis = list(title = "", tickangle = -30, categoryorder = "array",
                   categoryarray = ord, tickfont = list(color="#94A3B8"),
                   gridcolor = "rgba(255,255,255,0.06)"),
      yaxis = list(title = "Daily Usage (h)", tickfont = list(color="#94A3B8"),
                   titlefont = list(color="#F1F5F9"),
                   gridcolor = "rgba(255,255,255,0.06)"),
      plot_bgcolor  = "transparent",
      paper_bgcolor = "transparent",
      showlegend = FALSE,
      font   = list(family = "Inter", color = "#94A3B8"),
      margin = list(l = 10, r = 10, t = 10, b = 70)
    )
  })
  
  # ══ COUNTRY STORY ═══════════════════════════════════════════════════
  
  country_data <- reactive({ df[df$Country == input$s_country, ] })
  
  output$country_headline <- renderUI({
    cd <- country_data(); all <- df; req(nrow(cd) > 0)
    avg_add   <- round(mean(cd$Addicted_Score,        na.rm=TRUE), 1)
    avg_sleep <- round(mean(cd$Sleep_Hours_Per_Night, na.rm=TRUE), 1)
    avg_use   <- round(mean(cd$Avg_Daily_Usage_Hours, na.rm=TRUE), 1)
    pct_yes   <- round(mean(cd$Affects_Academic_Performance=="Yes", na.rm=TRUE)*100)
    glob_add   <- round(mean(all$Addicted_Score,        na.rm=TRUE), 1)
    glob_sleep <- round(mean(all$Sleep_Hours_Per_Night, na.rm=TRUE), 1)
    glob_use   <- round(mean(all$Avg_Daily_Usage_Hours, na.rm=TRUE), 1)
    glob_yes   <- round(mean(all$Affects_Academic_Performance=="Yes", na.rm=TRUE)*100)
    
    delta_tag <- function(val, glob, high_bad = TRUE) {
      diff <- round(val - glob, 1)
      if (abs(diff) < 0.1) return(span(style="color:#94A3B8; font-size:0.73rem;", "≈ avg"))
      bad <- (diff > 0) == high_bad
      col <- if (bad) "#EE1D52" else "#25D366"
      sym <- if (diff > 0) "▲" else "▼"
      span(style=paste0("color:",col,"; font-size:0.73rem; font-weight:600;"),
           paste0(sym, " ", abs(diff), " vs global"))
    }
    div(
      div(class="country-story-header", input$s_country),
      div(class="country-story-sub", paste0(nrow(cd), " students in the dataset")),
      div(
        span(class="stat-pill", span(class="pill-val", avg_add),   " Addiction  ", delta_tag(avg_add, glob_add, TRUE)),
        span(class="stat-pill", span(class="pill-val", avg_sleep), " Sleep hrs  ", delta_tag(avg_sleep, glob_sleep, FALSE)),
        span(class="stat-pill", span(class="pill-val", avg_use),   " Daily use  ", delta_tag(avg_use, glob_use, TRUE)),
        span(class="stat-pill", span(class="pill-val", paste0(pct_yes,"%")), " Acad. affected  ",
             delta_tag(pct_yes, glob_yes, TRUE))
      )
    )
  })
  
  output$country_platform <- renderPlotly({
    cd <- country_data(); req(nrow(cd) > 0)
    plat <- cd %>% count(Most_Used_Platform) %>% arrange(n)
    cols <- sapply(plat$Most_Used_Platform, get_platform_color)
    plot_ly(plat, y = ~Most_Used_Platform, x = ~n, type = "bar", orientation = "h",
            marker = list(color = cols, line = list(color = "rgba(0,0,0,0.3)", width = 1)),
            hovertemplate = "<b>%{y}</b>: %{x} students<extra></extra>"
    ) %>% layout(
      xaxis = list(title="Students", tickfont=list(color="#94A3B8"), gridcolor="rgba(255,255,255,0.06)"),
      yaxis = list(title="", tickfont=list(color="#94A3B8")),
      plot_bgcolor="transparent", paper_bgcolor="transparent",
      font=list(family="Inter",color="#94A3B8"), margin=list(l=10,r=10,t=10,b=40)
    )
  })
  
  output$country_addiction_hist <- renderPlotly({
    cd <- country_data(); all <- df; req(nrow(cd) > 0)
    plot_ly() %>%
      add_histogram(data=all, x=~Addicted_Score, name="Global", nbinsx=20,
                    marker=list(color="rgba(148,163,184,0.22)", line=list(color="rgba(148,163,184,0.4)",width=0.5))) %>%
      add_histogram(data=cd, x=~Addicted_Score, name=input$s_country, nbinsx=20,
                    marker=list(color="rgba(238,29,82,0.65)", line=list(color="#EE1D52",width=0.8))) %>%
      layout(barmode="overlay",
             xaxis=list(title="Addiction Score", tickfont=list(color="#94A3B8"), gridcolor="rgba(255,255,255,0.06)"),
             yaxis=list(title="Count", tickfont=list(color="#94A3B8"), gridcolor="rgba(255,255,255,0.06)"),
             legend=list(orientation="h", y=-0.2, bgcolor="rgba(26,26,46,0.85)", font=list(color="#F1F5F9")),
             plot_bgcolor="transparent", paper_bgcolor="transparent",
             font=list(family="Inter",color="#94A3B8"), margin=list(l=10,r=10,t=10,b=60)
      )
  })
  
  output$country_radar <- renderPlotly({
    cd <- country_data(); all <- df; req(nrow(cd) > 0)
    metrics <- c("Addiction","Daily Use","Sleep Quality","Mental Health")
    norm <- function(x,lo,hi) pmax(0,pmin(10,(x-lo)/(hi-lo)*10))
    cvals <- c(norm(mean(cd$Addicted_Score,na.rm=T),1,10),
               norm(mean(cd$Avg_Daily_Usage_Hours,na.rm=T),1,12),
               norm(mean(cd$Sleep_Hours_Per_Night,na.rm=T),3,10),
               norm(mean(cd$Mental_Health_Score,na.rm=T),1,10))
    gvals <- c(norm(mean(all$Addicted_Score,na.rm=T),1,10),
               norm(mean(all$Avg_Daily_Usage_Hours,na.rm=T),1,12),
               norm(mean(all$Sleep_Hours_Per_Night,na.rm=T),3,10),
               norm(mean(all$Mental_Health_Score,na.rm=T),1,10))
    plot_ly(type="scatterpolar", fill="toself") %>%
      add_trace(r=c(gvals,gvals[1]), theta=c(metrics,metrics[1]),
                name="Global Avg", line=list(color="#94A3B8",width=1.5),
                fillcolor="rgba(148,163,184,0.12)") %>%
      add_trace(r=c(cvals,cvals[1]), theta=c(metrics,metrics[1]),
                name=input$s_country, line=list(color="#E1306C",width=2),
                fillcolor="rgba(225,48,108,0.2)") %>%
      layout(
        polar=list(bgcolor="transparent",
                   radialaxis=list(visible=TRUE, range=c(0,10),
                                   tickfont=list(size=8,color="#94A3B8"),
                                   gridcolor="rgba(255,255,255,0.08)", linecolor="rgba(255,255,255,0.08)"),
                   angularaxis=list(tickfont=list(size=11,color="#F1F5F9"),
                                    linecolor="rgba(255,255,255,0.12)")),
        legend=list(orientation="h", y=-0.14,
                    bgcolor="rgba(26,26,46,0.85)", font=list(color="#F1F5F9")),
        plot_bgcolor="transparent", paper_bgcolor="transparent",
        font=list(family="Inter",color="#94A3B8"),
        margin=list(l=40,r=40,t=20,b=60)
      )
  })
  
  output$country_impact_gender <- renderPlotly({
    cd <- country_data(); req(nrow(cd) > 0)
    gd <- cd %>% count(Gender, Affects_Academic_Performance) %>%
      group_by(Gender) %>% mutate(pct=round(n/sum(n)*100,1)) %>% ungroup()
    plot_ly(gd, x=~Gender, y=~pct, color=~Affects_Academic_Performance, type="bar",
            colors=c("No"="#0077B5","Yes"="#EE1D52"),
            text=~paste0(pct,"%"), textposition="inside",
            insidetextfont=list(color="#ffffff"),
            hovertemplate="<b>%{x}</b> — %{data.name}<br>%{y:.1f}%<extra></extra>"
    ) %>% layout(
      barmode="stack",
      xaxis=list(title="", tickfont=list(color="#94A3B8")),
      yaxis=list(title="% Students", ticksuffix="%",
                 tickfont=list(color="#94A3B8"), gridcolor="rgba(255,255,255,0.06)"),
      legend=list(title=list(text="Affected?",font=list(color="#F1F5F9")),
                  orientation="h", y=-0.2, bgcolor="rgba(26,26,46,0.85)", font=list(color="#F1F5F9")),
      plot_bgcolor="transparent", paper_bgcolor="transparent",
      font=list(family="Inter",color="#94A3B8"), margin=list(l=10,r=10,t=10,b=60)
    )
  })
  
  # ── Data table ────────────────────────────────────────────────────────
  output$data_table <- renderDT({
    datatable(filtered(), options = list(
      pageLength = 15, scrollX = TRUE, dom = "frtip",
      columnDefs = list(list(className = "dt-center", targets = "_all"))
    ), class = "stripe hover", rownames = FALSE)
  })
}

shinyApp(ui, server)