ui = dashboardPage(
  dashboardHeader(title = "PhenotypeR"),
  ## menu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Code counts",
        tabName = "code_counts"
      ),
      menuItem(
        text = "Cohort overlap",
        tabName = "overlap"
      )
    )
  ),

  # ## body ----
  dashboardBody(
    use_theme(DUtheme),
    tabItems(
      # code_counts ----
      tabItem(
        tabName = "code_counts"
      ),
      # Cohort overlap ----
      tabItem(
        tabName = "cohort_overlap"
      )
      # end ----
    )
  )
)
