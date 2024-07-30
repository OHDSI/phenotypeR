ui = dashboardPage(
  dashboardHeader(title = "PhenotypeR"),
  ## menu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "CodelistDiagnostics",
        tabName = "codelist_diagnostics",
        menuSubItem(
          text = "Code use",
          tabName = "code_use"
        ),
        menuSubItem(
          text = "Orphan concepts",
          tabName = "orphan_codes"
        )
      ),
      menuItem(
        text = "Cohort diagnostics",
        tabName = "cohort_diagnostics",
        menuSubItem(
          text = "Cohort counts",
          tabName = "cohort_counts"
        ),
        menuSubItem(
          text = "Index events",
          tabName = "index_events"
        ),
        menuSubItem(
          text = "Cohort attrition",
          tabName = "cohort_attrition"
        ),
        menuSubItem(
          text = "Cohort overlap",
          tabName = "cohort_overlap"
        ),
        menuSubItem(
          text = "Cohort timing",
          tabName = "cohort_timing"
        ),
        menuSubItem(
          text = "Incidence",
          tabName = "incidence"
        ),
        menuSubItem(
          text = "Prevalence",
          tabName = "prevalence"
        ),
        menuSubItem(
          text = "Cohort characteristics",
          tabName = "cohort_characteristics"
        ),
        menuSubItem(
          text = "Large scale characteristics",
          tabName = "lsc"
        )
      )
    )
  ),

  # ## body ----
  dashboardBody(
    tabItems(
      # code_counts ----
      tabItem(
        tabName = "code_use",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "achilles_cdm_name",
            label = "Database",
            choices = databases,
            selected = databases,
            options = list(`actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "achilles_codelist_name",
            label = "Codelist",
            choices = codelist_names,
            selected = codelist_names[1],
            options = list(`actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "achilles_codelist_domain",
            label = "Domain",
            choices = codelist_domains,
            selected = codelist_domains,
            options = list(`actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        prettySwitch(
          inputId = "achilles_subjects",
          label = "Number of subjects",
          fill = TRUE,
          value = TRUE
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
        prettySwitch(
          inputId = "achilles_records",
          label = "Number of records",
          fill = TRUE,
          value = TRUE
        )),

        radioGroupButtons(
          inputId = "achilles_table_type",
          label = "Table type",
          choices = c("tidy", "raw"),
          status = "primary"
        ),
        uiOutput("table_achilles_code_count") %>%
           withSpinner()
      ),

      # orphan codes ----
      tabItem(
        tabName = "orphan_codes",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "orphan_cdm_name",
            label = "Database",
            choices = databases,
            selected = databases,
            options = list(`actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "orphan_codelist_name",
            label = "Codelist",
            choices = codelist_names,
            selected = codelist_names[1],
            options = list(`actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "orphan_codelist_domain",
            label = "Domain",
            choices = codelist_domains,
            selected = codelist_domains,
            options = list(`actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        prettySwitch(
          inputId = "orphan_subjects",
          label = "Number of subjects",
          fill = TRUE,
          value = TRUE
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          prettySwitch(
            inputId = "orphan_records",
            label = "Number of records",
            fill = TRUE,
            value = TRUE
          )),

        radioGroupButtons(
          inputId = "orphan_table_type",
          label = "Table type",
          choices = c("tidy", "raw"),
          status = "primary"
        ),
        uiOutput("table_orphan_code_count") %>%
          withSpinner()
      ),
      # Cohort counts ----
      tabItem(
        tabName = "cohort_counts",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "cohort_count_cdm_name",
            label = "Database",
            choices = databases,
            selected = databases,
            options = list(`actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "cohort_count_cohort_name",
            label = "Cohort",
            choices = cohort_names,
            selected = cohort_names[1],
            options = list(`actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        prettySwitch(
          inputId = "cohort_count_subjects",
          label = "Number of subjects",
          fill = TRUE,
          value = TRUE
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          prettySwitch(
            inputId = "cohort_count_records",
            label = "Number of records",
            fill = TRUE,
            value = TRUE
          )),

        radioGroupButtons(
          inputId = "cohort_count_table_type",
          label = "Table type",
          choices = c("tidy", "raw"),
          status = "primary"
        ),
        uiOutput("table_cohort_count_code_count") %>%
          withSpinner()
      ),
      # code_counts ----
      tabItem(
        tabName = "index_events",
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "index_events_cdm_name",
            label = "Database",
            choices = databases,
            selected = databases,
            options = list(`actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "index_events_cohort_name",
            label = "Cohort",
            choices = cohort_names,
            selected = cohort_names,
            options = list(`actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "index_events_codelist_name",
            label = "Codelist",
            choices = codelist_names,
            selected = codelist_names,
            options = list(`actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        prettySwitch(
          inputId = "index_events_subjects",
          label = "Number of subjects",
          fill = TRUE,
          value = TRUE
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          prettySwitch(
            inputId = "index_events_records",
            label = "Number of records",
            fill = TRUE,
            value = TRUE
          )),

        radioGroupButtons(
          inputId = "index_events_table_type",
          label = "Table type",
          choices = c("tidy", "raw"),
          status = "primary"
        ),
        uiOutput("table_index_events_code_count") %>%
          withSpinner()
      ),
      # Cohort attrition ----
      tabItem(
        tabName = "cohort_attrition",
          div(
            style = "display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(
              inputId = "attrition_cohort_name",
              label = "Cohort",
              choices = cohort_names,
              selected = cohort_names[1],
              options = list(`actions-box` = TRUE,
                             size = 10,
                             `selected-text-format` = "count > 3"),
              multiple = TRUE
            )
          ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Table",
            gt_output("gt_cohort_attrition") %>%
              withSpinner()
            ),
          tabPanel(
            "Plot",
            grVizOutput("gg_cohort_attrition")
            )
        )
      ),
      # Cohort overlap ----
      tabItem(
        tabName = "cohort_overlap",
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Table",
        gt_output("gt_cohort_overlap") %>%
          withSpinner()),
        tabPanel(
          "Plot",
          plotlyOutput("gg_cohort_overlap") %>%
            withSpinner())
        )
      ),
      # Cohort timing ----
      tabItem(
        tabName = "cohort_timing",
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Table",
        gt_output("gt_cohort_timing") %>%
          withSpinner()),
        tabPanel(
          "Plot",
          plotlyOutput("gg_cohort_timing") %>%
            withSpinner())
        )
      )
      # end ----
    )
  )
)
