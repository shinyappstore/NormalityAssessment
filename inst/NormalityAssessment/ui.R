
library(shiny)
library(shinyalert)
library(shinyBS)
library(DT)
library(stringi)


navbarPage("Normality Assessment",


  ##############################################################################
  ################################ Home tab panel ##############################
  ##############################################################################

  tabPanel("Home", icon = icon("home"),

    # remove progress bar for fileInput()
    tags$style(".shiny-file-input-progress {display: none}"),


    column(2, p("")),

    column(8,
      h1(style = "color: green; font-weight: bold; text-align: center;",
        "Suggested steps:"
      ),
      br(),

      HTML("<h3>(1) Go to the <span style = 'color: green; font-weight: bold;'>
        Explore Simulated Data</span> tab."
      ),
      br(),

      h3(style = "margin-left: 1.5em;", "The implemented procedure is aimed at
        helping you develop a better understanding of the natural variability in
        data that is randomly generated from a particular distribution."
      ),

      br(),

      # <span style = 'text-indent: -36px; padding-left: 36px;'>

      HTML("<h3>(2) Continue to the <span style = 'color: green; font-weight: bold;'>
        Include Your Data</span> tab, if you have your own data."
      ),

      h3(style = "margin-left: 1.5em;", "The implemented procedure is aimed at
        helping you visually test whether your data plausibly come from a
        normally-distributed population."
      )
    ),

    column(2, p(""))
  ),




  ##############################################################################
  ###################### Explore Simulated Data tab panel ######################
  ##############################################################################

  tabPanel("Explore Simulated Data", icon = icon("bolt"),

    # tweak style of popup modals
    tags$head(tags$style(HTML(".modal-lg {width: 95%; margin-top: 0.1%}"))),
    tags$head(tags$style(HTML(".modal-footer {display: none}"))),


    # well panel -- inputs
    column(3,
      div(style = "margin-top: -2em;"),
      h3(style = "color: red; font-weight: bold;", "Step 1"),
      p(style = "color: red; font-weight: bold;", "Specify the inputs below, and
        then click the 'Make Plots' button to make a grid of plots."
      ),

      wellPanel(
        radioButtons("pop_dist", "Distribution of Population",
          choices = c(
            "Basic: Select Shape" = "basic",
            "Advanced: Select Distribution" = "advanced"
          )
        ),

        conditionalPanel(
          condition = "input.pop_dist == 'basic'",

          selectInput("pop_shape", "Shape",
            choices = c("Severely Left Skewed", "Very Left Skewed",
              "Left Skewed", "Normal", "Right Skewed", "Very Right Skewed",
              "Severely Right Skewed"
            ),
            selected = "Normal"
          )
        ),

        conditionalPanel(
          condition = "input.pop_dist == 'advanced'",

          selectInput("pop_fam", "Family",
            choices = c("Normal", "t", "F", "Chi-square", "Uniform",
              "Exponential", "Gamma", "Beta"
            ),
            selected = "Normal"
          ),


          conditionalPanel(
            condition = "input.pop_fam == 'Normal'",

            splitLayout(
              numericInput("norm_mean", "Mean", value = 0),
              numericInput("norm_sd", "Std. Dev.", value = 1)
            )
          ),


          conditionalPanel(
            condition = "input.pop_fam == 't'",

            numericInput("t_df", "DF", value = 10, width = "50%")
          ),


          conditionalPanel(
            condition = "input.pop_fam == 'F'",

            splitLayout(
              numericInput("f_df1", "DF 1", value = 5),
              numericInput("f_df2", "DF 2", value = 5)
            )
          ),


          conditionalPanel(
            condition = "input.pop_fam == 'Chi-square'",

            numericInput("chisq_df", "DF", value = 5, width = "50%")
          ),


          conditionalPanel(
            condition = "input.pop_fam == 'Uniform'",

            splitLayout(
              numericInput("uni_min", "Minimum", value = 0),
              numericInput("uni_max", "Maximum", value = 1)
            )
          ),


          conditionalPanel(
            condition = "input.pop_fam == 'Exponential'",

            numericInput("exp_rate", "Rate", value = 5, width = "50%")
          ),


          conditionalPanel(
            condition = "input.pop_fam == 'Gamma'",

            splitLayout(
              numericInput("gamma_shape", "Shape", value = 5),
              numericInput("gamma_rate", "Rate", value = 5)
            )
          ),


          conditionalPanel(
            condition = "input.pop_fam == 'Beta'",

            splitLayout(
              numericInput("beta_shape1", "Shape 1", value = 5),
              numericInput("beta_shape2", "Shape 2", value = 5)
            )
          )
        ),


        splitLayout(
          numericInput("sample_size", "Sample Size", value = 25),
          numericInput("n_plots_rorsch", "Number of Plots", value = 20)
        ),


        radioButtons("rorsch_plot_type", "Plot Type",
          choices = c(
            "Normal QQ Plot" = "rorsch_qq",
            "Histogram" = "rorsch_hist",
            "Both" = "rorsch_qq_and_hist"
          )
        ),


        column(12, align = "center",
          actionButton("make_rorsch_plots", "Make Plots",
            class = "btn btn-primary"
          )
        ),

        br()
      )
    ),


    # directions and plots
    column(9,
      column(6,
        div(style = "margin-top: -2em;"),
        h3(style = "color: red; font-weight: bold;", "Step 2"),

        p(style = "color: red; font-weight: bold;", "Look for any expected and
          unexpected characteristics in the plots below. This will help develop a
          better understanding of the natural variability in data that is
          randomly generated from a particular distribution."
        )
      ),


      column(4,
        div(style = "margin-top: -2em;"),
        h3(style = "color: red; font-weight: bold;", "Step 3 (optional)"),

        p(style = "color: red; font-weight: bold;", "If you have your own data to
          include, continue to the 'Include Your Data' tab."
        )
      ),


      # button for displaying larger plots in popup window
      column(2, align = "center",

        # QQ plots or histograms, but not both
        conditionalPanel(
          condition = "input.rorsch_plot_type == 'rorsch_qq' |
            input.rorsch_plot_type == 'rorsch_hist'",

          actionButton("make_rorsch_popup_single", HTML("View Larger<br>Plots"),
            class = "btn btn-success"
          ),

          bsModal("rorsch_popup_ui_single", NULL, "make_rorsch_popup_single",
            size = "large", uiOutput("rorsch_popup_ui_single")
          )
        ),


        # QQ plots and histograms
        conditionalPanel(
          condition = "input.rorsch_plot_type == 'rorsch_qq_and_hist'",

          actionButton("make_rorsch_popup_both", HTML("View<br>Larger"),
            class = "btn btn-success"
          ),

          bsModal("rorsch_popup_ui_both", NULL, "make_rorsch_popup_both",
            size = "large", uiOutput("rorsch_popup_ui_both")
          )
        )
      ),


      column(12,
        # either QQ plots or histograms
        conditionalPanel(
          condition = "input.rorsch_plot_type == 'rorsch_qq' |
            input.rorsch_plot_type == 'rorsch_hist'",

          plotOutput("rorsch_plot_single"),


          conditionalPanel(
            condition = "input.rorsch_plot_type == 'rorsch_hist'",

            column(12,
              sliderInput("hist_bins_rorsch", "Number of Bins", min = 5,
                max = 50, value = 15, width = "50%"
              )
            )
          )
        ),


        # QQ plots and histograms
        conditionalPanel(
          condition = "input.rorsch_plot_type == 'rorsch_qq_and_hist'",

          column(6, plotOutput("rorsch_plot_both_qq", height = 450)),
          column(6, plotOutput("rorsch_plot_both_hist", height = 450))
        )
      ),


      # display plots in main window
      column(12,
        conditionalPanel(
          condition = "input.rorsch_plot_type == 'rorsch_qq_and_hist'",

          column(4, offset = 8,
            sliderInput("hist_bins_rorsch_both", "Number of Bins", min = 5,
              max = 50, value = 15, width = "75%"
            )
          )
        )
      )
    )
  ),




  ##############################################################################
  ######################### Include Your Data tab panel ########################
  ##############################################################################

  tabPanel("Include Your Data", icon = icon("table"),

    tabsetPanel(type = "tabs",

      # Input Data and Users tab panel
      tabPanel("Input Data and Users",

        br(),


        # well panel -- inputs
        column(3,
          div(style = "margin-top: -2em;"),
          h3(style = "color: red; font-weight: bold;", "Step 1"),
          p(style = "color: red; font-weight: bold;", "Upload a data set or
            manually input your data. If uploading, make sure to click the
            'Upload Data Set' button."
          ),

          wellPanel(
            radioButtons("input_type", "Data input method",
              choices = c("Upload File" = "upload", "Manually" = "manually"),
              selected = "upload"
            ),

            tags$hr(),

            conditionalPanel(
              condition = "input.input_type == 'manually'",

              textAreaInput("user_data", "Either copy and paste a column from a
                spreadsheet (such as Excel) or type values in the box below,
                separating each by a comma.",
                placeholder = "0, 2, 8"
              )
            ),

            conditionalPanel(
              condition = "input.input_type == 'upload'",

              checkboxInput("view_file_types", strong(style = "color: green;",
                "Check box to view a list of compatible file types.")
              ),

              conditionalPanel(
                condition = "input.view_file_types",

                p("The app accepts files with the following extensions when
                  uploading data: .csv (CSV), .xlsx (Excel), .xls (Excel), .json
                  (JSON), .mtp (Minitab), .mat (MATLAB), .R (R), .RData (R),
                  .rda (R), .txt (Plain Text), .sas7bdat (SAS), .sav (SPSS), and
                  .dta (Stata)."
                ),
              ),


              fileInput("data_file", "Click 'Browse...' and select the desired
                file.",
                multiple = FALSE
              ),


              conditionalPanel(
                condition = "output.na_input_status",    # from server

                checkboxInput("no_header", strong(style = "color: green;",
                  "Check box if the data set does NOT contain variable/column
                  names in the first row.")
                ),

                checkboxInput("missing_input", strong(style = "color: green;",
                  "Check box if the data set contains missing data."),
                  value = FALSE
                ),

                conditionalPanel(
                  condition = "input.missing_input",

                  p("If the missing values in your data set are represented by
                    empty cells, you may uncheck the box. If they are represented
                    by something else (e.g., a period), specify the symbol,
                    character, etc. that represents the missing values. If there
                    are multiple, separate each by a comma. Any entries with
                    missing data will appears as empty cells."
                  ),

                  textInput("denote_missing_input", NULL, value = ".",
                    width = "25%"
                  )
                )
              ),


              column(12, align = "center",
                actionButton("upload_dataset", "Upload Data Set",
                  class = "btn btn-primary"
                )
              ),

              br()
            )
          )
        ),


        # display data set
        column(7,
          conditionalPanel(
            condition = "input.input_type == 'upload' & input.upload_dataset",

            column(12, align = "center",

              h4(style = "color: green; font-weight: bold;", "Only variables with
                numerical data are displayed below."
              ),

              br()
            ),


            DT::DTOutput("data_table_upload")
          ),

          conditionalPanel(
            condition = "input.input_type == 'manually'",

            tableOutput("table_manual")
          )
        ),

        column(2,
          div(style = "margin-top: -2em;"),
          h3(style = "color: red; font-weight: bold;", "Step 2"),
          p(style = "color: red; font-weight: bold;", "Specify the number of
            users."
          ),

          radioButtons("n_users_radio", "Number of Users",
            choices = c(
              "One (only me)" = "one",
              "Multiple (others and me)" = "multiple"
            )
          ),


          #div(style = "margin-top: -2em;"),
          h3(style = "color: red; font-weight: bold;", "Step 3"),
          p(style = "color: red; font-weight: bold;", "Continue to the 'Make
            Plots' tab."
          )
        )
      ),


      # Make Plots tabpanel
      tabPanel("Make Plots",

        br(),


        conditionalPanel(
          condition = "input.n_users_radio",

          # well panel -- inputs
          column(3,
            div(style = "margin-top: -2em;"),
            h3(style = "color: red; font-weight: bold;", "Step 1"),
            p(style = "color: red; font-weight: bold;", "Specify the inputs below,
              and then click the 'Make Plots' button."
            ),


            wellPanel(
              conditionalPanel(
                condition = "input.input_type == 'upload'",

                selectInput("select_var", "Variable of Interest",
                  choices = "", selected = ""
                )
              ),


              numericInput("n_plots_lineup", "Number of Plots", value = 20),
              tags$head(tags$style(
                type = "text/css", "#n_plots_lineup{width: 100px;}"
              )),


              radioButtons("lineup_plot_type", "Plot Type",
                choices = c(
                  "Normal QQ Plot" = "lineup_qq",
                  "Histogram" = "lineup_hist"
                )
              ),


              conditionalPanel(
                condition = "input.n_users_radio == 'multiple'",

                p("All users working together must use the same number below (any
                  whole number), which will ensure you all examine the same set of
                  plots. This value is the 'seed.' Please only click 'Make Plots'
                  once after inputting the number."
                ),

                numericInput("user_seed", NULL, value = 1, width = "100px")
              ),


              column(12, align = "center",
                actionButton("make_lineup_plots", "Make Plots",
                  class = "btn btn-primary"
                )
              ),

              br()
            )
          ),


          # directions and plots
          column(9,
            column(10,
              fluidRow(
                column(5,
                  div(style = "margin-top: -2em;"),

                  conditionalPanel(
                    condition = "input.n_users_radio == 'one'",

                    h3(style = "color: red; font-weight: bold;", "Step 2 (final
                      step)"
                    )
                  ),

                  conditionalPanel(
                    condition = "input.n_users_radio == 'multiple'",

                    h3(style = "color: red; font-weight: bold;", "Step 2")
                  ),


                  p(style = "color: red; font-weight: bold;", "Follow the
                    instructions below."
                  )
                ),

                conditionalPanel(
                  condition = "input.n_users_radio == 'multiple'",

                  column(7,
                    div(style = "margin-top: -2em;"),
                    h3(style = "color: red; font-weight: bold;", "Step 3"),
                    p(style = "color: red; font-weight: bold;", "Continue to the
                      'Multiple Users (cont.)' tab."
                    )
                  )
                )
              ),


              h5(style = "font-weight: bold;", "One of the plots below corresponds
                to the data for the variable you selected, while the remaining
                plots correspond to data sets randomly generated from the normal
                distribution with the same mean and SD as the data for the selected
                variable."
              ),

              h5(style = "font-weight: bold;", "Locate the plot that you believe
                corresponds to the data for the selected variable. Then click the
                'Identify My Plot' button to see if you were correct."
              ),


              conditionalPanel(
                condition = "input.n_users_radio == 'one'",

                h5(style = "color: mediumblue; font-weight: bold;", "\u2012 If you
                  were correct, then it is reasonable to conclude that data did not
                  come from a normally-distributed population."
                ),

                h5(style = "color: mediumblue; font-weight: bold;", "\u2012 If you
                  were not correct, then it is plausible that data came from a
                  normally-distributed population."
                )
              )
            ),

            column(2, align = "center",
              h4("My Plot:"),

              verbatimTextOutput("user_plot_number", placeholder = TRUE),
              tags$head(tags$style(type="text/css",
                "#user_plot_number{width: 60px;}"
              )),

              actionButton("identify", "Identify My Plot",
                class = "btn btn-primary", width = "135px"
              ),

              actionButton("make_lineup_popup", "View Larger Plots",
                class = "btn btn-success", width = "135px"
              ),

              bsModal("lineup_popup_ui", NULL, "make_lineup_popup",
                size = "large", uiOutput("lineup_popup_ui")
              )
            ),

            column(12,
              conditionalPanel(
                condition = "input.lineup_plot_type == 'lineup_qq' |
                  input.lineup_plot_type == 'lineup_hist'",

                plotOutput("lineup_plot"),


                conditionalPanel(
                  condition = "input.lineup_plot_type == 'lineup_hist'",

                  column(12, offset = 1,
                    sliderInput("hist_bins_lineup", "Number of Bins", min = 5,
                      max = 50, value = 15, width = "50%"
                    )
                  )
                )
              )
            )
          )
        )
      ),


      # Multiple Users tabpanel
      tabPanel("Multiple Users (cont.)",

        br(),

        conditionalPanel(
          condition = "input.n_users_radio != 'multiple'",

          br(),
          br(),

          column(3, p("")),

          column(6, align = "center",
            h3(style = "color: red; font-weight: bold;", "This tab only applies
              when the number of users specified in the 'Input Data and Users'
              tab is set to 'Multiple (others and me).'"
            )
          )
        ),


        conditionalPanel(
          condition = "input.n_users_radio == 'multiple'",

          # well panel -- inputs
          column(3,
            div(style = "margin-top: -2em;"),
            h3(style = "color: red; font-weight: bold;", "Step 1"),
            p(style = "color: red; font-weight: bold;", "Specify the inputs below,
              and then click the 'Calculate p-value' button."
            ),

            wellPanel(
              numericInput("n_users", "Total Number of Users", value = 1),
              tags$head(tags$style(type="text/css", "#n_users{width: 100px;}")),

              numericInput("n_correct", "Number of Users who Identified the Correct
                Plot", value = 0
              ),
              tags$head(tags$style(type="text/css", "#n_correct{width: 100px;}")),

              numericInput("n_plots_each", "Number of Plots Each User Viewed",
                value = 20
              ),
              tags$head(tags$style(type="text/css", "#n_plots_each{width: 100px;}")),


              column(12, align = "center",
                actionButton("calc_pvalue", "Calculate p-value",
                  class = "btn btn-primary",
                )
              ),

              br()
            )
          ),


          # output
          column(5,
            div(style = "margin-top: -2em;"),
            h3(style = "color: red; font-weight: bold;", "Step 2 (final step)"),
            p(style = "color: red; font-weight: bold;", "Compare the p-value
              below to your significance level to determine whether or not the
              data for the selected variable plausibly came from a
              normally-distributed population."
            ),

            br(),

            h5(style = "color: mediumblue; font-weight: bold;", "\u2012 If the
              p-value is less than the significance level, then it is reasonable
              to conclude the data did not come from a normally-distributed
              population."
            ),

            h5(style = "color: mediumblue; font-weight: bold;", "\u2012 If the
              p-value is greater than the significance level, then it is
              plausible the data came from a normally-distributed population."
            ),


            br(),


            fluidRow(
              column(12,
                div(
                  style = "display: inline-block; vertical-align: 1.75em;",

                  strong(paste0("Adjusted* p-value:", stri_dup(intToUtf8(160), 3)))
                ),

                div(
                  style = "display: inline-block;",

                  verbatimTextOutput("multiple_pvalue", placeholder = TRUE),

                  tags$head(tags$style(
                    type = "text/css", "#multiple_pvalue{width: 75px;}"
                  ))
                ),

                br(),

                br(),

                strong("*This p-value was correctly calculated (and adjusted
                  based on the number of users) if all users (1) independently
                  examined the (2) same set of plots (recall: using the same seed
                  was necessary to guarantee the same set of plots)."
                )
              )
            )
          )
        )
      )
    )
  ),




  ##############################################################################
  ############################## About tab panel ###############################
  ##############################################################################
  tabPanel("About", icon = icon("info-circle"),

    column(5,

      h3("The information below will be updated after the completion of the
        blind-review process for the associated article."
      ),

      br(),


      # information about the developers
      h4("Developers"),
      p("This tool was developed by Anonymous."),
      br(),

      # information about the app
      h4("About the App"),
      p("This tool creates normal quantile-quantile (QQ) plots and histograms for
        assessing normality. The methods implemented are based on recent
        developments made in graphical inference (see below for more). In the
        app, the features in the 'Explore Simulated Data' tab enable the user to
        run the Rorschach procedure, and those in the 'Include Your Data' tab
        allow the user to run the line-up procedure."
      ),
      br(),

      # references
      h4("References"),
      p("Buja, A., Cook, D., Hofmann, H., Lawrence, M., Lee, E. K., Swayne, D.
        F., & Wickham, H. (2009). Statistical inference for exploratory data
        analysis and model diagnostics. Philosophical Transactions of the Royal
        Society of London A: Mathematical, Physical and Engineering Sciences,
        367(1906), 4361-4383."
      ),

      p("Majumder, M., Hofmann, H., & Cook, D. (2013). Validation of visual
        statistical inference, applied to linear models. Journal of the American
        Statistical Association, 108(503), 942-956."
      ),

      p("Wickham, H., Cook, D., Hofmann, H., & Buja, A. (2010). Graphical
        inference for infovis. IEEE Transactions on Visualization and Computer
        Graphics, 16(6), 973-979."
      ),
      br(),

      # contact info
      h4("Contact"),
      p("Email: Anonymous"),
      br(),
      br(),

      # copyright statement
      p("Copyright \uA9 2019-2022 Anonymous."),
      p("The license statement can be found",
        a("here.", href = "https://choosealicense.com/licenses/mit/",
          target = "_blank"
        )
      )
    ),


    column(1, ""),


    column(4,

      br(),
      br(),
      br(),
      br(),

      img(src = "NormalityAssessment_Sticker_Image.png", height = "300px")
    )
  )
)



