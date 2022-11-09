
library(shiny)
library(shinyalert)
library(ggplot2); theme_set(theme_grey(20))
library(dplyr)
library(stringr)
library(rio)
library(rmatio)  # use rmatio rather than rio when importing .mat (MATLAB) files


shinyServer(function(session, input, output){

  # stop server when pop-up window closed
  session$onSessionEnded(function(x) stopApp())


  # create and initialize reactive stuff
  bag <- reactiveValues(

    # reset identify my plot output
    reset_plot_num = 0,

    # reset plot output
    do_rorsch_single = FALSE,
    do_rorsch_both = FALSE,
    do_lineup = FALSE
  )


  # number of bins for histograms
  hist_bins_rorsch <- reactive(input$hist_bins_rorsch)
  hist_bins_rorsch_both <- reactive(input$hist_bins_rorsch_both)
  hist_bins_lineup <- reactive(input$hist_bins_lineup)
  d_hist_bins_rorsch <- debounce(hist_bins_rorsch, 1000)          # 1000 = 1 second delay
  d_hist_bins_rorsch_both <- debounce(hist_bins_rorsch_both, 1000)
  d_hist_bins_lineup <- debounce(hist_bins_lineup, 1000)




  ##############################################################################
  ############## Rorschach Procedure (Explore Simulated Data) Stuff ############
  ##############################################################################

  ##### rorschach plots
  #######################################################

  # generate data
  observeEvent(input$make_rorsch_plots, {

    if (input$pop_dist == "basic") {

      # pop. distribution to randomly sample from
      dist_random <- ifelse(input$pop_shape == "Normal", rnorm, rbeta)

      # parameters of pop. distribution
      params <- switch(input$pop_shape,
        "Normal" = c(0, 1),
        "Left Skewed" = c(5, 2),
        "Very Left Skewed" = c(10, 2),
        "Severely Left Skewed" = c(50, 2),
        "Right Skewed" = c(2, 5),
        "Very Right Skewed" = c(2, 10),
        "Severely Right Skewed" = c(2, 50)
      )
    } else {

      # pop. distribution to randomly sample from
      dist_random <- switch(input$pop_fam,
        "Normal" = rnorm, "t" = rt, "F" = rf, "Uniform" = runif,
        "Chi-square" = rchisq, "Exponential" = rexp, "Gamma" = rgamma,
        "Beta" = rbeta
      )

      # parameters of pop. distribution
      params <- switch(input$pop_fam,
        "Normal" = c(input$norm_mean, input$norm_sd),
        "t" = input$t_df,
        "F" = c(input$f_df1, input$f_df2),
        "Uniform" = c(input$uni_min, input$uni_max),
        "Chi-square" = input$chisq_df,
        "Exponential" = input$exp_rate,
        "Gamma" = c(input$gamma_shape, input$gamma_rate),
        "Beta" = c(input$beta_shape1, input$beta_shape2)
      )
    }


    ### warning popup if invalid parameter inputs
    #############################################################
    if (input$pop_fam == "Normal") {

      issue_norm1 <- input$norm_sd < 0

      issue_norm2 <- anyNA(c(input$norm_mean, input$norm_sd))

      if (issue_norm1 | issue_norm2) {
        shinyalert("Warning!", "At least one parameter value provided is not valid.
          Please make sure a number is entered for each and
          that the standard deviation is not negative.",
          type = "error"
        )
      }

      validate(need(all(c(issue_norm1, issue_norm2) == FALSE), ""))

    } else if (input$pop_fam == "t") {

      issue_t1 <- input$t_df <= 0

      issue_t2 <- is.na(input$t_df)

      if (issue_t1 | issue_t2) {
        shinyalert("Warning!", "The degrees of freedom value provided is not
          valid. Please enter a positive number.",
          type = "error"
        )
      }

      validate(need(all(c(issue_t1, issue_t2) == FALSE), ""))

    } else if (input$pop_fam == "F") {

      issue_f1 <- any(c(input$f_df1, input$f_df2) <= 0)

      issue_f2 <- anyNA(c(input$f_df1, input$f_df2))

      if (issue_f1 | issue_f2) {
        shinyalert("Warning!", "At least one degrees of freedom value provided is
          not valid. Please enter a positive number for each.",
          type = "error"
        )
      }

      validate(need(all(c(issue_f1, issue_f2) == FALSE), ""))

    } else if (input$pop_fam == "Chi-square") {

      issue_chisq1 <- input$chisq_df <= 0

      issue_chisq2 <- is.na(input$chisq_df)

      if (issue_chisq1 | issue_chisq2) {
        shinyalert("Warning!", "The degrees of freedom value provided is not
          valid. Please enter a positive number.",
          type = "error"
        )
      }

      validate(need(all(c(issue_chisq1, issue_chisq2) == FALSE), ""))

    } else if (input$pop_fam == "Uniform") {

      issue_uni1 <- input$uni_min >= input$uni_max

      issue_uni2 <- anyNA(c(input$uni_min, input$uni_max))

      if (issue_uni1 | issue_uni2) {
        shinyalert("Warning!", "At least one parameter value provided is not valid.
          Please make sure a number is entered for each and that
          the maximum value is larger than the minimum value.",
          type = "error"
        )
      }

      validate(need(all(c(issue_uni1, issue_uni2) == FALSE), ""))

    } else if (input$pop_fam == "Exponential") {

      issue_exp1 <- input$exp_rate <= 0

      issue_exp2 <- is.na(input$exp_rate)

      if (issue_exp1 | issue_exp2) {
        shinyalert("Warning!", "The rate provided is not valid. Please enter a
          positive number.",
          type = "error"
        )
      }

      validate(need(all(c(issue_exp1, issue_exp2) == FALSE), ""))

    } else if (input$pop_fam == "Gamma") {

      issue_gamma1 <- any(c(input$gamma_shape, input$gamma_rate) <= 0)

      issue_gamma2 <- anyNA(c(input$gamma_shape, input$gamma_rate))

      if (issue_gamma1 | issue_gamma2) {
        shinyalert("Warning!", "At least one parameter value provided is
          not valid. Please enter a positive number for each.",
          type = "error"
        )
      }

      validate(need(all(c(issue_gamma1, issue_gamma2) == FALSE), ""))

    } else if (input$pop_fam == "Beta") {

      issue_beta1 <- any(c(input$beta_shape1, input$beta_shape2) <= 0)

      issue_beta2 <- anyNA(c(input$beta_shape1, input$beta_shape2))

      if (issue_beta1 | issue_beta2) {
        shinyalert("Warning!", "At least one parameter value provided is
          not valid. Please enter a positive number for each.",
          type = "error"
        )
      }

      validate(need(all(c(issue_beta1, issue_beta2) == FALSE), ""))

    }



    ### warning popup if sample size input not a whole number
    #############################################################

    # decimal value
    issue_sample_size1 <- mean(c(
      as.numeric(input$sample_size), as.integer(input$sample_size)
    )) != as.numeric(input$sample_size)

    # non-positive value
    issue_sample_size2 <- input$sample_size <= 0

    # input left blank
    issue_sample_size3 <- is.na(input$sample_size)

    if (issue_sample_size1 | issue_sample_size2 | issue_sample_size3) {
      shinyalert("Warning!", "The sample size provided is not valid. Please enter
        a positive whole number.",
        type = "error"
      )
    }

    validate(need(
      all(c(issue_sample_size1, issue_sample_size2, issue_sample_size3) == FALSE),
      ""
    ))


    ### warning popup if number of plots input not a whole number
    #############################################################

    # decimal value
    issue_n_plots1 <- mean(c(
      as.numeric(input$n_plots_rorsch), as.integer(input$n_plots_rorsch)
    )) != as.numeric(input$n_plots_rorsch)

    # non-positive value
    issue_n_plots2 <- input$n_plots_rorsch <= 0

    # input left blank
    issue_n_plots3 <- is.na(input$n_plots_rorsch)

    if (issue_n_plots1 | issue_n_plots2 | issue_n_plots3) {
      shinyalert("Warning!", "The number of plots input is not valid. Please enter
        a positive whole number.",
        type = "error"
      )
    }

    validate(need(
      all(c(issue_n_plots1, issue_n_plots2, issue_n_plots3) == FALSE),
      ""
    ))



    ### randomly sample data
    ##################################
    samples_rorsch <- 1:input$n_plots_rorsch

    datasets <- if (input$pop_fam %in% c("t", "Chi-square", "Exponential")) {

      lapply(seq_along(samples_rorsch), function(samples_rorsch) {
        data.frame(
          Data = dist_random(input$sample_size, params),
          samp = rep(samples_rorsch, each = input$sample_size),
          stringsAsFactors = FALSE)
      })

    } else {

      lapply(seq_along(samples_rorsch), function(samples_rorsch) {
        data.frame(
          Data = dist_random(input$sample_size, params[1], params[2]),
          samp = rep(samples_rorsch, each = input$sample_size),
          stringsAsFactors = FALSE)
      })
    }


    bag$all_data_rorsch <- do.call(rbind, datasets)


    # need this to reset plot output if any input changed
    if (input$rorsch_plot_type %in% c("rorsch_qq", "rorsch_hist")) {
      bag$do_rorsch_single <- input$make_rorsch_plots
    } else if (input$rorsch_plot_type == "rorsch_qq_and_hist") {
      bag$do_rorsch_both <- input$make_rorsch_plots
    }
  })



  output$rorsch_plot_single <- renderPlot({

    req(bag$all_data_rorsch)


    # clear output until make plot button clicked again
    if (bag$do_rorsch_single != FALSE) {

      if (input$rorsch_plot_type == "rorsch_qq") {

        bag$n_rows_rorsch <- if (input$n_plots_rorsch %in% 1:5) {
          1
        } else if (input$n_plots_rorsch %in% 6:10) {
          2
        } else if (input$n_plots_rorsch %in% 11:27) {
          3
        } else if (input$n_plots_rorsch %in% 28:36) {
          4
        }

        bag$qq_rorsch <- ggplot(bag$all_data_rorsch, aes(sample = Data)) +
          stat_qq() +
          stat_qq_line() +
          facet_wrap(~ samp, nrow = bag$n_rows_rorsch) +
          labs(x = "Theoretical Quantile", y = "Sample Quantile") +
          theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
            aspect.ratio = 1
          )

        bag$qq_rorsch

      } else if (input$rorsch_plot_type == "rorsch_hist") {

        # don't save plot, otherwise number of bins input breaks
        bag$hist_rorsch <- ggplot(bag$all_data_rorsch, aes(x = Data)) +
          geom_histogram(bins = d_hist_bins_rorsch()) +
          facet_wrap(~ samp) +
          labs(x = "Data Value", y = "Count") +
          theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)))

        bag$hist_rorsch
      }
    }
  })



  output$rorsch_plot_both_qq <- renderPlot({

    req(bag$all_data_rorsch)


    # clear output until make plot button clicked again
    if (bag$do_rorsch_both != FALSE) {

      bag$both_qq_rorsch <- ggplot(bag$all_data_rorsch, aes(sample = Data)) +
        stat_qq() +
        stat_qq_line() +
        facet_wrap(~ samp) +
        labs(x = "Theoretical Quantile", y = "Sample Quantile") +
        theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
          aspect.ratio = 1
        )

      bag$both_qq_rorsch
    }
  })


  output$rorsch_plot_both_hist <- renderPlot({

    req(bag$all_data_rorsch)


    # clear output until make plot button clicked again
    if (bag$do_rorsch_both != FALSE) {

      # don't save plot, otherwise number of bins input breaks
      bag$both_hist_rorsch <- ggplot(bag$all_data_rorsch, aes(x = Data)) +
        geom_histogram(bins = d_hist_bins_rorsch_both()) +
        facet_wrap(~ samp) +
        labs(x = "Data Value", y = "Count") +
        theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
          aspect.ratio = 1
        )

      bag$both_hist_rorsch
    }
  })



  ### reset Rorschach plot(s) if any input changed
  ####################################################
  observeEvent(
    c(input$pop_dist, input$pop_shape, input$pop_fam,
      input$norm_mean, input$norm_sd, input$t_df, input$f_df1, input$f_df2,
      input$chisq_df, input$uni_min, input$uni_max, input$exp_rate,
      input$gamma_shape, input$gamma_rate, input$beta_shape1, input$beta_shape2,
      input$sample_size, input$n_plots_rorsch, input$rorsch_plot_type
    ),
  {

    bag$do_rorsch_single <- FALSE
    bag$do_rorsch_both <- FALSE
  })



  ### make larger Rorschach plots in popup window
  ####################################################
  output$rorsch_popup_plots_single <- renderPlot({

    #   # clear output until make plot button clicked again
    if (bag$do_rorsch_single != FALSE) {

      if (input$rorsch_plot_type == "rorsch_qq") {
        bag$qq_rorsch
      } else if (input$rorsch_plot_type == "rorsch_hist") {
        bag$hist_rorsch
      }
    }
  })



  output$rorsch_popup_ui_single <- renderUI({

    # need tagList(), not list()
    tagList(
      h4(style = "color: green; font-weight: bold; text-align: center;",
        "After examining the plots below, click the \u2716 in the upper-right
        hand corner of this popup window to close out of the popup and return
        to the main part of the app."
      ),

      plotOutput("rorsch_popup_plots_single", height = 650)
    )
  })



  output$rorsch_popup_both_qq <- renderPlot({

    # clear output until make plot button clicked again
    if (bag$do_rorsch_both != FALSE) bag$both_qq_rorsch
  })

  output$rorsch_popup_both_hist <- renderPlot({

    # clear output until make plot button clicked again
    if (bag$do_rorsch_both != FALSE) bag$both_hist_rorsch
  })



  output$rorsch_popup_ui_both <- renderUI({

    # need tagList(), not list()
    tagList(
      h4(style = "color: green; font-weight: bold; text-align: center;",
        "After examining the plots below, click the \u2716 in the upper-right
        hand corner of this popup window to close out of the popup and return
        to the main part of the app."
      ),

      splitLayout(
        plotOutput("rorsch_popup_both_qq", height = 650),
        plotOutput("rorsch_popup_both_hist", height = 650)
      )
    )
  })




  ##############################################################################
  ################## Line-up Procedure (Include Your Data) Stuff ###############
  ##############################################################################

  ### upload data
  ##################################################
  observeEvent(input$data_file, {

    # determine file extension of uploaded file
    bag$file_extension <- tools::file_ext(input$data_file$datapath)
  })


  output$na_input_status <- reactive({
    req(bag$file_extension)

    bag$file_extension %in% c("csv", "xls", "xlsx", "txt")
  })

  # don't suspend value of na_input_status output
  outputOptions(output, "na_input_status", suspendWhenHidden = FALSE)


  output$na_output_status <- reactive({
    req(bag$file_extension)

    bag$file_extension %in% c("csv", "xls", "xlsx", "txt")
  })

  # don't suspend value of na_input_status output
  outputOptions(output, "na_output_status", suspendWhenHidden = FALSE)



  # store uploaded dataset and update variable names in selectInput box
  observeEvent(input$upload_dataset, {

    req(input$data_file)


    # read file
    if (bag$file_extension == "csv") {
      if (input$missing_input) {
        nas <- input$denote_missing_input %>%
          strsplit(split = ",") %>%  # split if see comma
          unlist() %>%
          str_trim()    # remove leading or trailing whitespace

        nas <- c("", " ", nas)
      } else {
        nas <- c("", " ")
      }

      header <- ifelse(input$no_header, FALSE, TRUE)


      # use read.csv() instead of fread() from data.table package that rio
      #  package uses
      bag$user_data_df <- read.csv(input$data_file$datapath, na.strings = nas,
        header = header
      )


      # rename variables if no names in original file
      if (header == FALSE) {
        for (i in 1:ncol(bag$user_data_df)) {
          names(bag$user_data_df)[i] <- paste0("Variable.", i)
        }
      }

    } else if (bag$file_extension %in% c("xls", "xlsx")) {
      if (input$missing_input) {
        nas <- input$denote_missing_input %>%
          strsplit(split = ",") %>%  # split if see comma
          unlist() %>%
          str_trim()    # remove leading or trailing whitespace

        nas <- c("", " ", nas)
      } else {
        nas <- c("", " ")
      }

      header <- ifelse(input$no_header, FALSE, TRUE)

      bag$user_data_df <- import(input$data_file$datapath, na = nas,
        col_names = header
      )


      # rename variables if no names in original file
      if (header == FALSE) {
        for (i in 1:ncol(bag$user_data_df)) {
          names(bag$user_data_df)[i] <- paste0("Variable.", i)
        }
      }

    } else if (bag$file_extension == "txt") {
      if (input$missing_input) {
        nas <- input$denote_missing_input %>%
          strsplit(split = ",") %>%  # split if see comma
          unlist() %>%
          str_trim()    # remove leading or trailing whitespace

        nas <- c("", " ", nas)
      } else {
        nas <- c("", " ")
      }

      header <- ifelse(input$no_header, FALSE, TRUE)

      bag$user_data_df <- import(input$data_file$datapath, na.strings = nas,
        header = header
      )


      # rename variables if no names in original file
      if (header == FALSE) {
        for (i in 1:ncol(bag$user_data_df)) {
          names(bag$user_data_df)[i] <- paste0("Variable.", i)
        }
      }

    } else if (bag$file_extension == "mat") {

      bag$user_data_df <- as.data.frame(read.mat(input$data_file$datapath))

    } else {

      bag$user_data_df <- import(input$data_file$datapath)
    }


    # remove non-numeric variables from uploaded dataset
    bag$user_data_df <- select_if(bag$user_data_df, is.numeric)


    updateSelectInput(session, "select_var", "Variable of Interest",
      choices = colnames(bag$user_data_df)
    )
  })



  # store selected dataset and update variable names in selectInput box
  observeEvent(input$select_data, {

    bag$user_data_df <- switch(input$app_data,
      "data1" = data.frame(Data = lm(faithful[, 2] ~ faithful[, 1])$residuals),
      "data2" = , data.frame(Data = rock[, 3]),
      "data3" = data.frame(Data = rock[, 2])
    )
  })


  # popup window w/ info about variables included in app
  output$var_info_ui <- renderUI({

    list(
      fluidRow(
        column(2, ""),
        column(8,
          h2(style = "color: #0073e6; font-weight: bold; text-align: center;",
            "Information about the Included Variables"
          ),


          h3(style = "color: #0073e6; font-weight: bold;", "Variable 1"),
          p("Information about the 'faithful' data set can be found
            by clicking",
            a("here.",
              href = "https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/faithful",
              target = "_blank"
            ),
            "In this app, the data are the residuals found when fitting a linear
            regression model with 'eruptions' as the independent variable and
            'waiting' as the dependent variable."
          ),


          h3(style = "color: #0073e6; font-weight: bold;", "Variables 2 & 3"),
          p("Information about the 'rock' data set can be found
            by clicking",
            a("here.",
              href = "https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/rock",
              target = "_blank"
            ),
            "In this app, variable 2 is the variable 'shape,' and variable 3 is
            the perimeter variable 'peri.'"
          )
        ),

        column(2, "")
      )
    )
  })



  ### print data in tabular form
  #################################################

  output$data_table_upload <- renderDT(bag$user_data_df,

    options = list(pageLength = 20, scrollX = TRUE)
  )


  output$table_manual <- renderTable({

    # convert user-input data to numeric vector
    user_data <- input$user_data %>%
      strsplit(split = "\n|,") %>%    # split if see comma or new line
      unlist() %>%
      gsub("[^0-9\\.]", "", .) %>%    # only keep integers and decimals
      as.numeric() %>%
      na.omit()    # remove any NAs

    (bag$user_data_df <- data.frame(Data = user_data))
  })


  output$table_app_data <- renderTable({

    bag$user_data_df
  })




  ##### lineup plots
  #######################################################

  # create and display lineup plots
  observeEvent(input$make_lineup_plots, {

    ### warning popup if no data set input
    #############################################################
    if (is.null(bag$user_data_df)) {
      shinyalert("Warning!", "You must first input data in the 'Input Data and
      Users' tab.",
        type = "error"
      )
    }

    validate(need(!is.null(bag$user_data_df), ""))



    ### warning popup if number of plots input not a whole number
    #############################################################

    # decimal value
    issue_n_plots1 <- mean(c(
      as.numeric(input$n_plots_lineup), as.integer(input$n_plots_lineup)
    )) != as.numeric(input$n_plots_lineup)

    # non-positive value
    issue_n_plots2 <- input$n_plots_lineup <= 0

    # input left blank
    issue_n_plots3 <- is.na(input$n_plots_lineup)

    if (issue_n_plots1 | issue_n_plots2 | issue_n_plots3) {
      shinyalert("Warning!", "The number of plots input is not valid. Please enter
        a whole number value.",
        type = "error"
      )
    }

    validate(need(
      all(c(issue_n_plots1, issue_n_plots2, issue_n_plots3) == FALSE),
      ""
    ))


    # warning popup if number of plots input bad
    bad_n_lineup_plots <- !(input$n_plots_lineup %in% 1:36)

    if (bad_n_lineup_plots) {
      shinyalert("Warning!", "The number of plots input is not valid. Please enter
        a positive whole number between 1 and 36.",
        type = "error"
      )
    }

    validate(need(bad_n_lineup_plots == FALSE, ""))



    # reset identify plot output as soon as new set of plots made
    bag$reset_plot_num <- 0


    if (input$input_type == "upload") {
      if (ncol(bag$user_data_df) > 1) {
        final_user_data <- data.frame(Data = na.omit(
          bag$user_data_df[, input$select_var])
        )
      } else {
        final_user_data <- data.frame(Data = na.omit(bag$user_data_df[, 1]))
      }
    } else if (input$input_type %in% c("manually", "app")) {
      final_user_data <- na.omit(bag$user_data_df)
    }


    # count how many numbers input by user; excludes extra "." (for missing data)
    #  and other non-numeric characters
    sample_size_user <- nrow(na.omit(final_user_data))


    if (input$n_plots_lineup == 1) {

      bag$all_data_lineup <- data.frame(Data = final_user_data$Data, samp = 1)

    } else {

      # set a seed if multiple users involved
      if (input$n_users_radio == "multiple") set.seed(input$user_seed)


      # randomly permute plot indices
      bag$plot_spots <- sample(input$n_plots_lineup)


      # generate synthetic datasets
      datasets_lineup <- lapply(1:(input$n_plots_lineup - 1), function(Data) {
        data.frame(Data = rnorm(
          sample_size_user,
          mean(final_user_data$Data, na.rm = TRUE),
          sd(final_user_data$Data, na.rm = TRUE)
        ))
      })
      sim_data_lineup <- do.call(rbind, datasets_lineup)


      # combine user data and simulated data
      bag$all_data_lineup <- rbind(final_user_data, sim_data_lineup)
      bag$all_data_lineup$samp <- rep(bag$plot_spots, each = sample_size_user)
    }


    bag$all_data_lineup


    # need this to reset plot output if any input changed
    bag$do_lineup <- input$make_lineup_plots
  })



  output$lineup_plot <- renderPlot({

    req(bag$all_data_lineup)


    # clear output until make plot button clicked again
    if (bag$do_lineup != FALSE) {

      if (input$lineup_plot_type == "lineup_qq") {

        bag$n_rows_lineup <- if (input$n_plots_lineup %in% 1:5) {
          1
        } else if (input$n_plots_lineup %in% 6:10) {
          2
        } else if (input$n_plots_lineup %in% 11:27) {
          3
        } else if (input$n_plots_lineup %in% 28:36) {
          4
        }

        bag$qq_lineup <- ggplot(bag$all_data_lineup, aes(sample = Data)) +
          stat_qq() +
          stat_qq_line() +
          facet_wrap(~ samp, nrow = bag$n_rows_lineup) +
          labs(x = "Theoretical Quantile", y = "Sample Quantile") +
          theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)),
            aspect.ratio = 1
          )

        bag$qq_lineup

      } else if (input$lineup_plot_type == "lineup_hist") {

        # don't save plot, otherwise number of bins input breaks
        bag$hist_lineup <- ggplot(bag$all_data_lineup, aes(x = Data)) +
          geom_histogram(bins = d_hist_bins_lineup()) +
          facet_wrap(~ samp) +
          labs(x = "Data Value", y = "Count") +
          theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)))

        bag$hist_lineup
      }
    }
  })




  ### print or clear plot number associated w/ user's data
  observeEvent(input$identify, bag$reset_plot_num <- 1 )


  ### print or clear plot number associated w/ user's data (cont.)
  output$user_plot_number <- renderText({

    if (bag$reset_plot_num == 0) {
      cat("")
    } else if (bag$reset_plot_num == 1) {
      bag$plot_spots[1]
    }
  })



  ### reset line-up plot(s) if any input changed
  ####################################################
  observeEvent(c(input$n_plots_lineup, input$lineup_plot_type, input$user_seed), {

    bag$do_lineup <- FALSE
  })



  ### make larger line-up plots in popup window
  ####################################################
  output$lineup_popup_plots <- renderPlot({

    if (input$lineup_plot_type == "lineup_qq") {
      bag$qq_lineup
    } else if (input$lineup_plot_type == "lineup_hist") {
      bag$hist_lineup
    }
  })





  # observeEvent(input$make_lineup_popup, {
  #
  #   # clear output until make plot button clicked again
  #   if (bag$do_lineup != FALSE) {
  #
  #     if (input$lineup_plot_type == "lineup_qq") {
  #
  #       output$lineup_popup_plots <- renderPlot(bag$qq_lineup)
  #
  #     } else if (input$lineup_plot_type == "lineup_hist") {
  #
  #       output$lineup_popup_plots <- renderPlot({
  #
  #         # don't save otherwise number of bins input breaks
  #         ggplot(bag$all_data_lineup, aes(x = Data)) +
  #           geom_histogram(bins = d_hist_bins_lineup()) +  # change back after activity
  #           facet_wrap(~ samp) +
  #           labs(x = "Data Value", y = "Count") +
  #           theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)))
  #       })
  #     }
  #   }
  # })


  output$lineup_popup_ui <- renderUI({

    # need tagList(), not list()
    tagList(
      h4(style = "color: green; font-weight: bold; text-align: center;",
        "After examining the plots below, click the \u2716 in the upper-right
        hand corner of this popup window to close out of the popup and return
        to the main part of the app."
      ),

      plotOutput("lineup_popup_plots", height = 650)
    )
  })



  # calculate and print p-value for multiple users working independently
  observeEvent(input$calc_pvalue, {

    pval <- 1 - pbinom(input$n_correct - 1, input$n_users, 1/input$n_plots_each)
    rounded_p <- round(pval, 4)

    output$multiple_pvalue <- renderText(

      ifelse(rounded_p >= 0.0001,
        format(rounded_p, scientific = FALSE),
        "<0.0001"
      )
    )
  })

})



