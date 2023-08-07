#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#

function(input, output, session) {

## DATA OVERVIEW ###############################################################
  
  ## data glimpse
  output$the_glimpse <- renderPrint({
    df %>% glimpse()
  })
  
  ### make data overview
  output$data_overview <- renderTable({
    df %>% overall_info2()
  })
  
  ### make data dictionary
  output$data_dictionary <- renderTable({
    df %>% variable_info()
  })

  ### add data type static bar chart
  output$static_datatype <- renderPlot({
    print(data_type_fig_static)
    
  })
  
  ### add missingness static figure
  output$static_missing <- renderPlot({
    print(missingness_fig_static)
    
  })
  
## MARGINAL BAR CHART ###########################################################
  ### categorical variable names for the user selected data ###
  cat_names <- reactive({
    df %>% 
      purrr::discard(is.numeric) %>%
      select(-student_ID) %>%
      names()
  })
  
  ### test the categorical variables were selected as we expect
  output$print_cat_names <- renderPrint({
    print( cat_names() )
  })
  
  ### check if there is at least one categorical variable
  atleast_1_cat <- reactive({
    length( cat_names() ) > 0
  })
  
  ### print test
  output$test_atleast_1_cat <- renderPrint({
    print( atleast_1_cat() )
  })
  
  ### define an output that uses an input function
  output$select_count_cat_1 <- renderUI({
    selectInput('user_count_cat_1',
                'Select the Variable to Count:',
                choices = cat_names(),
                selected = cat_names()[1])
  })
  
  ### check that the selected variable is in the selected dataset
  check_cat_in_df <- reactive({
    df %>%
      select( all_of(input$user_count_cat_1) ) %>%
      ncol()
  })
  
  # make plot
  output$plot_barchart_1 <- renderPlot({
    
    # check if there is are ZERO categoricals!!! 
    # use ! operator because atleast_1_cat() is TRUE if there is 1
    if( !atleast_1_cat() ){ return(NULL) }
    
    if( is.null(input$user_count_cat_1) ){ return(NULL) }
    
    # let the USER decide if the bar chart is horizontal or vertical
    if( input$type_barchart_1 == 'horizontal'){
      g1 <- df %>% 
        ggplot(mapping = aes(y = .data[[input$user_count_cat_1]]))
    } else {
      g1 <- df %>% 
        ggplot(mapping = aes(x = .data[[input$user_count_cat_1]]))
    }
    
    g1 +
      geom_bar(fill = 'pink', color = 'black') +
      theme_classic()
  })
  
## COMBO BAR CHART 2 VARIABLES ################################################# 
  # second categorical variable
  output$select_count_cat_2 <- renderUI({
    selectInput('user_count_cat_2',
                "Select the Variable to Condition By:",
                choices = cat_names(),
                selected = cat_names()[1])
  })
  
  ### make sure both columns are in the data
  check_cat_2_in_df <- reactive({
    df %>% 
      select(all_of(c(input$user_count_cat_1, input$user_count_cat_2))) %>% 
      ncol()
  })
  
  
  ### check for categorical variables
  atleast_2_cat <- reactive({
    length( cat_names() ) > 2
  })
  
  ### print test
  output$test_atleast_2_cat <- renderPrint({
    print( atleast_2_cat() )
  })
  
  ### OBSERVE 
  observe({
    
    first_cat_bar1 <- input$user_count_cat_1
    
    second_cat_bar1 <- cat_names()[ cat_names() != first_cat_bar1 ]
    
    updateSelectInput(session,
                      'user_count_cat_2',
                      choices = second_cat_bar1,
                      selected = second_cat_bar1[1])
  })
  
  # make plot
  output$plot_barchart_2 <- renderPlot({
    
    if( !atleast_1_cat() ){ return(NULL) }
    
    if( is.null(input$user_count_cat_1) ){ return(NULL) }
    
    if( !atleast_2_cat() ){ return(NULL) }
    
    if( is.null(input$user_count_cat_2) ){ return(NULL) }

    # let the USER decide if the bar chart is horizontal or vertical
    if( input$type_barchart_1 == 'horizontal'){
      g2 <- df %>% 
        ggplot(mapping = aes(y = .data[[input$user_count_cat_1]]))
    } else {
      
      g2 <- df %>% 
        ggplot(mapping = aes(x = .data[[input$user_count_cat_1]]))
    }
    
    g2 +
      geom_bar(mapping = aes(fill = .data[[input$user_count_cat_2]]),
               position = input$type_barchart_2) +
      theme_classic()
  })
  
## COMBO BAR CHART 3 VARIABLES ################################################# 
  ### select a second categorical variable 
  output$select_count_cat_3 <- renderUI({
    selectInput('user_count_cat_3',
                "Select the Variable to Condition By:",
                choices = cat_names(),
                selected = cat_names()[1])
  })

  ### make sure both columns are in the data...
  check_cat_3_in_df <- reactive({
    df %>% 
      select(all_of(c(input$user_count_cat_1, input$user_count_cat_3))) %>% 
      ncol()
  })
  
  ### select a third categorical variable 
  output$select_count_cat_4 <- renderUI({
    selectInput('user_count_cat_4',
                "Select the Variable to Facet by:",
                choices = cat_names(),
                selected = cat_names()[1])
  })

  ### make sure all columns are in the data
  check_cat_4_in_df <- reactive({
    df %>% 
      select(all_of(c(input$user_count_cat_1, input$user_count_cat_3, input$user_count_cat_4))) %>% 
      ncol()
  })
  
  ### check if there is at least three categoricals variable
  atleast_3_cat <- reactive({
    length( cat_names() ) > 3
  })
  
  ### print test
  output$test_atleast_3_cat <- renderPrint({
    print( atleast_3_cat() )
  })
  
  # observe (cat 1 -> cat 2)
  observe({
    
    first_cat_bar2 <- input$user_count_cat_1
    
    second_choices2 <- cat_names()[ cat_names() != first_cat_bar2 ]
    
    updateSelectInput(session,
                      'user_count_cat_3',
                      choices = second_choices2,
                      selected = second_choices2[1])
  })
  
  # observe (cat 1 &  2 -> cat 3)
  observe({
    
    first_cat_bar2a <- input$user_count_cat_1
    
    second_cat_bar2a <- input$user_count_cat_3
    
    third_choices2a <- cat_names() [ cat_names() != second_cat_bar2a ] [ cat_names() != first_cat_bar2a ]
    
    updateSelectInput(session,
                      'user_count_cat_4',
                      choices = third_choices2a,
                      selected = third_choices2a[1])
  })
  
  
  # make plot
  output$plot_barchart_3 <- renderPlot({

    if( !atleast_1_cat() ){ return(NULL) }
    
    if( is.null(input$user_count_cat_1) ){ return(NULL) }
    
    if( !atleast_3_cat() ){ return(NULL) }
    
    if( is.null(input$user_count_cat_3) ){ return(NULL) }

    # let the USER decide if the bar chart is horizontal or vertical
    if( input$type_barchart_1 == 'horizontal'){
      
      g3 <- df %>% 
        ggplot(mapping = aes(y = .data[[input$user_count_cat_1]]))
      
    } else {
      g3 <- df %>% 
        ggplot(mapping = aes(x = .data[[input$user_count_cat_1]]))
    }
    
    g3 +
      geom_bar(mapping = aes(fill = .data[[input$user_count_cat_3]]),
               position = input$type_barchart_3) +
      facet_wrap(~ .data[[input$user_count_cat_4]]) +
      theme_classic()
  })
  
## BASIC HISTOGRAM #############################################################
  ### cont variable names for the user selected data ###
  cont_names <- reactive({
    df %>% 
      purrr::discard(is.factor) %>%
      names()
  })
  
  ### create the output that will be displayed to the ui
  output$print_cont_names <- renderPrint({
    print( cont_names() )
  })
  
  ### check if there is at least one cont variable
  atleast_1_cont <- reactive({
    length(cont_names()) > 0
  })
  
  ### print test
  output$test_atleast_1_cont <- renderPrint({
    print(atleast_1_cont())
  })
  
  ### define an output that uses an input function
  output$select_cont_dist_1 <- renderUI({
    selectInput('user_cont_dist_1',
                'Select the Variable to Visualize:',
                choices = cont_names(),
                selected = cont_names()[1])
  })
  
  ### check that the selected variable is in the selected data set
  check_cont_in_df <- reactive({
    df %>%
      select( all_of(input$user_cont_dist_1) ) %>%
      ncol()
  })
  
  # make plot
  output$plot_dist_1 <- renderPlot({
    
    if( !atleast_1_cont() ) { return(NULL) }
    
    if( is.null(input$user_cont_dist_1) ){ return(NULL) }
    
    g <- df %>%
      ggplot(mapping = aes(x = .data[[ input$user_cont_dist_1 ]])) +
      geom_histogram(bins = input$cont_bins,
                     fill = 'slategrey',
                     color = 'black',
                     mapping = aes(y = after_stat(density))) +
      theme_classic()
    
    ## check if the user selected the KDE option
    if(input$cont_kde){
      g <- g + 
        geom_density(linewidth = 1.2, color = 'blue')
    }
    
    ## check if the user selected the RUG option
    if(input$cont_rug){
      g <- g +
        geom_rug(linewidth = 1, color = 'orange')
      
    }
    
    ## display the plot
    print( g )
    
  })

## FREQUENCY POLYGON ###########################################################
  ### categorical variable names for the user selected data ###
  cat_namesA <- reactive({
    df %>% 
      purrr::discard(is.numeric) %>%
      select(-student_ID) %>%
      names()
  })
  
  ### test the categorical variables were selected as we expect
  output$print_cat_namesA <- renderPrint({
    print( cat_namesA() )
  })
  
  ### check if there is at least one categorical variable
  atleast_1_catA <- reactive({
    length( cat_namesA() ) > 0
  })
  
  ### print rest
  output$test_atleast_1_catA <- renderPrint({
    print( atleast_1_catA() )
  })
  
  ### define an output that uses an input function
  output$select_count_cat_A <- renderUI({
    selectInput('user_count_cat_A',
                'Select the Variable to Count:',
                choices = cat_namesA(),
                selected = cat_namesA()[1])
  })
  
  ### check that the selected variable is in the selected dataset
  check_catA_in_df <- reactive({
    df %>%
      select( all_of(input$user_count_cat_A) ) %>%
      ncol()
  })
  
  ### define an output that uses an input function
  output$select_catA <- renderUI({
    selectInput('user_count_cat_A',
                'Select the Variable to Condition on:',
                choices = cat_namesA(),
                selected = cat_namesA()[1])
  })
  
  
  ## make plot
  output$plot_freqpoly <- renderPlot({
    
    if( !atleast_1_cont() ) { return(NULL) }
    
    if( is.null(input$user_cont_dist_1) ){ return(NULL) }
    
    if( !atleast_1_catA() ){ return(NULL) }
    
    if( is.null(input$user_count_cat_A) ){ return(NULL) }
    
    df %>%
      ggplot(mapping = aes(x = .data[[ input$user_cont_dist_1 ]])) +
      geom_freqpoly(bins = input$cont_bins2,
                    mapping = aes(y = after_stat(density), color = .data[[input$user_count_cat_A]]),
                    linewidth = 1.2) +
      theme_classic()

  })
## eCDF ########################################################################
  ### categorical variable names for the user selected data ###
  cat_namesB <- reactive({
    df %>% 
      purrr::discard(is.numeric) %>%
      select(-student_ID) %>%
      names()
  })
  
  ### test the categorical variables were selected as we expect
  output$print_cat_namesB <- renderPrint({
    print( cat_namesB() )
  })
  
  ### check if there is at least one categorical variable
  atleast_1_catB <- reactive({
    length( cat_namesB() ) > 0
  })
  
  ### print test
  output$test_atleast_1_catB <- renderPrint({
    print( atleast_1_catB() )
  })
  
  ### define an output that uses an input function
  output$select_count_cat_B <- renderUI({
    selectInput('user_count_cat_B',
                'Select the Variable to Count:',
                choices = cat_namesB(),
                selected = cat_namesB()[1])
  })
  
  ### check that the selected variable is in fact selected
  check_catB_in_df <- reactive({
    df %>%
      select( all_of(input$user_count_cat_B) ) %>%
      ncol()
  })
  
  ### define an output that uses an input function
  output$select_catB <- renderUI({
    selectInput('user_count_cat_B',
                'Select the Variable to Condition on:',
                choices = cat_namesB(),
                selected = cat_namesB()[1])
  })
  
  ## make plot
  output$plot_eCDF <- renderPlot({
    
    if( !atleast_1_cont() ) { return(NULL) }
    
    if( is.null(input$user_cont_dist_1) ){ return(NULL) }
    
    if( !atleast_1_catB() ){ return(NULL) }
    
    if( is.null(input$user_count_cat_B) ){ return(NULL) }
    
    df %>%
      ggplot(mapping = aes(x = .data[[ input$user_cont_dist_1 ]])) +
      stat_ecdf(mapping = aes(color = .data[[input$user_count_cat_B]]), linewidth = 1.2) +
      geom_hline(yintercept = .5, color = 'red', linetype = 'dashed', linewidth = 1.2) +
      theme_classic()
    
  })
## BOX PLOT ####################################################################
  
  ## CATEGORICAL ONE 
  ### categorical variable names for the user selected data ###
  cat_namesC <- reactive({
    df %>% 
      purrr::discard(is.numeric) %>%
      select(-student_ID) %>%
      names()
  })
  
  ### test the categorical variables were selected as we expect
  output$print_cat_namesC <- renderPrint({
    print( cat_namesC() )
  })
  
  ### check if there is at least one categorical variable
  atleast_1_catC <- reactive({
    length(cat_names()) > 0
  })
  
  ### print test
  output$test_atleast_1_catC <- renderPrint({
    print(atleast_1_catC())
  })
  
  ### check that the selected variable is in fact in the selected dataset
  check_catC_in_df <- reactive({
    df %>%
      select( all_of(input$user_count_cat_C) ) %>%
      ncol()
  })
  
  ### define an output that uses an input function
  output$select_catC <- renderUI({
    selectInput('user_count_cat_C',
                'Select the Variable on the y-axis:',
                choices = cat_namesC(),
                selected = cat_namesC()[1])
  })
  

  ## CATEGORICAL TWO
  ### select a second categorical variable 
  output$select_catD <- renderUI({
    selectInput('user_count_cat_D',
                "Select the Variable to Color by:",
                choices = cat_namesC(),
                selected = cat_namesC()[1])
  })
  
  ### OBSERVE to dynamically change the second selection
  ### based on the first selection!!
  observe({
    first_cat_CC <- input$user_count_cat_C
    
    second_choices_CC <- cat_namesC()[ cat_namesC() != first_cat_CC ]
    
    updateSelectInput(session,
                      'user_count_cat_D',
                      choices = second_choices_CC,
                      selected = second_choices_CC[1])
  })
  
  ### make sure both columns are in the data
  check_catD_in_df <- reactive({
    df %>% 
      select(all_of(c(input$user_count_cat_C, input$user_count_cat_D))) %>% 
      ncol()
  })
  
  ## CATEGORICAL THREE
  ### select a second categorical variable 
  output$select_catE <- renderUI({
    selectInput('user_count_cat_E',
                "Select the Variable to Facet by:",
                choices = cat_namesC(),
                selected = cat_namesC()[1])
  })
  
  
  # observe (cat 1 -> cat 2)
  observe({
    
    first_cat_box1 <- input$user_count_cat_C
    
    second_choices_box1 <- cat_names()[ cat_names() != first_cat_box1 ]
    
    updateSelectInput(session,
                      'user_count_cat_D',
                      choices = second_choices_box1,
                      selected = second_choices_box1[1])
  })
  
  # observe (cat 1 &  2 -> cat 3)
  observe({
    
    first_cat_box2 <- input$user_count_cat_C
    
    second_cat_box2 <- input$user_count_cat_D
    
    third_choices_box2 <- cat_names() [ cat_names() != second_cat_box2 ] [ cat_names() != first_cat_box2 ]
    
    updateSelectInput(session,
                      'user_count_cat_E',
                      choices = third_choices_box2,
                      selected = third_choices_box2[1])
  })

  ## make boxplot
  output$plot_boxplot <- renderPlot({
    
    if( !atleast_1_catC() ){ return(NULL) }
    
    if( is.null(input$user_count_cat_C) ){ return(NULL) }
    
    g4 <- df %>%
      ggplot(mapping = aes(x = .data[[ input$user_cont_dist_1 ]],
                           y = .data[[input$user_count_cat_C]])) +
      geom_boxplot(mapping = aes(color = .data[[input$user_count_cat_D]]),
                   alpha = .25) +
      stat_summary(mapping = aes(color = .data[[input$user_count_cat_D]]),
                   fun.data = 'mean_se',
                   fun.args = list(mult = 2),
                   position = position_dodge(.75),
                   linewidth = 1.2,
                   size = .88) +
      facet_wrap(~ .data[[input$user_count_cat_E]]) +
      theme_classic() +
      theme(legend.position = "bottom")
    
    if(input$flip_ax1){
      g4 <- g4 + 
        coord_flip()
    }
    
    print (g4)

  })
  
  ## make conditional means plot
  output$plot_condmeans <- renderPlot({
    g5 <- df %>%
      ggplot(mapping = aes(x = .data[[ input$user_cont_dist_1 ]],
                           y = .data[[input$user_count_cat_C]])) +
      stat_summary(fun.data = 'mean_se',
                   fun.args = list(mult = 2),
                   color = 'red',
                   linewidth = 1.2,
                   size = .88) +
      stat_summary(mapping = aes(color = .data[[input$user_count_cat_D]]),
                   fun.data = 'mean_se',
                   fun.args = list(mult = 2),
                   position = position_dodge(.75),
                   linewidth = 1.2,
                   size = .88) +
      facet_wrap(~ .data[[input$user_count_cat_E]]) +
      theme_classic() +
      theme(legend.position = "bottom")
    
    if(input$flip_ax2){
      g5 <- g5 + 
        coord_flip()
    }
    
    print (g5)
    
  })
  
## UNCONDITIONAL SCATTER PLOT ##################################################
  # first cont
  output$select_cont_dist_3 <- renderUI({
    selectInput('user_cont_dist_3',
                'Select the First Continous Variable to Visualize:',
                choices = cont_names(),
                selected = cont_names()[1])
  })
  
  # second cond
  output$select_cont_dist_4 <- renderUI({
    selectInput('user_cont_dist_4',
                'Select the Second Continous Variable to Visualize:',
                choices = cont_names(),
                selected = cont_names()[1])
  })
  
  # we need to observe
  observe({
    
    first_cont_var2 <- input$user_cont_dist_3
    
    second_cont_var2 <- cont_names()[ cont_names() != first_cont_var2 ]
    
    updateSelectInput(session,
                      'user_cont_dist_4',
                      choices = second_cont_var2,
                      select = second_cont_var2[1])
  })
  
  ### check if there is at least one cont variable
  atleast_1_contSP <- reactive({
    length(cont_names()) > 0
  })
  
  ### print so we can see the value of the at least one cont variable we created
  output$test_atleast_1_contSP <- renderPrint({
    print(atleast_1_contSP())
  })
  
  # check for cont
  check_cont_in_dfSP <- reactive({
    df %>%
      select( all_of(input$user_cont_dist_3) ) %>%
      ncol()
  })
  
  # make plot
  output$scatter_plot_1b <- renderPlot({
    
    if( !atleast_1_contSP() ) { return(NULL) }
    
    if( is.null(input$user_cont_dist_3) ){ return(NULL) }
    
    if( check_cont_in_dfSP() < 1 ) { return(NULL) }
    
    k <- df %>%
      ggplot(mapping = aes(x = .data[[ input$user_cont_dist_3 ]], y = .data[[ input$user_cont_dist_4 ]])) +
      geom_point(mapping = aes(size = 2), alpha = input$point_alpha1b, show.legend = "none") +
      theme_classic()
    
    if(input$cont_2e){
      k <- k + 
        geom_density_2d(color = 'purple', linewidth = 1.2, show.legend = "none")
    }
    
    if(input$cont_geomsmoothB){
      k <- k + 
        geom_smooth(formula = y ~ x, method = lm, show.legend = "none")
    }
    
    print(k)
    
  })
  
## COLOR BY CATEGORY SCATTER PLOT ##############################################
  ## categorical one
  ### categorical variable names for the user selected data ###
  cat_namesP2 <- reactive({
    df %>% 
      purrr::discard(is.numeric) %>%
      select(-student_ID) %>%
      names()
  })
  
  ### test the categorical variable
  output$print_cat_namesP2 <- renderPrint({
    print( cat_namesP() )
  })
  
  ### check if there is at least one categorical variable
  atleast_1_catP2 <- reactive({
    length(cat_namesP2()) > 0
  })
  
  ### print test
  output$test_atleast_1_catP2 <- renderPrint({
    print(atleast_1_catP2())
  })
  
  ### check that the selected variable is in fact in the selected dataset
  check_catP2_in_df <- reactive({
    df %>%
      select( all_of(input$user_count_cat_P2) ) %>%
      ncol()
  })
  
  ### define an output that uses an input function
  output$select_catP2 <- renderUI({
    selectInput('user_count_cat_P2',
                'Select the Variable to Color by:',
                choices = cat_namesP2(),
                selected = cat_namesP2()[1])
  })
  
  ## make plot
  output$scatter_plot_2b <- renderPlot({
    
    if( !atleast_1_contSP() ) { return(NULL) }
    
    if( is.null(input$user_cont_dist_3) ){ return(NULL) }
    
    if( !atleast_1_catP2() ) { return(NULL) }
    
    if( is.null(input$user_count_cat_P2) ){ return(NULL) }
    
    df %>%
      ggplot(mapping = aes(x = .data[[ input$user_cont_dist_3 ]], y = .data[[ input$user_cont_dist_4 ]])) +
      geom_point(mapping = aes(color = .data[[ input$user_count_cat_P2 ]]),
                 alpha = input$point_alpha2b) +
      scale_color_viridis_d() +
      theme_classic()
    
  })
  
## COLOR & FACET BY CATEGORY SCATTER PLOT ###################################### 
  ## selecting categorical 1
  cat_namesQ3 <- reactive({
    df %>%
      purrr::discard(is.numeric) %>%
      select(-student_ID) %>%
      names()
  })
  
  ### create the output
  output$print_cat_namesQ3 <- renderPrint({
    print( cat_namesY() )
  })
  
  ### check if there is at least one categorical variable
  atleast_1_catQ3 <- reactive({
    length(cat_names()) > 0
  })
  
  ### print test
  output$test_atleast_1_catQ3 <- renderPrint({
    print(atleast_1_catQ3())
  })
  
  ### define an output that uses an input function
  output$select_cat_Q3 <- renderUI({
    selectInput('user_count_cat_Q3',
                'Select the Variable to Color by:',
                choices = cat_namesQ3(),
                selected = cat_namesQ3()[1])
  })
  
  ## selecting categorical 2
  
  ### select a section categorical variable
  output$select_cat_R3 <- renderUI({
    selectInput('user_count_cat_R3',
                'Select the Variable to Facet by:',
                choices = cat_namesQ3(),
                selected = cat_namesQ3()[1])
  })
  
  ### we need to observe
  observe({
    first_catQ3 <- input$user_count_cat_Q3
    
    second_choicesQ3 <- cat_namesQ3()[ cat_namesQ3() != first_catQ3 ]
    
    updateSelectInput(session,
                      'user_count_cat_R3',
                      choices = second_choicesQ3,
                      select = second_choicesQ3[1])
  })
  
  # check for first categorical
  check_catQ3_in_df <- reactive({
    df %>% 
      select(all_of(c(input$user_count_cat_Q3))) %>% 
      ncol()
  })
  
  # check for second categorical
  check_catR3_in_df <- reactive({
    df %>% 
      select(all_of(c(input$user_count_cat_Q3, input$user_count_cat_R3))) %>% 
      ncol()
  })
  
  # at least one cat
  atleast_1_catQ3 <- reactive({
    length(cat_namesQ3()) > 0
  })
  
  # at least one cat
  atleast_1_catR3 <- reactive({
    length(cat_namesQ3()) > 0
  })

  ## make plot
  output$scatter_plot_3b <- renderPlot({
    
    if( !atleast_1_contSP() ) { return(NULL) }
    
    if( is.null(input$user_cont_dist_3) ){ return(NULL) }
    
    if( !atleast_1_catQ3() ) { return(NULL) }
    
    if( is.null(input$user_count_cat_Q3) ){ return(NULL) }
    
    df %>%
      ggplot(mapping = aes(x = .data[[ input$user_cont_dist_3 ]], y = .data[[ input$user_cont_dist_4 ]])) +
      geom_point(mapping = aes(color = .data[[ input$user_count_cat_Q3 ]]),
                 alpha = input$point_alpha3b) +
      facet_wrap(~ .data[[ input$user_count_cat_R3 ]]) +
      theme_classic()
    
  })
  
## SCHOOL COMPARE TAB ##########################################################
  
  ## filter reactive for school
  school_reactive <- reactive({
    df %>%
      filter(School %in% as.character(input$user_school))
  })
  
  ## selecting cont X axis
  ### cont variable names for the user selected data ###
  cont_namesX <- reactive({
    df %>%
      purrr::discard(is.factor) %>%
      names()
  })

  ### create the output
  output$print_cont_namesX <- renderPrint({
    print( cont_namesX() )
  })

  ### check if there is at least one cont variable
  atleast_1_contX <- reactive({
    length(cont_namesX()) > 0
  })

  ### test
  output$test_atleast_1_contX <- renderPrint({
    print(atleast_1_contX())
  })

  ### define an output that uses an input function
  output$select_cont_X <- renderUI({
    selectInput('user_cont_X',
                'Select the y-axis Variable:',
                choices = cont_namesX(),
                selected = cont_namesX()[1])
  })

  ## selecting categorical Y axis
  cat_namesY <- reactive({
    df %>%
      purrr::discard(is.numeric) %>%
      select(-c(student_ID, School)) %>%
      names()
  })

  ### create the output
  output$print_cat_namesY <- renderPrint({
    print( cat_namesY() )
  })

  ### check if there is at least one categorical variable
  atleast_1_catY <- reactive({
    length(cat_names()) > 0
  })

  ### print test
  output$test_atleast_1_catY <- renderPrint({
    print(atleast_1_catY())
  })

  ### define an output that uses an input function
  output$select_cat_Y <- renderUI({
    selectInput('user_cat_Y',
                'Select the x-axis Variable:',
                choices = cat_namesY(),
                selected = cat_namesY()[1])
  })

  
  ### select a section categorical variable
  output$select_cat_Z <- renderUI({
    selectInput('user_cat_Z',
                'Select the Variable to Facet by:',
                choices = cat_namesY(),
                selected = cat_namesY()[1])
  })

  ### we need to observe
  
  observe({
    first_catY <- input$user_cat_Y

    second_choicesZ <- cat_namesY()[ cat_namesY() != first_catY ]

    updateSelectInput(session,
                      'user_cat_Z',
                      choices = second_choicesZ,
                      select = second_choicesZ[1])
  })
  
  ## make plot
  output$plot_boxplot2 <- renderPlot({
    
    if( !atleast_1_catY() ){ return(NULL) }
    
    if( is.null(input$user_cat_Y) ){ return(NULL) }
    
    if( !atleast_1_catY() ){ return(NULL) }
    
    if( is.null(input$user_cat_Z) ){ return(NULL) }
    
    g3 <- school_reactive() %>%
      ggplot(mapping = aes(x = .data[[ input$user_cat_Y ]], y = .data[[ input$user_cont_X ]])) +
      geom_boxplot(mapping = aes(color = School), alpha = .25) +
      facet_wrap(~ .data[[input$user_cat_Z]]) +
      theme_classic()
    
    if(input$flip_ax3){
      g3 <- g3 + 
        coord_flip()
    }
    
    print (g3)

  })

## CORRELATION & PCA PLOTS ######################################################
  # CORRELATION PLOT
  output$basic_corrplot <- renderPlot({
    if(input$corr_reorder == FALSE){
      g1 <- df %>% 
        keep(is.numeric) %>% 
        cor() %>% 
        corrplot(type = 'upper', method = 'number')
    } else {
      g1 <- df %>% 
        keep(is.numeric) %>% 
        cor() %>% 
        corrplot(type = 'upper', method = 'color',
                 order = 'hclust', hclust.method = 'ward.D2')
    }
    g1})
  
  # PCA PLOT (VARIANCE EXPLAINED)
  output$pc_var_exp_static <- renderPlot({
    print(pc_var_exp_static)})
  
  # PC SCATTERPLOT
  output$pca_scatter_static <- renderPlot({
    print(pca_scatter_static)})
  
  # PC HEATMAP 
  output$heat_map_pca_static <- renderPlot({
    print(heat_map_pca_static)})
  
  # COSINE PLOT
  output$specialized_pca_scatter_static <- renderPlot({
    print(specialized_pca_scatter_static)})
  
  }
