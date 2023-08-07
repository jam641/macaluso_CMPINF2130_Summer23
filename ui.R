#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#

## WELCOME PAGE
navbarPage('Education App',
           tabPanel('Welcome!',
                    includeMarkdown('welcome_education.md'
                                    )),
           
           ## OVERVIEW TAB 
           tabPanel('Overview of Data',
                    tabsetPanel(type = 'pills',
                                tabPanel('Data Dictionary',
                                         h4('Overall Information About the Data Set'),
                                         tableOutput('data_overview'),
                                         h4('Information About the Variables in the Data Set'),
                                         tableOutput('data_dictionary')
                                         ),
                                tabPanel('Data Glimpse',
                                         h4('Display a `glimpse()` of the `education` Data Set'),
                                         verbatimTextOutput('the_glimpse')),
                                tabPanel('Visualize Data Types',
                                         h4('13 Variables are Categorical and 3 Variables are Continuous'),
                                         plotOutput('static_datatype')),
                                tabPanel('Visualize Missingness',
                                         h4('Missingness Visual'),
                                         plotOutput('static_missing'),
                                         h5('No Missingness: student_id, Gender, LunchType, School, MathScore, ReadingScore, and WritingScore'),
                                         h5('Yes Missingness: EthnicGroup, ParentEduc, TestPrep, ParentalMaritalStatus, PracticeSport, IsFirstChild, NrSiblings, TransportMeans, and WklyStudyHours'))
                                )),
           
           ## COUNTS - BAR CHARTS TAB
           tabPanel('Counts - Bar Charts',
                     sidebarLayout(
                       sidebarPanel(
                         uiOutput('select_count_cat_1'),
                         radioButtons('type_barchart_1',
                                      "Bar Chart Orientation:",
                                      choices = c("vertical", "horizontal"))
                       ),
                       mainPanel(
                         tabsetPanel(type = 'pills',
                                     tabPanel('Marginal Counts',
                                              h3('Marginal Counts Bar Chart'),
                                              plotOutput('plot_barchart_1')),
                                     tabPanel('Combinations - 2 Variables',
                                              h3('Combinations - 2 Variable Bar Chart'),
                                              uiOutput('select_count_cat_2'),
                                              radioButtons('type_barchart_2',
                                                           'Fill Position:',
                                                           choices = c("stack", "dodge"),
                                                           selected = 'stack'),
                                              plotOutput('plot_barchart_2')),
                                     tabPanel('Combinations - 3 Variables',
                                              h3('Combinations - 3 Variable Bar Chart'),
                                              uiOutput('select_count_cat_3'),
                                              uiOutput('select_count_cat_4'),
                                              radioButtons('type_barchart_3',
                                                           'Fill Position:',
                                                           choices = c("stack", "dodge"),
                                                           selected = 'stack'),
                                              plotOutput('plot_barchart_3')
                                              )
                                     )
                         )
                       )),
           
           ## DISTRIBUTIONS TAB
           tabPanel('Distributions',
                       sidebarLayout(
                         sidebarPanel(
                           uiOutput('select_cont_dist_1'),
                           ),
                         mainPanel(
                           tabsetPanel(type = 'pills',
                                       tabPanel('Marginal',
                                                h3('Marginal Distribution'),
                                                plotOutput('plot_dist_1'),
                                                sliderInput('cont_bins',
                                                            'Number of Bins:',
                                                            min = 1,
                                                            max = 50,
                                                            value = 30,
                                                            step = 1),
                                                checkboxInput('cont_kde',
                                                              'Include Kernal Density Estimate?',
                                                              value = FALSE),
                                                checkboxInput('cont_rug',
                                                              'Include Rug Marks?',
                                                              value = FALSE)
                                                
                                                ),
                                       tabPanel('Frequency Polygon',
                                                h3('Conditional Distribution: Frequency Polygon'),
                                                uiOutput('select_catA'),
                                                plotOutput('plot_freqpoly'),
                                                sliderInput('cont_bins2',
                                                            'Number of Bins:',
                                                            min = 1,
                                                            max = 50,
                                                            value = 30,
                                                            step = 1),
                                                ),
                                       tabPanel('eCDF',
                                                h3('Conditional Distribution: eCDF'),
                                                uiOutput('select_catB'),
                                                plotOutput('plot_eCDF')
                                                ),
                                       tabPanel('Box Plot & Conditional Means',
                                                h3('Conditional Distribution: Box Plot & Conditional Means'),
                                                uiOutput('select_catC'),
                                                uiOutput('select_catD'),
                                                uiOutput('select_catE'),
                                                h4('Box Plots'),
                                                plotOutput('plot_boxplot'),
                                                checkboxInput('flip_ax1',
                                                              'Flip Axes?',
                                                              value = FALSE),
                                                h4('Conditional Means'),
                                                h5('The Red Dot and CI Indicates the Conditional Average with Just the y-aesthetic and the Facet (Combination of 2 Categorical Varables)'),
                                                h5('The Other Colors (Dot and CI) are the Conditional Average (Combination of 3 Categorical Variables)'),
                                                plotOutput('plot_condmeans'),
                                                checkboxInput('flip_ax2',
                                                              'Flip Axes?',
                                                              value = FALSE))
                                       )
                           )
                         )),
           
           ## SCATTER PLOTS TAB
           tabPanel('Scatter Plots',
                    sidebarLayout(
                      sidebarPanel(
                        uiOutput('select_cont_dist_3'),
                        uiOutput('select_cont_dist_4')
                      ),
                      mainPanel(
                        tabsetPanel(type = 'pills',
                                    tabPanel('Unconditional',
                                             h3('Unconditional Scatter Plot'),
                                             plotOutput('scatter_plot_1b'),
                                             checkboxInput('cont_2e',
                                                           'Include 2D Density?',
                                                           value = FALSE),
                                             checkboxInput('cont_geomsmoothB',
                                                           'Include Trend Line?',
                                                           value = FALSE),
                                             sliderInput('point_alpha1b',
                                                         'Alpha Transparency:',
                                                         min = .1,
                                                         max = 1,
                                                         value = .5,
                                                         step = .1)),
                                    tabPanel('Color by Category',
                                             h3('Color by Category Scatter Plot'),
                                             uiOutput('select_catP2'),
                                             plotOutput('scatter_plot_2b'),
                                             sliderInput('point_alpha2b',
                                                         'Alpha Transparency:',
                                                         min = .1,
                                                         max = 1,
                                                         value = .2,
                                                         step = .1)),
                                    tabPanel('Color and Facet by Categoricals',
                                             h3('Color and Facet by Categoricals Scatter Plot'),
                                             uiOutput('select_cat_Q3'),
                                             uiOutput('select_cat_R3'),
                                             plotOutput('scatter_plot_3b'),
                                             sliderInput('point_alpha3b',
                                                         'Alpha Transparency:',
                                                         min = .1,
                                                         max = 1,
                                                         value = .2,
                                                         step = .1))
                      )
                    )
                    )),
           
           ## SCHOOL COMPARISONS TAB
           tabPanel('School Comparisons',
                    sidebarLayout(
                      sidebarPanel(
                        selectInput('user_school',
                                    'Select School(s):',
                                    choices = unique_school_names,
                                    multiple = TRUE,
                                    selectize = TRUE,
                                    selected = unique_school_names[1:2])
                      ),
                      mainPanel(
                        tabsetPanel(type = 'pills',
                                    tabPanel('School Comparisons',
                                             h3('Comparing Multiple Schools Across a Variety of Variables'),
                                             uiOutput('select_cont_X'),
                                             uiOutput('select_cat_Y'),
                                             uiOutput('select_cat_Z'),
                                             plotOutput('plot_boxplot2'),
                                             checkboxInput('flip_ax3',
                                                           'Flip Axes?',
                                                           value = FALSE),
                        )
                      )
                    ))),
           
           ## CORRELATION PLOT AND PCA TAB
           tabPanel('Correlation Plot & PCA',
                    tabsetPanel(type = 'pills',
                                tabPanel('Correlation Plot',
                                         h3('Correlation Plot'),
                                         plotOutput('basic_corrplot'),
                                         checkboxInput('corr_reorder',
                                                       'Switch from Number to Color Display?',
                                                       value = FALSE)),
                                tabPanel('PC Variance Explained & Variable Contribution to PC',
                                         h3('PC Variance Explained Plot'),
                                         plotOutput('pc_var_exp_static'),
                                         h3('Variable Contribution to PC Plot'),
                                         plotOutput('heat_map_pca_static')),
                                tabPanel('PC1 & PC2 Scatter Plot',
                                         h3('PC1 & PC2 Scatter Plot'),
                                         plotOutput('pca_scatter_static')),
                                tabPanel('Cosine Plot',
                                         h3('Cosine Plot'),
                                         plotOutput('specialized_pca_scatter_static'))
                    ))

)