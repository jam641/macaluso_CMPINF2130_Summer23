# Welcome to the Education App!

This app allows a user to explore the `education` data set that was retrieved from kaggle.com (https://www.kaggle.com/datasets/desalegngeb/students-exam-scores?resource=download).

This data set looks at some demographic and personal information (categorical variables) as well as performance on math, reading, and writing tests (continuous variables).

This app will be exploring the counts of all of the categorical variables as well as the distributions of all of the continuous variables.

This app will also allow the user to select different and multiple categorical grouping variables (i.e., via faceting and mapping onto color) to visualize the data.

Overall, the user can explore dynamic bar charts, dynamic distributions, dynamic scatter plots, and figures looking at correlations and PCA.


---------------------------------------------------------------------------------


## Below is the data dictionary provided via kaggle.



`Gender`: Gender of the student (male/female)

`EthnicGroup`: Ethnic group of the student (group A to E)

`ParentEduc`: Parent(s) education background (from some_highschool to master’s degree)

`LunchType`: School lunch type (standard or free/reduced)

`TestPrep`: Test preparation course followed (completed or none)

`ParentMaritalStatus`: Parent(s) marital status (married/single/widowed/divorced)

`PracticeSport`: How often the student parctice sport (never/sometimes/regularly))

`IsFirstChild`: If the child is first child in the family or not (yes/no)

`NrSiblings`: Number of siblings the student has (0 to 7)

`TransportMeans`: Means of transport to school (schoolbus/private)

`WklyStudyHours`: Weekly self-study hours(less that 5hrs; between 5 and 10hrs; more than 10hrs)

`MathScore`: math test score (0-100)

`ReadingScore`: reading test score (0-100)

`WritingScore`: writing test score (0-100)


A `School` variable was also created from an additional identification column (originally called `X`); there are 31 schools in the data set. The user can also compare variables between multiple different  `School`s.