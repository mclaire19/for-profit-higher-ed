library(shiny)
library(shinythemes)
library(tidyverse)
library(skimr)
library(waffle)
library(plyr)
library(dplyr)
library(scales)

final <- read_csv('data/fouryr.csv')

#select variables for analysis
#select ID, name of school, school type, region, cost variables, price variables,
#family income variables, net price variables, pct loan variables, pct pell variables,
#median debt variables, completion variables, repayment variables, earnings variables

final <- as.data.frame(select(final, UNITID, INSTNM, CONTROL, REGION, NPT4_PUB, NPT4_PRIV, 
                              NPT4_PROG, NPT4_OTHER, NPT41_PUB, NPT42_PUB, NPT43_PUB,
                              NPT44_PUB, NPT45_PUB, NPT41_PRIV, NPT42_PRIV, NPT43_PRIV,
                              NPT44_PRIV, NPT45_PRIV, NPT41_PROG, NPT42_PROG, NPT43_PROG,
                              NPT44_PROG, NPT45_PROG, NPT41_OTHER, NPT42_OTHER, NPT43_OTHER,
                              NPT44_OTHER, NPT45_OTHER, COSTT4_A, COSTT4_P, PCTPELL, PCTFLOAN,
                              C150_4, RPY_3YR_RT, MN_EARN_WNE_P10, MD_EARN_WNE_P10, 
                              MN_EARN_WNE_INC1_P10, MN_EARN_WNE_INC2_P10, MN_EARN_WNE_INC3_P10, 
                              TUITIONFEE_IN, TUITIONFEE_OUT, TUITIONFEE_PROG,
                              CDR3))

#convert control, region, and unitid to factor variables
final$CONTROL <- as.factor(final$CONTROL)
final$REGION <- as.factor(final$REGION)
final$UNITID <- as.factor(final$UNITID)

# Rename CONTROL factor variable
final$CONTROL <- mapvalues(final$CONTROL, from = c(1, 2, 3), to = c("Public", "Private Non-Profit", "Private For-Profit"))

#convert numeric variables to numeric
final$PCTPELL <- as.numeric(final$PCTPELL)
final$PCTFLOAN <- as.numeric(final$PCTFLOAN)
final$COSTT4_A <- as.numeric(final$COSTT4_A)
final$COST4_P <- as.numeric(final$COSTT4_P)
final$TUITIONFEE_IN <- as.numeric(final$TUITIONFEE_IN)
final$TUITIONFEE_OUT <- as.numeric(final$TUITIONFEE_OUT)
final$CDR3 <- as.numeric(final$CDR3)
final$RPY_3YR_RT <- as.numeric(final$RPY_3YR_RT)
final$C150_4 <- as.numeric(final$C150_4)

#get list of colleges
coll_list <- as.list(final$INSTNM)

loan <- final %>%
    group_by(CONTROL) %>%
    dplyr::summarize(pell = mean(PCTPELL, na.rm = TRUE),
                     nonpell = (1-mean(PCTPELL, na.rm = TRUE)),
                     floan = mean(PCTFLOAN, na.rm = TRUE),
                     nonfloan = (1-mean(PCTFLOAN, na.rm = TRUE)),
                     n = n()) %>%
    as.data.frame()

loanavg <- final %>%
    dplyr::summarize(pell = mean(PCTPELL, na.rm = TRUE),
                     nonpell = (1-mean(PCTPELL, na.rm = TRUE)),
                     floan = mean(PCTFLOAN, na.rm = TRUE),
                     nonfloan = (1-mean(PCTFLOAN, na.rm = TRUE)),
                     n = n()) %>%
    as.data.frame()

loanavg <- cbind("CONTROL" = "Average", loanavg)

loan <- rbind(loan, loanavg)

cost <- final %>%
    group_by(CONTROL) %>%
    dplyr::summarize(tuitionin = mean(TUITIONFEE_IN, na.rm = TRUE),
                     tuitionout = mean(TUITIONFEE_OUT, na.rm = TRUE)) %>%
    as.data.frame()

insttype <- c("Public (In-State)", 
              "Private Non-Profit", 
              "Private For-Profit", 
              "Public (Out of State)")
tuition <- c(9110, 29430, 14740, 20710)

cost <- data.frame(insttype, tuition)

ui <- fluidPage(
    
    #general theme
    theme = shinytheme("readable"),
    
    #title panel
    titlePanel("Exploring the Real Price Tag of For-Profit Education"),
    
#introduction text--------------------------------------------------
    fluidRow(
        column(12,
               p("Earnings are declining for workers without post-secondary education, and job opportunities for middle-skilled workers (such as clerical work or production) are declining as well. This gap in educational attainment drives wage inequality and limits financial security for workers in the middle-skill or low-skill sector. However, success in higher education is not guaranteed and is closely tied to preexisting wealth. Students from the wealthiest quintile are more likely to enroll in college and seek a four-year degree. Notably, students from the wealthiest income quintile are also much less likely to enroll in a for-profit institution than students from the least wealthy income quintile. These for-profit institutions are controversial because despite receiving federal funding, their outcomes are often worse than those for public or private non-profit institions."),
               p("The main mechanism for the federal government to support education is through Title IV funding, which provides grants or loans to eligible students attending eligible institutions. Since 1998, for-profit institutions have been subject to a restriction where only 90 percent of total institution revenue could be derived from federal funding sources such as federal grant, loan, and work-study programs. Despite this restriction, a sizeable portion of for-profit institutions still derive between 80 and 90 percent of their income from these sources, indicative of the reality that these colleges serve a large percentage of financially needy students.")
        )),
# - waffle 1 and waffle 2 PELL -------------------------------------------

sidebarLayout(
    sidebarPanel(
        h4('Select an Institution Type'),
        p('Change the default institution types using the following dropdowns:'),
        selectInput(
            inputId = 'collType3',
            label = 'Institution Type',
            choices = c('Private For-Profit',
                        'Private Non-Profit',
                        'Average',
                        'Public')),
        selectInput(
            inputId = 'collType4',
            label = 'Institution Type (Comparison)',
            choices = c('Average',
                        'Private Non-Profit',
                        'Private For-Profit',
                        'Public')),
    ),
    
    mainPanel(
        h3("Proportion of Student Population Receiving Pell Grants"),
        p('The following charts compare the proportion of students at Title-IV eligible four-year schools who are receiving Pell Grants (federal need-based grants) compared to those who are not. Each square in the chart represents one percentage point. To change the institution types for comparison, select from the dropdown menu.'),
        p('The default comparison, which shows the proportion of Pell Grant recipients at for-profit private institutions compared to the national average, confirms the fact that a higher proportion of students at for-profit schools are eligible for (and receive) need-based federal Pell Grants.'),
        fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                             plotOutput("waffle1"),
                             plotOutput("waffle2")))),
    
    position = c("left", "right"),
),

#introduction text--------------------------------------------------
fluidRow(
    column(12,
           p(),
           p("Students who cannot fund their full education with grants will often take out loans to cover the difference. Over the past ten years, outstanding student loan debt has tripled, and now is valued at $1.2 trillion. $1 trillion of that debt is backed by the federal government, which provides loans to students again through Title IV funding. Nearly sixty percent of full-time, four year students take out loans averaging $20,000, a fact that has broad macroeconomic policy implications and also imposes financial risk for both private and federal lenders.")
    )),

#second graph - waffle 3 and waffle 4 -------------------------------------------
sidebarLayout(
    sidebarPanel(
        h4('Select an Institution Type'),
        p('Change the default institution types using the following dropdowns:'),
        selectInput(
            inputId = 'collType5',
            label = 'Institution Type',
            choices = c('Private For-Profit',
                        'Private Non-Profit',
                        'Average',
                        'Public')),
        selectInput(
            inputId = 'collType6',
            label = 'Institution Type (Comparison)',
            choices = c('Average',
                        'Private Non-Profit',
                        'Private For-Profit',
                        'Public')),
    ),
    
    mainPanel(
        h3("Proportion of Student Population Receiving Federal Loans"),
        p('The following charts compare the proportion of students at Title-IV eligible four-year schools who are taking out federal loans compared to those who are not. Each square in the chart represents one percentage point. To change the institution types for comparison, select from the dropdown menu.'),
        p('The default comparison below shows that students at for-profit institutions are taking out federal loans at a substantially higher rate than the national average (and all other categories).'),
        fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                             plotOutput("waffle3"),
                             plotOutput("waffle4")))),
    
    position = c("left", "right"),
),
#introduction text--------------------------------------------------
fluidRow(
    column(12,
           p(),
           p("In addition to having a higher proportion of the student population taking out federal loans, the actual loan amounts students borrow remain quite high. This can likely be attributed to rising tuition costs in all sectors. In particular, while students who attend for-profit private institutions are similar to community college students in demographics and financial resources, they pay costs that are more similar to non-profit private institutions. Students at for-profit schools are also less likely to receive grant funding than other institution categories due to limited institutional resources.")
    )),
   
#first graph interactive -------------------------------------------------
    sidebarLayout(
        sidebarPanel(
            h4('Select an Institution'),
            p('Select (type to search) any four-year institution that is eligible for Title IV funding to compare annual tuition costs to category averages.'),
            selectInput(
                inputId = 'collType1',
                label = 'Institution Name',
                choices = coll_list)
        ),
        
        mainPanel(
            h3("Comparison of Annual Tuition Costs"),
            p('Select from the dropdown to change the comparison institution. The text below the graph displays the exact tuition amount (only in-state tuition is considered for public schools) and the institution type for the chosen comparison.'),
            p('From the comparison of the averages in each institution category, we can see that for-profit institutions have substantially higher average tuition costs than public schools, though private non-profit and out of state public school tuition are still higher on average. It is worth noting that tuition costs do not represent the full cost of attendance.'),
            plotOutput("barplot1")),
        
        position = c("left", "right"),
    ),
    
#text panel interactive -----------------------------------------------------
    fluidRow(
        column(1),
        column(10,
               uiOutput("depText")),
        column(1)
    ),

#introduction text--------------------------------------------------
fluidRow(
    column(12,
           p(),
           p("While for-profit colleges seem to charge more than their public in-state counterparts, target less-advantaged students, and eat up federal funding, the majority of the criticism they draw has to do with their poor outcomes. For-profit institutions have been linked to worse job outcomes and increased risk of defaulting on student loans, despite the amount of public funding that they consume through Title IV.")
    )),

#introduction text--------------------------------------------------
fluidRow(
    column(12,
           h3('Comparison of Outcome Metrics: Repayment Rate, Default Rate, and Completion Rate'),
           p('The following plot compares the rates for the outcomes of interest by institution type. These rates include the average three-year repayment rate (the proportion of borrowers who are not in default and are making progress towards paying them down within three years of entering repayment), the average three-year default rate (the cohort of borrowers who defaulted within three years of entering repayment) and the six-year completion rate (proportion of students who completed their degrees within six years of entering a four-year program).'),
           p('Private for-profit institutions perform worse on every metric; their repayment and completion rates are lower than average, and their default rates are higher than average.'),
    )),

#third graph - outcomes ---------- -------------------------------------------
fluidRow(column(1),
         column(10,
                plotOutput("lollipop1")),
         column(1)),
# text --- conclusions
fluidRow(
    column(12,
           h3('Conclusions'),
           p('Despite the evidence in this report that for-profit institutions target low-income students, source most of their funding from federal dollars, and worsen outcomes, these results are not novel. The limitations of for-profit institutions have been well-documented, and the Obama administration attempted to implement policies to hold these institutions more accountable.'),
           p('These policies would have required institutions to track "gainful employment" and earnings of their graduates, and cut off federal funding from those that fell short of specified targets. They resulted in a variety of lawsuits by for-profit institutions and were eventually rolled back by Betsy DeVos, the Secretary of Education under the Trump administration.'),
           p('However, this analysis shows that these rules are more important than ever, as they can provide meaningful protections for students who are lower-income or first generation, and fall into debt as a result of the high costs and poor outcomes of these institutions. It would also cut down on the financial risk for the federal government of making educational loans that graduates cannot repay given the limits of their education.')
    )),
    
#more elaboration text -----------------------------------------------------
fluidRow(
    column(12,
           h3('Data Source and Limitations'),
           p("This analysis uses data provided by the Department of Education College Scorecard (the most recently updated data, from 2017-2018) to explore the characteristics of relevant institutions. This data is an aggregation from a variety of sources, including the Integrated Postsecondary Education System (IPEDS), National Loan Student Data System (NLSDS), and administrative earnings data from tax records."),
           p('The data is restricted to only four-year institutions of higher education that receive Title IV funding; as long as the institutions receive this funding, some branch campuses are included. It is important to note that this data, particularly as it relates to demographics or outcomes, only applies to students who received Title IV funding and may not generalize to all students attending the institution. Additionally, some of the metrics, such as the default rate, are reported by the institutions themselves, and so these institutions have opportunities to artificially manipulate the data.'),
           p('Despite the possibility for gaming the results, for-profit institutions still perform worse in terms of default rates. The repayment rate and completion rate are calculated from the NSLDS data and so are not susceptible to this manipulation, and again, for-profit institutions perform worse. Additionally, given the high proportion of students at for-profit institutions receiving federal aid and the amount of federal money funneled through these institutions, limiting the data to Title IV recipients does not necessarily represent a flaw in the analysis, but rather a strong indictment of the use of federal funds at these institutions.')
    )),
)


# Define server logic
server <- function(input, output){

#barplot 1 output --------------------------------------------------------------------------------------  
    output$barplot1 <- renderPlot({
        tuit <- final %>% select(INSTNM, TUITIONFEE_IN) %>%
            filter(INSTNM == input$collType1) %>% 
            summarize(tuition = round(TUITIONFEE_IN, -1))
        newframe <- cbind(data.frame("insttype" = input$collType1), tuit)
        cost <- rbind(cost, newframe)
        cost <- cost %>% mutate(ToHighlight = ifelse(input$collType1, "yes", "no" ) )
        costbar <- ggplot(data = cost, aes(x = reorder(insttype, -tuition), 
                                           y = tuition,
                                           fill = factor(ifelse(insttype==input$collType1,
                                                                "Yes",
                                                                "No")))) +
            geom_bar(stat = "identity") +
            labs(x = 'Type of Institution',
                 y = 'Average Annual Tuition ($)') +
            theme_minimal() + 
            theme(plot.title = element_text(family = "Georgia", 
                                            size = 16, 
                                            face = "bold"),
                  axis.title = element_text(family = "Georgia", 
                                            size = 14, 
                                            face = "bold",
                                            hjust = 0.5),
                  axis.text.x = element_text(family = "Georgia",
                                           size = 12,
                                           vjust = 2)
                  ) + 
            scale_fill_manual(values=c("Yes" = "deepskyblue2", "No" = "grey50"), guide = FALSE)
        costbar

    })
    
#text output --------------------------------------------------------------------------------------        
    output$depText <- renderUI({
        singlecol <- final %>% 
            filter(INSTNM == input$collType1)
        singletuit <- singlecol$TUITIONFEE_IN
        singletype <- singlecol$CONTROL
        singletuitdol <- dollar(singletuit)
        h4(paste(input$collType1, 
                 " is a ", singletype, " institution and charges ", 
                 singletuitdol,
                 " in tuition annually. Total annual costs - including fees, books and living expenses - could be much higher.", 
                 sep = ""))
    })

# waffle 1 --------------------------------------------------------------------------------------
    output$waffle1 <- renderPlot({
        wafl <- loan %>%
            filter(CONTROL == input$collType3) %>%
            select(CONTROL, pell, nonpell) %>%
            mutate(
                pell = round(pell*100,0),
                nonpell = round(nonpell*100,0))
        
        wafl <- gather(wafl, "Status", "percent", 2:3)
        
        wafl %>%
            ggplot(aes(fill = Status, values = percent)) +
            geom_waffle(n_rows = 10, size = 0.75, colour = "white") +
            scale_fill_manual(
                name = NULL,
                values = c("grey50", "#1FAD9C"),
                labels = c("% Non-Pell Recipients", "% Pell Recipients")) +
            theme_void() +
            ggtitle(input$collType3) +
            theme(
                legend.position = "bottom",
                legend.text = element_text(family = "Georgia", size = 13),
                plot.title = element_text(family = "Georgia", 
                                          size = 16, 
                                          face = "bold",
                                          hjust = 0.5)
            )
    })

#waffle2 --------------------------------------------------------------------------------------    
    output$waffle2 <- renderPlot({
        wafl <- loan %>%
            filter(CONTROL == input$collType4) %>%
            select(CONTROL, pell, nonpell) %>%
            mutate(
                pell = round(pell*100,0),
                nonpell = round(nonpell*100,0))
        
        wafl <- gather(wafl, "Status", "percent", 2:3)
        
        wafl %>%
            ggplot(aes(fill = Status, values = percent)) +
            geom_waffle(n_rows = 10, size = 0.75, colour = "white") + 
            scale_fill_manual(
                name = NULL,
                values = c("grey50", "#1FAD9C"),
                labels = c("% Non-Pell Recipients", "% Pell Recipients")) +
            theme_void() +
            ggtitle(input$collType4) +
            theme(
                legend.position = "bottom",
                legend.text = element_text(family = "Georgia", size = 13),
                plot.title = element_text(family = "Georgia", 
                                          size = 16, 
                                          face = "bold",
                                          hjust = 0.5)
            )
    })

#waffle3 --------------------------------------------------------------------------------------
    output$waffle3 <- renderPlot({
        wafl2 <- loan %>%
            filter(CONTROL == input$collType5) %>%
            select(CONTROL, floan, nonfloan) %>%
            mutate(
                floan = round(floan*100,0),
                nonfloan = round(nonfloan*100,0))
        
        wafl2 <- gather(wafl2, "Status", "percent", 2:3)
        
        wafl2 %>%
            ggplot(aes(fill = Status, values = percent)) +
            geom_waffle(n_rows = 10, size = 0.75, colour = "white") + 
            scale_fill_manual(
                name = NULL,
                values = c("#9893DA", "grey50"),
                labels = c("% Receiving \nFederal Loans", "% Not Receiving \nFederal Loans")) +
            theme_void() +
            ggtitle(input$collType5) +
            theme(
                legend.position = "bottom",
                legend.text = element_text(family = "Georgia", size = 13),
                plot.title = element_text(family = "Georgia", 
                                          size = 16, 
                                          face = "bold",
                                          hjust = 0.5)
            )
    })

#waffle4 --------------------------------------------------------------------------------------
    output$waffle4 <- renderPlot({
        wafl2 <- loan %>%
            filter(CONTROL == input$collType6) %>%
            select(CONTROL, floan, nonfloan) %>%
            mutate(
                floan = round(floan*100,0),
                nonfloan = round(nonfloan*100,0))
        
        wafl2 <- gather(wafl2, "Status", "percent", 2:3)
        
        wafl2 %>%
            ggplot(aes(fill = Status, values = percent)) +
            geom_waffle(n_rows = 10, size = 0.75, colour = "white") + 
            scale_fill_manual(
                name = NULL,
                values = c("#9893DA", "grey50"),
                labels = c("% Receiving \nFederal Loans", "% Not Receiving \nFederal Loans")) +
            theme_void() +
            ggtitle(input$collType6) +
            theme(
                legend.position = "bottom",
                legend.text = element_text(family = "Georgia", size = 13),
                plot.title = element_text(family = "Georgia", 
                                          size = 16, 
                                          face = "bold",
                                          hjust = 0.5)
            )
    })
# lollipop --------------------------------------------------------------------
    output$lollipop1 <- renderPlot({
        outcomedata <- final %>%
            group_by(CONTROL) %>%
            dplyr::summarize(complete = mean(C150_4, na.rm = TRUE),
                             repay = mean(RPY_3YR_RT, na.rm = TRUE),
                             default = mean(CDR3, na.rm = TRUE)) %>%
            as.data.frame()

        outcomedataavg <- final %>%
            dplyr::summarize(complete = mean(C150_4, na.rm = TRUE),
                             repay = mean(RPY_3YR_RT, na.rm = TRUE),
                             default = mean(CDR3, na.rm = TRUE)) %>%
            as.data.frame()

        outcomedataavg <- cbind("CONTROL" = "Average", outcomedataavg)

        outcomedata <- rbind(outcomedata, outcomedataavg)

        outcomedata$default <- (outcomedata$default*-1)

        outcomedata <- gather(outcomedata, key = "Outcome", value = "Rate", 2:4)

        outcomedata$toHighlight <- ifelse(outcomedata$CONTROL == "Private For-Profit", "yes", "no")
        
        pal <- c(
            "yes" = "deeppink2",
            "no" = "grey50"
        )
        
        labs <- c(
            "3-Year \nRepayment Rate",
            "3-Year \nDefault Rate",
            "6-Year \nCompletion Rate"
        )
        
        outcomeloll3 <- ggplot(data = outcomedata, aes(x = Outcome, y = Rate, group = CONTROL)) + 
            geom_linerange(aes(x = Outcome, ymin = 0, ymax = Rate, colour = toHighlight), 
                           position = position_dodge(width = 1))+
            geom_point(aes(colour = toHighlight), size = 2,
                       position = position_dodge(width = 1)) +
            geom_text(aes(y = ifelse(Outcome == 'default', Rate -0.33, Rate + 0.34), 
                          label = ifelse(Outcome == 'default', paste0(CONTROL,': ',round(-Rate*100,1),'%'), paste0(CONTROL,': ',round(Rate*100,1),'%')),
                          colour = toHighlight), 
                      position = position_dodge(width = 1),
                      size = 4.3,
                      family = "Georgia") +
            geom_hline(aes(yintercept = 0), color = "grey35", size = 0.5) + 
            theme_minimal() + 
            theme(axis.title = element_text(family = 'Georgia',
                                            face = 'bold',
                                            size = 16),
                  axis.text.x = element_blank(),
                  panel.grid = element_blank(),
                  axis.text.y = element_text(family = 'Georgia',
                                             face = 'bold',
                                             size = 13),
                  axis.ticks.y = element_line(),
                  panel.background = element_rect(fill = NA),
                  legend.position = "None",
                  #axis.line = element_line()
                  ) +
            coord_flip() +
            scale_y_continuous(limits=c(-0.8, 1.4)) + 
            scale_color_manual(values=pal, limits = names(pal)) + 
            scale_x_discrete(labels=labs)
        
        outcomeloll3
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
