library(DT)
library(shiny)
library(ggplot2)
library(dplyr)
library(tibble)
library(wordcloud2)

shinyServer(function(input, output){
    # show bar chart of top cities by number of listed restaurants
    output$bar_rest <- renderPlot(ggplot(data=city_rest_cnt, aes(x=reorder(local_city, -total_cnt),y = total_cnt, fill=local_city, label=total_cnt)) +
        geom_bar(stat="identity", show.legend = FALSE) +coord_flip() +
        labs(title ="Count of listed restaurants by city in California", x ="Cities", y = "count of unique restaurants") +
            geom_label(aes(fill = local_city),colour = "white", fontface = "bold", show.legend = FALSE)
    )
    # show bar graph of top sub-menu items across the state
    output$bar_submenu = renderPlot(ggplot(data=sub_menu_data_all, aes(x=reorder(sub_menu_title, -total_count),y = total_count)) +
                                        geom_bar(stat="identity", fill="steelblue") +coord_flip() +
                                        labs(title ="Popular sub-menus(All California)", x ="sub-menu sections", y = "count across menus in CA"))
    
    # show bar graph of top sub-menu items for selected city
    output$bar_city_submenu = renderPlot(ggplot(data=filtered_set %>% filter(local_city == input$selectedCity) %>% select(., name,sub_menu_title) %>% distinct() %>% 
                                                    group_by(.,sub_menu_title)%>%summarise(.,total_count = n()) %>% arrange(.,desc(total_count)) %>%head(10), aes(x=reorder(sub_menu_title, -total_count),y = total_count)) +
                                        geom_bar(stat="identity", fill="steelblue") +coord_flip() +
                                        labs(title = paste("Popular sub-menus in",input$selectedCity) , x ="sub-menus", y = "city-wide count"))
    
    winsor_selected = reactive({
        #winsorize price data for selected input
        price_data = inner_join(filtered_set,sub_menu_data_all, by="sub_menu_title") %>% filter(local_city == input$selectedCity) %>%select(sub_menu_title, price)
        #clean outliers
        price_cut_off = price_data %>% group_by(sub_menu_title) %>% summarise(., upperB = quantile(price, probs=0.95, na.rm=T))
        price_data = inner_join(price_data,price_cut_off, by="sub_menu_title")
        price_data = price_data %>% filter((price >0.01) & price <= upperB)   
        price_data
    })
   
    
    pVal_matrix = reactive({
    #perform 2 sample t-tests on the means of like sub-menu prices across different cities
       cities_to_regress = top_cities
       if(input$selectedCity_reg != "ALL")
       {
           cities_to_regress = input$selectedCity_reg
       }
      
       ##create the master dataframe once
       master_relative_prices = setNames(data.frame(matrix(ncol = 5, nrow = 0), stringsAsFactors = FALSE), c("sub_menu_title", "p_value", "mean_ratio", "is_diff_price","local_city"))
       
       CL = (1 - as.numeric(input$selectedConfi))/2 ## 2 tailed test
       cat("alpha/2 for 2 tail-test:",CL,"\n")
       cat("user selection:",input$selectedCity_reg,"\n" )
       cat(cities_to_regress,"\n")
       cat(top_cities,"\n")
       for(test_c in cities_to_regress)
       {
           cat("\ndoing t-tests for city:",test_c,"\n")
           ##prepare test
           price_data_test = inner_join(filtered_set,sub_menu_data_all, by="sub_menu_title") %>% filter(local_city == test_c) %>%select(sub_menu_title, price)
           #clean outliers
           price_cut_off_test = price_data_test %>% group_by(sub_menu_title) %>% summarise(., upperB = quantile(price, probs=0.95, na.rm=T))
           price_data_test = inner_join(price_data_test,price_cut_off_test, by="sub_menu_title")
           price_data_test = price_data_test %>% filter((price >0.01) & price <= upperB)

           pVal_df = c()
           ratios_df = c()
           for(sm in sub_menu_ls)
           {
               control_prices = price_data_control%>% filter(sub_menu_title == sm) %>% select(price)
               test_prices    = price_data_test%>% filter(sub_menu_title == sm) %>% select(price)
               pVal_df[sm] = as.numeric((t.test(control_prices, test_prices, alternative = "two.sided")$p.value)[[1]]) #Conducting the t-test for same sub-menu across two cities
               ratios_df[sm] = mean(test_prices$price, na.rm= TRUE)/ mean(control_prices$price, na.rm = TRUE)

           }
           pVal_df = data.frame(enframe(pVal_df, name="sub_menu_title", value="p_value"), stringsAsFactors = FALSE)
           ratios_df = data.frame(enframe(ratios_df, name="sub_menu_title", value="mean_ratio"), stringsAsFactors = FALSE)

           pVal_df = inner_join(pVal_df, ratios_df, by="sub_menu_title")
           pVal_df$is_diff_price = ifelse(pVal_df$p_value < CL, "Yes", "No")

           pVal_df$mean_ratio = ifelse(pVal_df$is_diff_price == "Yes", pVal_df$mean_ratio, 1)
           pVal_df$local_city = test_c

           master_relative_prices = rbind(master_relative_prices, pVal_df)
       }
       master_relative_prices
        
    })
    
    ratings_regression = reactive({
        #run linear regression model lm(avg_price ~ ratings -1)
        #r-squared is much larger without the intercept.
        #thus regressions down without the intercept
        cities_to_regress = top_cities
        if(input$selectedCity_ratings != "ALL")
        {
            cities_to_regress = input$selectedCity_ratings
        }
        price_data_control = inner_join(filtered_set %>% filter(!is.na(ratings)),sub_menu_data_all, by="sub_menu_title") %>% filter(local_city %in% cities_to_regress) %>%
                                        select(local_city, sub_menu_title, ratings, price)
        #clean outliers
        price_cut_off_control = price_data_control %>% group_by(sub_menu_title) %>% summarise(., upperB = quantile(price, probs=0.95, na.rm=T))
        price_data_control = inner_join(price_data_control,price_cut_off_control, by="sub_menu_title")
        price_data_control = price_data_control %>% filter((price >0.01) & (price <= upperB))
        
        ratings_data = price_data_control %>% group_by(local_city, ratings) %>% summarise(avg_price = mean(price,na.rm= TRUE))
        ratings_data
 })
    
    ratings_modelOutputs = reactive({
        ##prepare regression coefficients and r-square for tabular display
        linearMod <- lm(avg_price ~ ratings -1, data=ratings_regression()) 
        modelCoeffs <- summary(linearMod)$coefficients  # model coefficients
        beta.estimate <- modelCoeffs["ratings", "Estimate"]
        pval.beta = modelCoeffs["ratings", "Pr(>|t|)"]
        adj_rsq = summary(linearMod)$adj.r.squared 
        data.frame("regression_beta1" = round(beta.estimate,5), "p_value" = pval.beta, "adjusted R-sqrd" = round(adj_rsq,5))
    })
    
    dishname_wCloud = reactive({
        #generate wordclould of words in dish names. freq set to mean price of the dish.
        #uses wordcloud2
        cities_to_cloud = top_cities
        if(input$selectedCity_wcloud != "ALL")
        {
            cities_to_cloud = input$selectedCity_wcloud
        }
        cat("cities to cloud:",cities_to_cloud,"\n")
        dishes_top_cities =  inner_join(filtered_set,sub_menu_data_all, by="sub_menu_title") 
        dishes_top_cities = dishes_top_cities%>%filter((local_city %in% cities_to_cloud) & (!is.na(dish_name) & (dish_name !="NA"))) %>% 
            select(sub_menu_title, dish_name, price, local_city) 
        clean_up = dishes_top_cities %>% group_by(sub_menu_title) %>% summarise(., upperB = quantile(price, probs=0.90, na.rm=T)) %>% filter(upperB > 20)
        dishes_top_cities = inner_join(dishes_top_cities, clean_up, by="sub_menu_title")
        dishes_top_cities = dishes_top_cities %>% filter(price >= upperB) %>% group_by(dish_name) %>% 
            summarise(cnt=n(), avg_price = mean(price,na.rm =TRUE))%>% select(word=dish_name,freq=avg_price)  
        dishes_top_cities$word = tolower(dishes_top_cities$word)
        dishes_top_cities
    })
    
  
    recommendation_engine = reactive({
        ##all comes down to this.
        #use regression coefficients, adjust mean prices per menu across cities normalized to prices in Los Angeles
        #present table  of recommended prices based on customer initial input of location, restaurant rating, menu prices
        master_relative_prices = setNames(data.frame(matrix(ncol = 5, nrow = 0), stringsAsFactors = FALSE), c("sub_menu_title", "p_value", "mean_ratio", "is_diff_price","local_city"))
        test_c = tolower(unique(customer_data$city))
       
        ##prepare test
        price_data_test = inner_join(filtered_set,sub_menu_data_all, by="sub_menu_title") %>% filter(local_city == test_c) %>%select(sub_menu_title, price)
        #clean outliers
        price_cut_off_test = price_data_test %>% group_by(sub_menu_title) %>% summarise(., upperB = quantile(price, probs=0.95, na.rm=T))
        price_data_test = inner_join(price_data_test,price_cut_off_test, by="sub_menu_title")
        price_data_test = price_data_test %>% filter((price >0.01) & price <= upperB)
        
        pVal_df = c()
        ratios_df = c()
        control_mean = c()
        for(sm in sub_menu_ls)
        {
            control_prices = price_data_control%>% filter(sub_menu_title == sm) %>% select(price)
            test_prices    = price_data_test%>% filter(sub_menu_title == sm) %>% select(price)
            pVal_df[sm] = as.numeric((t.test(control_prices, test_prices, alternative = "two.sided")$p.value)[[1]]) #Conducting the t-test for same sub-menu across two cities
            ratios_df[sm] = mean(test_prices$price, na.rm= TRUE)/ mean(control_prices$price, na.rm = TRUE)
            control_mean[sm] =  mean(control_prices$price, na.rm = TRUE)
        }
        pVal_df = data.frame(enframe(pVal_df, name="sub_menu_title", value="p_value"), stringsAsFactors = FALSE)
        ratios_df = data.frame(enframe(ratios_df, name="sub_menu_title", value="mean_ratio"), stringsAsFactors = FALSE)
        control_mean_df = data.frame(enframe(control_mean, name="sub_menu_title", value="control_mean"), stringsAsFactors = FALSE)
        
        pVal_df = inner_join(pVal_df, ratios_df, by="sub_menu_title")
        pVal_df = inner_join(pVal_df, control_mean_df, by="sub_menu_title")
        
        pVal_df$is_diff_price = ifelse(pVal_df$p_value < pricing_CL, "Yes", "No")
        
        pVal_df$mean_ratio = ifelse(pVal_df$is_diff_price == "Yes", pVal_df$mean_ratio, 1)
        pVal_df$local_city = test_c
        
        master_relative_prices = rbind(master_relative_prices, pVal_df)
        
        master_relative_prices = inner_join(master_relative_prices, customer_data, by="sub_menu_title")
        
        ###now get the regression beta for the city
        price_data_control = inner_join(filtered_set %>% filter(!is.na(ratings)),sub_menu_data_all, by="sub_menu_title") %>% filter(local_city == test_c) %>%select(local_city, sub_menu_title, ratings, price)
        
        #clean outliers
        price_cut_off_control = price_data_control %>% group_by(sub_menu_title) %>% summarise(., upperB = quantile(price, probs=0.95, na.rm=T))
        price_data_control = inner_join(price_data_control,price_cut_off_control, by="sub_menu_title")
        price_data_control = price_data_control %>% filter((price >0.01) & price <= upperB)
        
        ratings_data = price_data_control %>% group_by(local_city, ratings) %>% summarise(avg_price = mean(price,na.rm= TRUE))
        linearMod <- lm(avg_price ~ ratings -1, data=ratings_data) 
        modelCoeffs <- summary(linearMod)$coefficients  # model coefficients
        beta.estimate <- modelCoeffs["ratings", "Estimate"]
        pval.beta = modelCoeffs["ratings", "Pr(>|t|)"]
        master_relative_prices$beta.ratio = (master_relative_prices$ratings * ifelse(pval.beta < pricing_CL, beta.estimate,0))/master_relative_prices$control_mean
        master_relative_prices$weighted_adj = 0.5*(master_relative_prices$mean_ratio + master_relative_prices$beta.ratio)
        
        #winsorize price data for selected input and get range for control city
        price_data = inner_join(filtered_set,sub_menu_data_all, by="sub_menu_title") %>% filter(local_city == control_c) %>%select(sub_menu_title, price)
        #clean outliers
        price_cut_off = price_data %>% group_by(sub_menu_title) %>% summarise(., upperB = quantile(price, probs=0.95, na.rm=T))
        price_data = inner_join(price_data,price_cut_off, by="sub_menu_title")
        price_data = price_data %>% filter((price >0.01) & price <= upperB) %>% group_by(sub_menu_title) %>% 
            summarise(control.lower = min(price, na.rm = T), control.upper = max(price, na.rm =T))
        
        master_relative_prices = inner_join(master_relative_prices, price_data, by="sub_menu_title")
        
        master_relative_prices$recommended.lower = round(master_relative_prices$weighted_adj* master_relative_prices$control.lower,2)
        master_relative_prices$recommended.upper = round(master_relative_prices$weighted_adj* master_relative_prices$control.upper,2)
        
        master_relative_prices = master_relative_prices %>% select(sub_menu_title, Cust.Lower=Price.Lower, Cust.Upper = Price.Upper, Recommended.Lower= recommended.lower, Recommended.Upper = recommended.upper)
        master_relative_prices
        }
    )
        
    plotRecommendation = reactive({
        #same function logic as recommendationEngine about except plot recommended price range against customer's initial price points
        master_relative_prices = setNames(data.frame(matrix(ncol = 5, nrow = 0), stringsAsFactors = FALSE), c("sub_menu_title", "p_value", "mean_ratio", "is_diff_price","local_city"))
        test_c = tolower(unique(customer_data$city))
        
        ##prepare test
        price_data_test = inner_join(filtered_set,sub_menu_data_all, by="sub_menu_title") %>% filter(local_city == test_c) %>%select(sub_menu_title, price)
        #clean outliers
        price_cut_off_test = price_data_test %>% group_by(sub_menu_title) %>% summarise(., upperB = quantile(price, probs=0.95, na.rm=T))
        price_data_test = inner_join(price_data_test,price_cut_off_test, by="sub_menu_title")
        price_data_test = price_data_test %>% filter((price >0.01) & price <= upperB)
        
        pVal_df = c()
        ratios_df = c()
        control_mean = c()
        for(sm in sub_menu_ls)
        {
            control_prices = price_data_control%>% filter(sub_menu_title == sm) %>% select(price)
            test_prices    = price_data_test%>% filter(sub_menu_title == sm) %>% select(price)
            pVal_df[sm] = as.numeric((t.test(control_prices, test_prices, alternative = "two.sided")$p.value)[[1]]) #Conducting the t-test for same sub-menu across two cities
            ratios_df[sm] = mean(test_prices$price, na.rm= TRUE)/ mean(control_prices$price, na.rm = TRUE)
            control_mean[sm] =  mean(control_prices$price, na.rm = TRUE)
        }
        pVal_df = data.frame(enframe(pVal_df, name="sub_menu_title", value="p_value"), stringsAsFactors = FALSE)
        ratios_df = data.frame(enframe(ratios_df, name="sub_menu_title", value="mean_ratio"), stringsAsFactors = FALSE)
        control_mean_df = data.frame(enframe(control_mean, name="sub_menu_title", value="control_mean"), stringsAsFactors = FALSE)
        
        pVal_df = inner_join(pVal_df, ratios_df, by="sub_menu_title")
        pVal_df = inner_join(pVal_df, control_mean_df, by="sub_menu_title")
        
        pVal_df$is_diff_price = ifelse(pVal_df$p_value < pricing_CL, "Yes", "No")
        
        pVal_df$mean_ratio = ifelse(pVal_df$is_diff_price == "Yes", pVal_df$mean_ratio, 1)
        pVal_df$local_city = test_c
        
        master_relative_prices = rbind(master_relative_prices, pVal_df)
        
        master_relative_prices = inner_join(master_relative_prices, customer_data, by="sub_menu_title")
        
        ###now get the regression beta for the city
        price_data_control = inner_join(filtered_set %>% filter(!is.na(ratings)),sub_menu_data_all, by="sub_menu_title") %>% filter(local_city == test_c) %>%select(local_city, sub_menu_title, ratings, price)
        
        #clean outliers
        price_cut_off_control = price_data_control %>% group_by(sub_menu_title) %>% summarise(., upperB = quantile(price, probs=0.95, na.rm=T))
        price_data_control = inner_join(price_data_control,price_cut_off_control, by="sub_menu_title")
        price_data_control = price_data_control %>% filter((price >0.01) & price <= upperB)
        
        ratings_data = price_data_control %>% group_by(local_city, ratings) %>% summarise(avg_price = mean(price,na.rm= TRUE))
        linearMod <- lm(avg_price ~ ratings -1, data=ratings_data) 
        modelCoeffs <- summary(linearMod)$coefficients  # model coefficients
        beta.estimate <- modelCoeffs["ratings", "Estimate"]
        pval.beta = modelCoeffs["ratings", "Pr(>|t|)"]
        master_relative_prices$beta.ratio = (master_relative_prices$ratings * ifelse(pval.beta < pricing_CL, beta.estimate,0))/master_relative_prices$control_mean
        master_relative_prices$weighted_adj = 0.5*(master_relative_prices$mean_ratio + master_relative_prices$beta.ratio)
        
        #winsorize price data for selected input and get range for control city
        price_data = inner_join(filtered_set,sub_menu_data_all, by="sub_menu_title") %>% filter(local_city == control_c) %>%select(sub_menu_title, price)
        #clean outliers
        price_cut_off = price_data %>% group_by(sub_menu_title) %>% summarise(., upperB = quantile(price, probs=0.95, na.rm=T))
        price_data = inner_join(price_data,price_cut_off, by="sub_menu_title")
        price_data = price_data %>% filter((price >0.01) & price <= upperB) %>% group_by(sub_menu_title) %>% 
            summarise(control.lower = min(price, na.rm = T), control.upper = max(price, na.rm =T))
        
        master_relative_prices = inner_join(master_relative_prices, price_data, by="sub_menu_title")
        
        master_relative_prices$recommended.lower = round(master_relative_prices$weighted_adj* master_relative_prices$control.lower,2)
        master_relative_prices$recommended.upper = round(master_relative_prices$weighted_adj* master_relative_prices$control.upper,2)
        
        master_relative_prices = master_relative_prices %>% select(sub_menu_title, Cust.Lower=Price.Lower, Cust.Upper = Price.Upper, Recommended.Lower= recommended.lower, Recommended.Upper = recommended.upper)
        
        custPrices_df = data.frame(matrix(ncol=2, nrow=0))
        colnames(custPrices_df) = c("sub_menu_title","price")
        
        recommendPrices_df = data.frame(matrix(ncol=2, nrow=0))
        colnames(recommendPrices_df) = c("sub_menu_title","price")
        cat("number of rows:",nrow(master_relative_prices),"\n")
        for(idx in 1:nrow(master_relative_prices))
        {
            sub_menu_title = master_relative_prices[idx,"sub_menu_title"]
            price = c(master_relative_prices[idx,"Cust.Lower"],master_relative_prices[idx,"Cust.Upper"])
            temp_df = data.frame(sub_menu_title, price, stringsAsFactors = FALSE) 
            custPrices_df= rbind(custPrices_df, temp_df)
            
            
            price = c(master_relative_prices[idx,"Recommended.Lower"], master_relative_prices[idx,"Recommended.Upper"])
            temp_df = data.frame(sub_menu_title, price, stringsAsFactors = FALSE) 
            recommendPrices_df = rbind(recommendPrices_df,temp_df)
        }
        custPrices_df$party = "Customer Price"
        recommendPrices_df$party = "Recommended Price"
        
        allPrices = rbind(custPrices_df, recommendPrices_df)
        allPrices
        
    })

      
    output$violin_city_prices = renderPlot(#violin plot of mean prices across cities
        winsor_selected() %>% ggplot(aes(x = sub_menu_title, y = price)) + geom_violin(aes(fill = sub_menu_title)) +coord_flip() +
            labs(title = paste("Price distribution sub-menus:",input$selectedCity), x ="sub-menus", y = "meal price($)")
    )
    
    output$regressOut = renderPlot(#regression dot plot of yes or no
        pVal_matrix() %>% ggplot(aes( x = sub_menu_title, y = is_diff_price )) + geom_point(aes(color=is_diff_price), show.legend = F) + coord_flip() +
            facet_wrap(~local_city) + labs(title = "Dot plot showing whether mean of submenu prices in a particular city differ significantly from mean of like submenus in Los Angeles")
    )
    
    output$ratingsOut = renderPlot(#plot regression line with SE for avg_price ~ ratings regression
        ratings_regression() %>% ggplot( aes(x = ratings, y = avg_price)) + geom_point(aes(color = local_city)) + geom_smooth(method = lm, se=T)
    )
    
    output$recommendedPlot = renderPlot(##plot recommended price against customer suggested price
        plotRecommendation() %>% ggplot(aes(x=sub_menu_title,y=price, color = party)) + geom_line(size =2 , position=position_dodge(width=0.4)) +coord_flip()
    )
    
    output$wcloudOut = renderWordcloud2({#wordcloud output requires it's on special call
        dishname_wCloud() %>% wordcloud2()
    })
    
    # show regression data using DataTable
    output$betaTable <- DT::renderDataTable({
        datatable(ratings_modelOutputs(), rownames=FALSE)
    })
    
    # show customer input data using DataTable
    output$customerInput <- DT::renderDataTable({
        datatable(customer_data, rownames=FALSE)
    })
    
    # show recommended data using DataTable
    output$recommendedTable <- DT::renderDataTable({
        datatable(recommendation_engine(), rownames=FALSE)
    })
    
    
    # show statistics using infoBox
    output$rest_total <- renderInfoBox({
        total_rest <- filtered_set %>% select(url) %>% distinct()%>% summarise(n=n())
        infoBox("Restaurants", total_rest, icon = icon("hand-o-up"))
    })
    output$city_cnt <- renderInfoBox({
        city_total <- filtered_set %>% select(uber_city) %>% filter(endsWith(uber_city, "california")) %>% distinct() %>% summarise(n=n())
        infoBox("Cities analyzed", city_total, icon = icon("hand-o-up"))
    })
    output$total_submenus <- renderInfoBox({
        submenu_cnt = filtered_set %>% select(sub_menu_title) %>% distinct()%>% summarise(n=n())
        infoBox("Unique submenus",
                submenu_cnt, 
                icon = icon("hand-o-up"))
    })
    
    output$total_dishes <- renderInfoBox({
        dishes_cnt = filtered_set %>% select(dish_name) %>% distinct()%>% summarise(n=n())
        infoBox("Unique dishes",
                dishes_cnt, 
                icon = icon("hand-o-up"))
    })
    output$state_cnt <- renderInfoBox({
        state_cnt = filtered_set %>% select(state) %>% distinct()%>% summarise(n=n())
        infoBox("States analyzed",
                state_cnt, 
                icon = icon("hand-o-up"))
    })
    
    output$top_city <- renderInfoBox({
        top_city = (filtered_set %>% select(name, local_city) %>% distinct() %>% group_by(local_city) %>% summarise(n=n()) %>% top_n(1))$local_city
        infoBox("Top city by restaurant count",
                top_city, 
                icon = icon("hand-o-up"))
    })
    
})