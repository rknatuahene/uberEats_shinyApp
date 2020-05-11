
filtered_set = read.csv("data/CA_data.csv", stringsAsFactors = FALSE)  ##master clean dataset
customer_data = read.csv("data/customer_feed.csv", stringsAsFactors = F)  ##customer input for recommendation

#limit analysis to top 10 city in California by restaurant count
city_rest_cnt = filtered_set %>% select(.,name,local_city) %>% distinct() %>% group_by(.,local_city) %>% summarise(., total_cnt = n()) %>% filter(total_cnt > 100)%>%top_n(10, total_cnt)

#limit to top ten submenus by popularity
sub_menu_data_all = filtered_set %>% select(., name,sub_menu_title) %>% distinct() %>% 
  group_by(.,sub_menu_title)%>%summarise(.,total_count = n()) %>% arrange(.,desc(total_count)) %>%head(10)


#data sets used for regression 
sub_menu_ls = strsplit(paste0(sub_menu_data_all$sub_menu_title,collapse = "|"),"|", fixed= TRUE)[[1]] #get the top sub-menu categories

top_cities = strsplit(paste0(city_rest_cnt$local_city,collapse = "|"),"|", fixed= TRUE)[[1]] #get the top cities

control_c = "los angeles"
##prepare control
price_data_control = inner_join(filtered_set,sub_menu_data_all, by="sub_menu_title") %>% filter(local_city == control_c) %>%select(sub_menu_title, price)
#clean outliers
price_cut_off_control = price_data_control %>% group_by(sub_menu_title) %>% summarise(., upperB = quantile(price, probs=0.95, na.rm=T))
price_data_control = inner_join(price_data_control,price_cut_off_control, by="sub_menu_title")
price_data_control = price_data_control %>% filter((price >0.01) & price <= upperB)

##menu pricing - recommendation down at the 95% CL for the 2 sample 2-test
pricing_CL = 0.05/2 #2 sample t-test of equality of means

#end of data sets used for regression