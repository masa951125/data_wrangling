library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)

tab <- h %>% html_nodes("table")
tab <- tab[[2]]
tab <- tab %>% html_table()

tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)

h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()

guacamole <- list(recipe, prep_time, ingredients)
guacamole

get_recipe <- function(url){
  h <- read_html(url)
  recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
  prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
  ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
  return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
} 

get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")

#Baseball data
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
html_table(nodes[[8]])
(nodes[[1]])
html_table(nodes[1:4])

sapply(nodes[1:4], html_table)

sapply(nodes[19:21], html_table)

tab1 <- html_table(nodes[[10]])
tab2 <- html_table(nodes[[19]])

new_tab1 <-tab1[-1,-1]
new_tab2 <-tab2[-1,]
new_tab1 <- new_tab1 %>% setNames(c("Team", "Payroll", "Average"))
new_tab2 <- new_tab2 %>% setNames(c("Team", "Payroll", "Average"))

nrow(full_join(new_tab1, new_tab2, by="Team"))

url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"

brexit <- read_html(url)
tab <-html_nodes(brexit, "table")

tab1 <- html_table(tab, fill = TRUE)

sapply(tab1, ncol)

str(html_table(tab[[5]], fill=T))

str(sapply(tab, colnames))


