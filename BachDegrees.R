# Intro to Network Analysis
# Final Project
# College Majors Over the Years

# Import dataset
# To avoid any complications, removing "Multi/interdisciplinary studies", and "Other and not classified"
library(readxl)
bachdegrees <- read_excel("W_Fall22Classes/Network Analysis/bachdegrees.xls")

# Creating career clusters

# Agriculture, Food, and Natural Resources
# 'Agriculture' includes Agricultural, animal, plant, veterinary science, and related fields; and Natural resources and conservation
agriculture <- c("Agriculture and natural resources")

# Architecture and Construction
architecture <- c("Architecture and related services", "Precision production")

# Arts, Audio/Video Technology, and Communications
artsAndCom <- c("Communication, journalism, and related programs", "Communications technologies",
                "Visual and performing arts")

# Business, Management, and Administration
# ('Business' includes Business, management, marketing, and related support services; and Culinary, entertainment, and personal services)
business <- c("Business")

# Education and Training
education <- c("Education", "Library science")

# Government and Public Administration
government <- c("Public administration and social services", "Social sciences and history")

# Health Science
healthScience <- c("Biological and biomedical sciences", "Health professions and related programs",
                   "Physical sciences and science technologies", "Psychology")

# Human Services
humanService <- c("Area, ethnic, cultural, gender, and group studies", "Family and consumer sciences/human sciences",
                  "Parks, recreation, leisure, fitness, and kinesiology")

# Law, Public Safety, Corrections, and Security
law <- c("Homeland security, law enforcement, and firefighting", "Legal professions and studies")

# Liberal Arts
liberalArts <- c("English language and literature/letters", "Foreign languages, literatures, and linguistics", 
"Liberal arts and sciences, general studies, and humanities", "Philosophy and religious studies", "Theology and religious vocations")

# Science, Technology, Engineering, and Mathematics
# 'Engineering Technologies' includes Engineering technologies and engineering-related fields; Construction trades;
# and Mechanic and repair technologies/technicians.
stem <- c("Engineering", "Computer and information sciences and support services", "Engineering technologies",
          "Mathematics and statistics", "Military technologies and applied sciences")

# Transportation, Distribution, and Logistics
transportation <- c("Transportation and materials moving")

# Create column to categorize university majors by cluster
bachdegrees1 <- bachdegrees
bachdegrees1 <- within(bachdegrees1, {
  group = 1
  group[field %in% agriculture] = 1
  group[field %in% architecture] = 2
  group[field %in% artsAndCom] = 3
  group[field %in% business] = 4
  group[field %in% education] = 5
    group[field %in% government] = 6
    group[field %in% healthScience] = 7
    group[field %in% humanService] = 8
    group[field %in% law] = 9
    group[field %in% liberalArts] = 10
    group[field %in% stem] = 11
    group[field %in% transportation] = 12
})

# Over the different graphs, should see a change in what majors have the most 
# representation, and the highest number cluster should all/mostly be STEM careers.

# Create 4 separate datasets for 1990, 2000, 2010, 2020
library(dplyr)

degs90 <- bachdegrees1 %>% 
  mutate(numgrads = `1990-91`) %>%
  select(field, numgrads, grooup)

degs00 <- bachdegrees1 %>% 
  mutate(numgrads = `2000-01`) %>%
  select(field, numgrads, group)

degs10 <- bachdegrees1 %>%  
  mutate(numgrads = `2010-11`) %>%
  select(field, numgrads, group)

degs20 <- bachdegrees1 %>% 
  mutate(numgrads = `2019-20`) %>%
  select(field, numgrads, group)

# Building graphs
library(igraph)
# Create a Palette for all graphs to differentiate between categories
pal <- rainbow(n=length(unique(bachdegrees1$group)))

# 1990 GRAPH
fields90 = degs90 %>% select(field)
relations = degs90 %>% 
  mutate(field2 = degs90$field)

for (i in unique(select(degs90,group))$group){
  from = relations %>%
    filter(group == i) %>%
    select(field)
  
  to = relations %>%
    filter(group == i) %>%
    select(field2)
  
  # Form relationships between all students in each group
  if (i == 1){edge_list90 = tidyr::crossing(from, to)} 
  else {edge_list90 = bind_rows(edge_list90, tidyr::crossing(from, to))}
}

# Prevent self-loop edges and duplicate relationships
edge_list90 = edge_list90 %>% filter(field != field2) 
edge_list90 = edge_list90[!duplicated(t(apply(edge_list90, 1, sort))), ]

# Create graph and add the number of grads as an attribute
deg90_graph <- graph_from_data_frame(edge_list90, directed = FALSE, vertices = fields90)
deg90_graph <- set_vertex_attr(deg90_graph, "grads", value = degs90$numgrads)
deg90_graph <- set_vertex_attr(deg90_graph, "group", value = degs90$group)

# Add vertex attributes for grouping
V(deg90_graph)$color <- pal[degs90$group]
plot(deg90_graph, vertex.label.color = "black")
vertex.attributes(deg90_graph)

# Separate into 3 groups based on grad size
range(degs90$numgrads)

# bottom third 
low90_graph <- delete_vertices(deg90_graph, V(deg90_graph)[grads > 8000])

# middle third
mid90_graph <- delete_vertices(deg90_graph, V(deg90_graph)[grads < 8000])
mid90_graph <- delete_vertices(mid90_graph, V(mid90_graph)[grads >= 26000])

# top third
high90_graph <- delete_vertices(deg90_graph, V(deg90_graph)[grads < 26000])

# Plot all three graphs side by side
par(mfrow = c(1, 3))
plot(low90_graph, vertex.label.color = "black")
plot(mid90_graph, vertex.label.color = "black")
plot(high90_graph, vertex.label.color = "black")


#
#
#

# 2000'S GRAPHS

fields00 = degs00 %>% select(field)
relations00 = degs00 %>% 
  mutate(field2 = degs00$field)

for (i in unique(select(degs00,group))$group){
  from = relations %>%
    filter(group == i) %>%
    select(field)
  
  to = relations %>%
    filter(group == i) %>%
    select(field2)
  
  # Form relationships between all students in each group
  if (i == 1){edge_list00 = tidyr::crossing(from, to)} 
  else {edge_list00 = bind_rows(edge_list00, tidyr::crossing(from, to))}
}

# Prevent self-loop edges and duplicate relationships
edge_list00 = edge_list00 %>% filter(field != field2) 
edge_list00 = edge_list00[!duplicated(t(apply(edge_list00, 1, sort))), ]

# Create graph and add the number of grads as an attribute
deg00_graph <- graph_from_data_frame(edge_list00, directed = FALSE, vertices = fields00)
deg00_graph <- set_vertex_attr(deg00_graph, "grads", value = degs00$numgrads)
deg00_graph <- set_vertex_attr(deg00_graph, "group", value = degs00$group)

# Add vertex attributes for grouping
V(deg00_graph)$color <- pal[degs00$group]
plot(deg00_graph, vertex.label.color = "black")
vertex.attributes(deg00_graph)

# Separate into 3 groups based on grad size
range(degs00$numgrads)

# bottom third 
low00_graph <- delete_vertices(deg00_graph, V(deg00_graph)[grads > 10000])

# middle third
mid00_graph <- delete_vertices(deg00_graph, V(deg00_graph)[grads < 10000])
mid00_graph <- delete_vertices(mid00_graph, V(mid00_graph)[grads >= 38000])

# top third
high00_graph <- delete_vertices(deg00_graph, V(deg00_graph)[grads < 38000])

# Plot all three graphs side by side
par(mfrow = c(1, 3))
plot(low00_graph, vertex.label.color = "black")
plot(mid00_graph, vertex.label.color = "black")
plot(high00_graph, vertex.label.color = "black")

#
#
#

# 2010'S GRAPHS

fields10 = degs10 %>% select(field)
relations10 = degs10 %>% 
  mutate(field2 = degs10$field)

for (i in unique(select(degs10,group))$group){
  from = relations %>%
    filter(group == i) %>%
    select(field)
  
  to = relations %>%
    filter(group == i) %>%
    select(field2)
  
  # Form relationships between all students in each group
  if (i == 1){edge_list10 = tidyr::crossing(from, to)} 
  else {edge_list10 = bind_rows(edge_list10, tidyr::crossing(from, to))}
}

# Prevent self-loop edges and duplicate relationships
edge_list10 = edge_list10 %>% filter(field != field2) 
edge_list10 = edge_list10[!duplicated(t(apply(edge_list10, 1, sort))), ]

# Create graph and add the number of grads as an attribute
deg10_graph <- graph_from_data_frame(edge_list10, directed = FALSE, vertices = fields10)
deg10_graph <- set_vertex_attr(deg10_graph, "grads", value = degs10$numgrads)
deg10_graph <- set_vertex_attr(deg10_graph, "group", value = degs10$group)

# Add vertex attributes for grouping
V(deg10_graph)$color <- pal[degs10$group]
plot(deg10_graph, vertex.label.color = "black")
vertex.attributes(deg10_graph)

# Separate into 3 groups based on grad size
range(degs10$numgrads)

# bottom third 
low10_graph <- delete_vertices(deg10_graph, V(deg10_graph)[grads > 15000])

# middle third
mid10_graph <- delete_vertices(deg10_graph, V(deg10_graph)[grads < 15000])
mid10_graph <- delete_vertices(mid10_graph, V(mid10_graph)[grads >= 47000])

# top third
high10_graph <- delete_vertices(deg10_graph, V(deg10_graph)[grads < 47000])

# Plot all three graphs side by side
par(mfrow = c(1, 3))
plot(low10_graph, vertex.label.color = "black")
plot(mid10_graph, vertex.label.color = "black")
plot(high10_graph, vertex.label.color = "black")

#
#
#

# 2020'S GRAPHS

fields20 = degs20 %>% select(field)
relations20 = degs20 %>% 
  mutate(field2 = degs20$field)

for (i in unique(select(degs20,group))$group){
  from = relations %>%
    filter(group == i) %>%
    select(field)
  
  to = relations %>%
    filter(group == i) %>%
    select(field2)
  
  # Form relationships between all students in each group
  if (i == 1){edge_list10 = tidyr::crossing(from, to)} 
  else {edge_list10 = bind_rows(edge_list10, tidyr::crossing(from, to))}
}

# Prevent self-loop edges and duplicate relationships
edge_list10 = edge_list10 %>% filter(field != field2) 
edge_list10 = edge_list10[!duplicated(t(apply(edge_list10, 1, sort))), ]

# Create graph and add the number of grads as an attribute
deg20_graph <- graph_from_data_frame(edge_list10, directed = FALSE, vertices = fields20)
deg20_graph <- set_vertex_attr(deg20_graph, "grads", value = degs20$numgrads)
deg20_graph <- set_vertex_attr(deg20_graph, "group", value = degs20$group)

# Add vertex attributes for grouping
V(deg20_graph)$color <- pal[degs20$group]
plot(deg20_graph, vertex.label.color = "black")
vertex.attributes(deg20_graph)

# Separate into 3 groups based on grad size
range(degs20$numgrads)

# bottom third 
low20_graph <- delete_vertices(deg20_graph, V(deg20_graph)[grads > 12000])

# middle third
mid20_graph <- delete_vertices(deg20_graph, V(deg20_graph)[grads < 12000])
mid20_graph <- delete_vertices(mid20_graph, V(mid20_graph)[grads >= 55000])

# top third
high20_graph <- delete_vertices(deg20_graph, V(deg20_graph)[grads < 55000])

# Plot all three graphs side by side
par(mfrow = c(1, 3))
plot(low20_graph, vertex.label.color = "black")
plot(mid20_graph, vertex.label.color = "black")
plot(high20_graph, vertex.label.color = "black")

# Further Details
# Ratio of STEM grads to total grads
totalgrads90 = sum(degs90$numgrads)
stemGrads90 <- degs90 %>% filter(group == 11)
totalgrads90 / sum(stemGrads90$numgrads)

# Year 2000
totalgrads00 = sum(degs00$numgrads)
stemGrads00 <- degs00 %>% filter(group == 11)
totalgrads00 / sum(stemGrads00$numgrads)

# Year 2010
totalgrads10 = sum(degs10$numgrads)
stemGrads10 <- degs10 %>% filter(group == 11)
totalgrads10 / sum(stemGrads10$numgrads)

# Year 2020
totalgrads20 = sum(degs20$numgrads)
stemGrads20 <- degs20 %>% filter(group == 11)
totalgrads20 / sum(stemGrads20$numgrads)

# Grad growth vs. STEM growth
grad1 <- ((sum(degs00$numgrads) - sum(degs90$numgrads)) / sum(degs90$numgrads)) * 100
grad2 <- ((sum(degs10$numgrads) - sum(degs00$numgrads)) / sum(degs00$numgrads)) * 100
grad3 <- ((sum(degs20$numgrads) - sum(degs10$numgrads)) / sum(degs10$numgrads)) * 100

mean(grad1, grad2, grad3)
# Total grad growth rate avg: 14.42%

stem1 <- ((sum(stemGrads00$numgrads) - sum(stemGrads90$numgrads)) / sum(stemGrads90$numgrads)) * 100
stem2 <- ((sum(stemGrads10$numgrads) - sum(stemGrads00$numgrads)) / sum(stemGrads00$numgrads)) * 100
stem3 <- ((sum(stemGrads20$numgrads) - sum(stemGrads10$numgrads)) / sum(stemGrads10$numgrads)) * 100

mean(stem1, stem2, stem3)
# Total grad growth rate avg: 7.30%


