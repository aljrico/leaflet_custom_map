library(leaflet)
library(tidyverse)
library(shiny)
library(data.table)

movingPlugin <- htmltools::htmlDependency("Leaflet.heat", "99.99.99",
                             src = c(href = "https://github.com/ewoken/Leaflet.MovingMarker"),
                             script = "movingMarker.js"
)

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

origin <- list(
  longitude = c(15, 20),
  latitude = c(70, 75)
)

reactiveTrigger <- function() {
  counter <- reactiveVal(0)
  list(
    depend = function() {
      counter()
      invisible()
    },
    trigger = function() {
      counter(isolate(counter()) + 1)
    }
  )
}

counter <- R6::R6Class(
  public = list(
    initialize = function(reactive = FALSE) {
      private$reactive <- reactive
      private$value <- 0
      private$rxTrigger <- reactiveTrigger()
    },
    trigger = function() {
      if (private$reactive) private$rxTrigger$trigger()
      private$value <- private$value + 1
    },
    react = function() {
      if (private$reactive) private$rxTrigger$depend()
      return(private$value)
    }
  ),
  private = list(
    reactive = NULL,
    value = NULL,
    rxTrigger = NULL
  )
)
rabbit_dna = data.table(id = 1, speed = 1, lifespan = 10, vision_distance = 1, max_horny = 10, species = 'rabbit')
wolf_dna = data.table(id = 2, speed = 3, lifespan = 100, vision_distance = 2000, max_horny = 2, species = 'wolf')
rabbits_population <- 
  R6::R6Class(
  public = list(
    death_count = 0,
    dna = rbindlist(list(rabbit_dna, wolf_dna)),
    status = NULL,
    initialize = function(){
      first_rabbit = data.table(id = 1, longitude = -20, latitude = 70, life = rabbit_dna$lifespan, horny = rabbit_dna$max_horny, flee = FALSE, hunt = FALSE, target = NA)
      first_wolf = data.table(id = 2, longitude = -15, latitude = 70, life = wolf_dna$lifespan, horny = wolf_dna$max_horny, flee = FALSE, hunt = FALSE, target = NA)
      self$status = rbindlist(list(first_rabbit, first_wolf))
    },
    die = function(who){
      self$status[id == who, life := 0]
      self$status = self$status[life > 0]
      self$death_count = self$death_count + 1
    },
    scan_surroundings = function(who){
      lng = self$status[id == who]$longitude
      lat = self$status[id == who]$latitude
      max_vision = self$dna[id == who]$vision_distance
      max_horny = self$dna[id == who]$max_horny
      this_animal = self$dna[id == who]$species
      
      self$status[,distance := sqrt((longitude - lng)^2 + (latitude - lat)^2)]
      in_sight = self$status[distance > 0 & distance < max_vision]$id
      if(length(in_sight) > 0){
        number_foes = nrow(self$dna[id %in% in_sight & species != this_animal])
        number_friends = nrow(self$dna[id %in% in_sight & species == this_animal])
        in_sight_status = self$status[id %in% in_sight]
        target_id = in_sight_status[which.min(distance)]$id
        self$status[, target := as.numeric(target)][id == who, target := target_id]
        if(number_friends > 0) self$status[id == who, horny := min(horny + 1, max_horny)]
        if(number_foes   == 0) {
          if(this_animal == 'rabbit') self$status[id == who, flee := FALSE]
          if(this_animal == 'wolf')   self$status[id == who, hunt := FALSE]
        }else{
          print('Enemy spotted!')
          if(this_animal == 'rabbit') self$status[id == who, flee := TRUE]
          if(this_animal == 'wolf')   self$status[id == who, hunt := TRUE]
        }
      }else{
        self$status[id == who, target := NA]
      }
      self$status[, distance := NULL]
    },
    jump = function(who){
      r = self$dna[id == who]$speed
      angle = 2 * pi * runif(1, 0, 360) / 360
      closest_target = self$status[id == who]$target
      
      if(!is.na(closest_target)){
        compute_distance <- function(subject_id, target_id){
          target_lng <- self$status[id == target_id]$longitude
          target_lat <- self$status[id == target_id]$latitude
          
          y = target_lng - self$status[id == subject_id]$longitude
          x = target_lat - self$status[id == subject_id]$latitude
          
          distance = sqrt(x ^ 2 + y ^ 2)
          target_angle = atan(y/x)
          return(list(distance = distance, target_angle = target_angle))
        }
        
        distance = compute_distance(who, closest_target)$distance
        target_angle = compute_distance(who, closest_target)$target_angle
        # 
        # target_lng <- self$status[id == closest_target]$longitude
        # target_lat <- self$status[id == closest_target]$latitude
        # 
        # y = target_lng - self$status[id == who]$longitude
        # x = target_lat - self$status[id == who]$latitude
        # 
        # distance = sqrt(x ^ 2 + y ^ 2)
        # 
        # target_angle = atan(y/x)
        if(self$status[id == who]$flee){
          angle = target_angle
        } else if(self$status[id == who]$hunt){
          print('Attack!')
          
          
          angle = target_angle + pi
          r = min(distance, r)
          
            if(distance < 0.05){
              self$die(closest_target)
              print('Rabbit Killed!')
            }
        
          # new_distance = Inf 
          # while(distance < new_distance){
          #   angle = 2 * pi * runif(1, 0, 360) / 360
          # 
          #   new_lat = r * cos(angle) + self$status[id == who]$latitude
          #   new_lng = r * sin(angle) + self$status[id == who]$longitude
          #   
          #   y = target_lng - new_lng
          #   x = target_lat - new_lat
          #   
          #   new_distance = sqrt(x ^ 2 + y ^ 2)
          #   print(new_distance)
          #   if(new_distance < 0.05){
          #     self$die(closest_target)
          #     print('Rabbit Killed!')
          #     break
          #   }
          # }
          # r = min(new_distance, r)
          

        }
        
      }


      lat_movement = r * cos(angle)
      lng_movement = r * sin(angle)
      
      self$status[id == who, longitude := longitude + lat_movement]
      self$status[id == who, latitude  := latitude  + lng_movement]
      self$status[id == who, life := life - 1]
      if(self$status[id == who]$life == 0) self$die(who)
    },
    spawn = function(who){
      
      horny_level = self$status[id == who]$horny
      if(any(runif(horny_level) < 0.05)){
        # New individual ID
        new_id = max(self$status$id) + 1
        
        # Retrieve parent DNA
        parent_dna = self$dna[id == who]
        offspring_dna = parent_dna[, id := new_id]
        self$dna = rbindlist(list(offspring_dna, self$dna))
        
        # Retrieve parent status
        parent_status = self$status[id == who]
        offspring_status = parent_status[, id := new_id]
        offspring_status[, life := self$dna[id == new_id]$lifespan]
        self$status = rbindlist(list(offspring_status, self$status))
        
        # Off you go, little child
        self$jump(new_id)
        
        # No longer horny
        self$status[id == who, horny := 0]
      }
      
    }
    
  )
)

rabbits <- rabbits_population$new()
triggers <- reactiveValues()

trigger <- function(name) {
  if (is.null(triggers[[name]])) {
    triggers[[name]] <<- 0
  } else {
    triggers[[name]] <<- triggers[[name]] + 1
  }
}



ui <- fluidPage(  
  sliderInput("time_step", 
              "date",
              1, 
              1e2,
              value = 1,
              step=1,
              animate=TRUE
              ),
  leafletOutput("mymap")
)

server <- function(input, output, session) {
  
  observeEvent(input$time_step, {
    trigger('movement')
  })
  
  observeEvent(triggers$movement, {

    # Update positions
    for(i in rabbits$status$id){
      if(nrow(rabbits$status[id == i]) == 0) next
      rabbits$scan_surroundings(i)
      rabbits$spawn(i)
      rabbits$jump(i)
    }
    trigger("markers")


  })

  output$mymap <- renderLeaflet({
    
    tile_url <- "map/{z}/{x}/{y}.png"
    leaflet(options = leafletOptions(worldCopyJump = F)) %>%
      setView(mean(origin[[1]]), mean(origin[[2]]), 10) %>%
      addTiles(
        urlTemplate = tile_url,
        options = tileOptions(minZoom = 0, 
                              maxZoom = 4, 
                              noWrap = TRUE, 
                              continuousWorld = FALSE)
          
      ) %>% 
      registerPlugin(movingPlugin) %>% 
      setMaxBounds(lng1 = 45,  lat1 = 85,
                   lng2 = -180, lat2 = 35 )
  })

  observeEvent(triggers$markers, {
    
    icons <- 
      awesomeIconList(
        rabbit = makeAwesomeIcon(icon = 'bullseye', library = 'fa', markerColor = 'blue'),
        wolf = makeAwesomeIcon(icon = 'caret-up', library = 'fa', markerColor = 'orange')
        )

    # Get coordinates
    coords <- as.data.frame(rabbits$status[,c('id', 'longitude', 'latitude')])
    markers_data <- merge(rabbits$status, rabbits$dna, by = 'id', all.x = TRUE)[, c('id', 'species', 'longitude', 'latitude')]

    leafletProxy("mymap", data = markers_data) %>%
      clearMarkers() %>%
      addAwesomeMarkers(
                 lng = ~longitude, 
                 lat = ~latitude,
                 icon = ~icons[species])
    
    # Sys.sleep(10)

    # trigger("movement")
  })
  
}
shinyApp(ui, server)