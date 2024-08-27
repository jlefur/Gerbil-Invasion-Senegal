extensions [gis]

Globals [
  ;;ENVIRONMENT
  raster-data;; input raster
  vecteur-data;; input vector file
  list-saison-des-pluies;;list of months and duration of rainy seasons (2 values per year)
  list-nb-month-gestation;; Gestation period depends on rainfall. There is a number of months per rainy year.
  list-coordonnees-x;; list of x-coordinates for all populations
  list-coordonnees-y;; list of y-coordinates for all populations

  ;;GERBILS
  %-gestantes-month;;% of pregnant females per month (number used in the ‘reproduce’ procedure)
  monthly-distance-par-month;; distance travelled per month by propagules (nb patches)
  destination;; if true, population movement is possible
  temoin-fusion;; if true, merger case
  list-2-distances ;; at each iteration for each population, 2 distances: distance of the good patch and covered distance by month
  list-%gestantes;;% of pregnant females per month from start of the breeding period (August) until propagule departure
  rotate;; possible amplitude of a random direction (340°, i.e. -170;+170 degrees; <360° to avoid staying in one place)
  position-initiale-x-E;; eastern initial position
  position-initiale-x-W;; western initial position
  position-initiale-y-S;; southern initial position
  position-initiale-y-N;; northern initial position

  ;; TEMPORALITE
  debut-cycle;; allows to enter the reproduction phase
  periode-reprod;; indicates whether it's the breeding season
  month-debut-reproduction;; 1st month of breeding season
  month-fin-saison-des-pluies;; month at the end of the rainy season (1st month without rain)
  duree-reproduction-apres-fin-pluie;; duration of reproduction (number of months) after the end of the rainy season
  month-fin-reproduction;; end of breeding period
  month-fin-reproduction+1;; month following the end of breeding period
  month-fin-cycle;; useful for determining the end of cycle date (1st month after the departure of the last propagules of the year)

  ;; RESULTATS
  expansion-est;; maximum horizontal distance to east (= max(end position) - init position)(the scene's origin is bottom-left.)
  expansion-ouest;; horizontal distance to west (= min(position-finale) - position init)(the scene's origin is bottom-left.)
  expansion-sud;; maximum vertical distance to south (= position-init - min(position-finale))(the scene's origin is bottom-left.)
  expansion-nord;; maximum vertical distance (= position-init - min(position-finale))(the scene's origin is bottom-left.)
  ]

Patches-own [
  env-limits;;country limits
  env-river;; Senegal river
  env-occup_sol;; class Synmap reclassified
  env-ndvi;; Ndvi value (NOAA files - from 8km->4km resampling)
  env-pluvio;; monthly rainfall qty (mm)(GPCC data from January 1982 ro December 1998 - TRMM 3B42 from January 1999 to December 2013)
  env-pedo;6 soil classes: 1 clay; 2 other; 3 sand; 4 s-a; 5 NR; 6 sea-water (from IRD 1971 soil map)
  env-qualite-habitat;;habitat quality suitable for settlement
  env-qualite-habitat-ndvi; 1 when ndvi favorable
  env-qualite-habitat-land-cover;  1 when landcover favorable
  env-qualite-habitat-pedo;   1 when pedology favorable
  patch_sur_place;  patch on which the gerbil population is located
]

Breed [gerbils Gerbil]
gerbils-own [
  nb-males
  nb-females
  nb-jeunes
  list-females-gestantes
   ;; For each gerbil population, last values of the number of pregnant females (to calculate the number of young at the time of departure).
  ;; For each month, we store the last 4 values for pregnant females (corresponding to 1 month of reproduction, 2 months of gestation, then month of departure).
  ;; The first value (item0) is used to calculate the number of young at the time of departure.
  etat ;; reproduction (rainy season), sexual rest (dry season), exploration (propagules status)
  isStelled? ;; to determine whether a population has arrived or not
  age ;; number of months
  isMerged? ;; merge indicator (true: population stays and merges; false: population is merged with another population)
]

to setup
  __clear-all-and-reset-ticks ;;;clear all
  setup-variables
   ;; INITIAL LOADING OF LIMITS
  proc-env-limits
  proc-env-river

  ;; INITIAL LOADING OF ENVIRONMENT
  ;; SYNMAP grid loading
   proc-env-landcover
  ;; pedology loading
   proc-env-pedo
  ;; detection of preferential habitat
   proc-env-habitat

  ;; DISPLAY
   ;;environmental data display
   utility-display-data
   ;;display gerbil populations according to their initial position
  if nb-populations != 0 [
    ifelse Position? ="NW" [proc-gerbils-init-position 50 80 3 0][;;NW
     ifelse Position? ="NE" [proc-gerbils-init-position  80 80 3 0][;;NE
        ifelse Position? ="E" [proc-gerbils-init-position 105 40 0 3][;;E
           ifelse Position? ="S" [proc-gerbils-init-position 34 14 3 0][;;S
             ifelse Position? ="NW-NE" [proc-gerbils-init-position 50 80 3 0 proc-gerbils-init-position  80 80 3 0][;; NW et NE
              ifelse Position? ="NW-E" [proc-gerbils-init-position 50 80 3 0 proc-gerbils-init-position 105 40 0 3 ][;;NW et E
               ifelse Position? ="NE-E" [proc-gerbils-init-position  80 80 3 0 proc-gerbils-init-position 105 40 0 3][;;NE et E
                 ifelse Position? ="NW-S" [proc-gerbils-init-position 50 80 3 0 proc-gerbils-init-position 34 14 3 0 ][;;NW et S
                   ifelse Position? ="NE-S" [proc-gerbils-init-position  80 80 3 0 proc-gerbils-init-position 34 14 3 0 ][;;NE et S
                    ifelse Position? ="E-S" [proc-gerbils-init-position 105 40 0 3 proc-gerbils-init-position 34 14 3 0  ][;;E et S
  ]]]]]]]]]]]
end
;================================================GO=====================================================================================================


to go
  tick
 if ticks = 385 [stop];; end of simulation (january 1982 - december 2013)

;;====================================TIME DISPLAY======================================================================================================
  utility-display-temporality

;;===================================ENVIRONMENT========================================================================================================
  ;; rain data - GPCC (january 82-december 97) - TRMM 3B42 (january 98 - december 2013)
  proc-env-rainfall-monthly
  ;; NDVI grid - NOAA-GIMMS - January 1982 - December 2013
  proc-env-ndvi
  ;; annual loading of rainy season dates
 ;; for each year, the rainy season dates are used to determine the dates of the breeding period and cycle (until the propagules leave)
   if month-num = 8 [proc-env-rainfall-annual]
  ;; preferential habitat
  proc-env-habitat

;;=============================================GERBILS===============================================================================================
ifelse nb-populations != 0  [;;  at least one population
 ;; TO REPRODUCE
    ;;*************information on temporality (cycle, reproduction period)***********************************
  if month-num  = month-debut-reproduction [
    set debut-cycle true
    set periode-reprod true
    ask gerbils with [etat != "exploration"][;; mobile propagules do not reproduce; their reproductive phase begins only after installation.
    set etat "reproduction"
    set color red
    ]]
  if month-num  = month-fin-cycle and month-num < month-debut-reproduction [;;cycle: beginning of reproduction (in August) until the end of the young's departure (before August of the following year)
    set debut-cycle false]
  if month-num  = month-fin-reproduction[;;period-reprod: stops at the end of reproduction
    set periode-reprod false]

     ;;****************reproduction**********************************************************************************
 if debut-cycle [
   ask gerbils with [etat != "exploration"][;; mobile propagules do not reproduce
   proc-gerbils-reproduce]]
   proc-gerbils-propagules-departures

  ;;TO MOVE
  ;; distance traveled by propagules (function of patch size, here 4 km*4km)
    set monthly-distance-par-month round(monthly-distance / 3885);; "monthly-distance” interface item _ pixels are rather 3.80*3.97, i.e. an average of 3.885...)
    ask gerbils with [etat = "exploration"] [
    proc-gerbils-to-move];; to move

 ;; TO MERGE
  ;; Settled populations merge if they are in the same place
    proc-gerbils-merge_on_patch

  ;; AGE
    proc-gerbils-age

  ;; TO DIE
  proc-gerbils-die

  ;; DISPLAY
  utility-display-data
  utilitaire-expansion]

[;; DISPLAY
  utility-display-data]
end


;===========================================INITIALISATION-INPUTS====================================================================================================
;; global variables

to  setup-variables
  ;; TEMPORALITY
  set year 1981
  set month "Initialisation"
  set month-num 0
  set debut-cycle false
  set  list-coordonnees-x []
  set  list-coordonnees-y []
  set rotate 170; random direction between -170° and 170° (variable used with the plage-random utility)

  ;; RAINFALL/REPRODUCTION
  ;; 2 values per year (the start of reproduction (August) and the end of the rainy season (i.e. 1st month without rain)).
  set list-saison-des-pluies [8 11 8 11 8 11 8 11 8 11 8 11 8 11 8 11 8 11 8 11 8 12 8 12 8 11 8 11 8 11 8 11 8 11 8 11 8 12 8 12 8 11 8 11 8 12 8 11 8 11 8 12 8 11 8 12 8 10 8 11 8 11 8 11]
  ;; nb of months with gestation after the end of the rainy season
  set list-nb-month-gestation [2 1 2 2 2 2 3 3 2 2 2 3 1 3 2 2 2 4 3 3 2 4 3 4 3 2 4 4 5 2 4 4]
  ;; % pregnant females from start of breeding season (August) (see article)
  set list-%gestantes [6 36 34 27 14 17 31 6 14 10 0 3]
end



to proc-env-limits  ;;limits (sea and country): 0: sea; 1: Gambia 2: Mauritania 3: Senegal 4: Mali
 set raster-data gis:load-dataset "me-limits.asc"
 resize-world 0 (gis:width-of raster-data - 1) 0 (gis:height-of raster-data - 1)
 gis:set-world-envelope gis:envelope-of raster-data
 gis:apply-raster raster-data env-limits
end

to proc-env-river  ;; river Senegal
 set raster-data gis:load-dataset "me-river.asc"
 resize-world 0 (gis:width-of raster-data - 1) 0 (gis:height-of raster-data - 1)
 gis:set-world-envelope gis:envelope-of raster-data
 gis:apply-raster raster-data env-river
end

to proc-env-landcover  ;; SYNMAP grid
 set raster-data gis:load-dataset "me-synmap.asc"
 resize-world 0 (gis:width-of raster-data - 1) 0 (gis:height-of raster-data - 1)
 gis:set-world-envelope gis:envelope-of raster-data
 gis:apply-raster raster-data env-occup_sol
 ;; habitat quality - landcover
 ask patches with [env-occup_sol = 1 or env-occup_sol = 16] [set env-qualite-habitat-land-cover 3];;water, buildings
 ask patches with [env-occup_sol = 8 or env-occup_sol = 11 or env-occup_sol = 14 ] [set env-qualite-habitat-land-cover 1 ];;
 ask patches with [ (env-occup_sol >= 2 and env-occup_sol <= 7) or env-occup_sol = 9 or  env-occup_sol = 10 or env-occup_sol = 12 or  env-occup_sol = 13 or  env-occup_sol = 15] [set env-qualite-habitat-land-cover 2]
end

to proc-env-pedo ;; pedology
 set raster-data gis:load-dataset "me-pedo.asc"
 resize-world 0 (gis:width-of raster-data - 1) 0 (gis:height-of raster-data - 1)
 gis:set-world-envelope gis:envelope-of raster-data
 gis:apply-raster raster-data env-pedo
  ;; habitat quality - pedology
  ask patches with [env-pedo = 6] [set env-qualite-habitat-pedo 3];;water
  ask patches with [env-pedo = 2  or env-pedo = 3 or env-pedo = 4] [set env-qualite-habitat-pedo 1 ];;other (2) or sand (3) or sand-clay (4)
  ask patches with [env-pedo = 1  or env-pedo = 5] [set env-qualite-habitat-pedo 3 ];;clay (1) ou undetermined (5)
  ask patches with [env-pedo < 1  or env-pedo > 6] [set env-qualite-habitat-pedo 3];;pedology not specified
end

to proc-env-ndvi  ;; Monthly NDVI grid loading - NOAA data - January 82 December 2013
  let nom-fic word "ndvi/NOAA-"ticks
  set raster-data gis:load-dataset word nom-fic".asc"
  resize-world 0 (gis:width-of raster-data - 1) 0 (gis:height-of raster-data - 1)
  gis:set-world-envelope gis:envelope-of raster-data
  gis:apply-raster raster-data env-ndvi
 ;; habitat quality - NDVI
  ask patches [
   ifelse month-num >= 11 or month-num <= 6 [;;  quality 1 if ndvi between 19 and 24 in dry season
      ifelse env-ndvi >= NDVI_min and env-ndvi <= NDVI_max [set env-qualite-habitat-ndvi 1 ][set env-qualite-habitat-ndvi 3]]
    [set env-qualite-habitat-ndvi 1;; in the wet season, all ndvi are accepted; see article
 ]]
end

to  proc-env-habitat  ;; habitat quality (1, 2 or 3)
  if habitat-ndvi? [
     ask patches with [env-limits != 0][
       ifelse env-qualite-habitat-ndvi = 1 [set env-qualite-habitat 1] [set env-qualite-habitat 3]
   ]]
  if habitat-land-cover? [
     ask patches with [env-qualite-habitat-land-cover = 1 and env-limits != 0] [set env-qualite-habitat 1]
     ask patches with [env-qualite-habitat-land-cover = 2 and env-limits != 0] [set env-qualite-habitat 2]
     ask patches with [env-qualite-habitat-land-cover = 3 and env-limits != 0] [set env-qualite-habitat 3]
   ]
 if habitat-pedo? [
     ask patches with [env-qualite-habitat-pedo = 1 and env-limits != 0] [set env-qualite-habitat 1]
     ask patches with [env-qualite-habitat-pedo = 3 and env-limits != 0] [set env-qualite-habitat 3]
   ]
 if habitat-lc-pedo? [
      ask patches with [env-limits != 0][
        ifelse env-qualite-habitat-land-cover = 3 [;; cases where the land-cover is not habitable (water, buildings)
        set env-qualite-habitat 3] [
        ifelse env-qualite-habitat-land-cover = 1 and env-qualite-habitat-pedo = 1 [set env-qualite-habitat 1] [set env-qualite-habitat 3]
         ]]]

if habitat-lc-ndvi? [
      ask patches with [env-limits != 0][
        ifelse env-qualite-habitat-land-cover = 3 [;; cases where the land-cover is not habitable (water, buildings)
        set env-qualite-habitat 3] [
        ifelse env-qualite-habitat-land-cover = 1 and env-qualite-habitat-ndvi = 1 [set env-qualite-habitat 1] [set env-qualite-habitat 3]
          ]]]
if habitat-pedo-ndvi?  [
        ask patches with [env-limits != 0][
        ifelse env-qualite-habitat-land-cover = 3 [;; cases where the land-cover is not habitable (water, buildings)
        set env-qualite-habitat 3] [
        ifelse env-qualite-habitat-pedo = 1 and env-qualite-habitat-ndvi = 1 [set env-qualite-habitat 1] [set env-qualite-habitat 3]
         ]]]

if habitat-ndvi-landcover-pedo? [
      ;; question of combining criteria (good land-cover, good ndvi, good pedology)
     ask patches with [env-limits != 0][
        ifelse env-qualite-habitat-land-cover = 3 [;; cases where the land-cover is not habitable (water, buildings)
        set env-qualite-habitat 3] [
     ifelse env-qualite-habitat-land-cover = 1 and env-qualite-habitat-ndvi = 1 and env-qualite-habitat-pedo = 1 [set env-qualite-habitat 1] [set env-qualite-habitat 3]
      ]]]
end

to proc-env-rainfall-monthly  ;; Monthly loading of rainfall grid - GPCC (january 82-december 97) - TRMM 3B42 (january 98 - december 2013)
   let nom-fic word "pluvio/pluvio-"ticks
   set raster-data gis:load-dataset word nom-fic".asc"
   resize-world 0 (gis:width-of raster-data - 1) 0 (gis:height-of raster-data - 1)
   gis:set-world-envelope gis:envelope-of raster-data
   gis:apply-raster raster-data env-pluvio
end

to proc-env-rainfall-annual;;  loading of rainfall data and end of breeding period
  ;;Rainfall
  ;; month and duration of the rainy season for the current year
  set month-debut-reproduction first list-saison-des-pluies;; August : first month of procreation
  set list-saison-des-pluies remove-item 0 list-saison-des-pluies;; update start of rainy season for following year  (1er item)
  set month-fin-saison-des-pluies first list-saison-des-pluies
  set list-saison-des-pluies remove-item 0 list-saison-des-pluies;; update end of rainy season for following year  (2nd item)

  ;;Rainfall and breeding month
  ;; end of reproduction depends on rainfall (1 to 5 months after the end of the rainy season (1st month without rain))
  ;; duration of the gestation period for the current year. August is the first month of reproduction
  set duree-reproduction-apres-fin-pluie first list-nb-month-gestation
  set list-nb-month-gestation remove-item 0 list-nb-month-gestation;; update “list-nb-month-gestation” list for next year (1st item)
  set month-fin-reproduction  month-fin-saison-des-pluies + duree-reproduction-apres-fin-pluie

  set month-fin-reproduction  month-fin-reproduction mod 12
  set month-fin-reproduction+1  (month-fin-reproduction  + 1) mod 12
  set month-fin-cycle   (month-fin-reproduction + 3)  mod 12
  if month-fin-reproduction = 0 [;; when the month of end of reproduction is December
    set month-fin-reproduction 12]
  if month-fin-cycle = 0 [;;when the end-of-cycle month is December
    set month-fin-cycle 12]
end

to proc-gerbils-init-position[position-x position-y increment-x increment-y];;initial population loading according to x y position and horizontal and vertical spacing
   ;;the 4 arguments are: horizontal position, vertical position, horizontal distance between 2 populations, vertical distance between 2 populations

  create-gerbils nb-populations[
      set nb-males nb-init-males
      set nb-females nb-init-males
      set list-females-gestantes [0 0 0 0]
      set etat "estivation"
      set age 1
      set color black

      set position-x position-x + increment-x
      set position-y position-y + increment-y
      setxy position-x position-y
      set size 1
      set isStelled? false
      set isMerged? false

  set list-coordonnees-x lput position-x list-coordonnees-x;; all positions x
  set list-coordonnees-y lput position-y list-coordonnees-y];; all positions y
  set position-initiale-x-E max list-coordonnees-x;; position-initiale-x-E calculates expansion from the easternmost point
  set position-initiale-x-W min list-coordonnees-x;; position-initiale-x-W calculates expansion from the westernmost  point
  set position-initiale-y-S min list-coordonnees-y;; position-initiale-y-S calculates expansion from the southernmost  point
  set position-initiale-y-N max list-coordonnees-y;; position-initiale-y-N calculates expansion from the northernmost   point
end

;=========================================== Behavioral procedures for gerbil populations ===================================================================================
to proc-gerbils-reproduce
   ;; this procedure calculates the number of young from the number of pregnant females 3 months before departure (1 month of reproduction, 2 months of gestation then month of departure).

  utility-pregnant_females;; During the breeding season, this utility updates the list of pregnant females for each month.
  if not periode-reprod or etat = "estivation"[;; in this case, there are no pregnant gerbils
               set list-females-gestantes lput 0 list-females-gestantes
               set etat "estivation"
               set color yellow
               ]
  set list-females-gestantes remove-item 0 list-females-gestantes;;list update: after adding the month's value, we remove the oldest (item 0).

;; adjustment of values according to month: the % of pregnant gerbils in September is greater than (or equal to) that in August and October. (cf article)
if month-num = 9 [
    if item 3 list-females-gestantes < item 2 list-females-gestantes [;; nb September < nb August
    set list-females-gestantes replace-item  3 list-females-gestantes  item 2 list-females-gestantes ; then September = August
  ]]
if month-num = 10 [
  if item 3 list-females-gestantes > item 2 list-females-gestantes [;; nb October > nb September
  set list-females-gestantes replace-item  3 list-females-gestantes  item 2 list-females-gestantes ; then October = September
  ]]

;; Number of young (based on 1st element of list of gestating females and litter-size reproduction rate)
          let a first list-females-gestantes
          set nb-jeunes ceiling(litter-size * a)
          if (nb-jeunes > 0 and nb-jeunes < 1)[set nb-jeunes 1];; no population fraction!
end

to proc-gerbils-propagules-departures
  ;; this procedure creates propagule populations determined by the % of propagules; those remaining increase the sedentary population.
  ask gerbils with [nb-jeunes != 0] [;;population is created as soon as there are young people (i.e. over 2 months)
    ;; a new gerbil population is born; it inherits its parents' characteristics
        hatch-gerbils 1  [;;new propagule population
           rt utilitaire-plage-random -180 180 ;;random choice of direction; otherwise, the young population will have the same direction as its parents!
           set nb-females ceiling((nb-jeunes / 2) * %-propagules)
           set nb-males ceiling((nb-jeunes / 2) * %-propagules)
           if (nb-males > 0 and nb-males < 1) or (nb-females > 0 and nb-females < 1) [
              set nb-females 1 set nb-males 1];; no population fraction!
           set etat "exploration"
           set color blue
           set nb-jeunes 0
           set list-females-gestantes [0 0 0 0]
           set age 0
           set isStelled? false
           set isMerged? false
          ]
    ;; case of young gerbils that don't leave : settled population grows
         set nb-males ceiling(nb-males + (nb-jeunes / 2) * (1 - %-propagules))
         set nb-females ceiling(nb-females + (nb-jeunes / 2) * (1 - %-propagules))
         set nb-jeunes 0
         if (nb-males > 0 and nb-males < 1) or (nb-females > 0 and nb-females < 1) [
            set nb-females 1 set nb-males 1];; no population fraction!
         ;; some of these young females also become pregnant
         ;;  we remove the last value from list-gestantes (item 3) and recalculate it (in utility-pregnant_females)
         if month-num !=  month-fin-reproduction and etat = "reproduction"[
         set list-females-gestantes remove-item 3 list-females-gestantes]
         utility-pregnant_females
  ]
end

to proc-gerbils-to-move
  ;;  This procedure detects the nearest preferential habitat;
  ;; if it's accessible (i.e. less than monthly-distance-month), the propagule population settles there.
  ;; Otherwise, it moves forward by “monthly-distance-month” in a random direction.
  ;; The proc-gerbils-contour obstacle procedure prevents a population from going to sea!

    set isStelled? false
    set list-2-distances []
   let HabitatLePlusProche min-one-of patches with [env-qualite-habitat = 1]  [distance myself];; habitat le plus proche
   ifelse habitatLePlusProche = nobody [user-message (word "Choose a preferential habitat criterion before running the simulation...Also check that Ndvi Max is greater than Ndvi Min...")][

 ;;  Can be moved (not to the edge of the grid)
 if patch-ahead monthly-distance-par-month != nobody or patch-ahead distance habitatLePlusProche != nobody[

;; First advance
     ifelse age = 0 [;; first time, young propagules are advanced by distance covered
       proc-gerbils-avoid_obstacle
       fd monthly-distance-par-month;;
       ask patch-here [ifelse env-qualite-habitat = 1 [set patch_sur_place 1][set patch_sur_place 0]]
       ifelse patch_sur_place = 1 [set isStelled? true][set isStelled? false]
      ][

;; Movement
     ask patch-here [ifelse env-qualite-habitat = 1  [set patch_sur_place 1][set patch_sur_place 0 ]]
     ifelse patch_sur_place = 1 [set isStelled? true][set isStelled? false]
   if not isStelled? [
      ifelse distance habitatLePlusProche <= monthly-distance-par-month [;; the nearest preferential habitat is accessible (less than monthly-distance-par-month)
            face habitatLePlusProche
            reset-timer;; measures the time it takes to bypass an obstacle and avoid blockages in the event of encirclement
            proc-gerbils-avoid_obstacle
            fd distance habitatLePlusProche;;propagule populations move to and settle in accessible habitat
            if env-qualite-habitat = 1 [set isStelled? true
              ask  patch-here [set patch_sur_place 1
   ]]][
      ;; the nearest preferential habitat is not accessible  : propagule populations continue on their way...
     let opp-rotate -1 *  rotate
      rt utilitaire-plage-random  opp-rotate rotate;; random orientation
      reset-timer;; measures the time it takes to bypass an obstacle and avoid blockages in the event of encirclement
     proc-gerbils-avoid_obstacle
     fd monthly-distance-par-month
     set isStelled? false
  ]]]]]

 if isStelled? [;; in this case, the propagule population is settled ; status = reproduction if it's the breeding season, otherwise status = exual rest.
      ifelse periode-reprod [
        set etat "reproduction"
        set color red
   ][
        set etat "estivation"
        set color yellow
        ]]
end

to proc-gerbils-avoid_obstacle
  ;; This procedure tests the existence of an obstacle (sea for example). If it is encountered, the population changes direction.

  set monthly-distance-par-month monthly-distance / 3885;; each pixel is almost 4 km on a side
  let habitatLePlusProche min-one-of patches with [env-qualite-habitat-ndvi = 1]  [distance myself]
  let distance-habitat-le-plus-proche distance habitatLePlusProche
  set list-2-distances lput monthly-distance-par-month list-2-distances
  set list-2-distances lput distance-habitat-le-plus-proche list-2-distances
  let distance-obstacle min list-2-distances
  if patch-ahead distance-obstacle != nobody [
      ask patch-ahead distance-obstacle[;; avoiding obstacles
         ifelse env-limits = 0 [;; In this case, the target area is the sea
         set destination false][
         set destination true
         ]]
      if not destination [;; on encountering an obstacle, change direction (15-degree rotation)
        rt 15
        if timer > 5 [die]; after 5 seconds, die. before 5 seconds, change direction (rt 15)
         proc-gerbils-avoid_obstacle
     ]]
end


to proc-gerbils-merge_on_patch;;this procedure tests whether there are several populations on a patch.
  ;;If so, these populations merge into one and their numbers accumulate.


  ask gerbils [
    set isMerged? false
    let nb count gerbils-on patch-here - 1
  if nb != 0  [;; case of several populations on a patch
      set isMerged? true
      ;;initialize the numbers of the merged population
      let age-total 0
      let nb-tot-males 0
      let nb-tot-females 0
      let nb-tot-jeunes 0
      let nb-females-gest1 0
      let nb-females-gest2 0
      let nb-females-gest3 0
      let nb-females-gest4 0
      let etat-population etat

 ;; Loop of merging populations
 ask gerbils-here with [not isMerged?];;
    [set  nb-tot-females  nb-tot-females + nb-females
     set nb-tot-males nb-tot-males + nb-males
     set nb-tot-jeunes  nb-tot-jeunes +  nb-jeunes
     set nb-females-gest1 nb-females-gest1 + item 0 list-females-gestantes
     set nb-females-gest2 nb-females-gest2 + item 1 list-females-gestantes
     set nb-females-gest3 nb-females-gest3 + item 2 list-females-gestantes
     set nb-females-gest4 nb-females-gest4 + item 3 list-females-gestantes
     set age round(age * (nb-females + nb-males + nb-jeunes))
     set age-total age-total + age

     ifelse etat = "reproduction" [set etat-population "reproduction"][
       ifelse etat = "estivation" [set etat-population "estivation"][
       ]]
    die;; the population has merged
  ]
      ;;updated attributes of the merged population
 set nb-males nb-males + nb-tot-males
 set nb-females nb-females + nb-tot-females
 set nb-jeunes nb-jeunes + nb-tot-jeunes
 set nb-females-gest1 nb-females-gest1 + item 0 list-females-gestantes
 set nb-females-gest2 nb-females-gest2 + item 1 list-females-gestantes
 set nb-females-gest3 nb-females-gest3 + item 2 list-females-gestantes
 set nb-females-gest4 nb-females-gest4 + item 3 list-females-gestantes
 let list1 replace-item  0 list-females-gestantes nb-females-gest1
 let list2 replace-item  1 list1 nb-females-gest2
 let list3 replace-item 2 list2 nb-females-gest3
 set list-females-gestantes replace-item  3 list3 nb-females-gest4
 set nb-females-gest1 0
 set nb-females-gest2 0
 set nb-females-gest3 0
 set nb-females-gest4 0
 set age round(( age + age-total ) / (nb-males + nb-females + nb-jeunes));; age taking into account population size
 set etat etat-population
 ifelse etat = "reproduction" [set color red][ifelse etat = "estivation" [set color yellow][set color blue]]
 set isMerged? false
 ]]
end

to proc-gerbils-age
  ask gerbils [
   set age age + 1;; 1 month at each iteration
    ]
end

to proc-gerbils-die;;each month, the numbers of each population take into account the survival rate
  ask gerbils[
   set nb-males round(survival-rate * nb-males)
   set nb-females round(survival-rate * nb-females)
   set nb-jeunes round(survival-rate * nb-jeunes)
   ]

 ask gerbils [
  if nb-males = 0 or nb-females = 0 [
     die]
  ]
 ;; explorers in perdition... die after 1 year
ask gerbils with [etat = "exploration"][
if age >= 12 [die]]
end


;===========================================  DISPLAY and UTILITIES============================================================================================================

to  utility-display-temporality; this utility displays the month at each iteration
 set month-num  month-num + 1
 set month-num month-num mod 12
 if month-num = 0 [set month-num 12]
 ifelse month-num = 1 [set month "January"][
      ifelse month-num = 2 [set month "February"][
          ifelse month-num = 3 [set month "March"][
              ifelse month-num = 4 [set month "April"][
                  ifelse month-num = 5 [set month "May"][
                      ifelse month-num = 6 [set month "June"][
                          ifelse month-num = 7 [set month "July"][
                              ifelse month-num = 8 [set month "August"][
                                  ifelse month-num = 9 [set month "September"][
                                      ifelse month-num = 10 [set month "October"][
                                          ifelse month-num = 11 [set month "November"][
                                              ifelse month-num = 12 [set month "December"][
                                                set month-num "0"]
                                          ]]]]]]]]]]]
  if month-num = 1  [set year year + 1]
end

to utility-display-data;  this utility displays environmental data and preferential habitats according to the interface choices.
  if limits? [
    ;; limit display - Diva-Gis file
  ask patches with [env-limits = 0] [set pcolor blue + 3];;sea
  ask patches with [env-limits = 1] [set pcolor white - 1];;Gambia
  ask patches with [env-limits = 2] [set pcolor white - 1];; Mauritania
  ask patches with [env-limits = 3] [set pcolor white ];;Senegal
  ask patches with [env-limits = 4] [set pcolor white - 1];;Mali
  ]

   if Ndvi? [
  ;; NDVI display - NOAA-AVHRR-GIMMS 82-2013
  ;; WINDISP classification
  ask patches with [env-ndvi > -1 and env-ndvi < -0.076] [set pcolor sky + 3];blue ; water
   ask patches with [env-ndvi >= -0.076 and env-ndvi < 0.052 ] [set pcolor white];;white - no vegetation - "bare soil"
   ask patches with [env-ndvi >= 0.052 and env-ndvi < 0.156] [set pcolor  9];;white - minimum vegetation - "bare soil"
   ask patches with [env-ndvi >= 0.156 and env-ndvi < 0.204] [set pcolor  8];;very light grayir - minimum vegetation - "bare soil"
   ask patches with [env-ndvi >= 0.204 and env-ndvi < 0.272] [set pcolor  7];;light gray - minimum vegetation - "bare soil"
   ask patches with [env-ndvi >= 0.272 and env-ndvi < 0.344] [set pcolor  6];;gray - minimum vegetation - "bare soil"
   ask patches with [env-ndvi >= 0.344 and env-ndvi < 0.376] [set pcolor  5];;dark gray - minimum veget - "bare soil"
   ask patches with [env-ndvi >= 0.376 and env-ndvi < 0.492] [set pcolor  green - 2];light green - sparse veget
   ask patches with [env-ndvi >= 0.492 and env-ndvi < 0.576] [set pcolor  green]; green - light veget
   ask patches with [env-ndvi >= 0.576 and env-ndvi < 0.744] [set pcolor  green + 2];; dark green - medium veget
   ask patches with [env-ndvi >= 0.744 and env-ndvi <= 0.92] [set pcolor green + 3];; very dark green - heavy veget
   ask patches with [env-ndvi >= 0.92 and env-ndvi <= 1] [set pcolor green + 4];; very dark green
   ask patches with [env-ndvi = 65535] [set pcolor black];; black - no data
     ]
   if Rainfall? [
    ask patches with [env-pluvio = 0 and  env-limits != 0] [set pcolor 9.9];;white
    ask patches with [env-pluvio >= 1 and env-pluvio <= 9 and env-limits != 0] [set pcolor 48]
    ask patches with [env-pluvio = 10 and env-occup_sol != 1 and env-limits != 0] [set pcolor 47];;yellow
    ask patches with [env-pluvio >= 11 and env-pluvio <= 19 and env-limits != 0] [set pcolor 46]
    ask patches with [env-pluvio >= 20 and env-pluvio <= 30 and env-limits != 0] [set pcolor 45];;yellow
    ask patches with [env-pluvio >= 31 and env-pluvio <= 39 and env-limits != 0] [set pcolor 44]
    ask patches with [env-pluvio >= 40 and env-pluvio <= 80 and env-limits != 0] [set pcolor 43];;yellow
    ask patches with [env-pluvio >= 81 and env-pluvio <= 89 and env-limits != 0] [set pcolor 58]
    ask patches with [env-pluvio >= 90 and env-pluvio <= 140 and env-limits != 0] [set pcolor 55];;green
    ask patches with [env-pluvio >= 141 and env-pluvio <= 149 and env-limits != 0] [set pcolor 53]
    ask patches with [env-pluvio >= 150 and env-pluvio <= 200  and env-limits != 0] [set pcolor 52];;green
    ask patches with [env-pluvio >= 201 and env-pluvio <= 209  and env-limits != 0] [set pcolor 73]
    ask patches with [env-pluvio >= 210 and env-pluvio <= 260 and env-limits != 0] [set pcolor 93];;blue
    ask patches with [env-pluvio >= 261 and env-pluvio <= 269 and env-limits != 0] [set pcolor 92]
    ask patches with [env-pluvio >= 270 and env-pluvio != 65535 and env-limits != 0] [set pcolor 112];;purple missing values
    ask patches with [(env-pluvio = -999 or env-pluvio = 65535) and  env-limits != 0] [user-message ("missing data rainfall")]
   ;; affichage contour (littoral)
   ask patches with [env-limits = 0]  [set pcolor sky + 3 ]
   ]

 if Landcover? [;;  SYNMAP grid display reclassified into 16 classes
   ask patches with [env-occup_sol = 1] [set pcolor blue + 3];;eau
   ask patches with [env-occup_sol = 2] [set pcolor brown - 3]
   ask patches with [env-occup_sol = 3] [set pcolor brown - 2]
   ask patches with [env-occup_sol = 4] [set pcolor brown - 1]
   ask patches with [env-occup_sol = 5] [set pcolor brown]
   ask patches with [env-occup_sol = 6] [set pcolor brown + 1]
   ask patches with [env-occup_sol = 7] [set pcolor green - 3]
   ask patches with [env-occup_sol = 8] [set pcolor green - 2]
   ask patches with [env-occup_sol = 9] [set pcolor green - 1]
   ask patches with [env-occup_sol = 10] [set pcolor green]
   ask patches with [env-occup_sol = 11] [set pcolor lime - 2]
   ask patches with [env-occup_sol = 12] [set pcolor lime - 1]
   ask patches with [env-occup_sol = 13] [set pcolor lime]
   ask patches with [env-occup_sol = 14] [set pcolor lime + 1]
   ask patches with [env-occup_sol = 15] [set pcolor yellow + 4]
   ask patches with [env-occup_sol = 16] [set pcolor grey]
   ask patches with [env-occup_sol = 16] [set pcolor white]
     ]

  if Pedology? [;;  pedology display _ 6 classes
   ask patches with [env-pedo = 1] [set pcolor brown + 1];;clay
   ask patches with [env-pedo = 2] [set pcolor grey];;other
   ask patches with [env-pedo = 3] [set pcolor yellow + 4];;sand
   ask patches with [env-pedo = 4] [set pcolor   yellow + 1];;sandy-clay
   ask patches with [env-pedo = 5] [set pcolor white];; undetermined
   ask patches with [env-pedo = 6] [set pcolor blue + 3] ;;water
   ask patches with [env-pedo != 1 and env-pedo != 2 and env-pedo != 3 and env-pedo !=  4 and env-pedo != 5 and env-pedo != 6] [set pcolor red];;unknown
   ask patches with [env-pedo = 6] [set env-qualite-habitat-pedo 3];;water
   ]

  if river? [
    ask patches with [env-river = 1 or env-river = 2 or env-river = 3] [set pcolor blue + 4]]

if display-habitat? [
  ;; Preferential habitat display (1, 2 ou 3)
    ask patches with [env-qualite-habitat = 1 and env-limits != 0] [set pcolor 65 ]
    ask patches with [env-qualite-habitat = 2 and env-limits != 0] [set pcolor 67 ]
    ask patches with [env-qualite-habitat = 3 and env-limits != 0] [set pcolor white]
    ask patches with [env-qualite-habitat != 1 and env-qualite-habitat != 2 and env-qualite-habitat != 3 and env-limits != 0] [set pcolor red
    user-message("Habitat quality error - have you chosen a preferential habitat criterion? Have you checked Ndvi Max > Ndvi Min?")
  ]
    ;; contour display (coastline)
    ask patches with [env-limits = 0]   [
      set pcolor sky + 3
      set env-qualite-habitat 3]
    ]
end

to utility-pregnant_females;; this utility calculates the number of pregnant females according to month
 if month-num !=  month-fin-reproduction and etat = "reproduction"[
    ifelse month-num = 8 [;; August - beginning of breeding season
     set  %-gestantes-month item 0 list-%gestantes ][
        ifelse month-num = 9 [
           set  %-gestantes-month item 1 list-%gestantes][
         ifelse month-num = 10 [
           set  %-gestantes-month item 2 list-%gestantes][
         ifelse month-num = 11 [
           set  %-gestantes-month item 3 list-%gestantes][
         ifelse month-num = 12 [
           set  %-gestantes-month item 4 list-%gestantes][
         ifelse month-num = 1 [
           set  %-gestantes-month item 5 list-%gestantes][
         ifelse month-num = 2 [
           set  %-gestantes-month item 6 list-%gestantes][
         ifelse month-num = 3 [
           set  %-gestantes-month item 7 list-%gestantes][
         ifelse month-num = 4 [
           set  %-gestantes-month item 8 list-%gestantes][
         ifelse month-num = 5 [
           set  %-gestantes-month item 9 list-%gestantes][
         ifelse month-num = 6 [
           set  %-gestantes-month item 10 list-%gestantes][
        ifelse month-num = 7[
           set  %-gestantes-month item 11 list-%gestantes][
            user-message (word "error")
    ]]]]]]]]]]]]
 set %-gestantes-month %-gestantes-month * nb-females / 100
 if (%-gestantes-month > 0 and  %-gestantes-month < 1) [set %-gestantes-month 1];;no fraction of individuals!
 set list-females-gestantes lput ceiling(%-gestantes-month) list-females-gestantes
 ]
end


to utilitaire-expansion;;this utility calculates each month in meters, the 4 expansions south, north, east and west according to the initial position and the position at the end of the step. ask gerbils[
   ask gerbils[
     set list-coordonnees-x lput xcor list-coordonnees-x;; all positions at x
     set list-coordonnees-y lput ycor list-coordonnees-y;; all positions at y
  ]

  set expansion-est max list-coordonnees-x - position-initiale-x-E
  set expansion-ouest position-initiale-x-W - min list-coordonnees-x;;  min because the scene originates from bottom-left.
  set expansion-sud position-initiale-y-S - min list-coordonnees-y;; min because the scene originates from bottom-left.
  set expansion-nord max list-coordonnees-y - position-initiale-y-N
  set expansion-est expansion-est * 3885;;  expansion in meters (not number of patches)
  set expansion-ouest expansion-ouest * 3885;;  expansion in meters (not number of patches)
  set expansion-sud expansion-sud * 3885;; expansion in meters (not number of patches)
  set expansion-nord expansion-nord * 3885;; expansion in meters (not number of patches)
end



to-report utilitaire-plage-random [low high];; this utility returns a random number between two values.
  report low + random (high - low + 1)
end
@#$#@#$#@
GRAPHICS-WINDOW
453
10
1222
484
-1
-1
3.02
1
10
1
1
1
0
0
0
1
0
251
0
153
0
0
1
ticks
5.0

BUTTON
106
10
169
43
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
20
10
99
43
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
266
10
328
70
YEAR
1981.0
1
0
Number

TEXTBOX
13
258
423
356
blue : water\ndark to light brown : trees, trees/shrubs, trees/grass (broad), trees/grass (broad and needle), trees/crops\ndark to light green : shrubs, shrubs / grass, shrubs / crops, shrubs / sand\ndark to light lime green: grass, grass / crops, grass / sand, crops\nyellow: sand\ngrey: buildings
11
0.0
0

SWITCH
13
224
159
257
Landcover?
Landcover?
1
1
-1000

SWITCH
9
362
112
395
Ndvi?
Ndvi?
1
1
-1000

SWITCH
11
99
114
132
Rainfall?
Rainfall?
1
1
-1000

SWITCH
309
631
448
664
habitat-ndvi?
habitat-ndvi?
1
1
-1000

SWITCH
8
631
168
664
habitat-land-cover?
habitat-land-cover?
1
1
-1000

SWITCH
8
667
227
700
habitat-ndvi-landcover-pedo?
habitat-ndvi-landcover-pedo?
0
1
-1000

TEXTBOX
14
138
339
238
0 white\n1-80 mm : yellow gradient\n81-200 mm : green gradient\n201-206 mm : blue gradient\n>206 mm : purple\nblack : missing data\n
11
0.0
1

TEXTBOX
12
398
431
509
between -1 and -0.076: blue-water\nbetween -0.076 and 0.376: white to dark grey - bare soil\nbetween 0.376 and 0.492: light green - sparse vegetation\nbetween 0.492 and 0.576: green - light vegetation\nbetween 0.576 and 0.744: dark green - medium vegetation\nbetween 0.744 and 0.92: very dark green - heavy vegetation\nbetween 0.92 and 1 : black
11
0.0
1

SLIDER
8
556
100
589
NDVI_min
NDVI_min
0
1
0.19
0.01
1
NIL
HORIZONTAL

SLIDER
100
557
202
590
NDVI_max
NDVI_max
0
1
0.24
0.01
1
NIL
HORIZONTAL

SLIDER
9
914
209
947
monthly-distance
monthly-distance
0
10000
4000.0
100
1
mètres
HORIZONTAL

TEXTBOX
323
760
473
816
Population status :\n- red: reproduction\n- yellow: sexual rest\n- blue: exploration\n
11
0.0
1

SWITCH
8
702
166
735
display-habitat?
display-habitat?
0
1
-1000

SWITCH
287
99
399
132
Pedology?
Pedology?
1
1
-1000

TEXTBOX
290
139
454
251
light yellow : sand\ndark yellow : sandy-clay\nbrown : clay\ngrey : other\nblue : water (sea)\nwhite : undetermined
11
0.0
1

SWITCH
169
631
308
664
habitat-pedo?
habitat-pedo?
1
1
-1000

SWITCH
314
367
404
400
limits?
limits?
1
1
-1000

TEXTBOX
7
86
468
114
======================================================
11
0.0
1

TEXTBOX
11
535
436
553
=====================================================
11
0.0
1

TEXTBOX
350
75
429
93
ENVIRONMENT
11
0.0
1

TEXTBOX
302
523
461
547
PREFERENTIAL HABITAT
11
0.0
1

MONITOR
454
492
620
537
Southward expansion (km)
expansion-sud / 1000
0
1
11

SWITCH
314
408
404
441
river?
river?
1
1
-1000

SLIDER
9
794
124
827
nb-populations
nb-populations
0
100
5.0
1
1
NIL
HORIZONTAL

TEXTBOX
10
738
441
766
=====================================================
11
0.0
1

TEXTBOX
377
724
435
742
GERBILS
11
0.0
1

MONITOR
622
492
787
537
Eastward expansion (km)
expansion-est / 1000
0
1
11

SLIDER
126
795
227
828
nb-init-males
nb-init-males
1
1000
50.0
100
1
NIL
HORIZONTAL

TEXTBOX
239
833
475
916
nb: to obtain x males in August 82, the date on which the first breeding cycle begins, initialise \"nb-init-males\" to 10*x at a mortality rate of 0.75 (for example, 50 gives 5 males in August 82 per pop, and a population size of 10 individuals (5 males and 5 females)).
11
0.0
0

SLIDER
9
831
150
864
%-propagules
%-propagules
0.05
0.80
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
9
758
117
791
survival-rate
survival-rate
0.6
0.9
0.75
0.05
1
NIL
HORIZONTAL

MONITOR
790
492
955
537
Westward expansion (km)
expansion-ouest / 1000
0
1
11

MONITOR
957
493
1122
538
Northward expansion (km)
expansion-nord / 1000
0
1
11

CHOOSER
7
867
151
912
Position?
Position?
"NW" "NE" "E" "NW-NE" "NW-E" "NE-E" "NW-S" "NE-S" "E-S" "S"
0

SLIDER
120
759
228
792
litter-size
litter-size
3
5
3.4
0.1
1
NIL
HORIZONTAL

INPUTBOX
185
10
261
70
MONTH
Initialisation
1
0
String

SWITCH
304
594
450
627
habitat-lc-pedo?
habitat-lc-pedo?
1
1
-1000

SWITCH
167
594
308
627
habitat-lc-ndvi?
habitat-lc-ndvi?
1
1
-1000

SWITCH
8
594
169
627
habitat-pedo-ndvi?
habitat-pedo-ndvi?
1
1
-1000

INPUTBOX
330
10
404
70
month-num
0.0
1
0
Number

TEXTBOX
114
381
264
399
(WINDISP classification)
11
0.0
1

@#$#@#$#@
## WHAT IS IT ?

This model simulates the expansion of the Nigerian gerbil over 3 decades, from 1982 to 2013. Thirty years of fieldwork revealed a range expansion of almost two hundred kilometres, from the 1980s in northern Senegal to the 2010s at Dakar's latitude. The  substrate (density of sand), vegetation (surface covered by herbaceous plants), rainfall and vegetation index (NDVI) were selected as major factors explaining the expansion of this species. 
For each month from January 1982 to December 2013, the model shows in green the location of these preferential habitats and the movement of populations. Population status is displayed in three colours: breeding (red), sexual rest (yellow) and exploration (blue).

## HOW DOES IT WORKS ?
Every year, some propagules leave their natal area and search for a favourable place to settle. Preferential habitats are built using a combination of soil type, land-cover and NDVI. Once settled, a given population will experience either sexual rest or reproduction cycle, according to the period and the resources available. Reproduction will generate new propagule agents looking for new favourable habitats, and this repeatedly renewed process will bring out the species' expansion.

## HOW TO USE IT ?
The interface has three parts:
- The first part concerns environmental data and allows you to choose what you want to display: rainfall, pedology, land cover or NDVI.
- The second shows the preferential habitat locations, which can be defined according to a single (e.g. pedology) or several criteria (e.g. pedology and land use). The most realistic preferential location is the one that takes into account the 3 environmental criteria pedology, land cover and NDVI (item ‘habitat-ndvi-landcover-pedo’).
- The third part specifies the gerbil population parameters. 
A proposed most plausible scenario, given current knowledge on the subject, is composed of: a survival rate of 0.75, a litter size of 3.4 and a rate of propagules of 50% with a combined introduction into Senegal by the north and north-north-east.
At the end of a simulation lasting 3 decades, simulation exhibits an expansion of the gerbil from its point of departure in four directions: south, east, west and north. 
Using the ‘realistic’ parameterisation of preferential locations and the characteristics of the gerbils mentioned above, you can vary the monthly distance and assess its impact on the expansion (and on the southward expansion in particular). 

## THINGS TO TRY
Using the realistic parameters (litter-size=3.4, survival-rate=0.75, departure to the north, %-propagules=0.5, item habitat-ndvi-landcover-pedo? on ‘on’), modify the distance travelled per month and observe the southern expansion obtained at the end of the simulation.

## CREDITS AND REFERENCES
Program Cerise (in french): http://simmasto.org/infos/045/index.htm
Article in prep. "Unravelling the rapid range expansion in Senegal of the Nigerian gerbil (Gerbillus nigeriae) using a multifactorial modelling approach".

Enjoy :)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="nb_step" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>ask gerbilles with [etat = "exploration"]nb_step</metric>
  </experiment>
  <experiment name="ndvi" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>ndvi</metric>
    <metric>ndvi-moyen</metric>
    <enumeratedValueSet variable="Occupation_sol?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NDVI_min">
      <value value="0.17"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ndvi?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pluvio?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ANNEE">
      <value value="1981"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mois-num">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="itération">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-ndvi?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MOIS">
      <value value="&quot;Initialisation&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-land-cover?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-ndvi-land-cover?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NDVI_max">
      <value value="0.54"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="démographie" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>count turtles = 0</exitCondition>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
  </experiment>
  <experiment name="expansion-survie70" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion</metric>
  </experiment>
  <experiment name="test distance parcourue" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion</metric>
    <enumeratedValueSet variable="itération">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="limites?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pédo?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Occupation_sol?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ndvi?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-land-cover?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rotate">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Affichage-niche?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NDVI_min">
      <value value="0.17"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rayon-pour-fusionner">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-ndvi?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trace">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-ndvi-landcover-pédo?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-pédo?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fleuve?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ANNEE">
      <value value="1981"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mois-num">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-parcourue">
      <value value="10"/>
      <value value="100"/>
      <value value="200"/>
      <value value="300"/>
      <value value="500"/>
      <value value="800"/>
      <value value="1000"/>
      <value value="2000"/>
      <value value="3000"/>
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="capacite">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MOIS">
      <value value="&quot;Initialisation&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pluvio?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tx-survie">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NDVI_max">
      <value value="0.54"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fusionner?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="expansion populations" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion</metric>
    <enumeratedValueSet variable="niche-ndvi?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mois-num">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MOIS">
      <value value="&quot;Décembre&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-land-cover?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ANNEE">
      <value value="2013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-ndvi-landcover-pédo?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-parcourue">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rotate">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Occupation_sol?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-pédo?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ndvi?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NDVI_min">
      <value value="0.17"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pédo?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="itération">
      <value value="384"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fusionner?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="limites?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="capacite">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Affichage-niche?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fleuve?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tx-survie">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pluvio?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trace">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rayon-pour-fusionner">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-populations">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NDVI_max">
      <value value="0.54"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test-expansion" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion</metric>
    <enumeratedValueSet variable="ANNEE">
      <value value="2013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Affichage-niche?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ndvi?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NDVI_min">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-populations">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Occupation_sol?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="limites?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tx-survie">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trace">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-pédo?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NDVI_max">
      <value value="0.24"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pédo?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-ndvi-landcover-pédo?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fleuve?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-parcourue">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mois-num">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pluvio?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rotate">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MOIS">
      <value value="&quot;Décembre&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-land-cover?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-ndvi?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="itération">
      <value value="384"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="expansion70_die" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion</metric>
  </experiment>
  <experiment name="experiment-1000-4000" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>expansion</metric>
    <metric>nb-total-adultes</metric>
    <enumeratedValueSet variable="distance-parcourue">
      <value value="1000"/>
      <value value="2000"/>
      <value value="3000"/>
      <value value="4000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tx-survie">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rotate">
      <value value="160"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="comprendre-demo-distance" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>nb-die</metric>
    <enumeratedValueSet variable="distance-parcourue">
      <value value="1000"/>
      <value value="10000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-1000-10000-expansion-sud" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <enumeratedValueSet variable="distance-parcourue">
      <value value="1000"/>
      <value value="2000"/>
      <value value="3000"/>
      <value value="4000"/>
      <value value="5000"/>
      <value value="6000"/>
      <value value="7000"/>
      <value value="8000"/>
      <value value="9000"/>
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tx-survie">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rotate">
      <value value="160"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="init-pop" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <enumeratedValueSet variable="eff-init-males">
      <value value="10000"/>
      <value value="100000"/>
      <value value="200000"/>
      <value value="300000"/>
      <value value="400000"/>
      <value value="500000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="init-nb-pop" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <enumeratedValueSet variable="nb-populations">
      <value value="10"/>
      <value value="30"/>
      <value value="50"/>
      <value value="70"/>
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="init-nb-pop50-distance" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <enumeratedValueSet variable="nb-populations">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-parcourue">
      <value value="3000"/>
      <value value="4000"/>
      <value value="5000"/>
      <value value="6000"/>
      <value value="7000"/>
      <value value="8000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="A (taille pop de 1000 à 5000 et groupe)" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <enumeratedValueSet variable="eff-init-males">
      <value value="10000"/>
      <value value="20000"/>
      <value value="50000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-populations">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-parcourue">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Groupe?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="B (taille pop de 1000 à 5000 et non groupe)" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <enumeratedValueSet variable="eff-init-males">
      <value value="10000"/>
      <value value="20000"/>
      <value value="50000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-populations">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-parcourue">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Groupe?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="C (nb pop de 10 à 100 et groupe)" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <enumeratedValueSet variable="eff-init-males">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-populations">
      <value value="10"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-parcourue">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Groupe?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="D (nb pop de 10 à 100 et non groupe)" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <enumeratedValueSet variable="eff-init-males">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-populations">
      <value value="10"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-parcourue">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Groupe?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="E - distance de 1000 à 10000 - nb fusions et nb micro pop" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <metric>nb-die</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <enumeratedValueSet variable="Groupe?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-populations">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eff-init-males">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-parcourue">
      <value value="1000"/>
      <value value="2000"/>
      <value value="3000"/>
      <value value="4000"/>
      <value value="5000"/>
      <value value="6000"/>
      <value value="7000"/>
      <value value="8000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="E - distance de 9000 et 10000 - nb fusions et nb micro pop" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <metric>nb-die</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <enumeratedValueSet variable="Groupe?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-populations">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eff-init-males">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-parcourue">
      <value value="9000"/>
      <value value="10000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="F - sans et avec forcage &lt; 1 - nb fusions et nb micro pop" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <metric>nb-die</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <enumeratedValueSet variable="Groupe?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-populations">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eff-init-males">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-parcourue">
      <value value="3000"/>
      <value value="4000"/>
      <value value="5000"/>
      <value value="6000"/>
      <value value="7000"/>
      <value value="8000"/>
      <value value="9000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Forçage?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="G  - évolution démographie step par step 2 distances" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <metric>nb-die</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <enumeratedValueSet variable="Groupe?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-populations">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eff-init-males">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-parcourue">
      <value value="3000"/>
      <value value="8000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Forçage?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="H - rotate de 10 à 180 - Ndvi max 0.21" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <metric>nb-die</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <enumeratedValueSet variable="NDVI_max">
      <value value="0.21"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rotate">
      <value value="10"/>
      <value value="30"/>
      <value value="50"/>
      <value value="80"/>
      <value value="100"/>
      <value value="160"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="H - rotate de 10 à 180 - Ndvi max 0.20" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <metric>nb-die</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <enumeratedValueSet variable="NDVI_max">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rotate">
      <value value="10"/>
      <value value="30"/>
      <value value="50"/>
      <value value="80"/>
      <value value="100"/>
      <value value="160"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="I -nb niche qualité 1" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>nb-niche-1-mensuel</metric>
    <enumeratedValueSet variable="NDVI_max">
      <value value="0.2"/>
      <value value="0.24"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="scenario 1-a : demographie" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <enumeratedValueSet variable="nb-populations">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="scenario 1-b : demographie" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <enumeratedValueSet variable="eff-init-males">
      <value value="500"/>
      <value value="1000"/>
      <value value="3000"/>
      <value value="5000"/>
      <value value="10000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="scenario 0-mois par mois" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
  </experiment>
  <experiment name="scenario 3 - Ndvi non sélectif" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
  </experiment>
  <experiment name="scenario 0 - nouvelle version - distance parcourue de 1000 à 10000" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <enumeratedValueSet variable="distance-parcourue">
      <value value="1000"/>
      <value value="2000"/>
      <value value="3000"/>
      <value value="4000"/>
      <value value="5000"/>
      <value value="6000"/>
      <value value="7000"/>
      <value value="8000"/>
      <value value="9000"/>
      <value value="10000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="scenario 0- jeunes et PP" repetitions="2" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>nb-pop-jeunes</metric>
    <metric>nb-naitre&lt;1</metric>
  </experiment>
  <experiment name="tests de jeudi - comprendre évolution pop et nb adultes - sans groupe" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <enumeratedValueSet variable="eff-init-males">
      <value value="500"/>
      <value value="10000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="tests de jeudi 16/02 - augmentation pop avec distance parcourue" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <metric>compteur-reprod0</metric>
    <enumeratedValueSet variable="distance-parcourue">
      <value value="1000"/>
      <value value="10000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="der - tests de jeudi 2 mars - %gest variable pluvio et %propagules 0.5 2 ceiling jeunes" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <metric>count turtles with [etat = "exploration"]</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <metric>compteur-reprod0</metric>
  </experiment>
  <experiment name="der - tests de lundi 6 mars - %gest variable pluvio et %propagules 0.5 2 ceiling ; nb jeunes ; taille pop min, max, moy - mourir" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <metric>count turtles with [etat = "exploration"]</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <metric>compteur-reprod0</metric>
    <metric>nb-total-jeunes</metric>
    <metric>taille-max</metric>
    <metric>taille-min</metric>
    <metric>taille-moy</metric>
  </experiment>
  <experiment name="der - tests de mardi 7  mars - %gest variable pluvio et %propagules 0.5 jeuens et femelles à 1; nb jeunes ; taille pop min, max, moy - mourir" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <metric>count turtles with [etat = "exploration"]</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <metric>compteur-reprod0</metric>
    <metric>nb-total-jeunes</metric>
    <metric>taille-max</metric>
    <metric>taille-min</metric>
    <metric>taille-moy</metric>
  </experiment>
  <experiment name="der - tests de mercredi 8  mars - %gest variable pluvio et %propagules 0.5 jeuens et femelles à 1; nb jeunes ; taille pop min, max, moy - mourir" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <metric>count turtles with [etat = "exploration"]</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <metric>compteur-reprod0</metric>
    <metric>nb-total-jeunes</metric>
    <metric>taille-max</metric>
    <metric>taille-min</metric>
    <metric>taille-moy</metric>
  </experiment>
  <experiment name="der - tests de jeudi 9 mars - %gest variable pluvio et %propagules 0.05 jeunes et femelles à 1; nb jeunes ; taille pop min, max, moy - mourir" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <metric>count turtles with [etat = "exploration"]</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <metric>compteur-reprod0</metric>
    <metric>nb-total-jeunes</metric>
    <metric>taille-max</metric>
    <metric>taille-min</metric>
    <metric>taille-moy</metric>
  </experiment>
  <experiment name="der - tests de jeudi 9 mars -  %propagules 0.05 à 0.8" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <metric>count turtles with [etat = "exploration"]</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <metric>compteur-reprod0</metric>
    <metric>nb-total-jeunes</metric>
    <metric>taille-max</metric>
    <metric>taille-min</metric>
    <metric>taille-moy</metric>
    <metric>nb-total-explorateurs / (count gerbilles with [etat = "exploration"])</metric>
    <enumeratedValueSet variable="Borne-min-%-propagules">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="1a - demo - pop" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <metric>count turtles with [etat = "exploration"]</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <metric>nb-total-jeunes</metric>
    <metric>taille-max</metric>
    <metric>taille-min</metric>
    <metric>taille-moy</metric>
    <metric>nb-total-explorateurs / (count gerbilles with [etat = "exploration"])</metric>
    <enumeratedValueSet variable="nb-populations">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="1b - demo - nb individus" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <metric>count turtles with [etat = "exploration"]</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <metric>nb-total-jeunes</metric>
    <metric>taille-max</metric>
    <metric>taille-min</metric>
    <metric>taille-moy</metric>
    <metric>nb-total-explorateurs / (count gerbilles with [etat = "exploration"])</metric>
    <enumeratedValueSet variable="eff-init-males">
      <value value="50"/>
      <value value="100"/>
      <value value="200"/>
      <value value="300"/>
      <value value="400"/>
      <value value="500"/>
      <value value="600"/>
      <value value="700"/>
      <value value="800"/>
      <value value="900"/>
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="2 - position initiale (code à modifier : position-x à 80)" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <metric>count turtles with [etat = "exploration"]</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <metric>nb-total-jeunes</metric>
    <metric>taille-max</metric>
    <metric>taille-min</metric>
    <metric>taille-moy</metric>
    <metric>nb-total-explorateurs / (count gerbilles with [etat = "exploration"])</metric>
  </experiment>
  <experiment name="3a  - groupe" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <metric>count turtles with [etat = "exploration"]</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <metric>nb-total-jeunes</metric>
    <metric>taille-max</metric>
    <metric>taille-min</metric>
    <metric>taille-moy</metric>
    <metric>nb-total-explorateurs / (count gerbilles with [etat = "exploration"])</metric>
    <enumeratedValueSet variable="Groupe?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="3b  - forcage" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <metric>count turtles with [etat = "exploration"]</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <metric>nb-total-jeunes</metric>
    <metric>taille-max</metric>
    <metric>taille-min</metric>
    <metric>taille-moy</metric>
    <metric>nb-total-explorateurs / (count gerbilles with [etat = "exploration"])</metric>
    <enumeratedValueSet variable="Forçage">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="4  - propagules" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <metric>count turtles with [etat = "exploration"]</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <metric>nb-total-jeunes</metric>
    <metric>taille-max</metric>
    <metric>taille-min</metric>
    <metric>taille-moy</metric>
    <metric>nb-total-explorateurs / (count gerbilles with [etat = "exploration"])</metric>
    <enumeratedValueSet variable="borne-min-%-propagules">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="1c - demo - tx survie" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <metric>count turtles with [etat = "exploration"]</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <metric>nb-total-jeunes</metric>
    <metric>taille-max</metric>
    <metric>taille-min</metric>
    <metric>taille-moy</metric>
    <metric>nb-total-explorateurs / (count gerbilles with [etat = "exploration"])</metric>
    <enumeratedValueSet variable="tx-survie">
      <value value="0.6"/>
      <value value="0.65"/>
      <value value="0.7"/>
      <value value="0.75"/>
      <value value="0.8"/>
      <value value="0.85"/>
      <value value="0.9"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="5 - distance" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <metric>count turtles with [etat = "exploration"]</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <metric>nb-total-jeunes</metric>
    <metric>taille-max</metric>
    <metric>taille-min</metric>
    <metric>taille-moy</metric>
    <metric>nb-total-explorateurs / (count gerbilles with [etat = "exploration"])</metric>
    <enumeratedValueSet variable="distance-parcourue">
      <value value="1000"/>
      <value value="2000"/>
      <value value="3000"/>
      <value value="4000"/>
      <value value="5000"/>
      <value value="6000"/>
      <value value="7000"/>
      <value value="8000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="6 - points Leili - sénégal calib" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count gerbilles</metric>
    <metric>nb-total-adultes</metric>
    <metric>count gerbilles with [etat = "exploration"]</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <metric>nb-total-jeunes</metric>
    <metric>taille-max</metric>
    <metric>taille-min</metric>
    <metric>taille-moy</metric>
    <metric>nb-total-explorateurs / (count gerbilles with [etat = "exploration"])</metric>
    <enumeratedValueSet variable="Proba_min">
      <value value="&quot;600&quot;"/>
      <value value="&quot;650&quot;"/>
      <value value="&quot;700&quot;"/>
      <value value="&quot;750&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="7 - simullations NW /  distance" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count gerbilles</metric>
    <metric>nb-total-adultes</metric>
    <metric>count gerbilles with [etat = "exploration"]</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <metric>nb-total-jeunes</metric>
    <metric>taille-max</metric>
    <metric>taille-min</metric>
    <metric>taille-moy</metric>
    <metric>count gerbilles with [etat = "exploration"]</metric>
    <metric>nb-total-explorateurs / (count gerbilles with [etat = "exploration"])</metric>
    <metric>nb-patches-occupes</metric>
    <enumeratedValueSet variable="distance-parcourue">
      <value value="1000"/>
      <value value="2000"/>
      <value value="3000"/>
      <value value="3500"/>
      <value value="4000"/>
      <value value="4500"/>
      <value value="5000"/>
      <value value="5500"/>
      <value value="6000"/>
      <value value="7000"/>
      <value value="8000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="9 - simullations positions /  distance" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>expansion-nord</metric>
    <metric>expansion-ouest</metric>
    <metric>count gerbilles</metric>
    <metric>nb-total-adultes</metric>
    <metric>count gerbilles with [etat = "exploration"]</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <metric>nb-total-jeunes</metric>
    <metric>taille-max</metric>
    <metric>taille-min</metric>
    <metric>taille-moy</metric>
    <metric>count gerbilles with [etat = "exploration"]</metric>
    <metric>nb-total-explorateurs / (count gerbilles with [etat = "exploration"])</metric>
    <enumeratedValueSet variable="distance-parcourue">
      <value value="1000"/>
      <value value="2000"/>
      <value value="3000"/>
      <value value="3500"/>
      <value value="4000"/>
      <value value="4500"/>
      <value value="5000"/>
      <value value="5500"/>
      <value value="6000"/>
      <value value="7000"/>
      <value value="8000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Position?">
      <value value="&quot;N&quot;"/>
      <value value="&quot;N-E&quot;"/>
      <value value="&quot;E1&quot;"/>
      <value value="&quot;E2&quot;"/>
      <value value="&quot;E3&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="nb patches occupés et noin occupés" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>nb-patches-occupes</metric>
    <metric>nb-patches-non-occupes</metric>
  </experiment>
  <experiment name="densite" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>expansion-nord</metric>
    <metric>expansion-ouest</metric>
    <metric>count gerbilles</metric>
    <metric>nb-total-adultes</metric>
    <metric>count gerbilles with [etat = "exploration"]</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <metric>nb-total-jeunes</metric>
    <metric>taille-max</metric>
    <metric>taille-min</metric>
    <metric>taille-moy</metric>
    <metric>count gerbilles with [etat = "exploration"]</metric>
  </experiment>
  <experiment name="nb-jeunes" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>nb-pop-nouvelles</metric>
  </experiment>
  <experiment name="Analyse janvier 2018" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>expansion-nord</metric>
    <metric>expansion-ouest</metric>
    <metric>taille-moy</metric>
    <metric>nb-pop</metric>
    <metric>nb-total-adultes</metric>
    <metric>nb-pop-explorateurs</metric>
    <metric>nb-total-explorateurs</metric>
    <metric>nb-fusions</metric>
    <metric>nb-pop-absorbe</metric>
    <metric>nb-patches-occupes</metric>
    <metric>nb-patches-non-occupes</metric>
    <enumeratedValueSet variable="tx-survie">
      <value value="0.7"/>
      <value value="0.72"/>
      <value value="0.74"/>
      <value value="0.76"/>
      <value value="0.78"/>
      <value value="0.8"/>
      <value value="0.82"/>
      <value value="0.84"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tx-reprod">
      <value value="3"/>
      <value value="3.2"/>
      <value value="3.4"/>
      <value value="3.6"/>
      <value value="3.8"/>
      <value value="4"/>
      <value value="4.2"/>
      <value value="4.4"/>
      <value value="4.6"/>
      <value value="4.8"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-parcourue">
      <value value="2800"/>
      <value value="3000"/>
      <value value="3200"/>
      <value value="3400"/>
      <value value="3600"/>
      <value value="3800"/>
      <value value="4000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rotate">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-populations">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eff-init-males">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NDVI_max">
      <value value="0.24"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NDVI_min">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="borne-min-%-propagules">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-ndvi-landcover-pédo-bis?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Critère_NDVI?">
      <value value="&quot;BPM&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Position?">
      <value value="&quot;N-W/N-E&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Analyse novembre 2018" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>expansion-nord</metric>
    <metric>expansion-ouest</metric>
    <metric>taille-moy</metric>
    <metric>nb-pop</metric>
    <metric>nb-total-adultes</metric>
    <metric>nb-pop-explorateurs</metric>
    <metric>nb-total-explorateurs</metric>
    <metric>nb-fusions</metric>
    <metric>nb-pop-absorbe</metric>
    <metric>nb-patches-occupes</metric>
    <metric>nb-patches-non-occupes</metric>
    <enumeratedValueSet variable="borne-min-%-propagules">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tx-survie">
      <value value="0.6"/>
      <value value="0.65"/>
      <value value="0.7"/>
      <value value="0.75"/>
      <value value="0.8"/>
      <value value="0.85"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-parcourue">
      <value value="200"/>
      <value value="1000"/>
      <value value="2000"/>
      <value value="3000"/>
      <value value="4000"/>
      <value value="5000"/>
      <value value="6000"/>
      <value value="7000"/>
      <value value="8000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rotate">
      <value value="5"/>
      <value value="20"/>
      <value value="35"/>
      <value value="50"/>
      <value value="65"/>
      <value value="80"/>
      <value value="95"/>
      <value value="110"/>
      <value value="125"/>
      <value value="140"/>
      <value value="155"/>
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-populations">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eff-init-males">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tx-reprod">
      <value value="3.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NDVI_max">
      <value value="0.24"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NDVI_min">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-ndvi-landcover-pédo-bis?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Critère_NDVI?">
      <value value="&quot;BPM&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Position?">
      <value value="&quot;N&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Analyse février 2018 - nb-absorbe" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>expansion-nord</metric>
    <metric>expansion-ouest</metric>
    <metric>taille-moy</metric>
    <metric>nb-pop</metric>
    <metric>nb-total-adultes</metric>
    <metric>nb-pop-explorateurs</metric>
    <metric>nb-total-explorateurs</metric>
    <metric>nb-fusions</metric>
    <metric>nb-pop-absorbe</metric>
    <metric>nb-patches-occupes</metric>
    <metric>nb-patches-non-occupes</metric>
    <enumeratedValueSet variable="tx-survie">
      <value value="0.74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tx-reprod">
      <value value="3.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-parcourue">
      <value value="3800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rotate">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-populations">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eff-init-males">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NDVI_max">
      <value value="0.24"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NDVI_min">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="borne-min-%-propagules">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-ndvi-landcover-pédo-bis?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Critère_NDVI?">
      <value value="&quot;BPM&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Position?">
      <value value="&quot;N-W/N-E&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Analyse avril 2018 - nb pop 10" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>expansion-nord</metric>
    <metric>expansion-ouest</metric>
    <metric>taille-moy</metric>
    <metric>nb-pop</metric>
    <metric>nb-total-adultes</metric>
    <metric>nb-pop-explorateurs</metric>
    <metric>nb-total-explorateurs</metric>
    <metric>nb-fusions</metric>
    <metric>nb-pop-absorbe</metric>
    <metric>nb-patches-occupes</metric>
    <metric>nb-patches-non-occupes</metric>
    <enumeratedValueSet variable="eff-init-males">
      <value value="50"/>
      <value value="500"/>
      <value value="5000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="demo-oct_2018" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>count turtles</metric>
    <metric>nb-total-adultes</metric>
    <metric>count turtles with [etat = "exploration"]</metric>
    <metric>count turtles with [etat = "estivation" or etat = "reproduction"]</metric>
    <metric>nb-fusions</metric>
    <metric>nb-naitre&lt;1</metric>
    <metric>nb-total-jeunes</metric>
    <metric>taille-max</metric>
    <metric>taille-min</metric>
    <metric>taille-moy</metric>
    <metric>nb-total-explorateurs / (count gerbilles with [etat = "exploration"])</metric>
  </experiment>
  <experiment name="taille pop" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>liste-taille-pop</metric>
    <enumeratedValueSet variable="niche-ndvi-landcover-pédo?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-populations">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-ndvi?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Proba_max">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fleuve?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rotate">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-parcourue">
      <value value="3800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pluvio?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-ndvi-landcover-pédo-bis?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tx-reprod">
      <value value="3.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="LP_Points_Leyli?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Occupation_sol?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-land-cover?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ANNEE">
      <value value="1984"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MOIS">
      <value value="&quot;Février&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Proba_min">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mois-num">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="itération">
      <value value="26"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Groupe?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trace">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pédo?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Critère_NDVI?">
      <value value="&quot;BPM&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Position?">
      <value value="&quot;N-E&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eff-init-males">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tx-survie">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NDVI_min">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="limites?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ndvi?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Forçage?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="borne-min-%-propagules">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NDVI_max">
      <value value="0.24"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Occupation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Affichage-niche?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-pédo?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="espace_dispo" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>nb-niche-1-mensuel</metric>
    <metric>nb-patches-occupes</metric>
    <enumeratedValueSet variable="niche-ndvi-landcover-pédo?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-populations">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-ndvi?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Proba_max">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fleuve?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rotate">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-parcourue">
      <value value="3800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pluvio?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-ndvi-landcover-pédo-bis?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Occupation_sol?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="LP_Points_Leyli?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tx-reprod">
      <value value="3.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-land-cover?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ANNEE">
      <value value="2013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MOIS">
      <value value="&quot;Décembre&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Proba_min">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mois-num">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="itération">
      <value value="384"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trace">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Groupe?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Pédo?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tx-survie">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Position?">
      <value value="&quot;N-W/N-E&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eff-init-males">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Critère_NDVI?">
      <value value="&quot;BPM&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NDVI_min">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="limites?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ndvi?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Forçage?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="borne-min-%-propagules">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NDVI_max">
      <value value="0.24"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Occupation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Affichage-niche?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-pédo?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sept 2020" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>expansion-sud</metric>
    <metric>expansion-est</metric>
    <metric>expansion-nord</metric>
    <metric>expansion-ouest</metric>
    <metric>taille-moy</metric>
    <metric>nb-pop</metric>
    <metric>nb-total-adultes</metric>
    <metric>nb-pop-explorateurs</metric>
    <metric>nb-total-explorateurs</metric>
    <metric>nb-fusions</metric>
    <metric>nb-pop-absorbe</metric>
    <metric>nb-patches-occupes</metric>
    <metric>nb-patches-non-occupes</metric>
    <enumeratedValueSet variable="tx-survie">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.55"/>
      <value value="0.6"/>
      <value value="0.65"/>
      <value value="0.7"/>
      <value value="0.75"/>
      <value value="0.8"/>
      <value value="0.85"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="borne-min-%-propagules">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tx-reprod">
      <value value="3.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-parcourue">
      <value value="4000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rotate">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-populations">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eff-init-males">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NDVI_max">
      <value value="0.24"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NDVI_min">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-ndvi-landcover-pédo-bis?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Critère_NDVI?">
      <value value="&quot;BPM&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Position?">
      <value value="&quot;N&quot;"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
