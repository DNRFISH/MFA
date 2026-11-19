#' Interactive survey and effort map
#'
#' @description Starts with one marker per MDNRID. Click a waterbody to choose
#' a survey and display its individual effort locations. Species choices are
#' restricted to catch recorded for the current map view. Catch is total fish,
#' classified as zero plus four quartiles of positive catches. CPUE uses
#' CPUE_byGear() for both survey and effort CPUE (including its effort validation).
#' @param data Nonempty Survey, Efforts, or Catch result from FISH_query().
#' @param con Optional FISHub connection. Needed to retrieve effort locations
#'   and catch when these are not present in data.
#' @param Species Optional initially selected species; used where available.
#' @param Symbology One of "Both", "Color", or "Size".
#' @param Scale Currently "Relative" only.
#' @return A leaflet htmlwidget.
#' @export
MFA_map <- function(data, con = NULL, Species = NULL,
                    Symbology = "Both", Scale = "Relative") {
  Symbology <- match.arg(Symbology, c("Both", "Color", "Size"))
  Scale <- match.arg(Scale, "Relative")
  if (!is.data.frame(data) || !nrow(data)) stop("data must be a nonempty FISH_query data frame.")
  if (!all(c("SurveyId", "MDNRID", "WaterBodyName", "WaterLat", "WaterLong") %in% names(data)))
    stop("Missing required survey or waterbody fields, including WaterLat and WaterLong.")
  effort_input <- "SurveyEffortKey" %in% names(data)
  catch_input <- effort_input && all(c("Species", "TotalNumberCaught") %in% names(data))
  survey_rows <- dplyr::distinct(dplyr::as_tibble(data),
                                  .data$SurveyId, .data$MDNRID, .data$WaterBodyName,
                                  .data$WaterLat, .data$WaterLong)
  # The supplied data defines the set of surveys. Never expand it.
  ids <- unique(stats::na.omit(survey_rows$SurveyId))
  if (!length(ids)) stop("No survey IDs were provided.")
  batches <- split(ids, ceiling(seq_along(ids) / 50))
  query_batches <- function(type) dplyr::bind_rows(lapply(batches, function(batch) {
    FISH_query(con, QueryType = type, SurveyId = batch)
  }))
  if (!is.null(con)) {
    effort_data <- query_batches("Efforts")
  } else if (effort_input) {
    effort_data <- data
  } else {
    effort_data <- NULL
  }
  if (catch_input && is.null(con)) {
    catch_data <- data
  } else if (!is.null(con)) {
    catch_data <- query_batches("Catch")
  } else {
    catch_data <- NULL
  }
  if (!is.null(effort_data) && !all(c("SurveyId", "SurveyEffortKey", "StartingLatitude", "StartingLongitude") %in% names(effort_data)))
    stop("Effort data must include SurveyEffortKey and starting coordinates.")
  if (!is.null(catch_data) && nrow(catch_data) &&
      !all(c("SurveyId", "ModuleId", "Species", "TotalNumberCaught") %in% names(catch_data)))
    stop("Catch data must include ModuleId, Species, and TotalNumberCaught.")

  # One waterbody marker, even when there are many surveys/efforts there.
  first_valid <- function(x) {
    v <- suppressWarnings(as.numeric(x))
    v <- v[is.finite(v)]
    if (length(v)) v[1] else NA_real_
  }
  waters <- survey_rows |>
    dplyr::group_by(.data$MDNRID) |>
    dplyr::summarise(name = dplyr::first(.data$WaterBodyName),
                     lat = first_valid(.data$WaterLat),
                     lng = first_valid(.data$WaterLong),
                     n_surveys = dplyr::n_distinct(.data$SurveyId), .groups = "drop")
  waters$key <- paste0("w_", waters$MDNRID)

  efforts <- NULL
  if (!is.null(effort_data) && nrow(effort_data)) {
    efforts <- dplyr::as_tibble(effort_data) |>
      dplyr::filter(!is.na(.data$SurveyEffortKey)) |>
      dplyr::group_by(.data$SurveyId, .data$SurveyEffortKey) |>
      dplyr::summarise(MDNRID = dplyr::first(.data$MDNRID),
                       name = dplyr::first(.data$WaterBodyName),
                       lat = first_valid(.data$StartingLatitude),
                       lng = first_valid(.data$StartingLongitude),
                       effort_quantity = if ("EffortTotalQuantity" %in% names(effort_data)) first_valid(.data$EffortTotalQuantity) else NA_real_,
                       effort_units = if ("EffortTotalMeasurement" %in% names(effort_data)) paste(unique(stats::na.omit(as.character(.data$EffortTotalMeasurement))), collapse = ", ") else "",
                       alternate_quantity = if ("EffortAlternateQuantity" %in% names(effort_data)) first_valid(.data$EffortAlternateQuantity) else NA_real_,
                       alternate_units = if ("EffortAlternateMeasurement" %in% names(effort_data)) paste(unique(stats::na.omit(as.character(.data$EffortAlternateMeasurement))), collapse = ", ") else "",
                       gear = if ("GearType" %in% names(effort_data))
                         paste(unique(stats::na.omit(.data$GearType)), collapse = ", ")
                       else "", .groups = "drop")
    efforts$key <- paste0("e_", efforts$SurveyId, "_", efforts$SurveyEffortKey)
  }
  valid_coords <- function(d) {
    if (is.null(d)) return(d)
    ok <- is.finite(d$lat) & is.finite(d$lng) &
      abs(d$lat) <= 90 & abs(d$lng) <= 180 & !(d$lat == 0 & d$lng == 0)
    if (any(!ok)) warning(sum(!ok), " map locations omitted due to missing/invalid coordinates.")
    d[ok, , drop = FALSE]
  }
  waters <- valid_coords(waters)
  efforts <- valid_coords(efforts)
  if (!nrow(waters)) stop("No waterbody coordinates available to map.")

  # Catch is first summarized by module/species; then assigned to waterbody
  # and effort. A single module must not be multiplied by duplicate join rows.
  totals <- data.frame(key = character(), Species = character(), Total = numeric())
  if (!is.null(catch_data) && nrow(catch_data)) {
    cd <- dplyr::as_tibble(catch_data)
    base_catch <- cd |>
      dplyr::filter(!is.na(.data$Species), !is.na(.data$ModuleId)) |>
      dplyr::group_by(.data$SurveyId, .data$ModuleId, .data$Species) |>
      dplyr::summarise(Total = if (all(is.na(.data$TotalNumberCaught))) NA_real_
                       else sum(.data$TotalNumberCaught, na.rm = TRUE), .groups = "drop")
    water_lookup <- dplyr::distinct(cd, .data$SurveyId, .data$ModuleId, .data$MDNRID)
    wt <- dplyr::inner_join(base_catch, water_lookup, by = c("SurveyId", "ModuleId"))
    wt$key <- paste0("w_", wt$MDNRID)
    totals <- dplyr::bind_rows(totals, dplyr::select(wt, .data$key, .data$Species, .data$Total))
    if (!is.null(efforts) && nrow(efforts) && "SurveyEffortKey" %in% names(cd)) {
      effort_lookup <- dplyr::distinct(cd, .data$SurveyId, .data$ModuleId, .data$SurveyEffortKey)
      et <- dplyr::inner_join(base_catch, effort_lookup, by = c("SurveyId", "ModuleId"))
      et$key <- paste0("e_", et$SurveyId, "_", et$SurveyEffortKey)
      totals <- dplyr::bind_rows(totals, dplyr::select(et, .data$key, .data$Species, .data$Total))
    }
    totals <- totals |>
      dplyr::group_by(.data$key, .data$Species) |>
      dplyr::summarise(Total = if (all(is.na(.data$Total))) NA_real_
                       else sum(.data$Total, na.rm = TRUE), .groups = "drop")
  }
  if (!is.null(Species) && (!is.character(Species) || length(Species) != 1L))
    stop("Species must be a single species name.")
  all_species <- sort(unique(as.character(totals$Species)))
  if (!is.null(Species) && !Species %in% all_species)
    warning("Initially selected Species is not present in the supplied catch data.")

  # Data passed to the browser are compact; all navigation and symbology
  # updates occur locally without a Shiny server or additional queries.
  surveys_by_water <- split(as.character(survey_rows$SurveyId), as.character(survey_rows$MDNRID))
  surveys_by_water <- lapply(surveys_by_water, unique)
  water_ids <- as.character(waters$MDNRID)
  water_info <- lapply(seq_len(nrow(waters)), function(i) list(
    key = waters$key[i], water = water_ids[i], name = as.character(waters$name[i]),
    surveys = as.list(surveys_by_water[[water_ids[i]]]), count = waters$n_surveys[i]))
  effort_info <- if (is.null(efforts)) list() else lapply(seq_len(nrow(efforts)), function(i) list(
    key = efforts$key[i], water = as.character(efforts$MDNRID[i]),
    survey = as.character(efforts$SurveyId[i]), effort = as.character(efforts$SurveyEffortKey[i]),
    gear = as.character(efforts$gear[i]), name = as.character(efforts$name[i]),
    quantity = efforts$effort_quantity[i], units = as.character(efforts$effort_units[i]),
    altQuantity = efforts$alternate_quantity[i], altUnits = as.character(efforts$alternate_units[i])))
  # Use CPUE_byGear() for both survey and individual-effort CPUE.
  # Keep numerator and denominator so waterbody CPUE can be pooled correctly
  # across surveys (rather than averaging survey CPUE values).
  cpue_values <- list()
  if (!is.null(catch_data) && nrow(catch_data)) {
    required_cpue <- c("SurveyId", "SurveyEffortKey", "GearType", "Species",
                       "TotalNumberCaught", "EffortTotalQuantity",
                       "EffortTotalMeasurement", "EffortAlternateQuantity",
                       "EffortAlternateMeasurement", "EffortNumberofGearUsed")
    if (!all(required_cpue %in% names(catch_data)))
      stop("Catch data is missing fields required by CPUE_byGear().")

    survey_cpue <- CPUE_byGear(catch_data, SummaryLevel = "Survey")
    effort_cpue <- CPUE_byGear(catch_data, SummaryLevel = "Effort")
    survey_water <- dplyr::distinct(dplyr::as_tibble(catch_data),
                                    .data$SurveyId, .data$MDNRID)
    survey_cpue <- dplyr::left_join(survey_cpue, survey_water,
                                   by = "SurveyId", relationship = "many-to-many")
    make_cpue_values <- function(d, level) {
      lapply(seq_len(nrow(d)), function(i) {
        list(level = level,
             key = if (level == "Effort")
               paste0("e_", d$SurveyId[i], "_", d$SurveyEffortKey[i])
             else paste0("w_", d$MDNRID[i]),
             survey = as.character(d$SurveyId[i]),
             gear = as.character(d$GearType[i]),
             species = as.character(d$Species[i]),
             units = as.character(d$Unit[i]),
             total = as.numeric(d$TotalNumberCaught[i]),
             effort = as.numeric(d$TotalEffort[i]),
             cpue = as.numeric(d$CPUE[i]))
      })
    }
    cpue_values <- c(make_cpue_values(survey_cpue, "Survey"),
                     make_cpue_values(effort_cpue, "Effort"))
  }
  catch_values <- list()
  if (!is.null(catch_data) && nrow(catch_data)) {
    cd <- dplyr::as_tibble(catch_data)
    cv <- cd |>
      dplyr::filter(!is.na(.data$Species), !is.na(.data$SurveyEffortKey), !is.na(.data$ModuleId)) |>
      dplyr::group_by(.data$SurveyId, .data$ModuleId, .data$SurveyEffortKey, .data$Species) |>
      dplyr::summarise(total = if (all(is.na(.data$TotalNumberCaught))) NA_real_
                       else sum(.data$TotalNumberCaught, na.rm = TRUE), .groups = "drop") |>
      dplyr::group_by(.data$SurveyId, .data$SurveyEffortKey, .data$Species) |>
      dplyr::summarise(total = if (all(is.na(.data$total))) NA_real_
                       else sum(.data$total, na.rm = TRUE), .groups = "drop")
    catch_values <- lapply(seq_len(nrow(cv)), function(i) list(
      key = paste0("e_", cv$SurveyId[i], "_", cv$SurveyEffortKey[i]),
      species = cv$Species[i], total = if (is.finite(cv$total[i])) cv$total[i] else NULL))
  }
  catch_info <- split(totals[, c("Species", "Total")], totals$key)
  catch_info <- lapply(catch_info, function(d) {
    stats::setNames(lapply(d$Total, function(x) if (is.na(x)) NULL else as.numeric(x)), d$Species)
  })

  m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
    leaflet::addProviderTiles("Esri.WorldTopoMap") |>
    leaflet::addCircleMarkers(data = waters, lng = ~lng, lat = ~lat,
      layerId = ~key, radius = 5, color = "#142D3E", weight = 1,
      fillColor = "#277C78", fillOpacity = 0.85, group = "Waterbodies")
  if (!is.null(efforts) && nrow(efforts)) {
    m <- leaflet::addCircleMarkers(m, data = efforts, lng = ~lng, lat = ~lat,
      layerId = ~key, radius = 5, color = "#142D3E", weight = 1,
      fillColor = "#277C78", fillOpacity = 0.85, group = "Efforts")
  }
  if (nrow(waters) == 1L) {
    m <- leaflet::setView(m, lng = waters$lng[1], lat = waters$lat[1], zoom = 11)
  } else {
    m <- leaflet::fitBounds(m, min(waters$lng), min(waters$lat), max(waters$lng), max(waters$lat))
  }
  htmlwidgets::onRender(m, MFA_map_js(), data = list(
    waters = water_info, efforts = effort_info, catches = catch_info,
    cpueValues = cpue_values, catchValues = catch_values,
    initial = Species, mode = Symbology,
    palette = c("#d9f0ef", "#a6d9d2", "#68b9ac", "#318d82", "#14594f"),
    radii = c(4, 6, 8, 10, 13)
  ))
}


# Internal JavaScript controller for leaflet drilldown.
MFA_map_js <- function() {
  'function(el,x,opts) {
  var map=this, waters=opts.waters||[], efforts=opts.efforts||[], catches=opts.catches||{};
  var palette=opts.palette, radii=opts.radii, mode=opts.mode;
  var cpueValues=opts.cpueValues||[], catchValues=opts.catchValues||[];
  var metric="Catch", selectedGear="", selectedUnits="";
  var catchByEffort={};
  catchValues.forEach(function(c){if(!catchByEffort[c.key])catchByEffort[c.key]={};catchByEffort[c.key][c.species]=c.total;});
  function filteredCpue(level){return cpueValues.filter(function(e){
    return e.level===level && (!selectedGear||e.gear===selectedGear) &&
      (!selectedUnits||e.units===selectedUnits);
  });}
  function valueFor(key,sp){
    if(metric==="CPUE"){
      if(!selectedGear)return null;
      var level=key.startsWith("e_")?"Effort":"Survey";
      var rows=filteredCpue(level).filter(function(e){return e.key===key && e.species===sp;});
      if(!rows.length)return null;
      // Survey rows pool across surveys in a waterbody; effort rows are direct.
      // CPUE_byGear has already validated effort and produced these totals.
      var catchSum=0,effortSum=0;
      rows.forEach(function(e){if(Number.isFinite(e.total)&&Number.isFinite(e.effort)&&e.effort>0){
        catchSum+=e.total;effortSum+=e.effort;
      }});
      return effortSum>0?catchSum/effortSum:null;
    }
    // Catch symbology retains the original, unfiltered catch counts.
    var keys=key.startsWith("e_")?[key]:Object.keys(catchByEffort).filter(function(k){
      var info=efforts.find(function(e){return e.key===k;});return info&&("w_"+info.water)===key;
    });
    var total=0,hasCatch=false;
    keys.forEach(function(k){
      var info=efforts.find(function(e){return e.key===k;});
      if(selectedGear && (!info || !info.gear.split(", ").includes(selectedGear)))return;
      var v=catchByEffort[k]||{};
      if(Object.prototype.hasOwnProperty.call(v,sp)&&v[sp]!=null){total+=Number(v[sp]);hasCatch=true;}
    });
    if(!selectedGear && !key.startsWith("e_")){var wc=catches[key]||{};if(wc[sp]!=null)return Number(wc[sp]);}
    return hasCatch?total:0;
  }

  var waterMarkers={}, effortMarkers={}, currentWater=null, currentSurvey=null, selectedSpecies="";
  function marker(id){return map.layerManager.getLayer("marker",id);}
  waters.forEach(function(w){waterMarkers[w.key]=marker(w.key);});
  efforts.forEach(function(e){effortMarkers[e.key]=marker(e.key);});
  function safe(s){return String(s==null?"":s).replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;").replace(/\\"/g,"&quot;");}
  function fmt(n){return metric==="CPUE"?Number(n).toLocaleString(undefined,{maximumFractionDigits:4}):Math.round(Number(n)).toLocaleString();}
  function getTotal(key,sp){return valueFor(key,sp);}
  function effortDetail(p){
    var d="<b>"+safe(p.name)+"</b><br>Survey: "+safe(p.survey)+"<br>Effort: "+safe(p.effort)+"<br>Gear: "+safe(p.gear);
    if(p.quantity!=null && Number.isFinite(Number(p.quantity)))d+="<br>Effort: "+safe(p.quantity)+" "+safe(p.units);
    else if(p.units)d+="<br>Effort units: "+safe(p.units);
    if(p.altQuantity!=null && Number.isFinite(Number(p.altQuantity)))d+="<br>Alternate effort: "+safe(p.altQuantity)+" "+safe(p.altUnits);
    if(selectedSpecies){var v=getTotal(p.key,selectedSpecies);d+="<br><b>"+safe(selectedSpecies)+" "+metric+":</b> "+(v==null?"Missing":fmt(v));}
    return d;
  }
  function currentLocations(){return currentSurvey===null?waters:efforts.filter(function(e){return e.survey===currentSurvey && e.water===currentWater && (!selectedGear || e.gear.split(", ").includes(selectedGear));});}
  function availableSpecies(loc){var found={};var keys=new Set(loc.map(function(p){return p.key;}));cpueValues.forEach(function(e){if(e.species && keys.has(e.key))found[e.species]=true;});
      catchValues.forEach(function(e){if(e.species && keys.has(e.key))found[e.species]=true;});return Object.keys(found).sort();}
  var control=L.control({position:"topright"});
  control.onAdd=function(){
    var box=L.DomUtil.create("div","leaflet-bar");
    box.style.cssText="background:#fff;padding:8px 10px;min-width:205px;max-width:250px;font:12px Arial,sans-serif;color:#142D3E;box-shadow:0 1px 5px #999";
    var title=L.DomUtil.create("div","",box);title.textContent="MFA Map";title.style.cssText="font-weight:bold;font-size:13px;margin-bottom:6px";
    var back=L.DomUtil.create("button","",box);back.type="button";back.textContent="← All waterbodies";
    back.style.cssText="display:none;margin-bottom:7px;padding:4px 6px;cursor:pointer;width:100%";
    var waterLabel=L.DomUtil.create("div","",box);waterLabel.style.cssText="font-weight:bold;margin-bottom:5px";
    var surveyLabel=L.DomUtil.create("label","",box);surveyLabel.textContent="Survey";surveyLabel.style.cssText="display:none;font-weight:bold;margin:4px 0";
    var surveySelect=L.DomUtil.create("select","",box);surveySelect.setAttribute("aria-label","Survey");
    surveySelect.style.cssText="display:none;width:100%;padding:4px;margin-bottom:7px";
    var drill=L.DomUtil.create("button","",box);drill.type="button";drill.textContent="Display Catch/Effort";
    drill.style.cssText="display:none;width:100%;padding:5px;cursor:pointer;margin-bottom:7px";
    var speciesLabel=L.DomUtil.create("label","",box);speciesLabel.textContent="Location or Species";speciesLabel.style.cssText="display:block;font-weight:bold;margin:4px 0";
    var speciesSelect=L.DomUtil.create("select","",box);speciesSelect.setAttribute("aria-label","Location or Species");
    speciesSelect.style.cssText="width:100%;padding:4px;margin-bottom:6px";
    var gearLabel=L.DomUtil.create("label","",box);gearLabel.textContent="Gear Type";gearLabel.style.cssText="display:block;font-weight:bold;margin:4px 0";
    var gearSelect=L.DomUtil.create("select","",box);gearSelect.style.cssText="width:100%;padding:4px;margin-bottom:6px";
    var metricLabel=L.DomUtil.create("label","",box);metricLabel.textContent="Symbology";metricLabel.style.cssText="display:block;font-weight:bold;margin:4px 0";
    var metricSelect=L.DomUtil.create("select","",box);metricSelect.style.cssText="width:100%;padding:4px;margin-bottom:6px";
    var legend=L.DomUtil.create("div","",box);legend.setAttribute("aria-live","polite");
    var note=L.DomUtil.create("div","",box);note.style.cssText="font-size:11px;color:#555;margin-top:5px";
    function option(select,value,label){var o=document.createElement("option");o.value=value;o.textContent=label;select.appendChild(o);}
    option(metricSelect,"Catch","Total Catch");option(metricSelect,"CPUE","CPUE");
    function populateGear(){
      var relevant=cpueValues.filter(function(e){return e.level==="Survey" && (currentSurvey===null || (e.survey===currentSurvey && e.key==="w_"+currentWater));});
      var gears=Array.from(new Set(relevant.map(function(e){return e.gear;}).filter(Boolean))).sort();
      gearSelect.replaceChildren();option(gearSelect,"","All gears (catch only)");
      gears.forEach(function(g){option(gearSelect,g,g);});
      if(selectedGear&&!gears.includes(selectedGear))selectedGear="";
      if(metric==="CPUE"&&!selectedGear&&gears.length)selectedGear=gears[0];
      gearSelect.value=selectedGear;
      var units=Array.from(new Set(relevant.filter(function(e){return e.gear===selectedGear;}).map(function(e){return e.units;}).filter(Boolean)));
      if(metric==="CPUE"&&units.length>1){
        selectedUnits=units[0];
        note.textContent="Multiple effort units exist for this gear; using "+selectedUnits+". Filter to consistent units for CPUE.";
      }else selectedUnits="";
    }
    function populateSpecies(){
      var prior=selectedSpecies;
      speciesSelect.replaceChildren();option(speciesSelect,"","Locations only");
      availableSpecies(currentLocations()).forEach(function(sp){option(speciesSelect,sp,sp);});
      if(prior && Array.from(speciesSelect.options).some(function(o){return o.value===prior;})) selectedSpecies=prior;
      else selectedSpecies="";
      speciesSelect.value=selectedSpecies;
    }
    function showMarkers(){
      waters.forEach(function(w){var mk=waterMarkers[w.key];if(!mk)return;
        if(currentSurvey===null){if(!map.hasLayer(mk))map.addLayer(mk);}else if(map.hasLayer(mk))map.removeLayer(mk);
      });
      efforts.forEach(function(e){var mk=effortMarkers[e.key];if(!mk)return;
        if(currentSurvey!==null && e.survey===currentSurvey && e.water===currentWater && (!selectedGear||e.gear.split(", ").includes(selectedGear))){if(!map.hasLayer(mk))map.addLayer(mk);}
        else if(map.hasLayer(mk))map.removeLayer(mk);
      });
    }
    function styleAndLegend(){
      showMarkers();var loc=currentLocations(), sp=selectedSpecies;
      var values=loc.map(function(p){return sp?getTotal(p.key,sp):0;});
      var positive=values.filter(function(v){return v!=null && Number.isFinite(v) && v>0;}).sort(function(a,b){return a-b;});
      function quantile(p){if(!positive.length)return NaN;var h=(positive.length-1)*p,lo=Math.floor(h),hi=Math.ceil(h);return positive[lo]+(positive[hi]-positive[lo])*(h-lo);}
      var q=[0,quantile(.25),quantile(.5),quantile(.75),quantile(1)];
      loc.forEach(function(p,i){
        var mk=(currentSurvey===null?waterMarkers:effortMarkers)[p.key];if(!mk)return;
        var v=values[i],cls=v==null?null:v===0?0:1+(v>q[1]?1:0)+(v>q[2]?1:0)+(v>q[3]?1:0);
        var color=sp?(cls===null?"#999999":mode==="Size"?"#277C78":palette[cls]):"#277C78";
        var radius=sp?(cls===null?5:mode==="Color"?7:radii[cls]):5;
        mk.setStyle({color:"#142D3E",weight:1,fillColor:color,fillOpacity:.85});mk.setRadius(radius);
        var desc=currentSurvey===null?
          "<b>"+safe(p.name)+"</b><br>MDNRID: "+safe(p.water)+"<br>Surveys: "+p.count:
          effortDetail(p);
        if(sp && currentSurvey===null)desc+="<br><b>"+safe(sp)+" "+metric+":</b> "+(v==null?"Missing":fmt(v));
        if(currentSurvey===null)desc+="<br><i>Click this marker to choose a survey.</i>";
        mk.bindPopup(desc);
      });
      legend.replaceChildren();
      var heading=document.createElement("div");heading.textContent=metric==="CPUE"?"CPUE by Species"+(selectedUnits?" ("+selectedUnits+")":""):"Total Catch by Species";heading.style.cssText="font-weight:bold;margin:5px 0";legend.appendChild(heading);
      if(!sp){var msg=document.createElement("div");msg.textContent="Select a species to display catch.";legend.appendChild(msg);return;}
      var labels=["0"];
      for(var k=1;k<=4;k++){
        labels.push(positive.length?(metric==="CPUE"?fmt(k===1?Math.min.apply(null,positive):q[k-1])+" – "+fmt(q[k]):fmt(k===1?1:Math.floor(q[k-1])+1)+" – "+fmt(Math.floor(q[k]))):"No positive catch");
      }
      for(var j=0;j<5;j++){
        var row=document.createElement("div");row.style.cssText="display:grid;grid-template-columns:32px 1fr;align-items:center;column-gap:7px;height:29px";
        var slot=document.createElement("span");slot.style.cssText="display:flex;width:32px;height:29px;align-items:center;justify-content:center";
        var dot=document.createElement("span");var r=mode==="Color"?7:radii[j];
        dot.style.cssText="display:block;flex:none;border:1px solid #142D3E;border-radius:50%;box-sizing:border-box;width:"+(2*r)+"px;height:"+(2*r)+"px;background:"+(mode==="Size"?"#277C78":palette[j]);
        slot.appendChild(dot);var txt=document.createElement("span");txt.textContent=labels[j];row.appendChild(slot);row.appendChild(txt);legend.appendChild(row);
      }
      if(!note.textContent.startsWith("Multiple effort units"))note.textContent="Zero + four quartiles of positive values. CPUE calculated by CPUE_byGear().";
    }
    function resetBounds(){var loc=currentLocations(),mk=loc.map(function(p){return (currentSurvey===null?waterMarkers:effortMarkers)[p.key];}).filter(Boolean);
      if(mk.length>1){var b=L.featureGroup(mk).getBounds();if(b.isValid())map.fitBounds(b.pad(.12));}
      else if(mk.length===1)map.setView(mk[0].getLatLng(),11);
    }
    function enterWater(w){
      currentWater=w.water;currentSurvey=null;waterLabel.textContent=w.name;
      back.style.display="block";surveyLabel.style.display="block";surveySelect.style.display="block";
      surveySelect.replaceChildren();
      var surveyIds=Array.isArray(w.surveys)?w.surveys:(w.surveys==null?[]:[w.surveys]);
      surveyIds.forEach(function(id){option(surveySelect,String(id),"Survey "+id);});
      drill.style.display="block";
      drill.disabled=!surveyIds.length || !efforts.some(function(e){return e.water===w.water && surveyIds.some(function(id){return String(id)===e.survey;});});
      drill.title=drill.disabled?"Effort locations are unavailable. Supply con or effort/catch data.":"";
      populateGear();populateSpecies();styleAndLegend();
    }
    waters.forEach(function(w){var mk=waterMarkers[w.key];if(mk)mk.on("click",function(){enterWater(w);});});
    drill.addEventListener("click",function(){
      var survey=surveySelect.value;
      if(!efforts.some(function(e){return e.water===currentWater && e.survey===survey})){
        note.textContent="No effort coordinates for this survey.";return;
      }
      currentSurvey=survey;drill.style.display="none";surveyLabel.style.display="none";surveySelect.style.display="none";
      populateGear();populateSpecies();styleAndLegend();resetBounds();
    });
    // At coincident effort coordinates, a single click offers a chooser for every effort.
    // The selected effort is shown in the popup without moving its true location.
    function overlapping(e){
      var mk=effortMarkers[e.key];if(!mk)return [e];
      var pt=mk.getLatLng();
      return currentLocations().filter(function(other){
        var om=effortMarkers[other.key];if(!om)return false;
        var op=om.getLatLng();
        return Math.abs(op.lat-pt.lat)<1e-7 && Math.abs(op.lng-pt.lng)<1e-7;
      });
    }
    efforts.forEach(function(e){
      var mk=effortMarkers[e.key];if(!mk)return;
      mk.on("click",function(){
        if(currentSurvey===null)return;
        var matches=overlapping(e);
        if(matches.length<2){mk.bindPopup(effortDetail(e)).openPopup();return;}
        var wrap=document.createElement("div");
        var heading=document.createElement("div");heading.textContent=matches.length+" efforts at this location";heading.style.fontWeight="bold";wrap.appendChild(heading);
        var select=document.createElement("select");select.setAttribute("aria-label","Choose overlapping effort");select.style.cssText="width:100%;margin:5px 0;padding:3px";
        matches.forEach(function(item){option(select,item.key,"Effort "+item.effort+(item.gear?" ("+item.gear+")":""));});
        wrap.appendChild(select);
        var detail=document.createElement("div");wrap.appendChild(detail);
        select.value=e.key;
        function refresh(){var chosen=matches.find(function(item){return item.key===select.value;});detail.innerHTML=effortDetail(chosen||e);}
        select.addEventListener("change",refresh);refresh();
        mk.bindPopup(wrap,{maxWidth:320}).openPopup();
      });
    });
    back.addEventListener("click",function(){
      currentWater=null;currentSurvey=null;waterLabel.textContent="";back.style.display="none";
      surveyLabel.style.display="none";surveySelect.style.display="none";drill.style.display="none";
      populateGear();populateSpecies();styleAndLegend();resetBounds();
    });
    speciesSelect.addEventListener("change",function(){selectedSpecies=speciesSelect.value;styleAndLegend();});
    gearSelect.addEventListener("change",function(){selectedGear=gearSelect.value;populateGear();populateSpecies();styleAndLegend();});
    metricSelect.addEventListener("change",function(){metric=metricSelect.value;populateGear();populateSpecies();styleAndLegend();});
    L.DomEvent.disableClickPropagation(box);L.DomEvent.disableScrollPropagation(box);
    selectedSpecies=opts.initial||"";populateGear();populateSpecies();styleAndLegend();
    return box;
  };
  control.addTo(map);
}
'
}
