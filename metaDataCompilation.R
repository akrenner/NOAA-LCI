## compile ISO 19115 compliant metadata for Cook Inlet drifter data


## Core fields
### Title
### Geographic and Temporal Extents (bounding)
### Citation Information
### Abstract
### Keywords
### Data License -- Creative Commons 1.0 Universal Public Domain Dedication (CCO-1.0) encouraged


## NetCDF template??



rm (list=ls())
require ("geometa")
require ("sf")
LLprj <- 4326



## data sets to cover
## CTD  (output of aggregated files)
## zooplankton
## phytoplankton
## chlorophyll
## ocean acidification
## drifter


## load data
## supply abstract
## set bbox -- geographic boundaries
## define field names
## set file name for XML file

dsetL <- c ("ctd", "zoop", "phytop" #, "chlorop", nutrients
            , "drifter")

for (dset in dsetL){

  ## data-set specific steps
  if (dset == "ctd"){
    ### CTD

    ctdL <- list.files("~/tmp/LCI_noaa/data-products/CTD/", ".csv", full.names=TRUE)
    data <- lapply (seq_along (ctdL), function (i){read.csv(ctdL[i], skip=1)})
    data <- do.call (rbind, data) |>
      st_as_sf (coords=c("Longitude_DD", "Latitude_DD"), crs=LLprj, remove=FALSE)
    bbox <- st_bbox(data)
    tbox <- range (as.POSIXct(paste (data$Date, data$Time)))
    abstract <- "CTD casts were undertaken along predefined transects in lower
                  Cook Inlet (Transects 3, 6, 7) and  Kachemak Bay and
                  (AlongBay, 4, 9). In the years 2013 to 2017, Kachemak Bay was
                  surveyed monthly, lower Cook Inlet quarterly. From 2017 onwards
                  the Cook Inlet transects were no longer surveyed in full, but
                  the AlongBay transect was extended in quarterly surveys to
                  just north of Elisabeth Island. Hex files uploaded from the
                  instruments were processed using Seabird software. Most recent
                  versions of the data can be found on
                  https://github.com/akrenner/LCI R scripts used for processing
                  are available at https://github.com/akrenner/NOAA-LCI
                  CTD profiler also includes instruments to measure turbidity,
                  PAR, and fluorescence. At some stations, nutrient, ocean
                  acidification, chlorophyll, phytoplanton, and zooplankton
                  samples were taken concurrently. "
    keywords <- c("physical oceanography")
    fN <- "~/tmp/LCI_noaa/data-products/CTD/CookInletKachemakBay_CTD.xml"
    fN <- "~/tmp/LCI_noaa/data-products/CTD.xml"

    }else if (dset == "zoop"){  ## zoop
      ### zooplankton

      data <- read.csv("~/tmp/LCI_noaa/data-products/zooplankton_KachemakBay.csv", skip=1) |>
        st_as_sf (coords=c("Longitude_DD", "Latitude_DD"), crs=LLprj, remove=FALSE)
      bbox <- st_bbox(data)

      abstract <- "These data are part of the Gulf Watch Alaska (GWA), Environmental
    Drivers component, which is the long-term ecosystem monitoring program of the
    Exxon Valdez Oil Spill Trustee Council for the marine ecosystem affected by
    the 1989 oil spill.

This dataset is two comma-separated values (csv) files containing zooplankton
counts by species from samples collected during Lower Cook Inlet oceanographic
surveys. One data file (named' Raw') contains the raw zooplankton counts, whereas
the file named 'Processed' contains densities, where the counts have been divided
by the respective water volume (calculated from the flow meter and number of splits).

Zooplankton samples were collected during 2012-2019 as part of a long-term
oceanographic monitoring project in Kachemak Bay and lower Cook Inlet.
Zooplankton were collected quarterly in lower Cook Inlet and outer Kachemak Bay
and monthly in Kachemak Bay with a bongo style zooplankton net: 60 cm mouth
diameter/333 μm mesh (Aquatic Research Instruments, Hope, ID). To calculate
sample volume, a mechanical flow meter (General Oceanics) was attached to one
side of the bongo frame; zooplankton were sampled from the net with no flow meter
attached. At each station, 50 m vertical tows were conducted at a tow rate of
approximately 0.5 m/s with an average of 14.15 m3 sampled. Preserved samples were
identified and enumerated to lowest taxonomic classification possible by the
Prince William Sound Science Center located in Cordova, Alaska."
      # keywords: zooplankton, copepods,
      fN <- "~/tmp/LCI_noaa/data-products/zooplankton_KachemakBay.xml"

    }else if (dset == "phytop"){
    ### phytoplankton

    data <- read.csv ("~/tmp/LCI_noaa/data-products/phytoplankton.csv", skip=1) |>
      subset (!is.na (Longitude_DD)) |>
      subset (!is.na (Latitude_DD)) |>
      st_as_sf (coords=c("Longitude_DD", "Latitude_DD"), crs=LLprj, remove=FALSE)

    bbox <- st_bbox(data)
    abstract <- ""
    fN <- "~/tmp/LCI_noaa/data-products/phytoplankton.xml"

  }else if (dset == "chlorop"){
    ### chlorophyll

    abstract <- ""

  }else if (dset == "drifter"){
    ### drifter

    data <- read.csv ("~/tmp/LCI_noaa/data-products/drifter_cleaned.csv.gz") |>
      st_as_sf (coords=c("Long", "Lat"), crs=LLprj, remove=FALSE)
    bbox <- st_bbox (data)
    abstract <- ""
    fN <- "~/tmp/LCI_noaa/data-products/drifter_cleaned.xml"
  }


  ### template from geometa documentation

  #Example of ISO 19115/19139 metadata generated with geometa
  #This example is valid according to INSPIRE common metadata requirements

  md = ISOMetadata$new()
  md$setFileIdentifier("my-metadata-identifier")
  md$setParentIdentifier("my-parent-metadata-identifier")
  # md$setCharacterSet("utf8") # deprecated
  # md$addCharacterSet("utf8") # already the default
md$setLanguage("eng")
  # md$addLanguage ("eng") ## xxx error
  # md$setDateStamp(ISOdate(2015, 1, 1, 1))
  md$setDateStamp(Sys.time())
  md$setMetadataStandardName("ISO 19115:2003/19139")   ## XXXX verify!
  md$setMetadataStandardVersion("1.0")
  md$setDataSetURI("my-dataset-identifier")


  ## add contacts
  rp <- ISOResponsibleParty$new()  ## for metadata
  rp$setIndividualName("Martin Renner")
  rp$setOrganisationName("CSS, under contract to Kasitsna Bay Lab, NCCOS, NOAA")
  rp$setPositionName("Data Scientist")
  rp$setRole("pointOfContact")
  contact <- ISOContact$new()
  phone <- ISOTelephone$new()
  phone$setVoice("323 372-1870")
  address <- ISOAddress$new()
  address$setDeliveryPoint("95 Sterling Highway, Suite 2")
  address$setCity("Homer")
  address$setPostalCode("99603")
  address$setCountry("USA")
  address$setEmail("martin.renner@noaa.gov")
  contact$setAddress(address)
  rp$setContactInfo(contact)
  md$addContact(rp)

  rp <- ISOResponsibleParty$new()
  rp$setIndividualName("Kris Holderied")
  rp$setOrganisationName("Kasitsna Bay Lab, NCCOS, NOAA")
  rp$setPositionName("Interim Director")
  rp$setRole("pointOfContact")
  contact <- ISOContact$new()
  phone <- ISOTelephone$new()
  phone$setVoice("202-417-6937")
  address <- ISOAddress$new()
  address$setDeliveryPoint("95 Sterling Highway, Suite 2")
  address$setCity("Homer")
  address$setPostalCode("99603")
  address$setCountry("USA")
  address$setEmail("kris.holderied@noaa.gov")
  contact$setAddress(address)
  rp$setContactInfo(contact)
  md$addContact(rp)

  if (0){
    #add 3 contacts
    for(i in 1:3){
      rp <- ISOResponsibleParty$new()
      rp$setIndividualName(paste0("someone",i))
      rp$setOrganisationName("somewhere")
      rp$setPositionName(paste0("someposition",i))
      rp$setRole("pointOfContact")
      contact <- ISOContact$new()
      phone <- ISOTelephone$new()
      phone$setVoice(paste0("myphonenumber",i))
      phone$setFacsimile(paste0("myfacsimile",i))
      contact$setPhone(phone)
      address <- ISOAddress$new()
      address$setDeliveryPoint("theaddress")
      address$setCity("thecity")
      address$setPostalCode("111")
      address$setCountry("France")
      address$setEmail("someone@theorg.org")
      contact$setAddress(address)
      res <- ISOOnlineResource$new()
      res$setLinkage("http://somelink")
      res$setName("someresourcename")
      contact$setOnlineResource(res)
      rp$setContactInfo(contact)
      md$addContact(rp)
    }
  }

  # #VectorSpatialRepresentation
  # vsr <- ISOVectorSpatialRepresentation$new()
  # vsr$setTopologyLevel("geometryOnly")
  # geomObject <- ISOGeometricObjects$new()
  # geomObject$setGeometricObjectType("surface")
  # geomObject$setGeometricObjectCount(5L)
  # vsr$setGeometricObjects(geomObject)
  # md$addSpatialRepresentationInfo(vsr)

  #ReferenceSystem
  rs <- ISOReferenceSystem$new()
  rsId <- ISOReferenceIdentifier$new(code = "4326", codeSpace = "EPSG")
  rs$setReferenceSystemIdentifier(rsId)
  md$addReferenceSystemInfo(rs)
  # md$setReferenceSystemInfo(rs)

  #data identification
  ident <- ISODataIdentification$new()
  ident$setAbstract(abstract)
  ident$setPurpose("These data are part of a larger program that includes the NOAA
                 water quality monitoring project under Kachemak Bay Research
                 Reserve's System Wide Monitoring Program and lower Cook Inlet
                 and Kachemak Bay circulation modeling project under the Bureau
                 of Ocean Energy Management as well as providing information for
                 NOAA's Harmful Algal Bloom program and for local resource
                 managers.")


  ident$addCredit("Gulf Watch Alaska")
  ident$addCredit("Kachemak Bay National Estuarine Research Reserve")
  # ident$addCredit("credit3")
  ident$addStatus("completed")
  # ident$setLanguage("eng")
  ident$addLanguage("eng")
  ident$addCharacterSet("utf8") # deprecated -- and already present
  ident$addTopicCategory("biota")
  ident$addTopicCategory("oceans")

  #adding a point of contact
  rp <- ISOResponsibleParty$new()  ## for metadata and data itself
  rp$setIndividualName("Martin Renner, Kris Holderied, Dom Hondolero, and Paul Cziko")
  rp$setOrganisationName("Kasitsna Bay Lab, NCCOS, NOAA")
  # rp$setPositionName("someposition")
  rp$setRole("pointOfContact")
  contact <- ISOContact$new()
  phone <- ISOTelephone$new()
  # phone$setVoice("myphonenumber")
  # phone$setFacsimile("myfacsimile")
  # contact$setPhone(phone)
  address <- ISOAddress$new()
  address$setDeliveryPoint("95 Sterling Highway, Suite 2")
  address$setCity("Homer")
  address$setPostalCode("99603")
  address$setCountry("USA")
  address$setEmail("nccos.kasitsna@noaa.gov")
  contact$setAddress(address)
  res <- ISOOnlineResource$new()
  res$setLinkage("https://coastalscience.noaa.gov/about/facilities/alaska/")
  res$setName("Kasitsna Bay Lab")
  contact$setOnlineResource(res)
  rp$setContactInfo(contact)
  ident$addPointOfContact(rp)

  #citation
  ct <- ISOCitation$new()   ## XXX
  ct$setTitle("Conductivity Temperature Depth (CTD) data from Cook Inlet and Kachemak Bay")
  d <- ISODate$new()
  # d$setDate(ISOdate(2015, 1, 1, 1))
  d$setDate(Sys.time())
  # d$setDateType("publication")
  # ct$addDate(d)
  # ct$setEdition("1.0")
  # ct$setEditionDate(as.Date(ISOdate(2015, 1, 1, 1)))
  # ct$addIdentifier(ISOMetaIdentifier$new(code = "identifier"))
  # ct$addPresentationForm("mapDigital")
  ct$addCitedResponsibleParty(rp)
  ident$setCitation(ct)

  # #graphic overview
  # go1 <- ISOBrowseGraphic$new(
  #   fileName = "http://wwww.somefile.org/png1",
  #   fileDescription = "Map Overview 1",
  #   fileType = "image/png"
  # )
  # go2 <- ISOBrowseGraphic$new(
  #   fileName = "http://www.somefile.org/png2",
  #   fileDescription = "Map Overview 2",
  #   fileType = "image/png"
  # )
  # ident$addGraphicOverview(go1)
  # ident$addGraphicOverview(go2)

  #maintenance information
  mi <- ISOMaintenanceInformation$new()
  mi$setMaintenanceFrequency("monthly")
  # ident$setResourceMaintenance(mi)
  ident$addResourceMaintenance(mi)

  # #adding access legal constraints
  # #for INSPIRE controlled terms on access legal constraints, please browse the INSPIRE registry:
  # # http://inspire.ec.europa.eu/metadata-codelist/LimitationsOnPublicAccess/
  # lc <- ISOLegalConstraints$new()
  # lc$addAccessConstraint("otherRestrictions")
  # lc$addOtherConstraint(ISOAnchor$new(
  #   href = "http://inspire.ec.europa.eu/metadata-codelist/LimitationsOnPublicAccess/INSPIRE_Directive_Article13_1a",
  #   name = "public access limited according to Article 13(1)(a) of the INSPIRE Directive"
  # ))
  # ident$addResourceConstraints(lc)

  # #adding use legal constraints
  # #for INSPIRE controlled terms on use legal constraints, please browse the INSPIRE registry:
  # # http://inspire.ec.europa.eu/metadata-codelist/ConditionsApplyingToAccessAndUse
  # lc2 <- ISOLegalConstraints$new()
  # lc2$addUseLimitation("limitation1")
  # lc2$addUseLimitation("limitation2")
  # lc2$addUseLimitation("limitation3")
  # lc2$addAccessConstraint("otherRestrictions")
  # lc2$addOtherConstraint(ISOAnchor$new(
  #   href = "http://inspire.ec.europa.eu/metadata-codelist/ConditionsApplyingToAccessAndUse/noConditionsApply",
  #   name = "No conditions apply to access and use."
  # ))
  # ident$addResourceConstraints(lc2)
  #
  # #adding security constraints
  # sc <- ISOSecurityConstraints$new()
  # sc$setClassification("secret")
  # sc$setUserNote("ultra secret")
  # sc$setClassificationSystem("no classification in particular")
  # sc$setHandlingDescription("description")
  # ident$addResourceConstraints(sc)

  #adding extent
  extent <- ISOExtent$new()
  # bbox <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
  bbx <- ISOGeographicBoundingBox$new(minx=bbox[1], miny=bbox[2], maxx=bbox[3], maxy=bbox[4])
  extent$addGeographicElement(bbx)
  # extent$addGeographicElement(data)  ## xxx easier, should work -- but doesn't

  # extent$addTemporalElement()
  # extent$addVerticalElement()

  ident$addExtent(extent)


  #add keywords
  kwds <- ISOKeywords$new()
  for (j in seq_along(keywords)){
    kwds$addKeyword (keywords [j])
  }
  # kwds$addKeyword("keyword1")
  # kwds$addKeyword("keyword2")
  kwds$setKeywordType("theme")
  th <- ISOCitation$new()
  th$setTitle("General")
  th$addDate(d)
  kwds$setThesaurusName(th)
  ident$addKeywords(kwds)

  #supplementalInformation
  ident$setSupplementalInformation("some additional information")

  #spatial representation type
  ident$addSpatialRepresentationType("vector")

  md$addIdentificationInfo(ident)

  #Distribution
  distrib <- ISODistribution$new()
  dto <- ISODigitalTransferOptions$new()
  for(i in 1:3){
    or <- ISOOnlineResource$new()
    or$setLinkage(paste0("http://somelink",i))
    or$setName(paste0("name",i))
    or$setDescription(paste0("description",i))
    or$setProtocol("WWW:LINK-1.0-http--link")
    dto$addOnlineResource(or)
  }
  distrib$addDigitalTransferOptions(dto)
  md$setDistributionInfo(distrib)


  #create dataQuality object with a 'dataset' scope
  dq <- ISODataQuality$new()
  # scope <- ISOScope$new()  ## Error in self$checkMetadataStandardCompliance() :  Class 'ISOScope' can't be loaded with current metadata standard '19139'
  # scope$setLevel("dataset")
  # dq$setScope(scope)

  #add data quality reports...

  #add a report the data quality
  dc <- ISODomainConsistency$new()
  result <- ISOConformanceResult$new()
  spec <- ISOCitation$new()
  spec$setTitle("Data Quality check")
  spec$addAlternateTitle("This is is some data quality check report")
  d <- ISODate$new()
  d$setDate(as.Date(ISOdate(2015, 1, 1, 1)))
  d$setDateType("publication")
  spec$addDate(d)
  result$setSpecification(spec)
  result$setExplanation("some explanation about the conformance")
  result$setPass(TRUE)
  dc$addResult(result)
  dq$addReport(dc)

  #add INSPIRE reports?
  #INSPIRE - interoperability of spatial data sets and services
  dc_inspire1 <- ISODomainConsistency$new()
  cr_inspire1 <- ISOConformanceResult$new()
  cr_inspire_spec1 <- ISOCitation$new()
  cr_inspire_spec1$setTitle("Commission Regulation (EU) No 1089/2010 of 23 November 2010 implementing Directive 2007/2/EC of the European Parliament and of the Council as regards interoperability of spatial data sets and services")
  cr_inspire1$setExplanation("See the referenced specification")
  cr_inspire_date1 <- ISODate$new()
  cr_inspire_date1$setDate(as.Date(ISOdate(2010,12,8)))
  cr_inspire_date1$setDateType("publication")
  cr_inspire_spec1$addDate(cr_inspire_date1)
  cr_inspire1$setSpecification(cr_inspire_spec1)
  cr_inspire1$setPass(TRUE)
  dc_inspire1$addResult(cr_inspire1)
  dq$addReport(dc_inspire1)
  #INSPIRE - metadata
  dc_inspire2 <- ISODomainConsistency$new()
  cr_inspire2 <- ISOConformanceResult$new()
  cr_inspire_spec2 <- ISOCitation$new()
  cr_inspire_spec2$setTitle("COMMISSION REGULATION (EC) No 1205/2008 of 3 December 2008 implementing Directive 2007/2/EC of the European Parliament and of the Council as regards metadata")
  cr_inspire2$setExplanation("See the referenced specification")
  cr_inspire_date2 <- ISODate$new()
  cr_inspire_date2$setDate(as.Date(ISOdate(2008,12,4)))
  cr_inspire_date2$setDateType("publication")
  cr_inspire_spec2$addDate(cr_inspire_date2)
  cr_inspire2$setSpecification(cr_inspire_spec2)
  cr_inspire2$setPass(TRUE)
  dc_inspire2$addResult(cr_inspire2)
  dq$addReport(dc_inspire2)

  #add lineage (more example of lineages in ISOLineage documentation)
  lineage <- ISOLineage$new()
  lineage$setStatement("statement")
  dq$setLineage(lineage)

  md$addDataQualityInfo(dq)

  #XML representation of the ISOMetadata
  xml <- md$encode()

  require ("XML")  ## why not: numerous dependencies, needs to be compiled
  XML::saveXML (xml, file=fN)
  rm (xml, fN, md, dq, dc)
}
