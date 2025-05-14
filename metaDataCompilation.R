## compile ISO 19115 compliant metadata for Cook Inlet drifter data


## Core fields
### Title
### Geographic and Temporal Extents (bounding)
### Citation Information
### Abstract
### Keywords
### Data License -- Creative Commons 1.0 Universal Public Domain Dedication (CCO-1.0) encouraged


## NetCDF template??




require ("geometa")



## data sets to cover
## CTD  (output of aggregated files)
## zooplankton
## phytoplankton
## chlorophyll
## ocean acidification
## drifter


### template from geometa documentation

#Example of ISO 19115/19139 metadata generated with geometa
#This example is valid according to INSPIRE common metadata requirements

md = ISOMetadata$new()
md$setFileIdentifier("my-metadata-identifier")
md$setParentIdentifier("my-parent-metadata-identifier")
md$setCharacterSet("utf8")
md$setLanguage("eng")
# md$setDateStamp(ISOdate(2015, 1, 1, 1))
md$setDateStamp(Sys.time())
md$setMetadataStandardName("ISO 19115:2003/19139")
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

#VectorSpatialRepresentation
vsr <- ISOVectorSpatialRepresentation$new()
vsr$setTopologyLevel("geometryOnly")
geomObject <- ISOGeometricObjects$new()
geomObject$setGeometricObjectType("surface")
geomObject$setGeometricObjectCount(5L)
vsr$setGeometricObjects(geomObject)
md$addSpatialRepresentationInfo(vsr)

#ReferenceSystem
rs <- ISOReferenceSystem$new()
rsId <- ISOReferenceIdentifier$new(code = "4326", codeSpace = "EPSG")
rs$setReferenceSystemIdentifier(rsId)
md$setReferenceSystemInfo(rs)

#data identification
ident <- ISODataIdentification$new()
ident$setAbstract("CTD casts were undertaken along predefined transects in lower
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
                  samples were taken concurrently. ")
ident$setPurpose("purpose")
ident$addCredit("credit1")
# ident$addCredit("credit2")
# ident$addCredit("credit3")
ident$addStatus("completed")
ident$setLanguage("eng")
ident$setCharacterSet("utf8")
ident$addTopicCategory("biota")
ident$addTopicCategory("oceans")

#adding a point of contact
rp <- ISOResponsibleParty$new()  ## for metadata and data itself
rp$setIndividualName("Martin Renner and Kris Holderied")
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
d$setDate(ISOdate(2015, 1, 1, 1))
d$setDateType("publication")
ct$addDate(d)
ct$setEdition("1.0")
ct$setEditionDate(as.Date(ISOdate(2015, 1, 1, 1)))
ct$addIdentifier(ISOMetaIdentifier$new(code = "identifier"))
ct$addPresentationForm("mapDigital")
ct$addCitedResponsibleParty(rp)
ident$setCitation(ct)

#graphic overview
go1 <- ISOBrowseGraphic$new(
  fileName = "http://wwww.somefile.org/png1",
  fileDescription = "Map Overview 1",
  fileType = "image/png"
)
go2 <- ISOBrowseGraphic$new(
  fileName = "http://www.somefile.org/png2",
  fileDescription = "Map Overview 2",
  fileType = "image/png"
)
ident$addGraphicOverview(go1)
ident$addGraphicOverview(go2)

#maintenance information
mi <- ISOMaintenanceInformation$new()
mi$setMaintenanceFrequency("monthly")
ident$setResourceMaintenance(mi)

#adding access legal constraints
#for INSPIRE controlled terms on access legal constraints, please browse the INSPIRE registry:
# http://inspire.ec.europa.eu/metadata-codelist/LimitationsOnPublicAccess/
lc <- ISOLegalConstraints$new()
lc$addAccessConstraint("otherRestrictions")
lc$addOtherConstraint(ISOAnchor$new(
  href = "http://inspire.ec.europa.eu/metadata-codelist/LimitationsOnPublicAccess/INSPIRE_Directive_Article13_1a",
  name = "public access limited according to Article 13(1)(a) of the INSPIRE Directive"
))
ident$addResourceConstraints(lc)

#adding use legal constraints
#for INSPIRE controlled terms on use legal constraints, please browse the INSPIRE registry:
# http://inspire.ec.europa.eu/metadata-codelist/ConditionsApplyingToAccessAndUse
lc2 <- ISOLegalConstraints$new()
lc2$addUseLimitation("limitation1")
lc2$addUseLimitation("limitation2")
lc2$addUseLimitation("limitation3")
lc2$addAccessConstraint("otherRestrictions")
lc2$addOtherConstraint(ISOAnchor$new(
  href = "http://inspire.ec.europa.eu/metadata-codelist/ConditionsApplyingToAccessAndUse/noConditionsApply",
  name = "No conditions apply to access and use."
))
ident$addResourceConstraints(lc2)

#adding security constraints
sc <- ISOSecurityConstraints$new()
sc$setClassification("secret")
sc$setUserNote("ultra secret")
sc$setClassificationSystem("no classification in particular")
sc$setHandlingDescription("description")
ident$addResourceConstraints(sc)

#adding extent
extent <- ISOExtent$new()
bbox <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
extent$setGeographicElement(bbox)
ident$setExtent(extent)

#add keywords
kwds <- ISOKeywords$new()
kwds$addKeyword("keyword1")
kwds$addKeyword("keyword2")
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

md$setIdentificationInfo(ident)

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
distrib$setDigitalTransferOptions(dto)
md$setDistributionInfo(distrib)

#create dataQuality object with a 'dataset' scope
dq <- ISODataQuality$new()
scope <- ISOScope$new()
scope$setLevel("dataset")
dq$setScope(scope)

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

md$setDataQualityInfo(dq)

#XML representation of the ISOMetadata
xml <- md$encode()

require ("XML")
XML::saveXML (xml, file="~/tmp/LCI_noaa/data-products/test.xml")

require ("XimpLe")
cat (pasteXML (xml), file="~/tmp/LCI_noaa/data-products/testX.xml")
cat (pasteXML )
