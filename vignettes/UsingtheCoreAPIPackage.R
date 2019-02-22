## ----eval = FALSE--------------------------------------------------------
#  api<-coreAPI("PATH TO JSON FILE")
#  login<- authBasic(api)
#  response <-apiCall(login$coreApi,body,"json",,special=NULL,useVerbose=FALSE)
#  logOut(login$coreApi )

## ----eval = FALSE--------------------------------------------------------
#  
#  #get login information
#  
#  loginInfo<-coreAPI(CoreAccountInfo="path/account.json")
#  
#  # authenticate
#  response<- authBasic(loginInfo)
#  
#  #print the session ID
#  print(response$coreApi$jsessionId)
#  
#  #log out
#  logOut(response$coreApi)$success
#  

## ----eval = FALSE--------------------------------------------------------
#  
#  createEntity(response$coreApi,entityType)
#  

## ----eval = FALSE--------------------------------------------------------
#  
#  attributeValues<-list(SOURCE_LAB = "New York Medical Center",Number = 5,REQUESTOR="CEP")
#  

## ----eval= FALSE---------------------------------------------------------
#  associations<-list(SAMPLE_ENZYME=data.frame(name="",entitiyId="",barcode="ENZ1"))

## ----eval = FALSE--------------------------------------------------------
#  
#  entityType <- "Patient Sample"
#  
#  attributeValues<-list(SOURCE_LAB = "New York Medical Center",Number = 5,REQUESTOR="CEP")
#  
#  associations<-list(SAMPLE_ENZYME=data.frame(name="",entitiyId="",barcode="ENZ1"))
#  
#  createEntity(response$coreApi,entityType,attributeValues,associations)
#  

## ----eval = FALSE--------------------------------------------------------
#  entityBarcode <-"PS100"
#  
#  updateEntity(response$coreApi,entityType,entitybarcode,
#                        attributevalues=attributeValues,associations = associations)
#  

## ----eval = FALSE--------------------------------------------------------
#  
#  get<-getEntityByBarcode(response$coreApi, entityType, barcode)
#  
#  item <- get$entity

## ----eval = FALSE--------------------------------------------------------
#  
#  get<-getEntityById(response$coreApi, entityType, entityId = "ID")
#  
#  item <- get$entity

## ----eval = FALSE--------------------------------------------------------
#  
#  # attach to the attribute named File
#  
# items2<-attachFile(response$coreApi,barcode,"SAgraph.png",filepath,targetAttributeName="File")
#  
#  #attach to the entity
#  
#  attachFile(response$coreApi,barcode,filename="SAgraph.png",filepath=filepath,targetAttributeName="")
#  
#  

## ----eval =FALSE---------------------------------------------------------
#  item$entity$values$FILE
#  $stringData
#  [1] "{'file':'SAgraph.png.16','url':'https://experience.platformforscience.com/corelims?cmd=view-file&entityType=FILE ATTRIBUTE&entityId=9532882'}"
#  

## ----eval = FALSE--------------------------------------------------------
#  
#  
#  #Create Sample
#  
#  sample<-createEntity(response$coreApi, "WHOLE BLOOD", attributeValues = attributes)
#  
#  #get barcode of the sample
#  
#  sampleBC <-sample$entity$barcode
#  
#  #Create Lot
#  
#  item<-createSampleLot(login$coreApi,"WHOLE BLOOD LOT", sampleBarcode=sampleBC,
#                        attributes = lotAttributes )
#  

## ----eval = FALSE--------------------------------------------------------
#  
#  
#  api<-coreAPI(CoreAccountInfo = "testfiles/account.json")
#  
#  #login
#  
#  login<-authBasic(tapi)
#  
#  #create sample
#  
#  sample<-createEntity(r$coreApi,"Patient Sample")
#  
#  sampleBarcode<-sample$entity$barcode
#  
#  #create lot
#  
#  lot<-createSampleLot(r$coreApi,"PATIENT SAMPLE LOT",sampleBarcode)
#  
#  #create container
#  
#  cont <- createEntity(r$coreApi,containerType)
#  
#  cont_barcode <- cont$entity$barcode
#  
#  #fill container
#  
#  filledContainer<-updateCellContents(r$coreApi,containerType,
#                                               cont_barcode,"1", lot_barcode,2, "ml", 3, "uM")
#  
#  #log out
#  
#  out<-logOut(r$coreApi)
#  
#  

## ----eval = FALSE--------------------------------------------------------
#  
#  cell<-getCellContents(login$coreApi,"VIA9","1")
#  

## ----eval = FALSE--------------------------------------------------------
#  
#  contCell <- getCellContents(r$coreApi,cont$entity$barcode,"1")
#  
#  sourceCellID <-contCell$entity$cells[[1]]$cellId
#  
#  cont2Cell<- getCellContents(r$coreApi,cont2$entity$barcode,"1")
#  
#  destCellID <- cont2Cell$entity$cells[[1]]$cellId
#  
#  
#  move<-transferCellContents(r$coreApi,sourceCellID,destCellID,amount,concentration,
#                                              amountUnit, concentrationUnit,useVerbose = FALSE)
#  
#  

## ----eval = FALSE--------------------------------------------------------
#  
#  lineage<-getContainerLineage(r$coreApi,cont$entity$barcode)
#  

## ----eval=FALSE----------------------------------------------------------
#  
#   item <- createEntity(r$coreApi,entityType = experimentType,
#                                 attributeValues = list(TEST_ATTRIBUTE = "a value"),
#                                 associations = list(EXPERIMENT_ASSAY=
#                                 data.frame(name="",entityId ="",barcode=assayBarcode),
#                                 EXPERIMENT_PROTOCOL=
#                                 data.frame(name="",entityId ="",barcode=protocolBarcode))
#                                )
#  
#  newExptbarcode <-item$entity$barcode
#  
#  #add a container which creates an experiment sample
#  
#  update<-updateExperimentContainers(r$coreApi,containerBarcode, experimentType,                                                                     newExptbarcode,useVerbose = FALSE)
#  
#  

## ----eval = FALSE--------------------------------------------------------
#  
#  secondSample<-createExperimentSample(r$coreApi,experimentSampleType,sampleLot,newExptbarcode,useVerbose=FALSE)
#  

