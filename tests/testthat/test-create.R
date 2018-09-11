
context("Tests for create")

#setup to work with multiple environments

lapply(environments, function(x){
  con <- Connect(x)

     test_that(paste("test entity creation on: ", x),
            {
              #Get the metadata document
              md <- CoreAPIV2::updateMetadata(con$coreApi, useVerbose = verbose)

              out<-CoreAPIV2::getEntityMetadata(con$coreApi,POCO,useVerbose = FALSE)


              body<-out$template


              body[["TST_STRING"]] <- "ACME"

              body[["TST_INTEGER"]] <- 3

              body[["TST_BOOL"]] <- T
 
              body[["TST_FILE"]] <- NULL

              body[["TESTING@odata.bind"]] <- "/TESTPOCO('TP1')"
              
  

              return<-CoreAPIV2::createEntity(coreApi = con$coreApi,entityType = POCO,body=body,useVerbose = TRUE)

              barcode<-return$entity$Barcode


              b<-CoreAPIV2::getEntityByBarcode(con$coreApi,POCO,barcode,useVerbose=verbose)$entity

              expect_match(b$Barcode,barcode,all=verbose)

              expect_match(b$TST_STRING,"ACME",all=verbose)


          #test update attributes
          
              #you don't include file attribute names in updates

              updateValues<-list(TST_STRING = "My Lab",TST_BOOL = F) #,CI_FILE <- NULL, IMAGE_FILE  <- NULL)
              

              ue<-CoreAPIV2::updateEntityAttributes(con$coreApi,POCO,barcode,updateValues,useVerbose=FALSE)

              expect_match(ue$entity$TST_STRING,"My Lab",all=verbose)


        #test update associations    
              
              context <- "TESTING"
              
              updateValues<-list(TESTING = c("TESTPOCO", "TP2"))
              
              us<- CoreAPIV2::updateEntityAssociations(con$coreApi,POCO,barcode,updateValues,useVerbose=FALSE)
              

              
              as<-CoreAPIV2::getEntityAssociations(con$coreApi,POCO,barcode,context,fullMetadata = TRUE, useVerbose=FALSE)
              
              expect_match(as$entity[[1]]$Barcode,"TP2")
              
            #Change it back
              
              updateValues<-list(TESTING = c("TESTPOCO", "TP1"))
              
              us<- CoreAPIV2::updateEntityAssociations(con$coreApi,POCO,barcode,updateValues,useVerbose=FALSE)
              
              as<-CoreAPIV2::getEntityAssociations(con$coreApi,POCO,barcode,context,fullMetadata = TRUE, useVerbose=FALSE)
              
              expect_match(as$entity[[1]]$Barcode,"TP1")
              

               })
     CoreAPIV2::logOut(con$coreApi)

})

