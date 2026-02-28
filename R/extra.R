#' Repozitorium manager function
#'
#' This experimental function allows manage Dataverse repozitorium and datasets.
#' @param scope get or put
#' @param data_table OBM project table
#' @param params list which contains parameters for repozitorium
#' @keywords repozitorium
#' @return data frame or boolean
#' @examples
#' \donttest{
#' # Getting server conf
#' results <- obm_repo('get', params=list(server_conf=1))
#' }
#'
#' @details
#' The following code *illustrates* how you could use this function in practice,
#' but it is **not meant to be run directly** (e.g., depends on external data).
#'
#' \preformatted{
#' # Set the default server/project-repo for each of the following operations
#' #   - default is 0
#' #   - get possible names from server_conf query above
#' obm_repo('set', params=list(REPO='ABC-REPO'))
#' obm_repo('set', params=list(REPO='ABC-REPO', PARENT='dataverse/parent'))
#'
#' # Listing dataverse
#' results <- obm_repo('get', params=list(type='dataverse', contents=1))
#' results <- obm_repo('get', params=list(type='dataverse'))
#'
#' # Getting content of the named dataverse
#' results <- obm_repo('get', params=list(id='MY-DATAVERSE'))
#'
#' # Get JSON Representation of a Dataset
#' results <- obm_repo('get', params=list(type='datasets', 
#'                                        persistentUrl='https://doi.org/xxx/xxx/xxx'))
#' results <- obm_repo('get', params=list(type='datasets', id='XABC-1'))
#'
#' # Get versions of dataset
#' results <- obm_repo('get',params=list(type='datasets', id=42, version=''))
#' results <- obm_repo('get',params=list(type='datasets', id=42, version=':draft'))
#'
#' # Get files of dataset
#' results <- obm_repo('get',params=list(type='datasets', id=42, files='', version=''))
#'
#' # Get a file
#' results<-obm_repo('get',params=list(type='datafile', id=83))
#' results<-obm_repo('get',params=list(type='datafile', id=83, version=':draft'))
#'
#' # Create a dataverse
#' results <- obm_repo('put',params=list(type='dataverse'))
#'
#' # Create a dateset
#' results <- obm_repo('put',params=list(type='datasets', dataverse='my_dataset'))
#'
#' # Add file to dataset (referenced by id or persistentUrl)
#' results <- obm_repo('put',params=list(type='datafile', 
#'                                       file='examples/test_upload.csv',
#'                                       id='XABC-1' ))
#' results <- obm_repo('put',params=list(type='datafile', 
#'                                       file='examples/test_upload.csv', 
#'                                       persistentUrl='https://doi.org/xxx/xxx/xxx'))
#'
#' # Add object as file to dataset (referenced by id or persistentUrl)
#' # - automatically convert data object to JSON
#' # - returning with the last file's state
#' init.df <- data.frame()
#' results <- obm_repo('put',params=list(type='datafile', 
#'                                       id='XABC-1', 
#'                     data=list(results=res.list,
#'                               init_params=init.df)))
#'
#' # Delete file
#' results <- obm_repo('delete',params=list(type='datafile',
#'                                          id='XABC-1',
#'                                          PARENT_DATAVERSE='dataverse/parent'))
#'
#' # Set settings
#' # obm_repo('set',params=list(type='dataset',id='XABC-1'))
#' }
#' @export
obm_repo <- function (scope=NULL,
                      data_table=NULL,
                      params=NULL) {

    token <- get_obm("token")
    pds_url   <- get_obm("pds_url")
    api_v <- get_obm("api_version", default = 3)
    token_time <- get_obm("time")
    
    if (is.null(token) || is.null(pds_url)) {
        warning("OBM session not initialized. Run obm_init() first.")
        return(invisible(FALSE))
    }

    if (identical(scope, '')) {
        warning("Usage: obm_repo(scope, params = ...)")
        return(FALSE)
    }

    # --- Token refresh ---
    if (!is.null(token_time) && !is.null(token$expires_in)) {
        expiry <- token_time + token$expires_in
        if (length(expiry) && expiry < unclass(Sys.time())) {
          try({
            obm_refresh_token()
            token <- get_obm("token")
          }, silent = TRUE)
        }
    }


    if (is.null(data_table)) data_table <- get_obm("project")

    if (!is.null(get_obm("default_repo"))) {
        params$REPO <- get_obm("default_repo")
    }
    if (!is.null(get_obm("parent_dataverse"))) {
        params$PARENT_DATAVERSE = get_obm("parent_dataverse")
    }

    # Upload/create processes
    if (scope == 'put') {

        data_file <- NULL

        # Upload files
        if (params$type == 'datafile') {
            tmp_dir <- NULL
            if (!is.null(params$file)) {
                data_file <- params$file     # vector of files
                params <- within(params, rm(file))
            } else {
                if (!is.null(params$data)) {
                    oldwd <- getwd()
                    on.exit(setwd(oldwd))
                    tmp_dir <- paste("obm_temp_dir-",randtext(1),sep='')
                    dir.create(tmp_dir)
                    setwd(tmp_dir)
                    if (!is.null(params$currentSession)) {
                        # Create files from ws-data
                        save.image(file='currentSession.RData')
                        data_file <- 'currentSession.RData'
                    }

                    for ( i in 1:length(params$data) ) {
                        x <- rjson::toJSON(params$data[[i]])
                        writeLines(x, paste('o_',names(params$data)[i],'.json',sep=''), useBytes=T)
                        data_file <- c(data_file, paste(tmp_dir,'/','o_',names(params$data)[i],'.json',sep=''))
                    }
                    setwd("..")
                } else {
                    warning('params$file or params$data should be given if you would like to upload something')
                }
            }

            # Data from local files / choose and upload files
            if (!is.null( data_file )) {

                # Check file exist in the dataset for automatic replacing
                k <- obm_repo('get',params=list(type='datasets',id=params$id))
                dataset_state <- k$data$versionState
                existing_files <- k$data$files[[1]]$label
                for (i in 1:length(existing_files)) {
                    if (existing_files[i] == basename(data_file)) {
                        if (dataset_state == 'DRAFT') {
                            res <- obm_repo('get',params=list(type='datafile',id=k$data$files[[1]]$dataFile$id[i]))
                        } else {
                            params$replace_file <- k$data$files[[1]]$dataFile$id[i]
                        }
                    }
                }

                params <- rjson::toJSON(params)
                # Add files to the datasets
                # upload file
                for ( i in data_file ) {
                    h <- httr::POST(pds_url,
                            body=list(
                                      access_token=token$access_token,
                                      scope='use_repo',
                                      params=params,
                                      method='put',
                                      data_files=httr::upload_file(i)
                                      ),
                            encode="multipart")

                    if (httr::status_code(h) != 200) {
                        warning(paste("http error:",httr::status_code(h),h ))
                        return(FALSE)
                    }

                    j <- httr::content(h, "parsed", "application/json")

                    if (j$status == "success") {
                        # Processing response....?
                        z <- j$data
                    } else {
                        warning( j )
                        return(FALSE)
                    }
                }
                # return last file data
                return( z )

            }
            if (!is.null(tmp_dir)) {
                unlink(tmp_dir, recursive=TRUE)
            }
        # Create a dataverse
        } else if (params$type == 'dataverse') {
            if (is.null(params$metadata)) {
                m <- list(
                        "Name"='',
                        "Alias"='',
                        "ContactEmail0"='',
                        "ContactEmail1"='',
                        "Affiliation"='',
                        "Description"='',
                        "DataverseType"='')

                message( "You must fill the follwing metadata attributes:" )
                message( "  ", rownames(as.data.frame(unlist(m))), "\n" )

                m$Name <- readline(prompt="Enter name: ")
                m$Alias <- readline(prompt="Enter alias name: ")
                m$ContactEmail0 <- readline(prompt="Enter contact's email-1: ")
                m$ContactEmail1 <- readline(prompt="Enter contact's email-2: ")
                m$Affiliation <- readline(prompt="Enter author's affiliation: ")
                m$Description <- readline(prompt="Enter description: ")

                s <- list('DEPARTMENT','JOURNALS','LABORATORY','ORGANIZATIONS_INSTITUTIONS','RESEARCHERS','RESEARCH_GROUP',
                          'RESEARCH_PROJECTS','TEACHING_COURSES','UNCATEGORIZED');

                message ( unlist(s) )
                m$DataverseType <- readline(prompt="Enter dataverse type: ")

                params$metadata <- m
	    }

            params <- rjson::toJSON(params)
            h <- httr::POST(pds_url,
                    body=list(
                              access_token=token$access_token,
                              scope='use_repo',
                              params=params,
                              method='put'),
                    encode="form")

            if (httr::status_code(h) != 200) {
                warning(paste("http error:",httr::status_code(h),h ))
                return(FALSE)
            }

            j <- httr::content(h, "parsed", "application/json")

            if (j$status == "success") {
                return ( j$data )

                # Processing response ?
                # response can be lightweighted by the request type, e.g.
                # request: dataset by id
                # response: resp <- obm_repo(...)
                # names(resp$data$files[[1]][1,])
                # [1] "description"      "label"            "restricted"       "version"
                # [5] "datasetVersionId" "categories"       "dataFile"
                # nrow(ki$data$files[[1]])
                # 21
            } else {
                warning ( j )
                return(FALSE)
            }


        # Create datasets
        } else if (params$type == 'datasets') {
            if (is.null(params$metadata)) {
                m <- list(
                        "Title"='',
                        "AuthorName"='',
                        "AuthorAffiliation"='',
                        "ContactName"='',
                        "ContactEmail"='',
                        "Description"='',
                        "Subject"='')

                message( "You must fill the follwing metadata attributes:" )
                message( "  ", rownames(as.data.frame(unlist(m))), "\n" )

                m$Title <- readline(prompt="Enter title: ")
                m$AuthorName <- readline(prompt="Enter author's name: ")
                m$AuthorAffiliation <- readline(prompt="Enter author's affiliation: ")
                m$ContactName <- readline(prompt="Enter contact's name: ")
                m$ContactEmail <- readline(prompt="Enter contact's email: ")
                m$Description <- readline(prompt="Enter description: ")

                #s <- list('Agricultural Sciences',
                #    'Arts and Humanities',
                #    'Astronomy and Astrophysics',
                #    'Business and Management',
                #    'Chemistry',
                #    'Computer and Information Science',
                #    'Earth and Environmental Sciences',
                #    'Engineering',
                #    'Law',
                #    'Mathematical Sciences',
                #    'Medicine, Health and Life Sciences',
                #    'Physics',
                #    'Social Sciences',
                #    'Other')
                # Shortened list
                s <- list('Agricultural Sciences',
                    'Chemistry',
                    'Computer and Information Science',
                    'Earth and Environmental Sciences',
                    'Mathematical Sciences',
                    'Medicine, Health and Life Sciences',
                    'Social Sciences',
                    'Other')


                message ( unlist(s) )
                read <- strsplit(readline(prompt="Enter subjects (e.g. 10 11): "), "\\s", perl=TRUE)
                m$Subject <- s[as.numeric(read[[1]])]
                params$metadata <- m
	    }

            params <- rjson::toJSON(params)
            h <- httr::POST(pds_url,
                    body=list(
                              access_token=token$access_token,
                              scope='use_repo',
                              params=params,
                              method='put'),
                    encode="form")

            if (httr::status_code(h) != 200) {
                warning(paste("http error:",httr::status_code(h),h ))
                return(FALSE)
            }

            j <- httr::content(h, "parsed", "application/json")

            if (j$status == "success") {
                return( j$data )

                # Processing response
                # response can be lightweighted by the request type, e.g.
                # request: dataset by id
                # response: resp <- obm_repo(...)
                # names(resp$data$files[[1]][1,])
                # [1] "description"      "label"            "restricted"       "version"
                # [5] "datasetVersionId" "categories"       "dataFile"
                # nrow(ki$data$files[[1]])
                # 21
            } else {
                warning ( j )
                return(FALSE)
            }
        }
    }
    else if (scope == 'get') {
        # Getting files/info

        p <- params
        params <- rjson::toJSON(params)

        h <- httr::POST(pds_url,
                body=list(access_token=token$access_token,
                          scope='use_repo',
                          params=params,
                          method='get'),
                encode="form")

        #message (h)

        j <- try(httr::content(h),silent=T)
        if (inherits(j, "try-error")) {
            j <- httr::content(h,"text")
        }
        if (inherits(j, 'raw')) {
            jk <- httr::headers(h)
            return(list(header=jk$`content-disposition`,data=j))
        } else {
            if (!is.null(p$type) && p$type == 'datafile') {
                jk <- httr::headers(h)
                return(list(header=jk$`content-disposition`,data=j))
            }
            j <- httr::content(h, "parsed", "application/json")
        }

        return(j)
        if (j$status == "success") {
            # metadata uploded, get PID
            #z <- jsonlite::fromJSON(j$data)
            # dataset
            # response can be lightweighted by the request type, e.g.
            # response$data$metadataBlocks$citation$fields[[1]]
            # Dataset title: response$data$metadataBlocks$citation$fields[[1]]$value[[1]]
            # Dataset author: response$data$metadataBlocks$citation$fields[[1]]$value[[2]]$authorName$value
            # Dataset author affiliation: response$data$metadataBlocks$citation$fields[[1]]$value[[2]]$authorAffiliation$value
            # Dataset files: response$data$files
            return( j$data )
        } else {
            warning ( j )
            return(FALSE)
        } }
    else if (scope == 'delete') {

        if (params$type == 'dataverse') {
            message('Are you sure you want to delete your dataverse?\nYou cannot undelete this dataverse. (Yes | No)')

            answer <- readline(prompt="\n")
            if (answer != 'Yes') {
                message("Cancelled\n")
                return()
            }
        }
        if (params$type == 'datasets') {
            message('Are you sure you want to delete this dataset and all of its files?\nYou cannot undelete this dataset. (Yes | No)')

            answer <- readline(prompt="\n")
            if (answer != 'Yes') {
                message("Cancelled\n")
                return()
            }
        }
        if (params$type == 'datafile') {
            message('The file will be deleted after you continue. (Yes | No)')

            answer <- readline(prompt="\n")
            if (answer != 'Yes') {
                message("Cancelled\n")
                return()
            }
        }
        params <- rjson::toJSON(params)

        h <- httr::POST(pds_url,
                body=list(access_token=token$access_token,
                          scope='use_repo',
                          params=params,
                          method='delete'),
                encode="form")

        j <- httr::content(h)

        return(j)
    }
    else if (scope == 'set') {

        if (!is.null(params$REPO)) {
            set_obm("default_repo", params$REPO)
        }
        if (!is.null(params$PARENT)) {
            set_obm("parent_dataverse", params$PARENT)
            params$PARENT_DATAVERSE = params$PARENT
        }

        api_call <- NULL
        if (!is.null(params$publish)) {
            message("Are you sure you want to publish this dataverse?\nOnce you do so it must remain published. (Yes | No)")
            answer <- readline(prompt="\n")
            if (answer != 'Yes') {
                message("Cancelled\n")
                return()
            }
            api_call <- 1
        }

        if (!is.null(api_call)) {
            params <- rjson::toJSON(params)

            h <- httr::POST(pds_url,
                    body=list(access_token=token$access_token,
                              scope='use_repo',
                              params=params,
                              method='set'),
                    encode="form")

            j <- httr::content(h)

            return(j)
        }
    }
}


#' Background Computation Function
#'
#' This experimental function allows to manage background computation on an OpenBioMaps server.
#' @param action post, get-results, get-status
#' @param data_files list of files to sending
#' @param params computation control params
#' @param config_file is The computation_conf.yml file
#' @keywords computation
#' @return data frame or boolean
#' @examples
#' \donttest{
#' # Manage computational packs
#' results <- obm_computation(action='post',
#'                            data_files='examples/scripts/bombina.R',
#'                            params=list(package = 'my_project'),
#'                            config_file='examples/computation_conf.yml')
#' }
#' @export
obm_computation <- function (action='',
                             data_files=NULL,
                             params=NULL,
                             config_file=NULL) {

    token <- get_obm("token")
    url   <- get_obm("pds_url")
    api_v <- get_obm("api_version", default = 3)
    token_time <- get_obm("time")
    
    if (is.null(token) || is.null(url)) {
        warning("OBM session not initialized. Run obm_init() first.")
        return(invisible(FALSE))
    }

    if (identical(action, '')) {
        warning("Usage: obm_computation(action, params = ...)")
        return(FALSE)
    }

    # --- Token refresh ---
    if (!is.null(token_time) && !is.null(token$expires_in)) {
        expiry <- token_time + token$expires_in
        if (length(expiry) && expiry < unclass(Sys.time())) {
          try({
            obm_refresh_token()
            token <- get_obm("token")
          }, silent = TRUE)
        }
    }
    if (!is.null(api_v) && api_v >= 3) {
        stop("obm_computation is not supported in API v3")
    }

    params <- rjson::toJSON(params)

    if (action == 'post') {

        if (dir.exists(data_files)) {
            files2zip <- dir(data_files, full.names = TRUE)
            utils::zip(zipfile = 'scripts.zip', files = files2zip)
            data_files <- 'scripts.zip'
        }

        if (typeof(data_files)=='list') {
            #for ( i in data_files ) {
            #}
            utils::zip("scripts.zip", data_files)
            data_files <- 'scripts.zip'
        }

            h <- httr::POST(url,
                    body=list(access_token=token$access_token,
                              scope='computation',
                              params=params,
                              method='post', 
                              data_files=httr::upload_file(data_files),
                              config_file=httr::upload_file(config_file)),
                    encode="multipart")

            if (httr::status_code(h) != 200) {
                warning(paste("http error:",httr::status_code(h),h ))
                return(FALSE)
            }

            j <- httr::content(h, "parsed", "application/json")

            if (j$status == "success") {
                # Response array:
                # Computation ID, Remote server, ...
                z <- j$data
            } else {
                return( j )
            }
    } else if (action == 'get-status') {

        h <- httr::POST(url,
                        body=list(access_token=token$access_token,
                                  scope='computation',
                                  method='get-status',
                                  params=params),
                        encode='form')
        if (httr::status_code(h) != 200) {
            if (httr::status_code(h) == 403) {
                warning( "Resource access denied" )
                return(FALSE)
            }
            else if (httr::status_code(h) == 500) {
                warning( "Server error" )
                return(FALSE)
            }
        } else {
            j <- httr::content(h, "parsed", "application/json")

            if (j$status == "success") {
                # Response array:
                # Computation ID, Remote server, ...
                z <- j$data
            } else {
                return( j )
            }
        }
    } else if (action == 'get-results') {

        h <- httr::POST(url,
                        body=list(access_token=token$access_token,
                                  scope='computation',
                                  method='get-results',
                                  params=params),
                        encode='form')
        if (httr::status_code(h) != 200) {
            if (httr::status_code(h) == 403) {
                warning( "Resource access denied" )
                return(FALSE)
            }
            else if (httr::status_code(h) == 500) {
                warning( "Server error" )
                return(FALSE)
            }
        } else {
            j <- httr::content(h, "parsed", "application/json")

            if (j$status == "success") {
                # Response array:
                # Computation ID, Remote server, ...
                z <- j$data
            } else {
                return( j )
            }
        }
    } else {
        warning( "Action should be either: post, get-results or get-status")
        return(FALSE)
    }
}
