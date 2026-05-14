#' Originally CytoML's gatingset_to_flowjo function. It depends on a Docker image. 
#' The docker image in turn depends on cytolib package. The Docker image for 2.2 is broken, 
#' failing at .wsp parse gate level. Not suprising given cytolib is now at 2.20.
#' The Amazon image for 2.3.2 is still working for now apparently. 
#' 
#' @param gs The GatingSet object
#' @param outfile The outpath-filename.wsp location to save to
#' @param showHidden Default FALSE
#' 
#' @return A FlowJo.wsp of GatingSet gates
#' 
#' @export
#'
GatingSet_to_FlowJo <- function(gs, outFile, showHidden = FALSE,
  docker_img = "9ac7bef98184"){

  # Ensure either the executable binary or the docker image are present
  res <- check_binary_status()
  if (res!="binary_ok"){res <- check_docker_status(docker_img)}
  if (!(res[1] %in% c("binary_ok", "docker_ok"))){stop(res)}
    
  # Save the GatingSet or pass it's path to tmp
  if(is(gs, "GatingSet")){
    tmp <- tempfile()
    suppressMessages(save_gs(gs, tmp))
  } else {tmp <- gs}
  
  # Handle spaces in file path by expanding to absolute path and escaping spaces
  outFile <- gsub(" ", "\\\\ ", file.path(normalizePath(dirname(outFile)), basename(outFile)))

  # Create a temp file for saving to
  tmpfile <- tempfile()
  
  if(!res[1]=="binary_ok"){
    docker_img <- res[2] # The validated image name from check_docker_status()
    v1 <- packageVersion("cytolib")
    v2 <- system2("docker", paste0("run ", docker_img, " --cytolib-version"), stdout = TRUE)
    
    if (v1!=v2){
      warning("docker image '", docker_img, "' is built with different cytolib version of from R package: ", v2, " vs ", v1)
    }
    
    message(paste0("Using docker image ", docker_img, " to write FlowJo workspace..."))

    res <- suppressWarnings(system2("docker"
                                    , paste0("run"
                                             , " -v ", tmp, ":/gs"
                                             , " -v ", normalizePath(dirname(tmpfile)), ":/out "
                                             , docker_img
                                             , " --src=/gs --dest=/out/", basename(tmpfile)
                                             , " --showHidden=", showHidden)
                                    , stderr = TRUE)
    )
  }

  if(length(res) > 0)
    stop(res)
  else {
     file.copy(tmpfile, outFile)
  }
  
}

check_docker_status <- function(docker_img = NULL){
  if(Sys.info()["sysname"] == "Windows"){
    errcode <- system2("WHERE", "docker", stdout = FALSE)
  } else {
    errcode <- system2("command", " -v docker", stdout = FALSE)
  }
  
  if(errcode!=0){return("'docker' command is not found! ")}
    
  errcode <- system2("docker", " info", stdout = FALSE, stderr = FALSE)

  if(errcode!=0){return("'docker' is not running properly! ")}
   
  
  # Determine proper default image for this cytolib version
  if(is.null(docker_img)){
    docker_img <- "public.ecr.aws/x4k5d9i7/cytoverse/gs-to-wsp:latest"
    
    # # strip last patch numbers
    # cytolib_minor_version <- gsub("\\.[^.]*$", "", packageVersion("cytolib"))
    # 
    # # First try to match cytolib minor version directly to image name
    # docker_version_match <- system2("docker", paste0("  image inspect ", paste0(base_img, ":", cytolib_minor_version)), stdout = FALSE, stderr = FALSE)
    # if(docker_version_match==0){
    #   docker_img <- paste0(base_img, ":", cytolib_minor_version)
    # }else{
    # # Otherwise, check for devel tagged image and see if that minor version matches
    #   devel_img <- paste0(base_img, ":devel")
    #   if(system2("docker", paste0("  image inspect ", devel_img), stdout = FALSE, stderr = FALSE) == 0){
    #     devel_img_version <- system2("docker", paste0("run ", devel_img, " --cytolib-version"), stdout = TRUE)
    #     if(gsub("\\.[^.]*$", "", devel_img_version) == cytolib_minor_version)
    #       docker_img <- devel_img
    #   }
    # }
  }
  
  if(is.null(docker_img)){
    # Search for a valid default image failed
    return(paste0("No default docker image found\n",
                  "Please see help(gatingset_to_flowjo) about pulling the appropriate docker image."))
  }else{
    # Try candidate image
    errcode <- system2("docker", paste0("  image inspect ", docker_img), stdout = FALSE, stderr = FALSE)
    if(errcode!=0)
      return(paste0("docker image '", docker_img, "' is not present! "))
  }
  
  return(c("docker_ok", docker_img))
}

check_binary_status <- function(){
  if(Sys.info()["sysname"] == "Windows"){
    errcode <- suppressWarnings(system2("WHERE", "gs-to-flowjo", stdout = FALSE))
  } else {
    errcode <- suppressWarnings(system2("command", " -v gs-to-flowjo", stdout = FALSE))}
  if(errcode!=0){return(paste0("gs-to-flowjo binary is not present"))}
  return("binary_ok")
}
