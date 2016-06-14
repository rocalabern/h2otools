# roxygen2::roxygenise()

x.global.assign <- function(var, val) {
  if (!exists(var, envir=globalenv(), inherits=FALSE)) {
    assign(var, val, envir=globalenv(), inherits=FALSE)
  } else {
    message(paste0("Variable ",var," already exist at global environment. It will not be created."))
  }
}

x.global.rm <- function(var) {
  if (!exists(var, envir=globalenv(), inherits=FALSE)) {
    message(paste0("Variable ",var," doest not exist at global environment. It will not be deleted."))
  } else {
    rm(list=var, envir=globalenv(), inherits=FALSE)
  }
}

#' @title h2o.remote.off
#' @export
h2o.remote.off <- function() {
  x.global.rm("paramsLocal")
  x.global.rm("paramsRemote")

  x.global.rm("h2oInit")
  x.global.rm("h2o.init")
  x.global.rm("h2o.importFile")
  x.global.rm("h2o.exportFile")
  x.global.rm("h2o.loadModel")
  x.global.rm("h2o.saveModel")
  x.global.rm("h2o.model.import")
  x.global.rm("h2o.model.export")
}

#' @title h2o.remote.on
#' @export
h2o.remote.on <- function(temporal_table_name=paste0("temporal_table_",Sys.getenv("USER"),".csv")) {

#   who = Sys.getenv("USER")
#   sufix = tail(unlist(strsplit(tempfile(pattern=""), "/")), n=1)

  x.global.assign("paramsRemote",
    list(
      server_remote_temporal_csv = paste0(connData$server_remote_h2o_folder,"tables/",temporal_table_name),
      server_remote_folder_model = paste0(connData$server_remote_h2o_folder,"models/"),
      server_remote_folder_data = paste0(connData$server_remote_h2o_folder,"tables/")
    )
  )

  x.global.assign("paramsLocal",
    list(
      server_local_user = connData$server_big_user,
      server_local_pass = connData$server_big_pass
    )
  )

  x.global.assign("h2oInit",
    function(
      ip = connData$server_remote_h2o_ip,
      h2oMEM = params$h2oMEM,
      h2oThreads = params$h2oThreads,
      h2oNAME = params$h2oNAME,
      h2oPORT = params$h2oPORT,
      h2oJAR = system.file("java", "h2o.jar", package = "h2o"),
      h2oOutput = "nohup.out",
      sleep = 10,
      ...
    ) {

      message(paste0("[H2O REMOTE] h2oInit"))

      h2otools::h2oInitRemote(
        server_remote_h2o_ip = ip,
        h2oMEM = h2oMEM,
        h2oThreads = h2oThreads,
        h2oNAME = h2oNAME,
        h2oPORT = h2oPORT,
        sleep = sleep,
        ...)
    }
  )

  x.global.assign("h2o.init",
    function(
      ip = connData$server_remote_h2o_ip,
      port = params$h2oPORT,
      startH2O = FALSE,
      Xmx = params$h2oMEM,
      nthreads = h2oThreads,
      h2oNAME = params$h2oNAME,
      h2oJAR = system.file("java", "h2o.jar", package = "h2o"),
      h2oOutput = "nohup.out",
      sleep = 10,
      ...
    ) {

      message(paste0("[H2O REMOTE] h2o.init"))

      if (startH2O) {
        h2otools::h2oInitRemote(
          server_remote_h2o_ip = ip,
          h2oMEM = as.numeric(Xmx),
          h2oThreads = as.numeric(nthreads),
          h2oNAME = h2oNAME,
          h2oPORT = as.numeric(port),
          sleep = sleep,
          ...)
      }

      localH2O = h2o::h2o.init(ip = ip, port = port, startH2O = FALSE)

      return(localH2O)
    }
  )

  x.global.assign("h2o.importFile",
    function (
      object,
      path,
      key = "",
      parse = TRUE,
      header,
      header_with_hash,
      sep = "",
      col.names,
      parser_type = "AUTO",
      strFileLocal = path,
      strFileRemote = paramsRemote$server_remote_temporal_csv,
      transferFile = TRUE,
      deleteRemoteFile = TRUE,
      ...
    ) {

      message(paste0("[H2O REMOTE] h2o.importFile"))
      message(paste0("[H2O REMOTE] local  : ",strFileLocal))
      message(paste0("[H2O REMOTE] remote : ",strFileRemote))

      if (transferFile) {
        message(paste0("[H2O REMOTE] Transfering to remote... (h2o.importFile)"))
        con = sshtools::ssh.shell()
        sshtools::ssh.open(con, connData$server_remote_h2o_ip, connData$server_remote_h2o_port, connData$server_remote_h2o_user, connData$server_remote_h2o_pass)
        sshtools::scpFromLocal(con, strFileLocal, strFileRemote)
        sshtools::ssh.close(con)
      }

      output = h2o::h2o.importFile(object, strFileRemote, key, parse, header, header_with_hash, sep, col.names, parser_type, ...)

      if (transferFile & deleteRemoteFile) {
        message(paste0("[H2O REMOTE] Deleting remote... (",strFileRemote,")"))
        con = sshtools::ssh.shell()
        sshtools::ssh.open(con, connData$server_remote_h2o_ip, connData$server_remote_h2o_port, connData$server_remote_h2o_user, connData$server_remote_h2o_pass)
        sshtools::ssh.sendCommand(con, paste0("rm ",strFileRemote), TRUE)
        sshtools::ssh.close(con)
      }

      return(output)
    }
  )

  x.global.assign("h2o.exportFile",
    function(
      data,
      path,
      force = FALSE,
      name = last(unlist(strsplit(path, "/"))),
      strFileLocal = path,
      strFileRemote =  paste0(paramsRemote$server_remote_folder_data, name),
      transferFile = TRUE,
      ...
    ) {

      message(paste0("[H2O REMOTE] h2o.exportFile"))
      message(paste0("[H2O REMOTE] local  : ",strFileLocal))
      message(paste0("[H2O REMOTE] remote : ",strFileRemote))

      output = h2o::h2o.exportFile(data, strFileRemote, force, ...)

      if (transferFile) {
        message(paste0("[H2O REMOTE] Transfering to local... (h2o.exportFile)"))
        con = sshtools::ssh.shell()
        sshtools::ssh.open(con, connData$server_remote_h2o_ip, connData$server_remote_h2o_port, connData$server_remote_h2o_user, connData$server_remote_h2o_pass)
        sshtools::scpToLocal(con, strFileRemote, strFileLocal)
        sshtools::ssh.close(con)
      }

      return(output)
    }
  )

  x.global.assign("h2o.loadModel",
    function (
      object,
      path = "",
      name = last(unlist(strsplit(path, "/"))),
      strFileLocal = path,
      strFileRemote =  paste0(paramsRemote$server_remote_folder_model, name),
      transferFile = FALSE,
      ...
    ) {

      message(paste0("[H2O REMOTE] h2o.loadModel"))
      message(paste0("[H2O REMOTE] local  : ",strFileLocal))
      message(paste0("[H2O REMOTE] remote : ",strFileRemote))

      output = h2o::h2o.loadModel(object, strFileRemote, ...)

      return(output)
    }
  )

  x.global.assign("h2o.saveModel",
    function (
      object,
      dir = "",
      name = "",
      save_cv = TRUE,
      force = FALSE,
      strFileLocal = paste0(dir, name),
      strFileRemote =  paste0(paramsRemote$server_remote_folder_model, name),
      transferFile = TRUE,
      ...
    ) {

      message(paste0("[H2O REMOTE] h2o.saveModel"))
      message(paste0("[H2O REMOTE] local  : ",strFileLocal))
      message(paste0("[H2O REMOTE] remote : ",strFileRemote))

      output = h2o::h2o.saveModel(object, dir=paramsRemote$server_remote_folder_model, name=name, save_cv=save_cv, force=force, ...)

      if (transferFile) {
        message(paste0("[H2O REMOTE] Transfering to local... (h2o.saveModel)"))
        con = sshtools::ssh.shell()
        sshtools::ssh.open(con, connData$server_remote_h2o_ip, connData$server_remote_h2o_port, connData$server_remote_h2o_user, connData$server_remote_h2o_pass)
        sshtools::scpToOther(con, strFileRemote, strFileLocal, paramsLocal$server_local_user, paramsLocal$server_local_pass, connData$server_big_ip, connData$server_big_port, params="-q -r")
        sshtools::ssh.flushwait(con)
        sshtools::ssh.close(con)
      }

      return(output)
    }
  )

  x.global.assign("h2o.model.import",
    function (
      object,
      model_name,
      folder_models = params$folder_models,
      name = model_name,
      path = paste0(folder_models, model_name),
      strFileLocal = path,
      strFileRemote =  paste0(paramsRemote$server_remote_folder_model, name),
      transferFile = FALSE,
      deleteRemoteFile = TRUE,
      ...
    ) {

      message(paste0("[H2O REMOTE] h2o.model.import"))
      message(paste0("[H2O REMOTE] local  : ",strFileLocal))
      message(paste0("[H2O REMOTE] remote : ",strFileRemote))

      if (transferFile) {
        message(paste0("[H2O REMOTE] Transfering to remote... (h2o.model.import)"))
        con = sshtools::ssh.shell()
        sshtools::ssh.open(con, connData$server_big_ip, connData$server_big_port, paramsLocal$server_local_user, paramsLocal$server_local_pass)
        sshtools::scpToOther(con, strFileLocal, strFileRemote, connData$server_remote_h2o_user, connData$server_remote_h2o_pass, connData$server_remote_h2o_ip, connData$server_remote_h2o_port, params="-q -r")
        sshtools::ssh.flushwait(con)
        sshtools::ssh.close(con)
      }

      output = h2o::h2o.loadModel(object, strFileRemote, ...)

      tryCatch({
        strVarImpFile = paste0(folder_models,"/",model_name,"/",model_name,"_varimp.csv")
        if (file.exists(strVarImpFile)) {
          message(paste0("[H2O] Loading Var.Imp. ..."))
          dfVarImp = read.table(strVarImpFile)
          output@model$varimp = dfVarImp
          message(paste0("[H2O] Var.Imp. loaded"))
        } else {
          message(paste0("[H2O] Var.Imp. not loaded"))
        }
      }, error = function(e) {
        error_message <- e$message
        warning(paste0("Warn: [H2O] Error loading var.imp. : ", error_message))
      })

      if (transferFile & deleteRemoteFile) {
        message(paste0("[H2O REMOTE] Deleting remote... (",strFileRemote,")"))
        con = sshtools::ssh.shell()
        sshtools::ssh.open(con, connData$server_big_ip, connData$server_big_port, paramsLocal$server_local_user, paramsLocal$server_local_pass)
        sshtools::ssh.sendCommand(con, paste0("rm ",strFileRemote), TRUE)
        sshtools::ssh.close(con)
      }

      return(output)
    }
  )

  x.global.assign("h2o.model.export",
    function(
      localH2O,
      model,
      model_name,
      folder_models = params$folder_models,
      force = TRUE,
      type="GBM",
      strFileLocal = paste0(folder_models, model_name),
      strFileRemote =  paste0(paramsRemote$server_remote_folder_model, model_name),
      transferFile = TRUE,
      ...
    ) {

      message(paste0("[H2O REMOTE] h2o.model.export"))
      message(paste0("[H2O REMOTE] local  : ",strFileLocal))
      message(paste0("[H2O REMOTE] remote : ",strFileRemote))

      output = h2o::h2o.saveModel(model, dir=paramsRemote$server_remote_folder_model, model_name, force=force, ...)

      if (transferFile) {
        message(paste0("[H2O REMOTE] Transfering to local... (h2o.model.export)"))
        con = sshtools::ssh.shell()
        sshtools::ssh.open(con, connData$server_remote_h2o_ip, connData$server_remote_h2o_port, connData$server_remote_h2o_user, connData$server_remote_h2o_pass)
        sshtools::scpToOther(con, strFileRemote, strFileLocal, paramsLocal$server_local_user, paramsLocal$server_local_pass, connData$server_big_ip, connData$server_big_port, params="-q -r")
        sshtools::ssh.flushwait(con)
        sshtools::ssh.close(con)
      }

      tryCatch({
        if (sum(names(model@model)=="varimp")==1) {
          message(paste0("[H2O] Saving Var.Imp. ..."))
          strVarImpFile = paste0(folder_models,"/",model_name,"/",model_name,"_varimp.csv")
          write.table(model@model$varimp, file=strVarImpFile)
          message(paste0("[H2O] Var.Imp. saved."))
        } else {
          warning(paste0("Warn: [H2O] not saving var.imp."))
        }
      }, error = function(e) {
        error_message <- e$message
        warning(paste0("Warn: [H2O] Error saving var.imp. : ", error_message))
      })

      tryCatch({
        message(paste0("[H2O] Saving POJO ..."))
        strJavaFile = paste0(folder_models,"/",model_name,"/",model_name,".java")
        modelPOJO = h2o.model.getPOJO(localH2O, model, type=type)
        cat(modelPOJO, file=strJavaFile)
        message(paste0("[H2O] POJO saved."))
      }, error = function(e) {
        error_message <- e$message
        warning(paste0("Warn: [H2O] Error saving POJO : ", error_message))
      })

      return(output)
    }
  )
}