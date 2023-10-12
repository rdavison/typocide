module App_id = struct
  let qualifier = "lol"
  let organization = "qwerkey"
  let application = "typocide"
end

module Directories = Directories.Project_dirs (App_id)
