open CamomileLibrary

module Literal = struct
  type 'a t = Null
              | Bool of bool
              | String of (UTF8.t * UTF8.t)
              | Number of (UTF8.t * float)
end
type 'a t = Ident of string
