module type X = sig
  val x : int
end

module IncX (M : X) = struct
  let x = M.x + 1
end

module A = struct
  let x = 0
end

module B = IncX (A)
module C = IncX (B)

module AddX