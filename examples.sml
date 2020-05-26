local
  open Optics
in
val _ =
    print (Real.toString (Real.Math.sqrt (view (listT o sndL) [("a",0.5)])))
end
