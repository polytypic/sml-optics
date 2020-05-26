structure Optics :> OPTICS = struct
  datatype c = C of {exn: exn, stop: bool ref, over: bool}
  datatype ('s, 't) p = P of {inv: bool, map: c * 's option -> 't option}
  type ('s, 'f, 'g, 't) t = ('f, 'g) p -> ('s, 't) p

  exception Over
  exception Review
  exception View

  fun O map = P {inv = false, map = map}
  fun I map = P {inv = true, map = map}

  fun iso {fwd, bwd} (P {inv, map, ...}) =
      if inv
      then I (fn (c, _) => map (c, NONE) |> Option.map bwd)
      else O (fn (c, s) => map (c, Option.map fwd s) |> Option.map bwd)

  fun prism {decon, con} (P {inv, map, ...}) =
      if inv
      then I (fn (c, _) => map (c, NONE) |> Option.map con)
      else O (fn (c, s) => map (c, Option.chain decon s) |> Option.map con)

  fun lens {get, set} (P {map, ...}) =
      O (fn (c, SOME s) =>
            map (c, SOME (get s)) |> Option.map (fn g => set (g, s))
          | _ => NONE)

  fun over optic fg = let
    val P {map, ...} = O (#2 >> Option.map fg) |> optic
  in
    fn s =>
       case map (C {exn = Over, stop = ref false, over = true}, SOME s)
        of NONE => raise Over
         | SOME t => t
  end

  fun view optic = let
    exception Res of 'a option ref
    val P {map, ...} =
        O (fn (C {exn = Res res, stop, ...}, f) =>
              (res := f; stop := isSome f; NONE)
            | _ => NONE) |> optic
  in
    fn s => let
      val res = ref NONE
    in
      map (C {over = false, exn = Res res, stop = ref false}, SOME s) |> ignore
    ; case !res
       of NONE => raise View
        | SOME x => x
    end
  end

  fun review optic = let
    exception Rev of 'g
    val P {inv, map, ...} =
        P {inv = true,
           map = fn (C {exn = Rev g, ...}, _) => SOME g | _ => NONE} |> optic
  in
    if inv then
      fn g =>
         case map (C {exn = Rev g, stop = ref false, over = false}, NONE)
          of NONE => raise Review
           | SOME t => t
    else
      raise Review
  end

  fun fstL p = p |> lens {get = #1, set = fn (l, (_, r)) => (l, r)}
  fun sndL p = p |> lens {get = #2, set = fn (r, (l, _)) => (l, r)}

  fun someP p = p |> prism {con = SOME, decon = id}
  fun noneP p = p |> prism {
        con = fn () => NONE,
        decon = fn SOME _ => NONE | NONE => SOME ()
      }

  fun invertI optic = iso {fwd = review optic, bwd = view optic}

  val truncI = iso {fwd = real, bwd = trunc}

  fun listT (P {map, ...}) =
      O (fn (c as C {stop, over, ...}, xs) =>
            if over then let
              fun lp ys =
                  fn [] => SOME (List.rev ys)
                | x::xs =>
                  case map (c, SOME x)
                   of NONE => lp ys xs
                    | SOME y => lp (y::ys) xs
            in
              case xs
               of NONE => NONE
                | SOME xs => lp [] xs
            end else let
              val rec lp =
               fn [] => NONE
                | x::xs =>
                  (map (c, SOME x) |> ignore;
                   if !stop then NONE else lp xs)
            in
              case xs
               of NONE => NONE
                | SOME xs => lp xs
            end)
end
