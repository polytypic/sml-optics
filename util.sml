fun id x = x

infix |>
fun x |> f = f x

infix >>
fun (f >> g) x = x |> f |> g

structure Option = struct
  open Option
  fun chain xyO = fn SOME x => xyO x | NONE => NONE
end
