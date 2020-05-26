signature OPTICS = sig
  type ('s, 't) p
  type ('s, 'f, 'g, 't) t = ('f, 'g) p -> ('s, 't) p

  exception Over
  exception Review
  exception View

  val iso: {fwd: 's -> 'f, bwd: 'g -> 't} -> ('s, 'f, 'g, 't) t
  val prism: {decon: 's -> 'f option, con: 'g -> 't} -> ('s, 'f, 'g, 't) t
  val lens: {get: 's -> 'f, set: 'g * 's -> 't} -> ('s, 'f, 'g, 't) t

  val over: ('s, 'f, 'g, 't) t -> ('f -> 'g) -> 's -> 't
  val view: ('s, 'f, 'g, 't) t -> 's -> 'f
  val review: ('s, 'f, 'g, 't) t -> 'g -> 't

  val invertI: ('s, 'f, 'g, 't) t -> ('g, 't, 's, 'f) t

  val truncI: (int, real, real, int) t

  val someP: ('f option, 'f, 'g, 'g option) t
  val noneP: ('f option, unit, unit, 'g option) t

  val fstL: ('l1 * 'r, 'l1, 'l2, 'l2 * 'r) t
  val sndL: ('l * 'r1, 'r1, 'r2, 'l * 'r2) t

  val listT: ('f list, 'f, 'g, 'g list) t
end
