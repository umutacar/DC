signature BAG =
sig

  type 'a bag

  val printBag : ('a -> string) -> 'a bag -> unit
  val printBagContents : ('a -> string) -> 'a bag -> unit
  val printBagAsDecimal : 'a bag -> unit

  val mkEmpty : unit -> 'a bag
  val singleton : 'a -> 'a bag
  val size : 'a bag -> int
  val insert : 'a * 'a bag -> 'a bag
  val remove : 'a bag -> 'a * 'a bag
  val union : 'a bag * 'a bag -> 'a bag
  val split : 'a bag -> 'a bag * 'a bag

  val toList: ('a -> 'a list) -> 'a bag -> 'a list

end
