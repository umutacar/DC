signature CHUNK =
sig

  type 'a t
  type 'a chunk = 'a t

  val maxChunkSize : int

  val size : 'a chunk -> int
  val empty : unit -> 'a chunk
  val isFull : 'a chunk -> bool
  val isEmpty : 'a chunk -> bool

  val singleton : 'a -> 'a chunk
  val push : 'a * 'a chunk -> 'a chunk
  val pop : 'a chunk -> 'a * 'a chunk
  (* Return two chunks, fill the second chunk till its full *)
  val merge : 'a chunk * 'a chunk -> 'a chunk * 'a chunk
  (* split the chunk into two chunks with the same size (off by 1) *)
  val split : 'a chunk -> 'a chunk * 'a chunk

  val toList : 'a chunk -> 'a list
  val toString : ('a -> string) -> 'a chunk -> string
  val contentToString : ('a -> string) -> 'a chunk -> string

end
