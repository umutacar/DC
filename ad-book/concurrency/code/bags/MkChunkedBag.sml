functor MkChunkedBag (structure Chunk : CHUNK) :> BAG =
struct

  structure Chunk = Chunk
  structure Bag = Bag

  type 'a buffer = 'a Chunk.chunk
  type 'a bag = 'a buffer * 'a buffer * 'a buffer Bag.bag

  (* invariant:
    1. two buffers act like a stack whose top is the front of bf1,
      and whose bottom is the back of bf2.
    2. when the first buffer is nonempty, the second buffer must be full
    3. when the second buffer is empty, the first buffer must be empty
  *)

  exception EmptyBag
  exception IncompleteList
  exception SingletonTree

  val maxLeafSize = 8

  (** Utilities **)

  fun printBag toString (cb as (bf1, bf2, b)) =
    let
      val buf222 = Chunk.toString toString bf1
      val _ = print "1!!";
      val buf222 = Chunk.toString toString bf2
      val _ = print "2!!";
      val bufs = "Buffers:" ^ "\n" ^ (Chunk.toString toString bf1) ^ "\n" ^
        (Chunk.toString toString bf2) ^ "\n"
    in
      print ("ChunkedBag = \n" ^ bufs);
      Bag.printBag (Chunk.toString toString) b
    end

  fun printBagContents toString (cb as (bf1, bf2, b)) =
    let
      val bufs = "Buffer Contents:" ^ "\n" ^ (Chunk.contentToString toString
        bf1) ^ "\n" ^ (Chunk.contentToString toString bf2) ^ "\n"
    in
      print ("ChunkedBag Contents = \n" ^ bufs);
      Bag.printBagContents (Chunk.toString toString) b
    end

  fun printBagAsDecimal (cb as (bf1, bf2, b)) =
    print ("Decimal value: " ^ Int.toString ((Bag.bagToDecimal b)
    * maxLeafSize + (Chunk.size bf1) + (Chunk.size bf2)) ^ "\n")

  (** Mainline **)

  (* empty bag *)
  fun mkEmpty () =
    (Chunk.empty (), Chunk.empty (), Bag.mkEmpty ())

  fun size (cb as (bf1, bf2, b)) =
    (Chunk.size bf1) + (Chunk.size bf2) + (Bag.size b)

  (* insert element into a bag *)
  (* insert into the second buffer first, then the first one *)
  (* when the second one is full, call insertTree on the first buffer *)
  (* when the first one is als full, insert the second into bag
    and move the first buffer to the place of the second buffer *)
  fun insert (x, cb as (bf1, bf2, b)) =
    if (Chunk.isFull bf2)
    then
      if (Chunk.isFull bf1)
      then (Chunk.singleton x, bf1, Bag.insert (bf2, b))
      else (Chunk.push(x, bf1), bf2, b)
    else (* bf1 must be nil *)
      (bf1, Chunk.push(x, bf2), b)

  fun singleton x = insert (x, mkEmpty ())

  (* remove an element from a bag *)
  (* remove a full list from tree only when the both list is empty *)
  (* if the second list is empty, the first one must be *)
  fun remove (cb as (bf1, bf2, b)) =
    if (Chunk.isEmpty bf2)
    then
      let
        val (buf, b') = Bag.remove b
      in
        if Chunk.isEmpty buf then raise EmptyBag
        else let val (x, buf') = Chunk.pop buf
             in (x, (bf1, buf', b')) end
      end
    else
      if Chunk.isEmpty bf1
      then let val (y2, bf2') = Chunk.pop bf2
           in (y2, (bf1, bf2', b)) end
      else let val (y1, bf1') = Chunk.pop bf1
           in (y1, (bf1', bf2, b)) end

  (* wrapper for merge that return a chunked Bag *)
  fun insertTwoChunkToBag (bf1,bf2) b =
      let val (nbf1, nbf2) = Chunk.merge (bf1, bf2)
      in (nbf1, nbf2, b)
      end

  fun union (cb1 as (bfb1, bfb2, b), cb2 as (bfc1, bfc2, c)) =
    let
      val bc = Bag.union (b,c)
    in
      if Chunk.isEmpty bfc1
      then if Chunk.isEmpty bfb1
           then insertTwoChunkToBag (bfb2, bfc2) bc
           else insertTwoChunkToBag (bfb1, bfc2)
                    (Bag.insert (bfb2, bc))
      else if Chunk.isEmpty bfb1
           then insertTwoChunkToBag (bfc1, bfb2)
                    (Bag.insert (bfc2, bc))
          (* Both bfb2 and bfc2 must be full *)
           else insertTwoChunkToBag (bfb1, bfc1)
                (Bag.insert (bfb2, (Bag.insert (bfc2, bc))))
    end

   (* Note: the split is not perfectly balanced,
      when the size of inner bag is odd
      one bag will be greater than the other by maxChunkSize *)
   fun split (cb as (bf1, bf2, b)) =
     let
        val (nbf1, nbf1') = Chunk.split bf1
        val (nbf2, nbf2') = Chunk.split bf2
        val (nb, nb') = Bag.split b
      in
        ((insertTwoChunkToBag (nbf1, nbf2) nb),
         (insertTwoChunkToBag (nbf1', nbf2') nb'))
      end

    fun toList (elemToList : ('a -> 'a list)) (cb as (bf1, bf2, b)) =
      (Chunk.toList bf1) @ (Chunk.toList bf2) @ (Bag.toList Chunk.toList b)

end
