structure ArrayChunk :> CHUNK =
struct

  (* the last int is the number of not NONE elems in the array *)
  type 'a t = ('a option) array * int
  type 'a chunk = 'a t

  exception EmptyChunk
  exception InCompleteChunk
  exception FullChunk

  val maxChunkSize = 1024

  fun size (c as (arr, n)) = n
  fun empty () = (Array.array (maxChunkSize, NONE), 0)
  fun isFull c = (size c = maxChunkSize)
  fun isEmpty c = (size c = 0)


  fun push (x, c as (arr, n)) =
    if isFull c then raise FullChunk
    else
      let val () = Array.update (arr, n, SOME(x))
      in (arr, n+1) end

  fun singleton x = push (x, empty ())

  fun pop (c as (arr, n)) =
    if isEmpty c then raise EmptyChunk
    else
      let
        val x = Option.valOf (Array.sub (arr, n-1))
        val () = Array.update (arr, n-1, NONE)
      in
        (x, (arr, n-1))
      end

  fun merge (c1, c2) =
    let
      fun merge' (c1', c2') =
        if ((isFull c2') orelse (isEmpty c1')) then (c1', c2')
        else
           let
             val (x, c1'') = pop c1'
             val c2'' = push (x, c2')
           in
             merge' (c1'', c2'')
           end
    in
      if ((size c1) > (size c2)) then merge' (c2, c1) else merge' (c1, c2)
    end

  fun split (c as (arr, n)) =
    let
      fun split' a (c1, c2) =
        if (a = 0) then (c1, c2)
        else
          let
            val (x, c2') = pop c2
            val c1' = push (x, c1)
          in
            split' (a-1) (c1', c2')
          end
    in
      split' (Int.div (n, 2)) (empty (), c)
    end

  (* in order to simulate a stack, we print the elems in reverse order *)
  fun contentToString elemToString (c as (arr, n)) =
    let
      fun optionToString x =
        case x of NONE => "_" | SOME(y) => elemToString y
      fun contentToString' a =
        if a = 0 then ""
        else (optionToString (Array.sub(arr, a-1))) ^ ","
            ^ contentToString' (a-1)
    in
      contentToString' maxChunkSize
    end

  fun toString elemToString c =
    "<" ^ (contentToString elemToString c) ^ ">"

  fun toList (c as (arr, n)) =
    let
      fun toList' a l =
        if (a = n) then l
        else toList' (a+1) (Option.valOf(Array.sub (arr, a))::l)
    in
      toList' 0 []
    end

end

structure ChunkedArrayBag = MkChunkedBag(structure Chunk = ArrayChunk)
