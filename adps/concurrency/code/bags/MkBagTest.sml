functor MkBagTest (structure Bag : BAG) =
struct

  structure Bag = Bag
  open Bag

  fun insNToBag (i,n) b =
    if i < n then
      let
        (*val _ = print ("Inserting " ^ (Int.toString i) ^ "\n")*)
        val b' = insert (i, b)
        (*val _ = printBag Int.toString b' *)
      in
        insNToBag (i+1,n) b'
      end
    else
      b

  fun insNToList (i,n) l =
    if i < n then insNToList (i+1, n) (i::l) else l

  fun listCompare cmp l1 l2 =
    case (l1, l2) of
      (nil, nil) => true
    | (nil, _) => false
    | (_, nil) => false
    | (x::l1', y::l2') => (cmp (x,y)) andalso (listCompare cmp l1' l2')

  fun listToString elemToString l =
    let
      fun contentToString elemToString c =
        case c of
          [] => ""
        | a::c' => (elemToString a) ^ ","
                  ^ (contentToString elemToString c')
    in
      "[" ^ (contentToString elemToString l) ^ "]\n"
    end



  fun mergeAndSplitTest n =
    let
      val b = insNToBag (0,n) (mkEmpty ())
      val _ = print "** First bag:\n"
      val _ = printBagAsDecimal b
      (*val _ = printBagContents Int.toString b*)
      val _ = printBag Int.toString b

      val m = if (Int.mod (n,2)) = 0 then
                2*n
              else
                2*n + 1
      val c = insNToBag (n,m) (mkEmpty ())
      val _ = print "** Second bag:\n"
      val _ = printBagAsDecimal c
      (*val _ = printBagContents Int.toString c*)
      val _ = printBag Int.toString c

      val d = union (b,c)
      val _ = print "** Their union:\n"
      val _ = printBagAsDecimal d
      (*val _ = printBagContents Int.toString d*)
      val _ = printBag Int.toString d

      val (e,f) = split d
      val _ = print "** Their split:\n"
      val _ = print "First bag:\n"
      val _ = printBagAsDecimal e
      (*val _ = printBagContents Int.toString e*)
      val _ = printBag Int.toString e
      val _ = print "Second bag:\n"
      val _ = printBagAsDecimal f
      (*val _ = printBagContents Int.toString f*)
      val _ = printBag Int.toString f
    in
       ()
    end

  fun printTime n = print ((LargeInt.toString n) ^ "ms\n")

  fun calcTimeHelp f x =
    let
      val timeBefore = Time.toMilliseconds(Time.now ())
      val _ = f x
      val timeAfter = Time.toMilliseconds(Time.now ())
    in
      timeAfter - timeBefore
    end

  fun calcTimeSingle f x = printTime (calcTimeHelp f x)

  fun calcTimeTotalHelp f x n =
    let
      fun calcTimeTotalHelp' a res =
        if (a = 0) then res
        else calcTimeTotalHelp' (a-1) ((calcTimeHelp f x) + res)
    in
      calcTimeTotalHelp' n (LargeInt.fromInt 0)
    end

  fun calcTimeTotal f x n = printTime (calcTimeTotalHelp f x n)

  fun calcTimeAverage f x n =
    printTime (LargeInt.div((calcTimeTotalHelp f x n), LargeInt.fromInt n))

  fun remNFromBagTest n b l =
    if (n = 0) then (b,l,true)
    else
      let
        val (x, b') = remove b
        val x'::l' = l
        val true = (x = x')
      in
        remNFromBagTest (n-1) b' l'
      end

  fun stackOrderTest n =
    let
      val l = insNToList (0,n) nil
      val b1 = insNToBag (0,n) (mkEmpty ())
      val l1 = toList (fn x => [x]) b1
      (*val _ = print (listToString Int.toString l)*)
      (*val _ = print (listToString Int.toString l1)*)
      val true = listCompare (fn (a,b) => a = b) l l1
      val n2 = Int.div (n, 2)
      val (b2, l', true) = remNFromBagTest n2 b1 l
      val l'' = insNToList (n2, n) l'
      val b3 = insNToBag (n2, n) b2
      val (_, _, true) = remNFromBagTest n b3 l''
    in
      ()
    end

  fun splitTillSingleton b =
    if (size b) = 1 then ()
    else
      let
        val (b1, b2) = split b
        val _ = splitTillSingleton b1
        val _ = splitTillSingleton b2
      in
        ()
      end




end
