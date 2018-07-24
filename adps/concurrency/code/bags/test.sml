structure Test =
struct
  structure Bag = Bag
  structure CLBag = ChunkedListBag
  structure CABag = ChunkedArrayBag
  structure BagTest = MkBagTest(structure Bag = Bag)
  structure CLBagTest = MkBagTest(structure Bag = CLBag)
  structure CABagTest = MkBagTest(structure Bag = CABag)

  val testCase = 10

  (*
    use "BAG.sig";
    use "CHUNK.sig";
    use "bag.sml";
    use "MkChunkedBag.sml";
    use "ChunkedArrayBag.sml";
    use "ChunkedListBag.sml";
    use "MkBagTest.sml";
    use "test.sml";
  *)

  fun testInsertionSpeed n =
    let
      val () = BagTest.calcTimeAverage (BagTest.insNToList (0,n)) [] testCase
      val () = BagTest.calcTimeAverage (BagTest.insNToBag (0,n)) (Bag.mkEmpty ()) testCase
      val () = CLBagTest.calcTimeAverage (CLBagTest.insNToBag (0,n)) (CLBag.mkEmpty ()) testCase
      val () = CABagTest.calcTimeAverage (CABagTest.insNToBag (0,n)) (CABag.mkEmpty ()) testCase
    in
      ()
    end

  fun testUnionSingletonSpeed n =
    let
      (* create a singleton bag online before apply union *)
      val () = BagTest.calcTimeAverage (List.foldr (fn (a,b) => Bag.union (Bag.singleton a, b))
                (Bag.mkEmpty ())) (BagTest.insNToList (0,n) []) testCase
      val () = CLBagTest.calcTimeAverage (List.foldr (fn (a,b) => CLBag.union (CLBag.singleton a, b))
                (CLBag.mkEmpty ())) (BagTest.insNToList (0,n) []) testCase
      val () = CABagTest.calcTimeAverage (List.foldr (fn (a,b) => CABag.union (CABag.singleton a, b))
                (CABag.mkEmpty ())) (BagTest.insNToList (0,n) []) testCase
    in
      ()
    end

  fun testSplitTillSingletonSpeed n =
    let
      val () = BagTest.calcTimeSingle BagTest.splitTillSingleton
                (BagTest.insNToBag (0,n) (Bag.mkEmpty ()))
      val () = CLBagTest.calcTimeSingle CLBagTest.splitTillSingleton
                (CLBagTest.insNToBag (0,n) (CLBag.mkEmpty ()))
      val () = CABagTest.calcTimeSingle CABagTest.splitTillSingleton
                (CABagTest.insNToBag (0,n) (CABag.mkEmpty ()))
    in
      ()
    end


  fun testStackOrder n =
    let
      val () = BagTest.stackOrderTest n;
      val () = CLBagTest.stackOrderTest n;
      val () = CABagTest.stackOrderTest n;
    in
      ()
    end

  fun debugTest n =
    let
      val () = CABagTest.mergeAndSplitTest n;
    in
      ()
    end
end
