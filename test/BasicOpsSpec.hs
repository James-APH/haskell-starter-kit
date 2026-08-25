module BasicOpsSpec where

import OpsAndAlgorithms.BasicOps
import Test.Hspec

spec :: Spec
spec = do
    describe "head_" $ do
        it "returns the first element" $
            head_ ([1, 2, 3] :: [Integer]) `shouldBe` Just 1

        it "returns Nothing if the list is emtpy" $
            head_ ([] :: [Integer]) `shouldBe` Nothing

    describe "tail_" $ do
        it "returns the remaining elements in a list" $
            tail_ ([1, 2, 3] :: [Integer]) `shouldBe` Just [2, 3]

        it "returns Nothing if the list is empty" $
            tail_ ([] :: [Integer]) `shouldBe` Nothing

    describe "null_" $ do
        it "returns True for an empty list" $
            null_ ([] :: [Integer]) `shouldBe` True

        it "returns False for a non-empty list" $
            null_ ([1, 2, 3] :: [Integer]) `shouldBe` False

    describe "not_" $ do
        it "negates True" $
            not_ True `shouldBe` False

        it "negates False" $
            not_ False `shouldBe` True

    describe "isEven_" $ do
        it "returns True for even numbers" $
            isEven_ (4 :: Integer) `shouldBe` True

        it "returns False for odd numbers" $
            isEven_ (5 :: Integer) `shouldBe` False

        it "handles zero" $
            isEven_ (0 :: Integer) `shouldBe` True

        it "handles negative numbers" $
            isEven_ ((-4) :: Integer) `shouldBe` True

    describe "isOdd_" $ do
        it "returns True for odd numbers" $
            isOdd_ (5 :: Integer) `shouldBe` True

        it "returns False for even numbers" $
            isOdd_ (4 :: Integer) `shouldBe` False

        it "handles zero" $
            isOdd_ (0 :: Integer) `shouldBe` False

        it "handles negative numbers" $
            isOdd_ ((-5) :: Integer) `shouldBe` True

    describe "length_" $ do
        it "returns 0 for an empty list" $
            length_ ([] :: [Integer]) `shouldBe` 0

        it "returns the number of elements" $
            length_ ([1, 2, 3, 4] :: [Integer]) `shouldBe` 4

        it "works with a singleton list" $
            length_ ([42] :: [Integer]) `shouldBe` 1

    describe "last_" $ do
        it "returns the last element" $
            last_ ([1, 2, 3] :: [Integer]) `shouldBe` Just 3

        it "works with a singleton list" $
            last_ ([42] :: [Integer]) `shouldBe` Just 42

        it "returns Nothing for an empty list" $
            last_ ([] :: [Integer]) `shouldBe` Nothing

    describe "reverse_" $ do
        it "reverses a list" $
            reverse_ ([1, 2, 3, 4] :: [Integer]) `shouldBe` [4, 3, 2, 1]

        it "returns an empty list for an empty list" $
            reverse_ ([] :: [Integer]) `shouldBe` []

        it "does not change a singleton list" $
            reverse_ ([42] :: [Integer]) `shouldBe` [42]

    describe "max_" $ do
        it "returns the larger of two values" $
            max_ (5 :: Integer) 10 `shouldBe` 10

        it "returns the first value when it is larger" $
            max_ (10 :: Integer) 5 `shouldBe` 10

        it "handles equal values" $
            max_ (5 :: Integer) 5 `shouldBe` 5

        it "handles negative values" $
            max_ ((-5) :: Integer) (-10) `shouldBe` (-5)

    describe "min_" $ do
        it "returns the smaller of two values" $
            min_ (5 :: Integer) 10 `shouldBe` 5

        it "returns the second value when it is smaller" $
            min_ (10 :: Integer) 5 `shouldBe` 5

        it "handles equal values" $
            min_ (5 :: Integer) 5 `shouldBe` 5

        it "handles negative values" $
            min_ ((-5) :: Integer) (-10) `shouldBe` (-10)

    describe "maxElement_" $ do
        it "returns the maximum element" $
            maxElement_ ([3, 7, 2, 9, 4] :: [Integer]) `shouldBe` Just 9

        it "works with a singleton list" $
            maxElement_ ([42] :: [Integer]) `shouldBe` Just 42

        it "returns Nothing for an empty list" $
            maxElement_ ([] :: [Integer]) `shouldBe` Nothing

        it "handles negative numbers" $
            maxElement_ ([-5, -2, -10] :: [Integer]) `shouldBe` Just (-2)

    describe "minElement_" $ do
        it "returns the minimum element" $
            minElement_ ([3, 7, 2, 9, 4] :: [Integer]) `shouldBe` Just 2

        it "works with a singleton list" $
            minElement_ ([42] :: [Integer]) `shouldBe` Just 42

        it "returns Nothing for an empty list" $
            minElement_ ([] :: [Integer]) `shouldBe` Nothing

        it "handles negative numbers" $
            minElement_ ([-5, -2, -10] :: [Integer]) `shouldBe` Just (-10)

    describe "minmax_" $ do
        it "returns the minimum and maximum" $
            minmax_ (42 :: Integer) 39 `shouldBe` (39, 42)

        it "works when numbers are equal" $
            minmax_ (42 :: Integer) 42 `shouldBe` (42, 42)

        it "handles negative numbers" $
            minmax_ ((-1) :: Integer) (-10) `shouldBe` (-10, -1)

    describe "minmaxElement_" $ do
        it "returns the minimum and maximum" $
            minmaxElement_ ([3, 7, 2, 9, 4] :: [Integer]) `shouldBe` Just (2, 9)

        it "works with a singleton list" $
            minmaxElement_ ([42] :: [Integer]) `shouldBe` Just (42, 42)

        it "returns Nothing for an empty list" $
            minmaxElement_ ([] :: [Integer]) `shouldBe` Nothing

        it "handles negative numbers" $
            minmaxElement_ ([-5, -2, -10] :: [Integer]) `shouldBe` Just (-10, -2)

    describe "contains_" $ do
        it "returns True when an element exists" $
            contains_ (3 :: Integer) ([1, 2, 3, 4] :: [Integer]) `shouldBe` True

        it "returns False when an element does not exist" $
            contains_ (5 :: Integer) ([1, 2, 3, 4] :: [Integer]) `shouldBe` False

        it "returns False for an empty list" $
            contains_ (1 :: Integer) ([] :: [Integer]) `shouldBe` False

    describe "count_" $ do
        it "counts occurrences of an element" $
            count_ (2 :: Integer) ([1, 2, 2, 3, 2] :: [Integer]) `shouldBe` 3

        it "returns zero when the element is absent" $
            count_ (5 :: Integer) ([1, 2, 3] :: [Integer]) `shouldBe` 0

        it "returns zero for an empty list" $
            count_ (1 :: Integer) ([] :: [Integer]) `shouldBe` 0

    describe "countIf_" $ do
        it "counts elements satisfying a predicate" $
            countIf_ isEven_ ([1, 2, 3, 4, 5, 6] :: [Integer]) `shouldBe` 3

        it "returns zero when nothing matches" $
            countIf_ isEven_ ([1, 3, 5] :: [Integer]) `shouldBe` 0

        it "returns zero for an empty list" $
            countIf_ isEven_ ([] :: [Integer]) `shouldBe` 0

    describe "filter_" $ do
        it "keeps elements satisfying a predicate" $
            filter_ isEven_ ([1, 2, 3, 4, 5, 6] :: [Integer]) `shouldBe` [2, 4, 6]

        it "returns an empty list when nothing matches" $
            filter_ isEven_ ([1, 3, 5] :: [Integer]) `shouldBe` []

        it "returns an empty list for an empty input" $
            filter_ isEven_ ([] :: [Integer]) `shouldBe` []

    describe "anyOf_" $ do
        it "returns True when any element satisfies the predicate" $
            anyOf_ isEven_ ([1, 3, 4, 5] :: [Integer]) `shouldBe` True

        it "returns False when no element satisfies the predicate" $
            anyOf_ isEven_ ([1, 3, 5] :: [Integer]) `shouldBe` False

        it "returns False for an empty list" $
            anyOf_ isEven_ ([] :: [Integer]) `shouldBe` False

    describe "allOf_" $ do
        it "returns True when all elements satisfy the predicate" $
            allOf_ isEven_ ([2, 4, 6] :: [Integer]) `shouldBe` True

        it "returns False when at least one element fails" $
            allOf_ isEven_ ([2, 4, 5, 6] :: [Integer]) `shouldBe` False

        it "returns True for an empty list" $
            allOf_ isEven_ ([] :: [Integer]) `shouldBe` True

    describe "noneOf_" $ do
        it "returns True when no elements satisfy the predicate" $
            noneOf_ isEven_ ([1, 3, 5] :: [Integer]) `shouldBe` True

        it "returns False when an element satisfies the predicate" $
            noneOf_ isEven_ ([1, 2, 3] :: [Integer]) `shouldBe` False

        it "returns True for an empty list" $
            noneOf_ isEven_ ([] :: [Integer]) `shouldBe` True

    describe "substr_" $ do
        it "returns a substring" $
            substr_ "hello" 1 3 `shouldBe` Just "el"

        it "can take a substring from the beginning" $
            substr_ "hello" 0 2 `shouldBe` Just "he"

        it "can take a substring from the end" $
            substr_ "hello" 3 5 `shouldBe` Just "lo"

        it "returns an empty string when string provided is empty" $
            substr_ "" 2 4 `shouldBe` Just ""

        it "returns Nothing when end index in smaller than beginning" $
            substr_ "hello" 2 0 `shouldBe` Nothing

        it "returns Nothing when beginning index is greater than string length" $
            substr_ "hello" 7 0 `shouldBe` Nothing

        it "returns Nothing when beginning index is less than zero" $
            substr_ "hello" (-1) 1 `shouldBe` Nothing

        it "returns Nothing when end index is less than zero" $
            substr_ "hello" 1 (-1) `shouldBe` Nothing

    describe "take_" $ do
        it "takes the first n elements" $
            take_ (3 :: Integer) ([1, 2, 3, 4, 5] :: [Integer]) `shouldBe` [1, 2, 3]

        it "takes the entire list when n is large enough" $
            take_ (10 :: Integer) ([1, 2, 3] :: [Integer]) `shouldBe` [1, 2, 3]

        it "returns an empty list when n is zero" $
            take_ (0 :: Integer) ([1, 2, 3] :: [Integer]) `shouldBe` []

        it "returns an empty list from an empty list" $
            take_ (3 :: Integer) ([] :: [Integer]) `shouldBe` []

    describe "drop_" $ do
        it "drops the first n elements" $
            drop_ (2 :: Integer) ([1, 2, 3, 4, 5] :: [Integer]) `shouldBe` [3, 4, 5]

        it "returns an empty list when n is large enough" $
            drop_ (10 :: Integer) ([1, 2, 3] :: [Integer]) `shouldBe` []

        it "returns the original list when n is zero" $
            drop_ (0 :: Integer) ([1, 2, 3] :: [Integer]) `shouldBe` [1, 2, 3]

        it "works with an empty list" $
            drop_ (3 :: Integer) ([] :: [Integer]) `shouldBe` []

    describe "takeWhile_" $ do
        it "takes elements while the predicate is true" $
            takeWhile_ isEven_ ([2, 4, 6, 7, 8] :: [Integer]) `shouldBe` [2, 4, 6]

        it "returns an empty list when the first element fails" $
            takeWhile_ isEven_ ([1, 2, 4] :: [Integer]) `shouldBe` []

        it "takes the entire list when all elements match" $
            takeWhile_ isEven_ ([2, 4, 6] :: [Integer]) `shouldBe` [2, 4, 6]

        it "works with an empty list" $
            takeWhile_ isEven_ ([] :: [Integer]) `shouldBe` []

    describe "dropWhile_" $ do
        it "drops elements while the predicate is true" $
            dropWhile_ isEven_ ([2, 4, 6, 7, 8] :: [Integer]) `shouldBe` [7, 8]

        it "returns the original list when the first element fails" $
            dropWhile_ isEven_ ([1, 2, 4] :: [Integer]) `shouldBe` [1, 2, 4]

        it "returns an empty list when all elements match" $
            dropWhile_ isEven_ ([2, 4, 6] :: [Integer]) `shouldBe` []

        it "works with an empty list" $
            dropWhile_ isEven_ ([] :: [Integer]) `shouldBe` []

    describe "map_" $ do
        it "applies a function to every element" $
            map_ (* 2) ([1, 2, 3] :: [Integer]) `shouldBe` [2, 4, 6]

        it "works with an empty list" $
            map_ (* 2) ([] :: [Integer]) `shouldBe` []

        it "can change the element type" $
            map_ show ([1, 2, 3] :: [Integer]) `shouldBe` ["1", "2", "3"]

    describe "sum_" $ do
        it "sums all elements" $
            sum_ ([1, 2, 3, 4] :: [Integer]) `shouldBe` 10

        it "returns zero for an empty list" $
            sum_ ([] :: [Integer]) `shouldBe` 0

        it "handles negative numbers" $
            sum_ ([-1, -2, 3] :: [Integer]) `shouldBe` 0

    describe "product_" $ do
        it "multiplies all elements" $
            product_ ([1, 2, 3, 4] :: [Integer]) `shouldBe` 24

        it "returns one for an empty list" $
            product_ ([] :: [Integer]) `shouldBe` 1

        it "returns zero when an element is zero" $
            product_ ([1, 2, 0, 4] :: [Integer]) `shouldBe` 0

    describe "zip_" $ do
        it "combines two lists element by element" $
            zip_ ([1, 2, 3] :: [Integer]) ['a', 'b', 'c']
                `shouldBe` [(1, 'a'), (2, 'b'), (3, 'c')]

        it "stops at the shorter list" $
            zip_ ([1, 2, 3] :: [Integer]) ['a', 'b']
                `shouldBe` [(1, 'a'), (2, 'b')]

        it "works with an empty first list" $
            zip_ ([] :: [Integer]) [1, 2, 3] `shouldBe` []

        it "works with an empty second list" $
            zip_ ([1, 2, 3] :: [Integer]) ([] :: [Char]) `shouldBe` []

    describe "zipWith_" $ do
        it "combines elements using a function" $
            zipWith_ (+) ([1, 2, 3] :: [Integer]) [4, 5, 6]
                `shouldBe` [5, 7, 9]

        it "stops at the shorter list" $
            zipWith_ (+) ([1, 2, 3] :: [Integer]) [4, 5]
                `shouldBe` [5, 7]

        it "works with an empty list" $
            zipWith_ (+) ([] :: [Integer]) [1, 2, 3]
                `shouldBe` []

    describe "replace_" $ do
        it "replaces matching elements" $
            replace_ (2 :: Integer) (9 :: Integer) ([1, 2, 3, 2] :: [Integer])
                `shouldBe` [1, 9, 3, 9]

        it "leaves non-matching elements unchanged" $
            replace_ (5 :: Integer) (9 :: Integer) ([1, 2, 3] :: [Integer])
                `shouldBe` [1, 2, 3]

        it "works with an empty list" $
            replace_ (1 :: Integer) (2 :: Integer) ([] :: [Integer]) `shouldBe` []

    describe "foldl_" $ do
        it "folds from the left" $
            foldl_ (+) (0 :: Integer) ([1, 2, 3, 4] :: [Integer])
                `shouldBe` 10

        it "works with multiplication" $
            foldl_ (*) (1 :: Integer) ([1, 2, 3, 4] :: [Integer])
                `shouldBe` 24

        it "returns the initial value for an empty list" $
            foldl_ (+) (42 :: Integer) ([] :: [Integer]) `shouldBe` 42

        it "preserves left-associative behavior" $
            foldl_ (-) (0 :: Integer) ([1, 2, 3] :: [Integer])
                `shouldBe` (-6)

    describe "foldr_" $ do
        it "folds from the right" $
            foldr_ (+) (0 :: Integer) ([1, 2, 3, 4] :: [Integer])
                `shouldBe` 10

        it "returns the initial value for an empty list" $
            foldr_ (+) (42 :: Integer) ([] :: [Integer])
                `shouldBe` 42

        it "preserves right-associative behavior" $
            foldr_ (-) (0 :: Integer) ([1, 2, 3] :: [Integer])
                `shouldBe` 2
