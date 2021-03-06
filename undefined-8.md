# 모듈

**하스켈 모듈은 관련된 함수, 타입, 타입클래스들을 하나로 묶은것**입니다. **하스켈 프로그램은 모듈들의 집합**이고, 메인 모듈에서 다른 모듈들을 로딩하여 정의된 함수들을 사용하는 것입니다. 이렇게 코드를 여러개의 모듈로 분리하는 것은 여러가지 이점이 있습니다. 서로 의존성이 적은 코드\(loosely coupled\)들이 충분히 일반적인 모듈로 분리되면 **다양한 프로그램에서 재사용**할 수 있습니다. 또한 코드를 어떤 목적을 가진 부분들로 나누어서 관리할 수 있습니다.

하스켈의 표준 라이브러리는 각각 공통의 목적을 제공하는 함수와 타입들로 분리되어 있습니다. 여기에는 리스트를 다루는 모듈, 복잡한 숫자를 다루는 모듈, 동시성 프로그래밍을 위한 모듈 등이 있습니다. 지금까지 다루었던 모든 함수, 타입, 타입클래스들은 Prelude 모듈의 일부였고, 하스켈에 기본으로 imported되어 있습니다. 이번 챕터에서는 몇가지 유용한 모듈과 여기서 제공하는 함수들에 대해서 알아보겠습니다.

하스켈은 문법적으로 `import <module name>`로 모듈들을 가져옵니다. 이 구문은 반드시 어떤 함수가 선언되기 이전에 나와야 해서 파일의 최상당에 위치하는 것이 일반적입니다. 하나의 파일에서는 여러개의 모듈을 가져올 수 있고, 라인단위로 구분해서 import문을 추가하면 됩니다.

`Data.List` 모듈은 리스트를 동작시키는 유용한 함수들을 제공합니다.

```haskell
import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
```

`import Data.List`를 했을때 `Data.List`가 노출한 모든 함수들은 스크립트내 어디서든지 사용이 가능해집니다. `nub`은 리스트에 중복을 제거하는 함수로 `Data.List`에 정의되어 있습니다. `length . nub` 합성함수는 `\xs -> length (nub xs)`와 동일합니다.

```haskell
ghci> :m + Data.List
```

GHCI를 사용할때는 위와 같은 방법으로 `Data.List` 모듈내 모든 함수들을 가져올 수 있습니다.

```haskell
ghci> :m + Data.List Data.Map Data.Set
```

GHCI에서 여러개의 모듈을 한번에 로딩할때는 위와 같이 작성하면 됩니다. 하지만 만약에 이미 모듈을 가지고있는 스크립트를 로딩했다면 다시 `:m +`를 사용할 필요가 없습니다.

```haskell
import Data.List (nub, sort)
```

만약 모듈에서 두개의 함수만 필요하면, 위와 같이 필요한 함수만 가져올 수 있습니다.

```haskell
import Data.List hiding (nub)
```

만약 모듈에서 특정 함수만 제외하고 가져오고 싶다면, 위와 같이 할 수 있습니다. 여러개의 모듈이 동일한 이름의 함수를 export하고 있거나 잘못된 함수들을 제거할때 종종 사용됩니다. 예를들어 이미 우리 코드에서 `nub`라는 함수를 가지고 있어서 `Data.List`에서 `nub` 함수만 제외한 모든 함수를 가져오고 싶을때 사용됩니다.

```haskell
import qualified Data.Map
```

함수명이 충돌날때 해결하는 다른 방법으로 qualified import가 있습니다. `Data.Map`은 key, value 자료구조를 제공하는 모듈로 `filter`나 `null` 함수와 같이 `Prelude`와 같은 이름을 가진 함수들이 있습니다. 이때 위 예제와 같이 qualified를 사용하여 선언하면 `Data.Map`의 `filter`를 사용하기 위해서는 `Data.Map.filter`로 사용해야 합니다. 반면에 `filter`를 사용하면 기본으로 제공되는 `filter`가 사용됩니다.

```haskell
import qualified Data.Map as M
```

위 예제와 같이 `Data.Map`을 치환하여 `M.filter`로 사용할 수도 있습니다.

아래 링크에서 표준 라이브러리에 정의된 모듈들과 소스코드를 확인할 수 있습니다. 하스켈에서 어떤 모듈들과 함수들을 표준 라이브러리로 제공하는지 살펴보고 각 모듈의 소스코드를 보는 것은 하스켈을 이해하는 큰 도움이 됩니다.

[https://downloads.haskell.org/~ghc/latest/docs/html/libraries/](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/)

또한 [Hoogle](https://www.haskell.org/hoogle/)에서는 모듈, 함수, 타입 등을 검색할 수 있는 하스켈 검색엔진을 제공합니다.

## Data.List

`Data.List` 모듈은 리스트 다루는 모든 함수를 제공합니다. 편의를 위해서 `Data.List`의 일부 함수\(`map`, `filter` 등\)들은 `Prelude` 모듈에 export되어 있습니다. 또한 `Prelude`와 `Data.List`의 함수들은 이름 충돌이 발생하지 않습니다. 여기서는 `Data.List`에서 제공하는 함수들에 대해서 살펴보겠습니다.

### intersperse

리스트의 구성요소 하나와 리스트를 입력받아서, 리스트의 구성요소 사이사이에 입력받은 구성요소를 넣은 리스트를 리턴합니다.

```haskell
ghci> intersperse '.' "MONKEY"
"M.O.N.K.E.Y"
ghci> intersperse 0 [1,2,3,4,5,6]
[1,0,2,0,3,0,4,0,5,0,6]
```

### intercalate

중첩 리스트와 리스트를 입력받아서, 중첩 리스트의 리스트 사이사이에 입력된 리스트를 넣은 리스트를 리턴합니다.

```haskell
ghci> intercalate " " ["hey","there","guys"]
"hey there guys"
ghci> intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]]
[1,2,3,0,0,0,4,5,6,0,0,0,7,8,9]
```

### transpose

중첩 리스트를 재배열합니다. 중첩 리스트를 행렬로 본다면 열을 행으로 행을 열로 바꾼 리스트를 반환합니다.

```haskell
ghci> transpose [[1,2,3],[4,5,6],[7,8,9]]
[[1,4,7],[2,5,8],[3,6,9]]
ghci> transpose ["hey","there","guys"]
["htg","ehu","yey","rs","e"]
```

다항식 , , 를 더할때, 리스트로 `[0,3,5,9]`, `[10,0,0,9]`, `[8,5,1,-1]`와 같이 표현한다면 아래와 같이 계산할 수 있습니다.

```haskell
ghci> map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]
[18,8,6,17]
```

위와같이 3개의 리스트를 `transpose`하면 지수를 리스트의 인덱스로 설정하여 재배열한 후, 더하면 원하는 결과를 얻을 수 있습니다.

### foldl', foldl1'

각각 `foldl`, `foldl1` 함수에 비해 게으르지않고 엄격한 함수 입니다. 매우 큰 리스트를 게으르게 fold할때 스택 오버플로우 에러를 종종 볼 수 있습니다. 이런 에러를 발생시키는 주범은 fold를 할때 accumulator값이 실제로 갱신되지 않아서 입니다. 실제로는 accumulator가 실제 결과\(thunk라고 불림\)로 만들어질때 그 값을 계산하는데, 이것은 재귀호출 중간에 만들어지는 모든 accumulator에서 thunk들을 만들어 스택 오버플로우를 발생시킵니다. `foldl'`와 `foldl1'`는 게으른 함수가 아니라서 thunk를 스택에 쌓는대신 그때그때 중간 값을 계산합니다. 따라서 스택 오버플로우가 발생한다면 `foldl'`, `foldl1'` 함수로 전환해 볼 수 있습니다.

### concat

중첩 리스트를 하나의 리스트로 이어주는 함수입니다.

```haskell
ghci> concat ["foo","bar","car"]
"foobarcar"
ghci> concat [[3,4,5],[2,3,4],[2,1,1]]
[3,4,5,2,3,4,2,1,1]
```

이 함수는 1레벨만 중첩을 제거합니다. 따라서 두번 중첩된 리스트를 하나의 리스트로 만들고 싶다면 `concat`을 두번해야 합니다. 예를들어 `[[[2,3],[3,4,5],[2]],[[2,3],[3,4]]]`를 하나의 리스트로 만들기 위해서는 `concat`을 두번 호출해야 합니다.

### concatMap

맵핑 함수와 리스트를 받아서 입력 리스트를 맵핑하여 한개의 리스트로 합쳐주는 함수입니다.

```haskell
ghci> concatMap (replicate 4) [1..3]
[1,1,1,1,2,2,2,2,3,3,3,3]
```

### and

boolean의 리스트의 모든 값이 참이면 `True`를 리턴하는 함수입니다.

```haskell
ghci> and $ map (>4) [5,6,7,8]
True
ghci> and $ map (==4) [4,4,4,3,4]
False
```

### or

boolean의 리스트의 값중 하나라도 참이 있으면 `True`를 리턴하는 함수입니다.

```haskell
ghci> or $ map (==4) [2,3,4,5,6,1]
True
ghci> or $ map (>4) [1,2,3]
False
```

### any, all

`any`는 조건문\(predicate\)과 리스트를 입력받아서 리스트의 값들중 하나라고 조건문에 참이면 `True`를 리턴하는 함수입니다.  
`all`은 조건문\(predicate\)과 리스트를 입력받아서 모든 리스트의 값들이 조건문에 참이면 `True`를 리턴하는 함수입니다.  
`any`와 `all`은 리스트의 모든 구성요소를 조건문에 확인해야할때 `and`나 `or` 대신에 사용됩니다.

```haskell
ghci> any (==4) [2,3,5,6,1,4]
True
ghci> all (>4) [6,9,10]
True
ghci> all (`elem` ['A'..'Z']) "HEYGUYSwhatsup"
False
ghci> any (`elem` ['A'..'Z']) "HEYGUYSwhatsup"
True
```

### iterate

함수와 초기값을 받아서 초기값을 함수에 적용한 결과값을 다시 함수에 적용하는 것을 계속해서 반복하여 무한 리스트를 리턴하는 함수입니다.

```haskell
ghci> take 10 $ iterate (*2) 1 
[1,2,4,8,16,32,64,128,256,512]
ghci> take 3 $ iterate (++ "haha") "haha"
["haha","hahahaha","hahahahahaha"]
```

### splitAt

숫자와 리스트를 받아서 리스트를 입력받은 숫자 위치를 기준으로 분리한 후, 튜플을 리턴하는 함수입니다.

```haskell
ghci> splitAt 3 "heyman"
("hey","man")
ghci> splitAt 100 "heyman"
("heyman","")
*ghci> splitAt (-3) "heyman"
("","heyman")
ghci> let (a,b) = splitAt 3 "foobar" in b ++ a
"barfoo"
```

### takeWhile

조건문\(predicate\)과 리스트를 입력받아서 주어진 조건문이 참일때까지만 가져온 리스트를 리턴하는 함수입니다.

```haskell
ghci> takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1]
[6,5,4]
ghci> takeWhile (/=' ') "This is a sentence"
"This"
```

자연수의 세제곱값이 10000보다 작은 값들의 합을 구하려면 어떻게 해야 할까요?  
`[1..]`에 `^3`를 하고 filter를 쓰고 더하려고하면 무한 리스트이기 때문에 영원히 끝나지 않을 것입니다. 이 문제를 takeWhile을 사용하면 아래와 같이 구할 수 있습니다.

```haskell
ghci> sum $ takeWhile (<10000) $ map (^3) [1..]
53361
```

무한 리스트에 `^3`을 적용한 값이 10000보다 크면 중지하고 모든 수를 더해서 쉽게 답을 얻을 수 있습니다.

### dropWhile

`takeWhile`와 유사하게 조건문이 거짓일때까지의 값들을 제외한 리스트를 리턴하는 함수입니다.

```haskell
ghci> dropWhile (/=' ') "This is a sentence"
" is a sentence"
ghci> dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1]
[3,4,5,4,3,2,1]
```

`(Stock, year, month, date)`로 구성된 튜플의 리스트를 받았을때, stock이 천달러를 초과하는 시점은 아래와 같이 구할 수 있습니다.

```haskell
ghci> let stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]
ghci> head (dropWhile (\(val,y,m,d) -> val < 1000) stock)
(1001.4,2008,9,4)
```

### span

`takeWhile`과 유사하지만 리스트 쌍을 리턴하는 함수입니다. 동일한 조건\(predicate\)에 동일한 리스트를 입력으로 `takeWhile`을 호출한 결과가 첫번째 리스트가 되고 `takeWhile`에 의해서 제외된 값들이 두번째 리스트가 됩니다.

```haskell
ghci> let (fw, rest) = span (/=' ') "This is a sentence" in "First word:" ++ fw ++ ", the rest:" ++ rest
"First word: This, the rest: is a sentence"
```

### break

`span` 함수와 유사하지만 `break` 함수는 조건문이 첫번째 참일때 중단합니다. `break p`는 `span (not . p)`와 동일한 기능을 합니다.

```haskell
ghci> break (==4) [1,2,3,4,5,6,7]
([1,2,3],[4,5,6,7])
ghci> span (/=4) [1,2,3,4,5,6,7]
([1,2,3],[4,5,6,7])
```

위 예제와 같이 `break` 함수는 두번째 리스트의 첫번째 값이 조건문을 만족하는 첫번째 값입니다.

### sort

리스트를 정렬하는 함수입니다. 리스트내 값들의 타입클래스를 `Ord`에 포함되어야 합니다. 순서를 판단할 수 없는 값은 정렬을 할수없습니다.

```haskell
ghci> sort [8,5,3,2,1,6,4,2]
[1,2,2,3,4,5,6,8]
ghci> sort "This will be sorted soon"
"    Tbdeehiillnooorssstw"
```

### group

리스트를 받아서 동일한 값이 인접해 있으면 하위 리스트로 묶어주는 함수 입니다.

```haskell
ghci> group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
[[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]
```

만약 리스트를 그룹핑하기 전에 정렬하면 리스트에서 각 값들이 몇번 나왔는지 알 수 있습니다.

```haskell
ghci> map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
[(1,4),(2,7),(3,2),(5,1),(6,1),(7,1)]
```

### inits, tails

`init`과 `tail`를 재귀적으로 아무것도 없을때 까지 호출한 결과들의 리스트를 리턴합니다. 아래 화면을 보면 쉽게 이해할 수 있습니다.

```haskell
ghci> inits "w00t"
["","w","w0","w00","w00t"]
ghci> tails "w00t"
["w00t","00t","0t","t",""]
ghci> let w = "w00t" in zip (inits w) (tails w)
[("","w00t"),("w","00t"),("w0","0t"),("w00","t"),("w00t","")]
```

`fold` 함수를 사용해서 하위 리스트의 리스트를 검색하는 것을 구현하면 아래와 같습니다.

```haskell
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack = 
    let nlen = length needle
    in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)
```

먼저 검색할 대상 리스트에 `tails`를 호출하여 tail의 리스트를 만들고, 각 tail가 찾고있는 것으로 시작하는지 확인합니다. 여기서 `take nlen x == needle이` x가 needle로 시작하는지 확인하는 방법입니다.

### isInfixOf

입력받은 첫번째 하위 리스트가 두번째 대상 리스트에 포함되는지 검색하는 함수입니다. 만약 대상 리스트내의 어디에든 하위 리스트가 포함되면 `True`를 리턴합니다.

```haskell
ghci> "cat" `isInfixOf` "im a cat burglar"
True
ghci> "Cat" `isInfixOf` "im a cat burglar"
False
ghci> "cats" `isInfixOf` "im a cat burglar"
False
```

### isPrefixOf, isSuffixOf

`isPrefixOf`는 하위 리스트가 대상 리스트에 시작인지 확인하는 함수입니다.  
`isSuffixOf`는 하위 리스트가 대상 리스트에 마지막인지 확인하는 함수입니다.

```haskell
ghci> "hey" `isPrefixOf` "hey there!"
True
ghci> "hey" `isPrefixOf` "oh hey there!"
False
ghci> "there!" `isSuffixOf` "oh hey there!"
True
ghci> "there!" `isSuffixOf` "oh hey there"
False
```

### elem && notElem

리스트안에 입력받은 구성요소가 포함되는지 여부를 확인하는 함수입니다.

### partition

조건문\(predicate\)과 리스트를 입력받아서 리스트의 쌍을 리턴합니다. 첫번째 리스트는 조건문에 만족하는 구성요소들의 리스트이고, 두번째 리스트는 나머지 구성요소들의 리스트입니다.

```haskell
ghci> partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"
("BOBMORGAN","sidneyeddy")
ghci> partition (>3) [1,3,5,6,3,2,1,0,3,7]
([5,6,7],[1,3,3,2,1,0,3])
```

`span`과 `break`와 어떻게 다른지 이해하는 것이 중요합니다.

```haskell
ghci> span (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"
("BOB","sidneyMORGANeddy")
```

`span`과 `break`는 조건문에 만족하는 첫번째 구성요소까지 첫번째 리스트로 만들고, `partition`은 만족하는 모든 구성요소를 첫번째 리스트 만듭니다.

### find

조건문\(predicate\)과 리스트를 입력받아서 조건을 만족하는 첫번째 구성요소가 `Maybe`값으로 랩핑된 구성요소로 리턴됩니다. 다음 챕터에서 대수적인 데이터 타입에 대해서 더 자세히 다룰 것입니다. 여기서는 알아야 할 것은 `Maybe`**값은 어떤 값을 가지거나 아무것도 없을 수 있다는 점입니다.** 마치 리스트가 비어있거나 어떤 구성요소를 가질 수 있는 것처럼 `Maybe`값은 아무 것도 없거나 하나의 구성요소를 가질 수 있습니다. Integer의 리스트의 타입이 `[Int]`인 것처럼 Interger를 가질수도 있는 타입을 `Maybe Int` 라고 합니다.

```haskell
ghci> find (>4) [1,2,3,4,5,6]
Just 5
ghci> find (>9) [1,2,3,4,5,6]
Nothing
ghci> :t find
find :: (a -> Bool) -> [a] -> Maybe a
```

`find`의 리턴 타입은 `Maybe a`입니다. 따라서 `find`의 결과는 구성요소가 있거나 없을수도 있습니다.

주가가 1000$를 넘는 순간을 찾는 문제는 아래와 같이 풀 수도 있습니다.

```haskell
head (dropWhile (\(val,y,m,d) -> val < 1000) stock)
```

여기서 `head`를 사용하는 것은 안전하지 않다는 점에 유의해야 합니다. 주가가 영원히 1000$를 넘지않는다면 어떻게 될까요? `dropWhile`은 빈리스트를 리턴하고 head가 없기때문에 런타임 에러가 발생합니다. 하지만 `find`를 사용하면 아래와 같이 재작성될 수 있습니다.

```haskell
find (\(val,y,m,d) -> val > 1000) stock
```

이 경우는 주가가 영원히 1000$를 넘지않아도 `Nothing`을 리턴하기 때문에 안전합니다. 만약 유효한 값이 있다면 `Just (1001.4,2008,9,4)`와 같이 리턴할 것입니다.

### elemIndex

`elem`과 동일한 기능을 하지만 boolean값을 리턴하지않습니다. 찾고있는 구성요소의 index를 리턴합니다. 만약 찾는 구성요소가 리스트에 없으면 `Nothing`을 리턴합니다.

```haskell
ghci> :t elemIndex
elemIndex :: (Eq a) => a -> [a] -> Maybe Int
ghci> 4 `elemIndex` [1,2,3,4,5,6]
Just 3
ghci> 10 `elemIndex` [1,2,3,4,5,6]
Nothing
```

### elemIndices

`elemIndex`와 동일한 기능을 하지만 찾고있는 구성요소가 여러개인 경우 index들의 리스트를 리턴합니다. 구성요소가 한개도 없는 경우는 `Nothing`과 유사하게 빈리스트를 반환합니다.

```haskell
ghci> ' ' `elemIndices` "Where are the spaces?"
[5,9,13]
```

### findIndex, findIndices

`findIndex`는 `find`와 동일한 기능을 하지만 조건에 만족하는 첫번째 구성요소의 index를 리턴합니다.  
`findIndices`는 조건에 만족하는 모든 구성요소의 index 리스트를 리턴합니다.

```haskell
ghci> findIndex (==4) [5,3,2,1,6,4]
Just 5
ghci> findIndex (==7) [5,3,2,1,6,4]
Nothing
ghci> findIndices (`elem` ['A'..'Z']) "Where Are The Caps?"
[0,6,10,14]
```

### zip3, zip4, zipWith3, zipWith4

`zip3`는 3개의 리스트를 받아서 3개의 튜플의 리스트로 묶는 함수입니다.  
`zip4`는 4개의 리스트를 받아서 4개의 튜플의 리스트로 묶는 함수입니다.  
`zipWith3`는 3개의 인자를 받는 함수와 3개의 리스트를 받아서 묶은 후, 각 튜플을 입력받은 함수에 적용한 결과의 리스트를 리턴합니다.  
`zipWith4`는 4개의 인자를 받는 함수와 4개의 리스트를 받아서 묶은 후, 각 튜플을 입력받은 함수에 적용한 결과의 리스트를 리턴합니다.  
이런 `zip`과 `zipWith`의 변종 함수는 7개까지 있습니다. 또한 무한개의 리스트를 묶을때도 매우 좋은 방법이 있지만, 여기서는 다루지 않겠습니다.

```haskell
ghci> zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3]
[7,9,8]
ghci> zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]
[(2,2,5,2),(3,2,5,2),(3,2,3,2)]
```

`zip` 계열의 함수들은 모두 입력받은 리스트중 가장 짧은 리스트를 기준으로 잘려서 동작합니다.

### lines

문자열을 입력받아서 라인단위로 분리된 리스트를 리턴합니다. `lines`는 파일이나 외부입력을 다룰때 매우 유용한 함수입니다.

```haskell
ghci> lines "first line\nsecond line\nthird line"
["first line","second line","third line"]
```

예제에서 `\n`은 unix의 개행문자입니다. 하스켈의 문자열이나 문자들에서 백슬래쉬\(\\)는 특별한 의미를 가집니다.

### unlines

`lines`와 반대로 여러개의 문자열의 리스트를 `'\n'`를 사용하여 하나의 문자열로 합칩니다.

```haskell
ghci> unlines ["first line", "second line", "third line"]
"first line\nsecond line\nthird line\n"
```

### words, unwords

`words`는 하나의 라인을 문자열로 받아서 단어들의 리스트로 분리합니다.  
`unwords`는 단어들의 리스트를 하나의 문자열로 합칩니다.

```haskell
ghci> words "hey these are the words in this sentence"
["hey","these","are","the","words","in","this","sentence"]
ghci> words "hey these           are    the words in this\nsentence"
["hey","these","are","the","words","in","this","sentence"]
ghci> unwords ["hey","there","mate"]
"hey there mate"
```

### nub

리스트를 받아서 중복된 값들을 제거하고 유일한 값들의 리스트를 리턴합니다.

```haskell
ghci> nub [1,2,3,4,3,2,1,2,3,4,3,2,1]
[1,2,3,4]
ghci> nub "Lots of words and stuff"
"Lots fwrdanu"
```

### delete

한 개의 구성요소와 리스트를 받아서 리스트내에서 입력받은 구성요소와 처음으로 일치하는 구성요소만 삭제된 리스트를 리턴합니다.

```haskell
ghci> delete 'h' "hey there ghang!"
"ey there ghang!"
ghci> delete 'h' . delete 'h' $ "hey there ghang!"
"ey tere ghang!"
ghci> delete 'h' . delete 'h' . delete 'h' $ "hey there ghang!"
"ey tere gang!"
```

### //

두개의 리스트를 입력받아서 왼쪽 리스트에서 오른쪽 리스트와 매칭되는 값만 삭제한 리스트를 리턴합니다.

```haskell
ghci> [1..10] \\ [2,5,9]
[1,3,4,6,7,8,10]
ghci> "Im a big baby" \\ "big"
"Im a  baby"
```

`[1..10] // [2,5,9]`는 `delete 2 . delete 5 . delete 9 $ [1..10]`과 동일한 동작을 합니다.

### union

두개의 리스트를 받아서 두번째 리스트에서 첫번째 리스트와 중복되는 구성요소를 제거하고 첫번째 리스트와 합친 리스트를 리턴합니다. 즉, 두 리스트의 합집합을 구합니다.

```haskell
ghci> "hey man" `union` "man what's up"
"hey manwt'sup"
ghci> [1..7] `union` [5..10]
[1,2,3,4,5,6,7,8,9,10]
```

### intersect

두개의 리스트를 입력받아서 양쪽에 모두 존재하는 구성요소들의 리스트를 리턴합니다. 즉, 교집합을 구합니다.

```haskell
ghci> [1..7] `intersect` [5..10]
[5,6,7]
```

### insert

리스트에 삽입할 값과 정렬이 가능한 리스트를 받아서 대상 리스트에 넣을 값보다 작거나 같은 구성요소 다음에 삽입하는 함수입니다.

```haskell
ghci> insert 4 [3,5,1,2,8,2]
[3,4,5,1,2,8,2]
ghci> insert 4 [1,3,4,4,1]
[1,3,4,4,4,1]
```

이 예제에서 `4`는 `3`과 `5` 사이의 숫자로 해당 위치에 넣은 것을 확인할 수 있습니다.

```haskell
ghci> insert 4 [1,2,3,5,6,7]
[1,2,3,4,5,6,7]
ghci> insert 'g' $ ['a'..'f'] ++ ['h'..'z']
"abcdefghijklmnopqrstuvwxyz"
ghci> insert 3 [1,2,4,3,2,1]
[1,2,3,4,3,2,1]
```

만약 이미 정렬된 리스트에 넣는다면 결과 리스트도 정렬된 리스트입니다.

### genericLength, genericTake, genericDrop, genericSplitAt, genericIndex, genericReplicate

`length`, `take`, `drop`, `splitAt`, `!!`, `replicate`는 모두 입력 인자로 `Int`를 받거나, `Int`를 리턴하는 함수입니다. 함수에 따라서 `Integral`이나 `Num` 타입클래스\(함수에 따라서\)를 받는다면 좀 더 일반적이고 유용하게 사용될 수 있습니다. 이렇게 보다 일반적인 함수들을 `genericXXX` 형태 제공합니다. 예를들어 `length`의 타입은 `length :: [a] -> Int`입니다. 만약 `let xs = [1..6] in sum xs / length xs`로 숫자들의 리스트의 평균을 구한다면, `/`는 `Int`를 사용할 수 없기때문에 타입에러가 발생합니다. 반대로 `genericLength`의 타입은 `genericLength :: (Num a) => [b] -> a` 입니다. `Num`은 부동소수점처럼 동작할 수 있기때문에 `let xs = [1..6] in sum xs / genericLength xs`는 잘 동작합니다.

### nubBy, deleteBy, unionBy, intersectBy, groupBy

`nub`, `delete`, `union`, `intersect`, `group`은 각각 좀 더 일반적인 함수로 `nubBy`, `deleteBy`, `unionBy`, `intersectBy`, `groupBy` 함수를 가지고 있습니다. `genericXXX` 함수들은 동등성 체크를 `==`으로 하는반면에 `xxxBy` 함수들은 동등 함수를 받아서 비교한다는 점\(예를들어 `group`은 `groupBy (==)`과 동일\)에서 함수명 규칙을 다르게 적용하였습니다.

```haskell
ghci> let values = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]
ghci> groupBy (\x y -> (x > 0) == (y > 0)) values
[[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]
```

값이 0보다 큰것과 작은것을 기준으로 하위 리스트로 분류하는 함수입니다. 만약 여기서 `group`을 사용했다면 인접한 값을 함께 그룹핑할 것입니다. 하지만 여기서 우리가 얻고자 하는 것은 음수인지 아닌지에 따라서 분류하는 것입니다. 이 예제에서는 음수 섹션과 양수 섹션으로 명확히 구분된 것을 볼 수 있습니다. 동등 함수는 두개의 요소를 받아서 둘다 양수이거나 둘다 음수이면 `True`를 리턴합니다. `Data.Function.on`의 `on` 함수를 활용하면 동등함수를 좀 더 명확하게 작성할 수 있습니다.

### on

`on` 함수는 두개의 인자를 입력받는 함수\(f\), 한개의 인자를 받는 함수\(g\)를 받아서 두개의 값을 받아서 각각에 g를 적용하고 두개의 결과를 f를 적용하는 함수를 리턴합니다. `on` 함수는 아래와 같이 정의될 수 있습니다.

```haskell
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on` g = \x y -> f (g x) (g y)
```

따라서 ``(==) `on` (> 0)``은 `\x y -> (x > 0) == (y > 0)`과 같은 동등함수를 리턴합니다. `on`은 아래 예제와 같이 _By_ 함수와 함께 자주 사용됩니다.

```haskell
ghci> groupBy ((==) `on` (> 0)) values
[[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]
```

### sortBy, insertBy, maximumBy, minimumBy

`sort`, `insert`, `maximum`, `minimum` 함수도 좀더 일반적인 함수들을 가지고 있습니다. `groupBy`와 같은 함수들은 두개의 값이 동일한지를 결정하는 함수를 받습니다. `sortBy`, `insertBy`, `maximumBy`, `minimumBy` 함수는 한개의 값이 다른 값보다 큰지, 작은지, 같은지를 판단하는 함수를 입력받습니다. `sortBy`의 타입은 `sortBy :: (a -> a -> Ordering) -> [a] -> [a]` 입니다. `Ordering`은 `LT`, `EQ`, `GT`를 값으로 가집니다. `sort`는 `sortBy compare`와 같습니다. 왜냐하면 `compare`는 `Ord` 타입클래스인 두개의 값을 받아서 순서 관계를 리턴하기 때문입니다.

리스트들은 비교할 수는 있지만 사전식으로 비교가 됩니다. 만약 리스트의 리스트가 있을때 리스트의 내용이 아니라 내부 리스트의 길이에 따라서 정렬하려면 어떻게 해야할까요? 아래 예와 같이 `sortBy`를 사용해서 해결할 수 있습니다.

```haskell
ghci> let xs = [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]
ghci> sortBy (compare `on` length) xs
[[],[2],[2,2],[1,2,3],[3,5,4,3],[5,4,5,4,4]]
```

여기서 ``compare `on` length``는 마치 영어 문장을 쓰는 것 처럼 자연스러운 것을 볼 수 있습니다. ``compare `on` length``는 ``\x y -> length x `compare` length y``와 동일합니다. _By_ 함수에서 동등함수\(equality function\)를 받을때는 주로 `(==)`on`something` 형태로 쓰고, 비교함수\(ordering function\)를 받을때는 주로 ``compare `on` something`` 형태로 사용합니다.

## Data.Char

문자를 다루는데 유용한 함수를 포함하고 있는 모듈입니다. 문자열에 대한 filter나 mapping도 `Data.Char` 모듈에 포함됩니다.  
`Data.Char`에는 문자들에 대한 속성을 체크할 수 있는 여러가지 함수를 제공합니다.

### isControl

문자가 control 문자인지 확인합니다.

### isSpace

문자가 공백문자\(space, tab, newline, etc\)인지 확인합니다.

### isLower

소문자인지 확인합니다.

### isUpper

대문자인지 확인합니다.

### isAlpha

문자가 알파벳인지 확인합니다.

### isAlphaNum

문자가 알파벳이나 숫자인지 확인합니다.

### isPrint

출력가능한 문자인지 확인합니다. 예를들어 control 문자는 출력이 불가능합니다.

### isDigit

문자가 0-9의 10진수 숫자인지 확인합니다.

### isOctDigit

문자가 0-7 8진수 숫자인지 확인합니다.

### isHexDigit

문자가 0-F 16진수 숫자인지 확인합니다.

### isLetter

문자가 알파벳인지 확인합니다.

### isMark

문자가 유니코드 마크 문자인지 확인합니다. 앞글자와 결합하여 악센트가 있는 마디를 만드는 문자로서 예를들면 프랑스어가 있습니다.

### isNumber

문자가 numeric인지 확인합니다.

### isPunctuation

문자가 구두점인지 확인합니다.

### isSymbol

문자가 수학기호나 화폐문자인지 확인합니다.

### isSeparator

유니코드 공백과 구분 기호를 확인합니다.

### isAscii

유니코드 문자셋의 처음 128개 문자인지 확인합니다.

### isLatin1

유니코드 문자셋의 처음 256개 문자인지 확인합니다.

### isAsciiUpper

아스키 대문자인지 확인합니다.

### isAsciiLower

아스키 소문자인지 확인합니다.

지금까지 살펴본 함수들의 타입은 `Char -> Bool`입니다. 문자열같은 것을 필터링하거나 `Data.List`의 `all` 함수와 함께 활용되기도 합니다.

```haskell
ghci> all isAlphaNum "bobby283"
True
ghci> all isAlphaNum "eddy the fish!"
False
```

`all`은 조건문\(predicate\)와 리스트를 받아서 모든 값이 조건을 만족하면 `True`를 리턴합니다.

```haskell
ghci> words "hey guys its me"
["hey","guys","its","me"]
ghci> groupBy ((==) `on` isSpace) "hey guys its me"
["hey"," ","guys"," ","its"," ","me"]
```

`isSpace`를 활용해서 `words` 함수를 흉내낸 예제입니다. 하지만 공백이 제거되지 않고 있습니다.

```haskell
ghci> filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ "hey guys its me"
["hey","guys","its","me"]
```

`filter`를 사용하여 공백문자를 제거하였습니다.

```haskell
ghci> generalCategory ' '
Space
ghci> generalCategory 'A'
UppercaseLetter
ghci> generalCategory 'a'
LowercaseLetter
ghci> generalCategory '.'
OtherPunctuation
ghci> generalCategory '9'
DecimalNumber
ghci> map generalCategory " \t\nA9?|"
[Space,Control,Control,UppercaseLetter,DecimalNumber,OtherPunctuation,MathSymbol]
```

`GeneralCategory`은 `Ordering`과 같은 열거형\(enumeration\) 타입입니다. 이것은 문자를 몇가지 가능한 카테고리로 보여줍니다. 이 카테고리를 확인할 수 있는 함수로 `generalCategory`가 있습니다. 이 함수의 타입은 `generalCategory :: Char -> GeneralCategory`입니다. 이 함수는 총 31개의 카테고리를 가지고 잇습니다. `GeneralCategory`는 `Eq` 타입클래스에 속해있고, `generalCategory c == Space`와 같이 테스트할 수 있습니다.

### toUpper

문자를 대문자로 바꾸어 줍니다. 공백이나 숫자등은 변경되지 않습니다.

### toLower

문자를 소문자로 바꾸어 줍니다.

### toTitle

문자를 title-case로 바꾸어 줍니다. 여기서 title-case를 대문자와 동일합니다.

### digitToInt

문자를 `Int`로 바꾸어 줍니다. 이때 문자는 `'0'..'9'`, `'a'..'f'`, `'A'..'F'` 범위내에 있어야 합니다.

```haskell
ghci> map digitToInt "34538"
[3,4,5,3,8]
ghci> map digitToInt "FF85AB"
[15,15,8,5,10,11]
```

### intToDigit

`digitToInt`와 반대입니다. `0..15` 범위의 `Int`를 받아서 소문자로 바꾸어 줍니다.

```haskell
ghci> intToDigit 15
'f'
ghci> intToDigit 5
'5'
```

### ord, ord

문자를 상응하는 아스키 코드 숫자로 바꾸어 주거나 그 반대로 바꾸어 줍니다.

```haskell
ghci> ord 'a'
97
ghci> chr 97
'a'
ghci> map ord "abcdefgh"
[97,98,99,100,101,102,103,104]
```

두 문자의 `ord` 차이는 곧 유니코드 테이블에서 얼마나 떨어져있는지를 의미합니다.

```haskell
encode :: Int -> String -> String
encode shift msg = 
    let ords = map ord msg
        shifted = map (+ shift) ords
    in  map chr shifted
```

Caesar cipher는 문자를 알파벳의 고정된 숫자만큼 이동시켜 메시지를 암호화하는 원시적인 방법입니다.  
먼저, 문자열을 숫자들의 리스트로 바꾸고, 숫자들의 리스트를 다시 문자로 바꾸기전에 각 숫자에 shift한 양을 추가합니다.  
만약 composition을 사용한다면 함수의 몸제는 `map (chr . (+shift) . ord) msg`와 같이 작성될 수 있습니다 실행하면 아래와 같은 결과를 볼 수 있습니다.

```haskell
ghci> encode 3 "Heeeeey"
"Khhhhh|"
ghci> encode 4 "Heeeeey"
"Liiiii}"
ghci> encode 1 "abcd"
"bcde"
ghci> encode 5 "Marry Christmas! Ho ho ho!"
"Rfww~%Hmwnxyrfx&%Mt%mt%mt&"
```

메시지를 디코딩하는 것은 기본적으로 메시지를 처음 옮긴 장소의 수만큼 뒤로 이동하는 것입니다.

```haskell
decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg
```

```haskell
ghci> encode 3 "Im a little teapot"
"Lp#d#olwwoh#whdsrw"
ghci> decode 3 "Lp#d#olwwoh#whdsrw"
"Im a little teapot"
ghci> decode 5 . encode 5 $ "This is a sentence"
"This is a sentence"
```

## Data.Map

연관 리스트\(또는 사전\)는 순서가 중요하지않은 키값 쌍을 저장하는데 사용되는 리스트입니다. 예를들어 핸드폰 번호를 저장하기 위해서 사람의 이름을 키로하고, 전화번호를 값으로 하여 연관 리스트에 넣을 수 있습니다. 저장되는 순서는 중요하지 않고, 단지 올바른 사람에 해당하는 올바른 번호를 가져올 수 있으면 됩니다.

```haskell
phoneBook =   
    [("betty","555-2938")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ]
```

첫번째 값은 key이고 두번째값은 value 입니다.

```haskell
findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs
```

주어진 key에 해당하는 value를 찾아주는 함수입니다. 간단하게 key와 리스트를 받아서, 리스트에 key와 같은 것으로 filter합니다. 필터링한 리스트의 첫번째 key-value에서 value만 리턴합니다. 만약 찾는 key가 리스트에 없다면 filter한 리스트는 빈리스트가 되어 head를 가져오는 과정에서 런타임 에러가 발생합니다. 따라서 에러를 막기위해 `Maybe`를 사용합니다.

```haskell
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs) = if key == k
                         then Just v
                         else findKey key xs
```

만약 key를 찾지못하면 `Nothing`을 리턴하고, 찾으면 key에 상응하는 값인 `Just something`을 리턴합니다. 이 예제는 종료조건 -&gt; 리스트의 head, tail 분리 -&gt; 재귀호출로 이어지는 fold 패턴입니다. 따라서 아래와 같이 fold를 사용하여 구현할 수 있습니다.

```haskell
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing
```

**일반적으로 표준 리스트 재귀 패턴을 직접 사용하는 것보다는 fold를 사용하는 것이 가독성과 식별이 쉽습니다.** `foldr`이 보이면 모든 사람은 fold를 한다는 것을 알지만, 재귀를 쓰면 코드를 이해하기 위해서 시간을 들여야 합니다.

```haskell
ghci> findKey "penny" phoneBook
Just "853-2492"
ghci> findKey "betty" phoneBook
Just "555-2938"
ghci> findKey "wilma" phoneBook
Nothing
```

우리는 `Data.List`에서도 `lookup` 함수를 구현할 수 있습니다. key 해당하는 value를 얻고 싶으면 찾을때까지 리스트를 순회할 수 있습니다. `Data.Map` 모듈은 이것은 좀 더 빠르게 하는\(내부적으로 트리를 가지고 있기때문에\) 함수를 제공하고, 또한 많은 유틸리티 함수들을 제공합니다. 여기서부터는 연관리스트 대신에 맵의 동작을 살펴보겠습니다.

```haskell
import qualified Data.Map as Map
```

`Data.Map`은 `Prelude` 및 `Data.List`와 충돌하는 함수를 가지고 있으므로 qualified import를 사용합니다.  
`Data.Map`에 제공하는 함수들을 추리면 아래와 같습니다.

### fromList

연관리스트를 받아서 동일한 연관성을 가지는 맵을 리턴합니다.

```haskell
ghci> Map.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]
fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]
ghci> Map.fromList [(1,2),(3,4),(3,2),(5,5)]
fromList [(1,2),(3,2),(5,5)]
```

만약 연관리스트에 중복된 키가 있다면 제거됩니다. `fromList`의 타입은 아래와 같습니다.

```haskell
Map.fromList :: (Ord k) => [(k, v)] -> Map.Map k v
```

타입을 보면 `k`,`v` 쌍의 리스트를 받아서 타입`k`의 키들을 타입`v`로 맵핑하는 맵을 리턴합니다. 주목할 점은 일반 리스트를 연관 리스트로 만들때, 키는 같지만\(Eq 타입클래스\) 여기서는 트리내에 배치되기 위해서 순서가 있어야합니다.

키값이 `Ord` 타입클래스가 아닐때를 제외하면, key-value 연관을 위해서 항상 `Data.Map`을 사용해야 합니다.

### empty

비어있는 맵을 나타내는 것으로서 입력이 없고 단지 비어있는 맵을 리턴합니다.

```haskell
ghci> Map.empty
fromList []
```

### insert

key, value, map을 입력받아서 맵에 해당 key, value를 포함한 새로운 맵을 리턴합니다.

```haskell
ghci> Map.empty
fromList []
ghci> Map.insert 3 100 Map.empty
fromList [(3,100)]
ghci> Map.insert 5 600 (Map.insert 4 200 ( Map.insert 3 100  Map.empty))
fromList [(3,100),(4,200),(5,600)]
ghci> Map.insert 5 600 . Map.insert 4 200 . Map.insert 3 100 $ Map.empty
fromList [(3,100),(4,200),(5,600)]
```

비어있는 맵과 `insert`를 사용하면 `fromList`를 직접 구현할 수 있습니다.

```haskell
fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty
```

foldr를 사용하여 비어있는 맵에서 오른쪽부터 접으면서 key-value 쌍을 accumulator에 넣습니다.

### null

맵이 비어있는지 검사합니다.

```haskell
ghci> Map.null Map.empty
True
ghci>d Map.null $ Map.fromList [(2,3),(5,5)]
False
```

### size

맵의 크기를 알려줍니다.

```haskell
ghci> Map.size Map.empty
0
ghci> Map.size $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)]
5
```

### singleton

key, value를 받아서 입력받은 key-value쌍 한개만 가진 맵을 리턴합니다.

```haskell
ghci> Map.singleton 3 9
fromList [(3,9)]
ghci> Map.insert 5 9 $ Map.singleton 3 9
fromList [(3,9),(5,9)]
```

### lookup

```Data.List``lookup```처럼 맵에서 동작합니다. 찾는 것이 있으면 `Just something`, 없으면 `Nothing`을 리턴합니다.

### member

key와 맵을 입력받아서 맵안에 key가 있는지를 알려줍니다.

```haskell
ghci> Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)]
True
ghci> Map.member 3 $ Map.fromList [(2,5),(4,5)]
False
```

### map && filter

리스트의 `map`,`filter`와 동일한 기능을 합니다.

```haskell
ghci> Map.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)]
fromList [(1,100),(2,400),(3,900)]
ghci> Map.filter isUpper $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')]
fromList [(2,'A'),(4,'B')]
```

### toList

`fromList`와 반대의 기능을 합니다.

```haskell
ghci> Map.toList . Map.insert 9 2 $ Map.singleton 4 3
[(4,3),(9,2)]
```

### keys && elems

`keys`는 key의 리스트를 리턴하고 `map fst . Map.toList`와 동일합니다.  
`elems`는 value의 리스트를 리턴하고 `map snd . Map.toList`와 동일합니다.

### fromListWith

`fromList`와 유사하지만 중복된 키들을 버리지 않고, 함수에 적용하여 결정하는 함수입니다.

```haskell
phoneBook =   
    [("betty","555-2938")  
    ,("betty","342-2492")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("patsy","943-2929")  
    ,("patsy","827-9162")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ,("penny","555-2111")  
    ]
```

```haskell
phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String  
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs
```

```haskell
ghci> Map.lookup "patsy" $ phoneBookToMap phoneBook
"827-9162, 943-2929, 493-2928"
ghci> Map.lookup "wendy" $ phoneBookToMap phoneBook
"939-8282"
ghci> Map.lookup "betty" $ phoneBookToMap phoneBook
"342-2492, 555-2938"
```

여기서 만약 `fromList`를 사용한다면, 키가 중복된 몇몇 값들을 사라질 것입니다. 위와 같이 `fromListWith`를 사용하여 customize할 수 있습니다.

```haskell
phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]  
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs
```

```haskell
ghci> Map.lookup "patsy" $ phoneBookToMap phoneBook
["827-9162","943-2929","493-2928"]
```

만약 중복된 키가 있으면 해당하는 키의 값들을 묶어서 찾은 키에 해당하는 모든 값들을 하나의 리스트에 리턴합니다. 번호들을 묶기 위해서 `++`를 사용할 수 있습니다.

```haskell
ghci> Map.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]
fromList [(2,100),(3,29),(4,22)]
```

또다른 예로 중복된 키가 발견되면 값들중 가장 큰 것만 남기는 함수를 만들 수 있습니다.

```haskell
ghci> Map.fromListWith (+) [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]
fromList [(2,108),(3,62),(4,37)]
```

또는 중복된 키의 값들을 모두 더할 수도 있습니다.

### insertWith

`fromList`에 `fromListWith`가 있는 것처럼 `insert`에는 `insertWith`가 잇습니다. 맵에 key-value쌍을 넣지만, 만약 맵에 키가 이미 존재한다면 무엇을 해야할지를 결정하는 함수를 제공합니다.

```haskell
ghci> Map.insertWith (+) 3 100 $ Map.fromList [(3,4),(5,103),(6,339)]
fromList [(3,104),(5,103),(6,339)]
```

여기서는 `Data.Map`에 있는 몇개의 함수만 알아보았습니다.

## Data.Set

**Set은 내부적으로 트리로 구현되어 있기때문에 모든 구성요소들에는 중복이 없고, 순서가 있습니다.** Set은 리스트와 동일한 기능을할때 더 빠르게 동작합니다. Set의 주요 동작은 멤버쉽을 확인하거나, 추가, 삭제, Set을 리스트로 변환 등을 할 수 있습니다.

```haskell
import qualified Data.Set as Set
```

`Data.Set`은 `Prelude` 및 `Data.List`와 충돌하는 함수를 가지고 있으므로 qualified import를 사용합니다.

```haskell
text1 = "I just had an anime dream. Anime... Reality... Are they so different?"  
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"
```

위와같은 두개의 텍스트가 있을때, 양쪽에서 모두 사용되는 문자들을 찾아보겠습니다.

### fromList

리스트를 받아서 Set으로 바꾸는 함수입니다.

```haskell
ghci> let set1 = Set.fromList text1
ghci> let set2 = Set.fromList text2
ghci> set1
fromList " .?AIRadefhijlmnorstuy"
ghci> set2
fromList " !Tabcdefghilmnorstuvwy"
```

위 예제에서 볼 수 있듯이 Set안의 모든 아이템은 유일하고, 순서가 있습니다.

### intersection

두개의 Set을 받아서 양쪽에서 동일하게 가지고 있는 구성요소들의 리스트를 리턴합니다. 즉, 교집합을 구합니다.

```haskell
ghci> Set.intersection set1 set2
fromList " adefhilmnorstuy"
```

### difference

두개의 Set을 받아서 첫번째 Set에는 있는데 두번째 Set에는 없는 구성요소들의 리스트를 리턴합니다.

```haskell
ghci> Set.difference set1 set2
fromList ".?AIRj"
ghci> Set.difference set2 set1
fromList "!Tbcgvw"
```

### union

두개의 Set을 받아서 양쪽에 있는 모든 유일한 문자들의 리스트를 리턴합니다.

```haskell
ghci> Set.union set1 set2
fromList " !.?AIRTabcdefghijlmnorstuvwy"
```

### null, size, member, empty, singleton, insert, delete

```haskell
ghci> Set.null Set.empty
True
ghci> Set.null $ Set.fromList [3,4,5,5,4,3]
False
ghci> Set.size $ Set.fromList [3,4,5,3,4,5]
3
ghci> Set.singleton 9
fromList [9]
ghci> Set.insert 4 $ Set.fromList [9,3,8,1]
fromList [1,3,4,8,9]
ghci> Set.insert 8 $ Set.fromList [5..10]
fromList [5,6,7,8,9,10]
ghci> Set.delete 4 $ Set.fromList [3,4,5,4,3,4,5]
fromList [3,5]
```

### isSubsetOf

두개의 Set을 받아서 첫번째 Set이 두번째 Set의 subset인지 확인합니다.

```haskell
ghci> Set.fromList [2,3,4] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]
True
ghci> Set.fromList [1,2,3,4,5] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]
True
ghci> Set.fromList [1,2,3,4,5] `Set.isProperSubsetOf` Set.fromList [1,2,3,4,5]
False
ghci> Set.fromList [2,3,4,8] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]
False
```

### map && filter

```haskell
ghci> Set.filter odd $ Set.fromList [3,4,5,6,7,2,3,4]
fromList [3,5,7]
ghci> Set.map (+1) $ Set.fromList [3,4,5,6,7,2,3,4]
fromList [3,4,5,6,7,8]
```

### toList

Set은 가끔 리스트에서 중복된 값을 제거하기 위해서 사용됩니다. `fromList`로 리스트를 Set으로 바꾸고 `toList`로 다시 리스트로 만들면 중복 데이터가 제거됩니다.`Data.List`에는 이미 중복 제거를 위한 `nub` 함수이 있지만 거대한 리스트에서 중복을 제거할때는 'nub'을 사용하는 것보다 빠릅니다. 그러나 `nub`은 구성요소의 타입이 `Eq`이면 사용이 가능하지만, Set을 사용하는 방법은 `Ord`이어야 합니다.

```haskell
ghci> let setNub xs = Set.toList $ Set.fromList xs
ghci> setNub "HEY WHATS CRACKALACKIN"
" ACEHIKLNRSTWY"
ghci> nub "HEY WHATS CRACKALACKIN"
"HEY WATSCRKLIN"
```

크기가 큰 리스트에서는 `setNub`은 일반적으로 `nub`보다 빠릅니다. 하지만 위 예제에서 볼 수 있듯이 `nub`은 `setNub`과 다르게 리스트의 구성요소의 순서를 보존합니다.

## 모듈 만들기

하스켈도 다른언어와 마찬가지로 모드를 여러개의 파일로 분리할 수 있습니다. 프로그래밍을 할때 비슷한 기능을 하는 함수를 하나의 모듈로 묶어서 관리하는 것은 좋은 습관입니다. 이렇게하면 다른 모듈에서 import해서 함수를 재사용할 수 있습니다.

여기서는 기하학적 물체의 부피와 면적을 계산하는 몇가지 기능을 제공하는 모듈을 `Geometry.hs` 파일에 만들겟습니다. 그리고 모듈명은 `Geometry`로 하겠습니다. 먼저 제공할 함수들을 명시하고, 각 함수를 구현해보겠습니다.

```haskell
module Geometry  
( sphereVolume  
, sphereArea  
, cubeVolume  
, cubeArea  
, cuboidArea  
, cuboidVolume  
) where  

sphereVolume :: Float -> Float  
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)  

sphereArea :: Float -> Float  
sphereArea radius = 4 * pi * (radius ^ 2)  

cubeVolume :: Float -> Float  
cubeVolume side = cuboidVolume side side side  

cubeArea :: Float -> Float  
cubeArea side = cuboidArea side side side  

cuboidVolume :: Float -> Float -> Float -> Float  
cuboidVolume a b c = rectangleArea a b * c  

cuboidArea :: Float -> Float -> Float -> Float  
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2  

rectangleArea :: Float -> Float -> Float  
rectangleArea a b = a * b
```

꽤 표준적이 기하학이지만 유의할 점이 몇가지 있습니다. 여기서 큐브는 직육면체의 특별한 경우일 뿐이므로 모든 길이가 같은 직육면체로 처리하여 면적과 부피를 정의했습니다. 변의 길이에 따라서 사각형의 넓이를 계산하는 `rectangleArea`라는 헬퍼함수도 정의했습니다. `cuboidArea`와 `cuboidVolume` 함수에서 이 헬퍼함수를 사용했지만 export하지는 않았습니다. 왜냐하면 이 모듈에서는 3차원 객체를 다루기위한 함수를 제공하는 모듈이기 때문입니다.

```haskell
import Geometry
```

모듈을 사용하기 위해서는 먼저 import 합니다. `Geometry.hs` 파일은 import하는 프로그램과 동일한 폴더에 있어야 합니다.

모듈들은 계층적인 구조로 만들수도 잇습니다. 각 모듈은 여러개의 서브모듈을 가질 수 있고 서브모듈들은 또 자신의 서브모듈들을 가질 수 있습니다. 여기서는 `Geometry`를 세개의 서브모듈을 가지 모듈로 쪼개보도록 하겠습니다.

우선 `Geometry`라는 폴더를 만들겠습니다. 여기서 폴더의 첫글자가 대문자라는 점을 명심하세요. 이 폴더에 `Sphere.hs`, `Cuboid.hs`, `Cube.hs` 파일을 만듭니다. 그리고 각 파일을 아래와 같이 작성합니다.

`Sphere.hs`

```haskell
module Geometry.Sphere  
( volume  
, area  
) where  

volume :: Float -> Float  
volume radius = (4.0 / 3.0) * pi * (radius ^ 3)  

area :: Float -> Float  
area radius = 4 * pi * (radius ^ 2)
```

`Cuboid.hs`

```haskell
module Geometry.Cuboid  
( volume  
, area  
) where  

volume :: Float -> Float -> Float -> Float  
volume a b c = rectangleArea a b * c  

area :: Float -> Float -> Float -> Float  
area a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2  

rectangleArea :: Float -> Float -> Float  
rectangleArea a b = a * b
```

`Cube.hs`

```haskell
module Geometry.Cube  
( volume  
, area  
) where  

import qualified Geometry.Cuboid as Cuboid  

volume :: Float -> Float  
volume side = Cuboid.volume side side side  

area :: Float -> Float  
area side = Cuboid.area side side side
```

먼저 `Geometry.Sphere`는 Geometry 폴더내에 있고 모듈이름은 `Geometry.Sphere` 입니다. 다른 두 모듈에도 동일한 방식으로 처리했습니다. 또한 각 모듈에 동일한 이름의 함수가 있습니다. 각 함수는 다른 모듈에 있기때문에 동일한 이름으로 정의할 수 있습니다. 대신 특정 모듈의 함수를 가져다 쓸때는 qualified import를 사용해야 합니다.

```haskell
import qualified Geometry.Sphere as Sphere  
import qualified Geometry.Cuboid as Cuboid  
import qualified Geometry.Cube as Cube
```

위와같이 qualified import를 한 후에 `Sphere.area`, `Sphere.volume`, `Cuboid.area`와 같이 호출해서 사용할 수 있습니다.

다음 번에는 실제로 많은 기능을 가진 파일을 작성할때 공통된 용도의 기능을 확인하고 별도의 모듈로 분리해보시기 바랍니다. 그리고 동일한 기능을 필요로하는 프로그램을 작성할때 모듈을 재사용할 수 있습니다.

