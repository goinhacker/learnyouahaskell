# 모듈

**하스켈 모듈은 관련된 함수, 타입, 타입클래스들을 하나로 묶은것**입니다. **하스켈 프로그램은 모듈들의 집합**이고, 메일 모듈에서 다른 모듈들을 로딩하여 정의된 함수들을 사용하는 것입니다. 이렇게 코드를 여러개의 모듈로 분리하는 것은 여러가지 이점이 있습니다. 서로 의존성이 적은 코드\(loosely coupled\)들이 충분히 일반적인 모듈로 분리되면 **다양한 프로그램에서 재사용**할 수 있습니다. 또한 코드를 어떤 목적을 가진 부분들로 나누어서 관리할 수 있습니다.

하스켈의 표준 라이브러리는 각각 공통의 목적을 제공하는 함수와 타입들을 포함하는 모듈로 분리되어 있습니다. 여기에는 리스트를 다루는 모듈, 복자한 숫자를 다루는 모듈, 동시성 프로그래밍을 위한 모듈 등이 있습니다. 지금까지 다루었던 모든 함수, 타입, 타입클래스들은 Prelude 모듈의 일부였고, 하스켈에 기본으로 imported되어 있습니다. 이번 챕터에서는 몇가지 유용한 모듈과 여기서 제공하는 함수들에 대해서 알아보겠습니다.

하스켈은 문법적으로 `import <module name>`로 모듈들을 가져옵니다. 이 구문은 반드시 어떤 함수가 선언되기 이전에 나와야 해서 파일의 최상당에 위치하는 것이 일반적입니다. 하나의 파일에서는 여러개의 모듈을 가져올 수 있고, 라인단위로 구분해서 import문을 추가하면 됩니다. `Data.List` 모듈은 리스트를 동작시키는 유용한 함수들을 제공합니다.

```haskell
import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
```

`import Data.List`를 했을때 `Data.List`가 노출한 모든 함수들은 스크립트내 어디서든지 사용이 가능해집니다. `nub`은 리스트에 중복을 제거하는 함수로 `Data.List`에 정의되어 있습니다. `length . nub` 합성함수는 `\xs -> length (nub xs)`와 동일합니다.

```bash
ghci> :m + Data.List
```

GHCI를 사용할때는 위와 같은 방법으로 `Data.List` 모듈내 모든 함수들을 가져올 수 있습니다.

```bash
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

`Data.List` 모듈은 리스트 다루는 모든 기능을 제공합니다. 편의를 위해서 `Data.List`의 일부 함수\(`map`, `filter` 등\)들은 `Prelude` 모듈에 export되어 있습니다. 또한 `Prelude`와 `Data.List`의 함수들은 이름 충돌이 발생하지 않습니다. 여기서는 `Data.List`에서 제공하는 함수들에 대해서 살펴보겠습니다.

#### intersperse

Element와 리스트를 입력받아서, 리스트의 Element 사이사이에 입력된 Element를 넣은 리스트를 만들어 줍니다.

![](/assets/스크린샷 2017-03-08 오전 1.45.08.png)

#### intercalate

리스트들의 리스트와 리스트를 입력받아서, 리스트와 리스트 사이사이에 입력된 리스트를 넣은 리스트를 만들어 줍니다.

![](/assets/스크린샷 2017-03-08 오전 1.49.38.png)

#### transpose

리스트의 리스트를 재배열합니다. 중첩 리스트를 행렬로 본다면 열을 행으로 행을 열로 바뀐 리스트를 반환합니다.

![](/assets/스크린샷 2017-03-08 오전 1.53.19.png)

다항식 $$3x^2 + 5x + 9$$, $$10x^3 + 9$$, $$8x^3 + 5x^2 + x - 1$$를 모두 더할때, 리스트로 `[0,3,5,9]`, `[10,0,0,9]`, `[8,5,1,-1]`와 같이 표현한다면 아래와 같이 계산할 수 있습니다.

![](/assets/스크린샷 2017-03-08 오전 2.01.17.png)

위와같이 3개의 리스트를 `transpose`하면 지수를 리스트의 인덱스로 설정하여 재배열한 후, 더하면 원하는 결과를 얻을 수 있습니다.

#### foldl' && foldl1'

각각 `foldl`, `foldl1` 함수에 비해 게으르지않고 엄격한 함수 입니다. 매우 큰 리스트를 게으르게 fold할때 스택 오버플로우 에러를 종종 볼 수 있습니다. 이런 에러를 발생시키는 주범은 fold를 할때 accumulator값이 실제로 갱신되지 않아서 입니다. 실제로는 accumulator가 실제 결과\(thunk라고 불림\)가 만들어질때 그 값을 계산하는데, 이것은 모든 중간 accumulator에서 발생하고, 모든 이런 thunk들이 스택 오버플로우를 발생시킵니다. `foldl'`와 `foldl1'`는 게으른 함수가 아니라서 thunk를 스택에 쌓는대신 그때그때 중간 값을 계산합니다. 따라서 스택 오버플로우가 발생한다면 `foldl'`, `foldl1'` 함수로 전환해 볼 수 있습니다.

#### concat

리스트의 리스트를 요소들을 하나의 리스트에 넣어주는 함수입니다.

![](/assets/스크린샷 2017-03-08 오전 2.32.44.png)

이 함수는 1레벨만 중첩을 제거합니다. 따라서 리스트의 리스트의 리스트를 하나의 리스트로 만들고 싶다면 `concat`을 두번해야 합니다. 예를들어 `[[[2,3],[3,4,5],[2]],[[2,3],[3,4]]]`를 하나의 리스트로 만들기 위해서는 `concat`을 두번 호출해야 합니다.

#### concatMap

맵핑 함수와 리스트를 받아서 입력 리스트를 맵핑하여 한개의 리스트로 합쳐주는 함수입니다.

![](/assets/스크린샷 2017-03-10 오전 12.13.32.png)

#### and

boolean의 리스트의 모든 값이 참이면 `True`를 리턴하는 함수입니다.

![](/assets/스크린샷 2017-03-10 오전 12.16.27.png)

#### or

boolean의 리스트의 값중 하나라도 참이 있으면 `True`를 리턴하는 함수입니다.

![](/assets/스크린샷 2017-03-10 오전 12.20.28.png)

#### any && all

`any`는 조건문\(predicate\)과 리스트를 입력받아서 리스트의 값들중 하나라고 조건문에 참이면 `True`를 리턴하는 함수입니다.   
`all`은 조건문\(predicate\)과 리스트를 입력받아서 모든 리스트의 값들이 조건문에 참이면 `True`를 리턴하는 함수입니다.  
`any`와 `all`은 리스트의 모든 구성요소를 조건문에 확인해야할때 `and`나 `or` 대신에 사용됩니다.

![](/assets/스크린샷 2017-03-10 오전 12.25.27.png)

#### iterate

함수와 초기값을 받아서 초기값을 함수에 적용한 결과값을 다시 함수에 적용하는 것을 계속해서 반복하여 무한 리스트를 리턴하는 함수입니다.

![](/assets/스크린샷 2017-03-10 오전 2.23.30.png)

#### splitAt

숫자와 리스트를 받아서 리스트를 입력받은 숫자 위치를 기준으로 분리한 후, 튜플을 리턴하는 함수입니다.

![](/assets/스크린샷 2017-03-10 오전 2.26.56.png)

#### takeWhile

조건문\(predicate\)과 리스트를 입력받아서 주어진 조건문이 참일때까지만 가져온 리스트를 리턴하는 함수입니다.

![](/assets/스크린샷 2017-03-10 오전 2.29.33.png)

자연수의 세제곱값이 10000보다 작은 값들의 합을 구하려면 어떻게 해야 할까요?  
`[1..]`에 `^3`를 하고 filter를 쓰고 더하려고하면 무한 리스트이기 때문에 영원히 끝나지 않을 것입니다. 이 문제를 takeWhile을 사용하면 아래와 같이 구할 수 있습니다.

![](/assets/스크린샷 2017-03-10 오전 2.36.56.png)

무한 리스트에 `^3`을 적용한 값이 10000보다 크면 중지하고 모든 수를 더해서 쉽게 답을 얻을 수 있습니다.

#### dropWhile

`takeWhile`와 유사하게 조건문이 거짓일때까지의 값들일 제외한 리스트를 리턴하는 함수입니다.

![](/assets/스크린샷 2017-03-10 오전 2.42.31.png)

`(Stock, year, month, date)`로 구성된 튜플의 리스트를 받았을때, stock이 천달러를 초과하는 시점은 아래와 같이 구할 수 있습니다.

![](/assets/스크린샷 2017-03-10 오전 2.46.07.png)

#### span

`takeWhile`과 유사하지만 리스트 쌍을 리턴하는 함수입니다. 동일한 조건\(predicate\)에 동일한 리스트를 입력으로 `takeWhile`을 호출한 결과 리스트가 첫번째 리스트가 됩니다. 반대로 두번째 리스트를 `takeWhile`에 의해서 제외된 값들의 리스트입니다.

```haskell
ghci> let (fw, rest) = span (/=' ') "This is a sentence" in "First word:" ++ fw ++ ", the rest:" ++ rest  
"First word: This, the rest: is a sentence"
```

#### break

`span` 함수와 유사하지만 `break` 함수는 조건문이 첫번째 참일때 중단합니다. `break p`는 `span (not . p)`와 동일한 기능을 합니다.

![](/assets/스크린샷 2017-03-10 오전 3.06.41.png)

위 예제와 같이 `break` 함수는 두번째 리스트의 첫번째 값이 조건문을 만족하는 첫번째 값입니다.

#### sort

리스트를 정렬하는 함수입니다. 리스트내 값들의 타입클래스를 `Ord`에 포함되어야 합니다. 순서를 판단할 수 없는 값은 정렬을 할수없습니다.

![](/assets/스크린샷 2017-03-10 오전 3.10.28.png)

#### group

리스트를 받아서 동일한 값이 인접해 있으면 하위 리스트로 묶어주는 함수 입니다.

![](/assets/스크린샷 2017-03-10 오전 3.12.31.png)

만약 리스트를 그룹핑하기 전에 정렬하면 리스트에서 각 값들이 몇번 나왔는지 알 수 있습니다.

![](/assets/스크린샷 2017-03-10 오전 3.15.31.png)

#### inits && tails

#### ![](/assets/스크린샷 2017-03-11 오후 8.35.40.png)

![](/assets/스크린샷 2017-03-11 오후 8.35.49.png)



#### isInfixOf

#### 

#### ![](/assets/스크린샷 2017-03-11 오후 8.35.57.png)

#### 

#### 

#### isPrefixOf && isSuffixOf

#### ![](/assets/스크린샷 2017-03-11 오후 8.36.06.png)

#### 

#### elem && notElem

#### 

#### partition

![](/assets/스크린샷 2017-03-11 오후 8.36.13.png)

![](/assets/스크린샷 2017-03-11 오후 8.36.20.png)



#### find



![](/assets/스크린샷 2017-03-11 오후 8.36.28.png)

#### 

#### elemIndex

#### ![](/assets/스크린샷 2017-03-11 오후 8.36.40.png)

#### 

#### elemIndices

#### ![](/assets/스크린샷 2017-03-11 오후 8.36.49.png)

#### 

#### findIndex && findIndices



![](/assets/스크린샷 2017-03-11 오후 8.36.57.png)







#### zip3 && zip4 && zipWith3 && zipWith4

#### 

#### ![](/assets/스크린샷 2017-03-11 오후 8.37.05.png)

#### lines

#### ![](/assets/스크린샷 2017-03-11 오후 8.37.13.png)

#### unlines

#### ![](/assets/스크린샷 2017-03-11 오후 8.37.21.png)

#### 

#### words && unwords

#### ![](/assets/스크린샷 2017-03-11 오후 8.37.29.png)

#### 

#### nub

#### ![](/assets/스크린샷 2017-03-11 오후 8.37.36.png)

#### 

#### delete

#### ![](/assets/스크린샷 2017-03-11 오후 8.37.43.png)

#### 

#### //



![](/assets/스크린샷 2017-03-11 오후 8.38.02.png)

#### 

#### union

#### ![](/assets/스크린샷 2017-03-11 오후 9.15.32.png)

#### intersect

#### ![](/assets/스크린샷 2017-03-11 오후 9.15.41.png)

#### insert

#### ![](/assets/스크린샷 2017-03-11 오후 9.15.50.png)



![](/assets/스크린샷 2017-03-11 오후 9.15.59.png)



#### genericLength && genericTake && genericDrop && genericSplitAt && genericIndex && genericReplicate

#### 

#### 

#### nubBy && deleteBy && unionBy && intersectBy && groupBy

#### 

![](/assets/스크린샷 2017-03-11 오후 9.21.44.png)



#### on



![](/assets/스크린샷 2017-03-11 오후 9.22.37.png)



#### sortBy && insertBy && maximumBy && minimumBy

## 

## ![](/assets/스크린샷 2017-03-11 오후 9.22.47.png)

## Data.Char

#### 

#### 

#### isControl

checks whether a character is a control character.

#### isSpace

checks whether a character is a white-space characters. That includes spaces, tab characters, newlines, etc.

#### isLower

checks whether a character is lower-cased.

#### isUpper

checks whether a character is upper-cased.

#### isAlpha

checks whether a character is a letter.

#### isAlphaNum

checks whether a character is a letter or a number.

#### isPrint

checks whether a character is printable. Control characters, for instance, are not printable.

#### isDigit

checks whether a character is a digit.

#### isOctDigit

checks whether a character is an octal digit.

#### isHexDigit

checks whether a character is a hex digit.

#### isLetter

checks whether a character is a letter.

#### isMark

checks for Unicode mark characters. Those are characters that combine with preceding letters to form latters with accents. Use this if you are French.

#### isNumber

checks whether a character is numeric.

#### isPunctuation

checks whether a character is punctuation.

#### isSymbol

checks whether a character is a fancy mathematical or currency symbol.

#### isSeparator

checks for Unicode spaces and separators.

#### isAscii

checks whether a character falls into the first 128 characters of the Unicode character set.

#### isLatin1

checks whether a character falls into the first 256 characters of Unicode.

#### isAsciiUpper

checks whether a character is ASCII and upper-case.

#### isAsciiLower

checks whether a character is ASCII and lower-case.

#### 

#### ![](/assets/스크린샷 2017-03-11 오후 9.22.58.png)

#### ![](/assets/스크린샷 2017-03-11 오후 9.23.08.png)

#### ![](/assets/스크린샷 2017-03-11 오후 9.23.16.png)



![](/assets/스크린샷 2017-03-11 오후 9.23.27.png)

#### toUpper

#### toLower

#### toTitle

#### digitToInt

#### ![](/assets/스크린샷 2017-03-11 오후 9.23.36.png)

#### 

#### intToDigit

#### ![](/assets/스크린샷 2017-03-11 오후 9.23.43.png)

#### 

#### ord

## ![](/assets/스크린샷 2017-03-11 오후 9.23.50.png)

![](/assets/스크린샷 2017-03-11 오후 9.24.02.png)

![](/assets/스크린샷 2017-03-11 오후 9.24.11.png)

## Data.Map

#### fromList

#### empty

#### insert

#### null

#### size

#### singleton

#### lookup

#### member

#### map && filter

#### toList

#### keys && elems

#### fromListWith

#### insertWith

## Data.Set

#### fromList

#### intersection

#### difference

#### union

#### null && size && member && empty && singleton && insert && delete

#### map && filter

#### toList



