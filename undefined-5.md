# 함수

## 간단한 함수 만들기

`baby.hs`라는 파일 만들어 아래와 같이 입력한 후 저장합니다.

```haskell
doubleMe x = x + x
```

위에서 작성한 내용은 하스켈의 함수 입니다. `doubleMe`는 함수명이고 첫번째 x는 함수의 인자입니다. 함수를 호출할 때와 유사하게 space로 함수명과 인자를 구분합니다. `=`의 오른쪽은 함수가 할일을 정의하는 부분입니다.

```haskell
~/dev/practice/haskell$ ls
baby.hs
~/dev/practice/haskell$ ghci
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /Users/wodydjjang/.ghc/ghci.conf
Loaded GHCi configuration from /Users/wodydjjang/.ghci
ghci> :l baby
[1 of 1] Compiling Main            (baby.hs, interpreted)
Ok, modules loaded: Main.
ghci>d doubleMe 9
18
ghci> doubleMe 8.3
16.6
```

작성한 파일을 로딩하여 사용해보았습니다. ghci에서 `:`은 명령 모드입니다. `:l` 명령을 통해서 작성된 hs 파일을 로딩할 수 있습니다. 로딩 후 해당 함수를 실행하면 곱하기 2가 동작하는 것을 확인할 수 있습니다.

```haskell
ghci> doubleUs x y = x*2 + y*2
ghci> doubleUs 4 9
26
ghci> doubleUs 2.3 34.2
73.0
ghci> doubleUs 28 88 + doubleMe 123
478
```

위와같이 인자를 두개로 받는 함수를 정의해서 동작시켜 보았습니다. 다른 함수와의 조합도 간단히 수행해 봤습니다. \(하스켈의 함수가 수학의 함수와 상당히 유사하게 생기지 않았나요?\) 위에서 선언한 `doubleUs` 함수는 `doubleMe` 함수를 재사용하여 아래와 같이 선언하여 사용할 수도 있습니다.

```haskell
doubleUs x y = doubleMe x + doubleMe y
```

이 예제는 매우 간단하지만 하스켈을 사용할 때 자주 쓰이는 패턴입니다. 기본적인 함수들을 만들고 함수들의 조합으로 복잡한 함수를 만들 수 있습니다. 이런 방식으로 불필요한 중복도 피할 수 있습니다. 만약 어떤 수학자가 `doubleMe`가 곱하기 2가 아니라 3인 것을 증명했다면, 간단히 `doubleMe`를 x + x + x로 수정하여 `doubleUs`까지 함께 해결할 수 있습니다.

하스켈에서 함수들은 특정한 순서가 없습니다. 따라서 `doubleMe`를 먼저 정의하고 `doubleUs`를 하거나 또는 다른 방식으로 해도 관계가 없습니다.

```haskell
doubleSmallNumber x = if x > 100  
                        then x  
                        else x*2
```

위와 같이 `x`가 100보다 작을때만 2를 곱하는 함수를 만들었습니다. 여기서 하스켈의 `if`문을 살펴볼 수 있습니다. 위의 예제를 보면 **다른 명령형 언어와 다른 점은** `else`**구문이 반드시 존재**해야 한다는 점입니다. 명령형 언어에서는 `else`를 생략할 수 있었지만 하스켈에서는 모든 expression과 함수는 반드시 리턴을 해야합니다.

**하스켈의** `if`**문은 expression**입니다. expression은 기본적으로 어떤 값을 리턴하는 코드 조각을 의미합니다. 예를들어 `5`는 `5`라는 값을 리턴하기 때문에 expression입니다. `4 + 8, x + y`도 역시 값을 리턴하기 때문에 expression이라고 할 수 있습니다. 하스켈의 `if`문은 `else`가 생략될 수 없으므로, 항상 어떤 값을 리턴하기 때문에 expression 입니다. 만약에 `doubleSmallNumber`의 모든 결과값에 1을 더하고 싶다면 아래와 같이 수정할 수 있습니다.

```haskell
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
```

여기서 괄호를 생략하면 `x`가 100보다 클때만 1을 더하게 될 것 입니다. 여기서 함수명 뒤에 `‘`\(apostrophe\)가 있는 것을 볼 수 있습니다. 하스켈에서는 함수명에 `‘`를 사용할 수 있습니다. 함수명에 `‘`를 쓰는 경우는 게으르지 않은 함수\(isn’t lazy\)나 함수 또는 변수가 약간 수정된 버전등을 표시할 때 사용됩니다. 함수명에서 `‘`가 허용되는 문자이기 때문에 아래와 같은 함수를 만들 수도 있습니다.

```haskell
conanO'Brien = "It's a-me, Conan O'Brien!"
```

이 예제에서도 두가지 알아야 할 특성들이 있습니다. 먼저 함수 이름의 첫번째 글자가 대문자가 아니라는 점입니다. **하스켈에서는 함수의 첫번째 글자로 대문자를 허용하지 않습니다.**이 부분은 나중에 다시 살펴보겠습니다. 두번째는 어떤 인자도 없다는 점입니다. **함수가 어떤 인자도 받지 않을때, definition\(or a name\)이라고 말합니다.** 그 이유는 이름들이 가리키는 것을 한번 정의하면 바꿀 수 없기때문입니다. \(상수를 설명하는 것으로 보입니다.\)

## Pattern Matching

함수를 선언할때 여러가지 패턴으로 함수의 바디를 나누어서 심플하고 읽기 좋은 코드를 작성할 수 있습니다. 숫자, 문자, 리스트, 튜플 등과 같은 데이터 타입으로 패턴 매칭을 할 수 있습니다.

```haskell
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"
```

패턴매칭을 사용한 첫번째 예 입니다. `lucky`라는 함수가 호출됐을때 위에서부터 순서대로 매칭되는 함수의 바디를 실행합니다. 따라서 위 예제의 경우, 입력 파라메터가 7일 경우에만 첫번째 바디에 매칭되어 “LUCKY NUMBER SEVEN!”이 출력될 것입니다. 아래는 실행한 예입니다.

```haskell
ghci> lucky 5
"Sorry, you're out of luck, pal!"
ghci> lucky 7
"LUCKY NUMBER SEVEN!"
```

```haskell
sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"
```

이 예제에서는 입력값이 1 ~5이 아니면 “Not between 1 and 5”를 출력할 것입니다. 만약 마지막 패턴을 맨위로 옮기면 우선순위에 의해서 항상 “Not between 1 and 5”가 출력될 것 입니다.

```haskell
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

재귀를 이용해서 구현한 factorial 함수 입니다. 하스켈에서 재귀는 중요하고, 이 부분은 다른 챕터에서 다루도록 하겠습니다. factorial 함수에 3을 넘기면 `3 * factorial 2`, `3 * (2 * factorial 1)`, `3 * (2 * (1 * factorial 0))` 순서대로 재귀적인 계산이 수행될 것 입니다. 최종적으로 0에 매칭됐을때 1이되면서, `3 * (2 * (1 * 1))`이되어 6이 됩니다.

만약 여기서 두번째 패턴을 맨위로 올린다면 0을 포함한 모든 숫자에 매칭되어 종료되지 않을 것입니다. 따라서 패턴 매칭에서 패턴을 적는 순서는 매우 중요합니다. 이 순서는 **항상 좀 더 세부적인 패턴을 먼저 적고 일반적인 것을 나중에 적어야 합니다.**

```haskell
charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
```

만약 패턴매칭이 실패하면 아래 예제와 같은 예외가 발생하게 됩니다.

```haskell
ghci> charName 'a'
"Albert"
ghci> charName 'b'
"Broseph"
ghci> charName 'h'
"*** Exception: tut.hs:(53,0)-(55,21): Non-exhaustive patterns in function charName
```

따라서 이런 예외발생을 막기위해서는 패턴을 만들때 반드시 **모든 패턴에 걸릴 수 있는 패턴을 포함**해야 합니다.

**패턴매칭은 튜플에서도 사용**할 수 있습니다.

```haskell
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)
```

위 예제는 패턴매칭을 사용하지 않고 두개의 백터를 더하는 함수를 만든 것입니다. 벡터의 x,y를 각각 더하기 위해서 먼저 튜플의 x 컴포넌트와 y를 분리하여 더하였습니다. 잘 동작하는 함수이지만 패턴매칭을 사용하여 아래와 같이 개선할 수 있습니다.

```haskell
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
```

가독성도 개선되고, 내부에서 다른 함수를 호출하지도 않은 것을 확인할 수 있습니다. 또한 이미 모든 패턴에 대해서 처리하고 있습니다.

`fst`와 `snd` 함수는 두개의 컴포넌트를 추출하는 함수 입니다. 만약 튜플에서 세개의 컴포넌트를 추출해야한다면 어떻게 해야할 까요? 기본적으로 세개의 컴포넌트에서 추출하는 함수는 제공하지 않지만, 아래와 같이 직접 만들어서 사용할 수 있습니다.

```haskell
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z
```

이 예제에서 `_`는 리스트 정의\(list comprehensions\)내에 있는 것과 동일합니다. 쉽게말해서 `_`**는 어떤 입력이 들어오든지 상관하지 않겠다는 의미**입니다.

```haskell
ghci> let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
ghci> [a+b | (a,b) <- xs]
[4,7,6,8,11,4]
```

이 예제를 통해서 **리스트 정의내에서도 패턴매칭을 사용할 수 있음**을 확인할 수 있습니다. **만약 패턴 매칭이 실패하면, 그냥 무시하고 다음으로 넘어갑니다.**

리스트에서도 패턴 매칭이 사용될 수 있습니다. `[]` 또는 `:`를 사용하면 쉽게 리스트 패턴 매칭을 할 수 있습니다. `[1, 2, 3]`은 `1:2:3:[]`의 문법적 표현이고, 사실은 동일한 리스트를 의미합니다.`x:xs`는 리스트의 head를 `x`에 나머지 tail을 `xs`에 바인딩하게 됩니다. 만약 리스트 원소가 한개라면 `xs`는 빈 리스트로 바인딩 됩니다.

> Note: `x:xs`는 재귀함수에서 특히 많이 사용되는 패턴입니다. `:`를 포함하는 패턴은 길이가 1이상인 리스트에만 매칭될 수 있습니다.

만약 리스트의 첫번째부터 세개의 원소를 변수에 바인딩하고 나머지 전부를 변수에 바인딩하고 싶다면 `x:y:z:zs`와 같은 패턴을 사용하면 됩니다. 이 경우, 리스트는 3개 이상의 원소를 포함하고 있어야 합니다.

지금까지 배운 패턴매칭을 사용하여 `head` 함수를 구현하면 아래와 같습니다.

```haskell
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x
```

```haskell
ghci> head' [4,5,6]
4
ghci> head' "Hello"
'H'
```

위 예제에서 사용된 `_`는 실제로 어떤 것에도 바인딩하지 않습니다. 또한 여러개의 변수로 매칭하려고 할때 괄호로 묶어서 표현하는 것을 확인할 수 있습니다.

예제에서 잘못된 입력이 들어왔을때 `error` 함수를 사용했는데, `error` 함수는 문자열을 입력받아서 런타임 에러를 만듭니다. 여기서 입력받은 문자열로 발생한 에러에 대한 상세 정보를 줄 수 있습니다. `error` 함수는 프로그램의 종료를 야기시키기 때문에 남발하는 것은 좋지 않습니다.

```haskell
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y
```

위 예제의 `tell` 함수는 빈 리스트, 원소가 1개인 리스트, 원소가 2개인 리스트, 원소가 2개 이상인 리스트를 처리했기때문에 안전합니다. 여기서 `(x:[])`와 `(x:y:[])`는 각각 `[x]`, `[x,y]`로 쓸수도 있습니다. 이렇게 쓸때는 괄호는 필요 없습니다. `(x:y:_)`는 2개 이상의 어떤 리스트든 매칭되어야 하기때문에 중괄호로 작성될 수는 없습니다.

```haskell
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs
```

`length` 함수를 패턴매칭과 재귀를 사용해서 구현하면 위와 같습니다. 이 예제에서도 첫번째 패턴에서 빈 리스트를 처리하였고, 두번째 패턴에서 모든 리스트에 대해서 처리하였습니다.

```haskell
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs
```

패턴 매칭과 재귀를 사용하여 `sum` 함수를 구현한 예제입니다.

```haskell
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
```

```haskell
ghci> capital "Dracula"
"The first letter of Dracula is D"
```

이 예제에서는 패턴의 앞에 `@`를 붙이고 `all`이라는 이름을 넣은 것을 볼 수 있습니다. 여기서 `all@`을 통해서 `x:xs`를 전부 적는것보다 쉽게 리스트 전체를 받아올 수 있습니다.

**패턴 매칭안에서는** `++`**는 사용될 수 없습니다.** 만약 `(xs ++ ys)` 패턴을 쓴다면, 어떤게 첫번째고 어떤게 두번째 리스트인지 알 수 없습니다.

## Guard

가드는 어떤 값의 속성의 참, 거짓 여부를 테스트하기 위한 방법으로 `if`문을 사용하는 것과 유사합니다. 가드를 사용하면 여러개의 조건문이 필요할 때 보기 좋은 코드를 작성할 수 있습니다.

```haskell
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"
```

간단히 BMI를 입력받아서 비만 여부를 판단해서 출력해주는 함수를 작성하였습니다. 이 예제에서 가드를 사용하였습니다. **가드는 함수명과 파라메터 뒤에** `|`**를 사용해서 표시**합니다. 가드는 기본적으로 boolean 입니다. 만약 `True`면, 해당하는 함수의 바디가 사용됩니다. 만약 `False`면 다음 가드로 넘어갑니다.

```haskell
ghci> bmiTell 24.3
"You're supposedly normal. Pffft, I bet you're ugly!"
```

24.3을 입력으로 실행하면 위와 같이 정상 결과를 출력하게 됩니다. 명령형 언어의 `if else`문을 생각하면 가독성이 많이 좋아진 것을 확인할 수 있습니다. 가드는 이런 거대한 `if else`문의 좋은 대안이라고 할 수 있습니다.

마지막 가드는 대부분의 경우, `otherwise`가 있습니다. `otherwise`**는 모든 조건에 대해서 만족하는** `True`입니다. 만약 **모든 가드가 만족하지 못할때** `otherwise`**가 없다면, 다음 패턴으로 넘어가게 됩니다.** 이처럼 가드는 패턴과 함께 유용하게 활용될 수 있습니다. **만약 조건에 만족하는 가드도 없고, 매칭되는 패턴도 없다면 에러가 발생할 것 입니다.**

```haskell
bmiTell :: (RealFloat a) => a -> a -> String 
bmiTell weight height 
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!" 
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!" 
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise                 = "You're a whale, congratulations!"
```

```haskell
ghci> bmiTell 85 1.90
"You're supposedly normal. Pffft, I bet you're ugly!"
```

위 예제와 같이 여러개의 파라메터를 가드에 사용할 수도 있습니다.

가드를 쓸때는 함수명과 파라메터들 뒤에 `=`을 사용하지 않는 다는 것에 주의해야 합니다.

```haskell
max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b
```

`max` 함수를 가드를 사용해서 다시 구현한 예제입니다. 여기서 가드는 한 라인으로 작성될 수 있지만, 가독성이 떨어지기 때문에 짧은 함수라도 멀티라인으로 작성하는 것을 권장합니다.

```haskell
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT
```

```haskell
ghci> 3 `myCompare` 2
GT
```

`compare` 함수를 작성한 예제입니다. 여기서 `backtick`을 사용해도 동일하게 가드를 사용할 수 있음을 확인하실 수 있습니다.

## Where

```haskell
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                   = "You're a whale, congratulations!"
```

이전 챕터에서 가드를 사용해서 `bmiTell` 함수를 구현한 예제입니다. 여기에서 가드들의 뒤에 `where`**라는 키워드를 넣으면 다양한 이름이나 함수들을 정의할 수 있습니다.**

```haskell
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2
```

`bmiTell` 함수는 `where`를 사용하여 다시 작성하였습니다. `where`**에서 선언된 이름들은 가드들의 내부에서 사용될 수 있음**을 확인하실 수 있습니다. 이렇게 변경함으로써 우리는 중복된 값을 두번 입력할 필요가 없고, 계산식의 변경에도 한번만 변경하여 적용할 수 있습니다. 또한 이름으로 표현함으로써 가독성이 향상되고 bmi가 한번만 계산된다는 점에서 성능도 향상됩니다.

`bmiTell`**내의** `where`**에서 선언된 이름들은 함수 내에서만 사용**할 수 있습니다. 따라서 다른 네임스페이스에 영향을 주지 않습니다. 여기서 **모든 이름들은 한개의 컬럼에서 정렬되어야 합니다.**

`where`**에 선언된 이름은 서로 다른 패턴들끼리는 공유되지 않습니다.** 만약 한개의 함수에 있는 여러개의 패턴들이 공유하려면 전역으로 선언해야 합니다.

```haskell
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0
```

`where`**에서도 패턴매칭을 통한 바인딩이 가능합니다.** 위 예제는 패턴매칭을 사용해서 바인딩하는 것으로 수정한 예제입니다.

```haskell
initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname
```

이 예제는 성과 이름의 이니셜을 출력하는 함수를 `where`를 사용해서 구현한 예제입니다. 사실 바로 함수의 파라메터에 패턴매칭을 사용하면 되지만 `where`를 사용하는 것을 보여주기 위한 예제입니다.

```haskell
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2
```

지금까지는 `where`에 상수를 정의해서 사용했지만, 위 예제와 같이 함수를 정의해서 사용할 수 있습니다. `calcBmis` 함수는 몸무게와 키쌍들의 리스트를 받아서 bmi의 리스트를 반환하는 함수입니다.

`where` 바인딩도 중첩이 가능하고, 함수를 만들기 위해 자주 쓰일 수 있습니다. `where` 절에는 헬퍼 함수를 선언하고, 각 헬퍼함수는 또 자신의 `where`를 선언하여 사용합니다.

## Let

where를 사용해서 가드나 함수내에서 사용될 변수를 함수의 마지막에 정의하는 것을 보았습니다. let은 어디서나 사용될 수 있고 그 자체로 표현식\(expression\)이지만 매우 지역적이라서 가드의 범위도 넘어가지 않습니다. let 바인딩은 패턴매칭 등 하스켈의 어떤 구성에서든 값과 이름을 바인딩하기위해 사용됩니다.

```haskell
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea
```

위 예제는 높이와 반지름으로 원통의 표면적을 구하는 함수 입니다.

let의 형태는 `let <bindings> in <expression>`입니다. `<bindings>`에서는 `<expression>`에서 접근할 수 있는 이름들을 선언합니다. 위 예제의 경우는 `where` 바인딩을 사용해서 정의할 수도 있습니다. 여기서 **이름들이 주의할 것은 한개의 컬럼에 정렬되어야 한다는 것**입니다. 이 예제에서 `let`과 `where`의 차이점이 단지 `let`은 바인딩을 먼저하고 사용한다는 것입니다.

`let`과 `where`의 차이점은 `let`**은 그 자체로서 표현식\(expression\)이고,** `where`**는 문법적인 구조물이라는 점**입니다. 따라서 `let`은 표현식인 `if` 구문이나 `if else` 구문의 거의 어디에서나 사용될 수 있습니다.

```haskell
ghci> [if 5 > 3 then "Woo" else "Boo", if 'a' > 'b' then "Foo" else "Bar"]
["Woo", "Bar"]
ghci> 4 * (if 10 > 5 then 10 else 0) + 2
42
```

`let` 바인딩을 사용하면 아래와같이 사용할 수 있습니다

```haskell
ghci> 4 * (let a = 9 in a + 1) + 2
42
ghci> [let square x = x * x in (square 5, square 3, square 2)]
[(25,9,4)]
```

`let` 바인딩이 지역 범위에서 사용될 수 있는 함수 선언에도 사용될 수 있습니다.

```haskell
ghci> (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)
(6000000,"Hey there!")
```

한개 라인에 여러개의 변수를 바인딩 할때는 동일 컬럼에 정렬할 수 없고, `;`을 사용해서 바인딩이 가능합니다

```haskell
ghci> (let (a,b,c) = (1,2,3) in a+b+c) * 100
600
```

이전에도 언급했듯이 `let` 바인딩에서도 패턴매칭을 사용할 수 있습니다. 따라서 튜플을 사용한 패턴 매칭을 사용하여 바인딩하면 `;`을 사용하지 않아도 됩니다.

```haskell
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]
```

`let` 바인딩은 리스트 정의내에서도 사용될 수 있습니다. 따라서 이전 챕터에서 작성했던 `calcBmis` 함수를 `where` 대신 `let`을 사용해서 재작성할 수 있습니다.

`let`은 리스트 정의 내에서 리스트 필터링하는 동작은 하지않고 단지 이름들을 바인딩하는 역할만 수행합니다. 리스트 정의내에서 정의된 `let`은 `|` 이전의 출력 부분\(output function\)에서도 참조할 수 있습니다. 하지만 `(w, h) <- xs` 부분에서는 `let` 바인딩보다 먼저 정의되었기 때문에 사용할 수 없습니다.

리스트 정의 내에서 `let`을 사용할때는 `|` 이전 부분에서 정의되었기때문에 `in` 부분은 생략할 수 있습니다. 하지만 `predicate`에서 `let in` 바인딩을 사용할 수 있고, 이름의 범위도 `predicate`에서만 보이도록 선언할 수 있습니다.

```haskell
ghci> let zoot x y z = x * y + z
ghci> zoot 3 9 2
29
ghci> let boot x y z = x * y + z in boot 3 4 2
14
ghci> boot
<interactive>:1:0: Not in scope: `boot'
```

`in` 부분은 함수를 선언할때나 GHCi에서 직접적으로 상수를 선언할 때 생략될 수 있습니다. 또한 전역적으로 사용될 수 있도록 선언할 수도 있습니다.

만약 `let` 바인딩이 정말 좋다면 왜 항상 `where` 바인딩 대신 `let` 바인딩을 사용하지 않을까요? `let` 바인딩은 표현식\(expression\)이고 그것의 범위가 지역에 한정되기 때문에 가드들을 가로지르며 사용될 수 없습니다. 또한 함수에서 사용된 후에 이름이 오면 함수의 변수들의 이름과 타입 선언이 함수의 몸체에 가깝다는 특징이 가독성을 좋게하여 `where`가 선호되기도 합니다.

## Case

```haskell
head' :: [a] -> a  
head' [] = error "No head for empty lists!"  
head' (x:_) = x
```

```haskell
head' :: [a] -> a  
head' xs = case xs of [] -> error "No head for empty lists!"  
                      (x:_) -> x
```

위 코드를 보면 알 수 있듯이 하스켈의 `case`문은 문법만 다를 뿐, 패턴매칭과 동일합니다.

```haskell
case expression of pattern -> result  
                   pattern -> result  
                   pattern -> result  
                   ...
```

`case` 구문의 위와 같은 형태로 사용됩니다. `expressions`은 첫번째 패턴부터 차례대로 매칭되고 적합한 패턴이 없으면 런타임 에러가 발생합니다.

```haskell
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."
```

패턴매칭이 함수의 파라메터를 매칭할때만 사용될 수 있는 반면에 `case`문은 많은 곳에서 사용이 가능합니다. 위 예제처럼 표현식의 중간에 들어가서 패턴매칭을 수행할 때 유용하게 사용될 수 있습니다.

```haskell
describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."
```

만약 동일한 함수를 패턴 매칭을 이용하면 위와 같이 구현할 수 있습니다.

