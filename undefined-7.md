# 고계 함수

## 고계함수\(Higher order functions\)란?

**하스켈의 함수들은 함수를 인자로 받거나 함수를 리턴값으로 반환하는 것이 가능합니다. 그리고 함수를 매개변수로 받거나 함수를 리턴하는 함수들을 고계함수\(Higher Order Functions\)라고 부릅니다. **명령형 언어에서 상태를 변경하고 반복문을 사용하는 등 단계별로 정의하는 것을 대신하려면 고계함수가 필수적입니다. 함수형 언어에서 고계함수는 프로그래밍 문제를 푸는 강력한 방법입니다.

## **커링 함수\(Curried functions\)**

**커링\(Currying\)이란 인자를 여러개 받는 함수를 분리하여, 인자를 하나 받는 함수의 체인으로 만드는 방법입니다. 하스켈에서 모든 함수는 일급객체이고 curried되어 있기때문에, 모든 함수가 한개의 인자만 받을 수 있습니다. **하스켈에서 여러개의 인자를 받는 모든 함수들은 실제로는 curried functions 입니다. 커링을 사용하면 **partial application**을 작성할 수 있도록 해주어 편리합니다. **Partial application은 커링으로 쪼개진 부분 함수들을 조립하여 어플리케이션을 작성하는 방식입니다.**

### max 함수

```haskell
ghci> max 4 5
5
ghci> (max 4) 5
5
```

max 함수를 보면 마치 두 개의 인자를 받아서 큰 값을 반환하는 것 처럼 보입니다. 하지만 실제로는 `max 4 5`를 실행하면 먼저 한 개의 인자를 받고 새로운 인자를 받아 두 인자중 더 큰 값을 반환하는 함수가 만들어 집니다. 즉, 인자를 한개만 받는 함수의 체인으로 커링됩니다.

위 두개의 호출은 정확히 동일합니다. 두개의 인자 사이에 공백\(space\)를 넣는 것은 함수의 간단한 기능일 뿐입니다. 여기서 공백\(space\)은 일종의 우선순위가 가장 높은 연산자와 같은 것입니다. max의 타입을 살펴보면 아래와 같습니다.

`max :: (Ord a) => a -> a -> a`

그리고 아래와 같이 작성해도 동일합니다.

`max :: (Ord a) => a -> (a -> a)`

이것을 해석하면 함수 max는 a를 인자로 받고, a를 받아서 a를 반환하는 함수\(a -&gt; a\)를 반환한다라고 할 수 있습니다.

그럼 이렇게 하는 것은 어떤 이점이 있을까요? 만약 입력 인자가 적은 함수를 호출하면 **partially applied** 함수를 돌려받는데, 이 partial application을 사용하면, 다른 함수에 넘기거나 데이터안에 넣을 수 있는 함수를 바로 생성할 수 있습니다.

### multThree 함수

```haskell
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z
```

`multThree 3 5 9` 또는 `((multThree 3) 5) 9` 호출하면 실제로는 무슨일이 벌어질 까요? 먼저 `multiThree`에 3이 적용되어 한개의 인자를 입력받는 함수를 리턴하게 됩니다. 이렇게 반환된 함수에 5가 적용되어 함수를 반환하고, 다시 이 함수에 9가 적용되어 135가 됩니다. 위의 타입은 아래와 같이 선언될 수도 있습니다.

```haskell
multiThree :: (Num a) => a -> (a -> (a ->a))
```

이 경우, 함수는 a를 입력 받아서 타입이 `(Num a) => a -> (a -> a)`인 함수를 리턴하는 것입니다. 이 함수는 다시 a를 입력받아서 타입이 `(Num a) => a -> a`인 함수를 리턴하고, 마지막으로 이 함수는 a를 입력받아서 a를 리턴하게 됩니다.

```haskell
ghci> let multTwoWithNine = multThree 9
ghci> multTwoWithNine 2 3
54
ghci> let multWithEighteen = multTwoWithNine 2
ghci> multWithEighteen 10
180
```

위 예제와 같이 인자가 적은 함수를 호출하여 새로운 함수를 만들고 재사용할 수 있습니다.

### compareWithHundred 함수

compareWithHundread 함수는 어떤 숫자를 받아서 100과 비교하는 함수입니다.

```haskell
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x
```

여기서 `compare 100`은 숫자 한개를 받아서 그것을 100과 비교하는 함수를 리턴하게 될 것입니다. 따라서 아래와 같이 재작성될 수 있습니다.

```haskell
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100
```

`compare 100`도 함수를 리턴하기 때문에 타입은 동일합니다. compare의 타입은 `(Ord a) => a -> (a -> Ordering)`이고, 100을 입력받아서 호출되면 `(Num a, Ord a) => a -> Ordering`을 리턴하게 됩니다. 여기서 100의 타입클래스가 Num이기 때문에 타입제한자에 Num이 추가되었습니다.

### 중위\(Infix\) 함수

중위\(Infix\) 함수들은 섹션을 사용해서 부분적으로 적용될 수 있습니다. **섹션을 사용하는 것은 간단하게 괄호로 묶어서 한쪽 인자를 제공**하는 것입니다.

```haskell
divideByTen :: (Floating a) => a -> a
divideByTen = (10)
```

이 함수에서 `divideByTen 200`을 호출하면 `(10) 200`이 실행되어 `200 / 10`과 동일합니다. 만약 중위 함수로 입력 문자가 대문자인지 확인하는 함수를 만든다면 아래와 같습니다.

```haskell
isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])
```

섹션에 대해서 `-`를 사용할 때는 주의해야 하는데, `(-4)`는 빼기 4를 의미하는 것입니다. 숫자 4를 빼는 것은 `(subtract 4)`를 사용해야 합니다.

### applyTwice 함수

함수는 함수를 인자로 받고, 함수를 리턴할 수 있습니다.

```haskell
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
```

함수를 인자로 받기 위해서는 applyTwice 함수의 타입처럼 괄호로 묶어야 합니다. 따라서 `(a -> a)`가 첫번째 인자, a를 두번째 인자로 받고 a를 리턴하는 함수 입니다.

함수의 구현부에서는 인자 f를 함수에 x를 인자로 넣어 사용한 후, 그 결과를 다시 f 함수의 입력 인자로 사용하였습니다. 이 함수를 사용해보면 아래와 같은 결과를 얻을 수 있습니다.

```haskell
ghci> applyTwice (+3) 10
16
ghci> applyTwice (++ " HAHA") "HEY"
"HEY HAHA HAHA"
ghci> applyTwice ("HAHA " ++) "HEY"
"HAHA HAHA HEY"
ghci> applyTwice (multThree 2 2) 9
144
ghci> applyTwice (3:) [1]  
[3,3,1]
```

여기서 partial application이 얼마나 유용한지 확인할 수 있습니다. 만약 인자가 한개인 함수가 필요하다면, 해당 별도의 인자를 입력하지 않고, 함수만 인자로 넣어주기만 하면 됩니다.

### zipwith 함수

zipWith 함수는 하나의 함수와 두개의 리스트를 받아서 두개의 리스트에서 상응하는 요소들을 입력된 함수에 적용하여 합치는 함수 입니다.

```haskell
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
```

첫번째 인자`(a -> b -> c)`는 두개의 인자 a, b를 입력 받아서 c를 리턴하는 함수 입니다. 여기서 a, b, c는 모두 같은 타입이어도 관계 없습니다. 즉, 타입이 `a -> a -> a`인 함수도 수용된다는 것입니다.

두번째 인자\(\[a\]\)는 a의 리스트 입니다. 여기서 a는 첫번째 인자로 들어온 함수의 첫번째 인자인 a와 같아야 합니다.

세번째 인자\(\[b\]\)는 b의 리스트 입니다. 여기서 b는 첫번째 인자로 들어온 함수의 두번째 인자인 b와 같아야 합니다.

함수를 만들때 특히, 고계함수를 만들때는 타입에 대한 확인이 없으면, 타입 선언을 생략할 수 있습니다. 그리고 나서 :t를 사용하여 함수의 타입을 확인할 수 있습니다.

zipWith 함수의 구현은 일반적인 zip 함수와 비슷한데, 종료조건은 zip 함수와 동일합니다. 차이점이 있다면 인자로 joining function을 받는다는 점입니다. 하지만 이 함수는 종료조건과는 관계가 없기 때문에 `_`를 사용하였습니다. 마지막 패턴도 `(x ,y)` 대신 `f x y`를 사용한 것을 제외하면 동일합니다.

충분히 일반적인 고계 함수는 zipWith 함수의 예와 같이 여러가지 다른 작업들을 할 수 있습니다. zipWith 함수를 실행해 보면 아래와 같이 동작합니다.

```haskell
ghci> zipWith' (+) [4,2,5,6] [2,6,2,3]
[6,8,7,9]
ghci> zipWith' max [6,3,2,1] [7,3,1,5]
[7,3,2,5]
ghci> zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]
["foo fighters","bar hoppers","baz aldrin"]
ghci> zipWith' (*) (replicate 5 2) [1..]
[2,4,6,8,10]
ghci> zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]
[[3,4,6],[9,20,30],[10,12,12]]
```

위 사용 예와 같이 고계 함수는 매우 융통성있게 사용될 수 있습니다. 명령형 언어에서는 보통 for, while, 변수에 값을 세팅하거나 상태를 체크하는 등의 방법을 사용하여 어떤 동작을 완성하고 인터페이스로 만듭니다. 함수형 프로그래밍에서는 두개의 리스트의 쌍을 검사하고 이 쌍에 어떤 것을 하거나 어떤 솔루션의 집합을 얻어서 필요 없는 것을 제거하는 것과 같은** 일반적인 패턴을 추상화하기 위해서 고계 함수를 사용**합니다.

### flip 함수

flip 함수는 하나의 함수를 입력 받아서 입력받은 함수의 첫번째 두 인자만 바뀐 함수를 리턴하는 함수 입니다.

```haskell
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x
```

함수의 타입을 보면 a, b를 인자로 받는 함수를 받아서 b, a를 인자로 받는 함수를 리턴하는 함수인 것을 알 수 있습니다. 하지만 하스켈의 함수들은 기본적으로 커링이기 때문에 두번째 괄호는 불필요합니다. 왜냐하면 `->`는 기본적으로 오른쪽 방향으로 결합하기 때문입니다. 즉, 아래 타입은 모두 같은 타입니다.

`(a -> b -> c) -> (b -> a -> c)`

`(a -> b -> c) -> (b -> (a -> c))`

`(a -> b -> c) -> b -> a -> c`

따라서 flip 함수는 아래와 같이 간단하게 재작성 될 수 있습니다.

```haskell
flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y
```

여기서 모든 함수가 curried라는 사실을 이용합니다. y, x 인자 없이 `flip' f`가 호출됐을 때, 이 두개의 인자가 교체된 함수 f를 리턴할 것 입니다. 인자가 교체된 함수는 일반적으로 다른 함수로 전달되지만, 미리 모든 호출이 끝났을때의 최종 결과를 무엇인지 먼저 생각하고 작성하면 고계함수를 만들때 커링을 장점을 이용할 수 있습니다.

```haskell
ghci> flip' zip [1,2,3,4,5] "hello"
[('h',1),('e',2),('l',3),('l',4),('o',5)]
ghci> zipWith (flip' div) [2,2..] [10,8,6,4,2]
[5,4,3,2,1]
```

### map 함수

map 함수는 함수와 리스트를 입력 받아 리스트내 모든 요소에 함수를 적용 시키는 함수 입니다.

```haskell
 map :: (a -> b) -> [a] -> [b]
 map _ [] = []
 map f (x:xs) = f x : map f xs
```

타입 선언을 통해서 a를 받아서 b를 리턴하는 함수와 a의 리스트를 입력으로 받아서 b의 리스트를 리턴하는 것을 알 수 있습니다. 함수 타입만 보아도 함수가 어떤 일을 하는지 대략적으로 알 수 있습니다. map 함수는 다양하고 유용하게 활용될 수 있는 고계 함수의 예 입니다.

```haskell
ghci> map (+3) [1,5,3,1,6]
[4,8,6,4,9]
ghci> map (++ "!") ["BIFF", "BANG", "POW"]
["BIFF!","BANG!","POW!"]
ghci> map (replicate 3) [3..6]
[[3,3,3],[4,4,4],[5,5,5],[6,6,6]]
ghci> map (map (^2)) [[1,2],[3,4,5,6],[7,8]]
[[1,4],[9,16,25,36],[49,64]]
ghci> map fst [(1,2),(3,5),(6,3),(2,6),(2,5)]
[1,3,6,2,2]
```

map 함수는 리스트 정의\(list comprehension\)으로도 동일한 기능의 함수를 표현할 수 있습니다. 예를들어 `map (+3) [1,5,3,1,6]`을 리스트 정의로 표현하면 `[x + 3 | x <- [1,5,3,1,6]]`이 됩니다. 하지만 단순히 리스트의 요소들에 함수를 적용하는 경우라면 map 함수를 사용하는 것이 더 가독성이 좋습니다.

### filter 함수

filter 함수는 predicate\(boolean을 리턴하는 함수\)와 리스트를 입력받아서, 리스트의 구성요소중 predicate를 만족하는 요소들의 리스트를 리턴합니다.

```haskell
 filter :: (a -> Bool) -> [a] -> [a]
 filter _ [] = []
 filter p (x:xs)
     | p x       = x : filter p xs
     | otherwise = filter p xs
```

만약 `p x`가 `True`로 평가되면 해당 x를 새로운 리스트에 추가하고, 아니면 추가하지 않습니다.

```haskell
ghci> filter (>3) [1,5,3,2,1,6,4,3,2,1]
[5,6,4]
ghci> filter (==3) [1,2,3,4,5]
[3]
ghci> filter even [1..10]
[2,4,6,8,10]
ghci> let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]
[[1,2,3],[3,4,5],[2,2]]
ghci> filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"
"uagameasadifeent"
ghci> filter (`elem` ['A'..'Z']) "i lauGh At You BecAuse u r aLL the Same"
"GAYBALLS"
```

filter 함수도 리스트 정의로 만들 수 있습니다. filter는 리스트 정의안에 있는 여러가지 predicate들과 같습니다. 리스트 정의에서는 여러개의 filter를 적용하거나 `&&` 등을 사용하여 조합할 수도있습니다. 상황에 따라서 filter를 쓰는 것과 리스트 정의 중 가독성이 좋은 것을 선택합니다.

### quicksort 함수

```haskell
 quicksort :: (Ord a) => [a] -> [a]
 quicksort [] = []
 quicksort (x:xs) = 
     let smallerSorted = quicksort (filter (<=x) xs)
         biggerSorted = quicksort (filter (>x) xs)
     in  smallerSorted ++ [x] ++ biggerSorted
```

이전 챕터에서는 리스트 정의를 이용해서 구현했던 quicksort 함수를 가독성이 좋은 filter를 사용하여 재작성한 것입니다.

### largestDivisible 함수

largestDivisible 함수는 100,000 이하의 숫자중 3829로 나누어 떨어지는 최대값을 구하는 함수입니다.

```haskell
 largestDivisible :: (Intergral a) => a
 largestDivisible = head (filter p [100000, 99999..])
     where p x = x `mod` 3829 == 0
```

우선 100,000 이하의 숫자들의 리스트를 만들고, 우리의 predicate인 `p`의 조건에 맞는 숫자들로 필터링 하였습니다. 내림차순으로 정렬되어있기 때문에 필터링된 리스트의 첫번째 값이 가장 큰 수입니다. 여기서 게으르게 평가되는 특성때문에 초기 입력 리스트를 유한한 리스트로 만들 필요가 없습니다. 왜냐하면 필터링된 리스트의 첫번째 값을 만나면 값을 평가하고 끝나기 때문입니다.

### takeWhile 함수

takeWhile 함수는 predicate와 리스트를 입력 받아서 리스트의 시작에서부터 predicate가 true인 모든 요소의 리스트를 리턴하는 함수입니다. 예를들어 `"elephants know how to party"`의 첫번째 단어를 받으려면 `takeWhile (/=' ') "elephants know how to party"`와 같이 사용하면 `elephants`를 리턴받을 수 있습니다.

10,000보다 작은 홀수 제곱 수의 합을 구하려면 어떻게 해야할까요?

```haskell
ghci> sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
166650
```

위와 같이 `[1..]`을 `(^2)`로 맵핑하고, odd 함수로 필터링한 리스트에서 `takeWhile (<10000)`을 사용하여 모든 홀수 제곱수를 구한 후, sum 함수로 합계를 구할 수 있습니다. 이것을 리스트 정의를 사용하여 구현하면 아래와 같습니다.

```haskell
ghci> sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])
166650
```

이렇게 무한 리스트를 맵핑하고 필터링하는 것이 가능한 이유는 하스켈의 게으른 특성 때문입니다. map이나 filter가 즉시 적용되지 않고 실제로 값이 필요할때만 평가됩니다.

### chain 함수

chain 함수는 자연수 한개를 받아서 짝수면 2로 나누고, 홀수면 3을 곱하고 1을 더하는 것을 반복하여 자연수가 1이 되어 종료할때 까지의 숫자들의 리스트를 리턴하는 함수 입니다. 만약 13으로 시작한다면 아래와 같이 계산되어 갑니다.

`13, 13*3+1(40), 40/2(20), 20/2(10), 10/2(5), 5*3+1(16), 8, 4, 2, 1`

```haskell
 chain :: (Integral a) => a -> [a]
 chain 1 = [1]
 chain n
     | even n = n:chain (n `div` 2)
     | odd n = n:chain (n*3 + 1)
```

```haskell
ghci> chain 10
[10,5,16,8,4,2,1]
ghci> chain 1
[1]
ghci> chain 30
[30,15,46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1]
```

### numLongChains 함수

numLongChains함수는 1과 100 사이의 모든 숫자를 chain 함수에 돌려서 리스트의 사이즈가 15보다 큰 경우의 수를 리턴하는 함수 입니다.

```haskell
  numLongChains :: Int
  numLongChains = length (filter isLong (map chain [1..100]))
      where isLong xs = length xs > 15
```

`[1..100]`의 값을 chain 함수로 맵핑하고 그중 리스트의 길이가 15보다 큰 것만 필터링하여 length 함수로 길이를 구하였습니다.

### listOfFuns 함수

```haskell
ghci> let listOfFuns = map (*) [0..]
ghci> (listOfFuns !! 4) 5
20
```

listOfFuns 함수의 예를 보면 map 함수를 통해서 `map (*2) [0..]`를 호출한 것과 같이 어떤 값의 리스트를 받는 것 뿐만 아니라, `map (*) [0..]`를 호출하여 `[(0*),(1*),(2*),(3*)..]`와 같은 함수들의 리스트를 받을 수도 있습니다. 이렇게 정의된 listOfFuns 함수는 한개의 인자를 받는 함수들의 리스트를 리턴하게됩니다. 이때 타입은 `(Num a) => [a -> a]`입니다.

`listOfFuns !! 4`와 같이 실행하면 listOfFuns 함수의 4번째 인덱스에 있는 `(4*)`를 리턴하게 됩니다. 따라서 위의 예제에서 `(4*) 5`가 `4 * 5`가 되어 최종 `20`을 리턴하게 됩니다.

## 람다함수

람다함수들은 기본적으로 단 한번만 필요할때 사용되는 이름이 없는 함수입니다. 일반적으로 우리는 고계함수에 넘기기 위한 목적으로 람다함수를 만듭니다. 람다함수를 만들기 위해서는 `\`를 먼저 적고, 스페이스에 의해서 분리된 인자들을 적습니다. 그 다음에 `->`를 적고 함수의 구현부를 작성합니다. 대개 람다함수는 오른쪽으로 확장되기 때문에 괄호로 묶어줍니다.

아래 예제는 numLongChains을 where를 사용하지 않고, 람다함수를 사용하여 재 구현한 것입니다.

```haskell
numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15)(map chain [1..100]))
```

람다함수는 표현식\(expression\)이기 때문에 위와 같이 사용될 수 있습니다. `(\ws -> length xs > 15)`는 길이가 15보다 큰지를 알려주는 함수를 리턴합니다.

커링이나 partial application의 동작을 제대로 이해하지 못한 사람들은 종종 람다함수를 필요없는 곳에 사용합니다. 예를들어, `map (+3) [1,6,3,2]`와 `map (/x -> x + 3) [1,6,3,2]`는 `(+3)`과 `(\x -> x + 3)`이 동일한 함수이기 때문에 람다를 사용할 필요가 없습니다. 이 경우, partial application을 사용하는 것이 훨씬 가독성이 좋습니다.

```haskell
ghci> zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
[153.0,61.5,31.0,15.75,6.6]
```

zipWith의 예와 같이 일반 함수와 동일하게 람다함수도 여러개의 인자를 받을 수 있습니다.

```haskell
ghci> map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]
[3,8,9,8,7]
```

일반함수와 마찬가지로 람다에서도 패턴매칭을 할 수 있습니다. 유일한 차이점은 한개의 인자에 여러개의 패턴을 정의할 수 없다는 점 입니다. 예를들어 한개의 인자에 `[]`와 `[x:xs]`, 두개의 패턴을 만들어 값을 가질 수 없습니다. 만약 람다함수안에서 매칭이 실패하면 런터임 에러가 발생하므로 주의해서 사용해야 합니다.

```haskell
addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z
```

```haskell
addThree :: (Num a) => a -> a -> a -> a
addThree = \x -> \y -> \z -> x + y + z
```

일반적으로 람다함수는 오른쪽으로 확장되지 않게하기 위해서 괄호로 묶습니다. 위의 두 함수는 함수가 기본적으로 커링으로 처리되기 때문에 동일한 함수가 됩니다. 위의 경우는 람다로 쓰는 것은 가독성이 좋지 않습니다.

```haskell
flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x
```

위의 flip 함수의 경우는 커링을 이용한 람다로 표시하는 것이 더 가독성이 좋습니다. `flip' f x y = f y x`로 작성하는 것보다 새로운 함수의 생성을 위해서 사용된다는 것이 분명해집니다. flip 함수는 보통 map이나 filter에 두개의 인자가 교체된 함수를 넘기는 용도로 사용됩니다. 따라서 함수가 부분적으로 적용되거나 함수의 인자로 사용된다면 람다를 사용하는 것이 좋습니다.

## fold 함수

이전 챕터에서 재귀에 대해서 이야기 하면서 리스트를 다루면서 재귀를 사용하는 함수들의 예를 살펴보았습니다. 보통은 빈리스트를 종료 조건으로 하였고, `x:xs` 패턴으로 리스트이 첫번째 요소와 나머지로 나누어서 처리하였습니다. 이런 일반적인 패턴을 캡슐화하기 위한 함수들로 fold 함수가 있습니다. fold 함수들은 map 함수의 일종으로 리스트를 어떤 단일 값으로 줄여주는 함수 입니다.

fold는 이진함수, 초기값\(accumulator라고 부릅니다.\), 줄일 대상 리스트를 입력 받습니다. 이진함수는 accumulator와 리스트의 첫번째\(또는 마지막\) 요소와 함께 호출되고, 새로운 accumulator를 리턴합니다. 그리고나서 이진함수는 새로운 accumulator와 줄어진 리스트의 첫번째\(또는 마지막\) 요소 등과 함께 다시 호출됩니다. 모든 리스트를 이렇게 한번 순회하고 나면 초기 리스트로 부터 줄여진 단일값, accumulator만 남게됩니다.

### foldl 함수

foldl 함수는 리스트를 왼쪽에서부터 줄이기때문에 left fold라고 부릅니다. 이진함수는 초기값과 리스트의 head 사이에 적용되어 새로운 accumulator를 생성합니다. 그리고나서 새로운 accumulator와 입력 리스트의 다음 요소와 함께 다시 호출됩니다. foldl 함수를 사용해서 sum 함수를 다시 구현하면 아래와 같습니다.

```haskell
  sum' :: (Num a) => [a] -> a
  sum' xs = foldl (\acc x -> acc + x) 0 xs
```

```haskell
ghci> sum' [3,5,2,1]
11
```

`\acc x -> acc + x`는 이진함수 입니다. `0`은 초기값이고, `xs`는 줄일 대상 리스트입니다. `0`은 이진함수 안에서 `acc` 인자로 사용됩니다. `sum' [3,5,2,1]`로 호출했을때 `x`는 3이 됩니다. 이후 로직은 아래와 같이 진행됩니다.

`acc: 0, xs: [3,5,2,1]`  
`acc: 0 + 3, xs: [5,2,1]`  
`acc: 3 + 5, xs: [2,1]`  
`acc: 8 + 2, xs: [1]`  
`acc: 10 + 1, x: []`  
`acc: 11`

최종적으로 리스트의 왼쪽에서부터 acc를 통해서 11로 줄여지는 과정을 확인할 수 있습니다.  
만약 여기서 커링을 활용한다면 아래와 같이 더 간결하게 작성될 수 있습니다.

```haskell
 sum' :: (Num a) => [a] -> a
 sum' = foldl (+) 0
```

람다함수인 `(\acc x -> acc + x)`는 `(+)`와 동일합니다. 그리고 `foldl (+) 0`은 리스트를 받는 함수를 리턴하기 때문에 `xs`인자는 생략할 수 있습니다. 일반적으로 만약 `foo a = bar b a`와 같은 함수는 커링때문에 `foo = bar b`로 재작성될 수 있습니다.

리스트에 해당 요소가 존재하는지 체크하는 `elem` 함수를 foldl을 사용해서 구현하면 아래와 같습니다.

```haskell
 elem' :: (Eq a) => a -> [a] -> Bool
 elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys
```

이 예제에서는 초기값과 accumulator가 boolean 입니다. **fold를 다룰때는 accumulator와 최종 결과값의 타입은 항상 동일합니다.** 만약 초기값을 무엇으로 해야할지 모르겠다면 일단 리스트에 해당 값이 없다고 가정하고 False로 하는게 좋습니다. 이렇게하면 빈리스트가 들어왔을때, 최종 결과를 초기값으로 쓸 수 있습니다. 그 다음에 현재 값이 존재하는지 확인합니다. 만약 있다면 acc를 True로 변경합니다. 만약 없다면 acc는 변경하지 않고 남겨둡니다.

### foldr 함수

right fold인, foldr 함수는 오른쪽에서부터 accumulator로 값을 줄여나갑니다. 또한 foldl 함수와는 반대로 이진함수는 첫번째 인자로 현재 값을 받고 두번째 인자로 accumulator를 받습니다\(`\x acc -> ...`\).

fold의 accumulator값\(최종 결과값도 마찬가지로..\)은 숫자, boolean 또는 리스트 등 어떤 타입이든 될 수 있습니다. map 함수를 foldr 함수를 이용해서 구현하면 accumulator는 리스트가 될 것 입니다. 그리고 초기값을 빈리스트가 됩니다.

```haskell
 map' :: (a -> b) -> [a] -> [b]
 map' f xs = foldr (\x acc -> f x : acc) [] xs
```

만약 `[1,2,3]`을 `(+3)`로 맵핑한다면, 우리는 오른쪽에서부터 리스트를 접근하면 아래와 같이 진행될 것입니다.

`acc: [], xs: [1,2,3]`  
`acc: 3 + 3:[], xs: [1,2]`  
`acc: 2 + 3:6:[], xs: [1]`  
`acc: 1 + 3:5:6:[], xs: []`  
`result: [4,5,6]`

map 함수는 아래와 같이 foldl 함수로도 구현할 수 있습니다.

`map' f xs = foldl (\acc x -> acc ++ [f x]) [] xs`

하지만 이경우, `++` 함수가 `:`에 비해서 비용이 매우 큽니다. 따라서 보통 **리스트를 새로운 리스트로 만들때는 right fold를 사용**합니다.

만약 리스트를 뒤짚는다면, left fold로 했을때와 같이 right fold를 할 수도 있고, 그 반대도 가능합니다. `sum` 함수는 left와 right fold 모두 거의 동일하게 구현될 수 있습니다. 한가지 큰 차이점은 **right fold는 무한 리스트에서 동작**한다는 점입니다. 명백하게 말하자면, 무한한 리스트를 어느 시점에서 가져 와서 오른쪽에서 접으면 결국 목록의 시작 부분에 도달하게됩니다. 하지만 반대로 왼쪽에서 접으면 영영 끝나지 않을 것입니다.

**fold 함수들은 리스트를 한번 순회하고 이것을 기반으로 어떤 것을 리턴하는 함수를 구현하는 경우는 언제든지 사용될 수 있습니다.** 이러한 이유로 함수형 프로그래밍에서 가장 유용한 함수인 map과 filter에서는 fold가 사용됩니다.

### foldl1과 foldr1 함수

foldl1과 foldr1 함수는 초기값이 없다는 점을 제외하고는 foldl, foldr 함수와 동일하게 동작합니다. 이 함수들은 리스트의 첫번째\(또는 마지막\) 요소를 초기값을 가정하고 시작합니다. 예를들어 `sum` 함수의 경우 `sum = foldl1 (+)`로 구현될 수 있습니다. `foldl1`과 `foldr1` 함수의 경우는 입력 리스트에 적어도 한개의 요소가 있어야 동작이 가능하기 때문에, 빈리스트가 들어오면 런타임 에러가 발생합니다. 반대로 `foldl`과 `foldr` 함수는 빈리스트가 들어와도 정상적으로 동작합니다. 따라서 빈리스트가 입력으로 들어오지 않을 경우에는 `foldl1`이나 `foldr1`을 사용할 수 있습니다.

표준 라이브러리에 있는 함수들을 fold를 사용해서 구현하면 아래와 같습니다.

```haskell
 maximum' :: (Ord a) => [a] -> a
 maximum' = foldr1 (\x acc -> if x > acc then x else acc)

 reverse' :: [a] -> [a]
 reverse' = foldl (\acc x -> x : acc) []

 product' :: (Num a) => [a] -> a
 product' = foldr1 (*)

 filter' :: (a -> Bool) -> [a] -> [a]
 filter' p = foldr (\x acc -> if p x then x : acc else acc) []

 head' :: [a] -> a
 head' = foldr1 (\x _ -> x)

 last' :: [a] -> a
 last' = foldl1 (\_ x -> x)
```

`head` 함수의 경우는 패턴매칭에 의한 더 나은 구현방법이 있지만, 여기서는 fold를 사용하는 것을 보여주기 위한 예제로 구현했습니다.  
`reverse'` 함수의 경우, `\acc x -> x : acc`는 단지 인자들이 뒤짚히는 `:` 함수 처럼 보입니다. 따라서 `foldl (flip (:)) []`와 같이 작성될 수도 있습니다. 다른 방법으로 right, left fold의 동작을 살펴보면 아래와 같습니다.  
right fold에서 이진함수를 f, 초기값을 z일때, `[3,4,5,6]`을 입력받으면 `f 3 (f 4 (f 5 (f 6 z)))`와 같이 수행됩니다. 만약 함수 f를 `+`로 하고, 초기값을 0으로 하면, `3 + (4 + (5 + (6 + 0)))`이 됩니다. 또는 만약 `+`를 prefix 함수로 작성하면 `(+) 3 ((+) 4 ((+) 5))`가 됩니다.  
left fold에서 이진함수를 g, accumulator를 z라고 하면 `g (g (g (g z 3) 4) 5) 6`가 됩니다. 만약 이진함수를 `flip (:)`로 하고 accumulator를 `[]`로 하면 `flip (:) (flip (:) (flip (:) (flip (:) [] 3) 4) 5) 6`가 됩니다. 그리고 최종 평가되는 값은 `6,5,4,3`입니다.

### scanl, scanr 함수

scanl, scanr 함수는 foldl과 foldr과 같이 수행되면서, 중간 중간에 accumulator값을 리스트 형태로 보여줍니다. foldl1과 foldr1과 유사한 scanl1과 scanr1 함수 역시 있습니다.

```haskell
ghci> scanl (+) 0 [3,5,2,1]
[0,3,8,10,11]
ghci> scanr (+) 0 [3,5,2,1]
[11,8,3,1,0]
ghci> scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]
[3,4,5,5,7,9,9,9]
ghci> scanl (flip (:)) [] [3,2,1]
[[],[3],[2,3],[1,2,3]]
```

scanl을 사용할때는 최종결과값이 리스트의 마지막에 있고, scanr을 사용할때는 리스트의 처음에 있습니다.  
Scan은 fold 함수로 구현될 수있는 함수의 진행과정을 모니터링하기위해서 사용됩니다. 모든 자연수의 제곱근의 합이 1000을 넘으려면 몇개의 자연수가 필요할까요? 일단 모든 자연수의 제곱을 구하려면 `map sqrt [1..]`를 하면 됩니다. 여기서 합계를 구하려면 fold를 사용해야 합니다. 하지만 여기서는 합계가 아니라 합계가 구해지는 과정에 관심이 있기 때문에 scan을 사용해야 합니다. scan이 완료되면 1000이하에서 얼마나 많은 자연수의 합이 필요했는지 볼 수 있습니다. scan 리스트에 첫번째 합계는 1이고, 두번째는 1 + 2의 제곱근 입니다. 세번째는 + 3의 제곱근이 됩니다. 만약 합계가 1000이하가 나올때 숫자가 X라면 1000을 초과할때는 X + 1개의 자연수가 필요합니다.

```haskell
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
```

```haskell
ghci> sqrtSums
131
ghci> sum (map sqrt [1..131])
1005.0942035344083
ghci> sum (map sqrt [1..130])
993.6486803921487
```

여기서는 filter가 무한 리스트에서는 동작할 수 없기때문에 takeWhile을 사용하였습니다. 리스트가 오름차순이라는 것을 알고 있어도 filter는 사용할 수 없습니다. 그래서 1000보다 큰 합계가 처음 발생한 시점에 takeWhile을 사용하여 끊었습니다.

## Function application with $

여기서는 function application이라고 불리는 `$`함수에 대해서 살펴봅니다.

```haskell
($) :: (a -> b) -> a -> b
f $ x = f x
```

이 예제에서 `$`는 불필요한 연사자처럼 보입니다. 하지만 보통의 function application\(함수와 인자 사이에 공백을 넣는것\)은 우선순위가 높은 반면에 `$`** 함수는 가장 낮은 우선순위**를 가지고 있습니다. `f a b c`와 같이 공백을 사용한 function application은 `(((f a) b) c)`와 같이 왼쪽으로 연관지어 지어집니다. `$`**을 사용한 function application은 반대로 오른쪽으로 연관**지어 집니다.

그렇다면 `$`은 언제 사용될까요? **주로 많은 괄호를 사용하지 않고 작성하는데 도움**을 줍니다. 예를들어, `sum (map sqrt [1..130])`는 `sum $ map sqrt [1..130]`과 같이 괄호를 생략하여 재작성될 수 있습니다. 따라서 `$`를 만나면 오른쪽에 있는 것들이 왼쪽에 있는 함수의 인자로서 적용됩니다.

다른 예로, `sqrt 3 + 4 + 9`의 경우, 9, 4와 3의 제곱근이 한꺼번에 추가됩니다. 만약 여기서 의 제곱근을 얻으려면 `sqrt (3 + 4 + 9)` 또는 `sqrt $ 3 + 4 + 9`와 같이 재작성할 수 있습니다. 이렇게 되는 이유는 `$` 연산자의 우선순위가 가장 낮기 때문입니다.

`$`는 오른쪽으로 연관짓기 때문에 `f (g (z x))`가 `f $ g $ z x`와 같습니다. 따라서 `sum (filter (> 10) (map (*2) [2..10]))`는 `sum $ filter (> 10) $ map (*2) [2..10]`로 재작성할 수 있습니다.

```haskell
ghci> map ($ 3) [(4+), (10*), (^2), sqrt]
[7.0,30.0,9.0,1.7320508075688772]
```

이 예제와 같이 `$`는 괄호를 제거하는 것 외에도 마치 다른 함수처럼 취급될 수도 있습니다. 여기서는 함수의 리스트에 map 함수로 사용된 것을 확인할 수 있습니다.

## Function Composition

수학에서 Function Composition을 와 같이 표현하고, 는 g함수가 x를 인자로 호출된 결과를 f함수의 인자로 호출한 결과와 같습니다.  
하스켈에서 Function Composition도 매우 비슷합니다. 하스켈에서는 `.`을 사용하여 표현하고 아래와 정의될 수 있습니다.

```haskell
 (.) :: (b -> c) -> (a -> b) -> a -> c
 f . g = \x -> f (g x)
```

여기서 타입 선언에 주목해야합니다. `f`의 인자 값은 `g`의 리턴값과 같은 타입이어야 합니다. 따라서 `f . g`는 g의 인자와 동일한 타입의 인자를 받아서 f의 리턴값과 동일한 타입의 인자를 리턴합니다. `negate . (* 3)`은 3이 곱해진 숫자를 인자로 받고 그것을 음수로 바꾸는 함수를 리턴합니다.

Function Composition은 다른 함수에 바로 넘기기위한 함수를 만드는데도 사용합니다. 물론 람다함수를 넘길 수도 있지만, 많은 경우 Function Composition 이 더 명확하고 간결합니다.

어떤 숫자들의 리스트를 가지고 있고, 리스트내 모든 숫자를 음수로 바꾸는 함수는 어떻게 만들 수 있을까요?

```haskell
ghci> map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
[-5,-3,-6,-7,-3,-2,-19,-24]
```

한가지 방법은 위와 같이 각 숫자의 절대값을 음수로 만드는 것입니다. 이 예제에서는 람다함수를 사용하였습니다.

```haskell
ghci> map (negate . abs) [5,-3,-6,7,-3,2,-19,24]
[-5,-3,-6,-7,-3,-2,-19,-24]
```

만약 Function Composition을 사용하면 위와같이 재작성 될 수 있습니다. 따라서 여러개의 함수를 위와 같이 연결하여 구성할 수 있습니다. `f (g (z x))`는 `(f . g . z) x`와 동일합니다. 예를들어

```haskell
ghci> map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]
[-14,-15,-27]
```

를

```haskell
ghci> map (negate . sum . tail) [[1..5],[3..6],[1..7]]
[-14,-15,-27]
```

로 바꿀 수 있습니다.

만약 함수들이 여러개의 인자를 받아야 한다면 어떻게 해야할까요? 만약 이것을 Function Composition을 써서 작성하려면 한개의 인자만 받는 함수를 부분 적용해야만 합니다. `sum (replicate 5) (max 6.7 8.9)`는 `(sum . replicate 5 . max 6.7) 8.9`나 `sum . replicate 5 . max 6.7 $ 8.9`로 재작성 될 수 있습니다. 이렇게 하면 아래와 같이 동작합니다.

1. 함수가 생성될때 `max 6.7` 함수를 받아서 `repliacate 5`에 적용합니다. 
2. 결과를 받아서 그것의 합을 생성합니다. 
3. 마지막에 이 함수가 8.9를 인자로 호출됩니다. 

만약, Function Composition을 쓸때 많은 괄호를 없애기를 원한다면 다른 모든 함수들의 연결뒤에 `$`를 붙여서 재작성할 수 있습니다. 따라서 `replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))`은 `replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8]`로 재작성됩니다. 여기서 Functional Composition을 사용해서 세개의 괄호가 세개의 .으로 대체된 것을 확인할 수 있습니다.

Function Composition이 주로 사용되는 다른 예로는 point가 없는 함수\(point free style\)를 정의하는 것입니다.

```haskell
 sum' :: (Num a) => [a] -> a
 sum' xs = foldl (+) 0 xs
```

여기서 sum 함수의 양쪽에 `xs`가 있습니다. 하지만 커링을 쓰면 `foldl (+) 0`으로 동일한 기능을 합니다. `sum' = foldl (+) 0`와 같이 작성하는 것을 point free style이라고 부릅니다. 아래 예제는 어떻게 하면 point free style로 작성할 수 있을까요?

```haskell
 fn x = ceiling (negate (tan (cos (max 50 x))))
```

여기서는 양쪽에 있는 `x`를 제거할 수 없습니다. `cos (max 50)`와 같이하면 함수의 코사인을 얻을 수가 없습니다. 여기서 composition을 쓰면 아래와 같이 작성되어 x를 제거될 수 있습니다.

```haskell
 fn = ceiling . negate . tan . cos . max 50
```

많은 경우에 point free style은 코드를 좀 더 가독성이 좋고 간결하게 합니다. 데이터와 어떻게 섞을지를 고민하는 것 대신에 함수에 대해서 생각하고, 어떤 종류의 함수가 그 함수들의 결과들과 결합하면 될지를 생각하게 합니다. Composition을 사용하면 단순한 함수들의 조합들로 더 복잡한 함수를 만들 수 있습니다. 하지만 대다수의 경우 너무 복잡한 함수를 point free style 함수로 작성하면 가독성이 떨어집니다. 가장 선호되는 스타일은 _let_을 사용해서 중간 결과값들을에 이름을 달아놓고 사용하거나 하나의 거대한 체인을 만드는 대신 문제를 작은 문제로 쪼개서 함수를 구성하여 가독성을 좋게 만들어야 합니다.

이전에 map과 filter 챕터에서 10,000보다 작은 모든 홀수 제곱의 합을 찾는 문제는 풀었습니다. 이 문제는 아래와 같이 리팩토링 될 수 있습니다.

```haskell
 oddSquareSum :: Integer
 oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
```

Function Composition을 사용하면 아래와 같이 재작성 됩니다.

```haskell
 oddSquareSum :: Integer
 oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]
```

하지만 가독성을 고려하면 아래와 같이 작성합니다.

```haskell
 oddSquareSum :: Integer
 oddSquareSum = 
     let oddSquares = filter odd $ map (^2) [1..]
         belowLimit = takeWhile (<10000) oddSquares
     in sum belowLimit
```

