# 리스트 소개

리스트는 같은 타입의 구성 요소를 여러개 넣을 수 있는 자료구조 입니다.

> 아래 예제에서 `let`은 GHCI에서 이름을 정의하기 위해서 사용되는 것으로 스크립트에서는 사용되지 않는 키워드입니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command let lostNumbers = [4,8,15,16,23,42]]
**[prompt ghci> ]**[command lostNumbers]
[4,8,15,16,23,42]
```

하스켈에서 리스트는 중괄호([])로 표시하고, 값들을 콤마(,)로 구분합니다. 만약 `[1,2,’a’,3,’b’,’c’,4]`와 같은 리스트를 만들기 위해서 시도하면 하스켈에서는 char는 숫자가 아니라는 에러를 발생 시킬 것입니다. 하스켈에서 `“hello”`라는 문자열은 `[‘h’,’e’,’l’,’l’,’o’]`라는 char의 리스트입니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command [1,2,3,4] ++ [9,10,11,12]]
[1,2,3,4,9,10,11,12]
**[prompt ghci> ]**[command "hello" ++ " " ++ "world"]
"hello world"
**[prompt ghci> ]**[command ['w','o'] ++ ['o','t']]
"woot"
```

하스켈에서는 두 리스트를 합칠 때, `++` 연산자를 사용합니다. 하스켈 내부적으로는 `++` 연산을 사용하면 `++` 왼쪽의 리스트 전체를 방문하는데, 거대한 리스트를 다루는게 아니라면 문제가 없습니다. 하지만 5000만개의 데이터를 가진 리스트의 끝에 추가하려면 시간이 필요할 것 입니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command 'A':" SMALL CAT"]
"A SMALL CAT"
**[prompt ghci> ]**[command 5:[1,2,3,4,5]]
[5,1,2,3,4,5]
```

리스트의 앞에 넣을때는 `:` 연산자를 사용합니다. 이 연산자는 숫자나 숫자/문자의 리스트 또는 문자들의 리스트도 인자로 사용될 수 있습니다. 반면에 `++` 연산자는 두개의 리스트만 인자로 사용됩니다.

`[1,2,3]`이라는 리스트는 사실은 `1:2:3:[]`와 같습니다. 여기서 `[]`은 비어있는 리스트입니다. 아래와 같은 표기는 모두 다른 리스트를 의미합니다.

* [] : 빈 리스트
* [[]] : 빈 리스트 한개를 포함하는 리스트
* [[],[],[]] : 3개의 빈 리스트를 포함하는 리스트

```haskell
**[terminal]
**[prompt ghci> ]**[command "Steve Buscemi" !! 6]
'B'
**[prompt ghci> ]**[command [9.4,33.2,96.2,11.2,23.25] !! 1]
33.2
```

리스트에서 특정 인덱스의 값을 얻어올때는 `!!` 연산자를 사용합니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command let b = [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]]
**[prompt ghci> ]**[command b]
[[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
**[prompt ghci> ]**[command b ++ [[1,1,1,1]]]
[[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3],[1,1,1,1]]
**[prompt ghci> ]**[command [6,6,6]:b]
[[6,6,6],[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
**[prompt ghci> ]**[command b !! 2]
[1,2,2,3,4]
```

리스트는 리스트를 포함할 수 있습니다. 리스트안의 리스트는 다른 길이를 가질 수 있지만 다른 타입의 값을 가지고 있는 리스트들이 공존할 수는 없습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command [3,2,1] > [2,1,0]]
True
**[prompt ghci> ]**[command [3,2,1] > [2,10,100]]
True
**[prompt ghci> ]**[command [3,4,2] > [3,4]]
True
**[prompt ghci> ]**[command [3,4,2] > [2,4]]
True
**[prompt ghci> ]**[command [3,4,2] == [3,4,2]]
True
```

리스트는 리스트에 포함된 값으로 비교가 가능합니다. `<`,`<=`,`>`,`>=` 비교 연산자를 사용해서 두 리스트의 크기를 비교할 수 있습니다. 비교할때는 사전적인 순서로 비교가되고, 앞에서부터 비교가 됩니다.

하스켈에서는 리스트를 가지고 놀 수 있는 아래와 같은 기본 함수들을 제공합니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command head [5,4,3,2,1]]
5
**[prompt ghci> ]**[command tail [5,4,3,2,1]]
[4,3,2,1]
**[prompt ghci> ]**[command last [5,4,3,2,1]]
1
**[prompt ghci> ]**[command init [5,4,3,2,1]]
[5,4,3,2]
```

* `head` : 리스트의 첫번째 값을 가져옵니다.
* `tail` : 리스트의 첫번째 값을 제외한 나머지 값들의 리스트를 가져옵니다.
* `last` : 리스트의 마지막 값을 가져옵니다.
* `init` : 리스트의 마지막 값을 제외한 나머지 값들의 리스트를 가져옵니다.

리스트를 몬스터에 비유하면 아래 그림처럼 표현될 수 있습니다.

![](https://cdn-images-1.medium.com/max/1600/1*--KhDurZEmWtsvEch6Ma5A.png)



```haskell
**[terminal]
**[prompt ghci> ]**[command head []]
*** Exception: Prelude.head: empty list
```

빈 리스트에는 `head`, `tail`, `last`, `init`을 사용할 수 없습니다. 이 에러는 컴파일 타임에 잡을 수 없기때문에 위와같은 에러를 대비하는 것이 좋습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command length [5,4,3,2,1]]
5
```

`length`로 리스트의 길이를 얻을 수 있습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command null [1,2,3]]
False
**[prompt ghci> ]**[command null []]
True
```

`null`을 사용하여 리스트가 비어있는지 체크할 수 있습니다. `null` 대신 `xs == []` (`xs`가 리스트 이름이라면..)를 사용해서 체크해도 됩니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command reverse [5,4,3,2,1]]
[1,2,3,4,5]
```

`reverse`로 리스트를 뒤집을 수 있습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command take 3 [5,4,3,2,1]]
[5,4,3]
**[prompt ghci> ]**[command take 1 [3,9,3]]
[3]
**[prompt ghci> ]**[command take 5 [1,2]]
[1,2]
**[prompt ghci> ]**[command take 0 [6,6,6]]
[]
```

`take`로 리스트에서 주어진 개수 만큼만 리스트를 가져올 수 있습니다. 리스트의 크기보다 큰 개수를 요구하면 리스트 전체를 읽어오고, 0을 요구하면 빈 리스트를 반환하는 것을 확인할 수 있습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command drop 3 [8,4,2,1,5,6]]
[1,5,6]
**[prompt ghci> ]**[command drop 0 [1,2,3,4]]
[1,2,3,4]
**[prompt ghci> ]**[command drop 100 [1,2,3,4]]
[]
```

`drop`으로 리스트의 앞에서부터 주어진 개수만큼 값을 버릴 수 있습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command minimum [8,4,2,1,5,6]]
1
**[prompt ghci> ]**[command maximum [1,9,2,3,4]]
9
```

`minimum`, `maximum`을 사용하여 리스트에서 최소값, 최대값을 가져올 수 있습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command sum [5,2,1,6,3,2,5,7]]
31
**[prompt ghci> ]**[command product [6,2,1,2]]
24
**[prompt ghci> ]**[command product [1,2,5,6,7,9,2,0]]
0
```

`sum`으로 모든 리스트내 값의 합을 구할 수 있고, `product`로 모든 값을 곱을 구할 수 있습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command 4 `elem` [3,4,5,6]]
True
**[prompt ghci> ]**[command 10 `elem` [3,4,5,6]]
False
```

`elem`은 주어진 값이 리스트에 존재하는 값인지를 체크할 수 있습니다. `elem`은 일반적으로 가독성을 위해 infix 함수(인자를 양옆에 받는 방식)로 사용됩니다.

여기까지 리스트의 기초적인 함수들을 살펴보았습니다. 더 많은 함수는 이후에 다루게 됩니다.

# 리스트의 범위 설정

하스켈은 리스트를 만들때 모든 값을 입력하지 않고, 특정 범위의 값을 자동으로 만들어 주는 방법들을 학습합니다. 범위 지정자(range)를 사용하면 숫자(1-10)나 문자(a-z)같은 열거될 수 있는 타입에 대해서는 하스켈이 내부적으로 리스트의 다음에 나올 값을 예측하여 넣어줍니다.

하스켈에서는 아래 예제와 같이 범위(Range)를 지정하여 리스트를 생성할 수 있습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command [1..20]]
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
**[prompt ghci> ]**[command ['a'..'z']]
"abcdefghijklmnopqrstuvwxyz"
**[prompt ghci> ]**[command ['K'..'Z']]
"KLMNOPQRSTUVWXYZ"
```

예제와 같은 범위 지정은 열거형 타입(Number, Character)만 가능합니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command [2,4..20]]
[2,4,6,8,10,12,14,16,18,20]
**[prompt ghci> ]**[command [3,6..20]]
[3,6,9,12,15,18]
```

Range는 위 예제와 같이 증분 단위를 설정할 수 있습니다. 하지만 `[1,2,4,8,16..100]`과 같이 2²로 리스트를 만드는 등의 규칙을 만들어서 생성할 수는 없습니다. 또한 20부터 1까지의 리스트를 생성할 때는 `[20..1]`로 쓰면 안되고, `[20,19..1]`로 사용해야 합니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command [0.1, 0.3 .. 1]]
[0.1,0.3,0.5,0.7,0.8999999999999999,1.0999999999999999]
```

위와같이 Range에 실수를 사용했을때도 이상한 결과가 나오는 것을 확인하실 수 있습니다. Ragne에는 실수를 사용하지 않는 것이 좋겠습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command [1..]]
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85
```

위와 같이 Range의 최대값을 지정해주지 않으면 무한대를 의미합니다. (지면관계상 생략함) 만약 13의 배수, 24개까지 포함한 리스트를 생성한다면 아래와 같이 두가지 방법을 사용하실 수 있습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command [13,26..24*13]]
[13,26,39,52,65,78,91,104,117,130,143,156,169,182,195,208,221,234,247,260,273,286,299,312]
**[prompt ghci> ]**[command take 24 [13,26..]]
[13,26,39,52,65,78,91,104,117,130,143,156,169,182,195,208,221,234,247,260,273,286,299,312]
```

하지만 `take`를 사용하는 방법이 더 좋습니다. 왜냐하면 하스켈은 게으르기 때문입니다. 24개의 결과를 `take`를 사용하여 무한대로부터 가져와도 하스켈은 미리 계산하지 않습니다. 따라서 13을 곱하면서 리스트를 만드는 것보다 두번째 방법이 더 효율적입니다

```haskell
**[terminal]
**[prompt ghci> ]**[command take 10 (cycle [1,2,3])]
[1,2,3,1,2,3,1,2,3,1]
**[prompt ghci> ]**[command take 12 (cycle "LOL ")]
"LOL LOL LOL "
```

* `cycle` : 입력받은 리스트를 무한히 반복하는 함수 입니다. 따라서 `take` 함수같은 것을 사용하여 필요한 만큼만 얻어올 수 있습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command take 10 (repeat 5)]
[5,5,5,5,5,5,5,5,5,5]
```

* `repeat` : 입력받은 Element 한개를 무한히 반복하는 함수입니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command replicate 3 10]
[10,10,10]
```

* `replicate` : 첫번째 인자는 반복 횟수이고, 두번째는 반복할 Element 입니다. `take 3 repeat 10`이랑 같은 결과입니다.

# 리스트 정의

수학에서 Set을 정의(set comprehensions)할 때 “S = {2 • x \| x ∈ 𝐍, x ≤ 10}”와 같이 자연수에서 첫번째부터 10개의 짝수의 집합을 표현하는 것을 배운적이 있을 것입니다. 이것을 함수라고 한다면 x는 변수, N은 입력 셋, x ≤ 10은 조건이라고 할 수 있습니다. 하스켈의 리스트는 수학의 set comprehensions과 매우 유사한 방식으로 집합을 표현하는 것이 가능합니다.

지금까지 배운대로라면 자연수에서 10개의 짝수의 집합을 구하려면 아래와 같은 방법으로 얻을 수 있습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command take 10 [2,4..]]
[2,4,6,8,10,12,14,16,18,20]
```

하지만 만약 더 복잡한 함수에 다른 집합을 적용하려면 어떻게 할 수 있을까? 이때 list comprehension을 사용하실 수 있습니다. 하스켈의 list comprehension은 수학의 set comprehension과 매우 유사한데, 아래와 같은 문법으로 사용될 수 있습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command [x*2 | x <- [1..10]]]
[2,4,6,8,10,12,14,16,18,20]
```

[1..10]은 x의 범위를 나타냅니다. <- 이 표시는 대략 from으로 생각하면 될 것 같습니다. 하스켈에서는 위와 같이 수학과 비슷한 표현식으로 리스트를 정의할 수 있습니다. 이제부터 x의 범위를 결정하는 조건을 추가해보도록 하겠습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command [x*2 | x <- [1..10], x*2 >= 12]]
[12,14,16,18,20]
```

**조건식을 추가할 때는 콤마(,)로 구분**하여 추가할 수 있습니다. 이 예제에서는 x 곱하기 2가 12보다 크거나 같은 경우에 대한 조건을 추가하였습니다. 그 결과 x 곱하기 2가 12보다 크거나 같은 x만 가지고 x * 2를 수행한 것을 확인하실 수 있습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command [ x | x <- [50..100], x `mod` 7 == 3]]
[52,59,66,73,80,87,94]
```

이 예제는 50부터 100 사이의 숫자 중에 7로 나누어서 나머지가 3인 수만 리스트로 생성한 것입니다. 아직 배우지는 않았지만 추가된 조건에 대해서 내부적으로는 filter를 사용하고 있습니다.

```haskell
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
```

```haskell
**[terminal]
**[prompt ghci> ]**[command boomBangs [7..13]]
["BOOM!","BOOM!","BANG!","BANG!"]
```

입력받은 리스트의 홀수 숫자에 대해서 10보다 작으면 “BOOM!”, 10보다 크거나 같으면 “BANG”으로 변경하는 예제입니다. 여기서는 재사용을 위해서 boomBangs라는 함수에 넣어주고 실행하였습니다.

뒤에서 부터 `odd x` 조건은 `odd`라는 함수를 사용하여 홀수이면 `true` 아니면 `false`를 리턴하는 조건 식입니다. 따라서 홀수로 filtering합니다. `x <- xs`에서 `xs`는 boomBangs 함수의 입력 파라메터이므로 x의 범위는 입력으로 받은 리스트로 한정됩니다. 따라서 위 예제에서 x의 범위는 [7, 9, 11, 13]이 됩니다. `if x &lt; 10 then “BOOM!” else “BANG!”`에서는 결과를 만들어 주는 부분입니다. [7,9,11,13]에서 하나씩 꺼내와서 조건문에 수행합니다. 최종적으로 [“BOOM!”, “BOOM!”, “BANG!”, “BANG!”]이 출력된 것을 보실수 있습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]]
[10,11,12,14,16,17,18,20]
```

이 예제와 같이 **여러개의 조건을 추가할 수 있습니다.**

하스켈의 list comprehensions에서는 **여러개의 리스트들로 부터 조건식을 적용하여 filtering된 리스트들 간의 조합(combinations)으로 입력 범위를 만들 수도 있습니다. **아래 예제를 통해서 쉽게 이해할 수 있을 것입니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command [ x*y | x <- [2,5,10], y <- [8,10,11]]]
[16,20,22,40,50,55,80,100,110]
```

[2,5,10], [8,10,11] 두개의 리스트의 조합(combinations)으로 `x*y`가 수행되어 9개의 결과를 가진 리스트가 출력된 것을 확인하실 수 있습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]]
[55,80,100,110]
```

리스트에 50 이상의 결과만 출력하고 싶다면 위와 같이 조건을 추가하면 됩니다

```haskell
**[terminal]
**[prompt ghci> ]**[command let nouns = ["hobo","frog","pope"]]
**[prompt ghci> ]**[command let adjectives = ["lazy","grouchy","scheming"]]
**[prompt ghci> ]**[command [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]]
["lazy hobo","lazy frog","lazy pope","grouchy hobo","grouchy frog",  
"grouchy pope","scheming hobo","scheming frog","scheming pope"]
```

형용사 리스트와 명사 리스트를 조합한 스트링 리스트를 만들고 싶다면 위와 같이 할 수 있습니다. (cf: 궁금해서 찾아봤는데.. 아쉽게도 하스켈에서는 String interpolation을 지원하지 않네요.)

```haskell
length' xs = sum [1 | _ <- xs]
```

```haskell
**[terminal]
**[prompt ghci> ]**[command length' [1,2,3,4,5]]
5
```

`length` 함수를 직접 새로운 버전으로 구현한 예제입니다. 여기서 처음으로 `_`가 나왔습니다. **`_`의 의미는 리스트로부터 어떤것을 나오든 상관하지 않고, 사용할 일이 없는 변수명 대신 _를 사용하는 것입니다. **위 예제에서는 `_`를 사용하여 리스트의 모든 구성요소에 대해서 값에 상관없이 1로 전환하여 합계를 구하여 리스트의 길이를 구하였습니다.

```haskell
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
```

```haskell
**[terminal]
**[prompt ghci> ]**[command removeNonUppercase "Hahaha! Ahahaha!"]
"HA"
**[prompt ghci> ]**[command removeNonUppercase "IdontLIKEROGS"]
"ILIKEFROGS"
```

**하스켈에서는 문자열도 리스트이기 때문에 문자열에 대해서도 list comprehensions을 사용할 수 있습니다. **이 예제는 입력받은 문자열에서 소문자를 전부 지우는 함수를 만들어 실행해보았습니다. 여기서는 `elem` 함수를 사용하여 A부터 Z에 해당하는 문자만 남기는 방식으로 구현하였습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]]
**[prompt ghci> ]**[command [ [ x | x <- xs, even x ] | xs <- xxs]]
[[2,2,4],[2,4,6,8],[2,4,2,6,2,6]]
```

이 예제와 같이 **중첩된 리스트를 사용할때는 list comprehension을 중첩하여 사용할 수도 있습니다. **xxs에 할당된 리스트안의 리스트들에 모든 홀수값이 제거된 것을 확인할 수 있습니다. list comprehension은 ghci 환경이 아니라면 여러개의 라인으로 하용할 수 있습니다



