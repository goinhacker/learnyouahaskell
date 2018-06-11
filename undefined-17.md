# 모노이드

하스켈에서 타입클래스는 공통적인 어떤 행위들을 가진 타입의 인터페이스를 표현하는데 사용됩니다. 지금까지 우리는 동일한 값인지 비교하는 타입들을 위한 `Eq`, 순서를 가진 타입의 `Ord`에서부터 좀 더 복잡한 행위들을 정의한 `Functor`나 `Applicative` 타입클래스까지 다루었습니다. 

타입을 만들때는 어떤한 행위들을 지원할지를 생각합니다. 타입이 어떤 동작을 할 수 있느냐에 따라서 어떤 타입클래스의 인스턴스로 만들지를 결정합니다. 예를들어 타입의 값들이 같은지 비교하려면, `Eq` 타입클래스의 인스턴스가 되야 합니다. 타입이 어떤 펑터의 일종이라면 `Functor`의 인스턴스가 되어야 합니다.

`*`는 두개의 숫자를 받아서 곱하는 함수 입니다. 만약 어떤 값 `x`와 `1`을 곱하면 결과는 항상 그대로 `x` 를 반환합니다. `1 * x`나 `x * 1`, 순서는 관계없이 결과는 `x` 입니다. 유사하게 `++`는 두개의 리스트를 받아서 결합하는 함수입니다.  그리고 `*`의 1처럼 결과를 바꾸지 않는 입력으로 빈리스트 `[]` 가 있습니다. 

```haskell
ghci> 4 * 1  
4  
ghci> 1 * 9  
9  
ghci> [1,2,3] ++ []  
[1,2,3]  
ghci> [] ++ [0.5, 2.5]  
[0.5,2.5]
```

`*`의 `1`과 `++`의 `[]`는 아래와 같은 공통된 특징을 가지고 있습니다. 

* 두개의 파라메터를 입력으로 받는 함수다.
* 파라메터와 리턴값의 타입이 동일하다.
* 바이너리 함수에 사용됐을때 이미 존재하는 값을 다른 값을 변경하지 않는다.

`*`와 `++`의 또다른 공통된 특징으로 결합 법칙이 있습니다. 여러개의 값으로 하나의 값을 구할때 순서에 관계없이 결과가 동일하다는 점입니다. 즉, `(3 * 4) * 5`와 `3 * (4 * 5)`의 결과는 동일하게 `60` 입니다. `++`도 같은 특징을 가집니다. 

```haskell
ghci> (3 * 2) * (8 * 5)  
240  
ghci> 3 * (2 * (8 * 5))  
240  
ghci> "la" ++ ("di" ++ "da")  
"ladida"  
ghci> ("la" ++ "di") ++ "da"  
"ladida" 
```

이러한 특징을 연관성\(associativity\)라고 부릅니다. `*`와 `++`은 모두 연관성이 있지만, 예를들어 `-`는 `(5 - 3) - 4`와 `5 - (3 - 4)`의 결과값이 다르므로 연관성이 없습니다. 

모노이드\(monoid\)를 이해하는 것은 이러한 특징을 알고 사용하는데서 시작됩니다. 모노이드는 연관 바이너리 함수\(associative binary function\)와 이 함수의 항등값\(identity\)으로 동작하는 값\(예를들어 `1`이나 `[]`와 같은 값\)을 가질때 사용됩니다. 하스켈에는 매우 다양한 모노이드가 있습니다. 따라서 모노이드와 같은 동작을 가진 `Monoid`라는 타입클래스가 있습니다. 

```haskell
class Monoid m where  
    mempty :: m  
    mappend :: m -> m -> m  
    mconcat :: [m] -> m  
    mconcat = foldr mappend mempty
```

`Monoid`는 `import Data.Monoid`에 정의되어 있습니다.

`Monoid`를 보면 `Functor`나 `Applicative`와 다르게 `m`이 어떤 타입 파라메터도 요구하지 않습니다. 

첫번째 함수인 `mempty` 는 파라메터를 받지 않기때문에 실제로는 함수가 아니고, 다형성 상수 입니다. 다형성 상수를 예를들면 `Bounded`의 `minBound` 같은 것입니다. `mempty`는 어떤 모노이드의 항등값\(identity\) 값을 나타냅니다. 

{% hint style="info" %}
`Bounded`는 하스켈에서 아래와 같이 정의되어 있습니다. 

```haskell
class  Bounded a  where
 minBound         :: a
 maxBound         :: a
```
{% endhint %}

다음으로 `mappend`는 바이너리 함수입니다. 두 값을 입력받고, 입력값들의 타입이 리턴값의 타입과 같습니다. `mappend`라는 이름은 `++` 처럼 어떤 값에 추가해야할 것 같지만, 실제로는 `*`와 같은 바이너리 함수도 있습니다. 따라서 함수명은 무시하고 두개의 모노이드 값을 받아서 다른 어떤 값을 리턴하는 바이너리 함수로 생각해야 합니다. 

마지막으로 `mconcat` 함수는 모노이드 값의 리스트\(`[m]`\)를 받아서, 리스트의 값들이 `mappend`로 줄여진 값을 반환합니다. `mconcat` 함수의 경우, 기 구현체를 가집니다. 기본 구현체는 `foldr`에 `mappend` 함수와 `mempty`\(초기값\)를 매개변수로 호출했습니다. 대부분의 `Monoid` 인스턴스에 기본 구현체가 적합하기때문에 `mconcat`에 대해서는 크게 신경쓰지 않아도 됩니다. 따라서 `Monoid`의 인스턴스를 만들때 `mempty`와 `mappend`만 작성하는 것으로 충분합니다.  

이게 어떤 `Monoid`의 인스턴스들을 보기전에 모노이드 법칙에 대해서 간단히 살펴보겠습니다. 우리는 이미 항등값을 가져야하고, 바이너리 함수는 연관성을 가져야 한다고 이야기했습니다. 이러한 규칙을 따르지 않는 `Monoid`의 인스턴스를 만들수는 있지만, 모노이드처럼 동작하지 않을 것이기 때문에 유용하지 않습니다. 따라서 인스턴스를 만들때 아래와 같은 법칙을 따르도록 해야합니다.  

* ``mempty `mappend` x = x``
* ``x `mappend` mempty = x``
* ``(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)``

앞의 두가지 법칙은 `mempty`가 항등값으로 `mappend`에 입력됐을때, 순서에 관계없이 항상 결과가 동일해야 한다는 것을 말합니다. 그리고 세번째 법칙은 `mappend`가 연관성에 대한 법칙입니다. 즉, 여러개의 값으로 `mappend`를 사용해서 하나의 값을 줄일때 순서에 관계없이 결과가 같아야 합니다. 하스켈에서는 이러한 법칙을 따르도록 강제하지 않았기 때문에 프로그래머가 인스턴스를 만들때 주의해서 작성해야 합니다. 

### 리스트 모노이드

리스트는 모노이드입니다. `++` 함수와 빈리스트 `[]`로 모노이드가 될 수 있습니다. 

```haskell
instance Monoid [a] where  
    mempty = []  
    mappend = (++) 
```

리스트는 리스트가 가진 값의 타입에 관계없이 `Monoid` 타입클래스의 인스턴스입니다. 그리고 `Monoid`는 구체적인 타입을 기술해야하기 때문에 `instance Monoid []`가 아니라 `instance Monoid [a]`로 선언했습니다. 실제로 아래와 같이 사용해보면 아래와 같습니다. 

```haskell
ghci> [1,2,3] `mappend` [4,5,6]  
[1,2,3,4,5,6]  
ghci> ("one" `mappend` "two") `mappend` "tree"  
"onetwotree"  
ghci> "one" `mappend` ("two" `mappend` "tree")  
"onetwotree"  
ghci> "one" `mappend` "two" `mappend` "tree"  
"onetwotree"  
ghci> "pang" `mappend` mempty  
"pang"  
ghci> mconcat [[1,2],[3,6],[9]]  
[1,2,3,6,9]  
ghci> mempty :: [a]  
[] 
```

마지막 라인에서는 타입 어노테이션을 기술해 주었습니다. 그 이유는 그냥 `mempty`만 입력하면 GHCI에서 어떤 인스턴스를 사용한 것인지 알 수 없기때문입니다. 따라사 리스트의 인스턴스라는 것을 명시했습니다. 여기서 빈리스트는 어떤 타입이든 가질 수 있기때문에, `[Int]`나 `[String]`가 아닌 일반적인 타입으로 `[a]`를 명시할 수 있습니다. 

mconcat은 기본 구현체를 이미 가지고 있어서 따로 구현하지 않아도 동작합니다. 리스트의 경우, mconcat은 그냥 concat 함수와 동일합니다. 예제에서 `[[1, 2], [3, 6], [9]]`를 넣었을때, `[1, 2, 3, 6, 9]`와 같이 펼쳐서\(flatten\) 반환했습니다. 그 이유는 리스트안에서 모든 인접한 리스트간의 `++`를 수행한 것과 동일하기 때문입니다. 

모노이드 법칙은 실제 리스트 인스턴스에도 적용됩니다. 여러개의 리스트로 mappend\(or ++\)를 호출해도 순서에 관계없이 최종 결과는 동일합니다. 그리고 빈리스트에 대해서 기존 리스트를 변경하지않고, 그대로 반환합니다. 여기서 주의할 점은 모노이드가 `a mappend b`가 `b mappend a`와 같을 필요는 없다는 것입니다. 리스트의 경우에 아래와 같이 확인할 수 있습니다. 

```haskell
ghci> "one" `mappend` "two"  
"onetwo"  
ghci> "two" `mappend` "one"  
"twoone"
```

이전에 살펴보았던 `*`의 경우는 `3 * 5`와 `5 * 3`의 결과가 같았지만, 모든 모노이드에 대해서 적용되는게 아니라는 것을 확인할 수 있습니다. 

### `Product`와 `Sum`

우리는 이미 숫자에 대해서 모노이드인 `*`를 확인해봤습니다. `*`는 바이너리 함수이고, 항등값\(identity value\)는 `1`입니다. 그리고 또다른 바이너리 함수로 `+`가 있고, 항등값은 `0`입니다. 

```haskell
ghci> 0 + 4  
4  
ghci> 5 + 0  
5  
ghci> (1 + 3) + 5  
9  
ghci> 1 + (3 + 5)  
9 
```

0을 어떤 값에서 더해도 값을 변경하지 않고, 연광성을 가진 것도 확인했습니다. 따라서 숫자가 모노이드가 되는 방법은 두가지\(`*`와 `+`\)입니다. 여기서 기억해야할 것은 어떤 타입은 여러가지 방법으로 동일한 타입클래스의 인스턴스가 될 수 있다는 것입니다. 어떤 타입을 _newtype_으로 래핑한 후에 다시 타입클래스의 인스턴스로 만들 수도 있습니다. 

`Data.Monoid` 모듈은 `Product`와 `Sum`이라는 타입을 가지고 있습니다. 그중 `Product`는 아래와 같이 정의되어 있습니다. 

```haskell
newtype Product a =  Product { getProduct :: a }  
    deriving (Eq, Ord, Read, Show, Bounded) 
```

간단하게 몇가지 타입클래스의 인스턴스로서 하나의 타입 파라메터를 받는 _newtype_으로 래핑되었습니다. 이 타입을 모노이드의 인스턴스로 만들면 아래와 같습니다. 

```haskell
instance Num a => Monoid (Product a) where  
    mempty = Product 1  
    Product x `mappend` Product y = Product (x * y)
```

`mempty`는 `Product` 생성자에 `1`을 넣어서 래핑했습니다. `mappend`는 `Product`가 가진 두 숫자를 곱하고, 다시  `Product`로 래핑했습니다. 그리고 여기서 `a`는 `Num a` 클래스 한정자에 의해서 `Num`의 인스턴스이어야 합니다. 이제 `Product a`를 모노이드로 사용하면 아래와 같습니다. 

```haskell
ghci> getProduct $ Product 3 `mappend` Product 9  
27  
ghci> getProduct $ Product 3 `mappend` mempty  
3  
ghci> getProduct $ Product 3 `mappend` Product 4 `mappend` Product 2  
24  
ghci> getProduct . mconcat . map Product $ [3,4,2]  
24
```

이 예제는 모노이드 타입클래스의 예를 보여주지만, 그렇다고 `3 * 9`와 `3 * 1`을 대신해서 사용하지는 않을 것입니다. 모노이드 인스턴스의 장점은 뒤에서 살펴볼 것 입니다. 

`Sum`은 `Product`와 유사하게 정의하면 아래와 같이 동일한 방식으로 사용할 수 있습니다. 

```haskell
ghci> getSum $ Sum 2 `mappend` Sum 9  
11  
ghci> getSum $ mempty `mappend` Sum 3  
3  
ghci> getSum . mconcat . map Sum $ [1,2,3]  
6 
```

### `Any`와 `All`

모노이드의 동작을 할 수 있는 또다른 예를 보겠습니다. 첫번째로 `||` \(or 함수\)를 보겠습니다. or 함수는 바이너리 함수이고, 항등값으로 `False`가 있습니다. 함수의 두 입력중 하나라도  `True`가 있으면  `True`를 반환하므로, 기존 값이 `False`이든 `True`이든 `Flase`와 or 하면 기존 값을 그대로 반환하게 됩니다. `Any` _newtype_ 생성자는 이런 기능의 `Monoid` 인스턴스 입니다.

```haskell
newtype Any = Any { getAny :: Bool }  
    deriving (Eq, Ord, Read, Show, Bounded)
```

`Monoid`의 인스턴스로 정의하면 아래와 같습니다. 

```haskell
instance Monoid Any where  
        mempty = Any False  
        Any x `mappend` Any y = Any (x || y)
```

`Any`를 세개 이상 `mappend`해도 하나라도 `True`가 있으면 `True`가 됩니다. 

```haskell
ghci> getAny $ Any True `mappend` Any False  
True  
ghci> getAny $ mempty `mappend` Any True  
True  
ghci> getAny . mconcat . map Any $ [False, False, False, True]  
True  
ghci> getAny $ mempty `mappend` mempty  
False
```

이번에는 반대로 `&&` \(and 함수\)를 `Monoid`의 인스턴스로 만들어 보겠습니다. &&도 바이너리 함수이고, True가 항등원이 됩니다. 그리고 모든 매개변수가 True일때만 True를 반환합니다. 아래와 같이 정의됩니다. 

```haskell
newtype All = All { getAll :: Bool }  
        deriving (Eq, Ord, Read, Show, Bounded)
```

```haskell
instance Monoid All where  
        mempty = All True  
        All x `mappend` All y = All (x && y)
```

`All` 타입의 `mappend`는 모든 값이 `True`일때만, `True`를 반환합니다.

```haskell
ghci> getAll $ mempty `mappend` All True  
True  
ghci> getAll $ mempty `mappend` All False  
False  
ghci> getAll . mconcat . map All $ [True, True, True]  
True  
ghci> getAll . mconcat . map All $ [True, True, False]  
False
```

`Any`와 `All`에 대해서 `mconcat`이 유용해 보이지만, 일반적으로는 `Bool`의 리스트를 받아서 `or`와 `and` 함수를 사용하는 것이 더 간단합니다. 

### `Ordering` 모노이드

Ordering은 어떤 값들을 비교해서 LT, EQ, GT, 세가지 값이 될 수 있는 타입입니다. 

```haskell
ghci> 1 `compare` 2  
LT  
ghci> 2 `compare` 2  
EQ  
ghci> 3 `compare` 2  
GT 
```

리스트, 숫자, Boolean에서는 이미 존재하는 모노이드로 어떤 동작를 하는지 살펴본 것에 불과했습니다. `Ordering`으로는 `Monoid` 인스턴스를 직접 만들어 보면서 모노이드에 대해서 좀 더 깊숙히 알아볼 것입니다.

```haskell
instance Monoid Ordering where  
    mempty = EQ  
    LT `mappend` _ = LT  
    EQ `mappend` y = y  
    GT `mappend` _ = GT
```

`mappend` 함수는 두개의 `Ordering` 값을 받는데, `EQ`일때는 제외하면 `mappend`의 왼쪽 매개변수만 사용했습니다. 여기서 `EQ`는 항등값입니다. 

만약 두 단어를 알파벳순으로 비교하는 한다면, 처음 두 문자가 같으면 다음 두 문자를 비교하는 과정을 반복할 것입니다. 예를들어 "ox"와 "on"을 비교한다면, 첫번째 문자가 같으므로 두번째 문자를 비교합니다. 'x'가 'n'보다는 크므로 "ox"가 더 큽니다. 여기에 "oix"와 "oin"과 같이 동일한 위치에 동일한 문자를 추가해도 순서는 바뀌지 않습니다. 

여기서 주의해야할 점은 ``x `mappend` y``와 ``y `mappend` x``가 같지 않다는 것입니다. EQ가 아닐때는 왼쪽 매개변수만 유지하기 때문에, ``LT `mappend` GT``는 `LT`이고 ``GT `mappend` LT``는 `GT`가 됩니다. 

```haskell
ghci> LT `mappend` GT
LT
ghci> GT `mappend` LT
GT
ghci> mempty `mappend` LT
LT
ghci> mempty `mappend` GT
GT
```

그럼 이제 모노이드가 어떻게 유용한지 살펴보기 위해서 두 문자열을 받아서 길이를 비교해서 Ordering을 반환하는 함수를 작성해보겠습니다. 단, 만약에 문자열의 길이가 같으면, EQ를 반환하는 대신에 사전 순서로 비교합니다.  이 함수는 아래와 같이 작성될 수 있습니다.

```haskell
lengthCompare :: String -> String -> Ordering
lengthCompare x y = let a = length x `compare` length y
                        b = x `compare` y
                    in  if a == EQ then b else a
```

여기서 `a`는 길이를 비교하고, `b`는 알파벳 순으로 비교합니다. 따라서 `if a == EQ then b else a`에서 `a`의 결과가 `EQ`이면 `b`를 수행해서 최종 결과를 반환합니다. 

모노이드 `Ordering`을 사용하면 아래와 같이 재작성될 수 있습니다. 

```haskell
import Data.Monoid
lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x compare length y) mappend
                    (x compare y)
```









































