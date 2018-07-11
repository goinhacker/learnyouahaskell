# 모노이드

하스켈에서 타입클래스는 공통적인 어떤 행위들을 가진 타입의 인터페이스를 표현하는데 사용됩니다. 지금까지 우리는 동일한 값인지 비교하는 타입들을 위한 `Eq`, 순서를 가진 타입의 `Ord`에서부터 좀 더 복잡한 행위들을 정의한 `Functor`나 `Applicative` 타입클래스까지 다루었습니다. 

타입을 만들때는 어떠한 행위들을 지원할지를 생각합니다. 타입이 어떤 동작을 할 수 있느냐에 따라서 어떤 타입클래스의 인스턴스로 만들지를 결정합니다. 예를들어 타입의 값들이 같은지 비교하려면, `Eq` 타입클래스의 인스턴스가 되야 합니다. 타입이 어떤 펑터의 일종이라면 `Functor`의 인스턴스가 되어야 합니다.

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

첫번째 함수인 `mempty` 는 파라메터를 받지 않기때문에 실제로는 함수가 아니고, 다형성 상수 입니다. 다형성 상수를 예를들면 `Bounded`의 `minBound` 같은 것입니다. `mempty`는 모노이드의 항등값\(identity\) 값을 나타냅니다. 

{% hint style="info" %}
`Bounded`는 하스켈에서 아래와 같이 정의되어 있습니다. 

```haskell
class  Bounded a  where
 minBound         :: a
 maxBound         :: a
```
{% endhint %}

다음으로 `mappend`는 바이너리 함수입니다. 두 값을 입력받고, 입력값들의 타입이 리턴값의 타입과 같습니다. `mappend`라는 이름은 `++` 처럼 어떤 값에 추가해야할 것 같지만, 실제로는 `*`와 같은 바이너리 함수도 있습니다. 따라서 함수명은 무시하고 두개의 모노이드 값을 받아서 다른 어떤 값을 리턴하는 바이너리 함수로 생각해야 합니다. 

마지막으로 `mconcat` 함수는 모노이드 값의 리스트\(`[m]`\)를 받아서, 리스트의 값들이 `mappend`로 줄여진 값을 반환합니다. `mconcat` 함수의 경우, 기본 구현체를 가집니다. 기본 구현체는 `foldr`에 `mappend` 함수와 `mempty`\(초기값\)를 매개변수로 호출했습니다. 대부분의 `Monoid` 인스턴스에 기본 구현체가 적합하기때문에 `mconcat`에 대해서는 크게 신경쓰지 않아도 됩니다. 따라서 `Monoid`의 인스턴스를 만들때 `mempty`와 `mappend`만 작성하는 것으로 충분합니다.  

이제 어떤 `Monoid`의 인스턴스들을 보기전에 모노이드 법칙에 대해서 간단히 살펴보겠습니다. 우리는 이미 항등값을 가져야하고, 바이너리 함수는 연관성을 가져야 한다고 이야기했습니다. 이러한 규칙을 따르지 않는 `Monoid`의 인스턴스를 만들수는 있지만, 모노이드처럼 동작하지 않을 것이기 때문에 유용하지 않습니다. 따라서 인스턴스를 만들때 아래와 같은 법칙을 따르도록 해야합니다.  

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

0을 어떤 값에서 더해도 값을 변경하지 않고, 연광성을 가진 것도 확인했습니다. 따라서 숫자가 모노이드가 되는 방법은 두가지\(`*`와 `+`\)입니다. 여기서 기억해야할 것은 타입은 여러가지 방법으로 동일한 타입클래스의 인스턴스가 될 수 있다는 것입니다. 어떤 타입을 _newtype_으로 래핑한 후에 다시 타입클래스의 인스턴스로 만들 수도 있습니다. 

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

리스트, 숫자, Boolean에서는 이미 존재하는 모노이드로 어떤 동작를 하는지 살펴본 것에 불과했습니다.

#### **그래서 대체 모노이드는 왜 쓰나요?**

 `Ordering`으로는 `Monoid` 인스턴스를 직접 만들어 보면서 모노이드에 대해서 좀 더 깊숙히 알아볼 것입니다.

```haskell
instance Monoid Ordering where  
    mempty = EQ  
    LT `mappend` _ = LT  
    EQ `mappend` y = y  
    GT `mappend` _ = GT
```

`mappend` 함수는 두개의 `Ordering` 값을 받는데, `EQ`일때는 제외하면 `mappend`의 왼쪽 매개변수만 사용했습니다. 여기서 `EQ`는 항등값입니다.  

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
lengthCompare x y = (length x `compare` length y) `mappend`
                    (x `compare` y) 
```

이 함수를 실행해보면 아래와 같이 동작합니다. 

```haskell
ghci> lengthCompare "zen" "ants"
LT
ghci> lengthCompare "zen" "ant"
GT
```

`mappend`를 사용하면 `EQ`가 아닐때 왼쪽 매개변수가 항상 결과가 되지만, 여기서는 왼쪽인 `EQ`이면  오른쪽 매개변수가 결과를 만들었습니다. 따라서 더 중요한 기준이 되는 길이 비교를 첫번째 매개변수로 하였습니다. 여기에 만약 모음자의 개수로 비교하는 함수를 추가하고, 이게 알파벳순 비교보다 중요하다면, 함수를 아래와 같이 수정하면 됩니다. 

```haskell
import Data.Monoid

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (vowels x `compare` vowels y) `mappend`
                    (x `compare` y)
    where vowels = length . filter (`elem` "aeiou")
```

여기서는 문자열을 받아서 얼마나 많은 모음자가 있는지 알려주는 헬퍼 함수 `vowels`를 만들었습니다. 이 함수는 `"aeiou"`를 포함한 문자만 필터링하고, `length`로 개수를 구하도록 구현되어 있습니다. 이제 실행해보면 아래와 같이 동작합니다. 

```haskell
ghci> lengthCompare "zen" "anna"  
LT  
ghci> lengthCompare "zen" "ana"  
LT  
ghci> lengthCompare "zen" "ann"  
GT
```

첫번째는 길이 비교, 두번째는 모음자 개수 비교, 세번째는 길이와 모음자 개수가 모두 같아서 알파벳순으로 비교되었습니다. 

**Ordering 모노이드는 여러가지 다른 기준들을 쉽게 비교할 수 있도록 해줍니다. 중요한 기준부터 순서대로 넣어서 비교할 수 있기 때문에 매우 유용합니다.** 

### `Maybe` 모노이드

이번에는 `Maybe a`를 `Monoid`의 인스턴스로 만드는 다양한 방법을 알아보고, 왜 유용한지 살펴보겠습니다. 

첫번째 방법은 타입 파라메터인 `a`가 모노이드인 경우에만, `Maybe a`를 모노이드로 취급하는 것입니다. 그리고나서 `mappend` 함수에는 `Just`로 랩핑된 값을 사용해서 구현합니다. `Nothing`을 항등값으로 사용하고, `mappend` 함수의 매개변수중 하나라도 Nothing이면 나머지 값을 그대로 유지합니다. 이 방법으로 Maybe 모노이드를 작성하면 아래와 같습니다. 

```haskell
instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    Nothing `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
```

여기서 주의할 점은 `a`는 타입 한정자에 의해서 `Monoid`의 인스턴스이어야 한다는 점입니다. 그리고 `mappend`의 두 입력값중 하나라도 `Nothing`이면, 나머지 입력값 `m`은 그대로 반환됩니다. 만약 `Just`로 래핑된 값 두개를 입력받으면, `Just`안의 두값을 `mappend`에 적용한 결과값을 `Just`로 래핑한 값이 반환됩니다. `Just`안의 어떤 값을 `Monoid`의 인스턴스로 한정했기 때문에 `m1`과 `m2`는 모노이드이고, 따라서 `mappend`의 매개변수로 넣을 수 있습니다. 

```haskell
ghci> Nothing `mappend` Just "andy"
Just "andy"
ghci> Just LT `mappend` Nothing
Just LT
ghci> Just (Sum 3) `mappend` Just (Sum 4)
Just (Sum {getSum = 7})
```

`Maybe` 모노이드는 실패할 수있는 연산의 결과를 모노이드로 처리할때 사용됩니다. 연산이 실패해도 체크하지않고 계속해서 모노이드로 사용할 수 있습니다. 

만약 `Maybe`가 가진 값이 `Monoid`가 아닐때는 어떻게 해야할까요? 이전 인스턴스 선언에서 포함된 값이 모노이드의 인스턴스이어야 하는 경우는 `mappend`의 두 매개변수가 `Just`일때 였습니다. 이 값이 모노이드라는 한정자없이 동작하는 `Maybe` 모노이드를 만드려면, 첫번째 매개변수를 고정하고 두번째 매개변수를 버리면됩니다. 먼저 첫번째 매개변수를 고정하기 위해서 `First a` 타입을 만듭니다. 

```haskell
newtype First a = First { getFirst :: Maybe a }
    deriving (Eq, Ord, Read, Show)
```

이 타입은 `Maybe a`를 받아서 _newtype_으로 래핑합니다. `Monoid`의 인스턴스는 아래와같이 작성됩니다.

```haskell
instance Monoid (First a) where
    mempty = First Nothing
    First (Just x) `mappend` _ = First (Just x)
    First Nothing `mappend` x = x
```

`mempty`는 `Nothing`을 `First` 생성자로 래핑한 것입니다. `mappend`의 첫번째 매개변수가 `Just`면 두번째 매개변수는 무시합니다. 만약 첫번째 매개변수가 `Nothing`이면 두번째 매개변수는 `Just`나 `Nothing`에 관계없이 그대로 반환합니다. 

```haskell
ghci> getFirst $ First (Just 'a') `mappend` First (Just 'b')
Just 'a'
ghci> getFirst $ First Nothing `mappend` First (Just 'b')
Just 'b'
ghci> getFirst $ First (Just 'a') `mappend` First Nothing
Just 'a'
```

여러개의 `Maybe` 중에 누가 첫번째 `Just`인지 알고싶을때, `First`와 `mconcat` 함수를 사용하여 간단하게 구현할 수 있습니다. 

```haskell
ghci> getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]
Just 9
```

만약에 `mappend`에 두개의 `Just`가 입력됐을때, 두번째 `Just` 매개변수를 유지한려면, `Data.Monoid`에서 제공하는 `Last a` 타입을 사용할 수 있습니다. `Last a`를 사용하면 `First a`와 유사하게 여러개의 `Just`중 마지막 `Just`값을 구할 수 있습니다.

```haskell
ghci> getLast . mconcat . map Last $ [Nothing, Just 9, Just 10]
Just 10
ghci> getLast $ Last (Just "one") `mappend` Last (Just "two")
Just "two"
```

### fold 자료구조에서 모노이드를 사용하기

모노이드는 다양한 데이터 구조에서 fold를 정의하는데 도움을 됩니다. 지금까지는 리스트에 대해서 fold를 배웠지만, 사실 트리와 같은 거의 모든 데이터 구조에 fold를 정의하는 것이 가능합니다. 

fold를 정의할 수 있는 많은 데이터 구조들이 있기때문에 `Foldable`이라는 타입 클래스가 존재합니다. `Functor`가 매핑을 할 수 있는 타입 클래스인 것처럼, `Foldable`은 fold라는 행위를 할 수 있습니다. `Data.Foldable`에 정의되어 있고, `Prelude`에 정의된 함수이름과 충돌나기 때문에 _qualified import_를 사용해야 합니다. 

```haskell
import qualified Foldable as F
```

여기서는 `Foldable`을 `F`라는 이름으로 _import_ 했습니다. `Foldable`은 `foldr`, `foldl`, `foldr1`, `foldl1` 함수를 가지고 있습니다. 이 함수들은 `Prelude`에도 존재하는 것으로 이전 장에서 배웠습니다. 그러면 `Foldable`의 `foldr`과 `Prelude`의 `foldr`에는 어떤 차이가 있는지 알아보겠습니다. 

```haskell
ghci> :t foldr  
foldr :: (a -> b -> b) -> b -> [a] -> b  
ghci> :t F.foldr  
F.foldr :: (F.Foldable t) => (a -> b -> b) -> b -> t a -> b
```

`foldr`은 리스트를 받아서 접는 반면에, `F.foldr`은 리스트 외에 어떤 타입이든 받아서 접을 수 있습니다. 따라서 리스트에 대해서는 두 함수의 동작은 동일합니다.

```haskell
ghci> foldr (*) 1 [1,2,3]  
6  
ghci> F.foldr (*) 1 [1,2,3]  
6 
```

그럼 이번에는 `Foldable`의 `fold` 함수들이 `Maybe`에도 동작하는지 확인해 보겠습니다. 

```haskell
ghci> F.foldl (+) 2 (Just 9)  
11  
ghci> F.foldr (||) False (Just True)  
True 
```

하지만 `Maybe`에 대해서 `fold`를 사용하는 것은 별 의미가 없습니다. 리스트에 한개의 값만 들어간 경우와 동일하기 때문입니다. 따라서 좀 더 복잡한 데이터 구조의 예를 살펴보겠습니다.

```haskell
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
```

`Tree`는 값이 없거나, 하나의 값과 두개의 다른 `Tree`를 가진 노드입니다. 위와 같이 정의하고나서 `Functor`의 인스턴스를 만들면 `fmap` 함수를 사용할 수 있습니다. 여기서는 `Foldable`의 인스턴스로 만들어서 `fold` 함수들을 사용할 수 있도록 하겠습니다. 먼저 `Foldable`의 인스턴스인 타입 생성자를 만들어서 직접 `foldr`을 구현하는 방법이 있습니다. 하지만 좀 더 쉬운 방법은 `Foldable` 타입 클래스의 포함된 `foldMap` 함수를 구현하는 것 입니다. `foldMap` 함수의 타입은 아래와 같습니다. 

```haskell
foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
```

첫번째 매개변수는 `Foldable`에 포함된 타입\(`a`\)의 값을 받아서 모노이드 값을 반환하는 함수입니다. 두번째 매개변수는 `a` 타입을 포함한 `Foldable`입니다. 이 함수는 `Foldable` 에 함수를 매핑하여 모노이드 값을 포함하는 `Foldable` 을 만듭니다. 그 다음에 두 모노이드 값들 사이에 `mappend`를 하면 하나의 모노이드 값으로 합칠 수 있습니다. `Foldable`의 인스턴스를 만들면 자연스럽게 이러한 기능을 사용할 수 있습니다. 따라서 그냥 어떤 타입에 대해서 `foldMap` 함수만 구현하면, `foldr`과 `foldl` 함수는 자동으로 해당 타입에 대해서 사용할 수 있습니다. 

`Foldable`의 인스턴스인 `Tree`를 만들면 아래와 같습니다. 

```haskell
instance F.Foldable Tree where  
    foldMap f Empty = mempty  
    foldMap f (Node x l r) = F.foldMap f l `mappend`  
                             f x           `mappend`  
                             F.foldMap f r
```

트리가 비어있다면 모노이드를 만들때 주어질 값이 없고, 하위 트리도 없기때문에 `mempty`가 됩니다. 

트리가 비어있지 않으면 값과 두개의 하위 트리가 존재하기 때문에, 재귀적으로 왼쪽/오른쪽 트리에 `f` 함수를 적용합니다. 여기서 `foldMap` 함수는 하나의 모노이드 값을 반환합니다. 또한 값 `x`에도 `f` 함수를 적용합니다. 이렇게 되면 모두 세개의 모노이드 값이 됩니다. 이제 이 값들을 하나의 모노이드 값으로 묶기 위해서 `mappend`를 사용합니다. 자연스럽게 왼쪽 하위 트리 &gt; 노드 값 &gt; 오른쪽 하위 트리, 순서로 수행됩니다. 

여기에 값을 받아서 모노이드 값을 반환하는 함수를 제공하지 않아도 됩니다. `foldMap` 함수의 매개변수로서 그 함수를 받고, 어디에 적용할지와 어떻게 하나의 모노이드로 합칠지만 고민하면 됩니다. 

이제 트리 타입의 `Foldable` 인스턴스를 만들었습니다. 그리고 `foldr`, `foldl` 함수를 사용할 수 있게 되었습니다. 

```haskell
testTree = Node 5  
            (Node 3  
                (Node 1 Empty Empty)  
                (Node 6 Empty Empty)  
            )  
            (Node 9  
                (Node 8 Empty Empty)  
                (Node 10 Empty Empty)  
            ) 
```

위와같은 트리 인스턴스를 하나 생성하면, 아래와 같이 `foldl` 함수를 테스트할 수 있습니다.

```haskell
ghci> F.foldl (+) 0 testTree  
42  
ghci> F.foldl (*) 1 testTree  
64800 
```

foldMap 함수가 Foldable의 인스턴스를 만들때만 유용한 것은 아닙니다. 어떤 구조를 하나의 모노이드 값으로 줄이는데 유용합니다. 예를 들어, 트리안에 3이 있는지 알고싶다면 아래와 같이 할 수 있습니다. 

```haskell
ghci> getAny $ F.foldMap (\x -> Any $ x == 3) testTree  
True 
```

여기서 `\x -> Any $ x == 3` 함수는 값을 받아서 `Bool`을 래핑한 `Any`라는 모노이드 값을 반환합니다. `foldMap`은 트리의 모든 구성요소에 이 함수를 적용하고, 생성된 모노이드들을 `mappend`가 있는 하나의 모노이드로 만듭니다. 

```haskell
ghci> getAny $ F.foldMap (\x -> Any $ x > 15) testTree  
False 
```

이 예제는 트리의 모든 노드에 람다 함수가 적용된 후에 `Any False` 값을 반환합니다. 여기서 결과가 `True`가 되려면 `Any`에 대한 `mappend`가 적어도 하나의 `True` 값을 매개변수로 가져야 합니다. `testTree`는 `15` 이상의 값을 가진 노드가 하나도 없기때문에 결과가 `False` 입니다.

또한 `foldMap`에 `\x -> [x]`을 적용하면 쉽게 트리를 리스트로 만들 수 있습니다. 먼저 트리의 모든 요소에 함수가 적용되면서 리스트가 되고, `mappend`에 의해서 모든 리스트가 하나의 리스트로 결합됩니다. 

```haskell
ghci> F.foldMap (\x -> [x]) testTree  
[1,3,6,5,8,9,10] 
```

이와같은 동작은 트리뿐만 아니라 `Foldable`의 모든 인스턴스에 대해서 동작합니다. 



