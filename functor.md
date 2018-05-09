# Functor

하스켈의 순수성, 고차함수, 매개변수화된 대수적 데이터 타입, 타입클래스의 조합은 다른 언어보다 훨씬 고차원의 다형성을 구현 가능하게 합니다. 타입들이 가진 거대한 계층 구조에 대해서 생각할 필요가 없습니다.\(마치 Java의 객체의 계층구조와 같은...\) 대신 어떤 타입이 어떤 타입클래스와 연결되어있고, 어떤 동작들을 할 수 있는지 알고있으면 됩니다. 예를들어 `Int`는 값이 같은지 비교하거나, 순서를 정하는 등의 동작들을 할 수 있습니다.

타입클래스를 사용해서 자신만의 타입을 정의할 수 있습니다. 정의하려는 타입을 어떤 타입클래스에 연결하는지에 따라서 해당 타입의 동작이 정해집니다. 하스켈은 강력한 타입시스템으로 인해 타입 선언만으로도 함수에 대해서 많은 것을 알 수 있어서, 매우 일반적이고 추상적인 동작을 정의한 타입클래스를 만들 수 있습니다. 예를들어 값이 같은지 확인하거나 순서를 비교할 수 있는 타입클래스는 매우 추상화된 동작입니다. 하지만 항상 사용해왔기때문에 특별하게 느껴지지 않습니다. 이전에 살펴본 펑터도 매핑할 수 있는 것이라는 동작을 가진 타입클래스 입니다. 펑터도 유용하면서도 매우 추상적인 타입클래스를 설명해주는 예입니다. 이번 챕터에서는 이 펑터에 대해서 자세히 알아보겠습니다. 또한 펑터의 좀더 유용하고 강력한 버전인 실용적인 펑터\(Applicative Functors\)와 모노이드\(Monoids\)에 대해서도 살펴볼 것 입니다.

## Functor

우리는 이미 [타입클래스 챕터](https://jaeyongcho.gitbooks.io/learnyouahaskell/content/d0c0-c785-d074-b798-c2a4.html)에서 펑터에 대해서 공부했습니다. 기억이 안난다면 간단히 복습해볼 것을 권장합니다.

간단히 리마인드 해보면, 펑터는 `List`, `Maybe`, `Tree`와 같이 맵핑할 수 있는 타입의 타입클래스입니다. `Functor` 타입클래스는 `fmap`이라는 함수 한개를 가집니다. `fmap` 함수의 타입은 `fmap :: (a -> b) -> f a -> f b` 입니다. 타입의 의미는 `a`를 받아서 `b`를 반환하는 함수와 `a` 한개 또는 여러개를 담은 박스를 받아서 `b`를 한개 또는 여러개 담은 박스를 반환합니다. 박스안에 아이템은 입력받은 함수가 적용됩니다.

> 주의: 직관적으로 펑터가 하는 일에 대한 이해를 돕기위해서, 주로 박스에 비유합니다. 뒤에서 배울 실용적인 펑터나 모나드를 설명할때도 박스를 사용할 것 입니다. 사람들이 펑터를 처음 이해하기에 박스는 괜찮은 비유지만 그것을 그대로 받아들여선 안됩니다. 왜냐하면 모든 펑터에 대해서 적합한 비유는 아니기때문입니다. 펑터의 좀 더 정확한 단어는 computational context 입니다. 이 context에서 computation은 `Maybe`나 `Either a`처럼 값을 가지거나 실패했을 수도 있고, 리스트처럼 여러개의 값을 가질 수 있는 것 입니다.

타입 생성자를 `Functor`의 인스턴스로 만들고 싶다면, kind는 `* -> *`가 됩니다. 즉, 하나의 구체적인 타입만 타입 파라메터로 받는다는 것 입니다. 예를들어 `Maybe`는 인스턴스로 만들 수 있습니다. 왜냐하면 `Maybe Int`나 `Maybe String`처럼 구체적인 타입으로 만들기 위해서 하나의 타입 파라메터만 필요하기 때문입니다. 만약 `Either`처럼 두개의 파라메터가 필요하면, 타입 파라메터가 한개가 될때까지 부분적용해야 합니다. 따라서 `instance Functor Either where`와 같이 작성할 수 없고, `instance Functor (Either a) where`와 같이 작성해야 합니다. 그리고나서 `Either a`만을 위한 `fmap` 함수를 선언하면 `fmap :: (b -> c) -> Either a b -> Either a c`가 됩니다. 타입 선언에서 알 수 있듯이 `Either a` 부분은 고정되어 있습니다. 왜냐하면 `Either a`는 한개의 타입 파라메터만 받는 반면에 `Either`는 두개를 받기 때문입니다. 따라서 `fmap :: (b -> c) -> Either b -> Either c`와 같이 작성할 순 없습니다.

지금부터 얼마나 많은 타입\(실제로는 타입 생성자\)들이 `[]`, `Maybe`, `Either a`, `Tree`처럼 `Functor`의 인스턴스인지 살펴보겠습니다. 먼저 `IO`와 `(->) r`이라 불리는 펑터의 인스턴스에 대해서 알아보겠습니다.

만약 어떤 값이 `IO String` 타입이면, 외부에서 문자열을 가져오는 IO 작업이라는 의미입니다. `IO String`의 결과를 어떤 이름에 바인딩하기 위해서는 _do_안에서 `<-`를 사용해야 합니다. 이전에 I/O 작업은 바깥 세상에 있는 박스에서 어떤 값을 꺼내서 가져오는 것이라고 비유하였습니다. I/O 작업내에서 값에 대한 검증을 할수도 있지만, 검증 후에 다시 `IO`에 값을 넣어서 반환해야 합니다. 이와같은 `IO`의 동작도 펑터같은 방식으로 동작하는 것을 볼 수 있습니다.

이제 `IO`가 어떻게 `Functor`의 인스턴스가 되는지 살펴보겠습니다. I/O 작업을 통해서 `fmap` 함수를 수행할때, I/O 작업의 결과값을 `fmap` 함수에 적용한 상태로 I/O 작업으로 재포장하여 돌려받게 됩니다.

```haskell
instance Functor IO where  
    fmap f action = do  
        result <- action  
        return (f result)
```

I/O 작업을 매핑한 결과값이 다시 I/O 작업이 되어야 하기 때문에 _do_ 구문내에 두개의 작업을 묶었습니다. `fmap`의 구현부에서 기존 I/O 작업 `action`을 수행하고 결과값 `result`를 `fmap` 함수에 적용하였습니다. `return (f result)`에서 `return`은 어떤 동작도 하지만 결과만 I/O 작업으로 만드는 함수입니다\(결과를 반환하는 return이 아님\). _do_ 블럭이 생성하는 I/O 작업은 항상 마지막 작업의 결과값을 가지고 있습니다. 여기서는 마지막 작업의 결과가 아닌 `f result`의 결과를 포함한 I/O 작업을 반환해야 하기때문에 return을 사용하여 `f result`를 묶어주었습니다.

```haskell
main = do line <- getLine   
          let line' = reverse line  
          putStrLn $ "You said " ++ line' ++ " backwards!"  
          putStrLn $ "Yes, you really said" ++ line' ++ " backwards!"
```

프롬프트에서 입력받은 값을 뒤짚어서 출력하는 간단한 함수입니다. `fmap`을 사용하면 아래와 같이 작성할 수 있습니다.

```haskell
main = do line <- fmap reverse getLine  
          putStrLn $ "You said " ++ line ++ " backwards!"  
          putStrLn $ "Yes, you really said" ++ line ++ " backwards!"
```

`fmap reverse`는 `Just "Blah"`를 `Just "halb"`로 바꾸어줍니다. 여기에 `getLine`을 호출해서 얻은 `IO String` 타입의 I/O 작업을 `reverse`로 맵핑하고 I/O 작업을 반환합니다. `Maybe` 박스안의 어떤 값을 함수에 적용하는 것처럼 `IO` 박스도 가능합니다. 단지 `IO` 박스가 위치한 외부 세계에서 어떤 것을 가져온다는 점만 다릅니다. 이렇게 얻은 I/O 작업을 `<-`로 바인딩하면 `reverse`에 적용된 값이 `line`이라는 이름으로 바인딩됩니다.

만약 어떤 I/O 작업의 결과에 "!"를 붙이고 싶다면, `fmap (++"!") getLine`와 같이 사용하면 됩니다.

`IO`의 `fmap` 함수의 타입은 `fmap :: (a -> b) -> IO a -> IO b` 입니다. `fmap`는 함수와 I/O 작업을 받아서 기존의 I/O 작업에 포함된 값에 함수를 적용한 결과값을 포함한 새로운 I/O 작업을 반환합니다.

I/O 작업에서 `fmap`을 사용하면 코드가 간결해집니다. 만약 펑터 내부에서 어떤 데이터를 여러번 변환한다면 람다나 함수 합성을 통해서 선언하여 사용할 수 있습니다.

```haskell
import Data.Char  
import Data.List  

main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine  
          putStrLn line
```

```haskell
**[terminal]
**[prompt $ ]**[command runhaskell fmapping_io.hs]
hello there
E-R-E-H-T- -O-L-L-E-H
```

`intersperse '-' . reverse . map toUpper`는 문자열 한개를 입력받는 함수입니다. `toUpper`로 대문자로 만들고, `reverse`로 뒤짚은 후에 `intersperse '-'`로 문자열 사이사이에 "-"를 넣어줍니다. 이 부분은 `(\xs -> intersperse '-' (reverse (map toUpper xs)))`와 같이 람다로 표현할 수 있습니다.

`Functor`의 다른 인스턴스 예로 `(->) r`가 있습니다. 함수의 타입 `r -> a`는 `2 + 3`이 `(+) 2 3`과 동일한 것 처럼 `(->) r a`와 동일합니다. 여기서 `(->)`은 `Either`와 유사하게 두개의 타입 파라메터를 받는 타입생성자입니다. 그러나 이전에도 언급했던것처럼 `Functor` 인스턴스로 만드려면 한개의 타입 파라메터를 받는 타입 생성자이어야 합니다. 따라서 `(->)`는 `Functor`의 인스턴스가 될 수 없습니다. `Functor`의 인스턴스로 만드려면 `(->) r`로 부분 적용\(partially apply\)해야 합니다. 이런 함수를 펑터로 만들기 위해서는 `Control.Monad.Instaces`를 사용합니다.

```haskell
instance Functor ((->) r) where  
    fmap f g = (\x -> f (g x))
```

`fmap`의 타입은 `fmap :: (a -> b) -> f a -> f b`입니다. 여기서 `f`를 모두 `(->) r` 바꾸면, `fmap :: (a -> b) -> ((->) r a) -> ((->) r b)`가 됩니다. 이제 `(->) r a`를 `(->) r b`를 중위 연산자를 사용하여 `r -> a`, `r -> b`와 같이 일반적인 함수 형태로 바꾸면, `fmap :: (a -> b) -> (r -> a) -> (r -> b)`가 됩니다.

이 타입을 보면 `Maybe`나 리스트처럼 어떤 함수를 맵핑하여 어떤 함수를 만들어줍니다. `fmap :: (a -> b) -> (r -> a) -> (r -> b)`는 `a`에서 `b`가되는 함수와 `r`에서 `a`가되는 함수를 받아서 `r`에서 `b`가되는 함수를 반환합니다. 이것은 자세히보면 함수 합성의 동작과 같다는 것을 알 수 있습니다. 따라서 위에서 정의한 인스턴스는 아래와 같이 함수 합성을 사용하여 재작성될 수 있습니다.

```haskell
instance Functor ((->) r) where  
    fmap = (.)
```

따라서 함수들을 `fmap`에 맵핑한다는 것은 일종의 합성이라는 것을 알 수 있습니다. `:m + Control.Monad.Instances`를 하면, 아래와같이 실제 함수에 대한 맵핑을 할 수 있습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command :t fmap (*3) (+100)]
fmap (*3) (+100) :: (Num a) => a -> a
**[prompt ghci> ]**[command fmap (*3) (+100) 1]
303
**[prompt ghci> ]**[command (*3) `fmap` (+100) $ 1]
303
**[prompt ghci> ]**[command (*3) . (+100) $ 1]
303
**[prompt ghci> ]**[command fmap (show . (*3)) (*100) 1]
"300"
```

`fmap`을 중위 연산으로 호출한 것을 보면 `.`와 똑같다는 것을 알 수 있습니다. `(+100)`을 `(*3)`로 맵핑한 결과는 하나의 입력을 받은 함수입니다. 이 함수는 입력값을 받아서 먼저 `(+100)`을 호출하고, `(*3)`을 호출합니다.

`fmap (*3) (+100)`을 박스에 비유하면, `(+100)` 함수는 마지막 결과를 포함하는 박스입니다. `(+100)`에 `fmap (*3)`을 사용하는 것은 어떤 결과를 만들기전에 `(+100)`하는 함수를 만들고, 그 결과는 `(*3)`이 적용될 것 입니다. 이렇게 `fmap`은 `.`처럼 동작하게 됩니다. 따라서 `fmap`에 함수들을 사용하는 것은 함수 합성과 동일합니다.

`fmap`의 타입 `fmap :: (a -> b) -> f a -> f b`는 간결함을 위해서 `(Functor f) =>`와 같은 타입한정자를 생략하였습니다. 커링에 대해서 다룰때, 하스켈의 모든 함수들은 실제로 하나의 매개변수만 받는 함수라고 했습니다. 함수 `a -> b -> c`는 실제로는 `a`을 받아서 `b -> c`를 반환하는 함수입니다. 그리고 `b -> c` 역시 하나의 매개변수를 받아서 `c`를 반환하는 함수입니다. 따라서 `a -> b -> c`는 `a -> (b -> c)`와 같이 사용하면 더 커링처럼 보이게 할 수 있습니다.

만약 `fmap :: (a -> b) -> (f a -> f b)`과 같이 작성하면 `fmap`은 `a -> b` 함수를 받아서 함수 `f a -> f b`를 반환합니다. 이러한 함수를 _lifting_ 함수라고 부릅니다. GHCI에 타입을 보면 아래와 같습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command :t fmap (*2)]
fmap (*2) :: (Num a, Functor f) => f a -> f a
**[prompt ghci> ]**[command :t fmap (replicate 3)]
fmap (replicate 3) :: (Functor f) => f a -> f [a]
```

`fmap (*2)`는 숫자를 가진 펑터 f를 받아서 숫자를 가진 펑터를 반환하는 함수다. 여기서 펑터는 리스트가 될 수도있고, `Maybe`나 `Either String`이 될 수도 있다. `fmap (replicate 3)`은 어떤 타입을 가진 펑터\(a functor over any type\)를 받아서 동일한 타입의 리스트를 가진 펑터를 반환한다.

> _a functor over numbers_라는 말은 _a functor that has numbers in it_과 같은 말이다.

`fmap`은 함수와 펑터를 받아서 함수에 맵핑한 것은 가진 펑터를 반환하거나, 함수와 그 함수를 펑터위에 올려서 동작시키는 함수 입니다. 하스켈에서는 두가지 모두 동일합니다.

타입 `fmap (replicate 3) :: (Functor f) => f a -> f [a]`은 함수가 어떤 펑터와도 동작할 것을 의미한다. 어떤 펑터를 사용하는지에 따라서 정확한 동작이 결정됩니다. 만약 리스트에 `fmap (replicate 3)`를 사용하면 리스트안의 `fmap`의 구현체인 `map`을 쓸 것 입니다. 만약 `Maybe a`를 쓰면 `Just`안의 값에 `replicate 3`이 적용되거나 `Nothing`이 그대로 유지됩니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command fmap (replicate 3) [1,2,3,4]]
[[1,1,1],[2,2,2],[3,3,3],[4,4,4]]
**[prompt ghci> ]**[command fmap (replicate 3) (Just 4)]
Just [4,4,4]
**[prompt ghci> ]**[command fmap (replicate 3) (Right "blah")]
Right ["blah","blah","blah"]
**[prompt ghci> ]**[command fmap (replicate 3) Nothing]
Nothing
**[prompt ghci> ]**[command fmap (replicate 3) (Left "foo")]
Left "foo"
```

지금부터는 **펑터의 법칙\(functor laws\)**에 대해서 알아보겠습니다. 펑터가 되기위해서는 몇가지 법칙이 만족해야 합니다. 모든 펑터는 특정 종류의 펑터와 같은 특성 및 동작을 나타낼 것 입니다. 펑터는 맵핑이 될 수 있어야 합니다. 펑터의 `fmap`을 호출하면 펑터를 사용해서 함수를 맵핑해야 합니다. 펑터의 모든 인스턴스가 지켜야하는 두가지 법칙이 있습니다.

**1. **`id`** 함수를 펑터를 통해서 맵핑한다면, 반환되는 펑터는 본래의 펑터와 같아야 한다.**

이 말을 공식으로 쓰면, `fmap id = id` 입니다. 펑터에서 `fmap id`를 하면, 반드시 펑터위에서 `id`를 호출한 것과 동일해야 한다. 여기서 `id`는 매개변수로 받은 것을 가공없이 그대로 반환하는 항등 함수\(identity function\)입니다. 즉, `\x -> x`가 항등 함수입니다. 펑터를 맵핑될 수 있는 것이다라고 생각했을때, `fmap id = id` 법칙은 당연해보입니다.

이 법칙이 펑터의 몇가지 값에 대해서 지켜지는지 확인해보겠습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command fmap id (Just 3)]
Just 3
**[prompt ghci> ]**[command id (Just 3)]
Just 3
**[prompt ghci> ]**[command fmap id [1..5]]
[1,2,3,4,5]
**[prompt ghci> ]**[command id [1..5]]
[1,2,3,4,5]
**[prompt ghci> ]**[command fmap id []]
[]
**[prompt ghci> ]**[command fmap id Nothing]
Nothing
```

`Maybe`의 `fmap` 구현체를 보면 왜 펑터의 첫번째 법칙이 지켜지는 알 수 있습니다.

```haskell
instance Functor Maybe where  
    fmap f (Just x) = Just (f x)  
    fmap f Nothing = Nothing
```

`id`는 구현할때 `f`의 매개변수입니다. `Just x`에 `fmap id`를 하면, 결과는 `Just (id x)`입니다. 그리고 `id`는 그대로 매개변수를 반환하기 때문에 `Just (id x)`는 `Just x`와 같습니다. 따라서 `Just`로 값을 가진 `Maybe`로 `id`를 맵핑하면 동일한 값을 그대로 돌려받습니다.

`Nothing`에 `id`를 맵핑하면 동일한 값 `Nothing`을 그대로 반환합니다. 따라서 두개의 `fmap` 구현체 모두 `fmap id = id` 법칙을 만족합니다.

**2. 두 함수를 합성한 다음 결과 함수를 펑터로 맵핑하는 것은 첫번째 함수를 펑터에 맵핑한 다음에 다른 함수를 맵핑하는 것과 동일하다.** 이 법칙을 공식화하면 `fmap (f . g) = fmap f . fmap g`입니다. 또는 다른 방법으로 펑터를 F로 표기하여 `fmap (f . g) F = fmap f (fmap g F)`와 같이 작성할 수 있습니다.

어떤 타입이 이 펑터 법칙에 따른다면, 맵핑에 관해서는 다른 펑터와 동일한 동작을 한다고 볼 수 있습니다. 위 법칙에서 `fmap`의 구현부를 통해 타입에 어떻게 두번째 법칙을 적용할 수 있는지 알아봤습니다. 그리고 `Maybe`의 `fmap`을 사용하여 첫번째 법칙을 따르는 것을 확인했습니다.

동일하게 `Maybe`가 어떻게 두번째 펑터 법칙을 따르는지 알 수 있습니다. 만약 `Nothing`에 `fmap (f . g)`를 적용하면, `Nothing`이 됩니다. 어떤 함수의 `fmap`이든 `Nothing`을 적용하면 `Nothing`을 반환하기 때문입니다. 마찬가지로 동일한 이유로 `fmap f (fmap g Nothing)`도 결과는 `Nothing`이 됩니다. 따라서 `Maybe`의 `Nothing`은 두번째 법칙을 따릅니다.

`Just something`에 대해서도 살펴보겠습니다. `fmap (f . g) (Just x)`는 `Just ((f . g) x)`가 되고 다시 `Just (f (g x))`가 됩니다. `fmap f (fmap g (Just x))`에서 `fmap g (Just x)`는 `Just (g x)`이므로 `fmap f (Just (g x))`로 바꿀 수 있고, 이것은 다시 `Just (f (g x))`이므로 `fmap (f . g) = fmap f . fmap g`를 만족합니다.

약간 혼란스러울 수 있지만 함수 합성을 생각하면 이해할 수 있습니다. 많은 경우에 타입이 컨테이너 또는 함수처럼 작동하기 때문에 어떻게 이러한 법칙을 만족하는지 직관적으로 알 수 있습니다. 직접 타입은 여러가지 값으로 실행하면서 법칙을 만족하는지 확인할 수도 있습니다.

이번에는 펑터 법칙을 만족하지 못하는 펑터 타입클래스의 인스턴스를 살펴보겠습니다.

```haskell
data CMaybe a = CNothing | CJust Int a deriving (Show)
```

여기서 C는 _counter_를 나타냅니다. `Maybe a`와 상당히 유사한 데이터 타입입니다. 하지만 `Just` 부분에서 두개의 필드를 받는다는 점이 다릅니다. `CJust` 값 생성자의 첫번째 필드의 타입은 `Int`이고, 어떤 숫자같은 것 입니다. 두번째 필드 `a`는 입력되는 타입에 따라서 `CMaybe a`의 구체적인 타입이 달라집니다. 이 새로운 타입을 사용해보겠습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command CNothing]
CNothing
**[prompt ghci> ]**[command CJust 0 "haha"]
CJust 0 "haha"
**[prompt ghci> ]**[command :t CNothing]
CNothing :: CMaybe a
**[prompt ghci> ]**[command :t CJust 0 "haha"]
CJust 0 "haha" :: CMaybe [Char]
**[prompt ghci> ]**[command CJust 100 [1,2,3]]
CJust 100 [1,2,3]
```

`CNothing` 생성자는 입력 필드가 없습니다. `CJust`의 첫번째 필드는 숫자이고, 두번째 필드는 어떤 타입이든 될 수 있습니다. 이 타입을 `fmap`을 사용할 수 있는 `Functor`의 인스턴스로 만들어 보겠습니다. `fmap`은 두번째 필드를 함수에 적용시키고, 첫번째 필드는 값을 1 증가시킵니다.

```haskell
instance Functor CMaybe where  
    fmap f CNothing = CNothing  
    fmap f (CJust counter x) = CJust (counter+1) (f x)
```

`CJust` 상자가 기본적으로 비어있지 않다는 점만 제외하면 `Maybe`와 유사합니다. `CJust`가 가지고 있는 것을 단순히 함수에 적용하는 것 뿐만 아니라 counter값을 1 증가시키고 있습니다. 이제 직접 실행해보겠습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command fmap (++"ha") (CJust 0 "ho")]
CJust 1 "hoha"
**[prompt ghci> ]**[command fmap (++"he") (fmap (++"ha") (CJust 0 "ho"))]
CJust 2 "hohahe"
**[prompt ghci> ]**[command fmap (++"blah") CNothing]
CNothing
```

`CMaybe`는 펑터의 법칙을 만족할까요? 이것은 판단하기 위해서는 아래 한가지 예제를 보면 알 수 있습니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command fmap id (CJust 0 "haha")]
CJust 1 "haha"
**[prompt ghci> ]**[command id (CJust 0 "haha")]
CJust 0 "haha"
```

첫번째 펑터의 법칙인 `fmap id = id`를 만족하지 못하는 것을 알 수 있습니다. 따라서 `CMaybe`는 펑터 타입클래스의 인스턴스이지만, 펑터의 법칙에 어긋나므로 펑터가 아닙니다. `CMaybe`를 펑터처럼 사용하면 오류가 발생할 수 있습니다. 펑터를 사용할때는 먼저 몇가지 함수를 합성하고나서 펑터로 맵핑하든지, 또는 각 함수를 펑터를 통해서 연속적으로 맵핑하든지 상관이 없어야 합니다. 하지만 `CMaybe`의 경우는 몇번 맵핑되었는지를 기록하고 있기때문에 문제가 됩니다. `CMaybe`가 펑터의 법칙을 만족하려면 동일한 `fmap`을 사용할때는 `Int` 필드가 변경되지 않아야 합니다.

처음에는 펑터의 법칙이 조금 복잡하고 불필요한 것처럼 보일 수 있습니다. 그러나 타입이 두가지 법칙을 모두 만족한다는 것을 알게되면, 어떻게 동작할 것인가에 대해서 동일한 가정을 할 수 있습니다. 타입이 펑터의 법칙을 따르는 경우, 타입의 값에 대해 `fmap`을 호출하면 함수를 맵핑하는 것외에 어떤 것도 하지 않는다는 것을 알 수 있습니다. 이것은 펑터가 가지고 있어야할 행위\(behaviors\)를 추론할 수 있는 법칙을 사용하고, 펑터가 안정적으로 동작하는 함수를 만들 수 있기 때문에 더 추상적이고 확장성이 좋은 코드가 될 수 있습니다.

표준 라이브러리의 모든 펑터 인스턴스들은 이 법칙을 준수합니다. 하지만 이것을 믿기 어렵다면 직접 확인해볼 수 있습니다. 그리고 다음에 펑터의 인스턴스 타입을 만들때는 펑터의 법칙을 준수하는지 반드시 확인해야 합니다. 펑터를 많이 다루다보면 펑터의 속성과 동작을 직관적으로 볼 수 있고, 타입이 펑터의 법칙을 준수하는지 직관적으로 확인할 수 있습니다.

또한 펑터를 문맥에서 값을 출력하는 것으로 볼 수도 있습니다. 예를들어 `Just 3`은 값이 있거나, 비어있는 문맥에서 3을 출력합니다. `[1, 2, 3]`은 여러개의 값을 가지고있거나 비어있는 문맥에서 1, 2, 3을 출력합니다. 함수 `(+3)`는 주어진 매개변수에 따른 값을 출력합니다.

펑터를 값을 출력하는 것으로 생각하면 펑터를 통한 맵핑은 값을 변경하는 펑터의 출력에 변환\(transformation\)을 연결하는 것으로 생각할 수 있습니다. `fmap (+3) [1,2,3]`은 `[1,2,3]`의 출력을 `(+3)` 변환에 연결하는 것입니다. 따라서 리스트의 출력은 항상 `(+3)`이 적용됩니다. 함수를 맵핑하는 예제로 `famp (+3) (*3)`는 `(*3)`의 마지막 출력을 `(+3)` 변환에 연결하는 것 입니다. 이를통해서 왜 `fmap (+3) (*3)`이 함수 합성인 `(+3) . (*3)`와 같고, `\x -> ((x * 3) + 3)`과 같은지 직관적으로 이해할 수 있습니다. 수행 결과는 여전히 어떤 숫자를 입력으로 주는 경우에만 3을 곱한 다음에 변환을 거쳐 3이 추가되는 함수입니다. 이 동작은 함수 합성에서 일어나는 일이라는 것을 알 수 있습니다.

