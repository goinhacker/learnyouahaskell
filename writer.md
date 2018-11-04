# Writer 모나드 for 로깅

#### 시작하기 전에..

이전 장에서 모나드가 어떻게 컨텍스트안에서 값을 받아서 함수에 적용하는지 살펴보았습니다. `>>=` 와 do 키워드를 사용하면 컨텍스트가 핸들링되는 과정에서 값을 다루는데 집중할 수 있었습니다. 실패할 가능성을 담는 컨텍스트인 Maybe 모나드, 프로그램에서 비결정적\(non-determinism\)을 다루는 리스트 모나드와 IO 모나드가 어떻게 동작하는지 배웠습니다. 

이번 챕터에서는 몇가지 다른 모나드들에 대해서 살펴볼 것입니다. 모든 종류의 모나드적인 값\(monadic value\)을 사용해서 프로그램을 더 깔끔하게 만드는 방법을 알게될 것입니다. 그리고 이런 과정을 통해서 모나드에 대한 더 분명한 직관을 만들 것입니다. 

여기서 살펴볼 모나드들은 모두 `mtl` 패키지의 일부 입니다. 하스켈 패키지는 모듈의 모음이고, `mtl` 패키지는 하스켈 플랫폼에서 제공하는 것입니다. `mtl` 패키지를 이미 가지고 있는지 확인하려면 커맨드라인에서 `ghc-pkg`를 입력해보면 설치된 패키지 리스트를 확인할 수 있습니다. 

#### Writer 모나드는 어디에 쓰나?

`Writer` 모나드는 로그를 위한 값들을 추가하기 위한 모나드 입니다. `Writer`를 사용하면 모든 로그 값이 하나의 로그 값으로 결합되어 결과에 첨부되도록하면서 계산을 수행 할 수 있습니다. 예를들어 디버깅 목적으로 내부에서 무슨일이 일어났는지 설명하는 문자열이 필요할때 활용할 수 있습니다. 

갱에 노상강도가 몇명인지에 따라서 빅갱인지 확인하는 함수를 만들어 보겠습니다. 간단하게 만들면 아래와 같습니다. 

```haskell
isBigGang :: Int -> Bool  
isBigGang x = x > 9
```

이번에는 단순히 True, False로 반환하는 대신, 왜 그런 결과가 나왔는지 로그를 추가하려고 합니다. 간단하게 `Bool`과 함께 문자열을 반환하면 해결됩니다. 

```haskell
isBigGang :: Int -> (Bool, String)  
isBigGang x = (x > 9, "Compared gang size to 9.")
```

`Bool` 대신 문자열이 포함된 튜플을 반환했습니다.

```haskell
ghci> isBigGang 3  
(False,"Compared gang size to 9.")  
ghci> isBigGang 30  
(True,"Compared gang size to 9.") 
```

`isBigGang` 함수는 일반적인 값을 받아서, 컨텍스트를 반환합니다. 여기서 우리는 `(3, "Smallish gang.")`와 같은 로그를 `isBigGang` 함수의 반환값에 붙이고 싶습니다. 이와같이 어떤 일반적인 값을 받아서 컨텍스트를 반환하는 함수가 있을때, 어떻게 컨텍스트를 가진 값을 받아서 다른 함수의 입력으로 넣을수 있나요? 

`Maybe` 모나드를 공부할때, 우리는 `Maybe a`와 `a -> Maybe b`를 받아서 `a`값을 함수에 적용하는  `applyMaybe` 함수\(이하, `>>=`\)를 만들었습니다. 이 함수에서는 실패할 가능성이 있는 컨텍스트안의 값도 일반적인 값처럼 다룰 수 있게 합니다. 

동일한 맥락으로 첨가할 로그를 포함한, `(a, String)`와 `a -> (b, String)` 타입의 함수를 받아서 값을 함수에 넣는 함수, `applyLog`를 만들어 보겠습니다. `(a, String)`은 실패할 가능성이 있는 값을 포함한 컨텍스트가 아니고, 추가 로그를 가진 컨텍스트입니다. 따라서 `applyLog` 함수는 기존 값이 가진 로그를 버리지않고, 함수가 반환하는 결과값과 합쳐야 합니다. `applyLog` 함수를 작성하면 아래와 같습니다. 

```haskell
applyLog :: (a,String) -> (a -> (b,String)) -> (b,String)  
applyLog (x,log) f = let (y,newLog) = f x in (y,log ++ newLog)
```

어떤 컨텍스트를 가진 값을 가지고 있고 어떤 함수의 입력으로 넣을때, 일반적으로 값을 컨텍스트와 분리해서 값을 함수에 적용합니다. 예를들어 `Maybe` 모나드에서는 값이 `Just x`이면 `x`를 분리해서 함수에 적용했습니다. 여기서는 튜플을 다루기때문에 실제값을 로그와 분리하기가 매우 간단합니다. 그래서 우선 값 `x`를 얻어서 함수 `f`에 적용했습니다. 그 결과값으로 `(y, newLog)`를 구했습니다. 여기서 `y`는 새로운 결과값이고 `newLog`는 새로운 로그입니다. 하지만 `newLog`는 기존의 로그를 포함하고 있지 않으므로 기존 로그에 새로운 로그를 붙인 `(y, log ++ newLog)`를 반환합니다. 

이제 `applyLog` 함수의 동작을 살펴보겠습니다. 

```haskell
ghci> (3, "Smallish gang.") `applyLog` isBigGang  
(False,"Smallish gang.Compared gang size to 9")  
ghci> (30, "A freaking platoon.") `applyLog` isBigGang  
(True,"A freaking platoon.Compared gang size to 9")
```

결과는 이전과 비슷하지만, 결과로그안에 갱단의 규모에 대한 로그가 추가되었습니다. 아래는 몇가지 `applyLog` 활용 예입니다.

```haskell
ghci> ("Tobin","Got outlaw name.") `applyLog` (\x -> (length x, "Applied length."))  
(5,"Got outlaw name.Applied length.")  
ghci> ("Bathcat","Got outlaw name.") `applyLog` (\x -> (length x, "Applied length"))  
(7,"Got outlaw name.Applied length")
```

예제에서 `x`는 튜플이 아니고, 일반 문자열입니다. 하지만 `applyLog` 함수는 기존의 로그를 포함한 컨텍스트를 유지하면서 새로운 로그를 추가해서 동작하는 것을 알 수 있습니다. 

#### 모노이드를 활용한 일반화

지금까지 `applyLog`는 `(a, String)` 타입을 입력으로 받았지만, 로그의 타입이 반드시 `String`일 이유는 없습니다. 로그를 붙이기 위해서 `++` 함수를 사용했으므로, 리스트에 대해서도 문제없이 동작할 것입니다. 그래서 `applyLog`의 타입을 바꾸어 보겠습니다.

```haskell
applyLog :: (a,[c]) -> (a -> (b,[c])) -> (b,[c])
```

이제 로그는 리스트입니다. 값의 타입은 리스트에 포함되어있고, 기존 로그와 새로운 로그, 그리고 함수가 반환하는 로그의 타입은 일치해야 합니다. 그렇지 않으면 `++` 함수를 사용할 수 없을 것입니다.

`bytestrings`에도 동작할까요? 안될 이유는 없지만 현재는 리스트에 대해서만 동작합니다. 동작시키려면 마치 `bytestrings`를 위한 `applyLog` 함수를 새로 만들어야 할 것 같습니다. 그러나 리스트와 `bytestrings`는 모두 모노이드입니다. 이말은, 둘다 모노이 타입클래스의 인스턴스이고, `fmap` 함수와 `mappend` 함수를 가지고 있다는 것을 의미합니다. 

```haskell
ghci> [1,2,3] `mappend` [4,5,6]  
[1,2,3,4,5,6]  
ghci> B.pack [99,104,105] `mappend` B.pack [104,117,97,104,117,97]  
Chunk "chi" (Chunk "huahua" Empty)
```

이제 `applyLog` 함수가 모든 모노이드에 대해서 동작하도록 할 수 있습니다. 이것을 위해서는 함수의 타입뿐만 아니라 `++`를 `mappend`로 바꾸어야 합니다. 

```haskell
applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)  
applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)
```

이제 추가된 값의 타입이 모노이드이기때문에, 더이상 값과 로그로 생각할 이유가 없습니다. 이제 값이 어떤 모노이와 함께 튜플로 반환될 것입니다. 예를들어 어떤 아이템의 이름과 모노이드인 가격을 튜플로 가질 수 있습니다. `Sum` _newtype_을 사용해서 아이템을 조작할때 가격이 추가되는지 확인해 보겠습니다.  아래 예제의 `addDrink`는 음식에 음료를 추가하는 함수입니다.

```haskell
import Data.Monoid  
  
type Food = String  
type Price = Sum Int  
  
addDrink :: Food -> (Food,Price)  
addDrink "beans" = ("milk", Sum 25)  
addDrink "jerky" = ("whiskey", Sum 99)  
addDrink _ = ("beer", Sum 30)
```

여기서 음식은 문자열로 표현하고, 가격은 얼마나 많은 센트가 비용으로 드는지를 유지하기 위해서 `Int`를 `Sum` _newtype_으로 래핑했습니다. 

```haskell
ghci> Sum 3 `mappend` Sum 9  
Sum {getSum = 12}  
```

예제와 같이 `Sum`에 `mappend`를 사용하면 래핑된 값끼리 더해집니다.

`addDrink` 함수의 동작은 간단합니다. `beans`를 먹으면 "milk"와 함께 추가되는 가격을 래핑한  `Sum 25`를 반환합니다. 다른 어떤 음식을 먹을때도 마찬가지 입니다. `addDrink` 함수만으로는 별로 유용하지 않습니다. 하지만 `applyLog` 함수에 적용하면 꽤 유용하게 사용될 수 있습니다.

```haskell
ghci> ("beans", Sum 10) `applyLog` addDrink  
("milk",Sum {getSum = 35})  
ghci> ("jerky", Sum 25) `applyLog` addDrink  
("whiskey",Sum {getSum = 124})  
ghci> ("dogmeat", Sum 5) `applyLog` addDrink  
("beer",Sum {getSum = 35})
```

milk는 25센트입니다. 하지만 beans와 함께 먹으면 10센트가 추가됩니다. 따라서 가격은 35센트가 됩니다. 

이제 추가할 값이 반드시 로그가 아니어도 된다는 점이 확인되었습니다. 이 값은 어떤 모노이드 값이든 될 수 있고,  각 값들을 합치는 방법은 모노이드에 의존합니다. 로그를 기록할때 추가했지만, 이제는 숫자를 합산할 수도 있습니다.

addDrink 함수가 리턴하는 값의 타입이 \(Food, Price\)이기 때문에, 그 결과값을 다시 addDrink 함수의 입력으로 넣을 수 있습니다. 그래서 우리가 음식과 함께 마실 것을 추가할때 비용이 얼마나 드는지 알려줍니다. 

```haskell
ghci> ("dogmeat", Sum 5) `applyLog` addDrink `applyLog` addDrink  
("beer",Sum {getSum = 65}) 
```

`dogmeat`을 먹을때 추가되는 `beer`의 가격이 30 센트입니다. 따라서 `addDrink` 함수를 사용해서 처음 맥주를 추가하면 `("beer", Sum 35)`입니다. 그리고 한번더 맥주를 추가하면 `("beer", Sum 65)`가 되는 것을 알 수 있습니다. 

#### Writer 타입

지금까지 모나딕값처럼 작용하는 모노이드를 붙인 값을 보았습니다. 이번에는 `Monad` 인스턴스에 대해서도 적용해보겠습니다. `Control.Monad.Writer` 모듈에는 `Monad` 인스턴스와 함께 `Writer w a`가 있습니다. 그리고 이 타입을 다루기위한 유용한 함수들을 제공합니다. 

모나드를 값에 붙이기 위해서 튜플안에 그것들을 추가하면 됩니다. `Write w a` 타입은 단지 이것을 _newtype_으로 래핑한 것 입니다. 이것은 아래와 같이 선언되어 있습니다. 

```haskell
newtype Writer w a = Writer { runWriter :: (a, w) }
```

_newtype_으로 래핑되어 `Monad` 인스턴스가 될 수 있고, 그 타입이 일반 튜플과 분리되어 있습니다. `a`와 `w` 타입 매개변수는 각각 값과 붙여질 모노이드 값의 타입을 나타냅니다. 모나이드 인스턴스는 아래와 같이 정의됩니다. 

```haskell
instance (Monoid w) => Monad (Writer w) where  
    return x = Writer (x, mempty)  
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
```

먼저 `>>=` 함수는 기본적으로 튜플이 `Writer` _newtype_으로 래핑되어 있다는 점을 제외하고는  `applyLog` 함수와 유사합니다. 따라서 구현부에서 패턴매칭을 통해서 언래핑하였습니다. `x`값을 얻어서 `f` 함수에 적용했습니다. 그리고 그 결과값은 `Writer w a`를 반환합니다. 여기서 `let`은 `f x`의 결과값을 다시 패턴매칭하기 위해서 사용했습니다. `y`는 새로운 결과값입니다. `mappend`를 사용해서 기존의 모노이드값과 새로운 모노이드를 결합합니다. 그리고 이렇게 만들어진 튜플을 다시 `Writer` 생성자로 포장했습니다.

`return` 함수는 값을 입력받아서 최소한의 컨텍스트안에 넣어서 반환합니다. `Writer`에서 최소한의 컨텍스트는 가능한 다른 모노이드에 영향을 미치지 않는 것입니다. 따라서 항등원을 의미하는 `mempty`가 가장 적합할 것 입니다. `mempty`는 `""`, `Sum 0`, 비어있는 bytestrings 같은 것들이 됩니다. `mempty`와 다른 모노이드를 `mappend`하면 결과를 항상 다른 모노이드가 그대로 반환됩니다. 따라서 `return`으로 `Writer`를 만들고 `>>=`를 사용해서 함수의 입력으로 넣으면, 함수의 반환값이되는 모노이드를 그대로 반환할 것입니다. 예제와 같이 숫자 `3`에 `return`을 여러번 사용해보면, 모두 다른 모노이드와의 쌍이 반환됩니다. 

```haskell
ghci> runWriter (return 3 :: Writer String Int)  
(3,"")  
ghci> runWriter (return 3 :: Writer (Sum Int) Int)  
(3,Sum {getSum = 0})  
ghci> runWriter (return 3 :: Writer (Product Int) Int)  
(3,Product {getProduct = 1})
```

`Writer`는 `Show`의 인스턴스가 아니기 때문에 `runWriter`를 사용해서 `Writer` 값을 튜플로 변환했습니다. `String`일때는 빈 문자열을 반환하고, `Sum`일때는 더하기의 항등원인 `0`을 래핑한 결과를 반환했습니다. `Product`는 곱하기의 항등원인 `1`을 반환한 것을 확인할 수 있습니다. 

`Writer` 인스턴스는 `fail`이 구현되어 있지 않습니다. 따라서 `do` 구문에서 패턴매칭에 실패하면 `error`가 호출됩니다. 

#### do 구문에서 Writer 사용하기

이제 우리는 모나드 인스턴스가 생겼습니다. `Writer` 값을 `do` 구문에서 사용할 수 있습니다. 이것은 여러개의 `Writer` 값들을 가지고 작업을 할때 유용합니다. 다른 모나드들과 같이 컨텍스트를 신경쓰지않고 일반 값들처럼 사용할 수 있습니다. 이 경우, 붙여진 모든 모노이드 값들이 최종 결과값에 `mappend`로 반영되어 집니다. 아래는 두 숫자를 곱하기 위한 `Writer`를 `do` 구문에서 사용한 예제입니다.

```haskell
import Control.Monad.Writer  
  
logNumber :: Int -> Writer [String] Int  
logNumber x = Writer (x, ["Got number: " ++ show x])  
  
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    return (a*b)
```

`logNumber` 함수는 숫자를 받아서 `Writer`를 반환합니다. 모노이드는 문자열의 리스트이고, 각 문자열은 가지고있는 숫자에 대해서 설명합니다. `multWithLog`는 `3`과 `5`를 곱한 `Writer` 값입니다. 그리고 `3`과 `5`에 대한 로그가 최종로그에 붙여집니다. 마지막으로 최소한의 컨텍스트안에 넣어주는 `return`을 사용해서 `a*b`를 반환했습니다. 이 과정에서 우리는 어떤 로그도 직접 추가하지 않았습니다. 동작시켜 보겠습니다. 

```haskell
ghci> runWriter multWithLog  
(15,["Got number: 3","Got number: 5"])
```

모노이드 값을 특정 시점에만 포함시키고 싶을때도 있을 것입니다. 이때는 `tell` 함수를 사용합니다. 이 함수는 `MonadWriter` 타입클래스에 있고, `["This is going on"]`와 같은 모노이드값을 받아서 더미값 `()`에 원하는 모노이드 값만 붙여진 `Writer` 값을 생성합니다. 결과로 `()`를 가진 모나드 값을 가지고 있을때는 변수에 할당하지 않습니다. 하지만 새로 작성된 `multWithLog`는 `tell`을 사용해서 이러한 부가정보를 포함할 수 있습니다. 

```haskell
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    tell ["Gonna multiply these two"]  
    return (a*b)
```

여기서 `(a*b)`가 마지막 라인에 있다는 것이 중요합니다. 왜냐하면 `do` 표현에서 마지막 라인의 결과는 전체 `do` 구문의 결과를 의미하기 때문입니다. `tell` 함수를 마지막에 넣으면, 최종 결과는 `()`가 될 것입니다. 다시 동작시켜보면 아래와 같습니다. 

```haskell
ghci> runWriter multWithLog  
(15,["Got number: 3","Got number: 5","Gonna multiply these two"])
```

#### 프로그램에 로깅하기

유클리드는 두개의 숫자를 받아서 최대공약수를 구하는 알고리즘입니다. 하스켈은 이미 최대공약수를 구하는 `gcd` 함수를 가지고 있습니다. 하지만 여기서 직접 구현하면서 로깅을 포함시켜 볼 것입니다. 아래는 일반적인 알고리즘입니다.

```haskell
gcd' :: Int -> Int -> Int  
gcd' a b   
    | b == 0    = a  
    | otherwise = gcd' b (a `mod` b) 
```

알고리즘은 매우 심플합니다. 먼저 두번째 값이 0인지 체크하고 만약 0이면 첫번째 입력 값을 반환합니다. 만약 두번째 값이 0이 아니면, 두번째 값과 첫번째 값에서 두번째 값을 나눈 나머지 값의 최대공약수를 구해서 반환합니다. 예를들어 8과 3의 최대공약수를 구한다면, 3이 0이 아니기 때문에 3과 8에서 3를 나눈 나머지인 2의 최대공약수를 구합니다. 다음에 3과 2의 최대공약수를 구하는데, 이번에도 2는 0이 아니므로, 2와 1의 최대공약수를 구합니다. 여전히 두번째 값이 0이 아니므로 1과 0의 최대공약수를 구하고, 비로소 두번째 값이 0이므로 최종 결과는 1이 됩니다. 동작시켜보면 아래와 같습니다. 

```haskell
ghci> gcd' 8 3  
1
```

이제 결과를 반환하는 컨텍스트에 로그로 모노이드 값을 넣을 것입니다. 로깅을 위한 모노이드로는 문자열의 리스트를 사용할 것입니다. 따라서 `gcd` 함수의 타입은 아래와 같습니다. 

```haskell
gcd' :: Int -> Int -> Writer [String] Int 
```

이제 함수에 로그를 추가해보겠습니다.

```haskell
import Control.Monad.Writer  
  
gcd' :: Int -> Int -> Writer [String] Int  
gcd' a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        gcd' b (a `mod` b)
```

`gcd'` 함수는 두개의 `Int` 값을 받아서 `Writer [String] Int`를 반환합니다. 여기서 각 `Int`는 로그 컨텍스트를 포함하게 됩니다. `b`가 `0`이며, 결과를 그대로 주는 대신 로그를 `Writer`에 포함해서 반환하기 위해서 `do`구문을 사용합니다. 먼저 `tell` 함수를 사용해서 계산이 끝났다는 것을 로깅하고, `return`을 사용해서 `do` 구문의 결과 `a`를 반환합니다. `do` 구문 대신에 아래와 같이 작성할 수도 있습니다. \(개인적으로는 `do` 구문을 사용하는 것이 낫다고 생각한다.\)

```haskell
Writer (a, ["Finished with " ++ show a])
```

 다음으로 `b`가 `0`이 아닌 경우,  `mod` 연산자를 사용해서 나머지를 구할때 사용한 `a`와 `b`를 로깅하였습니다. 그리고나서 `do` 구문의 두번째 라인에 재귀적으로 `gcd'`를 호출합니다. 여기서 `gcd'`는 결과적으로 `Writer` 값을 반환한다는 점을 기억해야 합니다. 따라서 ``gcd' b (a `mod` b)``는 올바른 결과를 반환하게 됩니다. 

새로운 `gcd'` 함수는 로그를 통해서 내부적으로 어떻게 동작하는지 흐름을 추적하기에 용이합니다. 이제 새로운 `gcd'`를 사용해보겠습니다. `Wrtier [String] Int`를 언래핑하면 튜플를 얻을 수 있고, 튜플의 첫번째 값이 계산 결과값입니다.

```haskell
ghci> fst $ runWriter (gcd' 8 3)  
1 
```

잘 동작한 것을 확인했으나 튜플의 첫번째 값만 가져왔으므로 로그는 출력되지 않았습니다. 로그는 문자열의 리스트이기 때문에, `mapM_ putStrLn`을 사용해서 화면에 출력하도록 하겠습니다. 

```haskell
ghci> mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)  
8 mod 3 = 2  
3 mod 2 = 1  
2 mod 1 = 0  
Finished with 1
```

우리가 만든 알고리즘이 어떻게 동작하는지 알 수 있는 아주 좋은 방법입니다. 이러한 과정을 직접 로그를 계속 추가하지않고, 값이 바뀌는 것만으로 모나딕 값들을 만들어갑니다. 로그에 대해서는 `>>=`를 구현해 주는 것만으로 `Writer`가 알아서 처리해줍니다. 이러한 로깅 매카니즘은 일반 값을 `Writer`로 바꾸는 것만으로도 어떤 함수에든 추가할 수 있습니다. 그리고 `do` 구문을 사용해서 가독성을 높일 수 있습니다. 

#### 비효율적인 리스트 생성 문제

`Writer`를 사용할때, 리스트가 매우 느리게 동작할 수 있기 때문에 주의깊게 사용해야 합니다. 이게 느려지는 이유는 `mappend`를 위해서 리스트가 `++` 함수를 사용하고, `++` 함수를 사용하는 것은 리스트의 맨뒤에 추가하는 것이기 때문입니다. 리스트가 매우 길어지면, 이 작업은 매우 느려질 수 있습니다. 

gcd' 함수에서는 아래와 같이 리스트에 추가하기 때문에 로깅이 빠릅니다. 

```haskell
a ++ (b ++ (c ++ (d ++ (e ++ f)))) 
```

여기서는 리스트가 왼쪽에서 오른쪽으로 쌓이는 데이터 구조입니다. 그리고 이때는 리스트의 왼쪽 부분을 완전히 만든다음에 오른쪽의 긴 리스트를 추가하기 때문에 효율적으로 동작합니다. 하지만 주의해서 사용하지 않으면 Writer 모나드가 아래와 같은 붙이는 리스트를 사용하게 될 수 있습니다. 

```haskell
((((a ++ b) ++ c) ++ d) ++ e) ++ f 
```

이때는 오른쪽에서 왼쪽으로 데이터를 쌓고 있습니다. 이때는 리스트의 왼쪽 부분에 오른쪽 부분을 추가할 때마다 처음부터 왼쪽 부분을 구성해야 하기 때문에 비효율적입니다.

아래 gcd' 함수는 로깅이 반대 순서로 동작하도록 작성된 것입니다. 먼저 나중에 수행될 연산에 대한 로그를 생성한 다음에 현재 단계의 로그를 마지막에 추가합니다.   

```haskell
import Control.Monad.Writer  
  
gcdReverse :: Int -> Int -> Writer [String] Int  
gcdReverse a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        result <- gcdReverse b (a `mod` b)  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        return result 
```

이 예제는 재귀를 먼저 수행하고 결과에 결과값을 할당합니다. 그리고 현재 단계의 로그가 추가됩니다. 하지만 현재 단계는 재귀에 의해 생성된 로그의 마지막 부분에 있습니다. 결과적으로 최종 결과는 재귀의 결과가 됩니다. 실행해보면 아래와 같습니다. 

```haskell
ghci> mapM_ putStrLn $ snd $ runWriter (gcdReverse 8 3)  
Finished with 1  
2 mod 1 = 0  
3 mod 2 = 1  
8 mod 3 = 2
```

이 경우는 ++ 함수가 오른쪽 방향이 아닌 왼쪽 방향으로 사용되기 때문에 비효율적으로 동작합니다.

####  효율적으로 동작하는 리스트 만들기



































