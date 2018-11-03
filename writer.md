# Writer 모나드

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

지금까지 모나딕값처럼 작용하는 모노이드를 붙인 값을 보았습니다. 이번에는 `Monad` 인스턴스에 대해서도 적용해보겠습니다. `Control.Monad.Writer` 모듈에는 `Monad` 인스턴스와 함께 `Writer w a`를 노출하고 있습니다. 그리고 이 타입을 다루기위한 유용한 함수들을 제공합니다. 

모나드를 값에 붙이기 위해서 튜플안에 그것들을 추가하면됩니다. `Write w a` 타입은 단지 이것을 _newtype_으로 래핑한 것 입니다. 이것은 아래와 같이 선언되어 있습니다. 

```haskell
newtype Writer w a = Writer { runWriter :: (a, w) }
```

newtype으로 래핑되어 Monad 인스턴스가 될 수 있고, 그 타입이 일반 튜플과 분리되어 있습니다. a와 w 타입 매개변수는 각각 값과 붙여질 모노이드 값의 타입을 나타냅니다. 모나이드 인스턴스는 아래와 같이 정의됩니다. 

```haskell
instance (Monoid w) => Monad (Writer w) where  
    return x = Writer (x, mempty)  
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
```

먼저 &gt;&gt;= 함수는 기본적으로 applyLog 함수와 동일합니다. 



































