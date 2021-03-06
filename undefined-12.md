# 타입클래스

## 타입클래스

지난 챕터에서 몇가지 하스켈의 표준 타입클래스들을 배우고, 어떤 타입들이 속하는지 보았습니다. 또한 사용자 정의 타입을 표준 타입클래스로부터 파생시키는 방법을 살펴보았습니다. 이번 섹션에서는 사용자 정의 타입클래스를 만들고, 그것으로부터 파생되는 타입을 만들어 보겠습니다.

그동안 배운 타입클래스에 대한 내용을 요약하면 타입클래스는 인터페이스와 같다는 것입니다. 타입클래스는 동일한지 비교하거나, 순서를 비교하는 등의 몇가지 동작을 정의합니다. 그리고 타입은 타입클래스의 인스턴스로 만듬으로써 해당 동작을 할 수 있습니다. 타입클래스들의 동작은 함수들을 정의하거나 구현한 선언을 입력하여 얻을 수 있습니다. 어떤 타입이 타입클래스의 인스턴스라는 것은 타입이 타입클래스의 동작을 할 수 있다는 것을 의미합니다.

타입클래스는 자바나 파이썬의 클래스와는 전혀 관계가 없습니다. 이 부분이 헷갈리수 있으므로 지금부터는 명령형 언어의 클래스들에 대해서는 완전히 잊어야 합니다!!

예를들어, `Eq` 타입클래스는 동일한지를 확인할 수 있는데 `==`과 `/=` 함수를 정의하고 있습니다. 만약 `Car`라는 타입이 `Eq` 타입클래스의 인스턴스라면, 두개의 차가 동일한지 여부를 `==`함수로 확인할 수 있습니다.

`Eq` 타입클래스는 표준 prelude 모듈에 아래와 같이 정의되어 있습니다.

```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
```

여기서 `class Eq a where`는 `Eq`라는 새로운 타입클래스를 정의하는 것을 의미합니다. 여기서 `a`는 타입 변수이고, `Eq`의 인스턴스가 될 타입의 역할을 수행합니다. `a` 다른 이름으로 쓰일 수 있고, 한개의 문자가 아니어도 되지만 소문자로 구성되어야 합니다. 그 다음에는 여러가지 함수들이 정의되었습니다. 여기서 함수의 바디를 구현하는 것은 필수가 아닙니다. 따라서 그냥 함수의 선언만 명시해도 됩니다.

어쨋든 여기는 `Eq`가 정의한 함수에 대해서 함수의 바디를 구현했지만, 상호 재귀 관점에서만 정의했습니다. `Eq`의 인스턴스 두개가 다르지 않다면 같은 것이고, 같지않다면 다를 것입니다. 이런 구현은 반드시 하지않아도 되지만 어떤 도움을 줄 수 있는지 보게 될 것 입니다.

```haskell
data TrafficLight = Red | Yellow | Green
```

TrafficLight는 신호등의 상태를 정의합니다. TrafficLight는 클래스 인스턴스를 파생하지 않았습니다. `Eq`나 `Show`로 파생시킬수도 있지만, 여기서는 직접 어떤 인스턴스를 작성해보도록 하겠습니다.

```haskell
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False
```

_instance_ 키워드를 사용해서 인스턴스로 작성하였습니다. _class_는 새로운 타입클래스를 정의하고, _instance_는 타입클래스의 타입 인스턴스를 만듭니다. `Eq`를 정의할때 `class Eq a where`로 작성하고 여기서 `a`는 나중에 인스턴스가 만들어질때 타입의 역할을 한다고 했습니다. 여기서는 `instance Eq TrafficLight where`로 작성하여 바로 인스턴스로 만들었기 때문에 좀 더 명확합니다. `a`를 실제 타입으로 변경하였습니다.

_class_ 선언안에서 `==`은 `\=`와 그 반대의 관점에서 정의되었기 때문에, 인스턴스 선언안에 둘 중 하나를 겹쳐서 사용했습니다. 이것을 타입클래스의 **최소한의 완전한 선언**이라고 부릅니다\(타입이 클래스처럼 동작할 수 있도록 하기위해 구현해야하는 최소한의 함수\). `Eq`가 최소한의 완전한 선언을 수행하려면 `==` 또는 `\=`중 하나를 overwrite해야 합니다. `Eq`를 간단히 정의하면 아래와 같습니다.

```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (\=) :: a -> a -> Bool
```

이렇게 선언된 타입클래스의 인스턴스 타입을 만들때는 두 함수 모두 구현해야 합니다. 왜냐하면 하스켈은 두 함수가 어떻게 관련되어 있는지 알지못하기 때문입니다. 최소한의 완전한 선언은 `==`과 `\=` 모두가 될 것입니다.

이번에는 TrafficLight를 `Show`의 인스턴스로 만들어 보겠습니다. `Show`에 대한 최소한의 완전한 정의를 만족하기 위해서, 값을 문자열로 변환하는 `show` 함수를 구현해야 합니다.

```haskell
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"
```

```haskell
ghci> Red == Red
True
ghci> Red == Yellow
False
ghci> Red `elem` [Red, Yellow, Green]
True
ghci> [Red, Yellow, Green]
[Red light,Yellow light,Green light]]
```

간단하게 `Eq`는 파생시키는 방법으로도 동일한 효과를 얻을수 있습니다.\(여기서는 교육을 목적으로 직접 작성\) 하지만 `Show`의 경우는 그냥 파생시키면 직접 문자열을 `"Red light"`와 같이 지정하여 출력할 수 없습니다. 이 경우는 반드시 _instance_ 선언으로 직접 작성해야 합니다.

또한 타입클래스의 서브클래스인 타입클래스를 만들수도 있습니다. `Num` 타입클래스에 대한 _class_ 선언은 매우 길지만, 첫줄은 아래와 같습니다.

```haskell
class (Eq a) => Num a where
  ...
```

이전에도 언급했듯이 클래스 제약안에는 여러개를 넣을 수 있습니다. 따라서 위의 선언은 `class Num a where`에서 `a`가 `Eq`의 인스턴스여야 한다는 제약만 추가한 것입니다. 이 선언을 통해서 숫자를 고려한 어떤 타입을 고려하기전에, 이 타입의 값이 같은지 여부를 판단할 수 있는 값인지 확인해야 합니다. 단지 _class_ 선언에 대한 클래스 제약을 만드는 것이 전부입니다. _class_나 _instance_ 선언안에 함수의 바디를 정의할때, `a`의 타입은 `Eq`의 인스턴스이고 `==`를 사용할 수 있다는 것을 가정할 수 있습니다.

`Maybe`나 리스트 타입은 어떻게 타입클래스의 인스턴스로 만들 수 있을까요? 여기서 `Maybe`와 `TrafficLight`의 차이점은 `Maybe`는 그 자체적으로 구체적인 타입\(concrete type\)이 아니고 한개의 파라메터를 받아서 구체적인 타입을 만드는 타입 생성자\(type constructor\)라는 점입니다.\(예를들면, `Maybe Char` 처럼..\)

여기서 다시한번 `Eq` 타입클래스를 살펴보겠습니다.

```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
```

이 타입 선언에서 `a`는 **함수안의 모든 타입들은 이미 구체화\(concrete\)**되어 있어야 하기때문에, 구체적인 타입\(concrete type\)으로 사용된다는 것을 알 수 있습니다\(예를들면, 함수의 타입이 `a -> Maybe`일수는 없지만, `a -> Maybe a`나 `Maybe Int -> Maybe String`일수는 있습니다.\). 따라서 아래와 같이 선언할수는 없습니다.

```haskell
instance Eq Maybe where
   ...
```

왜냐하면 `a`는 구체적인 타입이어야 하는데 `Maybe`는 타입 생성자이기 때문입니다. 이것을 모든 타입에 대해서 `instance Eq (Maybe Int) where`, `instance Eq (Maybe Char) where`와 같이 작성하는 것은 매우 번거로운 일입니다. 그래서 아래와 같이 작성합니다.

```haskell
instance Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False
```

이렇게 작성하면 모든 타입에 대해서 `Maybe something` 형태의 `Eq` 인스턴스를 만들 수 있습니다. `class Eq a where`의 `a`와 마찬가지로 `(Maybe m)`은 구체적인 타입입니다\(여기서 `m`은 소문자여야함\). `m`이 어떤 타입이든 `(Maybe m)`의 형태로 `Eq`의 인스턴스가 될 수 있습니다.

여기에는 한가지 문제가 있습니다. `Maybe` 자체는 `==`을 사용할 수 있다는 것은 알지만, `Maybe`가 가지고 있는 `m`이 `Eq`의 인스턴스인지는 알 수 없습니다. 따라서 아래와 같이 _instance_ 선언을 수정해야 합니다.

```haskell
instance (Eq m) => Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False
```

여기서는 _instance_ 선언에 클래스 한정을 추가해 주었습니다. 이렇게하면 `Maybe m`뿐만 아니라 `m`도 `Eq`의 인스턴스 입니다. 이것은 실제로 하스켈이 인스턴스를 파생시키는 방법입니다.

대부분의 경우 _class_ 선언내에서 클래스 한정은 타입클래스의 서브클래스인 타입클래스를 만들기 위해서 사용됩니다. 그리고 _instance_ 선언에서는 어떤 타입의 내용물\(contents\)에 대한 요구사항을 표현하기 위해서 사용됩니다. 예를들어, `Maybe`에 포함된 내용물\(contents\)도 `Eq` 타입클래스의 인스턴스라는 것입니다.

인스턴스를 만들때 타입 선언안에서 타입이 구체적인 타입으로 사용된다면\(`a -> a -> Bool`에서 `a`처럼\), 타입 파라메터들을 제공하고 괄호를 추가하여 구체적인 타입으로 끝나야 합니다.

> 인스턴스를 만들려는 타입이 _class_ 선언의 파라메터를 대체한다는 점을 고려해야 합니다. `class Eq a where`의 `a`는 인스턴스로 만들어질때 실제 타입으로 대체될 것 입니다. 따라서 타입을 함수 타입 선언안에도 넣도록 해야합니다. `(==) :: Maybe -> Maybe -> Bool`는 안되지만 `(==) :: (Eq m) => Maybe m -> Maybe m -> Bool`는 가능합니다. 이렇게되면 어떤 경우에 관계없이 `==`이 항상 `(==) :: (Eq a) => a -> a -> Bool`의 타입을 갖는 것을 고려해야 합니다.
>
> 만약 타입클래스의 인스턴스가 무엇인지 알려면, GHCI에서 `:info YourTypeClass`라고 치면 됩니다. 따라서 `:info Num`이라고 치면 타입클래스가 정의하는 함수와 타입클래스에 있는 타입 목록을 보여줍니다. 또한 `:info`는 타입과 타입 생성자에 대해서도 동작합니다. 만약 `:info Maybe`라고 하면, `Maybe`가 인스턴스인 모든 타입클래스들을 보여줄 것 입니다. 또한 `info`는 함수의 타입 선언을 보여줄 수도 있습니다.

## yes-no 타입클래스 예제

자바스크립트와 같은 약한 타입 언어에서는 if 표현식안에 거의 모든 것을 넣을 수 있습니다. 예를들어, `if (0) alert("YEAH!") else alert("NO!")`, `if ("") alert("YEAH!") else alert("NO!")`, `if (false) alert("YEAH!") else alert("NO!")` 등의 같은 표현이 가능합니다. 그리고 결과는 모두 `"NO!"`를 출력합니다. 만약 `if ("WHAT") alert("YEAH!") else alert("NO!")`는 `"YEAH!"`를 출력합니다. 왜냐하면 자바스크립트에서 비어져있지 않은 문자열은 true이기 때문입니다.

하스켈에서는 boolean을 엄격하게 `Bool`로 사용하는 것이 좋지만, 여기서는 자바스크립트처럼 동작하도록 구현해 보겠습니다. 먼저 _class_ 선언으로 시작합니다.

```haskell
class YesNo a where
    yesno :: a -> Bool
```

`YesNo` 타입클래스는 하나의 함수로 정의되었습니다. 이 함수는 참과 거짓의 의미를 가질 수 있는 타입의 값을 받아서 참인지 여부를 알려줍니다. 여기서 `a`는 구체적인 타입이 되어야 합니다.

다음으로 숫자에 대한 인스턴스를 정의하겠습니다. 여기서는 자바스크립트처럼 0이면 거짓, 1이면 참으로 하겠습니다.

```haskell
instance YesNo Int where
    yesno 0 = False
    yesno _ = True
```

리스트에 대한 인스턴스는 빈리스트면 거짓이고, 그렇지 않으면 참입니다.\(문자열도 리스트로 정의됩니다.\)

```haskell
instance YesNo [a] where
    yesno [] = False
    yesno _ = True
```

`Bool`은 이미 참과 거짓의 분명하기 때문에 아래와 같이 정의합니다.

```haskell
instance YesNo Bool where
    yesno = id
```

여기서 `id`는 어떤 파라메터를 받아서 동일한 것을 리턴해주는 표준 라이브러리 함수 입니다.

`Maybe a`에 대한 인스턴스도 정의하겠습니다.

```haskell
instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False
```

여기서도 `Maybe`의 내용물에 대한 어떠한 가정도 없기 때문에 클래스 한정자는 불필요합니다. 그냥 `Nothing`이면 거짓을 `Just`이면 참이 됩니다. `Maybe`만으로는 구체적인 타입이 될 수 없기때문에\(`Maybe -> Bool`이라는 함수가 존재할 수 없듯이\) `(Maybe a)`를 넣어주어야 합니다\(`Maybe a -> Bool`은 가능하므로\). 이렇게하면 `Maybe something`의 `something`이 무엇이든지 `YesNo`의 일부가 됩니다.

이전 챕터에서 정의한 `Tree a` 타입도 만들 수 있습니다. 비어있는 트리는 거짓이고 그렇지 않으면 참입니다.

```haskell
instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True
```

TrafficLight는 빨간불인 경우에만 거짓으로 하고, 아래와 같이 정의하겠습니다.

```haskell
instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True
```

이제 정의한 인스턴스들을 확인해보면 아래와 같습니다.

```haskell
ghci> yesno $ length []
False
ghci> yesno "haha"
True
ghci> yesno ""
False
ghci> yesno $ Just 0
True
ghci> yesno True
True
ghci> yesno EmptyTree
False
ghci> yesno []
False
ghci> yesno [0,0,0]
True
ghci> :t yesno
yesno :: (YesNo a) => a -> Bool
```

이제 `YesNo`의 값들로 동작하는 가짜 if문을 만들어보겠습니다.

```haskell
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult
```

`yesnoIf` 함수는 참거짓을 판단할 값과, 참일때 리턴할 값과 거짓일때 리턴할 값을 받습니다. 실제로 실행해보면 아래와 같습니다.

```haskell
ghci> yesnoIf [] "YEAH!" "NO!"
"NO!" 
ghci> yesnoIf [2,3,4] "YEAH!" "NO!"
"YEAH!"
ghci> yesnoIf True "YEAH!" "NO!"
"YEAH!"
ghci> yesnoIf (Just 500) "YEAH!" "NO!"
"YEAH!"
ghci> yesnoIf Nothing "YEAH!" "NO!"
"NO!"
```

## Functor 타입클래스

지금까지 `Ord`, `Eq`, `Show`, `Read` 등 표준 라이브러리에 정의된 많은 타입클래스를 보았습니다. 여기서는 **맵핑할 수 있는 것\(can be mapped over\)**이라는 기능을 가진 `Functor` 타입클래스에 대해서 알아보겠습니다. 리스트를 다룰때 맵핑이 매우 자주 쓰이는데, 바로 이 리스트가 `Functor` 타입클래스에 포함됩니다.

`Functor` 타입클래스는 아래와 같이 구현됩니다.

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

여기에는 `fmap`이라는 한개의 함수가 정의되어 있고, 기본 구현체는 없습니다. 지금까지의 타입클래스 정의에서는 `(==) :: (Eq a) => a -> a -> Bool`의 `a`처럼 타입클래스내의 타입의 역할을 수행하는 타입 변수가 구체적인 타입이었습니다. 그러나 `Functor`의 `f`는 구체적인 타입\(`Int`, `Bool`, `Maybe String`과 같이 값을 가질 수 있는 타입\)은 아니지만 한개의 타입 파라메터를 받는 타입 생성자\(type constructor\)입니다. 잠깐 관련 내용을 복습해보면 `Maybe Int`는 구체적인 타입이지만, `Maybe`는 파라메터로 하나의 타입을 받는 타입 생성자입니다. `fmap`은 `a` 타입을 받아서 `b` 타입을 리턴하는 함수와 `a` 타입이 적용된 펑터를 받아서 `b` 타입이 적용된 펑터를 리턴합니다.

`fmap`을 타입 선언을 보면 `map`의 타입 시그니쳐인 `map :: (a -> b) -> [a] -> [b]`를 떠올려 볼 수 있습니다. `map`도 `a` 타입을 받아서 `b` 타입을 리턴하는 함수와 `a` 타입을 가진 리스트를 받아서 `b` 타입을 가진 리스트를 리턴합니다. 사실은 `map`은 리스트에서만 동작하는 `fmap`일 뿐입니다. 리스트는 아래와 같은 방법으로 `Functor` 타입클래스의 인스턴스로 만들 수 있습니다.

```haskell
instance Functor [] where
    fmap = map
```

`fmap :: (a -> b) -> f a -> f b`를 통해서 `f`가 하나의 타입을 받는 타입 생성자가 되어야 하는 것을 알 수 있기때문에, `instance Functor [a] where`와 같이 작성하지 않았습니다.

`[a]`는 이미 구체화된 타입\(리스트가 어떤 타입을 포함하든\)입니다. 반면에 `[]`는 하나의 타입을 받아서 `[Int]`, `[String]`, `[[String]]`과 같은 타입을 생성하는 타입 생성자 입니다.

리스트로 만들었을때 `fmap`은 `map`과 동일하기 때문에, 아래와 같이 리스트에 대해서 동일한 결과를 리턴합니다.

```haskell
map :: (a -> b) -> [a] -> [b]
ghci> fmap (*2) [1..3]
[2,4,6]
ghci> map (*2) [1..3]
[2,4,6]
```

빈리스트인 경우에는 `map`과 `fmap`은 모두 빈리스트를 리턴합니다. 이 경우는 그냥 `[a]` 타입의 목록을 `[b]` 타입의 목록으로 바꿉니다.

Functor는 마치 무엇인가를 담을 수 있는 컨테이너 같습니다. 리스트는 무한대의 작은 칸이있는 컨테이너에 비유할 수 있습니다. 리스트에는 아무것도 없이 비어있거나, 하나는 가득차고 나머지는 비어있거나 또는 많은 수가 채워져 있을수도 있습니다. 또한 `Maybe a` 타입도 컨테이너와 같은 속성을 가지고 있습니다. 값이 `Nothing`일때 처럼 아무것도 가지고 있지 않거나, 값이 `Just "HAHA"`와 같은 어떤 아이템을 가질 수도 있습니다. 아래는 `Maybe`를 펑터로 만드는 예제입니다.

```haskell
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
```

여기서도 `Maybe`와 `YesNo`를 다룰때 처럼, `instance Functor (Maybe m) where` 대신에 `instance Functor Maybe where`로 작성되었습니다. `Functor`**는 구체화된 타입이 아닌 타입 한개를 받는 타입 생성자를 요구합니다.** 만약 `f`들을 `Maybe`로 교체하면, `fmap`은 `Maybe` 타입에 대해서 `(a -> b) -> Maybe a -> Maybe b`와 같이 동작합니다. 하지만, 만약 `f`들을 `(Maybe m)`으로 교체하면, `(a -> b) -> Maybe m a -> Maybe m b`와 같이되서 `Maybe`가 하나의 타입 파라메터만 받는다는 점에서 위배됩니다.

어쨋든 `fmap` 구현 부분은 상당히 간단합니다. 만약 `Nothing`으로 비워져있다면, `Nothing`을 리턴합니다. 만약 비어있지 않고 `Just`안에 포장된 값이 있으면, `Just`가 가진 아이템에 입력받은 함수를 적용합니다.

```haskell
ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") (Just "Something serious.")
Just "Something serious. HEY GUYS IM INSIDE THE JUST"
ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") Nothing
Nothing
ghci> fmap (*2) (Just 200)
Just 400
ghci> fmap (*2) Nothing
Nothing
```

이전 챕터에서 만들었던 `Tree a`도 `Functor`로 만들어질 수 있습니다. `Tree`도 비어있거나 다양한 타입이 들어갈 수 있는 컨테이너가 될 수 있습니다. 그리고 `Tree`의 타입 생성자는 정확히 하나의 타입 파라메터를 받습니다. 만약 `fmap`를 `Tree`만 적용이 가능한 함수로 만들면, `(a -> b) -> Tree a -> Tree b`가 됩니다.

이번에는 펑터 인스턴스를 정의하기 위해서 재귀를 사용할 것입니다. 빈트리로 맵핑되면 빈트리를 리턴하고, 비어있지 않으면 트리의 루트, 왼쪽 서브트리, 오른쪽 서브트리까지 모두 해당 타입으로 맵핑된 트리가 될 것 입니다.

```haskell
instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)
```

```haskell
ghci> fmap (*2) EmptyTree
EmptyTree
ghci> fmap (*4) (foldr treeInsert EmptyTree [5,7,3,2,1,7])
Node 28 (Node 4 EmptyTree (Node 8 EmptyTree (Node 12 EmptyTree (Node 20 EmptyTree EmptyTree)))) EmptyTree
```

이번에는 `Either a b`를 펑터로 만들어 보겠습니다. `Functor` 타입클래스는 하나의 타입 파라메터를 받는 타입 생성자를 받습니다. 그런데 `Either`의 경우 두개의 타입 파라메터를 받습니다. 이때는 `Either`를 부분 적용하여 한개의 파라메터만 제공하고, 하나의 자유 파라메터\(free parameter\)를 갖도록 할 수 있습니다. 아래는 표준 라이브러리에 정의된 `Either a` 입니다.

```haskell
instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x
```

`Either a`는 한개의 파라메터만 받는 타입 생성자고, `Either`는 두개의 파라메터가 필요하기 때문에 `Either a`를 사용하였습니다. 이것을 `fmap`에 적용해보면 `(b -> c) -> (Either a) b -> (Either a) c`이고, `(b -> c) -> Either a b -> Either a c`와 같습니다.

구현부를 살펴보면, `Right` 값 생성자의 경우는 `(f x)`로 함수에 맵핑하였지만 `Left`는 그렇게 하지 않았습니다. 이렇게된 이유는 아래 `Either a b` 타입의 정의 때문입니다.

```haskell
data Either a b = Left a | Right b
```

`Left`와 `Right`를 하나의 함수에 맵핑하려면 `a`와`b`가 같은 타입이어야 합니다. 예를들어 만약 맵핑 함수가 문자열을 받아서 문자열을 리턴하고 `b`는 문자열인데, `a`는 숫자 타입이라면 동작되지 않습니다.

`fmap`의 타입이 `Either` 값에서만 동작하는 것을 보면, 첫번째 파라메터는 동일하게 유지되어야하는 반면에 두번째 파라메터는 변경될 수 있습니다. 그리고 첫번째 파라메터의 타입은 `Left` 값 생성자에 의해서 정해집니다.

이것을 컨테이너로 비유하면, `Left` 왜 비어있는지를 알려주는 메시지가 측면에 적여있는 빈 컨테이너로 생각할 수 있습니다.

`Data.Map`의 맵들도 어떤 값들을 가지고 있을 수 있기때문에 Functor로 만들 수 있습니다. `Map k v`의 경우, `fmap`은 `Map k v` 타입의 맵에 `v -> v'` 맵핑 함수를 적용하여 `Map k v'`를 리턴합니다.

> `'`는 특별한 의미를 가진 것은 아니고, 하스켈에서 변수명에 사용할 수 있는 문자입니다. 대개 유사한데 약간 바꼈을때 사용합니다.

여기서 `Map k`에 대한 `Functor`는 직접 만들어 보시기 바랍니다!

`Functor` 타입클래스를 통해 타입클래스들이 higher-order 컨셉을 어떻게 나타낼 수 있는지 알았습니다. 또한 부분 적용된 타입으로 인스턴스를 만드는 방법도 연습하였습니다. 다음 챕터에서는 Functor를 사용하기 위한 몇가지 법칙에 대해서 살펴볼 것 입니다.

## 타입의 종류 Kinds

타입 생성자는 다른 타입들을 파라메터로 받아서 구체화된 타입들을 생성합니다. 이것은 파라메터로 값들을 받아서 값들을 생성하는 함수와 유사합니다. 타입 생성자들은 함수처럼 부분 적용이 가능합니다.\(예를들어, `Either String`은 한개의 타입을 받아서 `Either String Int`와같은 구체적인 타입을 생성하는 타입입니다.\)

이번 섹션에서는 타입 선언을 사용해서 함수에 값을 적용하는 방법을 정의한 것처럼 타입 생성자에 타입을 적용하는 방법을 정의할 것 입니다. **여기서 다루는 내용을 이해하지 못한다해도 앞으로 하스켈을 공부하는데 지장은 없습니다.** 하지만 이 절에서 설명하는 것을 이해하면 타입 시스템에 대한 깊은 이해를 얻을 수 있습니다.

`3`, `"YEAH"`, `takeWhile`과 같은 값들은 타입을 가집니다.\(함수도 값으로 넘길 수 있기때문에 값입니다.\) 타입들은 값들에 대한 설명을 해주는 값들이 담은 라벨입니다. 하지만 타입은 **kinds**라고 불리는 그들 자신에 대한 라벨을 가지고 있습니다. kind는 타입의 타입입니다. 좀 이상하고 혼란 스럽게 들리지만 실제로는 굉장한 개념입니다. 그렇다면 kinds는 무엇이고 무엇이 좋을까요?

GHCI에서 `:k` 명령을 사용하면 타입의 kind를 알 수 있습니다.

```haskell
ghci> :k Int
Int :: *
```

여기서 `*`는 구체적인 타입을 의미합니다. 구체적인 타입은 타입 파라메터를 받지않는 타입이고, 값은 이 타입만 결정되어 다른 구체적인 타입이 될 수 없습니다.

`Maybe`의 kind는 아래와 같습니다.

```haskell
ghci> :k Maybe
Maybe :: * -> *
```

`Maybe` 타입 생성자는 한개의 `Int`와 같은 구체적인 타입을 받아서 `Maybe Int`와 같은 구체적인 타입을 리턴합니다. `Maybe`에 타입 파라메터를 적용해보면 kind는 아래와 같습니다.

```haskell
ghci> :k Maybe Int
Maybe Int :: *
```

예상대로 `Maybe`에 타입 파라메터를 넣으면 구체적인 타입이 리턴됩니다. `:t isUpper`와 `:t isUpper 'A'`을 실행해보면 `isUpper`의 타입은 `Char -> Bool`이고, `isUpper 'A'`은 값이 항상 `True`이기때문에 타입은 `Bool`입니다. 하지만 `Char -> Bool`와 `True`의 kind는 모두 `*`입니다. 따라서 타입과 kind는 완전히 다른 개념입니다. 타입은 값의 라벨이고, kind는 타입의 라벨이며 이 둘은 수렴하지 않습니다.

`Either`의 kind를 살펴보겠습니다.

```haskell
ghci> :k Either
Either :: * -> * -> *
```

`Either`는 구체적인 타입을 만들기 위한 타입 파라메터로 두개의 구체적인 타입을 받습니다. `Either`도 두개의 값을 받아서 어떤 것을 리턴하는 함수의 타입 선언과 유사합니다. 타입 생성자도 함수처럼 커링이되서 부분 적용이 가능합니다.

```haskell
ghci> :k Either String
Either String :: * -> *
ghci> :k Either String Int
Either String Int :: *
```

`Either`를 `Functor` 타입클래스에 포함되도록할때, `Either`의 부분 적용을 사용했었습니다. 이렇게 했던 이유는 `Either`는 두개의 파라메터가 필요한데 `Functor`는 한개의 파라메터만 받기 때문입니다. 다시말해서 `Functor`의 kind는 `* -> *`인데 `Either`의 kind인 `* -> * -> *`에 적용하기 위해서 `* -> *`를 부분 적용한 것 입니다. 다시한번 `Functor`의 클래스 선언을 보면 아래와 같습니다.

```haskell
class Functor f where   
    fmap :: (a -> b) -> f a -> f b
```

여기서 `Functor`의 구체적인 타입을 만들기 위해서 `f` 구체적인 타입변수 하나만 받는 것을 확인할 수 있습니다. 따라서 `Functor`의 kind는 `* -> *`가 됩니다.

이제 Tofo라는 타입클래스를 예로들어 보겠습니다.

```haskell
class Tofu t where  
    tofu :: j a -> t a j
```

`j a`는 `tofu` 함수가 매개변수로 받는 값의 타입으로 사용되기 때문에 `j a`의 kind는 `*`\(구체적인 타입\)이어야 합니다. `j a`에서 `a`를 `*`로 가정하면 `j`의 kind는 `* -> *`입니다. `t`는 두개의 타입을 받아서 구체적인 타입을 생성해야 합니다. `t a j`에서 `a`의 kind는 `*`고 `j`의 kind는 `* -> *`이므로 `t`의 kind는 `* -> (* -> *) -> *`입니다. 따라서 구체적인 타입 `a`와 구체적인 타입 `j`를 받는 타입 생성자를 받아서 구체적인 타입을 생성합니다.

kind가 `* -> (* -> *) -> *`인 타입을 하나 만들어보면 아래와 같습니다.

```haskell
data Frank a b  = Frank {frankField :: b a} deriving (Show)
```

`a`를 `*`\(구체적인 타입\)으로 가정하면, `b`는 하나의 타입 파라메터 받으므로 kind는 `* -> *`입니다. 여기서 `a`와 `b`는 `Frank`의 두개의 파라메터이므로 kind가 `* -> (* -> *) -> *`가 됩니다. 첫번째 `*`는 `a`이고 `(* -> *)`는 `b`입니다. 이제 `Frank`의 값을 만들어 타입을 확인해 보겠습니다.

```haskell
ghci> :t Frank {frankField = Just "HAHA"}
Frank {frankField = Just "HAHA"} :: Frank [Char] Maybe
ghci> :t Frank {frankField = Node 'a' EmptyTree EmptyTree}
Frank {frankField = Node 'a' EmptyTree EmptyTree} :: Frank Char Tree
ghci> :t Frank {frankField = "YES"}
Frank {frankField = "YES"} :: Frank Char []
```

`frankField`는 `a b` 현태의 타입을 가지고 있기 때문에, 값들도 유사한 형태의 타입을 가져야만 합니다. 따라서 `Maybe [Char]`을 타입으로 가지는 `Just "HAHA"`가 될 수 있습니다. 또한 `[Char]`\(=`List Char`\) 타입인 `['Y','E','S']`도 가능합니다. 여기서 `Frank`의 값들의 타입은 `Frank`의 kind와 상응한다는 것을 알 수 있습니다. `[Char]`의 kind는 `*`이고 `Maybe`의 kind는 `* -> *`입니다. 값을 가지려면 구체적인 타입어야하고 그래서 완전히 적용되어하기 때문에 `Frank blah blaah`의 모든 값의 kind는 `*`입니다.

`Frank`를 `Tofu` 타입클래스의 인스턴스로 만들면 아래와 같습니다. `tofu`는 `j a`\(예를들어 `Maybe Int`\)를 받아서 `t a j` 리턴합니다. 여기서 `Frank`를 `j`와 바꾸면, 결과는 `Frank Int Maybe`가 됩니다.

```haskell
instance Tofu Frank where  
    tofu x = Frank x
```

```haskell
ghci> tofu (Just 'a') :: Frank Char Maybe
Frank {frankField = Just 'a'}
ghci> tofu ["HELLO"] :: Frank [Char] []
Frank {frankField = ["HELLO"]}
```

한가지 예를 더 살펴보겠습니다.

```haskell
data Barry t k p = Barry { yabba :: p, dabba :: t k }
```

`Functor`의 kind는 `* -> *`인데 `Barry`는 3개의 파라메터를 받아서 `something -> something -> something -> *` 형태가 될 것 입니다. `Barry` 예에서 `p`의 kind는 `*`입니다. `k`는 `*`로 가정하면 `t`의 kind는 `* -> *`입니다. 이제 something을 kind로 바꾸어 보면 `(* -> *) -> * -> * -> *`가 됩니다. GHCI에서 확인해 보면 아래와 같습니다.

```haskell
ghci> :k Barry
Barry :: (* -> *) -> * -> * -> *
```

이제 `Functor`의 인스턴스로 만드려면 앞의 두개의 타입 파라메터를 부분 적용해서 왼쪽을 `* -> *`로 만들어야 합니다. 이말은 인스턴스 선언의 시작이 `instance Functor (Barry a b) where`된다는 것을 의미합니다. 이것을 `fmap`에 적용해서 `Functor`의 `f`를 `Barry c d`로 바꾸어 보면, `fmap :: (a -> b) -> Barry c d a -> Barry c d b`가 됩니다. `Barry`의 세번째 타입 파라메터가 자체 필드로 변경되어 들어갔습니다.

```haskell
instance Functor (Barry a b) where  
    fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}
```

이번 섹션에서는 타입 파라메터가 동작하는 방식을 보고, 타입 선언으로 함수 파라메터를 형식화한 것 처럼, 타입을 형식화한 kind를 살펴 보았습니다. 여기서 함수와 타입 생성자는 유사하지만 두가지 차이점을 발견할 수 있습니다. 실제 하스켈이 동작할때, 직접 kind를 다룰 필요는 없습니다. 보통 표준 타입클래스의 인스턴스를 만들때, 자체 정의한 타입을 `* -> *`, 또는`*`으로 부분 적용할 뿐입니다. 이번 섹션에서 다룬 내용을 이해할 필요는 없지만, kind가 어떻게 동작하는지 이해하면 하스켈의 타입시스템을 좀 더 깊이 파악할 수 있습니다.

