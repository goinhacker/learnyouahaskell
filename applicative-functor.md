## Applicative functors

이번 챕터에서는 펑터에 업그레이드 버전인 Applicative 펑터에 대해서 알아보겠습니다. 하스켈에서는 `Control.Applicative` 모듈에 `Applicative` 타입클래스로 정의되어 있습니다. 

하스켈에서는 기본적으로 모든 함수는 커링이다. 이말은 여러개의 매개변수를 받는 함수들은 사실은 하나의 매개변수를 받아서 리턴하는 함수들의 체인으로 이루어져있다는 것 이다. 만약 `a -> b -> c` 타입의 함수가 있다면, 두개의 매개변수를 받아서 `c`를 반환하는 함수라고 한다. 하지만 실제로는 `a`를 받아서 `b -> c`를 반환하는 함수다. 따라서 `f x y` 또는 `(f x) y`로 함수를 호출할 수 있다. 이러한 매커니즘은 적은 매개변수의 함수 호출를 부분적으로 적용하여, 그 결과를 다른 함수로 전달할 수 있게 합니다. 

펑터로 맵핑을 할때 보통은 하나의 매개변수만 받는 함수로 맵핑하였습니다. 하지만 만약 두개의 매개변수를 받는 `*` 같은 함수를 펑터로 맵핑할때는 어떻게 될까요? 예를들어 `fmap (*) (Just 3)`의 결과는 `Just ((*) 3)`이고, 이것은 `Just (* 3)`와 같습니다. 따라서 결과는 `Just`로 랩핑된 함수를 얻었습니다!!

```haskell
**[terminal]
**[prompt ghci> ]**[command :t fmap (++) (Just "hey")]
fmap (++) (Just "hey") :: Maybe ([Char] -> [Char])
**[prompt ghci> ]**[command :t fmap compare (Just 'a')]
fmap compare (Just 'a') :: Maybe (Char -> Ordering)
**[prompt ghci> ]**[command :t fmap compare "A LIST OF CHARS"]
fmap compare "A LIST OF CHARS" :: [Char -> Ordering]
**[prompt ghci> ]**[command :t fmap (\x y z -> x + y / z) [3,4,5,6]]
fmap (\x y z -> x + y / z) [3,4,5,6] :: (Fractional a) => [a -> a -> a]
```

`compare`의 타입은 `(Ord a) => a -> a -> Ordering` 입니다. 문자의 리스트를 `compare`로 맵핑하면 `Char -> Ordering` 타입의 함수의 리스트를 반환합니다. `(Ord a) => a -> Ordering` 함수의 리스트가 아닌 이유는 첫번째 매개변수 `a`가 `Char`이므로 두번째 매개변수 `a`는 이미 `Char`로 결정되었기 때문입니다. 

매개변수가 여러개인 경우에는 펑터를 적용하면 함수를 가지고 있는 펑터가된다는 것을 알았습니다. 그렇다면 이것들로 무엇을 할 수 있을까요? 우선 매개변수로 함수를 받는 함수를 맵핑할 수 있습니다. 왜냐하면 펑터안에 있는 것이 뭐든간에 매개변수로 맵핑하는 함수에 입력으로 주어지기 때문입니다.

```haskell
**[terminal]
**[prompt ghci> ]**[command let a = fmap (*) [1,2,3,4]]
**[prompt ghci> ]**[command :t a]
a :: [Integer -> Integer]
**[prompt ghci> ]**[command fmap (\f -> f 9) a]
[9,18,27,36]
```

만약 값이 `Just (3 *)`인 펑터와 값이 `Just 5`인 펑터를 가지고 있을때, `Just (3 *)`에서 함수만 꺼내서 `Just 5`에 맵핑하고 싶다면 어떻게 해야할까요? 일반적인 펑터는 기존 펑터에 함수를 맵핑하는 기능만 제공합니다. 내부에 함수가 포함된 펑터에서 `\f -> f 9`를 맵핑할때도 일반 함수로 맵핑했습니다. 따라서 `fmap`을 사용해서는 펑터 내부의 함수를 다른 펑터 내부의 함수로 맵핑할 수 없습니다. 펑터내의 함수를 얻기 위해서 `Just` 생성자에 대한 패턴 매칭을 사용할 수 있습니다. 그리고 나서 `Just 5`를 맵핑합니다. 하지만 이 작업을 수행하는 더 일반적이고 추상화된 방법으로 펑터를 활용할 수 있습니다. 

`Control.Applicative` 모듈에 정의된 `Applicative` 타입클래스는 `pure`와 `<*>` 함수가 있습니다. 그리고 default 구현체는 제공하지 않습니다. 따라서 어플리케이티브 펑터로 만들기 위해서는 두 함수 모두 정의해야합니다. 이 클래스는 아래와 같이 생겼습니다. 

```haskell
class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b
``` 

이 간단한 세라인짜리 선언은 많은 것을 말해줍니다. 먼저 `Applicative` 클래스의 정의와 한정자를 선언하였습니다. 만약 타입생성자를 `Applicative` 타입클래스의 부분으로 만들려면 먼저 `Functor`를 가지고 있어야 합니다. 따라서 타입생성자가 `Applicative` 타입클래스의 부분일때는 `Functor`에도 포함되므로 `fmap`을 사용할 수 있습니다. 

`pure` 함수의 타입은 `pure :: a -> f a` 입니다. `f`는 여기서 어플리케이티브 펑터 인스턴스입니다. `pure`는 임의의 타입의 값을 받아서 안에 그 값을 포함한 어플리케이티브 펑터를 반환합니다. `a -> f a`는 상자에 비유하면, 어떤 값을 받아서 그 결과값이 안에 들어있는 어플리케이티브 펑터 상자에 포장합니다. 

`pure`는 어떤 값을 받아서 어떤 기본 컨텍스트안에 넣는 것 입니다. 

`<*>` 함수의 타입은 `f (a -> b) -> f a -> f b` 입니다. 이것은 `fmap :: (a -> b) -> f a -> f b`인 `fmap`이 업그레이드된 버전입니다. `fmap`은 함수를 받아서 적용한 후, 펑터에 다시 넣는 반면, `<*>` 함수는 함수를 가진 펑터와 또다른 펑터를 받아서, 첫번째 펑터에서 함수를 빼서 두번째 펑터에 맵핑합니다. 여기서 추출한다는 것은 실제로 실행(run)하고나서 추출(extract)하는 것을 의미합니다. 

이제 `Maybe`를 `Applicative`의 인스턴스로 만들어 보겠습니다. 

```haskell
instance Applicative Maybe where  
    pure = Just  
    Nothing <*> _ = Nothing  
    (Just f) <*> something = fmap f something
```














