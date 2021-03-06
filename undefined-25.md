# Reader 모나드\(함수 모나드\)

애플리케이티브 펑터 챕터에서 함수 타입, `(->) r`는 `Functor`의 인스턴스라는 것을 알았습니다. 

```haskell
ghci> let f = (*5)  
ghci> let g = (+3)  
ghci> (fmap f g) 8  
55 
```

위와같이 `fmap f g`는 먼저 입력값 `8`을 `g`에 적용한 후, 그 결과값을 `f`에 적용합니다. 이와같은 함수들은 애플리케이티브 펑터입니다. 그리고 애플리케이티브 펑터는 이전의 연산 결과를 가지고 다음 연산을 진행합니다.

```haskell
ghci> let f = (+) <$> (*2) <*> (+10)  
ghci> f 3  
19  
```

예를들어 표현식, `(+) <$> (*2) <*> (+10)`는 어떤 숫자를 받아서 `(*2)`와 `(+10)`를 구하고, 각 결과를 더해서 반환하는 함수입니다. 예를들어 `3`을 입력으로 넣으면 `3`을 `(*2)`와 `(+10)`에 적용해서 `6`과 `13`을 만들고, `(+)`에 `6`과 `13`을 입력으로 넣어 `19`를 만듭니다. 

`(->) r`은 펑터, 애플리케이티브 펑터일 뿐만 아니라 모나이드 이기도 합니다. 다른 모나딕 값들처럼 함수도 어떤 컨텍스트내의 값으로 볼 수 있습니다. **함수 컨텍스트는 값이 아직 나타나지 않고, 어떤 것을 적용하고 나서야 결과값을 얻는 것**입니다. 

그럼 이제 펑터나 애플리케이티브 펑터처럼 함수의 `Monad`의 인스턴스를 살펴보겠습니다. `Control.Monad.Instances`에는 아래와 같이 정의되어 있습니다. 

```haskell
instance Monad ((->) r) where  
    return x = \_ -> x  
    h >>= f = \w -> f (h w) w 
```

우리는 이미 함수에 대한 `pure`를 보았습니다. `return`은 `pure`와 동일합니다. 어떤 값을 받아서  항상 그 값을 결과로서 가지고 있는 최소한의 컨텍스트에 넣습니다. 항상 입력된 값을 결과로 만드는 함수는 입력값을 무시하고 값을 반환하는 함수 `\_ -> x`가 됩니다.

`>>=`는 어떤 모나딕 값을 함수에 넣기위해서 사용됩니다. 그리고 결과는 항상 모나딕 값이 됩니다. 따라서 이 경우, 어떤 함수를 다른 함수에 입력으로 주었을때, 그 결과 역시 함수가 되어야 합니다. 그래서 `>>=`의 구현부는 람다로 시작합니다. `>>=`의 모든 구현은 항상 모나딕 값으로 부터 결과를 분리한 다음에 그 결과에 함수 `f`를 적용했습니다. 여기서도 동일합니다. 함수로 부터 결과를 얻기위해서는 어떤 것을 그 함수에 적용해야 합니다. 그래서 `(h w)`와 같이 값 `w`를 `h`함수에 적용한 결과를 함수 `f`에 적용합니다. 이 결과는 함수인 모나딕 값을 반환하고, 이 반환된 함수에 `w`를 적용합니다.

이렇게 봐서는 `>>=`가 어떻게 동작하는지 이해하기 어렵습니다. 아래 예를 보겠습니다. 

```haskell
import Control.Monad.Instances  
  
addStuff :: Int -> Int  
addStuff = do  
    a <- (*2)  
    b <- (+10)  
    return (a+b) 
```

이 함수는 위에서 본 애플리케이티브 표현과 동일한데,  여기서는 함수가 모나드의 인스턴스입니다. _do_ 표현은 항상 모나딕 값을 가집니다. 여기서 모나딕 값의 결과는 함수입니다. `addStuff` 함수는 어떤 숫자를 받아서 `(*2)`를 숫자에 적용하고 결과는 `a`가 됩니다.  `(+10)`은 `(*2)`가 적용된 숫자와 동일한 숫자에 적용되어 `b`가 됩니다. 그리고 나서 함수의 결과로 `(a + b)`를 `return`으로 함수 컨텍스트에 담습니다. 실행해보면 아래와 같은 결과를 얻을 수 있습니다.

```haskell
ghci> addStuff 3  
19 
```

입력 숫자 `3`에 `(*2)`와 `(+10)`를 모두 적용하고, `return (a + b)`를 해서 항상 결과로서 `a+b`를 가지도록 합니다. 이런 이유로 함수 모나드는 항상 reader 모나드라고 불립니다. 모든 함수들은 공통의 source로 부터 읽어옵니다.  `addStuff` 함수를 좀더 보기좋게 작성하면 아래와 같습니다.

```haskell
addStuff :: Int -> Int  
addStuff x = let  
    a = (*2) x  
    b = (+10) x  
    in a+b
```

**Reader 모나드는 함수를 어떤 컨텍스트 내의 값으로 다룰 수 있게합니다.** 그래서 함수가 반환할 내용을 이미 알고있는 것처럼 행동할 수 있습니다. 함수들을 하나의 함수로 묶은 다음에 함수의 매개변수를 모든 함수에 입력으로 넣어줍니다. 따라서 우리가 **하나의 매개변수를 가진 많은 함수들에 동일한 입력값을 주고 사용할때, Reader 모나드를 사용할 수 있습니다.** 이렇게 하면 `a`와 `b`같은 각 **함수들의 미래의 결과들을 가지고 동작을 구현**할 수 있습니다. 













