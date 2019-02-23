# 예외

`Maybe`는 실패할 가능성을 포함하는 컨텍스트이다. `Maybe`는 `Just somthing` 또는 `Nothing`가 된다. 여기서 `Nothing`는 실패를 의미하지만, 왜 실패했는지에 대한 부가적인 정보를 담을 수는 없다. 

반면에 `Either e a`는 실패를 포함할 뿐만 아니라, 실패에 대한 부가정보를 담을 수 있다. `Either`는 성공했을때의 결과를 나타내는 `Right`와 실패를 의미하는 `Left`가 될 수 있다. 예를들어

```haskell
ghci> :t Right 4  
Right 4 :: (Num t) => Either a t  
ghci> :t Left "out of cheese error"  
Left "out of cheese error" :: Either [Char] b 
```

`Either`는 `Maybe`의 향상된 버전이기 때문에 모나드가 될 수 있다. 왜냐하면 `Either`도 실패할 가능성을 포함하고 있고, 단지 실패한 이유에 대한 표시만 추가한 것이기 때문이다. `Control.Monad.Error`에 `Maybe`와 유사한 `Either`의 `Monad` 인스턴스가 있다. 

```haskell
instance (Error e) => Monad (Either e) where  
    return x = Right x   
    Right x >>= f = f x  
    Left err >>= f = Left err  
    fail msg = Left (strMsg msg)  
```

`return`은 항상 값을 받아서 기본이되는 최소한에 컨텍스트에 값을 넣는다. 여기서는 입력 값이 그대로 성공했을때의 결과값이 되기 때문에 입력 값을 넣은 `Right`를 반환한다. `Maybe`의 `return` 함수와 거의 `유사하다.` 

`>>=`는 `Right`, `Left` 두가지 경우에 대한 처리를 해주었다. `Right`일때는 값을 함수 `f`에 적용해서 반환한다. 이것은 `Maybe`의 `Just`와 유사하다. `Left`인 경우는 에러에 대한 설명인 입력 값을 그대로 `Left`에 넣어서 반환한다. 

`Either e`의 `Monad` 인스턴스는 추가적인 요구사항이 있다. 그것은 `Either`가 `Left`일때 포함하는 타입 매개변수 `e`가 `Error` 타입클래스의 인스턴스라는 점이다. `Error` 타입클래스는 에러 메세지와 같은 행위를 포함하는 타입들이다. 그리고 이러한 행위를 정의하는 `strMsg` 함수가 있다. 이 함수는 문자열 형태로 오류를 받아서 그 값을 반환한다. `Error` 타입클래스의 인스턴트의 가장 대표적인 예가 `String`이다. `String`일때 `strMsg` 함수는 그냥 입력된 문자열을 그대로 반환한다. 

```haskell
ghci> :t strMsg  
strMsg :: (Error a) => String -> a  
ghci> strMsg "boom!" :: String  
"boom!"  
```

하지만 일반적으로 `Either`를 사용해서 에러를 표현할때 `Stirng`을 사용한다. 하지만 이러한 문제에 대해서 걱정할 필요는 없다. 왜냐하면 `do` 구문안에서 패턴매칭에 실패하면, 이 실패를 나타내는 `Left`가 사용되기 때문이다. 몇가지 예를 살펴보자. 

```haskell
ghci> Left "boom" >>= \x -> return (x+1)  
Left "boom"  
ghci> Right 100 >>= \x -> Left "no way!"  
Left "no way!" 
```

`>>=`를 사용해서 `Left`에 어떤 람다함수를 넣으면 위와같이 입력 함수는 무시하고, `Left`가 반환된다.

`Right`에 `>>=`로 함수를 넣어서 실행하면 입력 값을 함수에 적용한다. 예제에서는 함수 내부에서 결과적으로 `Left`를 반환하고 있기때문에 최종 결과도 `Left`가 된다. 

`Right`를 어떤 함수에 입력으로 넣을때 아래와 같이 특이한 오류를 만날 수 있다. 

```haskell
ghci> Right 3 >>= \x -> return (x + 100)  
  
<interactive>:1:0:  
    Ambiguous type variable `a' in the constraints:  
      `Error a' arising from a use of `it' at <interactive>:1:0-33  
      `Show a' arising from a use of `print' at <interactive>:1:0-33  
    Probable fix: add a type signature that fixes these type variable(s)
```

`Right` 부분만 화면에 출력하고 했는데, 하스켈은 `Either e a` 타입에서 `e` 부분에 대한 타입을 알지 못한다고 알려주었다. 이것은 `Monad` 인스턴트의 `Error e` 라는 타입 제한때문이다. 따라서 `Either` 모나드를 사용하다가 이런 에러는 만난다면 아래와 같이 처리하면 된다. 

```haskell
ghci> Right 3 >>= \x -> return (x + 100) :: Either String Int  
Right 103 
```

이외에 다른 부분은 이전 챕터에서 배운 `Maybe`  모나드와 거의 유사하다. 이전 챕터에서 `Maybe`를 사용해서 만들었던 줄타기 예제도 `Either`로 재작성할 수 있다. 연습삼아서 직접 만들어 보라.































