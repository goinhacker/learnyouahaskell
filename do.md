# do 구문

하스켈에는 모나드를 사용하기 위한 특별한 문법 _do_가 있습니다. _do_ 구문은 이미 I/O를 다룰때 만나봤습니다. 여러개의 I/O 작업을 하나로 묶을때 _do_를 사용했습니다. 사실 _do_ 구문은 IO만을 위한 문법은 아닙니다. 모든 모나드에 사용할 수 있습니다. 원리는 동일합니다. 어떤 일련의 모나드 값들을 묶을때 사용합니다. 여기서는 _do_ 구문을 어떻게 사용하는지, 왜 유용한지에 대해서 알아보겠습니다.

```haskell
ghci> Just 3 >>= (\x -> Just (show x ++ "!"))  
Just "3!" 
```

이제는 친숙한 모나드 예제입니다. 우리는 예제에서 `3`이 어떻게 람다 함수의 `x`값이되서 입력되는지 이미 알고 있습니다. 만약에 람다 함수안에 또다른 `>>=`가 있으면 어떻게 될까요?

```haskell
ghci> Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))  
Just "3!" 
```

바깥쪽 람다에서 `\y -> Just (show x ++ y)`에 `Just "!"`를 넣어서 `>>=`를 중첩되게 사용되었습니다. 이 안쪽 람다에서 `y`는 `"!"`가 되고, `x`는 여전히 `3`이됩니다. 이것은 다음과 같은 표현을 떠올리게 합니다.

```haskell
ghci> let x = 3; y = "!" in show x ++ y  
"3!"
```

이 두개의 방식의 차이는 첫번째 예제의 값들은 모나드 값이라는 점입니다. 이 값들을 실패에 대한 컨텍스트를 가집니다. 따라서 아래와 같이 모나드중 하나를 실패로 바꿀수 있습니다. 

```haskell
ghci> Nothing >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))  
Nothing  
ghci> Just 3 >>= (\x -> Nothing >>= (\y -> Just (show x ++ y)))  
Nothing  
ghci> Just 3 >>= (\x -> Just "!" >>= (\y -> Nothing))  
Nothing
```

첫번째 라인은 처음부터 `Nothing`을 입력했기 때문에 최종결과는 `Nothing`이 됩니다. 두번째 라인은 `Just 3`을 받아서 `x`가 `3`이 됐지만, 람다안에서 `Nothing`을 넣었기 때문에 `Nothing`을 반환했습니다. 마지막 라인도 마찬가지 입니다. 따라서 `let` 구문에서 변수에 값을 할당하는 것과 비슷합니다. 다만, 그 값이 모나드 값입니다. 

이 점을 좀 더 이해하기 쉽게 하기 위해서, 아래와 같이 표현을 바꿔보겠습니다. 

```haskell
foo :: Maybe String  
foo = Just 3   >>= (\x -> 
      Just "!" >>= (\y -> 
      Just (show x ++ y)))
```

 이와같은 성가진 람다 표기를 막기위해서 하스켈은 `do` 구문을 제공합니다.  `do`를 사용하면 아래와 같이 다시 작성할 수 있습니다. 

```haskell
foo :: Maybe String  
foo = do  
    x <- Just 3  
    y <- Just "!"  
    Just (show x ++ y) 
```

각 단계마다 `Maybe` 값이 `Just`인지 `Nothing`인지 확인하지 않고도 `Maybe`에서 값을 추출하는 것 같습니다. 만약 `Nothing`에서 값을 추출하려고 했다면, _do_ 표현식 전체의 결과가 `Nothing`이 됩니다. _**do**_ **표현식은 모나드 값을 연결하는 다른 문법적 표현일 뿐입니다.**

_do_ 표현식에서는 모든 라인이 모나드 값입니다. 결과를 검사하기 위해서 `<-`를 사용합니다. 만약 `Maybe String`이 있고 `<-`를 사용해서 변수에 바인딩하면, 변수의 타입은 `String`이 됩니다. 이것은 `>>=`를 사용해서 람다에 모나드값을 제공하는 것과 같습니다. `Just (show x ++ y)`같은 _do_ 표현식의 마지막 모나드 값은 결과를 `<-`로 바인딩 할 수 없습니다. 왜냐하면 _do_ 표현식으로 `>>=`의 체인으로 바꾸어 표현될 수 없기 때문입니다. 오히려 마지막 모나드 값의 결과는 가능한 실패를 고려한 앞의 모든 모나드 값 전체의 결과입니다.

```haskell
ghci> Just 9 >>= (\x -> Just (x > 8))  
Just True 
```

예제에서 `>>=`의 왼쪽 매개변수가 `Just`이기 때문에, 람다는 `9`를 적용해서 `Just True`가 됩니다. _do_를 사용해서 재작성하면 아래와 같습니다. 

```haskell
marySue :: Maybe Bool  
marySue = do   
    x <- Just 9  
    Just (x > 8) 
```

이 두가지를 비교해 보면 전체 모나드 값의 결과가 _do_ 표현식의 마지막 모나드 값의 결과인 이유를 쉽게 파악할 수 있습니다. 

줄타기 예제도 _do_를 사용해서 표현할 수 있습니다. `landLeft`와 `landRight`는 새의 수와 막대를 받아서, 만약 넘어지지 않으면 `Just`에 감싸진 막대를 반환하고, 넘어지면 `Nothing`을 반환합니다. `>>=`의 체인을 사용하면, 각 단계가 이전의 결과에 의존하고 실패할 가능성을 가진 컨텍스트로 연결됩니다. 두마리가 왼쪽에 앉고나서 두마리가 오른쪽에 앉고, 왼쪽에 한마리가 앉은 것을 _do_를 사용해서 작성하면 아래와 같습니다.  

```haskell
routine :: Maybe Pole  
routine = do  
    start <- return (0,0)  
    first <- landLeft 2 start  
    second <- landRight 2 first  
    landLeft 1 second
```

실행하면 아래와 같습니다. 

```haskell
ghci> routine  
Just (3,2)
```

`>>=`를 사용해서 표현하면 `return (0,0) >>= landLeft 2`와 같은 식으로 표현해야 합니다.

_do_ 표현식은 한줄씩 작성하기 때문에, 명령형 코드와 유사해 보입니다. 하지만 _do_ 표현식은 컨텍스트\(이 경우 성공 or 실패\)안에서 각 라인의 각 값이 이전 라인의 결과에 의존적이기 때문에 순차적인 것 입니다.  

여기서 다시한번 `Maybe`를 모나드를 사용하지 않고 작성한 코드로 보겠습니다. 

```haskell
routine :: Maybe Pole  
routine =   
    case Just (0,0) of   
        Nothing -> Nothing  
        Just start -> case landLeft 2 start of  
            Nothing -> Nothing  
            Just first -> case landRight 2 first of  
                Nothing -> Nothing  
                Just second -> landLeft 1 second 
```

`Just (0,0)`은 `start`가 되고, `landLeft 2 start`는 `first`가 되는 등의 동작을 합니다. 그리고 중간에 `Nothing`은 무조건 미끄러지게 하는 바나나입니다. 이것을 `do`를 사용해서 재작성하면 아래와 같습니다.  

```haskell
routine :: Maybe Pole  
routine = do  
    start <- return (0,0)  
    first <- landLeft 2 start  
    Nothing  
    second <- landRight 2 first  
    landLeft 1 second 
```

_do_ 구문에서는 `<-`를 사용해서 모나딕 값을 바인딩하지 않으면 `>>`를 넣은 것처럼 모나드 값뒤에 모든 결과를 무시합니다.  즉, `_ -> Nothing`과 동일합니다. 

_do_ 구문을 사용할지 명시적으로 `>>=`를 사용할지는 여러분의 선택 사항입니다.

_do_ 구문에서는 모나딕 값을 어떤 이름에 바인딩할때, _let_ 표현식과 함수 매개변수처럼 패턴 매칭을 활용할 수 있습니다. 

```haskell
justH :: Maybe Char  
justH = do  
    (x:xs) <- Just "hello"  
    return x 
```

`"hello"`의 첫번째 문자를 얻기 위해서 패턴 매칭을 사용했습니다. 그리고 나서 그 결과를 보면 `justH`는 `Just 'h'`가 됩니다. 

만약에 패턴 매칭이 실패하면 어떻게 될까요? 함수에서 패턴 매칭이 실패하면 다음 패턴을 매칭합니다. 주어진 함수의 모든 패턴을 통과하면 오류가 발생하고 프로그램이 중단\(crash\)됩니다. 반대로 _let_ 표현식에서 실패한 패턴 매칭은 즉시 에러가 발생합니다. 왜냐하면 _let_ 표현식에는 패턴을 나열하는 매커니즘이 없기 때문입니다. _do_ 표현에서는 패턴 매칭이 실패하면, `fail` 함수가 호출됩니다. 이 함수는 `Maybe` 타입 클래스에 정의된 함수로, 실패한 패턴 매칭으로 인해서 프로그램이 중단되는 대신, 현재 모나드의 컨텍스트에서 실패하도록 합니다. 기본 구현은 아래와 같습니다. 

```haskell
fail :: (Monad m) => String -> m a  
fail msg = error msg 
```

그래서 기본적으로 프로그래밍은 중단되지만, 가능한 실패의 컨텍스트\(`Maybe`\)를 포함하는 모나드는 보통 자체적으로 아래와 같이 구현합니다. 

```haskell
fail _ = Nothing 
```

이것은 에러 메시지를 무시하고 `Nothing`으로 만듭니다. 이것은 프로그램이 중단되는 것보다 좋습니다. 따라서 아래와같은 _do_ 표현식에서 패턴 매칭에 실패하면 

```haskell
wopwop :: Maybe Char  
wopwop = do  
    (x:xs) <- Just ""  
    return x 
```

남은 라인이 `Nothing`이 됩니다.

```haskell
ghci> wopwop  
Nothing
```

실패한 패턴으로 인해서 프로그램 전만에 오류가 발생하는 대신, 모나드 컨텍스트 내에서 오류가 발생했습니다. 이것은 매우 깔끔한 처리입니다. 



















