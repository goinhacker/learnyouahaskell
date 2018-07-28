# Maybe 모나드

이번 챕터에서는 `Maybe`가 어떻게 모나드와 결합될 수 있는지 살펴봅시다. 모나드를 이해하려면 반드시 애플리케이티브 펑터의 인스턴스들이 어떻게 동작하는지를 이해하고 있어야 합니다. 왜냐하면 모나드는 `Applicative`를 업그레이드하는 것 이상도 이하도 아니기 때문입니다. 

`Maybe a` 타입의 값은 실패할 가능성이있는 컨텍스트에 있는 `a` 타입의 값을 나타냅니다. `Just "dharma"`의 값은 `"dharma"` 문자열있는 것을 의미하지만, `Nothing`은 값이 없거나 문자열을 계산결과 봤을때 계산이 실패했음을 의미합니다.

우리가 `Maybe`를 펑터로 봤을때, `Just`에 `fmap`을 적용하면 값이 바뀌지만, `Nothing`이면 바꿀 값이 없기때문에 그대로 `Nothing`을 반환합니다.

```haskell
ghci> fmap (++"!") (Just "wisdom")  
Just "wisdom!"  
ghci> fmap (++"!") Nothing  
Nothing
```

`Maybe`를 애플리케이티브 펑터로 봐도 유사합니다. `Maybe` 안에 함수와 `Maybe` 안에 값을 적용하기 위해서 `<*>`를 사용하는데, 두개의 `Maybe`가 모두 `Just`일때만 `Just`에 결과를 넣어서 반환합니다. 하나라도 `Nothing`이면 결과는 `Nothing`이 됩니다. 

```haskell
ghci> Just (+3) <*> Just 3  
Just 6  
ghci> Nothing <*> Just "greed"  
Nothing  
ghci> Just ord <*> Nothing  
Nothing 
```

애플리케이티브 펑터 스타일로 작성해도 결과는 동일합니다. 

```haskell
ghci> max <$> Just 3 <*> Just 6  
Just 6  
ghci> max <$> Just 3 <*> Nothing  
Nothing
```

`>>=`는 모나드값\(monadic value\)과 일반적인 값\(nomal value\)을 받아서 모나드값을 반환하는 함수를 입력으로 받습니다. 그리고 이것을 함수에 적용해서 모나드값을 반환합니다. 여기서 함수가 모나드값을 일반적인 값으로 받아서 처리하려면 모나드값의 컨텍스트를 고려해야 합니다. 

```haskell
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
```

`Maybe`의 경우, `>>=`는 `Maybe a`와 `a -> Maybe b` 타입의 함수를 받고, `Maybe a`를 이 함수에 적용해야 합니다. 이해를 돕기위해서 쉬운 예를 살펴보겠습니다.

```haskell
ghci> (\x -> Just (x+1)) 1  
Just 2  
ghci> (\x -> Just (x+1)) 100  
Just 101 
```

`/x -> Just (x+1)`은 어떤 숫자를 받아서 `1`을 더하고 `Just`로 래핑합니다. 여기에 `1`을 넣으면 `Just 2`가 반환되고 `100`을 넣으면 `Just 101`이 반환됩니다. 근데 이 함수에 `Maybe` 값을 넣으려면 어떻게 할까요? Maybe 애플리케이티브 펑터의 방법을 생각해 보면 답은 쉽게 나올 것 입니다. 

만약에 `Just` 값을 입력으로 넣으면 `Just` 내부의 값을 꺼내서 함수에 적용하고, 만약 `Nothing`을 입력으로 넣으면, 함수는 그대로 두고, `Nothing`을 반환할 것 입니다. 여기서는 `>>=`를 `applyMaybe`라고 하고, 위와 같은 함수를 그대로 구현해보면 아래와 같습니다.

```haskell
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b  
applyMaybe Nothing f  = Nothing  
applyMaybe (Just x) f = f x
```

이제 이 코드를 한번 사용해보겠습니다. 아래 예제에서는 _infix_ 함수를 사용해서 입력 `Maybe` 값이 왼쪽에 있습니다. 

```haskell
ghci> Just 3 `applyMaybe` \x -> Just (x+1)  
Just 4  
ghci> Just "smile" `applyMaybe` \x -> Just (x ++ " :)")  
Just "smile :)"  
ghci> Nothing `applyMaybe` \x -> Just (x+1)  
Nothing  
ghci> Nothing `applyMaybe` \x -> Just (x ++ " :)")  
Nothing
```

`applyMaybe`는 `Just` 값과 함수를 받아서, `Just`안에 값을 꺼내서 함수에 적용하는데 성공했습니다. `Nothing`을 넣었을때는 역시 `Nothing`을 반환합니다. 만약 입력 함수가 `Nothing`을 반환한다면 어떻게 동작할까요? 

```haskell
ghci> Just 3 `applyMaybe` \x -> if x > 2 then Just x else Nothing  
Just 3  
ghci> Just 1 `applyMaybe` \x -> if x > 2 then Just x else Nothing  
Nothing
```

입력 함수의 결과값이 `Nothing`때는 `applyMaybe`의 결과값도 `Nothing`인것을 확인할 수 있습니다. 이것은 애플리케이티브 펑터와 매우 유사합니다. 애플리케이티브 펑터도 어딘가에 `Nothing`이 하나라도 존재하면 결과가 `Nothing`입니다. 

이번 절에서는 `Maybe`에 대해서 컨텍스트값\(fancy value\)을 일반적인 값을 받아서 컨텍스트값을 반환하는 함수의 입력으로 넣는 방법을 살펴보았습니다. 그런데 이게 왜 필요하고, 어디에 유용할까요? 

애플리케이티브 펑터는 일반적인 함수를 받아서 컨텍스트에 맞는 값으로 동작시킬 수 있기 때문에 모나드보다 유용해 보일 수 있습니다. 하지만 실제로 모나드는 애플리케이티브 펑터를 개선한 버전이기 때문에, 애플리케이티브 펑터와 동일한 기능을 할 수 있을 뿐만 아니라 더 재미난 일들도 할 수 있습니다. 

다음절에서는 Maybe 모나드를 활용해서 모나드가 언제 유용한지 살펴보기전에, 모나드 타입클래스에 대해서 살펴볼 것 입니다.











