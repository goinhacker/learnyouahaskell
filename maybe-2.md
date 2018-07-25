# Maybe 모나드

이제는 놀라울 것도 없이 `Maybe`도 모나드입니다. 이번 챕터에서는 `Maybe`가 어떻게 모나드와 결합될 수 있는지 살펴봅시다. 모나드를 이해하려면 반드시 애플리케이티브 펑터의 인스턴스들이 어떻게 동작하는지를 이해하고 있어야 합니다. 왜냐하면 모나드는 `Applicative`를 업그레이드하는 것 이상도 이하도 아니기 때문입니다. 

`Maybe a` 타입의 값은 실패할 가능성이있는 컨텍스트에 있는 `a` 타입의 값을 나타냅니다. `Just "dharma"`의 값은 `"dharma"` 문자열있는 것을 의미하지만, `Nothing`은 값이 없거나 문자열을 계산결과 봤을때 계산이 실패했음을 의미합니다.

우리가 `Maybe`를 펑터로 봤을때, `Just`에 `fmap`을 적용하면 값이 바뀌지만, `Nothing`이면 바꿀 값이 없기때문에 그대로 `Nothing`을 반환합니다.

```haskell
ghci> fmap (++"!") (Just "wisdom")  
Just "wisdom!"  
ghci> fmap (++"!") Nothing  
Nothing
```

`Maybe`를 애플리케이티브 펑터로 봐도 유사합니다. `Maybe` 안에 함수와 `Maybe` 안에 값을 적용하기 위해서 `<*>`를 사용하는데, 두개의 `Maybe`가 모두 `Just`일때만 `Just`에 결과를 넣어서 반환합니다. 하나라도 `Nothing`이면 결과를 `Nothing`이 됩니다. 

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





























